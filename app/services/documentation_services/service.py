import asyncio
import json
import uuid
from pathlib import Path
from typing import Dict, Any, List

from langchain.tools import ToolRuntime, tool
from app.core.tools.context import ProjectContext

from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select

from app.config.settings import settings
from app.config.llm_config import get_llm
from app.db.repositories.source_file import SourceFileRepository
from app.db.models.source_file import SourceFile

from app.services.dependency_extractor.extractors import (
    extract_cobol_dependencies,
    extract_jcl_dependencies,
    extract_ca7_dependencies,
    extract_assembly_dependencies,
    extract_pli_dependencies,
    extract_copybook_dependencies,
    extract_rexx_dependencies,
    extract_parmlib_dependencies,
    extract_pli_copybook_dependencies
)

# Import the new components
from .graph_engine import GraphAnalyzer
from .agent import create_documentation_graph
from .models import DocAgentState
from app.core.tools.file_tools import view_file, grep_search
from app.core.tools.artifact_tools import create_artifact_tools
from langchain_core.messages import SystemMessage, HumanMessage, ToolMessage

class DocumentationService:
    """
    Orchestrates the Agentic Documentation Pipeline using LangGraph.
    Generates file-wise Technical and Functional PDFs in separate artifact folders.
    """

    def __init__(self, session: AsyncSession):
        self.session = session
        self.repository = SourceFileRepository(session)
        self.artifacts_path = settings.get_artifacts_path()

    async def run_full_pipeline(self, project_id: uuid.UUID):
        """
        Main entry point for the background task.
        1. Builds the Global Graph.
        2. Runs the Agent for every file in the project.
        """
        try:
            logger.info(f"🚀 Starting Agentic Documentation Pipeline for Project: {project_id}")
            
            # 1. Load System-wide Context (Combined JSONs from all parsers)
            system_data = await self._load_all_parsed_jsons(project_id)
            if not system_data:
                logger.error(f"No parsed data found for {project_id}. Documentation aborted.")
                return

            dependency_mapped_data = self._map_dependencies(system_data)
            
            # 3. Initialize Graph Engine with the MAPPED data
            analyzer = GraphAnalyzer(dependency_mapped_data)
            logger.info(f"📊 System Graph built with {analyzer.graph.number_of_nodes()} nodes.")

            project_id_str = str(project_id)
    
            # We create a "Surgical" tool for the Agent that calls your REAL tool
            # but hard-codes the project_id behind the scenes.
            
            class RuntimeShim:
                def __init__(self, pid):
                    self.context = type('obj', (object,), {'project_id': pid})
        
            shim = RuntimeShim(project_id_str)

            # 2. Wrap the tools so the LLM only sees the 'Public' arguments
            # We manually pass the shim into the tool's underlying function (.func)
            
            @tool("view_file")
            def agent_view_file(filepath: str, start_line: int, end_line: int) -> str:
                """View specific lines from a project file. Path format: 'type/filename'."""
                # .func accesses the original python function under the @tool decorator
                return view_file.func(filepath, start_line, end_line, runtime=shim)

            @tool("grep_search")
            def agent_grep_search(pattern: str, file_pattern: str = "*") -> str:
                """Search for a pattern in project files. Returns matching lines and line numbers."""
                return grep_search.func(pattern, file_pattern, runtime=shim)

            # 3. Setup Agent Tools & LLM
            artifact_tools = create_artifact_tools(project_id_str)
            llm = get_llm()
            tools = {
                "view_file": agent_view_file,
                "grep_search": agent_grep_search,
                "read_artifact": artifact_tools[1] # read_artifact
                # Additional tools (artifact_tools, search_docs) can be added here
            }

            # 4. Compile the LangGraph State Machine
            agent_app = create_documentation_graph(llm, analyzer, tools)

            # 5. Fetch all source files associated with the project
            files = await self._get_project_files(project_id)
            logger.info(f"📂 Found {len(files)} files to document.")

            # 6. Execute Agent for each file
            # Note: For large projects, consider using a semaphore to limit concurrency
            tasks = []
            for file_record in files:
                tasks.append(
                    self._process_single_file(agent_app, project_id, file_record)
                )
            
            await asyncio.gather(*tasks)

            logger.info(f"✅ Full pipeline complete for project {project_id}")

        except Exception as e:
            logger.exception(f"❌ Agentic Pipeline failed for project {project_id}: {e}")

    async def _process_single_file(self, agent_app, project_id: uuid.UUID, file_record: SourceFile):
        """Invoke the LangGraph agent for one specific file record."""
        try:
            logger.info(f"🤖 Agent researching: {file_record.filename} ({file_record.file_type})")
            
            initial_state: DocAgentState = {
                "project_id": str(project_id),
                "target_file": file_record.filename,
                "file_type": file_record.file_type.upper(),
                "iterations": 0, # Initialize
                "mermaid_graph": "", # To be populated by Research Node
                "code_snippets": "", # To be populated by Research Node
                "functional_json": {},
                "technical_json": {},
                "messages": []
            }

            config = {"recursion_limit": 50}

            # Run the Graph
            await agent_app.ainvoke(initial_state, config=config)
            
        except Exception as e:
            logger.error(f"Failed to document file {file_record.filename}: {e}")

    async def _load_all_parsed_jsons(self, project_id: uuid.UUID) -> Dict[str, Any]:
        """
        Gathers parsed output from all file type subfolders.
        Returns a map: {'COBOL': [...], 'JCL': [...]}
        """
        parsed_dir = self.artifacts_path / str(project_id) / "parsed_outputs"
        system_data = {}

        if not parsed_dir.exists():
            return {}

        for type_folder in parsed_dir.iterdir():
            if type_folder.is_dir():
                file_type = type_folder.name.upper()
                consolidated = type_folder / "_consolidated.json"
                if consolidated.exists():
                    with open(consolidated, "r", encoding="utf-8") as f:
                        data = json.load(f)
                        system_data[file_type] = data
        
        return system_data
    
    def _map_dependencies(self, raw_data: Dict[str, List[Dict]]) -> Dict[str, Dict]:
        """
        Runs the extraction strategies to turn raw parsed lists 
        into dictionaries of relationships for the GraphAnalyzer.
        """
        mapped = {}
        
        # Strategy mapping table
        extract_strategies = {
            'COBOL': extract_cobol_dependencies,
            'JCL': extract_jcl_dependencies,
            'CA7': extract_ca7_dependencies,
            'ASSEMBLY': extract_assembly_dependencies,
            'PLI': extract_pli_dependencies,
            'COPYBOOK': extract_copybook_dependencies,
            'PLI_COPYBOOK': extract_pli_copybook_dependencies,
            'REXX' : extract_rexx_dependencies,
            'PARMLIB' : extract_parmlib_dependencies
        }

        for file_type, extractor_func in extract_strategies.items():
            if file_type in raw_data:
                # We pass the raw list to the extractor function
                # The result is the dict like {"program_calls": [], ...}
                mapped[file_type] = extractor_func(raw_data[file_type])
            else:
                # Default empty structure if type is missing
                mapped[file_type] = {}

        return mapped

    async def _get_project_files(self, project_id: uuid.UUID) -> List[SourceFile]:
        """Queries the DB for all source files in this project."""
        stmt = select(SourceFile).where(SourceFile.project_id == project_id)
        result = await self.session.execute(stmt)
        return list(result.scalars().all())

    # --- Job Status & Metadata Methods ---

    async def get_job_status(self, project_id: uuid.UUID) -> Dict:
        """
        Checks for the existence of the requirement folders to determine progress.
        """
        base = self.artifacts_path / str(project_id)
        tech_dir = base / "technical_requirements"
        func_dir = base / "functional_requirements"
        
        is_ready = tech_dir.exists() and func_dir.exists()
        
        return {
            "project_id": project_id,
            "status": "COMPLETED" if is_ready else "PENDING/IN_PROGRESS",
            "progress": 100.0 if is_ready else 0.0,
            "paths": {
                "technical": str(tech_dir) if tech_dir.exists() else None,
                "functional": str(func_dir) if func_dir.exists() else None
            }
        }