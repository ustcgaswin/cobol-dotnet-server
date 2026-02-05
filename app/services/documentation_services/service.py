import asyncio
import json
import uuid
from pathlib import Path
from typing import Dict, Any, List

from langchain.tools import ToolRuntime, tool
from app.core.tools.context import ProjectContext

from fpdf import FPDF

from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select

from app.config.settings import settings
from app.config.llm import get_llm, DOCGEN
from app.db.repositories.source_file import SourceFileRepository
from app.db.models.source_file import SourceFile
from app.db.models.project import Project, ProjectStatus



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
from ...api.schemas.doc_models import DocAgentState
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

    async def _update_project_status(self, project_id: uuid.UUID, mode: str, status: ProjectStatus):
        """Helper to update the project's documentation status in the DB."""
        try:
            # Fetch project
            result = await self.session.execute(select(Project).where(Project.id == project_id))
            project = result.scalar_one_or_none()
            
            if project:
                if mode == "ALL":
                    project.technical_document_status = status
                    project.functional_document_status = status
                elif mode == "TECHNICAL":
                    project.technical_document_status = status
                elif mode == "FUNCTIONAL":
                    project.functional_document_status = status
                
                await self.session.commit()
                logger.info(f"Updated Project {project_id} status to {status} for mode {mode}")
        except Exception as e:
            logger.error(f"Failed to update project status: {e}")        

    async def run_full_pipeline(self, project_id: uuid.UUID, mode: str = "ALL"):
        """
        Main entry point for the background task.
        1. Builds the Global Graph.
        2. Runs the Agent for every file in the project.
        """
        try:
            logger.info(f"Starting Agentic Documentation Pipeline for Project: {project_id}")

            await self._update_project_status(project_id, mode, ProjectStatus.IN_PROGRESS)
            
            # 1. Load System-wide Context (Combined JSONs from all parsers)
            system_data = await self._load_all_parsed_jsons(project_id)
            if not system_data:
                logger.error(f"No parsed data found for {project_id}. Documentation aborted.")
                return

            dependency_mapped_data = self._map_dependencies(system_data)
            
            # 3. Initialize Graph Engine with the MAPPED data
            analyzer = GraphAnalyzer(dependency_mapped_data)
            logger.info(f"System Graph built with {analyzer.graph.number_of_nodes()} nodes.")

            project_id_str = str(project_id)
    
            
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
            llm = get_llm(DOCGEN)
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
            logger.info(f"Found {len(files)} files to document.")

            # 6. Execute Agent for each file
            # Note: For large projects, consider using a semaphore to limit concurrency
            tasks = []
            for file_record in files:
                tasks.append(
                    self._process_single_file(agent_app, project_id, file_record, mode)
                )
            
            await asyncio.gather(*tasks)

            self._generate_consolidated_placeholders(project_id, mode)
            await self._update_project_status(project_id, mode, ProjectStatus.COMPLETED)

            logger.info(f"Full pipeline complete for project {project_id}")

        except Exception as e:
            logger.exception(f"Agentic Pipeline failed for project {project_id}: {e}")

    async def _process_single_file(self, agent_app, project_id: uuid.UUID, file_record: SourceFile, mode: str):
        """Invoke the LangGraph agent for one specific file record."""
        try:
            logger.info(f"Agent researching: {file_record.filename} ({file_record.file_type})")
            
            initial_state: DocAgentState = {
                "project_id": str(project_id),
                "target_file": file_record.filename,
                "file_type": file_record.file_type.upper(),
                "generation_mode": mode,
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

    # async def get_job_status(self, project_id: uuid.UUID, required_type: str = "ALL") -> Dict:
    #     """
    #     Checks for folder existence based on what was requested.
    #     """
    #     base = self.artifacts_path / str(project_id)
    #     tech_dir = base / "technical_requirements"
    #     func_dir = base / "functional_requirements"
        
    #     has_tech = tech_dir.exists() and any(tech_dir.iterdir())
    #     has_func = func_dir.exists() and any(func_dir.iterdir())
        
    #     is_ready = False
    #     if required_type == "ALL":
    #         is_ready = has_tech and has_func
    #     elif required_type == "TECHNICAL":
    #         is_ready = has_tech
    #     elif required_type == "FUNCTIONAL":
    #         is_ready = has_func

    #     return {
    #         "project_id": project_id,
    #         "status": "COMPLETED" if is_ready else "PROCESSING",
    #         "mode_requested": required_type,
    #         "artifacts": {
    #             "technical_path": str(tech_dir) if has_tech else None,
    #             "functional_path": str(func_dir) if has_func else None
    #         }
    #     }

    async def get_job_status(self, project_id: uuid.UUID, required_type: str = "ALL") -> Dict:
        """
        Checks for the existence of the CONSOLIDATED PDF files.
        """
        base = self.artifacts_path / str(project_id)
        
        # Define paths
        tech_pdf = base / "Technical_Specifications.pdf"
        func_pdf = base / "Functional_Specifications.pdf"
        
        has_tech = tech_pdf.exists()
        has_func = func_pdf.exists()
        
        # Determine status
        is_ready = False
        if required_type == "ALL":
            is_ready = has_tech and has_func
        elif required_type == "TECHNICAL":
            is_ready = has_tech
        elif required_type == "FUNCTIONAL":
            is_ready = has_func

        # If files exist, set progress to 100, else keep it 0 (or estimate based on per-file if needed)
        progress = 100.0 if is_ready else 0.0

        return {
            "project_id": project_id,
            "status": "COMPLETED" if is_ready else "PROCESSING",
            "mode_requested": required_type,
            "progress": progress,
            "artifacts": {
                # Return the direct paths to the consolidated files
                "technical_path": str(tech_pdf) if has_tech else None,
                "functional_path": str(func_pdf) if has_func else None
            }
        }

    def _generate_consolidated_placeholders(self, project_id: uuid.UUID, mode: str):
        """Creates dummy consolidated PDF files based on the mode using FPDF2."""
        base = self.artifacts_path / str(project_id)
        base.mkdir(parents=True, exist_ok=True)

        def create_pdf(filename: str, title: str):
            pdf = FPDF()
            pdf.add_page()
            
            # Title
            pdf.set_font("helvetica", "B", 16)
            pdf.cell(0, 10, f"Document: {title}", new_x="LMARGIN", new_y="NEXT", align="C")
            
            # Metadata
            pdf.set_font("helvetica", "", 12)
            pdf.ln(10) # Line break
            pdf.cell(0, 10, f"Project ID: {project_id}", new_x="LMARGIN", new_y="NEXT")
            pdf.cell(0, 10, "Status: Placeholder for Consolidated Documentation", new_x="LMARGIN", new_y="NEXT")
            pdf.cell(0, 10, "Generated by: Agentic Documentation Pipeline", new_x="LMARGIN", new_y="NEXT")
            
            # Save
            path = base / filename
            pdf.output(str(path))
            logger.info(f"📄 Created placeholder: {path}")

        # Logic based on mode
        if mode in ["ALL", "TECHNICAL"]:
            create_pdf("Technical_Specifications.pdf", "Technical Specifications")
        
        if mode in ["ALL", "FUNCTIONAL"]:
            create_pdf("Functional_Specifications.pdf", "Functional Specifications")