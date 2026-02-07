# import asyncio
# import json
# import uuid
# from pathlib import Path
# from typing import Dict, Any, List

# from langchain.tools import ToolRuntime, tool
# from app.core.tools.context import ProjectContext

# from fpdf import FPDF

# from loguru import logger
# from sqlalchemy.ext.asyncio import AsyncSession
# from sqlalchemy import select

# from app.config.settings import settings
# from app.config.llm_config import get_llm
# from app.db.repositories.source_file import SourceFileRepository
# from app.db.models.source_file import SourceFile
# from app.db.models.project import Project, ProjectStatus



# from app.services.dependency_extractor.extractors import (
#     extract_cobol_dependencies,
#     extract_jcl_dependencies,
#     extract_ca7_dependencies,
#     extract_assembly_dependencies,
#     extract_pli_dependencies,
#     extract_copybook_dependencies,
#     extract_rexx_dependencies,
#     extract_parmlib_dependencies,
#     extract_pli_copybook_dependencies
# )

# # Import the new components
# from .graph_engine import GraphAnalyzer
# from .agent import create_documentation_graph
# from ...api.schemas.doc_models import DocAgentState
# from app.core.tools.file_tools import view_file, grep_search
# from app.core.tools.artifact_tools import create_artifact_tools
# from langchain_core.messages import SystemMessage, HumanMessage, ToolMessage

# class DocumentationService:
#     """
#     Orchestrates the Agentic Documentation Pipeline using LangGraph.
#     Generates file-wise Technical and Functional PDFs in separate artifact folders.
#     """

#     def __init__(self, session: AsyncSession):
#         self.session = session
#         self.repository = SourceFileRepository(session)
#         self.artifacts_path = settings.get_artifacts_path()

#     async def _update_project_status(self, project_id: uuid.UUID, mode: str, status: ProjectStatus):
#         """Helper to update the project's documentation status in the DB."""
#         try:
#             # Fetch project
#             result = await self.session.execute(select(Project).where(Project.id == project_id))
#             project = result.scalar_one_or_none()
            
#             if project:
#                 if mode == "ALL":
#                     project.technical_document_status = status
#                     project.functional_document_status = status
#                 elif mode == "TECHNICAL":
#                     project.technical_document_status = status
#                 elif mode == "FUNCTIONAL":
#                     project.functional_document_status = status
                
#                 await self.session.commit()
#                 logger.info(f"Updated Project {project_id} status to {status} for mode {mode}")
#         except Exception as e:
#             logger.error(f"Failed to update project status: {e}")        

#     async def run_full_pipeline(self, project_id: uuid.UUID, mode: str = "ALL"):
#         """
#         Main entry point for the background task.
#         1. Builds the Global Graph.
#         2. Runs the Agent for every file in the project.
#         """
#         try:
#             logger.info(f"Starting Agentic Documentation Pipeline for Project: {project_id}")

#             await self._update_project_status(project_id, mode, ProjectStatus.IN_PROGRESS)
            
#             # 1. Load System-wide Context (Combined JSONs from all parsers)
#             system_data = await self._load_all_parsed_jsons(project_id)
#             if not system_data:
#                 logger.error(f"No parsed data found for {project_id}. Documentation aborted.")
#                 return

#             dependency_mapped_data = self._map_dependencies(system_data)
            
#             # 3. Initialize Graph Engine with the MAPPED data
#             analyzer = GraphAnalyzer(dependency_mapped_data)
#             logger.info(f"System Graph built with {analyzer.graph.number_of_nodes()} nodes.")

#             project_id_str = str(project_id)
    
            
#             class RuntimeShim:
#                 def __init__(self, pid):
#                     self.context = type('obj', (object,), {'project_id': pid})
        
#             shim = RuntimeShim(project_id_str)

#             # 2. Wrap the tools so the LLM only sees the 'Public' arguments
#             # We manually pass the shim into the tool's underlying function (.func)
            
#             @tool("view_file")
#             def agent_view_file(filepath: str, start_line: int, end_line: int) -> str:
#                 """View specific lines from a project file. Path format: 'type/filename'."""
#                 # .func accesses the original python function under the @tool decorator
#                 return view_file.func(filepath, start_line, end_line, runtime=shim)

#             @tool("grep_search")
#             def agent_grep_search(pattern: str, file_pattern: str = "*") -> str:
#                 """Search for a pattern in project files. Returns matching lines and line numbers."""
#                 return grep_search.func(pattern, file_pattern, runtime=shim)

#             # 3. Setup Agent Tools & LLM
#             artifact_tools = create_artifact_tools(project_id_str)
#             llm = get_llm()
#             tools = {
#                 "view_file": agent_view_file,
#                 "grep_search": agent_grep_search,
#                 "read_artifact": artifact_tools[1] # read_artifact
#                 # Additional tools (artifact_tools, search_docs) can be added here
#             }

#             # 4. Compile the LangGraph State Machine
#             agent_app = create_documentation_graph(llm, analyzer, tools)

#             # 5. Fetch all source files associated with the project
#             files = await self._get_project_files(project_id)
#             logger.info(f"Found {len(files)} files to document.")

#             # 6. Execute Agent for each file
#             # Note: For large projects, consider using a semaphore to limit concurrency
#             tasks = []
#             for file_record in files:
#                 tasks.append(
#                     self._process_single_file(agent_app, project_id, file_record, mode)
#                 )
            
#             await asyncio.gather(*tasks)

#             self._generate_consolidated_placeholders(project_id, mode)
#             await self._update_project_status(project_id, mode, ProjectStatus.COMPLETED)

#             logger.info(f"Full pipeline complete for project {project_id}")

#         except Exception as e:
#             logger.exception(f"Agentic Pipeline failed for project {project_id}: {e}")

#     async def _process_single_file(self, agent_app, project_id: uuid.UUID, file_record: SourceFile, mode: str):
#         """Invoke the LangGraph agent for one specific file record."""
#         try:
#             logger.info(f"Agent researching: {file_record.filename} ({file_record.file_type})")
            
#             initial_state: DocAgentState = {
#                 "project_id": str(project_id),
#                 "target_file": file_record.filename,
#                 "file_type": file_record.file_type.upper(),
#                 "generation_mode": mode,
#                 "iterations": 0, # Initialize
#                 "mermaid_graph": "", # To be populated by Research Node
#                 "code_snippets": "", # To be populated by Research Node
#                 "functional_json": {},
#                 "technical_json": {},
#                 "messages": []
#             }

#             config = {"recursion_limit": 50}

#             # Run the Graph
#             await agent_app.ainvoke(initial_state, config=config)
            
#         except Exception as e:
#             logger.error(f"Failed to document file {file_record.filename}: {e}")

#     async def _load_all_parsed_jsons(self, project_id: uuid.UUID) -> Dict[str, Any]:
#         """
#         Gathers parsed output from all file type subfolders.
#         Returns a map: {'COBOL': [...], 'JCL': [...]}
#         """
#         parsed_dir = self.artifacts_path / str(project_id) / "parsed_outputs"
#         system_data = {}

#         if not parsed_dir.exists():
#             return {}

#         for type_folder in parsed_dir.iterdir():
#             if type_folder.is_dir():
#                 file_type = type_folder.name.upper()
#                 consolidated = type_folder / "_consolidated.json"
#                 if consolidated.exists():
#                     with open(consolidated, "r", encoding="utf-8") as f:
#                         data = json.load(f)
#                         system_data[file_type] = data
        
#         return system_data
    
#     def _map_dependencies(self, raw_data: Dict[str, List[Dict]]) -> Dict[str, Dict]:
#         """
#         Runs the extraction strategies to turn raw parsed lists 
#         into dictionaries of relationships for the GraphAnalyzer.
#         """
#         mapped = {}
        
#         # Strategy mapping table
#         extract_strategies = {
#             'COBOL': extract_cobol_dependencies,
#             'JCL': extract_jcl_dependencies,
#             'CA7': extract_ca7_dependencies,
#             'ASSEMBLY': extract_assembly_dependencies,
#             'PLI': extract_pli_dependencies,
#             'COPYBOOK': extract_copybook_dependencies,
#             'PLI_COPYBOOK': extract_pli_copybook_dependencies,
#             'REXX' : extract_rexx_dependencies,
#             'PARMLIB' : extract_parmlib_dependencies
#         }

#         for file_type, extractor_func in extract_strategies.items():
#             if file_type in raw_data:
#                 # We pass the raw list to the extractor function
#                 # The result is the dict like {"program_calls": [], ...}
#                 mapped[file_type] = extractor_func(raw_data[file_type])
#             else:
#                 # Default empty structure if type is missing
#                 mapped[file_type] = {}

#         return mapped

#     async def _get_project_files(self, project_id: uuid.UUID) -> List[SourceFile]:
#         """Queries the DB for all source files in this project."""
#         stmt = select(SourceFile).where(SourceFile.project_id == project_id)
#         result = await self.session.execute(stmt)
#         return list(result.scalars().all())

#     # async def get_job_status(self, project_id: uuid.UUID, required_type: str = "ALL") -> Dict:
#     #     """
#     #     Checks for folder existence based on what was requested.
#     #     """
#     #     base = self.artifacts_path / str(project_id)
#     #     tech_dir = base / "technical_requirements"
#     #     func_dir = base / "functional_requirements"
        
#     #     has_tech = tech_dir.exists() and any(tech_dir.iterdir())
#     #     has_func = func_dir.exists() and any(func_dir.iterdir())
        
#     #     is_ready = False
#     #     if required_type == "ALL":
#     #         is_ready = has_tech and has_func
#     #     elif required_type == "TECHNICAL":
#     #         is_ready = has_tech
#     #     elif required_type == "FUNCTIONAL":
#     #         is_ready = has_func

#     #     return {
#     #         "project_id": project_id,
#     #         "status": "COMPLETED" if is_ready else "PROCESSING",
#     #         "mode_requested": required_type,
#     #         "artifacts": {
#     #             "technical_path": str(tech_dir) if has_tech else None,
#     #             "functional_path": str(func_dir) if has_func else None
#     #         }
#     #     }

#     async def get_job_status(self, project_id: uuid.UUID, required_type: str = "ALL") -> Dict:
#         """
#         Checks for the existence of the CONSOLIDATED PDF files.
#         """
#         base = self.artifacts_path / str(project_id)
        
#         # Define paths
#         tech_pdf = base / "Technical_Specifications.pdf"
#         func_pdf = base / "Functional_Specifications.pdf"
        
#         has_tech = tech_pdf.exists()
#         has_func = func_pdf.exists()
        
#         # Determine status
#         is_ready = False
#         if required_type == "ALL":
#             is_ready = has_tech and has_func
#         elif required_type == "TECHNICAL":
#             is_ready = has_tech
#         elif required_type == "FUNCTIONAL":
#             is_ready = has_func

#         # If files exist, set progress to 100, else keep it 0 (or estimate based on per-file if needed)
#         progress = 100.0 if is_ready else 0.0

#         return {
#             "project_id": project_id,
#             "status": "COMPLETED" if is_ready else "PROCESSING",
#             "mode_requested": required_type,
#             "progress": progress,
#             "artifacts": {
#                 # Return the direct paths to the consolidated files
#                 "technical_path": str(tech_pdf) if has_tech else None,
#                 "functional_path": str(func_pdf) if has_func else None
#             }
#         }

#     def _generate_consolidated_placeholders(self, project_id: uuid.UUID, mode: str):
#         """Creates dummy consolidated PDF files based on the mode using FPDF2."""
#         base = self.artifacts_path / str(project_id)
#         base.mkdir(parents=True, exist_ok=True)

#         def create_pdf(filename: str, title: str):
#             pdf = FPDF()
#             pdf.add_page()
            
#             # Title
#             pdf.set_font("helvetica", "B", 16)
#             pdf.cell(0, 10, f"Document: {title}", new_x="LMARGIN", new_y="NEXT", align="C")
            
#             # Metadata
#             pdf.set_font("helvetica", "", 12)
#             pdf.ln(10) # Line break
#             pdf.cell(0, 10, f"Project ID: {project_id}", new_x="LMARGIN", new_y="NEXT")
#             pdf.cell(0, 10, "Status: Placeholder for Consolidated Documentation", new_x="LMARGIN", new_y="NEXT")
#             pdf.cell(0, 10, "Generated by: Agentic Documentation Pipeline", new_x="LMARGIN", new_y="NEXT")
            
#             # Save
#             path = base / filename
#             pdf.output(str(path))
#             logger.info(f"📄 Created placeholder: {path}")

#         # Logic based on mode
#         if mode in ["ALL", "TECHNICAL"]:
#             create_pdf("Technical_Specifications.pdf", "Technical Specifications")
        
#         if mode in ["ALL", "FUNCTIONAL"]:
#             create_pdf("Functional_Specifications.pdf", "Functional Specifications")

import asyncio
import json
import uuid
from pathlib import Path
from typing import Dict, Any, List

from langchain.tools import ToolRuntime, tool
from fpdf import FPDF
from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select

from app.config.settings import settings
from app.config.llm import get_llm, DOCGEN, LLMModel
from app.db.repositories.source_file import SourceFileRepository
from app.db.models.source_file import SourceFile
from app.db.models.project import Project, ProjectStatus

# --- Import Models & Extractors ---
from app.api.schemas.doc_models import DocAgentState, FileSummary, SystemMetrics
from app.services.dependency_extractor.extractors import (
    extract_cobol_dependencies, extract_jcl_dependencies, extract_ca7_dependencies,
    extract_assembly_dependencies, extract_pli_dependencies, extract_copybook_dependencies,
    extract_rexx_dependencies, extract_parmlib_dependencies, extract_pli_copybook_dependencies
)
from app.services.parser import ParserService

from .graph_engine import GraphAnalyzer
from .agent import create_documentation_graph
from app.core.tools.file_tools import view_file, grep_search
from app.core.tools.artifact_tools import create_artifact_tools

# --- NEW: Import the Real Builders ---
from .renderers import TechnicalSpecBuilder, FunctionalSpecBuilder


class DocumentationService:
    """
    Orchestrates the Agentic Documentation Pipeline using LangGraph.
    Generates Master Technical and Functional Specifications.
    """

    def __init__(self, session: AsyncSession):
        self.session = session
        self.repository = SourceFileRepository(session)
        self.artifacts_path = settings.get_artifacts_path()
        self.parser_service = ParserService(session)

    async def _update_project_status(self, project_id: uuid.UUID, mode: str, status: ProjectStatus):
        """Helper to update the project's documentation status in the DB."""
        try:
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
    
    async def _generate_system_summary(self, metrics: SystemMetrics, top_modules_summaries: List[FileSummary]) -> Dict:
        """
        Calls the LLM *ONCE* to generate the Executive Summary / System Overview.
        This populates the Introduction, Glossary, and Swim Lane Diagram.
        """
        try:
            # Prepare context for LLM
            module_context = "\n".join([
                f"- {m.filename} ({m.file_type}): {m.business_overview.get('purpose', 'N/A')}" 
                for m in top_modules_summaries
            ])
            
            # Import Prompt
            from app.services.documentation_services.prompts import EXECUTIVE_SUMMARY_PROMPT
            
            prompt = EXECUTIVE_SUMMARY_PROMPT.format(
                JSON_FORMAT_INSTRUCTION="", # Already in string
                metrics=metrics.model_dump_json(),
                top_modules=module_context
            )
            
            # Call LLM (using a simple invoke, not the agent graph)
            llm = get_llm(DOCGEN, model=LLMModel.GPT4O_DEV)
            from langchain_core.messages import HumanMessage
            response = await llm.ainvoke([HumanMessage(content=prompt)])
            
            # Parse JSON
            import json
            clean_json = response.content.replace("```json", "").replace("```", "").strip()
            return json.loads(clean_json)
            
        except Exception as e:
            logger.error(f"Failed to generate system summary: {e}")
            return {}

    async def run_full_pipeline(self, project_id: uuid.UUID, mode: str = "ALL"):
        """
        Main entry point for the background task.
        """
        try:
            logger.info(f"Starting {mode} Agentic Documentation Pipeline for Project: {project_id}")
            await self._update_project_status(project_id, mode, ProjectStatus.IN_PROGRESS)
            
            # 1. Load System-wide Context
            system_data = await self._load_all_parsed_jsons(project_id)
            
            if not system_data:
                logger.info("Parsing data not found. Triggering Parser Service...")
                await self.parser_service.parse_project(project_id)
                
                system_data = await self._load_all_parsed_jsons(project_id)
                
                if not system_data:
                    logger.error(f"Parsing completed but no data found for {project_id}. Aborting.")
                    await self._update_project_status(project_id, mode, ProjectStatus.FAILED)
                    return

            dependency_mapped_data = self._map_dependencies(system_data)
            
            # 2. Initialize Graph Engine
            analyzer = GraphAnalyzer(dependency_mapped_data)
            logger.info(f"System Graph built with {analyzer.graph.number_of_nodes()} nodes.")

            # 3. Setup Agent Tools
            project_id_str = str(project_id)
            class RuntimeShim:
                def __init__(self, pid): self.context = type('obj', (object,), {'project_id': pid})
            shim = RuntimeShim(project_id_str)
            
            @tool("view_file")
            def agent_view_file(filepath: str, start_line: int, end_line: int) -> str:
                """
                View specific lines from a project file to read code or content.
                
                Args:
                    filepath: The relative path to the file (e.g. 'COBOL/PROG01.cbl')
                    start_line: The first line number to read (1-based)
                    end_line: The last line number to read
                """
                # We access .func to bypass the original tool's runtime requirement logic here
                return view_file.func(filepath, start_line, end_line, runtime=shim)

            @tool("grep_search")
            def agent_grep_search(pattern: str, file_pattern: str = "*") -> str:
                """
                Search for a specific text pattern across project files.
                
                Args:
                    pattern: The regex or text to search for
                    file_pattern: Optional glob pattern to filter files (e.g. '*.cbl')
                """
                return grep_search.func(pattern, file_pattern, runtime=shim)

            artifact_tools = create_artifact_tools(project_id_str)
            llm = get_llm(DOCGEN, model=LLMModel.GPT4_1_MINI)
            tools = {
                "view_file": agent_view_file,
                "grep_search": agent_grep_search,
                "read_artifact": artifact_tools[1]
            }

            # 4. Compile Agent Graph
            agent_app = create_documentation_graph(llm, analyzer, tools)

            # 5. Fetch Files
            files = await self._get_project_files(project_id)
            logger.info(f"Found {len(files)} files to document.")

            # 6. Execute Agent (Loop)
            tasks = []
            results = [] # To store the state results
            
            sem = asyncio.Semaphore(15)

            async def _bounded_process(file_record):
                async with sem:
                    return await self._process_single_file(agent_app, project_id, file_record, mode)

            for file_record in files:
                tasks.append(_bounded_process(file_record))
            
            completed_states = await asyncio.gather(*tasks)

            # 7. Collect Summaries for Master Doc
            all_summaries = []
            for state in completed_states:
                if not state: continue
                # Merge functional/technical json (they are identical now)
                raw_json = state.get("technical_json") or state.get("functional_json")
                if raw_json:
                    try:
                        summary = FileSummary(**raw_json)
                        all_summaries.append(summary)
                    except Exception as e:
                        logger.warning(f"Failed to parse summary for {state.get('target_file')}: {e}")

            # 8. Calculate Metrics
            type_counts = {}
            for s in all_summaries:
                type_counts[s.file_type] = type_counts.get(s.file_type, 0) + 1
            metrics = analyzer.get_metrics(len(all_summaries), type_counts)

            # Call the LLM to get the combined system overview
            system_overview_json = await self._generate_system_summary(metrics, all_summaries)

            # 9. Generate Graph Image (Needed for PDF)
            output_dir = self.artifacts_path / str(project_id)
            output_dir.mkdir(parents=True, exist_ok=True)
            
            image_paths = {}
            success = None

            # A. Technical Architecture Diagram (2.3)
            arch_path = output_dir / "architecture.png"
            if analyzer.generate_mermaid_png(str(arch_path)):
                image_paths['architecture'] = str(arch_path)
                success = str(arch_path)

            # B. System Context Diagram (1.4)
            ctx_path = output_dir / "context.png"
            ctx_mermaid = analyzer.generate_context_diagram()
            if analyzer.render_mermaid_code_to_png(ctx_mermaid, str(ctx_path)):
                image_paths['context'] = str(ctx_path)
                success = str(arch_path)

            # C. Functional Sequence Diagram (Functional 3.1)
            # Extracted from the LLM response in system_overview_json
            func_mermaid = system_overview_json.get('functional_flow_diagram', {}).get('mermaid_code')
            if func_mermaid:
                func_path = output_dir / "functional_flow.png"
                if analyzer.render_mermaid_code_to_png(func_mermaid, str(func_path)):
                    image_paths['functional'] = str(func_path)
                    success = str(arch_path)

            if success:
                logger.info(f"Successfully generated graph image at {success}")
            else:
                logger.warning("Failed to generate graph image via Mermaid API")

            # 10. Build Real PDFs with the Images dict
            if mode in ["ALL", "TECHNICAL"]:
                logger.info("Building Technical Specification...")
                tech_builder = TechnicalSpecBuilder(
                    all_summaries, metrics, analyzer, 
                    system_summary=system_overview_json,
                    images=image_paths # Passing the dict with 3 images
                )
                tech_builder.build()
                tech_builder.save(str(output_dir / "Technical_Specifications.pdf"))

            if mode in ["ALL", "FUNCTIONAL"]:
                logger.info("Building Functional Specification...")
                func_builder = FunctionalSpecBuilder(
                    all_summaries, metrics, analyzer, 
                    system_summary=system_overview_json,
                    images=image_paths # Passing the dict with 3 images
                )
                func_builder.build()
                func_builder.save(str(output_dir / "Functional_Specifications.pdf"))

            # Fallback: keep your placeholder logic if needed, but the above replaces it.
            # self._generate_consolidated_placeholders(project_id, mode)

            await self._update_project_status(project_id, mode, ProjectStatus.COMPLETED)
            logger.info(f"{mode} pipeline complete for project {project_id}")

        except Exception as e:
            logger.exception(f"Agentic Pipeline failed for project {project_id}: {e}")
            await self._update_project_status(project_id, mode, ProjectStatus.FAILED)

    async def _process_single_file(self, agent_app, project_id: uuid.UUID, file_record: SourceFile, mode: str) -> Dict:
        """Invoke the LangGraph agent and return the final state."""
        try:
            logger.info(f"Processing: {file_record.filename}")
            
            initial_state: DocAgentState = {
                "project_id": str(project_id),
                "target_file": file_record.filename,
                "file_type": file_record.file_type.upper(),
                "generation_mode": mode,
                "iterations": 0, 
                "research_complete": False,
                "mermaid_graph": "", 
                "code_snippets": "", 
                "functional_json": {},
                "technical_json": {},
                "messages": []
            }

            config = {"recursion_limit": 50}
            
            # Return the final state so we can harvest the JSON
            final_state = await agent_app.ainvoke(initial_state, config=config)
            return final_state
            
        except Exception as e:
            logger.error(f"Failed to document file {file_record.filename}: {e}")
            return {}

    async def _load_all_parsed_jsons(self, project_id: uuid.UUID) -> Dict[str, Any]:
        parsed_dir = self.artifacts_path / str(project_id) / "parsed_outputs"
        system_data = {}
        if not parsed_dir.exists(): return {}
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
        mapped = {}
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
                mapped[file_type] = extractor_func(raw_data[file_type])
            else:
                mapped[file_type] = {}
        return mapped

    async def _get_project_files(self, project_id: uuid.UUID) -> List[SourceFile]:
        stmt = select(SourceFile).where(SourceFile.project_id == project_id)
        result = await self.session.execute(stmt)
        return list(result.scalars().all())

    async def get_job_status(self, project_id: uuid.UUID, required_type: str = "ALL") -> Dict:
        base = self.artifacts_path / str(project_id)
        tech_pdf = base / "Technical_Specifications.pdf"
        func_pdf = base / "Functional_Specifications.pdf"
        
        has_tech = tech_pdf.exists()
        has_func = func_pdf.exists()
        
        is_ready = False
        if required_type == "ALL":
            is_ready = has_tech and has_func
        elif required_type == "TECHNICAL":
            is_ready = has_tech
        elif required_type == "FUNCTIONAL":
            is_ready = has_func

        progress = 100.0 if is_ready else 0.0

        return {
            "project_id": project_id,
            "status": "COMPLETED" if is_ready else "PROCESSING",
            "mode_requested": required_type,
            "progress": progress,
            "artifacts": {
                "technical_path": str(tech_pdf) if has_tech else None,
                "functional_path": str(func_pdf) if has_func else None
            }
        }

    def _generate_consolidated_placeholders(self, project_id: uuid.UUID, mode: str):
        """Keep this as a fallback or for testing."""
        base = self.artifacts_path / str(project_id)
        base.mkdir(parents=True, exist_ok=True)
        def create_pdf(filename: str, title: str):
            pdf = FPDF()
            pdf.add_page()
            pdf.set_font("helvetica", "B", 16)
            pdf.cell(0, 10, f"Document: {title}", new_x="LMARGIN", new_y="NEXT", align="C")
            path = base / filename
            pdf.output(str(path))
        if mode in ["ALL", "TECHNICAL"]: create_pdf("Technical_Specifications.pdf", "Technical Specifications")
        if mode in ["ALL", "FUNCTIONAL"]: create_pdf("Functional_Specifications.pdf", "Functional Specifications")