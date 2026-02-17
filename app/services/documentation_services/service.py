import asyncio
import json
import uuid
from pathlib import Path
from typing import Dict, Any, List
import re

from langchain.tools import ToolRuntime, tool
from fpdf import FPDF
from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select

from app.config.settings import settings
from app.config.llm_config import get_llm
from app.db.repositories.source_file import SourceFileRepository
from app.db.models.source_file import SourceFile
from app.db.models.project import Project, ProjectStatus

from app.api.schemas.doc_models import DocAgentState, FileSummary, SystemMetrics
from app.services.dependency_extractor.extractors import (
    extract_cobol_dependencies, extract_jcl_dependencies, extract_ca7_dependencies,
    extract_assembly_dependencies, extract_pli_dependencies, extract_copybook_dependencies,
    extract_rexx_dependencies, extract_parmlib_dependencies, extract_pli_copybook_dependencies
)
from app.services.parser import ParserService
from app.services.documentation_services.job_spec_builder import SingleJCLReportBuilder

from .graph_engine import GraphAnalyzer
from .agent import create_documentation_graph
from app.core.tools.file_tools import view_file, grep_search
from app.core.tools.artifact_tools import create_artifact_tools

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
    
    async def _generate_system_summary(
        self, 
        metrics: SystemMetrics, 
        top_modules_summaries: List[FileSummary],
        harvested_data: Dict[str, Any] = None # <--- Added this argument
    ) -> Dict:
        """
        Calls the LLM to generate the Executive Summary using metrics and harvested data.
        """
        try:
            # Prepare context for LLM
            module_context = "\n".join([
                f"- {m.filename} ({m.file_type}): {m.business_overview.get('purpose', 'N/A')}" 
                for m in top_modules_summaries
            ])
            
            # Add harvested insights to the context
            harvested_context = ""
            if harvested_data:
                harvested_context = (
                    f"\n\nADDITIONAL DISCOVERED DATA:\n"
                    f"- Total Reports Found: {harvested_data.get('total_reports')}\n"
                    f"- Total Interfaces Found: {harvested_data.get('total_interfaces')}\n"
                    f"- Sample Business Rules: {json.dumps(harvested_data.get('business_rules_sample', []))}\n"
                    f"- Known Acronyms: {json.dumps(harvested_data.get('acronyms', []))}"
                )

            # Import Prompt
            from app.services.documentation_services.prompts import EXECUTIVE_SUMMARY_PROMPT
            
            prompt = EXECUTIVE_SUMMARY_PROMPT.format(
                JSON_FORMAT_INSTRUCTION="", 
                metrics=metrics.model_dump_json(),
                top_modules=module_context + harvested_context # Append harvested info
            )
            
            # Call LLM
            llm = get_llm()
            from langchain_core.messages import HumanMessage
            response = await llm.ainvoke([HumanMessage(content=prompt)])
            
            # Parse JSON
            clean_json = response.content.replace("```json", "").replace("```", "").strip()
            try:
                return json.loads(clean_json)
            except json.JSONDecodeError:
                logger.warning("JSON truncated or invalid")
            
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
            llm = get_llm()
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
            
            sem = asyncio.Semaphore(5)

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
                # Merge functional/technical json
                raw_json = state.get("technical_json") or state.get("functional_json")
                if raw_json:
                    try:
                        summary = FileSummary(**raw_json)
                        all_summaries.append(summary)
                    except Exception as e:
                        logger.warning(f"Failed to parse summary for {state.get('target_file')}: {e}")

            # --- FIX 1: Calculate Metrics (This defines 'metrics') ---
            type_counts = {}
            for s in all_summaries:
                t = s.file_type.upper()
                type_counts[t] = type_counts.get(t, 0) + 1
            
            # Now 'metrics' is defined and ready to use
            metrics = analyzer.get_metrics(len(all_summaries), type_counts)

            # --- Data Harvesting ---
            all_business_rules = []
            all_acronyms = []
            all_reports = []
            all_interfaces = []
            
            for s in all_summaries:
                # Harvest Rules
                rules = s.business_overview.get('scope', [])
                if rules: all_business_rules.extend(rules)
                
                # Harvest Acronyms
                glossary_items = s.business_overview.get('glossary', [])
                if glossary_items: all_acronyms.extend(glossary_items)
                
                # Harvest Reports & Interfaces from JCL
                if s.file_type == 'JCL':
                    io = s.technical_analysis.get('io_datasets', [])
                    for ds in io:
                        if isinstance(ds, dict):
                            name = str(ds.get('dataset', '')).upper()
                            usage = str(ds.get('usage', '')).upper()
                            if 'SYSOUT' in name or '.RPT' in name or '.LIST' in name:
                                all_reports.append({"job": s.filename, "report": name})
                            if 'OLD' in usage or 'NEW' in usage:
                                if 'TEMP' not in name:
                                    all_interfaces.append({"job": s.filename, "dsn": name, "type": usage})

            # Deduplicate Harvested Data
            # Convert dictionary items back to list of dicts for the LLM
            unique_acronyms_dict = {g['term']: g['definition'] for g in all_acronyms if isinstance(g, dict)}
            unique_acronyms_list = [{"term": k, "definition": v} for k, v in unique_acronyms_dict.items()]
            
            unique_rules = list(set(all_business_rules))
            
            # --- Representative Sampling ---
            categories = ['Transaction Processing', 'Reporting', 'Data Maintenance', 'Utility']
            representative_summaries = []

            data_logic_files = [s for s in all_summaries if s.file_type in ['DCLGEN', 'COPYBOOK']]
            representative_summaries.extend(data_logic_files[:10])

            for cat in categories:
                cat_progs = [s for s in all_summaries if s.business_overview.get('functional_category') == cat]
                representative_summaries.extend(cat_progs[:5])

            # 8. Generate System Overview
            # FIX 2: Pass 'unique_rules' into harvested_data so it's used
            system_overview_json = await self._generate_system_summary(
                metrics, 
                representative_summaries,
                harvested_data={
                    "acronyms": unique_acronyms_list[:30],
                    "business_rules_sample": unique_rules[:50],
                    "total_reports": len(all_reports),
                    "total_interfaces": len(all_interfaces)
                }
            )

            # Call the new hierarchical JSON extractor
            hierarchical_data = await analyzer.generate_hierarchical_structure_json()

            # You can now log them exactly as you wanted
            logger.info(f"The arch json : {hierarchical_data.get('data_flow_architecture')}")
            logger.info(f"The process flow json : {hierarchical_data.get('process_flow_specification')}")

            system_overview_json.update(hierarchical_data)

            # SAFETY CHECK: Ensure system_overview_json is a dict and not None
            if not isinstance(system_overview_json, dict):
                logger.error("System summary returned non-dictionary. Using empty defaults.")
                system_overview_json = {}
            
            # Inject harvested lists into the summary for the Renderer to use directly
            system_overview_json["harvested_reports"] = all_reports
            system_overview_json["harvested_interfaces"] = all_interfaces
            system_overview_json["harvested_acronyms"] = unique_acronyms_list

            # 9. Generate Graph Image (Needed for PDF)
            output_dir = self.artifacts_path / str(project_id)
            output_dir.mkdir(parents=True, exist_ok=True)
            image_paths = {}

            # Generate the actual JPG files from the JSON
            if system_overview_json.get("data_flow_architecture"):
                arch_img = output_dir / "hierarchical_architecture.jpg"
                if analyzer.generate_visual_graph_from_json(system_overview_json["data_flow_architecture"], str(arch_img)):
                    image_paths['hierarchical_arch_link'] = str(arch_img)
                    logger.info("the architecture diagram generated")

            if system_overview_json.get("process_flow_specification"):
                proc_img = output_dir / "hierarchical_process.jpg"
                if analyzer.generate_visual_graph_from_json(system_overview_json["process_flow_specification"], str(proc_img)):
                    image_paths['hierarchical_proc_link'] = str(proc_img)
                    logger.info("the process flow diagram generated")
            
    
            success = None

            # A. Technical Architecture Diagram (2.3)
            # arch_path = output_dir / "architecture.png"
            # if analyzer.generate_mermaid_png(str(arch_path)):
            #     image_paths['architecture'] = str(arch_path)
            #     success = str(arch_path)
            
            # if success:
            #     logger.info(f"Successfully generated graph image at {success}")
            #     success = None
            # else:
            #     logger.warning("Failed to generate arch graph image via Mermaid API")

            # B. System Context Diagram (1.4)
            ctx_path = output_dir / "context.png"
            ctx_mermaid = analyzer.generate_context_diagram()
            if analyzer.render_mermaid_code_to_png(ctx_mermaid, str(ctx_path)):
                image_paths['context'] = str(ctx_path)
                success = str(ctx_path)
            
            if success:
                logger.info(f"Successfully generated graph image at {success}")
                success = None
            else:
                logger.warning("Failed to generate context graph image via Mermaid API")
            
            # #C process flow diagram
            # process_flow_path = output_dir / "process_flow.png"
            # # Logic: Ask analyzer to generate a JCL-only flow (Job A -> Job B)
            # process_mermaid = analyzer.generate_process_flow_diagram()

            # if analyzer.render_mermaid_code_to_png(process_mermaid, str(process_flow_path)):
            #     image_paths['process_flow'] = str(process_flow_path)
            #     success = str(process_flow_path)
            
            # if success:
            #     logger.info(f"Successfully generated graph image at {success}")
            #     success = None 
            # else:
            #     logger.warning("Failed to generate process graph image via Mermaid API")

            # C. Functional Sequence Diagram (Functional 3.1)
            # Extracted from the LLM response in system_overview_json
            func_mermaid = system_overview_json.get('functional_flow_diagram', {}).get('mermaid_code')
            if func_mermaid:
                clean_mermaid = func_mermaid.replace("```mermaid", "").replace("```", "").strip()
                if "sequenceDiagram" in clean_mermaid:

                    start_index = clean_mermaid.find("sequenceDiagram")
                    clean_mermaid = clean_mermaid[start_index:]
                else:
                    # Fallback if missing header
                    clean_mermaid = "sequenceDiagram\n" + clean_mermaid

                func_path = output_dir / "functional_flow.png"
                if analyzer.render_mermaid_code_to_png(clean_mermaid, str(func_path)):
                    image_paths['functional'] = str(func_path)
                    success = str(func_path)

            if success:
                logger.info(f"Successfully generated graph image at {success}")
            else:
                logger.warning("Failed to generate functional graph image via Mermaid API")
          
            # D. Batch Flow Diagram (NEW)
            # Uses the list of summaries to find JCL-to-JCL links
            batch_mermaid = analyzer.generate_batch_flow_diagram(all_summaries)
            
            if batch_mermaid:
                batch_path = output_dir / "batch_flow.png"
                if analyzer.render_mermaid_code_to_png(batch_mermaid, str(batch_path)):
                    image_paths['batch_flow'] = str(batch_path)
                    logger.info(f"Successfully generated batch flow image at {batch_path}")
                else:
                    logger.info(f"Failed generating batch flow image")
            else:
                logger.info(f"Batch Mermaid is missing")

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

    async def generate_single_jcl_runbook(self, project_id: uuid.UUID, jcl_filename: str) -> str:
        """
        Generates a targeted 'Runbook' for a specific JCL file.
        This mirrors 'run_full_pipeline' to ensure 100% data availability.
        """
        try:
            logger.info(f"Starting Runbook Generation for {jcl_filename}")

            # 1. Load System-wide Context (Exactly like run_full_pipeline)
            system_data = await self._load_all_parsed_jsons(project_id)
            if not system_data:
                await self.parser_service.parse_project(project_id)
                system_data = await self._load_all_parsed_jsons(project_id)

            # 2. Initialize Graph Engine (Uses the fixed _map_dependencies)
            dependency_mapped_data = self._map_dependencies(system_data)
            analyzer = GraphAnalyzer(dependency_mapped_data)

            # 3. Setup Agent Tools
            project_id_str = str(project_id)
            class RuntimeShim:
                def __init__(self, pid): self.context = type('obj', (object,), {'project_id': pid})
            shim = RuntimeShim(project_id_str)
            
            @tool("view_file")
            def agent_view_file(filepath: str, start_line: int, end_line: int) -> str:
                """Read lines from a source file."""
                return view_file.func(filepath, start_line, end_line, runtime=shim)

            @tool("grep_search")
            def agent_grep_search(pattern: str, file_pattern: str = "*") -> str:
                """Search for text patterns in files."""
                return grep_search.func(pattern, file_pattern, runtime=shim)

            tools = {"view_file": agent_view_file, "grep_search": agent_grep_search}
            llm = get_llm()
            agent_app = create_documentation_graph(llm, analyzer, tools)

            # 4. Fetch ALL Project Files (No filtering, matches run_full_pipeline)
            all_files = await self._get_project_files(project_id)
            total_files = len(all_files)
            logger.info(f"Runbook Pipeline: Analyzing all {total_files} files for full context.")

            # 5. Execute Agent Loop with Progress Logging
            tasks = []
            sem = asyncio.Semaphore(8) # Concurrency

            async def _bounded_process(file_record, idx):
                async with sem:
                    # PROGRESS LOGGING
                    logger.info(f"Analyzing File [{idx}/{total_files}]: {file_record.filename}")
                    return await self._process_single_file(agent_app, project_id, file_record, "ALL")

            for i, file_record in enumerate(all_files, 1):
                tasks.append(_bounded_process(file_record, i))
            
            completed_states = await asyncio.gather(*tasks)

            # 6. Collect All Summaries
            all_summaries = []
            for state in completed_states:
                if not state: continue
                raw_json = state.get("technical_json") or state.get("functional_json")
                if raw_json:
                    raw_json['filename'] = state['target_file']
                    raw_json['type'] = state['file_type']
                    all_summaries.append(FileSummary(**raw_json))

            # 7. Identify the Target JCL Summary
            clean_target = jcl_filename.upper().replace('.JCL', '').replace('.TXT', '').strip()
            target_summary = next((
                s for s in all_summaries 
                if s.filename.upper().replace('.JCL', '').replace('.TXT', '').strip() == clean_target
            ), None)

            if not target_summary:
                raise ValueError(f"Target JCL '{jcl_filename}' summary missing after full analysis.")

            # 8. Generate System Metrics (Required by Builder)
            type_counts = {}
            for s in all_summaries:
                t = s.file_type.upper()
                type_counts[t] = type_counts.get(t, 0) + 1
            metrics = SystemMetrics(total_files=len(all_summaries), files_by_type=type_counts, top_complex_modules=[])

            # 9. Build the Runbook PDF
            output_dir = self.artifacts_path / str(project_id) / "runbooks"
            output_dir.mkdir(parents=True, exist_ok=True)
            report_path = output_dir / f"{clean_target}_Runbook.pdf"

            logger.info(f"Full analysis complete. Rendering Runbook PDF for {target_summary.filename}...")
            
            # Pass everything to the builder
            builder = SingleJCLReportBuilder(
                jcl_summary=target_summary,
                summaries=all_summaries, 
                metrics=metrics,
                graph_analyzer=analyzer,
                output_dir=output_dir
            )
            
            builder.build()
            builder.save(str(report_path))
            
            logger.info(f"Runbook generated: {report_path}")
            return str(report_path)

        except Exception as e:
            logger.exception(f"Runbook Pipeline failed: {e}")
            raise e