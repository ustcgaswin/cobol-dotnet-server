import asyncio
import json
import uuid
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Any, List

from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession

from app.config.settings import settings
from app.core.storage import FileStorage
from app.db.repositories.source_file import SourceFileRepository

from app.services.documentation_services.dependency_mapper import DependencyMapper
from .documentation_services.context_provider import ContextProvider
from .documentation_services.llm_manager import LLMManager
from .documentation_services.pipeline_runner import PipelineRunner
from .documentation_services.report_generator import ReportGenerator


@dataclass
class DocumentationJobStatus:
    """Status of a long-running documentation task."""
    project_id: uuid.UUID
    is_complete: bool
    progress: float
    files_processed: int
    total_files: int
    file_path: str | None = None


class DocumentationService:
    """
    Service for orchestrating the AI Documentation Pipeline.
    Links parsed JSON outputs into a Knowledge Graph and generates a Master Manual.
    """

    def __init__(self, session: AsyncSession):
        self.repository = SourceFileRepository(session)
        self.storage = FileStorage()
        self.artifacts_path = settings.get_artifacts_path()

    def _get_parsed_dir(self, project_id: uuid.UUID) -> Path:
        """Directory where ParserService saved the JSON files."""
        return self.artifacts_path / str(project_id) / "parsed_outputs"

    def _get_doc_dir(self, project_id: uuid.UUID) -> Path:
        """Directory where the Master Manual and Graph will be saved."""
        path = self.artifacts_path / str(project_id) / "documentation"
        path.mkdir(parents=True, exist_ok=True)
        return path

    async def run_full_pipeline(self, project_id: uuid.UUID, output_path: str):
        """
        The main Background Task for generating documentation.
        """
        try:
            logger.info(f"Initializing Pipeline for Project: {project_id}")
            
            # 1. Load all Parsed JSONs produced by ParserService
            parsed_data = await self._load_all_parsed_jsons(project_id)
            if not parsed_data:
                logger.error(f"No parsed data found for project {project_id}. Run parsers first.")
                return

            # 2. Build Dependency Graph (NetworkX)
            mapper = DependencyMapper()
            graph = mapper.build_map(parsed_data)
            logger.info(f"Graph built with {graph.number_of_nodes()} nodes.")

            # 3. Initialize Context and LLM Managers
            ctx_provider = ContextProvider(graph)
            # Assuming LLMManager handles the 10k line chunking logic we discussed
            llm_worker = LLMManager(ctx_provider) 

            # 4. Initialize Pipeline Runner
            # This runner handles the parallel calls and FAISS indexing
            doc_dir = self._get_doc_dir(project_id)
            runner = PipelineRunner(
                project_dir=str(doc_dir), # Root for artifacts
                output_dir=str(doc_dir)
            )
            
            # Inject the already built graph and workers
            runner.mapper = mapper
            runner.llm_worker = llm_worker

            # 5. Run Logic Analysis (The 'Map' Phase)
            # This iterates through COBOL/PLI files and uses LLM
            program_reports = {}
            logic_files = [n for n, d in graph.nodes(data=True) if d.get('type') in ['cobol', 'pli', 'assembly']]
            
            for i, node in enumerate(logic_files):
                logger.info(f"Documenting Logic [{i+1}/{len(logic_files)}]: {node}")
                report = await llm_worker.document_large_file(node) # Use await if your LLM manager is async
                program_reports[node] = report
                
                # Push snippets to FAISS index
                runner._index_summaries(report)

            # 6. Generate the Master Manual (The 'Reduce' Phase)
            generator = ReportGenerator(
                output_dir=str(doc_dir),
                mapper_graph=graph,
                llm_manager=llm_worker
            )
            master_manual_path = generator.create_consolidated_report(program_reports)

            logger.info(f"Master Manual created at: {master_manual_path}")

        except Exception as e:
            logger.exception(f"Pipeline failed for project {project_id}: {e}")

    async def _load_all_parsed_jsons(self, project_id: uuid.UUID) -> List[Dict]:
        """
        Reads the '_consolidated.json' or individual files from 
        the ParserService artifact folders.
        """
        parsed_dir = self._get_parsed_dir(project_id)
        all_json_data = []

        if not parsed_dir.exists():
            return []

        # Walk through all subfolders (cobol, jcl, copybook, etc.)
        for file_type_dir in parsed_dir.iterdir():
            if file_type_dir.is_dir():
                consolidated_file = file_type_dir / "_consolidated.json"
                if consolidated_file.exists():
                    with open(consolidated_file, "r", encoding="utf-8") as f:
                        data = json.load(f)
                        if isinstance(data, list):
                            all_json_data.extend(data)
                        else:
                            all_json_data.append(data)
        
        return all_json_data

    async def get_job_status(self, project_id: uuid.UUID) -> DocumentationJobStatus:
        """
        Check if the documentation exists and calculate progress.
        """
        doc_dir = self._get_doc_dir(project_id)
        master_file = doc_dir / "Master_Project_Manual.md"
        
        is_complete = master_file.exists()
        
        # This is a simplified progress check
        # In a real app, you might store this in a Redis key or DB table
        return DocumentationJobStatus(
            project_id=project_id,
            is_complete=is_complete,
            progress=100.0 if is_complete else 0.0,
            files_processed=0,
            total_files=0,
            file_path=str(master_file) if is_complete else None
        )

    async def get_report_metadata(self, project_id: uuid.UUID) -> Dict | None:
        """Returns the path and size of the generated report."""
        status = await self.get_job_status(project_id)
        if status.is_complete:
            path = Path(status.file_path)
            return {
                "filename": path.name,
                "path": status.file_path,
                "size_kb": path.stat().st_size / 1024,
                "last_updated": datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc).isoformat()
            }
        return None