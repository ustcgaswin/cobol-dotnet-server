"""System Analyst Service - async task runner."""

import asyncio
import json
from datetime import datetime
from pathlib import Path
from uuid import UUID, uuid4

from langchain.messages import HumanMessage
from loguru import logger

from app.config.settings import settings
from app.core.tools.artifact_tools import create_artifact_tools
from app.core.tools.rag_tools import search_docs
from app.services.analyst.agent import create_analyst_agent
from app.services.analyst.tools import create_knowledge_tools, create_writer_tools, create_file_tracker_tools
from app.db.base import async_session_factory
from app.services.summarizer.service import SummarizerService
from app.services.dependency_extractor.service import DependencyExtractorService
from app.services.parser import ParserService
from app.config.llm.tracing import trace_execution, trace_tool

class AnalystService:
    """Service for running the System Analyst Agent.
    
    Uses asyncio.create_task for non-blocking execution.
    Status is persisted to _status.json for resilience.
    """
    
    # In-memory task registry (class-level)
    _tasks: dict[str, asyncio.Task] = {}
    _statuses: dict[str, dict] = {}
    
    def __init__(self, project_id: UUID):
        self.project_id = project_id
        self.output_path = self._get_output_path()
    
    def _get_output_path(self) -> Path:
        """Get the system_context output directory."""
        base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
        return base / str(self.project_id) / "system_context"
    
    def _ensure_output_path(self) -> Path:
        """Ensure the output directory exists.
        
        Only call this when writing files.
        """
        output_dir = self._get_output_path()
        output_dir.mkdir(parents=True, exist_ok=True)
        return output_dir
    
    def _get_status_file(self) -> Path:
        """Get the status file path."""
        return self._get_output_path() / "_status.json"
    
    def _get_artifacts_path(self) -> Path:
        """Get the project artifacts path."""
        return Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / str(self.project_id)
    
    def _update_status(self, run_id: str, status: str, phase: str = "", error: str = "") -> None:
        """Update and persist status."""
        status_data = {
            "run_id": run_id,
            "project_id": str(self.project_id),
            "status": status,
            "phase": phase,
            "error": error,
            "updated_at": datetime.utcnow().isoformat(),
        }
        self._statuses[run_id] = status_data
        
        try:
            self._ensure_output_path()
            with open(self._get_status_file(), "w") as f:
                json.dump(status_data, f, indent=2)
        except Exception as e:
            logger.warning(f"Failed to persist status: {e}")
            
    async def ensure_prerequisites(self) -> None:
        """Ensure Phase A artifacts exist (file_summaries.md, dependency_graph.md).
        
        If missing, triggers the respective services to generate them.
        """
        artifacts_path = self._get_artifacts_path()
        
        parsed_outputs_path = artifacts_path / "parsed_outputs"
        
        # 1. Check/Run Parsers (Required for Dependency Extraction)
        # We check if the folder exists. A more robust check might look for content,
        # but existence is a good proxy for "has run at least once".
        if not parsed_outputs_path.exists():
            logger.info("parsed_outputs missing. Triggering ParserService...")
            async with async_session_factory() as session:
                parser = ParserService(session)
                await parser.parse_project(self.project_id)
        
        # 2. Check/Generate File Summaries
        if not (artifacts_path / "file_summaries.md").exists():
            logger.info("file_summaries.md missing. Triggering SummarizerService...")
            async with async_session_factory() as session:
                summarizer = SummarizerService(self.project_id, session)
                await summarizer.generate()
        
        # 3. Check/Generate Dependency Graph
        if not (artifacts_path / "dependency_graph.md").exists():
            logger.info("dependency_graph.md missing. Triggering DependencyExtractorService...")
            extractor = DependencyExtractorService(self.project_id)
            await extractor.generate()
    
    async def run(self) -> str:
        """Start the analyst agent asynchronously.
        
        Returns:
            run_id for tracking
        """
        run_id = str(uuid4())
        
        task = asyncio.create_task(self._execute(run_id))
        self._tasks[run_id] = task
        
        logger.info(f"Started analyst run: {run_id} for project {self.project_id}")
        return run_id
    
    async def _execute(self, run_id: str) -> None:
        """Execute the analyst agent."""
        try:
            self._update_status(run_id, "running", phase="initializing")
            
            # Ensure prerequisites (Self-Healing)
            await self.ensure_prerequisites()
            
            project_id_str = str(self.project_id)
            
            # Auto-initialize tracker (ensures folder and tracker existence)
            from app.services.analyst.tools import _initialize_tracker_logic
            _initialize_tracker_logic(project_id_str)
            
            # Create tools with project_id bound
            artifact_tools = create_artifact_tools(project_id_str)
            writer_tools = create_writer_tools(project_id_str)
            knowledge_tools = create_knowledge_tools()
            file_tracker_tools = create_file_tracker_tools(project_id_str)
            rag_tools = [search_docs]
            all_tools = artifact_tools + writer_tools + knowledge_tools + file_tracker_tools + rag_tools
            
            # Wrap tools with tracing
            all_tools = [trace_tool(t) for t in all_tools]
            
            # Create agent
            agent = create_analyst_agent(
                tools=all_tools,
                project_id=project_id_str,
            )
            
            self._update_status(run_id, "running", phase="analyzing")
            
            # Run agent
            initial_message = HumanMessage(
                content="Analyze this project and generate complete system documentation. "
                        "Start by listing artifacts to understand what's available."
            )
            
            # Trace the entire agent execution
            with trace_execution(
                name="Analyst Agent Run",
                inputs={
                    "project_id": project_id_str,
                    "recursion_limit": 250
                },
                span_type="chain"
            ) as trace:
                try:
                    result = await agent.ainvoke(
                        {
                            "messages": [initial_message],
                            "project_id": project_id_str,
                            "iteration_count": 0,
                        },
                        config={"recursion_limit": 1000},
                    )
                    trace.set_result(result)
                except Exception as e:
                    trace.set_error(e)
                    raise
            
            self._update_status(run_id, "complete", phase="done")
            logger.info(f"Analyst run complete: {run_id}")
            
        except asyncio.CancelledError:
            self._update_status(run_id, "cancelled")
            logger.warning(f"Analyst run cancelled: {run_id}")
            raise
            
        except Exception as e:
            logger.error(f"Analyst run failed: {run_id} - {e}")
            self._update_status(run_id, "failed", error=str(e))
        
        finally:
            self._tasks.pop(run_id, None)
    
    def get_status(self, run_id: str) -> dict | None:
        """Get status for a run."""
        if run_id in self._statuses:
            return self._statuses[run_id]
        
        status_file = self._get_status_file()
        if status_file.exists():
            try:
                with open(status_file) as f:
                    return json.load(f)
            except Exception:
                pass
        
        return None
    
    @classmethod
    def get_status_by_run_id(cls, run_id: str) -> dict | None:
        """Get status for a run using only run_id (class method)."""
        return cls._statuses.get(run_id)
    
    def cancel(self, run_id: str) -> bool:
        """Cancel a running task."""
        if run_id in self._tasks:
            self._tasks[run_id].cancel()
            return True
        return False
