"""Code Generation Local Service - async task runner."""

import asyncio
import json
from datetime import datetime
from pathlib import Path
from uuid import UUID, uuid4

from langchain.messages import HumanMessage
from loguru import logger

from app.config.settings import settings
from app.core.tools.artifact_tools import create_artifact_tools
from app.db.base import async_session_factory
from app.db.repositories.project import ProjectRepository
from app.services.codegen_local.agent import create_codegen_agent
from app.services.codegen_local.tools.knowledge_tools import create_knowledge_tools
from app.services.codegen_local.tools.solution_tools import create_solution_tools
from app.services.codegen_local.tools.build_tools import create_build_tools
from app.services.codegen_local.tools.status_tools import create_status_tools
from app.services.codegen_local.tools.source_file_tools import create_source_file_tools


class CodegenLocalService:
    """Service for running the Code Generation Agent.
    
    Converts mainframe components to .NET code using an autonomous agent.
    Uses asyncio.create_task for non-blocking execution.
    
    Folder structure:
        code-migration/
        ├── codegen_status_local.json     (run status)
        ├── conversion_status_local.md    (component tracking)
        ├── issues_local.md               (issues log)
        ├── unknown_utilities_local.md    (unknown utilities)
        └── {project_name}-local/         (generated .NET solution)
    """
    
    _tasks: dict[str, asyncio.Task] = {}
    _statuses: dict[str, dict] = {}
    
    def __init__(self, project_id: UUID):
        self.project_id = project_id
        self._project_name: str | None = None
    
    def _get_codegen_path(self) -> Path:
        """Get the code-migration directory (parent folder for all outputs)."""
        base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
        codegen_dir = base / str(self.project_id) / "code-migration"
        codegen_dir.mkdir(parents=True, exist_ok=True)
        return codegen_dir
    
    def _get_output_path(self, project_name: str) -> Path:
        """Get the {project_name}-local output directory for generated code."""
        safe_name = project_name.replace(" ", "_").replace("/", "_")
        output_dir = self._get_codegen_path() / f"{safe_name}-local"
        output_dir.mkdir(parents=True, exist_ok=True)
        return output_dir
    
    def _get_status_file(self) -> Path:
        """Get the status file path."""
        return self._get_codegen_path() / "codegen_status_local.json"
    
    def _get_artifacts_path(self) -> Path:
        """Get the project artifacts path (Phase A outputs)."""
        base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
        return base / str(self.project_id)
    
    async def _get_project_name(self) -> str:
        """Get the project name from the database."""
        if self._project_name:
            return self._project_name
            
        async with async_session_factory() as session:
            repo = ProjectRepository(session)
            result = await repo.get_by_id(self.project_id)
            
            if result and result[0]:
                self._project_name = result[0].name
                return self._project_name
            
            # Fallback to project_id if name not found
            self._project_name = str(self.project_id)[:8]
            return self._project_name
    
    def _cleanup_build_artifacts(self, output_path: Path) -> None:
        """Remove .NET build artifacts (bin/, obj/) from output directory.
        
        Uses a robust approach: delete all files first, then directories bottom-up.
        
        Args:
            output_path: Path to the generated solution
        """
        import os
        import stat
        import time
        
        artifacts_to_remove = ["bin", "obj"]
        removed_count = 0
        
        def force_remove_file(filepath):
            """Force remove a file, handling read-only."""
            try:
                os.chmod(filepath, stat.S_IWRITE)
                os.remove(filepath)
            except Exception:
                pass
        
        try:
            for artifact_name in artifacts_to_remove:
                # Find all bin/ and obj/ folders
                for artifact_dir in list(output_path.rglob(artifact_name)):
                    if not artifact_dir.is_dir():
                        continue
                    
                    try:
                        # First pass: delete all files
                        for root, dirs, files in os.walk(str(artifact_dir), topdown=False):
                            for name in files:
                                force_remove_file(os.path.join(root, name))
                            # Small delay between directories
                            time.sleep(0.05)
                        
                        # Second pass: delete directories bottom-up
                        for root, dirs, files in os.walk(str(artifact_dir), topdown=False):
                            for name in dirs:
                                try:
                                    os.rmdir(os.path.join(root, name))
                                except Exception:
                                    pass
                        
                        # Finally remove the top directory
                        try:
                            os.rmdir(str(artifact_dir))
                            removed_count += 1
                            logger.debug(f"Removed: {artifact_dir}")
                        except Exception:
                            # Directory may have remaining locked files
                            logger.debug(f"Could not fully remove: {artifact_dir}")
                            
                    except Exception as e:
                        logger.warning(f"Failed to remove {artifact_dir}: {e}")
            
            if removed_count > 0:
                logger.info(f"Cleaned up {removed_count} build artifact folders")
                
        except Exception as e:
            logger.warning(f"Build cleanup error: {e}")
    
    def _check_prerequisites(self) -> tuple[bool, str]:
        """Check if Phase A outputs exist.
        
        Returns:
            (success, error_message)
        """
        artifacts_path = self._get_artifacts_path()
        
        required_files = ["file_summaries.md", "dependency_graph.md"]
        missing = []
        
        for filename in required_files:
            if not (artifacts_path / filename).exists():
                missing.append(filename)
        
        if missing:
            return False, f"Phase A outputs missing: {', '.join(missing)}. Run Phase A first."
        
        return True, ""
    
    def _update_status(
        self,
        run_id: str,
        status: str,
        phase: str = "",
        error: str = "",
        started_at: str = None,
        completed_at: str = None,
    ) -> None:
        """Update and persist status."""
        # Load existing status to preserve started_at
        existing = self._statuses.get(run_id, {})
        
        status_data = {
            "run_id": run_id,
            "project_id": str(self.project_id),
            "status": status,
            "phase": phase,
            "error": error,
            "started_at": started_at or existing.get("started_at"),
            "completed_at": completed_at,
            "updated_at": datetime.utcnow().isoformat(),
        }
        self._statuses[run_id] = status_data
        
        try:
            with open(self._get_status_file(), "w") as f:
                json.dump(status_data, f, indent=2)
        except Exception as e:
            logger.warning(f"Failed to persist status: {e}")
    
    async def run(self) -> dict:
        """Start the code generation agent asynchronously.
        
        Returns:
            Dict with run_id and status, or error if prerequisites not met
        """
        # Check prerequisites first
        ok, error_msg = self._check_prerequisites()
        if not ok:
            return {
                "success": False,
                "error": error_msg,
            }
        
        run_id = str(uuid4())
        
        task = asyncio.create_task(self._execute(run_id))
        self._tasks[run_id] = task
        
        logger.info(f"Started codegen run: {run_id} for project {self.project_id}")
        return {
            "success": True,
            "run_id": run_id,
            "project_id": str(self.project_id),
        }
    
    async def _execute(self, run_id: str) -> None:
        """Execute the code generation agent."""
        start_time = datetime.utcnow().isoformat()
        
        try:
            self._update_status(run_id, "running", phase="initializing", started_at=start_time)
            
            project_id_str = str(self.project_id)
            
            # Get project name for output folder
            project_name = await self._get_project_name()
            output_path = self._get_output_path(project_name)
            
            logger.info(f"Output path: {output_path}")
            
            # Create all tools with project_id binding
            artifact_tools = create_artifact_tools(project_id_str)
            knowledge_tools = create_knowledge_tools()
            solution_tools = create_solution_tools(project_id_str, str(output_path))
            build_tools = create_build_tools(project_id_str, str(output_path))
            status_tools = create_status_tools(project_id_str)
            source_file_tools = create_source_file_tools(project_id_str)
            
            all_tools = (
                artifact_tools +
                knowledge_tools +
                solution_tools +
                build_tools +
                status_tools +
                source_file_tools
            )
            
            # Create agent
            agent = create_codegen_agent(
                tools=all_tools,
                project_id=project_id_str,
            )
            
            self._update_status(run_id, "running", phase="converting")
            
            # Run agent with project name in the message
            initial_message = HumanMessage(
                content=f"Convert this mainframe project '{project_name}' to .NET. "
                        f"Use '{project_name}' as the solution name when calling initialize_solution(). "
                        "Start by checking existing progress, then read the dependency graph "
                        "and begin converting components in dependency order."
            )
            
            result = await agent.ainvoke(
                {
                    "messages": [initial_message],
                    "project_id": project_id_str,
                    "iteration_count": 0,
                },
                config={"recursion_limit": 550},
            )
            
            # Clean up build artifacts before marking complete
            self._cleanup_build_artifacts(output_path)
            
            end_time = datetime.utcnow().isoformat()
            self._update_status(run_id, "complete", phase="done", completed_at=end_time)
            logger.info(f"Codegen run complete: {run_id}")
            
        except asyncio.CancelledError:
            end_time = datetime.utcnow().isoformat()
            self._update_status(run_id, "cancelled", completed_at=end_time)
            logger.warning(f"Codegen run cancelled: {run_id}")
            raise
            
        except Exception as e:
            end_time = datetime.utcnow().isoformat()
            logger.error(f"Codegen run failed: {run_id} - {e}")
            self._update_status(run_id, "failed", error=str(e), completed_at=end_time)
        
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
        """Get status for a run using only run_id."""
        return cls._statuses.get(run_id)
    
    def cancel(self, run_id: str) -> bool:
        """Cancel a running task."""
        if run_id in self._tasks:
            self._tasks[run_id].cancel()
            return True
        return False
