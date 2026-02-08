"""Code Generation Local Service - async task runner."""

import asyncio
import json
from datetime import datetime
from pathlib import Path
from uuid import UUID, uuid4

from langchain.messages import HumanMessage
from loguru import logger
import httpx

from app.config.settings import settings
from app.core.tools.artifact_tools import create_artifact_tools
from app.core.tools.rag_tools import search_docs
from app.db.base import async_session_factory
from app.db.repositories.project import ProjectRepository
from app.services.codegen_local.agent import create_codegen_agent
from app.services.codegen_local.tools.knowledge_tools import create_knowledge_tools
from app.services.codegen_local.tools.solution_tools import create_solution_tools
from app.services.codegen_local.tools.build_tools import create_build_tools
from app.services.codegen_local.tools.status_tools import create_status_tools
from app.services.codegen_local.tools.source_file_tools import create_source_file_tools
from app.services.codegen_local.tools.system_context_tools import create_system_context_tools
from app.services.analyst.service import AnalystService
from app.config.llm.tracing import trace_execution, trace_tool


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
    
    def __init__(self, project_id: UUID):
        self.project_id = project_id
        self._project_name: str | None = None
    
    def _get_codegen_path(self) -> Path:
        """Get the code-migration directory (parent folder for all outputs).
        
        Note: This does NOT create the directory. Use _ensure_codegen_path() 
        when you need to create it during code generation.
        """
        base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
        return base / str(self.project_id) / "code-migration"
    
    def _ensure_codegen_path(self) -> Path:
        """Get and create the code-migration directory if needed.
        
        Use this during code generation when directories need to be created.
        """
        codegen_dir = self._get_codegen_path()
        codegen_dir.mkdir(parents=True, exist_ok=True)
        return codegen_dir
    
    def _get_output_path(self, project_name: str) -> Path:
        """Get the {project_name}-local output directory for generated code."""
        safe_name = project_name.replace(" ", "_").replace("/", "_")
        return self._get_codegen_path() / f"{safe_name}-local"
    
    def _get_status_file(self) -> Path:
        """Get the status file path.
        
        Uses _ensure_codegen_path since status files are written during codegen.
        """
        return self._ensure_codegen_path() / "codegen_status_local.json"
    
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
                                # Start by checking existing progress, then read the dependency graph
                                # and begin converting components in dependency order.
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
    
    async def _ensure_prerequisites(self) -> None:
        """Ensure Phase A outputs and Analyst outputs exist.
        
        Delegates to AnalystService to generate missing artifacts.
        If system_context folder is missing, runs the full Analyst agent.
        """
        analyst_service = AnalystService(self.project_id)
        
        # 1. Ensure basic artifacts (file_summaries.md, dependency_graph.md)
        # This will auto-run parsing/summarization/dependency extraction if needed
        await analyst_service.ensure_prerequisites()
        
        # 2. Check if Analyst has run (system_context with functionality_catalog)
        system_context_path = Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / str(self.project_id) / "system_context"
        functionality_catalog = system_context_path / "functionality_catalog.md"
        
        if not functionality_catalog.exists():
            logger.info("functionality_catalog.md missing. Running Analyst agent...")
            run_id = await analyst_service.run()
            
            # Wait for Analyst to complete (poll status)
            max_wait = 600  # 10 minutes
            poll_interval = 5
            waited = 0
            status = None
            
            while waited < max_wait:
                status = analyst_service.get_status(run_id)
                if status and status.get("status") in ("complete", "failed", "cancelled"):
                    break
                await asyncio.sleep(poll_interval)
                waited += poll_interval
            
            if not status or status.get("status") != "complete":
                logger.warning(f"Analyst agent did not complete successfully: {status}")
            else:
                logger.info("Analyst agent completed")
    
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
        # Read from file to preserve start time if needed, since we no longer keep memory state
        current_data = {}
        status_file = self._get_status_file()
        if status_file.exists():
            try:
                with open(status_file, "r") as f:
                    current_data = json.load(f)
            except Exception:
                pass

        status_data = {
            "run_id": run_id,
            "project_id": str(self.project_id),
            "status": status,
            "phase": phase,
            "error": error,
            "started_at": started_at or current_data.get("started_at"),
            "completed_at": completed_at,
            "updated_at": datetime.utcnow().isoformat(),
        }
        
        try:
            with open(status_file, "w") as f:
                json.dump(status_data, f, indent=2)
        except Exception as e:
            logger.warning(f"Failed to persist status: {e}")
    
    async def run(self) -> dict:
        """Start the code generation agent asynchronously.
        
        Returns:
            Dict with run_id and status
        """
        # Note: We do NOT wait for prerequisites here to avoid blocking the API response.
        # They will be checked/generated in the background task (_execute).
        
        run_id = str(uuid4())
        
        task = asyncio.create_task(self._execute(run_id))
        self._tasks[run_id] = task
        
        logger.info(f"Started codegen run: {run_id} for project {self.project_id}")
        return {
            "success": True,
            "run_id": run_id,
            "project_id": str(self.project_id),
        }
    
    async def _update_db_status(self, status: str) -> None:
        """Update project status in the database."""
        from app.db.models.project import Project, ProjectStatus
        
        try:
            async with async_session_factory() as session:
                repo = ProjectRepository(session)
                # We need to get the project first to update it
                # Logic might vary depending on repo implementation, but standard update:
                project = await session.get(Project, self.project_id)
                if project:
                    project.code_migration_status = status
                    await session.commit()
        except Exception as e:
            logger.error(f"Failed to update DB status to {status}: {e}")

    async def _execute(self, run_id: str) -> None:
        """Execute the code generation agent."""
        from app.db.models.project import ProjectStatus

        start_time = datetime.utcnow().isoformat()
        
        try:
            # DB: Mark IN_PROGRESS
            await self._update_db_status(ProjectStatus.IN_PROGRESS.value)
            
            self._update_status(run_id, "running", phase="initializing", started_at=start_time)
            
            # 1. Ensure Prerequisites (Phase A Artifacts)
            # This might take time (parsing, summarizing, etc.) so we report status
            self._update_status(run_id, "running", phase="analyzing_dependencies")
            try:
                await self._ensure_prerequisites()
            except Exception as e:
                logger.error(f"Prerequisite check failed for run {run_id}: {e}")
                # We fail early if we can't even get the basic artifacts
                raise RuntimeError(f"Failed to prepare project artifacts: {e}") from e

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
            system_context_tools = create_system_context_tools(project_id_str)
            rag_tools = [search_docs]
            
            all_tools = (
                artifact_tools +
                knowledge_tools +
                solution_tools +
                build_tools +
                status_tools +
                source_file_tools +
                system_context_tools +
                rag_tools
            )
            
            # Wrap tools with tracing
            all_tools = [trace_tool(t) for t in all_tools]
            
            # Create agent
            agent = create_codegen_agent(
                tools=all_tools,
                project_id=project_id_str,
            )
            
            self._update_status(run_id, "running", phase="converting")
            
            # Read system overview if available (inject as context)
            system_overview_content = ""
            system_overview_path = Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / project_id_str / "system_context" / "system_overview.md"
            if system_overview_path.exists():
                try:
                    overview_text = system_overview_path.read_text(encoding="utf-8", errors="replace")
                    # Truncate if too long
                    if len(overview_text) > 4000:
                        overview_text = overview_text[:4000] + "\n... (truncated)"
                    system_overview_content = f"\n\n## System Overview (from Analyst)\n{overview_text}\n"
                except Exception as e:
                    logger.warning(f"Failed to read system overview: {e}")
            
            # Run agent with project name and system overview in the message
            initial_message = HumanMessage(
                content=f"Convert this mainframe project '{project_name}' to .NET. "
                        f"Use '{project_name}' as the solution name when calling initialize_solution(). "
                        "Start by checking existing progress, read the functionality catalog as your checklist, "
                        "then read the dependency graph and begin converting components in dependency order."
                        f"{system_overview_content}"
            )
            
            # Create codegen logs directory path
            codegen_logs_path = Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / project_id_str / "codegen_logs"
            
            # Trace the entire agent execution as a parent span
            with trace_execution(
                name="Codegen Agent Run",
                inputs={
                    "project_id": project_id_str,
                    "project_name": project_name,
                    "iteration_limit": 1000
                },
                span_type="chain"
            ) as trace:
                try:
                    result = await agent.ainvoke(
                        {
                            "messages": [initial_message],
                            "project_id": project_id_str,
                            "iteration_count": 0,
                            "codegen_logs_path": str(codegen_logs_path),
                        },
                        config={"recursion_limit": 1000},
                    )
                    trace.set_result(result)
                except Exception as e:
                    trace.set_error(e)
                    raise
            
            # Clean up build artifacts before marking complete
            self._cleanup_build_artifacts(output_path)
            
            end_time = datetime.utcnow().isoformat()
            self._update_status(run_id, "complete", phase="done", completed_at=end_time)
            
            # DB: Mark COMPLETED
            await self._update_db_status(ProjectStatus.COMPLETED.value)
            
            logger.info(f"Codegen run complete: {run_id}")
            
        except asyncio.CancelledError:
            end_time = datetime.utcnow().isoformat()
            self._update_status(run_id, "cancelled", completed_at=end_time)
            logger.warning(f"Codegen run cancelled: {run_id}")
            # DB: Reset to PENDING if cancelled? Or FAILED? 
            # Usually cancelled means user stopped it, so PENDING allows retry.
            await self._update_db_status(ProjectStatus.PENDING.value)
            raise
            
        except httpx.ConnectError as e:
            end_time = datetime.utcnow().isoformat()
            error_msg = f"LLM Connection Error: Could not connect to LLM service. Check your network or LLM configuration. Details: {e}"
            logger.error(f"Codegen run failed: {run_id} - {error_msg}")
            self._update_status(run_id, "failed", error=error_msg, completed_at=end_time)
            
            # DB: Mark FAILED
            await self._update_db_status(ProjectStatus.FAILED.value)

        except httpx.TimeoutException as e:
            end_time = datetime.utcnow().isoformat()
            error_msg = f"LLM Timeout Error: LLM service did not respond in time. Details: {e}"
            logger.error(f"Codegen run failed: {run_id} - {error_msg}")
            self._update_status(run_id, "failed", error=error_msg, completed_at=end_time)
            
            # DB: Mark FAILED
            await self._update_db_status(ProjectStatus.FAILED.value)
            
        except Exception as e:
            end_time = datetime.utcnow().isoformat()
            logger.error(f"Codegen run failed: {run_id} - {e}")
            self._update_status(run_id, "failed", error=str(e), completed_at=end_time)
            
            # DB: Mark FAILED
            await self._update_db_status(ProjectStatus.FAILED.value)
        
        finally:
            self._tasks.pop(run_id, None)
    

    
    def cancel(self, run_id: str) -> bool:
        """Cancel a running task."""
        if run_id in self._tasks:
            self._tasks[run_id].cancel()
            return True
        return False
    
    # -------------------------------------------------------------------------
    # File tree and content methods
    # -------------------------------------------------------------------------
    
    @staticmethod
    def encode_file_id(relative_path: str) -> str:
        """Encode a relative path to a URL-safe file ID."""
        import base64
        return base64.urlsafe_b64encode(relative_path.encode()).decode().rstrip('=')
    
    @staticmethod
    def decode_file_id(file_id: str) -> str:
        """Decode a file ID back to a relative path."""
        import base64
        from app.core.exceptions import InvalidFileIdError
        
        # Add padding if needed
        padding = 4 - len(file_id) % 4
        if padding != 4:
            file_id += '=' * padding
        
        try:
            return base64.urlsafe_b64decode(file_id).decode()
        except Exception:
            raise InvalidFileIdError(file_id)
    
    def get_file_tree(self, project_name: str) -> dict:
        """Build the file tree for generated code.
        
        Args:
            project_name: Project name for folder lookup
            
        Returns:
            Nested dict representing file tree
            
        Raises:
            GeneratedCodeNotFoundError: If output folder doesn't exist
        """
        from app.core.exceptions import GeneratedCodeNotFoundError
        
        output_path = self._get_output_path(project_name)
        
        if not output_path.exists():
            raise GeneratedCodeNotFoundError(str(self.project_id))
        
        # Check if directory has any files (recursive check)
        has_files = any(output_path.rglob("*") if output_path.is_dir() else [])
        if not has_files:
            raise GeneratedCodeNotFoundError(str(self.project_id))
        
        def build_tree(path: Path, relative_base: Path) -> dict:
            """Recursively build tree structure.
            
            Skips bin/ and obj/ build artifact directories.
            Handles errors gracefully for Windows path length issues.
            """
            name = path.name
            
            if path.is_file():
                rel_path = str(path.relative_to(relative_base)).replace("\\", "/")
                return {
                    "id": self.encode_file_id(rel_path),
                    "name": name,
                    "type": "file",
                }
            else:
                children = []
                try:
                    for child in sorted(path.iterdir(), key=lambda p: (p.is_file(), p.name.lower())):
                        # Skip build artifact directories
                        if child.is_dir() and child.name.lower() in ("bin", "obj"):
                            continue
                        try:
                            children.append(build_tree(child, relative_base))
                        except (OSError, PermissionError) as e:
                            # Skip inaccessible paths (e.g., Windows path length issues)
                            logger.warning(f"Skipping inaccessible path: {child} - {e}")
                            continue
                except (OSError, PermissionError) as e:
                    # Handle directory iteration errors
                    logger.warning(f"Could not iterate directory: {path} - {e}")
                
                return {
                    "name": name,
                    "type": "directory",
                    "children": children,
                }
        
        return build_tree(output_path, output_path)
    
    def get_file_content(self, project_name: str, file_id: str) -> dict:
        """Get content of a specific file.
        
        Args:
            project_name: Project name for folder lookup
            file_id: URL-safe Base64 encoded file path
            
        Returns:
            Dict with file info and content
            
        Raises:
            InvalidFileIdError: If file_id can't be decoded
            GeneratedCodeNotFoundError: If output folder doesn't exist
            GeneratedFileNotFoundError: If file doesn't exist
        """
        from app.core.exceptions import (
            GeneratedCodeNotFoundError,
            GeneratedFileNotFoundError,
        )
        
        output_path = self._get_output_path(project_name)
        
        if not output_path.exists():
            raise GeneratedCodeNotFoundError(str(self.project_id))
        
        relative_path = self.decode_file_id(file_id)
        file_path = output_path / relative_path
        
        # Security: ensure path is within output directory
        try:
            file_path = file_path.resolve()
            if not str(file_path).startswith(str(output_path.resolve())):
                raise GeneratedFileNotFoundError(file_id, relative_path)
        except Exception:
            raise GeneratedFileNotFoundError(file_id, relative_path)
        
        if not file_path.exists() or not file_path.is_file():
            raise GeneratedFileNotFoundError(file_id, relative_path)
        
        try:
            content = file_path.read_text(encoding="utf-8", errors="replace")
        except Exception as e:
            raise GeneratedFileNotFoundError(file_id, f"{relative_path} (read error: {e})")
        
        return {
            "id": file_id,
            "name": file_path.name,
            "path": relative_path,
            "content": content,
        }

    def create_zip(self, project_name: str) -> bytes:
        """Create a zip archive of the generated code.
        
        Args:
            project_name: Project name for folder lookup
            
        Returns:
            Bytes of the zip file
            
        Raises:
            GeneratedCodeNotFoundError: If output folder doesn't exist
        """
        import io
        import zipfile
        from app.core.exceptions import GeneratedCodeNotFoundError
        
        output_path = self._get_output_path(project_name)
        
        if not output_path.exists():
            raise GeneratedCodeNotFoundError(str(self.project_id))
        
        buffer = io.BytesIO()
        with zipfile.ZipFile(buffer, 'w', zipfile.ZIP_DEFLATED) as zf:
            for file_path in output_path.rglob('*'):
                # Skip directories
                if file_path.is_dir():
                    continue
                # Skip build artifacts (bin/, obj/)
                if any(p.lower() in ('bin', 'obj') for p in file_path.parts):
                    continue
                arcname = file_path.relative_to(output_path)
                zf.write(file_path, arcname)
        
        logger.info(f"Created zip for project {self.project_id} ({buffer.tell()} bytes)")
        return buffer.getvalue()

