"""Summarization API routes."""

import uuid
from typing import Optional

from fastapi import APIRouter, Depends, Query
from loguru import logger
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.services.project import ProjectService
from app.services.summarizer import SummarizerService


router = APIRouter(
    prefix="/projects/{project_id}/file-summaries",
    tags=["Summarization"]
)


class FileSummariesResponse(BaseModel):
    """Response for file summaries generation."""
    
    project_id: str
    status: str
    message: str


class FileSummariesResultResponse(BaseModel):
    """Response when summaries are complete (blocking mode)."""
    
    project_id: str
    output_path: str
    file_count: int


class FileSummariesStatusResponse(BaseModel):
    """Response for status check."""
    
    status: str
    total_files: Optional[int] = None
    completed_files: Optional[int] = None
    progress_percent: int = 0
    current_file: Optional[str] = None
    error: Optional[str] = None
    started_at: Optional[str] = None


def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    """Dependency for project service."""
    return ProjectService(db)


@router.post("")
async def create_file_summaries(
    project_id: uuid.UUID,
    blocking: bool = Query(default=False, description="If true, wait for completion (for pipeline use)"),
    db: AsyncSession = Depends(get_db),  
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse:
    """Generate file summaries for a project via LLM.
    
    This process:
    1. Chunks files at logical boundaries (divisions/paragraphs for COBOL, procedures for PL/I, etc.)
    2. Performs rolling summarization using LLM
    3. Generates structured file_summaries.md
    
    **Modes:**
    - `blocking=false` (default): Returns immediately, use GET /status to poll
    - `blocking=true`: Waits for completion (for pipeline integration)
    
    The output is saved to project_artifacts/{project_id}/file_summaries.md
    Per-file summaries are stored in project_artifacts/{project_id}/summaries/ for crash recovery.
    """
    # Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        from app.core.exceptions import ProjectNotFoundException
        raise ProjectNotFoundException(str(project_id))
    
    service = SummarizerService(project_id, db)
    
    if blocking:
        # Blocking mode for pipeline/internal use
        logger.info(f"Generating file summaries (blocking) for project {project_id}")
        result = await service.generate()
        
        response = FileSummariesResultResponse(
            project_id=str(project_id),
            output_path=result.get("output_path", ""),
            file_count=result.get("file_count", 0),
        )
        return APIResponse.ok(response)
    else:
        # Non-blocking mode for direct API calls
        logger.info(f"Starting async file summaries generation for project {project_id}")
        await service.run()
        
        response = FileSummariesResponse(
            project_id=str(project_id),
            status="started",
            message="File summarization started. Poll GET /status for progress.",
        )
        return APIResponse.ok(response)


@router.get("/status", response_model=APIResponse[FileSummariesStatusResponse])
async def get_file_summaries_status(
    project_id: uuid.UUID,
    db: AsyncSession = Depends(get_db),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[FileSummariesStatusResponse]:
    """Get the status of file summarization for a project.
    
    Returns:
    - `pending`: Summarization has not started
    - `in_progress`: Summarization is running (includes progress info)
    - `completed`: All summaries generated and merged to file_summaries.md
    - `failed`: Summarization encountered an error
    """
    # Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        from app.core.exceptions import ProjectNotFoundException
        raise ProjectNotFoundException(str(project_id))
    
    service = SummarizerService(project_id, db)
    status_data = service.get_status()
    
    response = FileSummariesStatusResponse(
        status=status_data.get("status", "unknown"),
        total_files=status_data.get("total_files"),
        completed_files=status_data.get("completed_files"),
        progress_percent=status_data.get("progress_percent", 0),
        current_file=status_data.get("current_file"),
        error=status_data.get("error"),
        started_at=status_data.get("started_at"),
    )
    
    return APIResponse.ok(response)
