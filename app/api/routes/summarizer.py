"""Summarization API routes."""

import uuid

from fastapi import APIRouter, Depends
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
    output_path: str
    file_count: int


def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    """Dependency for project service."""
    return ProjectService(db)


@router.post("", response_model=APIResponse[FileSummariesResponse])
async def create_file_summaries(
    project_id: uuid.UUID,
    db: AsyncSession = Depends(get_db),  
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[FileSummariesResponse]:
    """Generate file summaries for a project via LLM.
    
    This process:
    1. Chunks COBOL files at logical boundaries (divisions/paragraphs)
    2. Performs rolling summarization using LLM
    3. Generates structured file_summaries.md
    
    The output is saved to project_artifacts/{project_id}/file_summaries.md
    """
    # Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        from app.core.exceptions import ProjectNotFoundException
        raise ProjectNotFoundException(str(project_id))
    
    logger.info(f"Generating file summaries for project {project_id}")
    
    # Generate summaries
    service = SummarizerService(project_id, db)
    result = await service.generate()
    
    response = FileSummariesResponse(
        project_id=str(project_id),
        output_path=result.get("output_path", ""),
        file_count=result.get("file_count", 0),
    )
    
    return APIResponse.ok(response)
