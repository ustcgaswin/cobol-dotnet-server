"""Dependency graph API routes."""

import uuid

from fastapi import APIRouter, Depends
from loguru import logger
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.core.exceptions import ProjectNotFoundException
from app.services.dependency_extractor import DependencyExtractorService
from app.services.project import ProjectService


router = APIRouter(
    prefix="/projects/{project_id}/dependency-graph",
    tags=["Dependencies"]
)


class DependencyGraphResponse(BaseModel):
    """Response for dependency graph generation."""
    
    project_id: str
    output_path: str
    relationship_counts: dict[str, int]
    missing_file_types: list[str]


def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    """Dependency for project service."""
    return ProjectService(db)


@router.post("", response_model=APIResponse[DependencyGraphResponse])
async def create_dependency_graph(
    project_id: uuid.UUID,
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[DependencyGraphResponse]:
    """Generate dependency graph for a project.
    
    Reads all parsed outputs and generates dependency_graph.md containing:
    - Program-to-program call relationships
    - Program-to-copybook references
    - Program-to-SQL-table relationships
    - Program-to-file definitions and I/O operations
    - Copybook-to-copybook nested references
    - Gaps (unresolved dynamic calls, missing file types)
    
    The output is saved to project_artifacts/{project_id}/dependency_graph.md
    """
    # Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))
    
    logger.info(f"Generating dependency graph for project {project_id}")
    
    # Generate dependency graph
    service = DependencyExtractorService(project_id)
    result = await service.generate()
    
    response = DependencyGraphResponse(
        project_id=str(project_id),
        output_path=result['output_path'],
        relationship_counts=result['relationship_counts'],
        missing_file_types=result['missing_file_types'],
    )
    
    logger.info(f"Dependency graph generated: {result['relationship_counts']}")
    
    return APIResponse.ok(response)
