"""Project API routes."""

import uuid

from fastapi import APIRouter, Depends, status
from loguru import logger
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.api.schemas.project import ProjectCreate, ProjectResponse, ProjectUpdate
from app.core.exceptions import ProjectNotFoundException
from app.services.project import ProjectService

router = APIRouter(prefix="/projects", tags=["Projects"])


class DeleteResponse(BaseModel):
    """Response for delete operation."""
    
    message: str
    id: uuid.UUID


def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    """Dependency for project service."""
    return ProjectService(db)


@router.post("", response_model=APIResponse[ProjectResponse], status_code=status.HTTP_201_CREATED)
async def create_project(
    data: ProjectCreate,
    service: ProjectService = Depends(get_project_service),
) -> APIResponse[ProjectResponse]:
    """Create a new project."""
    logger.info(f"Creating project: {data.name}")
    project = await service.create_project(
        name=data.name,
        description=data.description,
    )
    logger.info(f"Project created successfully: {project.id}")
    return APIResponse.ok(ProjectResponse.model_validate(project))


@router.get("", response_model=APIResponse[list[ProjectResponse]])
async def list_projects(
    service: ProjectService = Depends(get_project_service),
) -> APIResponse[list[ProjectResponse]]:
    """List all projects."""
    logger.debug("Fetching all projects")
    
    # results is a list of tuples: [(project_obj, 5), (project_obj, 0), ...]
    results = await service.get_all_projects()
    
    response_data = []
    for project, count in results:
        # Convert ORM object to Pydantic model
        model = ProjectResponse.model_validate(project)
        # Inject the computed count
        model.file_count = count
        response_data.append(model)
        
    logger.debug(f"Found {len(response_data)} projects")
    return APIResponse.ok(response_data)


# UPDATE get_project
@router.get("/{project_id}", response_model=APIResponse[ProjectResponse])
async def get_project(
    project_id: uuid.UUID,
    service: ProjectService = Depends(get_project_service),
) -> APIResponse[ProjectResponse]:
    """Get project by ID."""
    logger.debug(f"Fetching project: {project_id}")
    
    result = await service.get_project(project_id)
    if not result:
        raise ProjectNotFoundException(str(project_id))
    
    # Unpack tuple
    project, count = result
    
    # Create response
    model = ProjectResponse.model_validate(project)
    model.file_count = count
    
    return APIResponse.ok(model)

@router.patch("/{project_id}", response_model=APIResponse[ProjectResponse])
async def update_project(
    project_id: uuid.UUID,
    data: ProjectUpdate,
    service: ProjectService = Depends(get_project_service),
) -> APIResponse[ProjectResponse]:
    """Update a project."""
    logger.info(f"Updating project: {project_id}")
    
    # FIX: Unpack the tuple here
    result = await service.get_project(project_id)
    if not result:
        raise ProjectNotFoundException(str(project_id))
    
    project, count = result 
    
    updated = await service.update_project(
        project=project,
        name=data.name,
        description=data.description,
        functional_document_status=data.functional_document_status,
        technical_document_status=data.technical_document_status,
        code_migration_status=data.code_migration_status,
    )
    logger.info(f"Project updated successfully: {project_id}")
    
    # Add count back to response
    model = ProjectResponse.model_validate(updated)
    model.file_count = count
    
    return APIResponse.ok(model)


@router.delete("/{project_id}", response_model=APIResponse[DeleteResponse])
async def delete_project(
    project_id: uuid.UUID,
    service: ProjectService = Depends(get_project_service),
) -> APIResponse[DeleteResponse]:
    """Delete a project."""
    logger.info(f"Deleting project: {project_id}")
    
    # FIX: Unpack the tuple here
    result = await service.get_project(project_id)
    if not result:
        raise ProjectNotFoundException(str(project_id))
    
    project, _ = result 
    
    await service.delete_project(project)
    logger.info(f"Project deleted successfully: {project_id}")
    
    return APIResponse.ok(DeleteResponse(
        message="Project deleted successfully",
        id=project_id,
    ))
