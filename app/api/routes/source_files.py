"""Source file API routes."""

import uuid

from fastapi import APIRouter, Depends, Form, UploadFile, status
from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.api.schemas.source_file import (
    DeleteSourceFileResponse,
    SourceFileResponse,
    UploadedFileInfo,
    UploadResponse,
)
from app.core.exceptions import ProjectNotFoundException, SourceFileNotFoundException
from app.services.project import ProjectService
from app.services.source_file import SourceFileService

router = APIRouter(prefix="/projects/{project_id}/files", tags=["Source Files"])


def get_source_file_service(db: AsyncSession = Depends(get_db)) -> SourceFileService:
    """Dependency for source file service."""
    return SourceFileService(db)


def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    """Dependency for project service."""
    return ProjectService(db)


@router.post("", response_model=APIResponse[UploadResponse], status_code=status.HTTP_201_CREATED)
async def upload_files(
    project_id: uuid.UUID,
    files: list[UploadFile],
    file_types: list[str] = Form(..., description="File type for each file (in same order as files)"),
    service: SourceFileService = Depends(get_source_file_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[UploadResponse]:
    """Upload one or more source files to a project.
    
    Each file must have a corresponding file_type in the same position.
    Supported types: cobol, copybook, jcl, rexx, catproc, proc, pli
    
    All files must pass validation (size limits) before any are saved.
    Duplicate files (same name and type) are overwritten.
    """
    # Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))
    
    logger.info(f"Uploading {len(files)} file(s) to project {project_id}")
    
    # Upload files
    results = await service.upload_files(project_id, files, file_types)
    
    # Build response
    uploaded = [
        UploadedFileInfo(
            id=r.id,
            filename=r.filename,
            file_type=r.file_type,
            size_bytes=r.size_bytes,
            overwritten=r.overwritten,
        )
        for r in results
    ]
    
    overwritten_count = sum(1 for r in results if r.overwritten)
    
    return APIResponse.ok(UploadResponse(
        uploaded=uploaded,
        total_count=len(uploaded),
        overwritten_count=overwritten_count,
    ))


@router.get("", response_model=APIResponse[list[SourceFileResponse]])
async def list_files(
    project_id: uuid.UUID,
    service: SourceFileService = Depends(get_source_file_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[list[SourceFileResponse]]:
    """List all source files for a project."""
    # Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))
    
    files = await service.get_project_files(project_id)
    return APIResponse.ok([SourceFileResponse.model_validate(f) for f in files])


@router.get("/{file_id}", response_model=APIResponse[SourceFileResponse])
async def get_file(
    project_id: uuid.UUID,
    file_id: uuid.UUID,
    service: SourceFileService = Depends(get_source_file_service),
) -> APIResponse[SourceFileResponse]:
    """Get source file metadata by ID."""
    source_file = await service.get_file(file_id)
    if not source_file or source_file.project_id != project_id:
        raise SourceFileNotFoundException(str(file_id))
    
    return APIResponse.ok(SourceFileResponse.model_validate(source_file))


@router.delete("/{file_id}", response_model=APIResponse[DeleteSourceFileResponse])
async def delete_file(
    project_id: uuid.UUID,
    file_id: uuid.UUID,
    service: SourceFileService = Depends(get_source_file_service),
) -> APIResponse[DeleteSourceFileResponse]:
    """Delete a source file."""
    # Verify file belongs to project
    source_file = await service.get_file(file_id)
    if not source_file or source_file.project_id != project_id:
        raise SourceFileNotFoundException(str(file_id))
    
    await service.delete_file(file_id)
    
    return APIResponse.ok(DeleteSourceFileResponse(
        message="Source file deleted successfully",
        id=file_id,
    ))
