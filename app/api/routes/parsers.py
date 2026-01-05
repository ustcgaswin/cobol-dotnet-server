"""Parser API routes."""

import uuid
from typing import Optional

from fastapi import APIRouter, Depends, Query
from loguru import logger
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.core.exceptions import ProjectNotFoundException, ParserNotFoundError
from app.core.parsers import PARSER_REGISTRY
from app.services.parser import ParserService
from app.services.project import ProjectService

# Project-scoped router for parsing actions
router = APIRouter(prefix="/projects/{project_id}/parse", tags=["Parsing"])

# Top-level router for parser metadata
parsers_metadata_router = APIRouter(prefix="/parsers", tags=["Parsers"])


# Response schemas
class ParseResultResponse(BaseModel):
    """Response for a single file parse result."""
    
    filename: str
    success: bool
    output_path: Optional[str] = None
    error: Optional[str] = None


class ParseSummaryResponse(BaseModel):
    """Response for parsing operation summary."""
    
    file_type: str
    total_files: int
    successful: int
    failed: int
    results: list[ParseResultResponse]
    consolidated_path: Optional[str] = None


def get_parser_service(db: AsyncSession = Depends(get_db)) -> ParserService:
    """Dependency for parser service."""
    return ParserService(db)


def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    """Dependency for project service."""
    return ProjectService(db)


@router.post("", response_model=APIResponse[list[ParseSummaryResponse]])
async def parse_project(
    project_id: uuid.UUID,
    file_types: Optional[list[str]] = Query(
        default=None,
        description="File types to parse. If not provided, parses all supported types."
    ),
    parser_service: ParserService = Depends(get_parser_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[list[ParseSummaryResponse]]:
    """Parse all source files for a project.
    
    Parses files from project_storage and saves outputs to project_artifacts.
    """
    # Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))
    
    logger.info(f"Starting parse for project {project_id}, file_types={file_types}")
    
    summaries = await parser_service.parse_project(project_id, file_types)
    
    response_data = [
        ParseSummaryResponse(
            file_type=s.file_type,
            total_files=s.total_files,
            successful=s.successful,
            failed=s.failed,
            results=[
                ParseResultResponse(
                    filename=r.filename,
                    success=r.success,
                    output_path=r.output_path,
                    error=r.error,
                )
                for r in s.results
            ],
            consolidated_path=s.consolidated_path,
        )
        for s in summaries
    ]
    
    total_files = sum(s.total_files for s in summaries)
    total_success = sum(s.successful for s in summaries)
    logger.info(f"Parse complete: {total_success}/{total_files} files successful")
    
    return APIResponse.ok(response_data)


@router.post("/{file_type}", response_model=APIResponse[ParseSummaryResponse])
async def parse_file_type(
    project_id: uuid.UUID,
    file_type: str,
    parser_service: ParserService = Depends(get_parser_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[ParseSummaryResponse]:
    """Parse all files of a specific type for a project."""
    # Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))
    
    # Verify file type has a parser
    if file_type not in PARSER_REGISTRY:
        raise ParserNotFoundError(f"No parser registered for file type: {file_type}")
    
    logger.info(f"Starting parse for project {project_id}, file_type={file_type}")
    
    summary = await parser_service.parse_file_type(project_id, file_type)
    
    response = ParseSummaryResponse(
        file_type=summary.file_type,
        total_files=summary.total_files,
        successful=summary.successful,
        failed=summary.failed,
        results=[
            ParseResultResponse(
                filename=r.filename,
                success=r.success,
                output_path=r.output_path,
                error=r.error,
            )
            for r in summary.results
        ],
        consolidated_path=summary.consolidated_path,
    )
    
    logger.info(f"Parse complete: {summary.successful}/{summary.total_files} files successful")
    
    return APIResponse.ok(response)


@parsers_metadata_router.get("/supported-types", response_model=APIResponse[list[str]])
async def get_supported_types() -> APIResponse[list[str]]:
    """Get list of file types that have parsers available."""
    return APIResponse.ok(list(PARSER_REGISTRY.keys()))
