"""Centralized exception handlers for FastAPI application."""

from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse
from loguru import logger
from sqlalchemy.exc import SQLAlchemyError

from app.api.schemas.common import APIResponse
from app.core.exceptions import (
    DatabaseConnectionError,
    DatabaseException,
    DatabaseHealthCheckError,
    ProjectCreationError,
    ProjectException,
    ProjectNotFoundException,
    ProjectValidationError,
)


async def database_exception_handler(
    request: Request, exc: DatabaseException
) -> JSONResponse:
    """Handle custom database exceptions."""
    logger.error(f"Database error: {exc.message}")
    
    # Determine error code based on exception type
    if isinstance(exc, DatabaseConnectionError):
        error_code = "DATABASE_CONNECTION_ERROR"
        status_code = 503
    elif isinstance(exc, DatabaseHealthCheckError):
        error_code = "DATABASE_HEALTH_CHECK_ERROR"
        status_code = 503
    else:
        error_code = "DATABASE_ERROR"
        status_code = 500
    
    response = APIResponse.fail(
        code=error_code,
        message=exc.message,
    )
    return JSONResponse(
        status_code=status_code,
        content=response.model_dump(mode="json"),
    )


async def project_exception_handler(
    request: Request, exc: ProjectException
) -> JSONResponse:
    """Handle project-related exceptions."""
    logger.error(f"Project error: {exc.message}")
    
    # Determine error code and status based on exception type
    if isinstance(exc, ProjectNotFoundException):
        error_code = "PROJECT_NOT_FOUND"
        status_code = 404
    elif isinstance(exc, ProjectValidationError):
        error_code = "PROJECT_VALIDATION_ERROR"
        status_code = 400
    elif isinstance(exc, ProjectCreationError):
        error_code = "PROJECT_CREATION_ERROR"
        status_code = 500
    else:
        error_code = "PROJECT_ERROR"
        status_code = 500
    
    response = APIResponse.fail(
        code=error_code,
        message=exc.message,
    )
    return JSONResponse(
        status_code=status_code,
        content=response.model_dump(mode="json"),
    )


async def sqlalchemy_exception_handler(
    request: Request, exc: SQLAlchemyError
) -> JSONResponse:
    """Handle SQLAlchemy errors."""
    logger.error(f"SQLAlchemy error: {str(exc)}")
    
    response = APIResponse.fail(
        code="DATABASE_ERROR",
        message="A database error occurred",
        details=str(exc) if logger.level("DEBUG") else None,
    )
    return JSONResponse(
        status_code=500,
        content=response.model_dump(mode="json"),
    )


async def generic_exception_handler(
    request: Request, exc: Exception
) -> JSONResponse:
    """Handle unhandled exceptions."""
    logger.exception(f"Unhandled exception: {str(exc)}")
    
    response = APIResponse.fail(
        code="INTERNAL_SERVER_ERROR",
        message="An unexpected error occurred",
    )
    return JSONResponse(
        status_code=500,
        content=response.model_dump(mode="json"),
    )


def register_exception_handlers(app: FastAPI) -> None:
    """Register all exception handlers with the FastAPI app."""
    app.add_exception_handler(DatabaseException, database_exception_handler)
    app.add_exception_handler(ProjectException, project_exception_handler)
    app.add_exception_handler(SQLAlchemyError, sqlalchemy_exception_handler)
    app.add_exception_handler(Exception, generic_exception_handler)
