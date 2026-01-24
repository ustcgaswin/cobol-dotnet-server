"""Centralized exception handlers for FastAPI application."""

import json

from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse
from loguru import logger
from sqlalchemy.exc import SQLAlchemyError

from app.api.schemas.common import APIResponse
from app.core.exceptions import (
    BuildError,
    ChunkError,
    ChunkerException,
    CodegenException,
    ConversionError,
    DatabaseConnectionError,
    DatabaseException,
    DatabaseHealthCheckError,
    FileSizeExceededError,
    FileWriteError,
    GeneratedCodeNotFoundError,
    GeneratedFileNotFoundError,
    IndexBuildError,
    IndexNotFoundError,
    InvalidFileIdError,
    InvalidFileTypeError,
    ParseError,
    ParserException,
    ParserNotFoundError,
    PrerequisiteError,
    ProjectCreationError,
    ProjectException,
    ProjectNotFoundException,
    ProjectValidationError,
    RAGException,
    SearchError,
    SolutionInitError,
    SourceFileException,
    SourceFileNotFoundException,
    UnsupportedFileTypeError,
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


async def source_file_exception_handler(
    request: Request, exc: SourceFileException
) -> JSONResponse:
    """Handle source file-related exceptions."""
    logger.error(f"Source file error: {exc.message}")
    
    # Determine error code and status based on exception type
    if isinstance(exc, SourceFileNotFoundException):
        error_code = "SOURCE_FILE_NOT_FOUND"
        status_code = 404
        details = None
    elif isinstance(exc, InvalidFileTypeError):
        error_code = "INVALID_FILE_TYPE"
        status_code = 400
        details = json.dumps({"supported_types": exc.supported_types})
    elif isinstance(exc, FileSizeExceededError):
        error_code = "FILE_SIZE_EXCEEDED"
        status_code = 400
        details = json.dumps(exc.violations)
    else:
        error_code = "SOURCE_FILE_ERROR"
        status_code = 500
        details = None
    
    response = APIResponse.fail(
        code=error_code,
        message=exc.message,
        details=details,
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


async def parser_exception_handler(
    request: Request, exc: ParserException
) -> JSONResponse:
    """Handle parser-related exceptions."""
    logger.error(f"Parser error: {str(exc)}")
    
    if isinstance(exc, ParserNotFoundError):
        error_code = "PARSER_NOT_FOUND"
        status_code = 400
    elif isinstance(exc, ParseError):
        error_code = "PARSE_ERROR"
        status_code = 400
    else:
        error_code = "PARSER_ERROR"
        status_code = 500
    
    response = APIResponse.fail(
        code=error_code,
        message=str(exc),
    )
    return JSONResponse(
        status_code=status_code,
        content=response.model_dump(mode="json"),
    )


async def chunker_exception_handler(
    request: Request, exc: ChunkerException
) -> JSONResponse:
    """Handle chunker-related exceptions."""
    logger.error(f"Chunker error: {str(exc)}")
    
    if isinstance(exc, UnsupportedFileTypeError):
        error_code = "UNSUPPORTED_FILE_TYPE"
        status_code = 400
    elif isinstance(exc, ChunkError):
        error_code = "CHUNK_ERROR"
        status_code = 400
    else:
        error_code = "CHUNKER_ERROR"
        status_code = 500
    
    response = APIResponse.fail(
        code=error_code,
        message=str(exc),
    )
    return JSONResponse(
        status_code=status_code,
        content=response.model_dump(mode="json"),
    )


async def rag_exception_handler(
    request: Request, exc: RAGException
) -> JSONResponse:
    """Handle RAG-related exceptions."""
    logger.error(f"RAG error: {str(exc)}")
    
    if isinstance(exc, IndexNotFoundError):
        error_code = "INDEX_NOT_FOUND"
        status_code = 404
    elif isinstance(exc, IndexBuildError):
        error_code = "INDEX_BUILD_ERROR"
        status_code = 500
    elif isinstance(exc, SearchError):
        error_code = "SEARCH_ERROR"
        status_code = 400
    else:
        error_code = "RAG_ERROR"
        status_code = 500
    
    response = APIResponse.fail(
        code=error_code,
        message=str(exc),
    )
    return JSONResponse(
        status_code=status_code,
        content=response.model_dump(mode="json"),
    )


async def codegen_exception_handler(
    request: Request, exc: CodegenException
) -> JSONResponse:
    """Handle code generation exceptions."""
    logger.error(f"Codegen error: {exc.message}")
    
    if isinstance(exc, PrerequisiteError):
        error_code = "PREREQUISITE_ERROR"
        status_code = 400
    elif isinstance(exc, SolutionInitError):
        error_code = "SOLUTION_INIT_ERROR"
        status_code = 500
    elif isinstance(exc, BuildError):
        error_code = "BUILD_ERROR"
        status_code = 500
    elif isinstance(exc, FileWriteError):
        error_code = "FILE_WRITE_ERROR"
        status_code = 500
    elif isinstance(exc, ConversionError):
        error_code = "CONVERSION_ERROR"
        status_code = 500
    elif isinstance(exc, GeneratedCodeNotFoundError):
        error_code = "GENERATED_CODE_NOT_FOUND"
        status_code = 404
    elif isinstance(exc, InvalidFileIdError):
        error_code = "INVALID_FILE_ID"
        status_code = 400
    elif isinstance(exc, GeneratedFileNotFoundError):
        error_code = "GENERATED_FILE_NOT_FOUND"
        status_code = 404
    else:
        error_code = "CODEGEN_ERROR"
        status_code = 500
    
    response = APIResponse.fail(
        code=error_code,
        message=exc.message,
    )
    return JSONResponse(
        status_code=status_code,
        content=response.model_dump(mode="json"),
    )


def register_exception_handlers(app: FastAPI) -> None:
    """Register all exception handlers with the FastAPI app."""
    app.add_exception_handler(ChunkerException, chunker_exception_handler)
    app.add_exception_handler(CodegenException, codegen_exception_handler)
    app.add_exception_handler(DatabaseException, database_exception_handler)
    app.add_exception_handler(ParserException, parser_exception_handler)
    app.add_exception_handler(ProjectException, project_exception_handler)
    app.add_exception_handler(RAGException, rag_exception_handler)
    app.add_exception_handler(SourceFileException, source_file_exception_handler)
    app.add_exception_handler(SQLAlchemyError, sqlalchemy_exception_handler)
    app.add_exception_handler(Exception, generic_exception_handler)



