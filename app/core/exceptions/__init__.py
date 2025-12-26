"""Core exceptions for the application."""

from app.core.exceptions.db_exceptions import (
    DatabaseConnectionError,
    DatabaseException,
    DatabaseHealthCheckError,
)
from app.core.exceptions.project import (
    ProjectCreationError,
    ProjectException,
    ProjectNotFoundException,
    ProjectValidationError,
)
from app.core.exceptions.source_file import (
    FileSizeExceededError,
    FileUploadError,
    InvalidFileTypeError,
    SourceFileException,
    SourceFileNotFoundException,
)

__all__ = [
    # Database
    "DatabaseException",
    "DatabaseConnectionError",
    "DatabaseHealthCheckError",
    # Project
    "ProjectException",
    "ProjectNotFoundException",
    "ProjectCreationError",
    "ProjectValidationError",
    # Source File
    "SourceFileException",
    "InvalidFileTypeError",
    "FileSizeExceededError",
    "SourceFileNotFoundException",
    "FileUploadError",
]
