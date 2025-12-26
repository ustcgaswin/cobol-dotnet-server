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
]
