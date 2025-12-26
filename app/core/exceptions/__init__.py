"""Core exceptions for the application."""

from app.core.exceptions.db_exceptions import (
    DatabaseConnectionError,
    DatabaseException,
    DatabaseHealthCheckError,
)

__all__ = [
    "DatabaseException",
    "DatabaseConnectionError",
    "DatabaseHealthCheckError",
]
