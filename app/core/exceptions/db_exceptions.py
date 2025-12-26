"""Custom database exceptions for handling database-related errors."""


class DatabaseException(Exception):
    """Base exception for all database-related errors."""

    def __init__(self, message: str = "A database error occurred"):
        self.message = message
        super().__init__(self.message)


class DatabaseConnectionError(DatabaseException):
    """Raised when database connection fails."""

    def __init__(self, message: str = "Failed to connect to the database"):
        super().__init__(message)


class DatabaseHealthCheckError(DatabaseException):
    """Raised when database health check fails."""

    def __init__(self, message: str = "Database health check failed"):
        super().__init__(message)
