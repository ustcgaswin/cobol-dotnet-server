"""Dependency extraction exceptions."""

class DependencyExtractionException(Exception):
    """Base exception for dependency extraction errors."""
    pass


class DependencyExtractionError(DependencyExtractionException):
    """General error during dependency extraction."""
    pass
