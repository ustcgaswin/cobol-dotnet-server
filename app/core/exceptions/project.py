"""Project-specific exceptions."""


class ProjectException(Exception):
    """Base exception for project-related errors."""

    def __init__(self, message: str = "A project error occurred"):
        self.message = message
        super().__init__(self.message)


class ProjectNotFoundException(ProjectException):
    """Raised when a project is not found."""

    def __init__(self, project_id: str | None = None):
        message = f"Project not found: {project_id}" if project_id else "Project not found"
        super().__init__(message)


class ProjectCreationError(ProjectException):
    """Raised when project creation fails."""

    def __init__(self, message: str = "Failed to create project"):
        super().__init__(message)


class ProjectValidationError(ProjectException):
    """Raised when project data validation fails."""

    def __init__(self, message: str = "Project validation failed"):
        super().__init__(message)
