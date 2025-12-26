"""Source file-specific exceptions."""


class SourceFileException(Exception):
    """Base exception for source file errors."""

    def __init__(self, message: str = "A source file error occurred"):
        self.message = message
        super().__init__(self.message)


class InvalidFileTypeError(SourceFileException):
    """Raised when file type is not supported."""

    def __init__(self, file_type: str, supported_types: list[str] | None = None):
        types_str = ", ".join(supported_types) if supported_types else "unknown"
        message = f"Invalid file type: '{file_type}'. Supported types: {types_str}"
        super().__init__(message)
        self.file_type = file_type
        self.supported_types = supported_types


class FileSizeExceededError(SourceFileException):
    """Raised when one or more files exceed size limits."""

    def __init__(self, violations: list[dict]):
        """
        Args:
            violations: List of dicts with filename, size_mb, limit_mb
        """
        self.violations = violations
        count = len(violations)
        message = f"{count} file(s) exceed size limits"
        super().__init__(message)


class SourceFileNotFoundException(SourceFileException):
    """Raised when a source file is not found."""

    def __init__(self, file_id: str | None = None):
        message = f"Source file not found: {file_id}" if file_id else "Source file not found"
        super().__init__(message)


class FileUploadError(SourceFileException):
    """Raised when file upload fails."""

    def __init__(self, filename: str, reason: str = "Upload failed"):
        message = f"Failed to upload '{filename}': {reason}"
        super().__init__(message)
        self.filename = filename
        self.reason = reason
