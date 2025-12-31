"""Chunker-related exceptions."""


class ChunkerException(Exception):
    """Base exception for chunker errors."""
    pass


class ChunkError(ChunkerException):
    """Raised when chunking fails."""
    
    def __init__(self, message: str, filepath: str | None = None):
        self.filepath = filepath
        if filepath:
            message = f"{filepath}: {message}"
        super().__init__(message)


class UnsupportedFileTypeError(ChunkerException):
    """Raised when file type is not supported by chunker."""
    
    def __init__(self, extension: str, supported: list[str]):
        self.extension = extension
        self.supported = supported
        message = f"Unsupported file type '{extension}'. Supported: {', '.join(supported)}"
        super().__init__(message)
