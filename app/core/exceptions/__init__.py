"""Core exceptions for the application."""

from app.core.exceptions.chunker import (
    ChunkerException,
    ChunkError,
    UnsupportedFileTypeError,
)
from app.core.exceptions.db_exceptions import (
    DatabaseConnectionError,
    DatabaseException,
    DatabaseHealthCheckError,
)
from app.core.exceptions.parser import (
    CA7ParseError,
    BindParseError,
    CobolParseError,
    CopybookParseError,
    CSVParseError,
    EmptyFileError,
    EncodingDetectionError,
    FixedLengthParseError,
    FlatFileParseError,
    InvalidLayoutError,
    InvalidSyntaxError,
    ParseError,
    ParserException,
    ParserNotFoundError,
    SchemaInferenceError,
    UnrecognizedClauseError,
    UnsupportedFeatureError,
)
from app.core.exceptions.project import (
    ProjectCreationError,
    ProjectException,
    ProjectNotFoundException,
    ProjectValidationError,
)
from app.core.exceptions.rag import (
    IndexBuildError,
    IndexNotFoundError,
    RAGException,
    SearchError,
)
from app.core.exceptions.source_file import (
    FileSizeExceededError,
    FileUploadError,
    InvalidFileTypeError,
    SourceFileException,
    SourceFileNotFoundException,
)

__all__ = [
    # Chunker
    "ChunkerException",
    "ChunkError",
    "UnsupportedFileTypeError",
    # Database
    "DatabaseException",
    "DatabaseConnectionError",
    "DatabaseHealthCheckError",
    # Parser
    "ParserException",
    "ParserNotFoundError",
    "ParseError",
    "InvalidSyntaxError",
    "UnsupportedFeatureError",
    "UnrecognizedClauseError",
    "CobolParseError",
    "CopybookParseError",
    "CA7ParseError",
    # Flat File Parser
    "FlatFileParseError",
    "CSVParseError",
    "FixedLengthParseError",
    "EmptyFileError",
    "SchemaInferenceError",
    "InvalidLayoutError",
    "EncodingDetectionError",
    # Project
    "ProjectException",
    "ProjectNotFoundException",
    "ProjectCreationError",
    "ProjectValidationError",
    # RAG
    "RAGException",
    "IndexNotFoundError",
    "IndexBuildError",
    "SearchError",
    # Source File
    "SourceFileException",
    "InvalidFileTypeError",
    "FileSizeExceededError",
    "SourceFileNotFoundException",
    "FileUploadError",
]