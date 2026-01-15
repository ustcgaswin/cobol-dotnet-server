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
from app.core.exceptions.dependency import (
    DependencyExtractionError,
    DependencyExtractionException,
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
    EmptyFileError,
    InvalidLayoutError,
    SchemaInferenceError,
    LLMAugmentationError,
    PARMLIBParseError,
    InvalidStatementError,
    SectionParseError,
    UtilityCommandError,
    REXXParseError,
    InvalidREXXSyntaxError,
    UnsupportedREXXFeatureError,
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
from app.core.exceptions.summarizer import (
    LLMContextError,
    SummarizationError,
    SummarizationException,
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
    "EmptyFileError",
    "InvalidLayoutError",
    "SchemaInferenceError",
    "LLMAugmentationError",
    # ParmLib Parser
    "PARMLIBParseError",
    "InvalidStatementError",
    "SectionParseError",
    "UtilityCommandError",
    # REXX Parser
    "REXXParseError",
    "InvalidREXXSyntaxError",
    "UnsupportedREXXFeatureError",
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
    # Summarizer
    "SummarizationException",
    "SummarizationError",
    "LLMContextError",
    # Dependency
    "DependencyExtractionException",
    "DependencyExtractionError",
]