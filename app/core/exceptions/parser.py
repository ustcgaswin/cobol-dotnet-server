"""Parser-related exceptions."""


class ParserException(Exception):
    """Base exception for parser errors."""
    pass


class ParserNotFoundError(ParserException):
    """Raised when no parser exists for a file type."""
    pass


class ParseError(ParserException):
    """Raised when file parsing fails.
    
    Base class for all parsing errors with optional line number and content tracking.
    """
    
    def __init__(
        self, 
        message: str, 
        filename: str | None = None,
        line_number: int | None = None,
        content: str | None = None,
    ):
        self.filename = filename
        self.line_number = line_number
        self.content = content
        
        if line_number:
            message = f"Line {line_number}: {message}"
        if filename:
            message = f"{filename}: {message}"
        if content:
            message = f"{message}\n  Content: {content[:100]}..."
        
        super().__init__(message)


class InvalidSyntaxError(ParseError):
    """Raised when invalid COBOL syntax is encountered."""
    pass


class UnsupportedFeatureError(ParseError):
    """Raised when an unsupported COBOL feature is encountered."""
    pass


class UnrecognizedClauseError(ParseError):
    """Raised when an unrecognized clause is found."""
    pass


class CobolParseError(ParseError):
    """Raised when COBOL parsing fails."""
    pass


class CopybookParseError(ParseError):
    """Raised when copybook parsing fails."""
    pass


class CA7ParseError(Exception):
    """Base exception for CA-7 parsing errors."""
    pass

class BindParseError(Exception):
    """Base exception for Bind parsing errors."""
    pass

class FlatFileParseError(ParseError):
    """Base exception for flat file parsing errors."""
    pass


class CSVParseError(FlatFileParseError):
    """Raised when CSV parsing fails."""
    pass


class FixedLengthParseError(FlatFileParseError):
    """Raised when fixed-length file parsing fails."""
    pass


class EmptyFileError(FlatFileParseError):
    """Raised when file contains no valid data."""
    pass


class SchemaInferenceError(FlatFileParseError):
    """Raised when schema cannot be inferred."""
    pass


class InvalidLayoutError(FlatFileParseError):
    """Raised when provided layout is invalid."""
    pass


class EncodingDetectionError(FlatFileParseError):
    """Raised when encoding cannot be determined."""
    pass
