"""Parser-related exceptions - Final simplified version."""


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


# COBOL/Copybook exceptions
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


# CA-7 exceptions
class CA7ParseError(Exception):
    """Base exception for CA-7 parsing errors."""
    pass


# PARMLIB exceptions (SIMPLIFIED - removed InvalidStatementError, SectionParseError, UtilityCommandError)
class PARMLIBParseError(ParseError):
    """Base exception for PARMLIB parsing failures."""
    pass


# REXX exceptions (SIMPLIFIED)
class REXXParseError(ParseError):
    """Raised when REXX parsing fails."""
    pass


# General file parsing exceptions
class EncodingDetectionError(ParseError):
    """Raised when encoding cannot be determined."""
    pass


class EmptyFileError(ParseError):
    """Exception raised when a file is empty or contains no valid data.
    
    Attributes:
        message: Description of the error
        filename: Path to the file being parsed
    """
    
    def __init__(self, message: str, filename: str = None):
        super().__init__(message, filename)