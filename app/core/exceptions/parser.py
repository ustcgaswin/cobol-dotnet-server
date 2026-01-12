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


class PARMLIBParseError(ParseError):
    """Base exception for PARMLIB parsing failures."""
    pass


class InvalidStatementError(PARMLIBParseError):
    """Raised when a PARMLIB statement has invalid syntax."""
    pass


class SectionParseError(PARMLIBParseError):
    """Raised when section parsing fails."""
    pass


class UtilityCommandError(PARMLIBParseError):
    """Raised when utility command parsing fails."""
    pass

class REXXParseError(ParseError):
    """Raised when REXX parsing fails."""
    pass


class InvalidREXXSyntaxError(ParseError):
    """Raised when invalid REXX syntax is encountered."""
    pass


class UnsupportedREXXFeatureError(ParseError):
    """Raised when an unsupported REXX feature is encountered."""
    pass

class EmptyFileError(ParseError):
    """Exception raised when a file is empty or contains no valid data.
    
    Attributes:
        message: Description of the error
        filename: Path to the file being parsed
    """
    
    def __init__(self, message: str, filename: str = None):
        super().__init__(message, filename)


class InvalidLayoutError(ParseError):
    """Exception raised when a fixed-length file layout is invalid.
    
    Attributes:
        message: Description of the error
        filename: Path to the file being parsed
    """
    
    def __init__(self, message: str, filename: str = None):
        super().__init__(message, filename)


class SchemaInferenceError(ParseError):
    """Exception raised when schema inference fails.
    
    Attributes:
        message: Description of the error
        filename: Path to the file being parsed
        column_name: Column where inference failed
    """
    
    def __init__(self, message: str, filename: str = None, column_name: str = None):
        super().__init__(message, filename)
        self.column_name = column_name


class LLMAugmentationError(ParseError):
    """Exception raised when LLM augmentation fails.
    
    Attributes:
        message: Description of the error
        filename: File being augmented
        retry_count: Number of retries attempted
        llm_error: Original error from LLM call
    """
    
    def __init__(
        self,
        message: str,
        filename: str = None,
        retry_count: int = None,
        llm_error: Exception = None
    ):
        super().__init__(message, filename)
        self.retry_count = retry_count
        self.llm_error = llm_error
    
    def __str__(self):
        parts = [self.args[0]]
        if self.filename:
            parts.append(f"File: {self.filename}")
        if self.retry_count:
            parts.append(f"Retries: {self.retry_count}")
        if self.llm_error:
            parts.append(f"LLM Error: {str(self.llm_error)[:100]}")
        return " | ".join(parts)