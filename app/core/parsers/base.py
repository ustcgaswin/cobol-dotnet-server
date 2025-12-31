"""Abstract base class for file parsers."""

from abc import ABC, abstractmethod


class BaseParser(ABC):
    """Abstract interface for file parsers.
    
    All parsers must inherit from this class and implement the parse_file method.
    """
    
    # File type this parser handles (e.g., "cobol", "copybook")
    FILE_TYPE: str = ""
    
    @abstractmethod
    def parse_file(self, filepath: str) -> dict:
        """Parse a single file and return structured data.
        
        Args:
            filepath: Path to the file to parse
            
        Returns:
            Dictionary containing parsed data
            
        Raises:
            FileNotFoundError: If file doesn't exist
            ParseError: If parsing fails
        """
        pass
