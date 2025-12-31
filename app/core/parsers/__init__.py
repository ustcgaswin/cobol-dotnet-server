"""Parser module for COBOL and related file types."""

from app.core.parsers.base import BaseParser
from app.core.parsers.cobol_parser import CobolParser
from app.core.parsers.copybook_parser import CopybookParser

# Registry mapping file types to parser classes
PARSER_REGISTRY: dict[str, type[BaseParser]] = {
    "cobol": CobolParser,
    "copybook": CopybookParser,
}


def get_parser(file_type: str) -> BaseParser:
    """Get a parser instance for the given file type.
    
    Args:
        file_type: Type of file to parse (e.g., "cobol", "copybook")
        
    Returns:
        Parser instance for the file type
        
    Raises:
        ParserNotFoundError: If no parser exists for the file type
    """
    from app.core.exceptions import ParserNotFoundError
    
    parser_class = PARSER_REGISTRY.get(file_type)
    if not parser_class:
        raise ParserNotFoundError(f"No parser registered for file type: {file_type}")
    return parser_class()


__all__ = [
    "BaseParser",
    "CobolParser", 
    "CopybookParser",
    "PARSER_REGISTRY",
    "get_parser",
]
