"""Parser module for COBOL, PL/I and related file types."""


from app.db.enums import SourceFileType
from app.core.parsers.base import BaseParser
from app.core.parsers.cobol_parser import CobolParser
from app.core.parsers.copybook_parser import CopybookParser
from app.core.parsers.pli_parser import PLIParser
from app.core.parsers.pli_copybook_parser import PLICopybookParser
from app.core.parsers.dclgen_parser import DclgenParser
from app.core.parsers.jcl_parser import JCLParser
from app.core.parsers.ca7_parser import CA7Parser


# Registry mapping file types to parser classes
PARSER_REGISTRY: dict[str, type[BaseParser]] = {
    SourceFileType.COBOL.value: CobolParser,
    SourceFileType.COPYBOOK.value: CopybookParser,
    SourceFileType.PLI.value: PLIParser,
    SourceFileType.PLI_COPYBOOK.value: PLICopybookParser,
    SourceFileType.DCLGEN.value: DclgenParser,  
    SourceFileType.JCL.value: JCLParser,
    SourceFileType.CA7.value: CA7Parser
}

def get_parser(file_type: str) -> BaseParser:
    """Get a parser instance for the given file type.
    
    Args:
        file_type: Type of file to parse (e.g., "cobol", "pli")
        
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
    "PLIParser",
    "PLICopybookParser",
    "DclgenParser", 
    "SourceFileType",
    "PARSER_REGISTRY",
    "get_parser",
    "JCLParser",
    "CA7Parser"
]