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
from app.core.parsers.bind_parser import BindParser
from app.core.parsers.parmlib_parser import PARMLIBParser
from app.core.parsers.rexx_parser import REXXParser
from app.core.parsers.flatfile_parser import CSVParser, FixedLengthParser
from app.core.parsers.assembly_parser import AssemblyParser # <--- 1. Import


# Registry mapping file types to parser classes
PARSER_REGISTRY: dict[str, type[BaseParser]] = {
    SourceFileType.COBOL.value: CobolParser,
    SourceFileType.COPYBOOK.value: CopybookParser,
    SourceFileType.PLI.value: PLIParser,
    SourceFileType.PLI_COPYBOOK.value: PLICopybookParser,
    SourceFileType.DCLGEN.value: DclgenParser,  
    SourceFileType.JCL.value: JCLParser,
    SourceFileType.PROC.value: JCLParser,
    SourceFileType.CA7.value: CA7Parser,
    SourceFileType.BIND.value: BindParser,
    SourceFileType.CSV.value: CSVParser,
    SourceFileType.FIXED_LENGTH.value: FixedLengthParser,
    SourceFileType.PARMLIB.value: PARMLIBParser,
    SourceFileType.REXX.value: REXXParser,
    SourceFileType.ASSEMBLY.value: AssemblyParser, 
}

def get_parser(file_type: str) -> BaseParser:
    """Get a parser instance for the given file type.
    
    Args:
        file_type: Type of file to parse (e.g., "cobol", "pli", "csv", "fixed_length")
        
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
    "CSVParser",
    "FixedLengthParser",
    "SourceFileType",
    "PARSER_REGISTRY",
    "REXXParser",
    "get_parser",
    "JCLParser",
    "CA7Parser",
    "BindParser",
    "PARMLIBParser",
    "AssemblyParser",
    "DclgenParser",
]