"""Chunker module for document chunking."""

from app.core.chunkers.base import BaseChunker
from app.core.chunkers.cobol import CobolChunker
from app.core.chunkers.copybook import CopybookChunker
from app.core.chunkers.markdown import MarkdownChunker
from app.core.chunkers.pdf import PDFChunker
from app.core.chunkers.pli import PliChunker
from app.core.chunkers.pli_copybook import PliCopybookChunker
from app.core.chunkers.jcl import JclChunker
from app.core.chunkers.ca7 import Ca7Chunker

# Registry mapping extensions to chunker classes
CHUNKER_REGISTRY: dict[str, type[BaseChunker]] = {
    ".pdf": PDFChunker,
    ".md": MarkdownChunker,
    ".cbl": CobolChunker,
    ".cob": CobolChunker,
    ".cobol": CobolChunker,
    ".cpy": CopybookChunker,
    ".copy": CopybookChunker,
    ".pli": PliChunker,
    ".pl1": PliChunker,
    ".inc": PliCopybookChunker,
    ".mac": PliCopybookChunker,
    ".proc": JclChunker,
    ".jcl": JclChunker,
    ".ca7": Ca7Chunker,
    ".ljob": Ca7Chunker,
    # ".txt":Ca7Chunker,
    ".job":Ca7Chunker
}


def get_chunker(extension: str) -> BaseChunker:
    """Get a chunker instance for the given file extension.
    
    Args:
        extension: File extension (e.g., ".pdf", ".md")
        
    Returns:
        Chunker instance for the file type
        
    Raises:
        ValueError: If no chunker exists for the extension
    """
    # Normalize extension
    if not extension.startswith("."):
        extension = f".{extension}"
    
    chunker_class = CHUNKER_REGISTRY.get(extension)
    if not chunker_class:
        supported = list(CHUNKER_REGISTRY.keys())
        raise ValueError(f"No chunker for extension '{extension}'. Supported: {supported}")
    
    return chunker_class()


__all__ = [
    "BaseChunker",
    "MarkdownChunker",
    "PDFChunker",
    "PliChunker",
    "PliCopybookChunker",
    "CHUNKER_REGISTRY",
    "get_chunker",
    "JclChunker",
    "Ca7Chunker"
]
