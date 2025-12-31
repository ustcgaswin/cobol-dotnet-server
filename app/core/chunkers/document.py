"""Document chunker using Docling for PDF and Markdown files."""

from pathlib import Path

import tiktoken
from docling.chunking import HybridChunker
from langchain_core.documents import Document
from langchain_docling import DoclingLoader
from langchain_docling.loader import ExportType
from loguru import logger

from app.config.settings import settings
from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError, UnsupportedFileTypeError


class DocumentChunker(BaseChunker):
    """Chunker for PDF and Markdown documents using Docling.
    
    Uses tiktoken encoding from settings (compatible with text-embedding-3-large)
    to ensure chunks don't exceed embedding model token limits.
    """
    
    SUPPORTED_EXTENSIONS = [".pdf", ".md"]
    
    def __init__(self, encoding_name: str | None = None):
        """Initialize chunker with tokenizer.
        
        Args:
            encoding_name: tiktoken encoding name. Defaults to settings.EMBEDDING_ENCODING
        """
        encoding = encoding_name or settings.EMBEDDING_ENCODING
        self.tokenizer = tiktoken.get_encoding(encoding)
    
    def chunk(self, filepaths: list[str]) -> list[Document]:
        """Chunk files and return documents.
        
        Args:
            filepaths: List of file paths to chunk
            
        Returns:
            List of Document objects with page_content and metadata
        """
        if not filepaths:
            return []
        
        # Validate file types
        for filepath in filepaths:
            ext = Path(filepath).suffix.lower()
            if ext not in self.SUPPORTED_EXTENSIONS:
                raise UnsupportedFileTypeError(ext, self.SUPPORTED_EXTENSIONS)
        
        try:
            logger.info(f"Chunking {len(filepaths)} file(s)")
            
            loader = DoclingLoader(
                file_path=filepaths,
                export_type=ExportType.DOC_CHUNKS,
                chunker=HybridChunker(tokenizer=self.tokenizer),
            )
            documents = loader.load()
            
            logger.info(f"Created {len(documents)} chunks from {len(filepaths)} file(s)")
            return documents
            
        except Exception as e:
            logger.error(f"Chunking failed: {e}")
            raise ChunkError(str(e)) from e
