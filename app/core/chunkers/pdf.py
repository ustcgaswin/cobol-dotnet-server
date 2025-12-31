"""PDF chunker using PyPDFLoader."""

from pathlib import Path

from langchain_community.document_loaders import PyPDFLoader
from langchain_core.documents import Document
from langchain_text_splitters import RecursiveCharacterTextSplitter
from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError, UnsupportedFileTypeError


class PDFChunker(BaseChunker):
    """Chunker for PDF documents using PyPDFLoader."""
    
    SUPPORTED_EXTENSIONS = [".pdf"]
    
    def __init__(self, chunk_size: int = 1000, chunk_overlap: int = 200):
        """Initialize PDF chunker.
        
        Args:
            chunk_size: Maximum size of each chunk in characters
            chunk_overlap: Overlap between chunks for context preservation
        """
        self.splitter = RecursiveCharacterTextSplitter(
            chunk_size=chunk_size,
            chunk_overlap=chunk_overlap,
        )
    
    def chunk(self, filepaths: list[str]) -> list[Document]:
        """Chunk PDF files and return documents."""
        if not filepaths:
            return []
        
        # Validate file types
        for filepath in filepaths:
            ext = Path(filepath).suffix.lower()
            if ext not in self.SUPPORTED_EXTENSIONS:
                raise UnsupportedFileTypeError(ext, self.SUPPORTED_EXTENSIONS)
        
        try:
            logger.info(f"Chunking {len(filepaths)} PDF file(s)")
            
            all_docs = []
            for filepath in filepaths:
                loader = PyPDFLoader(filepath)
                docs = loader.load()
                all_docs.extend(docs)
            
            # Split documents into chunks
            chunks = self.splitter.split_documents(all_docs)
            logger.info(f"Created {len(chunks)} chunks from {len(filepaths)} PDF file(s)")
            return chunks
            
        except Exception as e:
            logger.error(f"PDF chunking failed: {e}")
            raise ChunkError(str(e)) from e
