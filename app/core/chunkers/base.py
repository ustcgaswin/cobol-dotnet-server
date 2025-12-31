"""Abstract base class for document chunkers."""

from abc import ABC, abstractmethod

from langchain_core.documents import Document


class BaseChunker(ABC):
    """Abstract interface for document chunkers.
    
    All chunkers must inherit from this class and implement the chunk method.
    """
    
    # File extensions this chunker supports (e.g., [".pdf", ".md"])
    SUPPORTED_EXTENSIONS: list[str] = []
    
    @abstractmethod
    def chunk(self, filepaths: list[str]) -> list[Document]:
        """Chunk files and return documents.
        
        Args:
            filepaths: List of file paths to chunk
            
        Returns:
            List of Document objects with page_content and metadata
            
        Raises:
            ChunkError: If chunking fails
        """
        pass
