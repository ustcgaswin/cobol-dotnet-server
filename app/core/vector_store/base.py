"""Abstract base class for vector stores."""

from abc import ABC, abstractmethod

from langchain_core.documents import Document


class BaseVectorStore(ABC):
    """Abstract interface for vector stores."""
    
    @abstractmethod
    def add_documents(self, documents: list[Document]) -> int:
        """Add documents to the store. Returns count of documents added."""
        pass
    
    @abstractmethod
    def similarity_search(
        self, query: str, k: int = 4, filter: dict | None = None
    ) -> list[Document]:
        """Search for similar documents."""
        pass
    
    @abstractmethod
    def save(self) -> None:
        """Persist the store to disk."""
        pass
    
    @abstractmethod
    def load(self) -> bool:
        """Load the store from disk. Returns True if loaded successfully."""
        pass
    
    @abstractmethod
    def delete(self) -> None:
        """Delete the persisted store."""
        pass
    
    @abstractmethod
    def exists(self) -> bool:
        """Check if persisted store exists."""
        pass
    
    @property
    @abstractmethod
    def document_count(self) -> int:
        """Number of documents in the store."""
        pass
