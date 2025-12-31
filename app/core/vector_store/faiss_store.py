"""FAISS vector store implementation."""

import shutil
from pathlib import Path

from langchain_community.vectorstores import FAISS
from langchain_core.documents import Document
from langchain_core.embeddings import Embeddings
from loguru import logger

from app.core.vector_store.base import BaseVectorStore


class FAISSVectorStore(BaseVectorStore):
    """FAISS-based vector store with persistence."""
    
    def __init__(self, embeddings: Embeddings, index_path: str):
        """Initialize FAISS store.
        
        Args:
            embeddings: Embedding model to use
            index_path: Path to persist/load the index
        """
        self.embeddings = embeddings
        self.index_path = Path(index_path)
        self._store: FAISS | None = None
    
    def add_documents(self, documents: list[Document]) -> int:
        """Add documents to the store."""
        if not documents:
            return 0
        
        if self._store is None:
            # Create new store from documents
            self._store = FAISS.from_documents(documents, self.embeddings)
        else:
            # Add to existing store
            self._store.add_documents(documents)
        
        logger.info(f"Added {len(documents)} documents to FAISS store")
        return len(documents)
    
    def similarity_search(
        self, query: str, k: int = 4, filter: dict | None = None
    ) -> list[Document]:
        """Search for similar documents."""
        if self._store is None:
            logger.warning("FAISS store not initialized")
            return []
        
        if filter:
            return self._store.similarity_search(query, k=k, filter=filter)
        return self._store.similarity_search(query, k=k)
    
    def save(self) -> None:
        """Save index to disk."""
        if self._store is None:
            logger.warning("No store to save")
            return
        
        self.index_path.mkdir(parents=True, exist_ok=True)
        self._store.save_local(str(self.index_path))
        logger.info(f"Saved FAISS index to {self.index_path}")
    
    def load(self) -> bool:
        """Load index from disk. Returns True if successful."""
        if not self.exists():
            logger.info(f"No FAISS index found at {self.index_path}")
            return False
        
        self._store = FAISS.load_local(
            str(self.index_path),
            self.embeddings,
            allow_dangerous_deserialization=True,
        )
        logger.info(f"Loaded FAISS index from {self.index_path}")
        return True
    
    def delete(self) -> None:
        """Delete persisted index."""
        if self.index_path.exists():
            shutil.rmtree(self.index_path)
            logger.info(f"Deleted FAISS index at {self.index_path}")
        self._store = None
    
    def exists(self) -> bool:
        """Check if index exists on disk."""
        return (self.index_path / "index.faiss").exists()
    
    @property
    def document_count(self) -> int:
        """Number of documents in store."""
        if self._store is None:
            return 0
        return self._store.index.ntotal
    
    @property
    def is_loaded(self) -> bool:
        """Check if store is loaded in memory."""
        return self._store is not None

