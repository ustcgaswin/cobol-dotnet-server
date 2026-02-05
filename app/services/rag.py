"""RAG service for document indexing and search."""

from pathlib import Path

from langchain_core.documents import Document
from loguru import logger

from app.config.settings import settings
from app.config.llm import get_embeddings, get_llm, DOCGEN, LLMModel
from app.core.chunkers import get_chunker, CHUNKER_REGISTRY
from app.core.utils import find_files
from app.core.vector_store import FAISSVectorStore


class RAGService:
    """Service for RAG indexing and search operations."""
    
    # Supported extensions from chunker registry
    SUPPORTED_EXTENSIONS = list(CHUNKER_REGISTRY.keys())
    
    def __init__(self):
        self.store = FAISSVectorStore(
            embeddings=get_embeddings(DOCGEN),
            index_path=settings.FAISS_INDEX_PATH,
        )
        self.data_path = Path(settings.RAG_DATA_PATH)
    
    def build_index(self, force_rebuild: bool = False) -> dict:
        """Build the FAISS index from documents in data folder.
        
        Args:
            force_rebuild: If True, delete existing index first
            
        Returns:
            Status dict with document counts
        """
        if self.store.exists() and not force_rebuild:
            logger.info("Index already exists, skipping build")
            return {
                "status": "skipped",
                "message": "Index already exists. Use force_rebuild=True to rebuild.",
                "document_count": self.store.document_count,
            }
        
        if force_rebuild and self.store.exists():
            self.store.delete()
        
        # Find all supported files
        files = find_files(str(self.data_path), self.SUPPORTED_EXTENSIONS)
        if not files:
            logger.warning(f"No documents found in {self.data_path}")
            return {
                "status": "no_documents",
                "message": f"No .pdf or .md files found in {self.data_path}",
                "document_count": 0,
            }
        
        logger.info(f"Found {len(files)} files to index")
        
        # Group files by extension and chunk with appropriate chunker
        documents = []
        files_by_ext: dict[str, list[str]] = {}
        for filepath in files:
            ext = Path(filepath).suffix.lower()
            files_by_ext.setdefault(ext, []).append(filepath)
        
        for ext, ext_files in files_by_ext.items():
            chunker = get_chunker(ext)
            docs = chunker.chunk(ext_files)
            documents.extend(docs)
        
        # Add source folder metadata to help with filtering
        for doc in documents:
            # Extract source folder from file path
            source_path = Path(doc.metadata.get("source", ""))
            try:
                relative = source_path.relative_to(self.data_path.resolve())
                doc.metadata["source_folder"] = relative.parts[0] if relative.parts else "root"
            except ValueError:
                doc.metadata["source_folder"] = "unknown"
        
        # Add to store and save
        count = self.store.add_documents(documents)
        self.store.save()
        
        return {
            "status": "success",
            "message": f"Indexed {count} document chunks from {len(files)} files",
            "document_count": count,
            "files_processed": len(files),
        }
    
    def search(
        self, query: str, k: int = 4, source_folder: str | None = None
    ) -> list[Document]:
        """Search the index.
        
        Args:
            query: Search query
            k: Number of results to return
            source_folder: Optional filter by source folder (e.g., "cobol", "db2")
        """
        if not self.store.exists():
            if not self.store.load():
                logger.warning("No index available for search")
                return []
        
        filter_dict = {"source_folder": source_folder} if source_folder else None
        return self.store.similarity_search(query, k=k, filter=filter_dict)
    
    def delete_index(self) -> dict:
        """Delete the index."""
        if not self.store.exists():
            return {"status": "not_found", "message": "No index to delete"}
        
        self.store.delete()
        return {"status": "deleted", "message": "Index deleted successfully"}
    
    def get_status(self) -> dict:
        """Get index status."""
        exists = self.store.exists()
        if exists and not self.store.is_loaded:
            self.store.load()
        
        return {
            "exists": exists,
            "document_count": self.store.document_count if exists else 0,
            "index_path": str(self.store.index_path),
            "data_path": str(self.data_path),
        }
    
    def load_if_exists(self) -> bool:
        """Load index if it exists. Called at startup."""
        if self.store.exists():
            return self.store.load()
        return False
    
    def ask(
        self, query: str, k: int = 4, source_folder: str | None = None
    ) -> dict:
        """Ask a question and get an LLM-synthesized answer.
        
        Args:
            query: The question to answer
            k: Number of chunks to retrieve for context
            source_folder: Optional filter by source folder
            
        Returns:
            Dict with answer, sources, and metadata
        """
        llm = get_llm(DOCGEN, model=LLMModel.GPT4O_DEV)
        
        # Retrieve relevant chunks
        chunks = self.search(query, k=k, source_folder=source_folder)
        
        if not chunks:
            return {
                "answer": "No relevant documents found to answer this question.",
                "sources": [],
                "chunks_used": 0,
            }
        
        # Build context from chunks
        context = "\n\n---\n\n".join([
            f"Source: {chunk.metadata.get('source', 'Unknown')}\n{chunk.page_content}"
            for chunk in chunks
        ])
        
        # Create prompt
        prompt = f"""Based on the following context, answer the question. 
If the answer is not in the context, say "I don't have enough information to answer this question."

Context:
{context}

Question: {query}

Answer:"""
        
        # Get LLM response
        response = llm.invoke(prompt)
        answer = response.content if hasattr(response, 'content') else str(response)
        
        # Return result with sources
        sources = [
            {"source": chunk.metadata.get("source", "Unknown"), "content_preview": chunk.page_content[:200]}
            for chunk in chunks
        ]
        
        return {
            "answer": answer,
            "sources": sources,
            "chunks_used": len(chunks),
        }


# Singleton instance
rag_service = RAGService()

