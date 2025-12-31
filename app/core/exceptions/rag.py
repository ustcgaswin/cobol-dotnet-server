"""RAG-related exceptions."""


class RAGException(Exception):
    """Base exception for RAG errors."""
    pass


class IndexNotFoundError(RAGException):
    """Raised when index is not found."""
    
    def __init__(self, index_path: str):
        self.index_path = index_path
        super().__init__(f"RAG index not found at {index_path}")


class IndexBuildError(RAGException):
    """Raised when index building fails."""
    
    def __init__(self, message: str):
        super().__init__(f"Failed to build RAG index: {message}")


class SearchError(RAGException):
    """Raised when search fails."""
    
    def __init__(self, message: str):
        super().__init__(f"RAG search failed: {message}")
