"""OAuth-based LLM and Embeddings configuration.

Usage:
    from app.config.llm import get_llm, get_embeddings, CODEGEN, DOCGEN
    
    llm = get_llm(CODEGEN)
    response = llm.invoke("Hello!")
    
    embeddings = get_embeddings(CODEGEN)
    vector = embeddings.embed_query("Hello!")
"""

from app.config.llm.manager import LLMManager, CODEGEN, DOCGEN
from app.config.llm.client import OAuthLLMClient
from app.config.llm.embeddings import OAuthEmbeddings
from app.config.llm.stats import LLMStats, InstanceStats


# Singleton manager
_manager: LLMManager | None = None


def get_llm_manager() -> LLMManager:
    """Get the singleton LLM Manager instance."""
    global _manager
    if _manager is None:
        _manager = LLMManager()
        _manager.initialize()
    return _manager


def get_llm(instance_name: str) -> OAuthLLMClient:
    """Get an LLM client by instance name.
    
    Args:
        instance_name: One of CODEGEN or DOCGEN
        
    Returns:
        OAuthLLMClient instance
        
    Example:
        llm = get_llm(CODEGEN)
        response = llm.invoke("Generate code for...")
    """
    return get_llm_manager().get_llm(instance_name)


def get_embeddings(instance_name: str) -> OAuthEmbeddings:
    """Get an Embeddings client by instance name.
    
    Args:
        instance_name: One of CODEGEN or DOCGEN
        
    Returns:
        OAuthEmbeddings instance
    """
    return get_llm_manager().get_embeddings(instance_name)


__all__ = [
    # Functions
    "get_llm",
    "get_embeddings", 
    "get_llm_manager",
    # Constants
    "CODEGEN",
    "DOCGEN",
    # Types (for type hints)
    "OAuthLLMClient",
    "OAuthEmbeddings",
    "LLMStats",
    "InstanceStats",
]
