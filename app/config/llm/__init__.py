"""OAuth-based LLM and Embeddings configuration.

Usage:
    from app.config.llm import get_llm, get_embeddings, CODEGEN, DOCGEN
    from app.config.llm.models import LLMModel, EmbeddingsModel
    
    # Use default model
    llm = get_llm(CODEGEN)
    response = llm.invoke("Hello!")
    
    # Use specific model
    llm = get_llm(CODEGEN, model=LLMModel.GPT35_TURBO)
    
    # Embeddings
    embeddings = get_embeddings(DOCGEN)
    vector = embeddings.embed_query("Hello!")
"""

from typing import Optional

from app.config.llm.manager import LLMManager, CODEGEN, DOCGEN
from app.config.llm.client import OAuthLLMClient
from app.config.llm.embeddings import OAuthEmbeddings
from app.config.llm.stats import LLMStats, InstanceStats
from app.config.llm.models import (
    LLMModel, 
    EmbeddingsModel,
    DEFAULT_LLM_MODEL,
    DEFAULT_EMBEDDINGS_MODEL,
)


# Singleton manager
_manager: LLMManager | None = None


def get_llm_manager() -> LLMManager:
    """Get the singleton LLM Manager instance."""
    global _manager
    if _manager is None:
        _manager = LLMManager()
        _manager.initialize()
    return _manager


def get_llm(
    instance_name: str, 
    model: Optional[LLMModel] = None
) -> OAuthLLMClient:
    """Get an LLM client by instance name, optionally with a specific model.
    
    Args:
        instance_name: One of CODEGEN or DOCGEN (determines OAuth credentials)
        model: Optional LLMModel enum to use. If not specified, uses default.
               The model determines: model_name, max_tokens, temperature
        
    Returns:
        OAuthLLMClient instance configured for the specified model
        
    Example:
        # Default model
        llm = get_llm(CODEGEN)
        
        # Specific model for lighter tasks
        llm = get_llm(CODEGEN, model=LLMModel.GPT35_TURBO)
    """
    manager = get_llm_manager()
    base_client = manager.get_llm(instance_name)
    
    if model is None:
        return base_client
    
    # Create a new client with the specified model's config
    config = model.config
    return OAuthLLMClient(
        instance_name=instance_name,
        endpoint_url=base_client.endpoint_url,
        token_cache=base_client.token_cache,
        stats_tracker=base_client.stats_tracker,
        model_name=config.name,
        temperature=config.temperature,
        max_tokens=config.max_tokens,
        timeout=base_client.timeout,
        ssl_verify=base_client.ssl_verify,
    )


def get_embeddings(
    instance_name: str,
    model: Optional[EmbeddingsModel] = None
) -> OAuthEmbeddings:
    """Get an Embeddings client by instance name.
    
    Args:
        instance_name: One of CODEGEN or DOCGEN
        model: Optional EmbeddingsModel enum to use
        
    Returns:
        OAuthEmbeddings instance
    """
    manager = get_llm_manager()
    base_client = manager.get_embeddings(instance_name)
    
    if model is None:
        return base_client
    
    # Create a new client with the specified model
    config = model.config
    return OAuthEmbeddings(
        instance_name=instance_name,
        endpoint_url=base_client.endpoint_url,
        token_cache=base_client.token_cache,
        stats_tracker=base_client.stats_tracker,
        model_name=config.name,
        timeout=base_client.timeout,
        ssl_verify=base_client.ssl_verify,
    )


__all__ = [
    # Functions
    "get_llm",
    "get_embeddings", 
    "get_llm_manager",
    # Instance Constants
    "CODEGEN",
    "DOCGEN",
    # Model Enums
    "LLMModel",
    "EmbeddingsModel",
    # Types (for type hints)
    "OAuthLLMClient",
    "OAuthEmbeddings",
    "LLMStats",
    "InstanceStats",
]
