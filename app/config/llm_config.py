"""LLM and embedding model configuration."""

from langchain_anthropic import ChatAnthropic
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_openai import AzureOpenAIEmbeddings

from app.config.settings import settings


def get_llm() -> BaseChatModel:
    """
    Returns the configured LLM based on the provider in settings.
    
    Currently supports:
    - azure_anthropic: Claude via Azure AI Foundry
    
    To add a new provider:
    1. Add settings in settings.py
    2. Add elif block here
    """
    if settings.LLM_PROVIDER == "azure_anthropic":
        return ChatAnthropic(
            model=settings.AZURE_ANTHROPIC_MODEL,
            api_key=settings.AZURE_ANTHROPIC_API_KEY,
            base_url=settings.AZURE_ANTHROPIC_ENDPOINT,
            temperature=settings.LLM_TEMPERATURE,
            max_tokens=settings.LLM_MAX_TOKENS,
            timeout=settings.LLM_TIMEOUT,
        )
    
    raise ValueError(f"Unsupported LLM provider: {settings.LLM_PROVIDER}")


def get_embeddings() -> AzureOpenAIEmbeddings:
    """
    Returns the Azure OpenAI Embeddings instance.
    """
    if not settings.AZURE_OPENAI_EMBED_API_KEY:
        raise ValueError("Azure OpenAI Embeddings API Key is not configured.")

    return AzureOpenAIEmbeddings(
        azure_deployment=settings.AZURE_OPENAI_EMBED_DEPLOYMENT_NAME,
        openai_api_key=settings.AZURE_OPENAI_EMBED_API_KEY,
        azure_endpoint=settings.AZURE_OPENAI_EMBED_API_ENDPOINT,
        api_version=settings.AZURE_OPENAI_EMBED_VERSION or "2023-05-15",
    )


# Initialize at startup for early validation via health check
llm = get_llm()
embeddings = get_embeddings()