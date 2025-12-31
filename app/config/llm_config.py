from langchain_openai import AzureChatOpenAI, AzureOpenAIEmbeddings, ChatOpenAI
from langchain_anthropic import ChatAnthropic
from langchain_core.language_models.chat_models import BaseChatModel
from .settings import settings
# from langchain_azure_ai import AzureAIChatCompletions

def get_llm() -> BaseChatModel:
    """
    Returns the configured LLM based on the provider in settings.
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
 
    
    elif settings.LLM_PROVIDER == "azure_openai":
        # Standard Azure OpenAI integration
        return AzureChatOpenAI(
            azure_deployment=settings.AZURE_ANTHROPIC_MODEL, # In Azure, model usually matches deployment name
            api_key=settings.AZURE_ANTHROPIC_API_KEY,
            azure_endpoint=settings.AZURE_ANTHROPIC_ENDPOINT,
            api_version="2024-02-01", # Update as per your Azure requirements
            temperature=settings.LLM_TEMPERATURE,
            max_tokens=settings.LLM_MAX_TOKENS,
            timeout=settings.LLM_TIMEOUT,
        )
    
    else:
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
        chunk_size=1  # Recommended for Azure
    )

# Pre-initialize instances for reuse across the LangGraph
llm = get_llm()
embeddings = get_embeddings()