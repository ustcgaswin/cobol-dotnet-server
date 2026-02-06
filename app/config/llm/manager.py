"""LLM Manager - central manager for all LLM and Embeddings instances."""

from loguru import logger

from app.config.settings import settings
from app.config.llm.token_cache import TokenCache
from app.config.llm.client import OAuthLLMClient
from app.config.llm.embeddings import OAuthEmbeddings
from app.config.llm.stats import LLMStats


# Instance name constants
CODEGEN = "codegen"
DOCGEN = "docgen"


class LLMManager:
    """Manages multiple named LLM and Embeddings instances.
    
    Each instance has its own OAuth credentials and shares a TokenCache
    between its LLM and Embeddings clients.
    """
    
    def __init__(self):
        self._llm_instances: dict[str, OAuthLLMClient] = {}
        self._embeddings_instances: dict[str, OAuthEmbeddings] = {}
        self._token_caches: dict[str, TokenCache] = {}
        self._stats = LLMStats()
        self._initialized = False
    
    def initialize(self) -> None:
        """Initialize all configured instances."""
        if self._initialized:
            return
        
        logger.info("[LLMManager] Initializing...")
        
        instances = {
            CODEGEN: {
                "client_id": settings.CODEGEN_CLIENT_ID,
                "client_secret": settings.CODEGEN_CLIENT_SECRET,
            },
            DOCGEN: {
                "client_id": settings.DOCGEN_CLIENT_ID,
                "client_secret": settings.DOCGEN_CLIENT_SECRET,
            },
        }
        
        for name, creds in instances.items():
            if not creds["client_id"] or not creds["client_secret"]:
                logger.warning(f"[LLMManager] Skipping {name}: missing credentials")
                continue
            
            token_cache = TokenCache(
                instance_name=name,
                client_id=creds["client_id"],
                client_secret=creds["client_secret"],
                auth_url=settings.OAUTH_AUTH_URL,
                scope=settings.OAUTH_SCOPE,
                ssl_verify=settings.OAUTH_SSL_VERIFY,
                buffer_seconds=settings.OAUTH_TOKEN_REFRESH_BUFFER_SECONDS,
            )
            self._token_caches[name] = token_cache
            
            self._llm_instances[name] = OAuthLLMClient(
                instance_name=name,
                endpoint_url=settings.LLM_ENDPOINT_URL,
                token_cache=token_cache,
                stats_tracker=self._stats,
                model_name=settings.LLM_MODEL,
                temperature=settings.LLM_TEMPERATURE,
                max_tokens=settings.LLM_MAX_TOKENS,
                timeout=settings.LLM_TIMEOUT,
                ssl_verify=settings.OAUTH_SSL_VERIFY,
            )
            
            self._embeddings_instances[name] = OAuthEmbeddings(
                instance_name=name,
                endpoint_url=settings.EMBEDDINGS_ENDPOINT_URL,
                token_cache=token_cache,
                stats_tracker=self._stats,
                model_name=settings.EMBEDDINGS_MODEL,
                ssl_verify=settings.OAUTH_SSL_VERIFY,
            )
            
            logger.info(f"[LLMManager] Initialized: {name}")
        
        self._initialized = True
        logger.info(f"[LLMManager] Ready ({len(self._llm_instances)} instances)")
    
    def get_llm(self, instance_name: str) -> OAuthLLMClient:
        """Get an LLM client by instance name."""
        if not self._initialized:
            self.initialize()
        
        if instance_name not in self._llm_instances:
            available = list(self._llm_instances.keys())
            raise ValueError(f"Unknown LLM instance: '{instance_name}'. Available: {available}")
        return self._llm_instances[instance_name]
    
    def get_embeddings(self, instance_name: str) -> OAuthEmbeddings:
        """Get an Embeddings client by instance name."""
        if not self._initialized:
            self.initialize()
        
        if instance_name not in self._embeddings_instances:
            available = list(self._embeddings_instances.keys())
            raise ValueError(f"Unknown Embeddings instance: '{instance_name}'. Available: {available}")
        return self._embeddings_instances[instance_name]
    
    @property
    def stats(self) -> LLMStats:
        """Get the stats tracker for all instances."""
        return self._stats
    
    def get_available_instances(self) -> list[str]:
        """Get list of available instance names."""
        if not self._initialized:
            self.initialize()
        return list(self._llm_instances.keys())
