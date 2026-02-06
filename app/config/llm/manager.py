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
    
    Features:
    - Lazy initialization: Instances are created on first access
    - Shared token cache: LLM and Embeddings for same instance share one token
    - Centralized stats: All instances report to a single stats tracker
    - Graceful handling: Missing credentials skip instance without error
    
    Usage:
        manager = LLMManager()
        manager.initialize()
        
        llm = manager.get_llm(CODEGEN)
        embeddings = manager.get_embeddings(DOCGEN)
        stats = manager.stats.get_all_stats()
    """
    
    def __init__(self):
        logger.debug("[LLMManager] Creating new LLMManager instance")
        self._llm_instances: dict[str, OAuthLLMClient] = {}
        self._embeddings_instances: dict[str, OAuthEmbeddings] = {}
        self._token_caches: dict[str, TokenCache] = {}
        self._stats = LLMStats()
        self._initialized = False
    
    def initialize(self) -> None:
        """Initialize all configured instances.
        
        Reads credentials from settings and creates LLM/Embeddings clients
        for each configured instance. Instances with missing credentials
        are skipped with a warning.
        """
        if self._initialized:
            logger.debug("[LLMManager] Already initialized, skipping")
            return
        
        logger.info("=" * 60)
        logger.info("[LLMManager] INITIALIZING LLM MANAGER")
        logger.info("=" * 60)
        
        # Log configuration
        logger.info(f"[LLMManager] OAuth Auth URL: {settings.OAUTH_AUTH_URL}")
        logger.info(f"[LLMManager] LLM Endpoint URL: {settings.LLM_ENDPOINT_URL}")
        logger.info(f"[LLMManager] Embeddings Endpoint URL: {settings.EMBEDDINGS_ENDPOINT_URL}")
        logger.info(f"[LLMManager] SSL Verify: {settings.OAUTH_SSL_VERIFY}")
        logger.info(f"[LLMManager] Default LLM Model: {settings.LLM_MODEL}")
        logger.info(f"[LLMManager] Default Embeddings Model: {settings.EMBEDDINGS_MODEL}")
        
        # Instance configurations
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
            logger.info(f"[LLMManager] Checking instance: {name}")
            
            if not creds["client_id"] or not creds["client_secret"]:
                logger.warning(
                    f"[LLMManager] SKIPPING {name}: missing credentials | "
                    f"client_id={'set' if creds['client_id'] else 'MISSING'} | "
                    f"client_secret={'set' if creds['client_secret'] else 'MISSING'}"
                )
                continue
            
            logger.debug(
                f"[LLMManager] {name}: client_id={creds['client_id'][:8]}*** | "
                f"client_secret=***{creds['client_secret'][-4:]}"
            )
            
            # Create shared TokenCache for this instance
            logger.debug(f"[LLMManager] Creating TokenCache for {name}")
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
            
            # Create LLM client
            logger.debug(f"[LLMManager] Creating OAuthLLMClient for {name}")
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
            
            # Create Embeddings client (shares same TokenCache)
            logger.debug(f"[LLMManager] Creating OAuthEmbeddings for {name}")
            self._embeddings_instances[name] = OAuthEmbeddings(
                instance_name=name,
                endpoint_url=settings.EMBEDDINGS_ENDPOINT_URL,
                token_cache=token_cache,
                stats_tracker=self._stats,
                model_name=settings.EMBEDDINGS_MODEL,
                ssl_verify=settings.OAUTH_SSL_VERIFY,
            )
            
            logger.info(f"[LLMManager] ✓ Initialized instance: {name}")
        
        self._initialized = True
        logger.info("=" * 60)
        logger.info(
            f"[LLMManager] READY | Instances: {list(self._llm_instances.keys())} | "
            f"Total: {len(self._llm_instances)}"
        )
        logger.info("=" * 60)
    
    def get_llm(self, instance_name: str) -> OAuthLLMClient:
        """Get an LLM client by instance name.
        
        Args:
            instance_name: One of CODEGEN or DOCGEN
            
        Returns:
            OAuthLLMClient instance
            
        Raises:
            ValueError: If instance name is unknown or not configured
        """
        logger.debug(f"[LLMManager] get_llm('{instance_name}') called")
        
        if not self._initialized:
            logger.debug("[LLMManager] Not initialized, calling initialize()")
            self.initialize()
        
        if instance_name not in self._llm_instances:
            available = list(self._llm_instances.keys())
            logger.error(
                f"[LLMManager] Unknown LLM instance: '{instance_name}' | "
                f"Available: {available}"
            )
            raise ValueError(
                f"Unknown LLM instance: '{instance_name}'. Available: {available}"
            )
        
        llm = self._llm_instances[instance_name]
        logger.debug(
            f"[LLMManager] Returning LLM for '{instance_name}' | "
            f"model={llm.model_name} | endpoint={llm.endpoint_url}"
        )
        return llm
    
    def get_embeddings(self, instance_name: str) -> OAuthEmbeddings:
        """Get an Embeddings client by instance name.
        
        Args:
            instance_name: One of CODEGEN or DOCGEN
            
        Returns:
            OAuthEmbeddings instance
            
        Raises:
            ValueError: If instance name is unknown or not configured
        """
        logger.debug(f"[LLMManager] get_embeddings('{instance_name}') called")
        
        if not self._initialized:
            logger.debug("[LLMManager] Not initialized, calling initialize()")
            self.initialize()
        
        if instance_name not in self._embeddings_instances:
            available = list(self._embeddings_instances.keys())
            logger.error(
                f"[LLMManager] Unknown Embeddings instance: '{instance_name}' | "
                f"Available: {available}"
            )
            raise ValueError(
                f"Unknown Embeddings instance: '{instance_name}'. Available: {available}"
            )
        
        emb = self._embeddings_instances[instance_name]
        logger.debug(
            f"[LLMManager] Returning Embeddings for '{instance_name}' | "
            f"model={emb.model_name} | endpoint={emb.endpoint_url}"
        )
        return emb
    
    @property
    def stats(self) -> LLMStats:
        """Get the stats tracker for all instances."""
        return self._stats
    
    def get_available_instances(self) -> list[str]:
        """Get list of available instance names.
        
        Returns:
            List of configured instance names
        """
        logger.debug("[LLMManager] get_available_instances() called")
        
        if not self._initialized:
            logger.debug("[LLMManager] Not initialized, calling initialize()")
            self.initialize()
        
        available = list(self._llm_instances.keys())
        logger.debug(f"[LLMManager] Available instances: {available}")
        return available
