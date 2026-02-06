"""OAuth2 token caching with lazy refresh."""

import asyncio
import threading
import time
from dataclasses import dataclass

import httpx
import requests
from loguru import logger


@dataclass
class CachedToken:
    """Cached OAuth2 access token with expiry timestamp."""
    access_token: str
    expires_at: float  # Unix timestamp (UTC)


class TokenCache:
    """Thread-safe token cache with lazy refresh for a single OAuth2 client.
    
    Features:
    - Lazy refresh: Token is only fetched when needed
    - Buffer time: Refreshes token before actual expiry
    - Thread-safe: Uses locks for both sync and async contexts
    - Invalidation: Token can be invalidated on 401 responses
    """
    
    def __init__(
        self, 
        instance_name: str,
        client_id: str, 
        client_secret: str, 
        auth_url: str,
        scope: str = "customscope",
        ssl_verify: bool = False,
        buffer_seconds: int = 60
    ):
        self.instance_name = instance_name
        self.client_id = client_id
        self.client_secret = client_secret
        self.auth_url = auth_url
        self.scope = scope
        self.ssl_verify = ssl_verify
        self.buffer_seconds = buffer_seconds
        
        self._token: CachedToken | None = None
        self._sync_lock = threading.Lock()
        self._async_lock: asyncio.Lock | None = None
    
    def _get_async_lock(self) -> asyncio.Lock:
        """Lazily create async lock (must be in event loop context)."""
        if self._async_lock is None:
            self._async_lock = asyncio.Lock()
        return self._async_lock
    
    def _is_expired(self) -> bool:
        """Check if token is missing or needs refresh."""
        if self._token is None:
            return True
        
        current_time = time.time()
        effective_expiry = self._token.expires_at - self.buffer_seconds
        return current_time >= effective_expiry
    
    def invalidate(self) -> None:
        """Clear cached token (call on 401 response)."""
        logger.warning(f"[TokenCache:{self.instance_name}] Token invalidated (401)")
        self._token = None
    
    # ---- Sync Methods ----
    
    def get_token(self) -> str:
        """Get valid token, refreshing if needed (sync)."""
        with self._sync_lock:
            if self._is_expired():
                self._token = self._fetch_token_sync()
            return self._token.access_token
    
    def _fetch_token_sync(self) -> CachedToken:
        """Fetch new token via sync HTTP request."""
        logger.info(f"[TokenCache:{self.instance_name}] Fetching new token...")
        
        try:
            response = requests.post(
                self.auth_url,
                data={
                    "grant_type": "client_credentials",
                    "client_id": self.client_id,
                    "client_secret": self.client_secret,
                    "scope": self.scope,
                },
                headers={"Content-Type": "application/x-www-form-urlencoded"},
                timeout=30,
                verify=self.ssl_verify,
            )
            response.raise_for_status()
            
            data = response.json()
            expires_in = data.get("expires_in", 3600)
            expires_at = time.time() + expires_in
            
            logger.info(f"[TokenCache:{self.instance_name}] Token acquired (expires_in={expires_in}s)")
            
            return CachedToken(access_token=data["access_token"], expires_at=expires_at)
            
        except requests.RequestException as e:
            logger.error(f"[TokenCache:{self.instance_name}] Token fetch failed: {e}")
            raise
    
    # ---- Async Methods ----
    
    async def get_token_async(self) -> str:
        """Get valid token, refreshing if needed (async)."""
        async with self._get_async_lock():
            if self._is_expired():
                self._token = await self._fetch_token_async()
            return self._token.access_token
    
    async def _fetch_token_async(self) -> CachedToken:
        """Fetch new token via async HTTP request."""
        logger.info(f"[TokenCache:{self.instance_name}] Fetching new token (async)...")
        
        try:
            async with httpx.AsyncClient(verify=self.ssl_verify) as client:
                response = await client.post(
                    self.auth_url,
                    data={
                        "grant_type": "client_credentials",
                        "client_id": self.client_id,
                        "client_secret": self.client_secret,
                        "scope": self.scope,
                    },
                    headers={"Content-Type": "application/x-www-form-urlencoded"},
                    timeout=30,
                )
                response.raise_for_status()
            
            data = response.json()
            expires_in = data.get("expires_in", 3600)
            expires_at = time.time() + expires_in
            
            logger.info(f"[TokenCache:{self.instance_name}] Token acquired (expires_in={expires_in}s)")
            
            return CachedToken(access_token=data["access_token"], expires_at=expires_at)
            
        except httpx.RequestError as e:
            logger.error(f"[TokenCache:{self.instance_name}] Token fetch failed: {e}")
            raise
