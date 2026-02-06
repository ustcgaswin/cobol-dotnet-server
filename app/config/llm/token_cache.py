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
    - Buffer time: Refreshes token before actual expiry to prevent edge cases
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
        """Initialize token cache.
        
        Args:
            instance_name: Name of this instance (for logging)
            client_id: OAuth2 client ID
            client_secret: OAuth2 client secret
            auth_url: OAuth2 token endpoint URL
            scope: OAuth2 scope for token request
            ssl_verify: Whether to verify SSL certificates
            buffer_seconds: Refresh token this many seconds before expiry
        """
        self.instance_name = instance_name
        self.client_id = client_id
        self.client_secret = client_secret
        self.auth_url = auth_url
        self.scope = scope
        self.ssl_verify = ssl_verify
        self.buffer_seconds = buffer_seconds
        
        self._token: CachedToken | None = None
        self._sync_lock = threading.Lock()
        self._async_lock: asyncio.Lock | None = None  # Created lazily
        
        logger.debug(
            f"[TokenCache:{self.instance_name}] Initialized | "
            f"auth_url={self.auth_url} | client_id={self.client_id[:8]}*** | "
            f"scope={self.scope} | ssl_verify={self.ssl_verify}"
        )
    
    def _get_async_lock(self) -> asyncio.Lock:
        """Lazily create async lock (must be in event loop context)."""
        if self._async_lock is None:
            self._async_lock = asyncio.Lock()
        return self._async_lock
    
    def _is_expired(self) -> bool:
        """Check if token is missing or needs refresh (including buffer)."""
        if self._token is None:
            logger.debug(f"[TokenCache:{self.instance_name}] No cached token found")
            return True
        
        current_time = time.time()
        effective_expiry = self._token.expires_at - self.buffer_seconds
        is_expired = current_time >= effective_expiry
        
        if is_expired:
            logger.debug(
                f"[TokenCache:{self.instance_name}] Token expired/expiring | "
                f"current_time={current_time:.0f} | expires_at={self._token.expires_at:.0f} | "
                f"buffer={self.buffer_seconds}s"
            )
        else:
            remaining = effective_expiry - current_time
            logger.debug(
                f"[TokenCache:{self.instance_name}] Token valid | "
                f"expires_in={remaining:.0f}s | token=...{self._token.access_token[-20:]}"
            )
        
        return is_expired
    
    def invalidate(self) -> None:
        """Clear cached token (call on 401 response)."""
        logger.warning(f"[TokenCache:{self.instance_name}] INVALIDATING cached token (401 received)")
        self._token = None
    
    # ---- Sync Methods ----
    
    def get_token(self) -> str:
        """Get valid token, refreshing if needed (sync).
        
        Returns:
            Valid access token string
            
        Raises:
            requests.HTTPError: If token fetch fails
        """
        logger.debug(f"[TokenCache:{self.instance_name}] get_token() called (sync)")
        
        with self._sync_lock:
            if self._is_expired():
                logger.info(f"[TokenCache:{self.instance_name}] Token refresh required, fetching new token...")
                self._token = self._fetch_token_sync()
            else:
                logger.debug(f"[TokenCache:{self.instance_name}] Using cached token")
            
            return self._token.access_token
    
    def _fetch_token_sync(self) -> CachedToken:
        """Fetch new token via sync HTTP request."""
        logger.info(
            f"[TokenCache:{self.instance_name}] >>> POST {self.auth_url} | "
            f"grant_type=client_credentials | client_id={self.client_id[:8]}***"
        )
        
        start_time = time.time()
        
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
            
            elapsed = time.time() - start_time
            logger.info(
                f"[TokenCache:{self.instance_name}] <<< Response: {response.status_code} | "
                f"elapsed={elapsed:.2f}s"
            )
            
            response.raise_for_status()
            
            data = response.json()
            expires_in = data.get("expires_in", 3600)
            expires_at = time.time() + expires_in
            access_token = data["access_token"]
            
            logger.info(
                f"[TokenCache:{self.instance_name}] Token acquired | "
                f"expires_in={expires_in}s | token=...{access_token[-20:]}"
            )
            
            return CachedToken(access_token=access_token, expires_at=expires_at)
            
        except requests.RequestException as e:
            elapsed = time.time() - start_time
            logger.error(
                f"[TokenCache:{self.instance_name}] Token fetch FAILED | "
                f"error={type(e).__name__}: {str(e)[:100]} | elapsed={elapsed:.2f}s"
            )
            raise
    
    # ---- Async Methods ----
    
    async def get_token_async(self) -> str:
        """Get valid token, refreshing if needed (async).
        
        Returns:
            Valid access token string
            
        Raises:
            httpx.HTTPStatusError: If token fetch fails
        """
        logger.debug(f"[TokenCache:{self.instance_name}] get_token_async() called")
        
        async with self._get_async_lock():
            if self._is_expired():
                logger.info(f"[TokenCache:{self.instance_name}] Token refresh required (async), fetching...")
                self._token = await self._fetch_token_async()
            else:
                logger.debug(f"[TokenCache:{self.instance_name}] Using cached token (async)")
            
            return self._token.access_token
    
    async def _fetch_token_async(self) -> CachedToken:
        """Fetch new token via async HTTP request."""
        logger.info(
            f"[TokenCache:{self.instance_name}] >>> POST {self.auth_url} (async) | "
            f"grant_type=client_credentials | client_id={self.client_id[:8]}***"
        )
        
        start_time = time.time()
        
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
                
                elapsed = time.time() - start_time
                logger.info(
                    f"[TokenCache:{self.instance_name}] <<< Response: {response.status_code} | "
                    f"elapsed={elapsed:.2f}s (async)"
                )
                
                response.raise_for_status()
            
            data = response.json()
            expires_in = data.get("expires_in", 3600)
            expires_at = time.time() + expires_in
            access_token = data["access_token"]
            
            logger.info(
                f"[TokenCache:{self.instance_name}] Token acquired (async) | "
                f"expires_in={expires_in}s | token=...{access_token[-20:]}"
            )
            
            return CachedToken(access_token=access_token, expires_at=expires_at)
            
        except httpx.RequestError as e:
            elapsed = time.time() - start_time
            logger.error(
                f"[TokenCache:{self.instance_name}] Token fetch FAILED (async) | "
                f"error={type(e).__name__}: {str(e)[:100]} | elapsed={elapsed:.2f}s"
            )
            raise
