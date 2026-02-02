"""Custom Embeddings client with OAuth2 authentication.

Implements LangChain's Embeddings interface for full compatibility.
"""

import asyncio
import time
from typing import List

import httpx
import requests
from langchain_core.embeddings import Embeddings
from loguru import logger
from pydantic import BaseModel, Field

from app.config.llm.token_cache import TokenCache
from app.config.llm.stats import LLMStats


class OAuthEmbeddings(BaseModel, Embeddings):
    """Custom Embeddings client with OAuth2 authentication.
    
    Stats tracking is automatic - services don't need to handle it.
    
    Features:
    - OAuth2 token management with automatic refresh
    - 401 retry: If token is rejected, refresh and retry once
    - 429 rate limit handling: Wait and retry on rate limiting
    - Automatic usage statistics tracking
    - Both sync and async support
    
    Usage:
        from app.config.llm import get_embeddings, CODEGEN
        
        embeddings = get_embeddings(CODEGEN)
        vector = embeddings.embed_query("Hello!")  # Sync
        vectors = await embeddings.aembed_documents(["Hello!", "World!"])  # Async
    """
    
    instance_name: str = Field(description="Name of this instance")
    endpoint_url: str = Field(description="Embeddings API endpoint URL")
    token_cache: TokenCache = Field(description="Token cache for this instance")
    stats_tracker: LLMStats = Field(description="Stats tracker")
    
    timeout: float = Field(default=60.0)
    
    class Config:
        arbitrary_types_allowed = True
    
    def _call_with_auth_retry(self, texts: List[str]) -> List[List[float]]:
        """Make API call with automatic 401 retry (sync).
        
        Args:
            texts: List of texts to embed
            
        Returns:
            List of embedding vectors
            
        Raises:
            requests.HTTPError: If API call fails
            RuntimeError: If all retries exhausted
        """
        token = self.token_cache.get_token()
        
        for attempt in range(2):
            headers = {
                "Authorization": f"Bearer {token}",
                "Content-Type": "application/json",
            }
            payload = {"input": texts}
            
            response = requests.post(
                self.endpoint_url,
                json=payload,
                headers=headers,
                timeout=self.timeout,
            )
            
            # Handle 401 - refresh token and retry
            if response.status_code == 401 and attempt == 0:
                logger.warning(f"[{self.instance_name}] Embeddings 401, refreshing token")
                self.token_cache.invalidate()
                token = self.token_cache.get_token()
                continue
            
            # Handle rate limiting
            if response.status_code == 429:
                retry_after = int(response.headers.get("Retry-After", 60))
                logger.warning(f"[{self.instance_name}] Rate limited, waiting {retry_after}s")
                time.sleep(retry_after)
                continue
            
            response.raise_for_status()
            
            # Parse response (adjust based on actual API structure)
            data = response.json()
            return [item["embedding"] for item in data["data"]]
        
        raise RuntimeError(f"[{self.instance_name}] Failed to get embeddings after retries")
    
    async def _call_with_auth_retry_async(self, texts: List[str]) -> List[List[float]]:
        """Make API call with automatic 401 retry (async).
        
        Args:
            texts: List of texts to embed
            
        Returns:
            List of embedding vectors
            
        Raises:
            httpx.HTTPStatusError: If API call fails
            RuntimeError: If all retries exhausted
        """
        token = await self.token_cache.get_token_async()
        
        async with httpx.AsyncClient() as client:
            for attempt in range(2):
                headers = {
                    "Authorization": f"Bearer {token}",
                    "Content-Type": "application/json",
                }
                payload = {"input": texts}
                
                response = await client.post(
                    self.endpoint_url,
                    json=payload,
                    headers=headers,
                    timeout=self.timeout,
                )
                
                # Handle 401 - refresh token and retry
                if response.status_code == 401 and attempt == 0:
                    logger.warning(f"[{self.instance_name}] Embeddings 401, refreshing token")
                    self.token_cache.invalidate()
                    token = await self.token_cache.get_token_async()
                    continue
                
                # Handle rate limiting
                if response.status_code == 429:
                    retry_after = int(response.headers.get("Retry-After", 60))
                    logger.warning(f"[{self.instance_name}] Rate limited, waiting {retry_after}s")
                    await asyncio.sleep(retry_after)
                    continue
                
                response.raise_for_status()
                data = response.json()
                return [item["embedding"] for item in data["data"]]
        
        raise RuntimeError(f"[{self.instance_name}] Failed to get embeddings after retries")
    
    def embed_documents(self, texts: List[str]) -> List[List[float]]:
        """Embed a list of documents (sync).
        
        Args:
            texts: List of texts to embed
            
        Returns:
            List of embedding vectors
        """
        try:
            result = self._call_with_auth_retry(texts)
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=True)
            return result
        except Exception:
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=False)
            raise
    
    def embed_query(self, text: str) -> List[float]:
        """Embed a single query (sync).
        
        Args:
            text: Text to embed
            
        Returns:
            Embedding vector
        """
        return self.embed_documents([text])[0]
    
    async def aembed_documents(self, texts: List[str]) -> List[List[float]]:
        """Embed a list of documents (async).
        
        Args:
            texts: List of texts to embed
            
        Returns:
            List of embedding vectors
        """
        try:
            result = await self._call_with_auth_retry_async(texts)
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=True)
            return result
        except Exception:
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=False)
            raise
    
    async def aembed_query(self, text: str) -> List[float]:
        """Embed a single query (async).
        
        Args:
            text: Text to embed
            
        Returns:
            Embedding vector
        """
        result = await self.aembed_documents([text])
        return result[0]
