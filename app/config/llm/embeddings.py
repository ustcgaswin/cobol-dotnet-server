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
    """
    
    instance_name: str = Field(description="Name of this instance")
    endpoint_url: str = Field(description="Embeddings API endpoint URL")
    token_cache: TokenCache = Field(description="Token cache for this instance")
    stats_tracker: LLMStats = Field(description="Stats tracker")
    
    model_name: str = Field(default="text-embedding-ada-002-2-gs", description="Model name")
    timeout: float = Field(default=60.0)
    ssl_verify: bool = Field(default=False, description="Whether to verify SSL certificates")
    
    class Config:
        arbitrary_types_allowed = True
    
    def _call_with_auth_retry(self, texts: List[str]) -> List[List[float]]:
        """Make API call with automatic 401 retry (sync)."""
        token = self.token_cache.get_token()
        
        for attempt in range(2):
            headers = {
                "Authorization": f"Bearer {token}",
                "Content-Type": "application/json",
            }
            payload = {
                "input": texts,
                "model": self.model_name,
            }
            
            response = requests.post(
                self.endpoint_url,
                json=payload,
                headers=headers,
                timeout=self.timeout,
                verify=self.ssl_verify,
            )
            
            # Handle 401 - refresh token and retry
            if response.status_code == 401 and attempt == 0:
                logger.warning(f"[Embeddings:{self.instance_name}] Got 401, refreshing token")
                self.token_cache.invalidate()
                token = self.token_cache.get_token()
                continue
            
            # Handle rate limiting
            if response.status_code == 429:
                retry_after = int(response.headers.get("Retry-After", 60))
                logger.warning(f"[Embeddings:{self.instance_name}] Rate limited, waiting {retry_after}s")
                time.sleep(retry_after)
                continue
            
            response.raise_for_status()
            
            # Parse response - API returns payload.data structure
            data = response.json()
            if "payload" in data:
                embeddings = [item["embedding"] for item in data["payload"]["data"]]
            else:
                # Fallback for standard OpenAI format
                embeddings = [item["embedding"] for item in data["data"]]
            
            return embeddings
        
        raise RuntimeError(f"[{self.instance_name}] Failed to get embeddings after retries")
    
    async def _call_with_auth_retry_async(self, texts: List[str]) -> List[List[float]]:
        """Make API call with automatic 401 retry (async)."""
        token = await self.token_cache.get_token_async()
        
        async with httpx.AsyncClient(verify=self.ssl_verify) as client:
            for attempt in range(2):
                headers = {
                    "Authorization": f"Bearer {token}",
                    "Content-Type": "application/json",
                }
                payload = {
                    "input": texts,
                    "model": self.model_name,
                }
                
                response = await client.post(
                    self.endpoint_url,
                    json=payload,
                    headers=headers,
                    timeout=self.timeout,
                )
                
                if response.status_code == 401 and attempt == 0:
                    logger.warning(f"[Embeddings:{self.instance_name}] Got 401, refreshing token")
                    self.token_cache.invalidate()
                    token = await self.token_cache.get_token_async()
                    continue
                
                if response.status_code == 429:
                    retry_after = int(response.headers.get("Retry-After", 60))
                    logger.warning(f"[Embeddings:{self.instance_name}] Rate limited, waiting {retry_after}s")
                    await asyncio.sleep(retry_after)
                    continue
                
                response.raise_for_status()
                
                data = response.json()
                if "payload" in data:
                    embeddings = [item["embedding"] for item in data["payload"]["data"]]
                else:
                    embeddings = [item["embedding"] for item in data["data"]]
                
                return embeddings
        
        raise RuntimeError(f"[{self.instance_name}] Failed to get embeddings after retries")
    
    def embed_documents(self, texts: List[str]) -> List[List[float]]:
        """Embed a list of documents (sync)."""
        try:
            result = self._call_with_auth_retry(texts)
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=True)
            return result
        except Exception as e:
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=False)
            logger.error(f"[Embeddings:{self.instance_name}] embed_documents failed: {e}")
            raise
    
    def embed_query(self, text: str) -> List[float]:
        """Embed a single query (sync)."""
        return self.embed_documents([text])[0]
    
    async def aembed_documents(self, texts: List[str]) -> List[List[float]]:
        """Embed a list of documents (async)."""
        try:
            result = await self._call_with_auth_retry_async(texts)
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=True)
            return result
        except Exception as e:
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=False)
            logger.error(f"[Embeddings:{self.instance_name}] aembed_documents failed: {e}")
            raise
    
    async def aembed_query(self, text: str) -> List[float]:
        """Embed a single query (async)."""
        result = await self.aembed_documents([text])
        return result[0]
