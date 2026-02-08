"""Custom Embeddings client with OAuth2 authentication.

Implements LangChain's Embeddings interface for full compatibility.
"""

import asyncio
import time
from typing import List, Tuple, Any

import httpx
import requests
from langchain_core.embeddings import Embeddings
from loguru import logger
from pydantic import BaseModel, Field

from app.config.llm.token_cache import TokenCache
from app.config.llm.stats import LLMStats


# Retry configuration
MAX_RETRIES = 20
DEFAULT_RETRY_WAIT = 75  # seconds


class OAuthEmbeddings(BaseModel, Embeddings):
    """Custom Embeddings client with OAuth2 authentication.
    
    Stats tracking is automatic - services don't need to handle it.
    
    Features:
    - OAuth2 token management with automatic refresh
    - Retry on 401, 429, 502 errors
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
    
    def _log_error_response(self, status_code: int, response: requests.Response) -> None:
        """Log error response details."""
        try:
            body = response.json()
        except Exception:
            body = response.text[:500]
        logger.warning(
            f"[Embeddings:{self.instance_name}] Got {status_code} | response={body}"
        )
    
    def _log_error_response_async(self, status_code: int, response: httpx.Response) -> None:
        """Log error response details (async)."""
        try:
            body = response.json()
        except Exception:
            body = response.text[:500]
        logger.warning(
            f"[Embeddings:{self.instance_name}] Got {status_code} | response={body}"
        )
    
    def _extract_usage(self, data: dict) -> dict[str, int]:
        """Extract token usage from response."""
        usage = data.get("usage") or data.get("payload", {}).get("usage") or {}
        return {
            "total": usage.get("total_tokens", 0),
            "prompt": usage.get("prompt_tokens", 0),
            "completion": usage.get("completion_tokens", 0),
        }

    def _call_with_retry(self, texts: List[str]) -> Tuple[List[List[float]], dict[str, int]]:
        """Make API call with retry logic (sync)."""
        token = self.token_cache.get_token()
        
        for attempt in range(MAX_RETRIES):
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
            if response.status_code == 401:
                self._log_error_response(401, response)
                logger.warning(f"[Embeddings:{self.instance_name}] Refreshing token (attempt {attempt + 1}/{MAX_RETRIES})")
                self.token_cache.invalidate()
                token = self.token_cache.get_token()
                continue
            
            # Handle 429 - rate limited
            if response.status_code == 429:
                self._log_error_response(429, response)
                retry_after = int(response.headers.get("Retry-After", DEFAULT_RETRY_WAIT))
                logger.warning(f"[Embeddings:{self.instance_name}] Rate limited, waiting {retry_after}s (attempt {attempt + 1}/{MAX_RETRIES})")
                time.sleep(retry_after)
                continue
            
            # Handle 502 - bad gateway
            if response.status_code == 502:
                self._log_error_response(502, response)
                logger.warning(f"[Embeddings:{self.instance_name}] Bad gateway, waiting {DEFAULT_RETRY_WAIT}s (attempt {attempt + 1}/{MAX_RETRIES})")
                time.sleep(DEFAULT_RETRY_WAIT)
                continue
            
            response.raise_for_status()
            
            # Parse response - API returns payload.data structure
            data = response.json()
            embeddings = [item["embedding"] for item in data["payload"]["data"]]
            usage = self._extract_usage(data)
            
            return embeddings, usage
        
        raise RuntimeError(f"[{self.instance_name}] Failed to get embeddings after {MAX_RETRIES} retries")
    
    async def _call_with_retry_async(self, texts: List[str]) -> Tuple[List[List[float]], dict[str, int]]:
        """Make API call with retry logic (async)."""
        token = await self.token_cache.get_token_async()
        
        async with httpx.AsyncClient(verify=self.ssl_verify) as client:
            for attempt in range(MAX_RETRIES):
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
                
                # Handle 401 - refresh token and retry
                if response.status_code == 401:
                    self._log_error_response_async(401, response)
                    logger.warning(f"[Embeddings:{self.instance_name}] Refreshing token (attempt {attempt + 1}/{MAX_RETRIES})")
                    self.token_cache.invalidate()
                    token = await self.token_cache.get_token_async()
                    continue
                
                # Handle 429 - rate limited
                if response.status_code == 429:
                    self._log_error_response_async(429, response)
                    retry_after = int(response.headers.get("Retry-After", DEFAULT_RETRY_WAIT))
                    logger.warning(f"[Embeddings:{self.instance_name}] Rate limited, waiting {retry_after}s (attempt {attempt + 1}/{MAX_RETRIES})")
                    await asyncio.sleep(retry_after)
                    continue
                
                # Handle 502 - bad gateway
                if response.status_code == 502:
                    self._log_error_response_async(502, response)
                    logger.warning(f"[Embeddings:{self.instance_name}] Bad gateway, waiting {DEFAULT_RETRY_WAIT}s (attempt {attempt + 1}/{MAX_RETRIES})")
                    await asyncio.sleep(DEFAULT_RETRY_WAIT)
                    continue
                
                response.raise_for_status()
                
                # Parse response - API returns payload.data structure
                data = response.json()
                embeddings = [item["embedding"] for item in data["payload"]["data"]]
                usage = self._extract_usage(data)
                
                return embeddings, usage
        
        raise RuntimeError(f"[{self.instance_name}] Failed to get embeddings after {MAX_RETRIES} retries")
    
    def embed_documents(self, texts: List[str]) -> List[List[float]]:
        """Embed a list of documents (sync)."""
        try:
            result, usage = self._call_with_retry(texts)
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=True, tokens=usage)
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
            result, usage = await self._call_with_retry_async(texts)
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=True, tokens=usage)
            return result
        except Exception as e:
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=False)
            logger.error(f"[Embeddings:{self.instance_name}] aembed_documents failed: {e}")
            raise
    
    async def aembed_query(self, text: str) -> List[float]:
        """Embed a single query (async)."""
        result = await self.aembed_documents([text])
        return result[0]
