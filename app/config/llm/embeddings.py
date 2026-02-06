"""Custom Embeddings client with OAuth2 authentication.

Implements LangChain's Embeddings interface for full compatibility.
"""

import asyncio
import json
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
    
    model_name: str = Field(default="text-embedding-ada-002-2-gs", description="Model name for API requests")
    timeout: float = Field(default=60.0)
    ssl_verify: bool = Field(default=False, description="Whether to verify SSL certificates")
    
    class Config:
        arbitrary_types_allowed = True
    
    def _truncate_for_log(self, text: str, max_len: int = 100) -> str:
        """Truncate text for logging."""
        if len(text) <= max_len:
            return text
        return text[:max_len] + f"... [truncated, total {len(text)} chars]"
    
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
        logger.debug(f"[Embeddings:{self.instance_name}] Getting token for request...")
        token = self.token_cache.get_token()
        logger.debug(f"[Embeddings:{self.instance_name}] Token acquired: ...{token[-20:]}")
        
        for attempt in range(2):
            headers = {
                "Authorization": f"Bearer {token}",
                "Content-Type": "application/json",
            }
            payload = {
                "input": texts,
                "model": self.model_name,
            }
            
            # Log request details
            texts_preview = [self._truncate_for_log(t, 50) for t in texts[:3]]
            if len(texts) > 3:
                texts_preview.append(f"... and {len(texts) - 3} more")
            
            logger.info(
                f"[Embeddings:{self.instance_name}] >>> POST {self.endpoint_url} | "
                f"attempt={attempt+1}/2 | model={self.model_name} | text_count={len(texts)}"
            )
            logger.debug(
                f"[Embeddings:{self.instance_name}] Input texts: {texts_preview}"
            )
            
            start_time = time.time()
            
            try:
                response = requests.post(
                    self.endpoint_url,
                    json=payload,
                    headers=headers,
                    timeout=self.timeout,
                    verify=self.ssl_verify,
                )
                
                elapsed = time.time() - start_time
                logger.info(
                    f"[Embeddings:{self.instance_name}] <<< Response: {response.status_code} | "
                    f"elapsed={elapsed:.2f}s | content_length={len(response.content)}"
                )
                
            except requests.RequestException as e:
                elapsed = time.time() - start_time
                logger.error(
                    f"[Embeddings:{self.instance_name}] Request FAILED | "
                    f"error={type(e).__name__}: {str(e)[:100]} | elapsed={elapsed:.2f}s"
                )
                raise
            
            # Handle 401 - refresh token and retry
            if response.status_code == 401 and attempt == 0:
                logger.warning(
                    f"[Embeddings:{self.instance_name}] Got 401 Unauthorized | "
                    f"Invalidating token and retrying..."
                )
                self.token_cache.invalidate()
                token = self.token_cache.get_token()
                logger.info(f"[Embeddings:{self.instance_name}] New token acquired: ...{token[-20:]}")
                continue
            
            # Handle rate limiting
            if response.status_code == 429:
                retry_after = int(response.headers.get("Retry-After", 60))
                logger.warning(
                    f"[Embeddings:{self.instance_name}] Got 429 Rate Limited | "
                    f"Retry-After={retry_after}s | Waiting..."
                )
                time.sleep(retry_after)
                continue
            
            response.raise_for_status()
            
            # Parse response (adjust based on actual API structure)
            data = response.json()
            embeddings = [item["embedding"] for item in data["data"]]
            
            logger.debug(
                f"[Embeddings:{self.instance_name}] Parsed {len(embeddings)} embeddings | "
                f"dimension={len(embeddings[0]) if embeddings else 0}"
            )
            
            return embeddings
        
        logger.error(f"[Embeddings:{self.instance_name}] FAILED after all retries")
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
        logger.debug(f"[Embeddings:{self.instance_name}] Getting token for request (async)...")
        token = await self.token_cache.get_token_async()
        logger.debug(f"[Embeddings:{self.instance_name}] Token acquired (async): ...{token[-20:]}")
        
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
                
                # Log request details
                texts_preview = [self._truncate_for_log(t, 50) for t in texts[:3]]
                if len(texts) > 3:
                    texts_preview.append(f"... and {len(texts) - 3} more")
                
                logger.info(
                    f"[Embeddings:{self.instance_name}] >>> POST {self.endpoint_url} (async) | "
                    f"attempt={attempt+1}/2 | model={self.model_name} | text_count={len(texts)}"
                )
                logger.debug(
                    f"[Embeddings:{self.instance_name}] Input texts: {texts_preview}"
                )
                
                start_time = time.time()
                
                try:
                    response = await client.post(
                        self.endpoint_url,
                        json=payload,
                        headers=headers,
                        timeout=self.timeout,
                    )
                    
                    elapsed = time.time() - start_time
                    logger.info(
                        f"[Embeddings:{self.instance_name}] <<< Response: {response.status_code} | "
                        f"elapsed={elapsed:.2f}s (async)"
                    )
                    
                except httpx.RequestError as e:
                    elapsed = time.time() - start_time
                    logger.error(
                        f"[Embeddings:{self.instance_name}] Request FAILED (async) | "
                        f"error={type(e).__name__}: {str(e)[:100]} | elapsed={elapsed:.2f}s"
                    )
                    raise
                
                # Handle 401 - refresh token and retry
                if response.status_code == 401 and attempt == 0:
                    logger.warning(
                        f"[Embeddings:{self.instance_name}] Got 401 Unauthorized (async) | "
                        f"Invalidating token and retrying..."
                    )
                    self.token_cache.invalidate()
                    token = await self.token_cache.get_token_async()
                    logger.info(f"[Embeddings:{self.instance_name}] New token acquired (async): ...{token[-20:]}")
                    continue
                
                # Handle rate limiting
                if response.status_code == 429:
                    retry_after = int(response.headers.get("Retry-After", 60))
                    logger.warning(
                        f"[Embeddings:{self.instance_name}] Got 429 Rate Limited (async) | "
                        f"Retry-After={retry_after}s | Waiting..."
                    )
                    await asyncio.sleep(retry_after)
                    continue
                
                response.raise_for_status()
                data = response.json()
                embeddings = [item["embedding"] for item in data["data"]]
                
                logger.debug(
                    f"[Embeddings:{self.instance_name}] Parsed {len(embeddings)} embeddings (async) | "
                    f"dimension={len(embeddings[0]) if embeddings else 0}"
                )
                
                return embeddings
        
        logger.error(f"[Embeddings:{self.instance_name}] FAILED after all retries (async)")
        raise RuntimeError(f"[{self.instance_name}] Failed to get embeddings after retries")
    
    def embed_documents(self, texts: List[str]) -> List[List[float]]:
        """Embed a list of documents (sync).
        
        Args:
            texts: List of texts to embed
            
        Returns:
            List of embedding vectors
        """
        logger.info(
            f"[Embeddings:{self.instance_name}] embed_documents() called | text_count={len(texts)}"
        )
        
        try:
            result = self._call_with_auth_retry(texts)
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=True)
            
            logger.info(
                f"[Embeddings:{self.instance_name}] embed_documents() SUCCESS | "
                f"returned {len(result)} vectors"
            )
            return result
        except Exception as e:
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=False)
            logger.error(
                f"[Embeddings:{self.instance_name}] embed_documents() FAILED | "
                f"error={type(e).__name__}: {str(e)[:100]}"
            )
            raise
    
    def embed_query(self, text: str) -> List[float]:
        """Embed a single query (sync).
        
        Args:
            text: Text to embed
            
        Returns:
            Embedding vector
        """
        logger.debug(
            f"[Embeddings:{self.instance_name}] embed_query() called | "
            f"text={self._truncate_for_log(text)}"
        )
        return self.embed_documents([text])[0]
    
    async def aembed_documents(self, texts: List[str]) -> List[List[float]]:
        """Embed a list of documents (async).
        
        Args:
            texts: List of texts to embed
            
        Returns:
            List of embedding vectors
        """
        logger.info(
            f"[Embeddings:{self.instance_name}] aembed_documents() called (async) | text_count={len(texts)}"
        )
        
        try:
            result = await self._call_with_auth_retry_async(texts)
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=True)
            
            logger.info(
                f"[Embeddings:{self.instance_name}] aembed_documents() SUCCESS (async) | "
                f"returned {len(result)} vectors"
            )
            return result
        except Exception as e:
            self.stats_tracker.record_request(self.instance_name, "embeddings", success=False)
            logger.error(
                f"[Embeddings:{self.instance_name}] aembed_documents() FAILED (async) | "
                f"error={type(e).__name__}: {str(e)[:100]}"
            )
            raise
    
    async def aembed_query(self, text: str) -> List[float]:
        """Embed a single query (async).
        
        Args:
            text: Text to embed
            
        Returns:
            Embedding vector
        """
        logger.debug(
            f"[Embeddings:{self.instance_name}] aembed_query() called (async) | "
            f"text={self._truncate_for_log(text)}"
        )
        result = await self.aembed_documents([text])
        return result[0]
