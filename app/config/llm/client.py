"""Custom LLM client with OAuth2 authentication.

Implements LangChain's BaseChatModel for full compatibility with
LangChain/LangGraph agents and chains.
"""

import asyncio
import json
import time
from typing import Any, List, Optional

import httpx
import requests
from langchain_core.callbacks import CallbackManagerForLLMRun, AsyncCallbackManagerForLLMRun
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import AIMessage, BaseMessage, HumanMessage, SystemMessage
from langchain_core.outputs import ChatGeneration, ChatResult
from loguru import logger
from pydantic import Field

from app.config.llm.token_cache import TokenCache
from app.config.llm.stats import LLMStats


class OAuthLLMClient(BaseChatModel):
    """Custom LLM client with OAuth2 authentication.
    
    Implements BaseChatModel for full LangChain compatibility.
    Stats tracking is automatic - services don't need to handle it.
    
    Features:
    - OAuth2 token management with automatic refresh
    - 401 retry: If token is rejected, refresh and retry once
    - 429 rate limit handling: Wait and retry on rate limiting
    - Automatic usage statistics tracking
    - Both sync and async support
    
    Usage:
        from app.config.llm import get_llm, CODEGEN
        
        llm = get_llm(CODEGEN)
        response = llm.invoke("Hello!")  # Sync
        response = await llm.ainvoke("Hello!")  # Async
    """
    
    # --- Pydantic Fields ---
    instance_name: str = Field(description="Name of this LLM instance")
    endpoint_url: str = Field(description="LLM API endpoint URL")
    token_cache: TokenCache = Field(description="Token cache for this instance")
    stats_tracker: LLMStats = Field(description="Stats tracker")
    
    model_name: str = Field(default="gpt-40-dev", description="Model name for API requests")
    temperature: float = Field(default=0.1)
    max_tokens: int = Field(default=16384)
    timeout: float = Field(default=600.0)
    ssl_verify: bool = Field(default=False, description="Whether to verify SSL certificates")
    
    class Config:
        arbitrary_types_allowed = True
    
    @property
    def _llm_type(self) -> str:
        """Return identifier for this LLM type."""
        return "oauth-custom-llm"
    
    @property
    def _identifying_params(self) -> dict[str, Any]:
        """Return identifying parameters for this LLM instance."""
        return {
            "instance_name": self.instance_name,
            "endpoint_url": self.endpoint_url,
            "model_name": self.model_name,
            "temperature": self.temperature,
            "max_tokens": self.max_tokens,
        }
    
    def _format_messages(self, messages: List[BaseMessage]) -> list[dict]:
        """Convert LangChain messages to API format.
        
        Args:
            messages: List of LangChain BaseMessage objects
            
        Returns:
            List of dicts with 'role' and 'content' keys
        """
        formatted = []
        for msg in messages:
            if isinstance(msg, SystemMessage):
                role = "system"
            elif isinstance(msg, HumanMessage):
                role = "user"
            elif isinstance(msg, AIMessage):
                role = "assistant"
            else:
                role = "user"  # Default fallback
            
            formatted.append({"role": role, "content": msg.content})
        return formatted
    
    def _truncate_for_log(self, text: str, max_len: int = 200) -> str:
        """Truncate text for logging."""
        if len(text) <= max_len:
            return text
        return text[:max_len] + f"... [truncated, total {len(text)} chars]"
    
    def _call_with_auth_retry(self, messages: list[dict]) -> dict:
        """Make API call with automatic 401 retry (sync version).
        
        Args:
            messages: Formatted messages list
            
        Returns:
            API response as dict
            
        Raises:
            requests.HTTPError: If API call fails
            RuntimeError: If all retries exhausted
        """
        logger.debug(f"[LLM:{self.instance_name}] Getting token for request...")
        token = self.token_cache.get_token()
        logger.debug(f"[LLM:{self.instance_name}] Token acquired: ...{token[-20:]}")
        
        for attempt in range(2):  # Max 2 attempts (original + 1 retry on 401)
            headers = {
                "Authorization": f"Bearer {token}",
                "Content-Type": "application/json",
            }
            payload = {
                "model": self.model_name,
                "messages": messages,
                "temperature": self.temperature,
                "max_tokens": self.max_tokens,
            }
            
            # Log request details
            msg_preview = self._truncate_for_log(str(messages))
            logger.info(
                f"[LLM:{self.instance_name}] >>> POST {self.endpoint_url} | "
                f"attempt={attempt+1}/2 | model={self.model_name} | "
                f"temp={self.temperature} | max_tokens={self.max_tokens}"
            )
            logger.debug(
                f"[LLM:{self.instance_name}] Request payload: {msg_preview}"
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
                    f"[LLM:{self.instance_name}] <<< Response: {response.status_code} | "
                    f"elapsed={elapsed:.2f}s | content_length={len(response.content)}"
                )
                
            except requests.RequestException as e:
                elapsed = time.time() - start_time
                logger.error(
                    f"[LLM:{self.instance_name}] Request FAILED | "
                    f"error={type(e).__name__}: {str(e)[:100]} | elapsed={elapsed:.2f}s"
                )
                raise
            
            # Handle 401 - refresh token and retry
            if response.status_code == 401 and attempt == 0:
                logger.warning(
                    f"[LLM:{self.instance_name}] Got 401 Unauthorized | "
                    f"Invalidating token and retrying..."
                )
                self.token_cache.invalidate()
                token = self.token_cache.get_token()
                logger.info(f"[LLM:{self.instance_name}] New token acquired: ...{token[-20:]}")
                continue
            
            # Handle rate limiting
            if response.status_code == 429:
                retry_after = int(response.headers.get("Retry-After", 60))
                logger.warning(
                    f"[LLM:{self.instance_name}] Got 429 Rate Limited | "
                    f"Retry-After={retry_after}s | Waiting..."
                )
                time.sleep(retry_after)
                continue
            
            # Log response body for debugging
            try:
                response_data = response.json()
                response_preview = self._truncate_for_log(json.dumps(response_data))
                logger.debug(f"[LLM:{self.instance_name}] Response body: {response_preview}")
            except Exception:
                logger.debug(f"[LLM:{self.instance_name}] Response body (raw): {response.text[:200]}")
            
            response.raise_for_status()
            return response.json()
        
        logger.error(f"[LLM:{self.instance_name}] FAILED after all retries")
        raise RuntimeError(f"[{self.instance_name}] Failed to call LLM after retries")
    
    async def _call_with_auth_retry_async(self, messages: list[dict]) -> dict:
        """Make API call with automatic 401 retry (async version).
        
        Args:
            messages: Formatted messages list
            
        Returns:
            API response as dict
            
        Raises:
            httpx.HTTPStatusError: If API call fails
            RuntimeError: If all retries exhausted
        """
        logger.debug(f"[LLM:{self.instance_name}] Getting token for request (async)...")
        token = await self.token_cache.get_token_async()
        logger.debug(f"[LLM:{self.instance_name}] Token acquired (async): ...{token[-20:]}")
        
        async with httpx.AsyncClient(verify=self.ssl_verify) as client:
            for attempt in range(2):
                headers = {
                    "Authorization": f"Bearer {token}",
                    "Content-Type": "application/json",
                }
                payload = {
                    "model": self.model_name,
                    "messages": messages,
                    "temperature": self.temperature,
                    "max_tokens": self.max_tokens,
                }
                
                # Log request details
                msg_preview = self._truncate_for_log(str(messages))
                logger.info(
                    f"[LLM:{self.instance_name}] >>> POST {self.endpoint_url} (async) | "
                    f"attempt={attempt+1}/2 | model={self.model_name} | "
                    f"temp={self.temperature} | max_tokens={self.max_tokens}"
                )
                logger.debug(
                    f"[LLM:{self.instance_name}] Request payload: {msg_preview}"
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
                        f"[LLM:{self.instance_name}] <<< Response: {response.status_code} | "
                        f"elapsed={elapsed:.2f}s (async)"
                    )
                    
                except httpx.RequestError as e:
                    elapsed = time.time() - start_time
                    logger.error(
                        f"[LLM:{self.instance_name}] Request FAILED (async) | "
                        f"error={type(e).__name__}: {str(e)[:100]} | elapsed={elapsed:.2f}s"
                    )
                    raise
                
                # Handle 401 - refresh token and retry
                if response.status_code == 401 and attempt == 0:
                    logger.warning(
                        f"[LLM:{self.instance_name}] Got 401 Unauthorized (async) | "
                        f"Invalidating token and retrying..."
                    )
                    self.token_cache.invalidate()
                    token = await self.token_cache.get_token_async()
                    logger.info(f"[LLM:{self.instance_name}] New token acquired (async): ...{token[-20:]}")
                    continue
                
                # Handle rate limiting
                if response.status_code == 429:
                    retry_after = int(response.headers.get("Retry-After", 60))
                    logger.warning(
                        f"[LLM:{self.instance_name}] Got 429 Rate Limited (async) | "
                        f"Retry-After={retry_after}s | Waiting..."
                    )
                    await asyncio.sleep(retry_after)
                    continue
                
                # Log response body for debugging
                try:
                    response_data = response.json()
                    response_preview = self._truncate_for_log(json.dumps(response_data))
                    logger.debug(f"[LLM:{self.instance_name}] Response body: {response_preview}")
                except Exception:
                    logger.debug(f"[LLM:{self.instance_name}] Response body (raw): {response.text[:200]}")
                
                response.raise_for_status()
                return response.json()
        
        logger.error(f"[LLM:{self.instance_name}] FAILED after all retries (async)")
        raise RuntimeError(f"[{self.instance_name}] Failed to call LLM after retries")
    
    def _generate(
        self,
        messages: List[BaseMessage],
        stop: Optional[List[str]] = None,
        run_manager: Optional[CallbackManagerForLLMRun] = None,
        **kwargs: Any,
    ) -> ChatResult:
        """Sync generation - required by BaseChatModel.
        
        Args:
            messages: List of LangChain messages
            stop: Optional stop sequences
            run_manager: Optional callback manager
            **kwargs: Additional arguments
            
        Returns:
            ChatResult with generated response
            
        Raises:
            Exception: If API call fails (after automatic stats tracking)
        """
        logger.info(
            f"[LLM:{self.instance_name}] _generate() called | "
            f"message_count={len(messages)}"
        )
        
        formatted = self._format_messages(messages)
        
        try:
            # API call with auth retry
            data = self._call_with_auth_retry(formatted)
            
            # Parse response - API returns payload.choices structure
            if "payload" in data:
                content = data["payload"]["choices"][0]["message"]["content"]
            else:
                # Fallback for standard OpenAI format
                content = data["choices"][0]["message"]["content"]
            
            # Record success - AUTOMATIC, services don't do this
            self.stats_tracker.record_request(self.instance_name, "llm", success=True)
            
            logger.info(
                f"[LLM:{self.instance_name}] _generate() SUCCESS | "
                f"response_length={len(content)}"
            )
            logger.debug(
                f"[LLM:{self.instance_name}] Response content: {self._truncate_for_log(content)}"
            )
            
            return ChatResult(
                generations=[ChatGeneration(message=AIMessage(content=content))]
            )
        except Exception as e:
            # Record failure - AUTOMATIC
            self.stats_tracker.record_request(self.instance_name, "llm", success=False)
            logger.error(
                f"[LLM:{self.instance_name}] _generate() FAILED | "
                f"error={type(e).__name__}: {str(e)[:100]}"
            )
            raise
    
    async def _agenerate(
        self,
        messages: List[BaseMessage],
        stop: Optional[List[str]] = None,
        run_manager: Optional[AsyncCallbackManagerForLLMRun] = None,
        **kwargs: Any,
    ) -> ChatResult:
        """Async generation.
        
        Args:
            messages: List of LangChain messages
            stop: Optional stop sequences
            run_manager: Optional async callback manager
            **kwargs: Additional arguments
            
        Returns:
            ChatResult with generated response
            
        Raises:
            Exception: If API call fails (after automatic stats tracking)
        """
        logger.info(
            f"[LLM:{self.instance_name}] _agenerate() called (async) | "
            f"message_count={len(messages)}"
        )
        
        formatted = self._format_messages(messages)
        
        try:
            data = await self._call_with_auth_retry_async(formatted)
            
            # Parse response - API returns payload.choices structure
            if "payload" in data:
                content = data["payload"]["choices"][0]["message"]["content"]
            else:
                # Fallback for standard OpenAI format
                content = data["choices"][0]["message"]["content"]
            
            self.stats_tracker.record_request(self.instance_name, "llm", success=True)
            
            logger.info(
                f"[LLM:{self.instance_name}] _agenerate() SUCCESS (async) | "
                f"response_length={len(content)}"
            )
            logger.debug(
                f"[LLM:{self.instance_name}] Response content: {self._truncate_for_log(content)}"
            )
            
            return ChatResult(
                generations=[ChatGeneration(message=AIMessage(content=content))]
            )
        except Exception as e:
            self.stats_tracker.record_request(self.instance_name, "llm", success=False)
            logger.error(
                f"[LLM:{self.instance_name}] _agenerate() FAILED (async) | "
                f"error={type(e).__name__}: {str(e)[:100]}"
            )
            raise
