"""Custom LLM client with OAuth2 authentication.

Implements LangChain's BaseChatModel for full compatibility with
LangChain/LangGraph agents and chains, including tool calling.
"""

import asyncio
import json
import time
from typing import Any, List, Optional, Sequence

import httpx
import requests
from langchain_core.callbacks import CallbackManagerForLLMRun, AsyncCallbackManagerForLLMRun
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import (
    AIMessage,
    BaseMessage,
    HumanMessage,
    SystemMessage,
    ToolMessage,
)
from langchain_core.outputs import ChatGeneration, ChatResult
from langchain_core.tools import BaseTool
from loguru import logger
from pydantic import Field

from app.config.llm.token_cache import TokenCache
from app.config.llm.stats import LLMStats
from app.config.llm.tracing import trace_llm_call


# Retry configuration
MAX_RETRIES = 20
DEFAULT_RETRY_WAIT = 75  # seconds


class OAuthLLMClient(BaseChatModel):
    """Custom LLM client with OAuth2 authentication.
    
    Implements BaseChatModel for full LangChain compatibility.
    Stats tracking is automatic - services don't need to handle it.
    
    Features:
    - OAuth2 token management with automatic refresh
    - Retry on 401, 429, 502 errors
    - Automatic usage statistics tracking
    - Tool calling support (OpenAI-compatible)
    - Both sync and async support
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
        return "oauth-custom-llm"
    
    @property
    def _identifying_params(self) -> dict[str, Any]:
        return {
            "instance_name": self.instance_name,
            "endpoint_url": self.endpoint_url,
            "model_name": self.model_name,
            "temperature": self.temperature,
            "max_tokens": self.max_tokens,
        }
    
    def bind_tools(
        self,
        tools: Sequence[BaseTool],
        **kwargs: Any,
    ) -> "OAuthLLMClient":
        """Bind tools to the model for tool calling.
        
        Returns a new Runnable that will pass tools to _generate/_agenerate.
        """
        from langchain_core.runnables import RunnableBinding
        return RunnableBinding(bound=self, kwargs={"tools": tools, **kwargs})
    
    def _format_messages(self, messages: List[BaseMessage]) -> list[dict]:
        """Convert LangChain messages to OpenAI API format."""
        formatted = []
        for msg in messages:
            if isinstance(msg, SystemMessage):
                formatted.append({"role": "system", "content": msg.content})
            
            elif isinstance(msg, HumanMessage):
                formatted.append({"role": "user", "content": msg.content})
            
            elif isinstance(msg, AIMessage):
                # AI message - may include tool_calls
                message_dict = {"role": "assistant"}
                
                # Content should be string (empty is OK) or null only if tool_calls present
                if msg.content:
                    message_dict["content"] = msg.content
                elif hasattr(msg, "tool_calls") and msg.tool_calls:
                    message_dict["content"] = None  # OpenAI spec: null OK when tool_calls present
                else:
                    message_dict["content"] = ""  # Empty string when no tool_calls
                
                # Include tool_calls if present
                if hasattr(msg, "tool_calls") and msg.tool_calls:
                    message_dict["tool_calls"] = [
                        {
                            "id": tc.get("id", tc.get("name", "")),
                            "type": "function",
                            "function": {
                                "name": tc["name"],
                                "arguments": json.dumps(tc["args"]) if isinstance(tc["args"], dict) else tc["args"],
                            },
                        }
                        for tc in msg.tool_calls
                    ]
                
                formatted.append(message_dict)
            
            elif isinstance(msg, ToolMessage):
                # Tool response message
                formatted.append({
                    "role": "tool",
                    "content": msg.content,
                    "tool_call_id": msg.tool_call_id,
                })
            
            else:
                # Fallback for unknown message types
                formatted.append({"role": "user", "content": str(msg.content)})
        
        return formatted
    
    def _format_tools(self, tools: Sequence[BaseTool]) -> list[dict]:
        """Convert LangChain tools to OpenAI API format."""
        formatted = []
        for tool in tools:
            formatted.append({
                "type": "function",
                "function": {
                    "name": tool.name,
                    "description": tool.description or "",
                    "parameters": tool.args_schema.schema() if tool.args_schema else {"type": "object", "properties": {}},
                },
            })
        return formatted
    
    def _log_error_response(self, status_code: int, response: requests.Response) -> None:
        """Log error response details."""
        try:
            body = response.json()
        except Exception:
            body = response.text[:500]
        logger.warning(
            f"[LLM:{self.instance_name}] Got {status_code} | response={body}"
        )
    
    def _log_error_response_async(self, status_code: int, response: httpx.Response) -> None:
        """Log error response details (async)."""
        try:
            body = response.json()
        except Exception:
            body = response.text[:500]
        logger.warning(
            f"[LLM:{self.instance_name}] Got {status_code} | response={body}"
        )
    
    def _call_with_retry(self, messages: list[dict], tools: list[dict] | None = None) -> dict:
        """Make API call with retry logic (sync)."""
        token = self.token_cache.get_token()
        
        for attempt in range(MAX_RETRIES):
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
            
            # Include tools if provided
            if tools:
                payload["tools"] = tools
            
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
                logger.warning(f"[LLM:{self.instance_name}] Refreshing token (attempt {attempt + 1}/{MAX_RETRIES})")
                self.token_cache.invalidate()
                token = self.token_cache.get_token()
                continue
            
            # Handle 429 - rate limited
            if response.status_code == 429:
                self._log_error_response(429, response)
                retry_after = int(response.headers.get("Retry-After", DEFAULT_RETRY_WAIT))
                logger.warning(f"[LLM:{self.instance_name}] Rate limited, waiting {retry_after}s (attempt {attempt + 1}/{MAX_RETRIES})")
                time.sleep(retry_after)
                continue
            
            # Handle 502 - bad gateway
            if response.status_code == 502:
                self._log_error_response(502, response)
                logger.warning(f"[LLM:{self.instance_name}] Bad gateway, waiting {DEFAULT_RETRY_WAIT}s (attempt {attempt + 1}/{MAX_RETRIES})")
                time.sleep(DEFAULT_RETRY_WAIT)
                continue
            
            # Handle 400 - bad request (log request AND response for debugging)
            if response.status_code == 400:
                logger.error(f"[LLM:{self.instance_name}] 400 Bad Request")
                logger.error(f"[LLM:{self.instance_name}] REQUEST payload: {json.dumps(payload, indent=2, default=str)}")
                try:
                    resp_body = response.json()
                except Exception:
                    resp_body = response.text[:1000]
                logger.error(f"[LLM:{self.instance_name}] RESPONSE body: {resp_body}")
            
            response.raise_for_status()
            return response.json()
        
        raise RuntimeError(f"[{self.instance_name}] Failed to call LLM after {MAX_RETRIES} retries")
    
    async def _call_with_retry_async(self, messages: list[dict], tools: list[dict] | None = None) -> dict:
        """Make API call with retry logic (async)."""
        token = await self.token_cache.get_token_async()
        
        async with httpx.AsyncClient(verify=self.ssl_verify) as client:
            for attempt in range(MAX_RETRIES):
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
                
                # Include tools if provided
                if tools:
                    payload["tools"] = tools
                
                response = await client.post(
                    self.endpoint_url,
                    json=payload,
                    headers=headers,
                    timeout=self.timeout,
                )
                
                # Handle 401 - refresh token and retry
                if response.status_code == 401:
                    self._log_error_response_async(401, response)
                    logger.warning(f"[LLM:{self.instance_name}] Refreshing token (attempt {attempt + 1}/{MAX_RETRIES})")
                    self.token_cache.invalidate()
                    token = await self.token_cache.get_token_async()
                    continue
                
                # Handle 429 - rate limited
                if response.status_code == 429:
                    self._log_error_response_async(429, response)
                    retry_after = int(response.headers.get("Retry-After", DEFAULT_RETRY_WAIT))
                    logger.warning(f"[LLM:{self.instance_name}] Rate limited, waiting {retry_after}s (attempt {attempt + 1}/{MAX_RETRIES})")
                    await asyncio.sleep(retry_after)
                    continue
                
                # Handle 502 - bad gateway
                if response.status_code == 502:
                    self._log_error_response_async(502, response)
                    logger.warning(f"[LLM:{self.instance_name}] Bad gateway, waiting {DEFAULT_RETRY_WAIT}s (attempt {attempt + 1}/{MAX_RETRIES})")
                    await asyncio.sleep(DEFAULT_RETRY_WAIT)
                    continue
                
                # Handle 400 - bad request (log request AND response for debugging)
                if response.status_code == 400:
                    logger.error(f"[LLM:{self.instance_name}] 400 Bad Request")
                    logger.error(f"[LLM:{self.instance_name}] REQUEST payload: {json.dumps(payload, indent=2, default=str)}")
                    try:
                        resp_body = response.json()
                    except Exception:
                        resp_body = response.text[:1000]
                    logger.error(f"[LLM:{self.instance_name}] RESPONSE body: {resp_body}")
                
                response.raise_for_status()
                return response.json()
        
        raise RuntimeError(f"[{self.instance_name}] Failed to call LLM after {MAX_RETRIES} retries")
    
    def _parse_response(self, data: dict) -> AIMessage:
        """Parse API response into AIMessage with optional tool_calls."""
        # API returns payload.choices structure
        message = data["payload"]["choices"][0]["message"]
        
        content = message.get("content") or ""
        tool_calls_raw = message.get("tool_calls")
        
        # Parse tool_calls if present
        tool_calls = []
        if tool_calls_raw:
            for tc in tool_calls_raw:
                func = tc.get("function", {})
                args = func.get("arguments", "{}")
                
                # Parse arguments - they come as JSON string
                if isinstance(args, str):
                    try:
                        args = json.loads(args)
                    except json.JSONDecodeError:
                        args = {}
                
                tool_calls.append({
                    "id": tc.get("id", ""),
                    "name": func.get("name", ""),
                    "args": args,
                })
        
        return AIMessage(content=content, tool_calls=tool_calls)
    
    def _extract_usage(self, data: dict) -> dict[str, int]:
        """Extract token usage from response."""
        # Check top-level usage or nested payload usage
        usage = data.get("usage") or data.get("payload", {}).get("usage") or {}
        return {
            "total": usage.get("total_tokens", 0),
            "prompt": usage.get("prompt_tokens", 0),
            "completion": usage.get("completion_tokens", 0),
        }

    def _format_messages_anthropic(self, messages: List[BaseMessage]) -> tuple[dict | None, list[dict]]:
        """Format messages for Anthropic (extract system message)."""
        system_msg = None
        formatted = []
        
        for msg in messages:
            if isinstance(msg, SystemMessage):
                # Anthropic uses top-level system parameter
                system_msg = {"type": "text", "text": msg.content}
            
            elif isinstance(msg, HumanMessage):
                # Use content blocks format as shown in image
                content_blocks = [{"type": "text", "text": msg.content}]
                formatted.append({"role": "user", "content": content_blocks})
            
            elif isinstance(msg, AIMessage):
                content_blocks = [{"type": "text", "text": msg.content or ""}]
                formatted.append({"role": "assistant", "content": content_blocks})
            
            elif isinstance(msg, ToolMessage):
                # Anthropic tool results are user messages with tool_result content blocks
                # For now, simplistic mapping as user message
                formatted.append({"role": "user", "content": [{"type": "text", "text": f"Tool Result [{msg.tool_call_id}]: {msg.content}"}]})
            
            else:
                formatted.append({"role": "user", "content": [{"type": "text", "text": str(msg.content)}]})
        
        return system_msg, formatted

    def _parse_response_anthropic(self, data: dict) -> AIMessage:
        """Parse Anthropic API response."""
        # Check for payload wrapper as seen in image
        payload = data.get("payload", {})
        content_Wrapper = payload.get("content")
        
        # If content not in payload, check top-level (direct API)
        if not content_Wrapper:
             content_blocks = data.get("content", [])
        else:
             content_blocks = content_Wrapper

        text_content = ""
        
        for block in content_blocks:
            if block.get("type") == "text":
                text_content += block.get("text", "")
        
        # TODO: Handle tool_use blocks if we implement tool calling for Anthropic
        
        return AIMessage(content=text_content)

    def _call_with_retry_anthropic(self, messages: list[dict], tools: list[dict] | None = None) -> dict:
        """Anthropic-specific API call with retry."""
        token = self.token_cache.get_token()
        
        for attempt in range(MAX_RETRIES):
            headers = {
                "Authorization": f"Bearer {token}",
                "Content-Type": "application/json",
                "anthropic-version": "2023-06-01"
            }
            
            payload = {
                "model": self.model_name,
                "messages": messages,
                "max_tokens": self.max_tokens,
                "temperature": self.temperature,
            }
            
            if tools:
                payload["tools"] = tools
            
            response = requests.post(
                self.endpoint_url,
                json=payload,
                headers=headers,
                timeout=self.timeout,
                verify=self.ssl_verify,
            )
            
            if response.status_code == 401:
                self._log_error_response(401, response)
                logger.warning(f"[LLM:{self.instance_name}] Refreshing token (attempt {attempt + 1}/{MAX_RETRIES})")
                self.token_cache.invalidate()
                token = self.token_cache.get_token()
                continue
                
            if response.status_code == 429:
                retry_after = int(response.headers.get("Retry-After", DEFAULT_RETRY_WAIT))
                time.sleep(retry_after)
                continue
                
            if response.status_code == 502:
                time.sleep(DEFAULT_RETRY_WAIT)
                continue
                
            response.raise_for_status()
            return response.json()
        
        raise RuntimeError(f"[{self.instance_name}] Failed to call Anthropic LLM")

    async def _call_with_retry_async_anthropic(self, messages: list[dict], tools: list[dict] | None = None) -> dict:
        """Async Anthropic-specific API call."""
        token = await self.token_cache.get_token_async()
        
        async with httpx.AsyncClient(verify=self.ssl_verify) as client:
            for attempt in range(MAX_RETRIES):
                headers = {
                    "Authorization": f"Bearer {token}",
                    "Content-Type": "application/json",
                    "anthropic-version": "2023-06-01"
                }
                
                payload = {
                    "model": self.model_name,
                    "messages": messages,
                    "max_tokens": self.max_tokens,
                    "temperature": self.temperature,
                }
                
                if tools:
                    payload["tools"] = tools
                
                response = await client.post(
                    self.endpoint_url,
                    json=payload,
                    headers=headers,
                    timeout=self.timeout,
                )
                
                if response.status_code == 401:
                    self._log_error_response_async(401, response)
                    self.token_cache.invalidate()
                    token = await self.token_cache.get_token_async()
                    continue
                    
                if response.status_code == 429:
                    retry_after = int(response.headers.get("Retry-After", DEFAULT_RETRY_WAIT))
                    await asyncio.sleep(retry_after)
                    continue
                    
                if response.status_code == 502:
                    await asyncio.sleep(DEFAULT_RETRY_WAIT)
                    continue
                    
                response.raise_for_status()
                return response.json()
        
        raise RuntimeError(f"[{self.instance_name}] Failed to call Anthropic LLM")

    def _generate(
        self,
        messages: List[BaseMessage],
        stop: Optional[List[str]] = None,
        run_manager: Optional[CallbackManagerForLLMRun] = None,
        **kwargs: Any,
    ) -> ChatResult:
        """Sync generation - required by BaseChatModel."""
        # Standard OpenAI Formatting
        formatted_messages = self._format_messages(messages)
        
        # Format tools if provided (from bind_tools)
        tools = kwargs.get("tools")
        formatted_tools = None
        if tools:
            formatted_tools = self._format_tools(tools)
        
        try:
            with trace_llm_call(
                instance_name=self.instance_name,
                model_name=self.model_name,
                messages=messages,
                temperature=self.temperature,
                max_tokens=self.max_tokens,
            ) as trace:
                try:
                    # --- OPENAI CALL (Active) ---
                    data = self._call_with_retry(formatted_messages, formatted_tools)
                    ai_message = self._parse_response(data)
                    
                    # --- ANTHROPIC CALL (Uncomment to use) ---
                    # system_msg, ant_messages = self._format_messages_anthropic(messages)
                    # # Add system to payload if supported in _call_with_retry_anthropic
                    # data = self._call_with_retry_anthropic(ant_messages, formatted_tools)
                    # ai_message = self._parse_response_anthropic(data)
                    
                    usage = self._extract_usage(data)
                    
                    self.stats_tracker.record_request(
                        self.instance_name, "llm", success=True, tokens=usage
                    )
                    
                    result = ChatResult(
                        generations=[ChatGeneration(message=ai_message)]
                    )
                    
                    # Record success in trace
                    output_data = {"content": ai_message.content}
                    if ai_message.tool_calls:
                        output_data["tool_calls"] = ai_message.tool_calls
                    
                    trace.set_result(output_data, usage)
                    return result
                    
                except Exception as e:
                    trace.set_error(e)
                    raise e

        except Exception as e:
            self.stats_tracker.record_request(self.instance_name, "llm", success=False)
            logger.error(f"[LLM:{self.instance_name}] Generation failed: {e}")
            raise
    
    async def _agenerate(
        self,
        messages: List[BaseMessage],
        stop: Optional[List[str]] = None,
        run_manager: Optional[AsyncCallbackManagerForLLMRun] = None,
        **kwargs: Any,
    ) -> ChatResult:
        """Async generation."""
        # Standard OpenAI Formatting
        formatted_messages = self._format_messages(messages)
        
        # Format tools if provided (from bind_tools)
        tools = kwargs.get("tools")
        formatted_tools = None
        if tools:
            formatted_tools = self._format_tools(tools)
        
        try:
            with trace_llm_call(
                instance_name=self.instance_name,
                model_name=self.model_name,
                messages=messages,
                temperature=self.temperature,
                max_tokens=self.max_tokens,
            ) as trace:
                try:
                    # --- OPENAI CALL (Active) ---
                    data = await self._call_with_retry_async(formatted_messages, formatted_tools)
                    ai_message = self._parse_response(data)
                    
                    # --- ANTHROPIC CALL (Uncomment to use) ---
                    # system_msg, ant_messages = self._format_messages_anthropic(messages)
                    # # Add system to payload if supported in _call_with_retry_async_anthropic
                    # data = await self._call_with_retry_async_anthropic(ant_messages, formatted_tools)
                    # ai_message = self._parse_response_anthropic(data)

                    usage = self._extract_usage(data)
                    
                    self.stats_tracker.record_request(
                        self.instance_name, "llm", success=True, tokens=usage
                    )
                    
                    result = ChatResult(
                        generations=[ChatGeneration(message=ai_message)]
                    )
                    
                    # Record success in trace
                    output_data = {"content": ai_message.content}
                    if ai_message.tool_calls:
                        output_data["tool_calls"] = ai_message.tool_calls
                    
                    trace.set_result(output_data, usage)
                    return result
                    
                except Exception as e:
                    trace.set_error(e)
                    raise e

        except Exception as e:
            self.stats_tracker.record_request(self.instance_name, "llm", success=False)
            logger.error(f"[LLM:{self.instance_name}] Generation failed: {e}")
            raise
