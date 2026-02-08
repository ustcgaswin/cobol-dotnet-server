"""MLflow tracing utilities for LLM calls.

This module encapsulates all MLflow dependencies and logic, ensuring:
1.  Safe, lazy importing (no crash if mlflow is missing).
2.  Robust error handling (logging failures don't crash the app).
3.  Sanitization of complex objects (LangChain messages) for logging.
4.  Centralized control via settings.MLFLOW_ENABLED.
"""

from contextlib import contextmanager
from typing import Any, Iterator, Optional, List, Dict
import json
import time

from loguru import logger
from langchain_core.messages import BaseMessage, SystemMessage, HumanMessage, AIMessage, ToolMessage

from app.config.settings import settings


def _sanitize_messages(messages: List[BaseMessage]) -> List[Dict[str, Any]]:
    """Convert LangChain messages to a JSON-safe list of dicts for logging."""
    sanitized = []
    for msg in messages:
        role = "unknown"
        content = getattr(msg, "content", "")
        
        if isinstance(msg, SystemMessage):
            role = "system"
        elif isinstance(msg, HumanMessage):
            role = "user"
        elif isinstance(msg, AIMessage):
            role = "assistant"
            # Handle tool calls in AIMessage
            if hasattr(msg, "tool_calls") and msg.tool_calls:
                tool_names = [tc.get("name") for tc in msg.tool_calls]
                content = f"{content} [Tool Calls: {', '.join(tool_names)}]" if content else f"[Tool Calls: {', '.join(tool_names)}]"
        elif isinstance(msg, ToolMessage):
            role = "tool"
            # Truncate potentially large tool outputs
            s_content = str(content)
            if len(s_content) > 2000:
                content = s_content[:2000] + "... (truncated)"
        
        sanitized.append({
            "role": role,
            "content": str(content)
        })
    return sanitized


class TraceResult:
    """Container for capturing operation results within the trace."""
    def __init__(self):
        self.output: Any = None
        self.usage: Optional[Dict[str, int]] = None
        self.error: Optional[Exception] = None
    
    def set_result(self, output: Any, usage: Optional[Dict[str, int]] = None):
        self.output = output
        self.usage = usage
    
    def set_error(self, error: Exception):
        self.error = error


@contextmanager
def trace_llm_call(
    instance_name: str,
    model_name: str,
    messages: List[BaseMessage],
    temperature: float,
    max_tokens: int,
) -> Iterator[TraceResult]:
    """Context manager to trace an LLM call with MLflow (if enabled).

    Usage:
        with trace_llm_call(instance_name, model, messages, ...) as trace:
            try:
                response = llm.generate(...)
                trace.set_result(response, usage)
            except Exception as e:
                trace.set_error(e)
                raise
    """
    # 1. Feature Flag Check
    if not settings.MLFLOW_ENABLED:
        yield TraceResult()
        return

    # 2. Safe Import
    try:
        import mlflow
    except ImportError:
        logger.warning("MLFLOW_ENABLED is True but 'mlflow' package is not installed. Skipping trace.")
        yield TraceResult()
        return

    
    # 3. Start Span (Tracing API)
    # Use mlflow.start_span context manager correctly
    # 3. Start Span (Tracing API)
    inputs = {
        "instance": instance_name,
        "model": model_name,
        "temperature": temperature,
        "max_tokens": max_tokens,
        "messages": _sanitize_messages(messages)
    }

    trace_result = TraceResult()
    
    # Attempt to start the span
    span = None
    span_ctx = None
    
    try:
        # Get the context manager but don't enter yet
        span_ctx = mlflow.start_span(
            name=f"LLM:{instance_name}",
            span_type="llm"
        )
        # Manually enter to isolate startup errors
        span = span_ctx.__enter__()
        span.set_inputs(inputs)
    except Exception as e:
        logger.warning(f"Failed to start MLflow span: {e}")
        # Return fallback result - this yield is safe because we haven't yielded yet
        yield trace_result
        return

    # If we are here, span is active. We MUST exit it eventually.
    try:
        # Yield control to the caller
        yield trace_result

        # Success path (no exception from caller)
        outputs = {}
        if trace_result.error:
            span.set_status("ERROR")
            outputs["error"] = str(trace_result.error)
        else:
            span.set_status("OK")
            if trace_result.output:
                outputs["response"] = str(trace_result.output)
            
            if trace_result.usage:
                outputs["usage"] = trace_result.usage
                for k, v in trace_result.usage.items():
                    span.set_attribute(f"usage.{k}", v)

        span.set_outputs(outputs)

    except Exception as user_ex:
        # Exception raised by the caller (or our post-processing)
        try:
            span.set_status("ERROR")
            if isinstance(user_ex, Exception):
                span.set_outputs({"error": str(user_ex)})
            logger.error(f"Error in traced execution: {user_ex}")
        except Exception as logger_ex:
            logger.error(f"Failed to log error to span: {logger_ex}")
        
        # Re-raise to caller
        raise user_ex

    finally:
        # Exit the span context
        if span_ctx:
            try:
                 # Pass current exception info if any
                import sys
                exc_type, exc_val, exc_tb = sys.exc_info()
                span_ctx.__exit__(exc_type, exc_val, exc_tb)
            except Exception as exit_ex:
                logger.error(f"Failed to close MLflow span: {exit_ex}")


@contextmanager
def trace_execution(
    name: str,
    inputs: Optional[Dict[str, Any]] = None,
    span_type: str = "chain"
) -> Iterator[TraceResult]:
    """Context manager for high-level execution tracing (e.g. Agent runs).
    
    Creates a parent span that wraps multiple LLM calls.
    """
    # 1. Feature Flag Check
    if not settings.MLFLOW_ENABLED:
        yield TraceResult()
        return

    # 2. Safe Import
    try:
        import mlflow
    except ImportError:
        yield TraceResult()
        return

    trace_result = TraceResult()
    span = None
    span_ctx = None

    try:
        # Get the context manager but don't enter yet
        span_ctx = mlflow.start_span(
            name=name,
            span_type=span_type
        )
        # Manually enter to isolate startup errors
        span = span_ctx.__enter__()
        if inputs:
            span.set_inputs(inputs)
    except Exception as e:
        logger.warning(f"Failed to start MLflow span '{name}': {e}")
        yield trace_result
        return

    # If we are here, span is active.
    try:
        # Yield control to the caller
        yield trace_result

        # Success path
        outputs = {}
        if trace_result.error:
            span.set_status("ERROR")
            outputs["error"] = str(trace_result.error)
        else:
            span.set_status("OK")
            if trace_result.output:
                # Truncate if too long (agents can return huge state)
                s_output = str(trace_result.output)
                if len(s_output) > 5000:
                    outputs["result"] = s_output[:5000] + "... (truncated)"
                else:
                    outputs["result"] = s_output
        
        span.set_outputs(outputs)

    except Exception as user_ex:
        # Exception raised by the caller
        try:
            span.set_status("ERROR")
            span.set_outputs({"error": str(user_ex)})
            logger.error(f"Error in traced execution '{name}': {user_ex}")
        except Exception:
            pass
        
        raise user_ex

    finally:
        # Exit the span context
        if span_ctx:
            try:
                import sys
                exc_type, exc_val, exc_tb = sys.exc_info()
                span_ctx.__exit__(exc_type, exc_val, exc_tb)
            except Exception as exit_ex:
                logger.error(f"Failed to close MLflow span '{name}': {exit_ex}")


def trace_tool(tool: Any) -> Any:
    """Wraps a LangChain tool to trace its execution with MLflow.
    
    Monkey-patches the tool's _run and _arun methods to add spans.
    This provides visibility into WHICH tool is calling the LLM.
    """
    if not settings.MLFLOW_ENABLED:
        return tool

    # Avoid double-wrapping
    if getattr(tool, "_is_traced", False):
        return tool
        
    original_run = tool._run
    original_arun = tool._arun
    
    def _get_inputs(*args, **kwargs):
        """Helper to safely capture inputs."""
        try:
            # Simple heuristic: args[0] is usually the input string/dict
            inputs = {}
            if args:
                inputs["arg_0"] = str(args[0])
            if kwargs:
                inputs.update({k: str(v) for k, v in kwargs.items()})
            return inputs
        except Exception:
            return {"error": "failed_to_serialize_inputs"}

    def wrapped_run(*args, **kwargs):
        inputs = _get_inputs(*args, **kwargs)
        with trace_execution(f"Tool: {tool.name}", inputs=inputs, span_type="tool") as trace:
            try:
                result = original_run(*args, **kwargs)
                trace.set_result(str(result)) # Tools return strings usually
                return result
            except Exception as e:
                trace.set_error(e)
                raise

    async def wrapped_arun(*args, **kwargs):
        inputs = _get_inputs(*args, **kwargs)
        with trace_execution(f"Tool: {tool.name}", inputs=inputs, span_type="tool") as trace:
            try:
                result = await original_arun(*args, **kwargs)
                trace.set_result(str(result))
                return result
            except Exception as e:
                trace.set_error(e)
                raise

    # Patch the methods
    tool._run = wrapped_run
    tool._arun = wrapped_arun
    tool._is_traced = True
    
    return tool
