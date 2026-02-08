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

    # 3. Start Run
    run = None
    try:
        # nested=True allows us to participate in an existing run if one exists (e.g. parent chain)
        run = mlflow.start_run(nested=True, run_name=f"LLM:{instance_name}")
    except Exception as e:
        logger.warning(f"Failed to start MLflow run: {e}")
    
    trace_result = TraceResult()
    
    try:
        # Log Inputs
        if run:
            try:
                mlflow.log_params({
                    "instance": instance_name,
                    "model": model_name,
                    "temperature": temperature,
                    "max_tokens": max_tokens,
                })
                # Log messages as a JSON artifact or heavy param
                sanitized_msgs = _sanitize_messages(messages)
                mlflow.log_dict(sanitized_msgs, "messages.json")
            except Exception as e:
                logger.warning(f"Failed to log MLflow inputs: {e}")

        # Yield control
        yield trace_result

        # Log Outputs
        if run:
            try:
                if trace_result.error:
                    mlflow.set_tag("status", "ERROR")
                    mlflow.log_param("error", str(trace_result.error))
                else:
                    mlflow.set_tag("status", "OK")
                    if trace_result.output:
                        # Log response
                        mlflow.log_text(str(trace_result.output), "response.txt")
                    
                    if trace_result.usage:
                        # Log metrics
                        for k, v in trace_result.usage.items():
                            mlflow.log_metric(f"token_usage_{k}", v)
            except Exception as e:
                logger.warning(f"Failed to log MLflow outputs: {e}")

    except Exception as e:
        # Catch unexpected errors in the tracing logic itself
        logger.error(f"Error in MLflow tracing context: {e}")
        # Re-raise user code errors if they happened during yield? 
        # No, 'yield' creates a boundary. If exception happens in user code, it bubbles up here.
        # We need to catch it to log it, then re-raise.
        raise e
    finally:
        if run:
            try:
                mlflow.end_run()
            except Exception:
                pass
