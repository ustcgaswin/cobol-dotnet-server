"""LangGraph-based Code Generation Agent.

Converts mainframe components (COBOL, copybooks, JCL) to .NET code.
Uses LangGraph StateGraph for agent execution with tool calling.
Includes context management: pruning, summarization, and history dumping.
"""

import json
import re
from datetime import datetime
from pathlib import Path
from typing import Literal, Annotated

from langchain.messages import AnyMessage, SystemMessage, ToolMessage, HumanMessage
from langgraph.graph import StateGraph, START, END
from loguru import logger
from typing_extensions import TypedDict

from app.config.llm_config import get_llm
from app.config.settings import settings
from app.services.codegen_local.prompts import SYSTEM_PROMPT


# Summarization configuration
SUMMARIZE_THRESHOLD = 40  # Trigger summarization at this message count
KEEP_RECENT_MESSAGES = 10  # Keep last N messages after compression
PRUNE_AFTER_EXCHANGES = 4  # Prune tool outputs older than N exchanges (N*2 messages)

# Tools whose outputs should be pruned (large outputs)
PRUNEABLE_TOOLS = {"view_source_file", "read_artifact", "run_dotnet_build", "run_dotnet_test"}

# Special marker to signal message replacement
_RESET_MARKER = "__RESET_MESSAGES__"


def message_reducer(existing: list[AnyMessage], new: list[AnyMessage]) -> list[AnyMessage]:
    """
    Custom reducer for messages that supports both ADD and REPLACE.
    
    If new list starts with a string === _RESET_MARKER, replace existing with rest of new.
    Otherwise, append new to existing (normal behavior).
    """
    if existing is None:
        existing = []
    if not new:
        return existing
    
    # Check for reset marker
    if isinstance(new[0], str) and new[0] == _RESET_MARKER:
        # Replace mode: return everything after the marker
        logger.info(f"Message reducer: REPLACING {len(existing)} messages with {len(new) - 1}")
        return list(new[1:])
    
    # Normal add mode
    return existing + new


class CodegenState(TypedDict):
    """State for the code generation agent."""
    messages: Annotated[list[AnyMessage], message_reducer]
    project_id: str
    iteration_count: int
    codegen_logs_path: str  # Path for dumping session history


def _prune_old_tool_outputs(messages: list[AnyMessage]) -> list[AnyMessage]:
    """
    Replace large tool outputs from old messages with lightweight markers.
    Keeps recent outputs intact so LLM can act on them.
    """
    if len(messages) <= PRUNE_AFTER_EXCHANGES * 2:
        return messages
    
    # Messages to keep unpruned (last N exchanges)
    cutoff = len(messages) - (PRUNE_AFTER_EXCHANGES * 2)
    pruned = []
    
    for i, msg in enumerate(messages):
        if i >= cutoff:
            # Recent message - keep as is
            pruned.append(msg)
        elif isinstance(msg, ToolMessage):
            content = str(msg.content)
            # Check if this is a large output from pruneable tool
            if len(content) > 500:
                lines = content.count('\n')
                marker = f"[Pruned: {lines} lines. Re-call tool if needed.]"
                pruned.append(ToolMessage(content=marker, tool_call_id=msg.tool_call_id))
            else:
                pruned.append(msg)
        else:
            pruned.append(msg)
    
    return pruned


def _extract_structured_state(messages: list[AnyMessage]) -> str:
    """
    Extract progress state from message history deterministically.
    Parses tool call patterns to find converted components and files.
    """
    converted = []
    files_written = []
    errors_count = 0
    
    for msg in messages:
        content = str(getattr(msg, 'content', ''))
        
        # Pattern: log_component_status("NAME", "converted", ...)
        for match in re.finditer(r'log_component_status\(["\']([^"\']+)["\'],\s*["\']converted["\']', content):
            comp = match.group(1)
            if comp not in converted:
                converted.append(comp)
        
        # Pattern match in tool message responses: "Logged status: X = converted"
        for match in re.finditer(r'Logged status:\s*(\S+)\s*=\s*converted', content):
            comp = match.group(1)
            if comp not in converted:
                converted.append(comp)
        
        # Pattern: write_code_file("PATH", ...)
        for match in re.finditer(r'write_code_file\(["\']([^"\']+)["\']', content):
            path = match.group(1)
            if path not in files_written:
                files_written.append(path)
        
        # Pattern: "Wrote file:" in responses
        for match in re.finditer(r'Wrote file:\s*(\S+)', content):
            path = match.group(1)
            if path not in files_written:
                files_written.append(path)
        
        # Count errors
        if 'log_issue(' in content or 'Error:' in content:
            errors_count += 1
    
    # Format summary
    converted_list = ', '.join(converted[-15:]) if converted else 'none'
    if len(converted) > 15:
        converted_list += f' (+{len(converted) - 15} more)'
    
    summary = f"""[SESSION STATE]
Converted: {converted_list} ({len(converted)} total)
Files written: {len(files_written)}
Errors logged: {errors_count}
Continue converting remaining components."""
    
    return summary


def _dump_history(messages: list[AnyMessage], logs_path: Path) -> str:
    """
    Dump full conversation history to JSON file for recovery.
    Returns the filename of the created dump.
    """
    logs_path.mkdir(parents=True, exist_ok=True)
    
    # Find next session number
    existing = list(logs_path.glob("session_*.json"))
    counter = len(existing) + 1
    
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"session_{counter:03d}_{timestamp}.json"
    filepath = logs_path / filename
    
    # Serialize messages
    serialized = []
    for msg in messages:
        entry = {
            "role": msg.__class__.__name__.replace("Message", "").lower(),
            "content": str(getattr(msg, 'content', ''))
        }
        if hasattr(msg, 'tool_calls') and msg.tool_calls:
            entry["tool_calls"] = [
                {"name": tc["name"], "args": tc.get("args", {})}
                for tc in msg.tool_calls
            ]
        if hasattr(msg, 'tool_call_id'):
            entry["tool_call_id"] = msg.tool_call_id
        serialized.append(entry)
    
    dump = {
        "timestamp": datetime.now().isoformat(),
        "message_count": len(messages),
        "messages": serialized
    }
    
    try:
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(dump, f, indent=2, ensure_ascii=False)
        logger.info(f"Dumped session history to {filename}")
    except Exception as e:
        logger.warning(f"Failed to dump session history: {e}")
    
    return filename


def _compress_messages(
    messages: list[AnyMessage],
    logs_path: Path
) -> list[AnyMessage]:
    """
    Compress message history by:
    1. Dumping full history to file
    2. Extracting structured state
    3. Replacing old messages with summary + recent messages
    
    IMPORTANT: We must not split AIMessage/ToolMessage pairs.
    ToolMessages must have their corresponding AIMessage with tool_calls.
    """
    # Dump before compressing
    _dump_history(messages, logs_path)
    
    # Extract state summary
    summary = _extract_structured_state(messages)
    
    # Find a clean cut point - walk backward from target position
    # to find a message that's NOT a ToolMessage (which would need its AIMessage)
    target_keep = KEEP_RECENT_MESSAGES
    
    if len(messages) <= target_keep:
        # No compression needed
        return messages
    
    # Start from target position
    cut_idx = len(messages) - target_keep
    
    # Walk backward until we find a clean boundary (HumanMessage or AIMessage without pending tool calls)
    while cut_idx < len(messages):
        msg = messages[cut_idx]
        
        # ToolMessage needs its AIMessage - keep looking
        if isinstance(msg, ToolMessage):
            cut_idx += 1
            continue
        
        # Check if this is an AIMessage with tool_calls whose ToolMessages we'd cut
        if hasattr(msg, 'tool_calls') and msg.tool_calls:
            # Need to keep this AND its ToolMessages
            # Check if next messages are ToolMessages for these calls
            tool_call_ids = {tc["id"] for tc in msg.tool_calls}
            next_idx = cut_idx + 1
            while next_idx < len(messages):
                next_msg = messages[next_idx]
                if isinstance(next_msg, ToolMessage) and next_msg.tool_call_id in tool_call_ids:
                    tool_call_ids.discard(next_msg.tool_call_id)
                    next_idx += 1
                else:
                    break
            # If we haven't collected all ToolMessages, skip this AIMessage
            if tool_call_ids:
                cut_idx += 1
                continue
        
        # Clean boundary found
        break
    
    # Take from cut_idx onwards
    recent = messages[cut_idx:]
    
    # Create new compressed history
    compressed = [HumanMessage(content=summary)] + list(recent)
    
    logger.info(f"Compressed {len(messages)} messages to {len(compressed)} (cut at index {cut_idx})")
    return compressed


def create_codegen_agent(tools: list, project_id: str):
    """Create the Code Generation LangGraph agent.
    
    Args:
        tools: List of tools to bind
        project_id: Project ID for scoping operations
        
    Returns:
        Compiled LangGraph agent
    """
    model = get_llm()
    
    tools_by_name = {tool.name: tool for tool in tools}
    model_with_tools = model.bind_tools(tools)
    
    def llm_call(state: CodegenState) -> dict:
        """LLM decides whether to call a tool or respond."""
        messages = state["messages"]
        logs_path = Path(state.get("codegen_logs_path", ""))
        should_reset = False
        
        # Step 1: Prune old tool outputs
        messages = _prune_old_tool_outputs(messages)
        
        # Step 2: Compress if above threshold
        if len(messages) > SUMMARIZE_THRESHOLD and logs_path:
            messages = _compress_messages(messages, logs_path)
            should_reset = True  # Need to replace state, not add
        
        # Invoke LLM with system prompt + processed messages
        full_messages = [SystemMessage(content=SYSTEM_PROMPT)] + messages
        response = model_with_tools.invoke(full_messages)
        
        if should_reset:
            # Replace all messages with compressed + response
            return {
                "messages": [_RESET_MARKER] + messages + [response],
                "iteration_count": state.get("iteration_count", 0) + 1,
            }
        else:
            # Normal add mode
            return {
                "messages": [response],
                "iteration_count": state.get("iteration_count", 0) + 1,
            }
    
    def tool_node(state: CodegenState) -> dict:
        """Execute tool calls."""
        result = []
        last_message = state["messages"][-1]
        
        for tool_call in last_message.tool_calls:
            tool = tools_by_name.get(tool_call["name"])
            if not tool:
                observation = f"Error: Unknown tool '{tool_call['name']}'"
            else:
                try:
                    observation = tool.invoke(tool_call["args"])
                except Exception as e:
                    observation = f"Error: {e}"
                    logger.error(f"Tool {tool_call['name']} failed: {e}")
            
            result.append(
                ToolMessage(content=str(observation), tool_call_id=tool_call["id"])
            )
        
        return {"messages": result}
    
    def should_continue(state: CodegenState) -> Literal["tool_node", "__end__"]:
        """Decide if we should continue the loop or stop."""
        messages = state["messages"]
        last_message = messages[-1]
        
        # Higher iteration limit for complex conversions
        if state.get("iteration_count", 0) >= 500:
            logger.warning(f"Codegen agent reached iteration limit (500)")
            return END
        
        if last_message.tool_calls:
            return "tool_node"
        
        return END
    
    # Build the graph
    builder = StateGraph(CodegenState)
    
    builder.add_node("llm_call", llm_call)
    builder.add_node("tool_node", tool_node)
    
    builder.add_edge(START, "llm_call")
    builder.add_conditional_edges(
        "llm_call",
        should_continue,
        ["tool_node", END],
    )
    builder.add_edge("tool_node", "llm_call")
    
    agent = builder.compile()
    
    logger.info(f"Created Codegen Agent with {len(tools)} tools for project {project_id}")
    return agent
