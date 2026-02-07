"""LangGraph-based System Analyst Agent.

Uses LangGraph StateGraph for proper agent execution with tool calling.
"""

import operator
from typing import Literal, Annotated

from langchain.messages import AnyMessage, SystemMessage, ToolMessage, HumanMessage
from langgraph.graph import StateGraph, START, END
from loguru import logger
from typing_extensions import TypedDict

from app.config.llm import get_llm, DOCGEN, LLMModel
from app.services.analyst.prompts import SYSTEM_PROMPT


# Pruning configuration
PRUNE_AFTER_EXCHANGES = 5  # Prune tool outputs older than N exchanges (N*2 messages)
PRUNEABLE_TOOLS = {"read_artifact", "grep_artifact", "search_docs"}

# Special marker for future proofing (matches codegen pattern)
_RESET_MARKER = "__RESET_MESSAGES__"


def message_reducer(existing: list[AnyMessage], new: list[AnyMessage]) -> list[AnyMessage]:
    """
    Custom reducer for messages that supports both ADD and REPLACE.
    Matches Codegen Agent's pattern for consistency.
    """
    if existing is None:
        existing = []
    if not new:
        return existing
    
    # Check for reset marker
    if isinstance(new[0], str) and new[0] == _RESET_MARKER:
        return list(new[1:])
    
    return existing + new


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
            # Check if this is a large output
            if len(content) > 500:
                lines = content.count('\n')
                marker = f"[Pruned: {lines} lines. Re-call tool if needed.]"
                pruned.append(ToolMessage(content=marker, tool_call_id=msg.tool_call_id))
            else:
                pruned.append(msg)
        else:
            pruned.append(msg)
    
    return pruned


class AnalystState(TypedDict):
    """State for the analyst agent."""
    messages: Annotated[list[AnyMessage], message_reducer]
    project_id: str  # Passed through state, accessible to tools
    iteration_count: int


def create_analyst_agent(tools: list, project_id: str):
    """Create the System Analyst LangGraph agent.
    
    Args:
        tools: List of tools to bind (wrapped with project_id closure)
        project_id: Project ID for scoping tool operations
        
    Returns:
        Compiled LangGraph agent
    """
    # Get LLM from config
    model = get_llm(DOCGEN, model=LLMModel.GPT4O_DEV)
    
    # Bind tools to model
    tools_by_name = {tool.name: tool for tool in tools}
    model_with_tools = model.bind_tools(tools)
    
    def llm_call(state: AnalystState) -> dict:
        """LLM decides whether to call a tool or not."""
        # 1. Prune old tool outputs to save context
        pruned_messages = _prune_old_tool_outputs(state["messages"])
        
        # 2. Check if pruning actually changed anything (optimization)
        # If it did, we need to UPDATE the state with the pruned version
        # But we can't easily "update" past messages with operator.add (it appends).
        # However, our message_reducer supports __RESET_MESSAGES__.
        
        # Strategy: Pass PRUNED messages to the LLM.
        # We don't necessarily need to update state history immediately, 
        # BUT if we don't, the state keeps growing.
        # So we SHOULD update state.
        
        # For this turn, we use pruned messages + System Prompt
        messages = [SystemMessage(content=SYSTEM_PROMPT)] + pruned_messages
        response = model_with_tools.invoke(messages)
        
        # 3. Return response. 
        # Ideally we'd replace state with pruned_messages + response,
        # but for now let's just append response and rely on the fact 
        # that we prune *before* every call.
        # LangGraph state will technically keep growing on disk/memory 
        # unless we use the RESET trick.
        
        # Let's use the RESET trick if we pruned significantly.
        # But for simplicity and safety (as this is a fix), let's just 
        # use the pruned messages for the CONTEXT window (what we send to LLM)
        # and let the state grow (which is fine for history, just bad for token limits if we sent it all).
        # WAIT: modifying 'messages' variable above DOES send only pruned to LLM.
        # So this SOLVES the 400 error / cost issue immediately.
        
        return {
            "messages": [response],
            "iteration_count": state.get("iteration_count", 0) + 1,
        }
    
    def tool_node(state: AnalystState) -> dict:
        """Execute tool calls."""
        result = []
        last_message = state["messages"][-1]
        
        for tool_call in last_message.tool_calls:
            tool = tools_by_name[tool_call["name"]]
            try:
                observation = tool.invoke(tool_call["args"])
            except Exception as e:
                observation = f"Error: {e}"
            
            result.append(
                ToolMessage(content=str(observation), tool_call_id=tool_call["id"])
            )
        
        return {"messages": result}
    
    def should_continue(state: AnalystState) -> Literal["tool_node", "__end__"]:
        """Decide if we should continue the loop or stop."""
        messages = state["messages"]
        last_message = messages[-1]
        
        # Check iteration limit
        if state.get("iteration_count", 0) >= 200:
            logger.warning("Analyst agent reached iteration limit")
            return END
        
        # If the LLM makes a tool call, continue
        if last_message.tool_calls:
            return "tool_node"
        
        # Otherwise, we're done
        return END
    
    # Build the graph
    builder = StateGraph(AnalystState)
    
    # Add nodes
    builder.add_node("llm_call", llm_call)
    builder.add_node("tool_node", tool_node)
    
    # Add edges
    builder.add_edge(START, "llm_call")
    builder.add_conditional_edges(
        "llm_call",
        should_continue,
        ["tool_node", END],
    )
    builder.add_edge("tool_node", "llm_call")
    
    # Compile
    agent = builder.compile()
    
    logger.info(f"Created LangGraph Analyst Agent with {len(tools)} tools")
    return agent
