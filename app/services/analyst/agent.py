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
    model = get_llm(DOCGEN, model=LLMModel.GPT4_1_MINI)
    
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
    
    
    def verification_gate(state: AnalystState) -> dict:
        """Verifier that gates completion based on required artifacts."""
        messages = state["messages"]
        iteration = state.get("iteration_count", 0)
        
        # Don't verify if we're hitting the hard limit (let it end)
        if iteration >= 200:
            return {"messages": [HumanMessage(content="Hard iteration limit reached. Stopping.")]}

        # Check for completeness
        # We need to peek at the artifact tools or file system.
        # Since this is a pure node, we'll use the bound tools helper or direct check.
        # But we don't have direct access to tools here easily without importing.
        # BETTER STRATEGY: Use the message history or just check the file system directly?
        # Direct file check is safest for a "Supervisor" node.
        
        from app.config.settings import settings
        from pathlib import Path
        import json
        
        artifacts_path = Path(settings.PROJECT_ARTIFACTS_PATH) / project_id
        ctx_path = artifacts_path / "system_context"
        
        issues = []
        
        import re

        # 1. Functionality Catalog
        func_cat = ctx_path / "functionality_catalog.md"
        if not func_cat.exists() or len(func_cat.read_text(encoding="utf-8")) < 50:
            issues.append("ERROR: `functionality_catalog.md` is missing or empty.")
        else:
            content = func_cat.read_text(encoding="utf-8")
            # Check for IDs: Must match ### F001, ### F002, etc.
            if not re.search(r"###\s*F\d{3}", content):
                issues.append("ERROR: `functionality_catalog.md` must contain functionality IDs in format '### F001', '### F002', etc.")

        # 2. Job Chains
        job_chains = ctx_path / "job_chains.md"
        if not job_chains.exists():
             issues.append("WARNING: `job_chains.md` has not been created. If there are no jobs, create it with a 'No jobs found' note.")

        # 3. File Processing Coverage (Strict)
        tracker = ctx_path / "_processing_tracker.json"
        if tracker.exists():
            try:
                data = json.loads(tracker.read_text(encoding="utf-8"))
                pending = data.get("pending_files", [])
                if pending:
                    count = len(pending)
                    issues.append(f"ERROR: {count} files are still unprocessed. Use `get_unprocessed_files` and process them. All files must be accounted for.")
            except:
                issues.append("WARNING: Could not parse `_processing_tracker.json`.")
        else:
            issues.append("ERROR: `_processing_tracker.json` not found. You must initialize the file tracker.")

        # 4. Gaps Analysis (Check for CA-7/Process Flow refs)
        gaps_file = ctx_path / "gaps.md"
        if gaps_file.exists():
            gaps_content = gaps_file.read_text(encoding="utf-8")
            # Check each gap block for CA-7 rules
            gap_blocks = re.split(r"^### Gap:", gaps_content, flags=re.MULTILINE)
            for block in gap_blocks:
                if not block.strip(): continue
                
                lower_block = block.lower()
                # If gap is about scheduling/CA-7
                if "ca-7" in lower_block or "scheduler" in lower_block or "schedule" in lower_block:
                    # Must reference process_flow or manual process
                    if "process_flow" not in lower_block and "manual process" not in lower_block:
                         issues.append("ERROR: Found CA-7/Schedule gap without reference to 'process_flow.md'. You must check manual process flows first.")

        if not issues:
            return {"messages": [HumanMessage(content="[Automated Verification] Verification passed. Analysis complete.")]}
        
        # Logic for retries
        # We check how many "verification failures" we've had.
        # Simplest way: check how many HumanMessages come from THIS node.
        # Or just use a simple counter if we had one.
        # For now, let's rely on the LLM to fix it.
        # But we need to avoid infinite loops.
        # Let's count how many times we see "Verification Failed" in history.
        
        failure_count = sum(1 for m in messages if isinstance(m, HumanMessage) and "Verification Failed" in str(m.content))
        
        if failure_count > 2:
            logger.warning("Verification failed too many times. Stopping.")
            return {"messages": [HumanMessage(content=f"[Automated Verification] Verification failed 3 times. Stopping. Outstanding issues:\n" + "\n".join(issues))]}
            
        return {"messages": [HumanMessage(content="[Automated Verification] Verification Failed. Auto-Retrying. You must fix these issues before finishing:\n" + "\n".join(issues))]}

    def should_continue(state: AnalystState) -> Literal["tool_node", "verification_gate", "__end__"]:
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
        
        # If the LLM *says* it's done (no tool calls), we send it to verification
        # But we need to distinguish between "Verification passed" (END) and "Ready to stop" (GATE)
        # If the LAST message was a HumanMessage saying "Verification passed", then END.
        if isinstance(last_message, HumanMessage) and "Verification passed" in str(last_message.content):
            return END
            
        # If the LAST message was a HumanMessage saying "Verification Failed... Stopping", then END.
        if isinstance(last_message, HumanMessage) and "Stopping" in str(last_message.content):
             return END

        # Otherwise, the LLM thinks it's done. Send to Gate.
        return "verification_gate"
    
    # Build the graph
    builder = StateGraph(AnalystState)
    
    # Add nodes
    builder.add_node("llm_call", llm_call)
    builder.add_node("tool_node", tool_node)
    builder.add_node("verification_gate", verification_gate)
    
    # Add edges
    builder.add_edge(START, "llm_call")
    builder.add_edge("tool_node", "llm_call")
    
    builder.add_conditional_edges(
        "llm_call",
        should_continue,
        ["tool_node", "verification_gate", END],
    )
    
    # Gate either loops back to LLM (with feedback) or Ends
    # We essentially rely on the `llm_call` receiving the injected HumanMessage
    builder.add_edge("verification_gate", "llm_call")
    
    # Compile
    agent = builder.compile()
    
    logger.info(f"Created LangGraph Analyst Agent with {len(tools)} tools")
    return agent
