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


class AnalystState(TypedDict):
    """State for the analyst agent."""
    messages: Annotated[list[AnyMessage], operator.add]
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
        messages = [SystemMessage(content=SYSTEM_PROMPT)] + state["messages"]
        response = model_with_tools.invoke(messages)
        
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
