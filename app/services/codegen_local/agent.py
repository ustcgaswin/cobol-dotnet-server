"""LangGraph-based Code Generation Agent.

Converts mainframe components (COBOL, copybooks, JCL) to .NET code.
Uses LangGraph StateGraph for agent execution with tool calling.
"""

import operator
from typing import Literal, Annotated

from langchain.messages import AnyMessage, SystemMessage, ToolMessage
from langgraph.graph import StateGraph, START, END
from loguru import logger
from typing_extensions import TypedDict

from app.config.llm import get_llm, CODEGEN
from app.services.codegen_local.prompts import SYSTEM_PROMPT


class CodegenState(TypedDict):
    """State for the code generation agent."""
    messages: Annotated[list[AnyMessage], operator.add]
    project_id: str
    iteration_count: int


def create_codegen_agent(tools: list, project_id: str):
    """Create the Code Generation LangGraph agent.
    
    Args:
        tools: List of tools to bind
        project_id: Project ID for scoping operations
        
    Returns:
        Compiled LangGraph agent
    """
    model = get_llm(CODEGEN)
    
    tools_by_name = {tool.name: tool for tool in tools}
    model_with_tools = model.bind_tools(tools)
    
    def llm_call(state: CodegenState) -> dict:
        """LLM decides whether to call a tool or respond."""
        messages = [SystemMessage(content=SYSTEM_PROMPT)] + state["messages"]
        response = model_with_tools.invoke(messages)
        
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
