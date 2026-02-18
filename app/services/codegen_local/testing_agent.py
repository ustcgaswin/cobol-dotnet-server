"""Testing Agent for the Code Generation Service.

Responsible for Phase 2: Generating and verifying unit tests for the compiled solution.
"""

from typing import Annotated, TypedDict, List, Dict, Literal
from langgraph.graph import StateGraph, END, START
from langchain_core.messages import SystemMessage, HumanMessage, ToolMessage, AnyMessage
from loguru import logger
import operator

from app.config.llm import get_llm, CODEGEN, LLMModel
from app.config.settings import settings
from app.services.codegen_local.tools.solution_tools import create_solution_tools
from app.services.codegen_local.tools.build_tools import create_build_tools
from app.services.codegen_local.tools.source_file_tools import create_source_file_tools
from app.services.codegen_local.languages.testing_prompts import (
    TESTING_AGENT_PERSONA, DOTNET_TESTING_INSTRUCTIONS, JAVA_TESTING_INSTRUCTIONS
)

# --- State Definition ---

class TestingState(TypedDict):
    messages: Annotated[List[AnyMessage], operator.add]
    project_id: str
    target_language: str
    output_path: str
    source_path: str
    generated_files: List[str]  # Manifest of files to test
    iteration_count: int        # Global safety limit
    file_fix_attempts: Dict[str, int] # Track retries per file
    build_failed_in_phase_1: bool # Did Phase 1 fail to build?

# --- Nodes ---

def get_model_node(llm, tools):
    """Factory for the model node."""
    model_with_tools = llm.bind_tools(tools)
    tool_map = {t.name: t for t in tools}

    def model_node(state: TestingState):
        """Call the LLM."""
        messages = state["messages"]
        
        # If this is the VERY FIRST call (no messages yet or just system prompt? No, state starts empty usually),
        # we need to inject the System Prompt and Initial User Task.
        # But LangGraph passes state. If 'messages' is empty, we initialize.
        
        if not messages:
            project_id = state["project_id"]
            target_language = state["target_language"]
            build_failed = state.get("build_failed_in_phase_1", False)
            files = state.get("generated_files", [])
            
            # 1. Prepare Context
            lang_instructions = (
                DOTNET_TESTING_INSTRUCTIONS if target_language == "dotnet" else JAVA_TESTING_INSTRUCTIONS
            )
            
            # Filter manifest
            files_to_test = [
                f for f in files
                if ("/Services/" in f or "/Repositories/" in f or "/services/" in f.lower() or "/repositories/" in f.lower()) 
                and not ("/Jobs/" in f or "/jobs/" in f.lower())
            ]
            
            # System Prompt
            system_msg = SystemMessage(content=f"{TESTING_AGENT_PERSONA}\n\n{lang_instructions}")
            
            # Initial User Task
            task_msg = HumanMessage(content=f"""
Phase 1 (Implementation) is complete. 
Build Status: {'FAILED' if build_failed else 'SUCCESS'}.

Files to Test ({len(files_to_test)} candidate files):
{chr(10).join(files_to_test[:50])} {'... (truncated)' if len(files_to_test) > 50 else ''}

Your Task:
1. For each file, read the source code AND the original COBOL logic.
2. Write a semantic Unit Test file.
3. {'SKIP execution (Build Failed). Just write the tests.' if build_failed else 'RUN the tests using the build tools.'}
4. If tests fail, FIX the Service/Repository code, NOT just the test.

Begin.
            """)
            
            messages = [system_msg, task_msg]
            # Update state with initialized messages
            # Note: Since we return a dict to update state, and 'messages' is Annotated with add,
            # this will APPEND. But we want to set initial.
            # actually, if messages is empty in input, we typically just pass them to invoke.
            # We'll just pass these constructed messages + whatever was in state (empty) to the model.
            
            response = model_with_tools.invoke(messages)
            return {"messages": messages + [response], "iteration_count": 1}
        
        else:
            # Normal loop
            response = model_with_tools.invoke(messages)
            return {"messages": [response], "iteration_count": state["iteration_count"] + 1}

    return model_node

def get_tool_node(tools):
    """Factory for the tool execution node."""
    tool_map = {t.name: t for t in tools}
    
    def tool_node(state: TestingState):
        """Execute tools."""
        last_message = state["messages"][-1]
        results = []
        
        if hasattr(last_message, "tool_calls"):
            for tool_call in last_message.tool_calls:
                tool = tool_map.get(tool_call["name"])
                if tool:
                    try:
                        output = tool.invoke(tool_call["args"])
                    except Exception as e:
                        output = f"Error: {e}"
                else:
                    output = f"Error: Tool {tool_call['name']} not found."
                
                results.append(ToolMessage(content=str(output), tool_call_id=tool_call["id"]))
        
        return {"messages": results}
    
    return tool_node

def should_continue(state: TestingState) -> Literal["tools", "__end__"]:
    """Decide next step."""
    last_message = state["messages"][-1]
    
    if hasattr(last_message, "tool_calls") and last_message.tool_calls:
        return "tools"
    
    # Simple formatting check: if LLM says "I am done" or "Task complete", we end.
    # checking for basic stop words or just ending if no tool calls
    # For now, let's assume if no tool calls, it's satisfied.
    return "__end__"

# --- Graph Definition ---

def create_testing_graph(llm_client_unused) -> StateGraph:
    """Create the LangGraph for the Testing Agent.
    
    Args:
        llm_client_unused: Kept for signature compatibility, but we fetch unique model config inside.
    """
    # 1. Setup Model & Tools
    # We re-create tools here because they need runtime values (project_id etc)
    # But wait, create_testing_graph is called ONCE at module level or per request?
    # It's called in service.py: `testing_agent = create_testing_graph(None)`
    # The tools depend on state (project_id), which is only available at runtime node execution.
    # So we need to bind tools INSIDE the node, or use a "config" based tool binding.
    # actually, standard LangGraph pattern is to build the graph once, but the nodes function 
    # receives 'state' which has the data.
    # BUT 'tools' need project_id to be initialized.
    # LIMITATION: We cannot initialize tools dynamically in the compile phase easily if values are in state.
    # SOLUTION: We will initialize tools inside `model_node` and `tool_node` dynamically based on state.
    # This means `get_model_node` needs to be defined logic, not a closure over specific tool instances.
    
    pass 

# Let's redefine properly for dynamic tool creation

def model_node_dynamic(state: TestingState):
    """Dynamics model node that creates tools on the fly."""
    project_id = state["project_id"]
    target_language = state["target_language"]
    output_path = state["output_path"]
    source_path = state["source_path"]
    
    # Re-create tools for this specific run
    solution_tools = create_solution_tools(project_id, output_path, source_path, target_language)
    build_tools = create_build_tools(output_path, target_language, project_id, include_test_tools=True)
    source_tools = create_source_file_tools(project_id, source_path)
    tools = solution_tools + build_tools + source_tools
    
    model = get_llm(CODEGEN, model=LLMModel.CLAUDE_SONNET_4_5)
    model_with_tools = model.bind_tools(tools)
    
    # Initialize messages if empty
    messages = state["messages"]
    if not messages:
        # Construct initial context (same as before)
        files = state.get("generated_files", [])
        build_failed = state.get("build_failed_in_phase_1", False)
        
        lang_instructions = (
                DOTNET_TESTING_INSTRUCTIONS if target_language == "dotnet" else JAVA_TESTING_INSTRUCTIONS
        )
        files_to_test = [
                f for f in files
                if ("/Services/" in f or "/Repositories/" in f or "/services/" in f.lower() or "/repositories/" in f.lower()) 
                and not ("/Jobs/" in f or "/jobs/" in f.lower())
        ]
        
        system_msg = SystemMessage(content=f"{TESTING_AGENT_PERSONA}\n\n{lang_instructions}")
        task_msg = HumanMessage(content=f"""
Phase 1 (Implementation) is complete. 
Build Status: {'FAILED' if build_failed else 'SUCCESS'}.

Files to Test ({len(files_to_test)} candidate files):
{chr(10).join(files_to_test[:50])} {'... (truncated)' if len(files_to_test) > 50 else ''}

Your Task:
1. For each file, read the source logic.
2. Write a semantic Unit Test file.
3. {'SKIP execution (Build Failed). Just write the tests.' if build_failed else 'RUN the tests using the build tools.'}
4. If tests fail, FIX the Service/Repository code.

Begin by reading the files.
        """)
        messages = [system_msg, task_msg]
        
        response = model_with_tools.invoke(messages)
        return {"messages": messages + [response], "iteration_count": 1}
    
    # Normal loop
    response = model_with_tools.invoke(messages)
    return {"messages": [response], "iteration_count": state["iteration_count"] + 1}


def tool_node_dynamic(state: TestingState):
    """Dynamic tool node."""
    project_id = state["project_id"]
    target_language = state["target_language"]
    output_path = state["output_path"]
    source_path = state["source_path"]
    
    # Re-create tools
    solution_tools = create_solution_tools(project_id, output_path, source_path, target_language)
    build_tools = create_build_tools(output_path, target_language, project_id, include_test_tools=True)
    source_tools = create_source_file_tools(project_id, source_path)
    tools = solution_tools + build_tools + source_tools
    tool_map = {t.name: t for t in tools}
    
    last_message = state["messages"][-1]
    results = []
    
    if hasattr(last_message, "tool_calls"):
        for tool_call in last_message.tool_calls:
            tool = tool_map.get(tool_call["name"])
            if tool:
                try:
                    logger.info(f"[TestingAgent] Invoking {tool_call['name']}")
                    output = tool.invoke(tool_call["args"])
                except Exception as e:
                    output = f"Error: {e}"
            else:
                output = f"Error: Tool {tool_call['name']} not found."
            
            results.append(ToolMessage(content=str(output), tool_call_id=tool_call["id"]))
    
    return {"messages": results}


def create_testing_graph(llm_client_unused) -> StateGraph:
    workflow = StateGraph(TestingState)
    
    workflow.add_node("agent", model_node_dynamic)
    workflow.add_node("tools", tool_node_dynamic)
    
    workflow.add_edge(START, "agent")
    
    workflow.add_conditional_edges(
        "agent",
        should_continue,
        {
            "tools": "tools",
            "__end__": END
        }
    )
    
    workflow.add_edge("tools", "agent")
    
    return workflow.compile()
