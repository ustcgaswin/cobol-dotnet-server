# from langgraph.graph import StateGraph, END
# from langgraph.prebuilt import ToolNode
# from ...api.schemas.doc_models import DocAgentState
# from .nodes import DocAgentNodes

# def create_documentation_graph(llm, analyzer, tools_dict):
#     nodes = DocAgentNodes(llm, analyzer, tools_dict)
#     workflow = StateGraph(DocAgentState)

#     workflow.add_node("researcher", nodes.researcher_node)
#     workflow.add_node("call_tools", ToolNode(list(tools_dict.values())))
#     workflow.add_node("generate_functional", nodes.functional_gen_node)
#     workflow.add_node("generate_technical", nodes.technical_gen_node)
#     workflow.add_node("render_docx", nodes.render_node)

#     workflow.set_entry_point("researcher")

#     def should_continue(state: DocAgentState):
#         if state.get("research_complete"):
#             return "generate_technical"
            
#         last_message = state["messages"][-1]
#         if hasattr(last_message, 'tool_calls') and last_message.tool_calls:
#             return "call_tools"
            
#         return "generate_technical"

#     workflow.add_conditional_edges(
#         "researcher",
#         should_continue,
#         {
#             "call_tools": "call_tools",
#             "generate_technical": "generate_technical"
#         }
#     )

#     workflow.add_edge("call_tools", "researcher")
#     workflow.add_edge("generate_technical", "generate_functional")
#     workflow.add_edge("generate_functional", "render_docx")
#     workflow.add_edge("render_docx", END)

#     return workflow.compile()
from typing import Literal
from langgraph.graph import StateGraph, END
from langgraph.prebuilt import ToolNode
from app.api.schemas.doc_models import DocAgentState
from .nodes import DocAgentNodes

def create_documentation_graph(llm, analyzer, tools_dict):
    """
    Constructs the State Graph for the Agent.
    Orchestrates: Research Loop -> Conditional Generation -> Rendering.
    """
    nodes = DocAgentNodes(llm, analyzer, tools_dict)
    workflow = StateGraph(DocAgentState)

    # 1. Register Nodes
    # 'researcher' prepares tool calls or decides to stop
    workflow.add_node("researcher", nodes.researcher_node)
    
    # 'call_tools' actually executes the Python functions (view_file, etc.)
    workflow.add_node("call_tools", ToolNode(list(tools_dict.values())))
    
    # Generation nodes
    workflow.add_node("generate_technical", nodes.technical_gen_node)
    workflow.add_node("generate_functional", nodes.functional_gen_node)
    
    # Rendering node (saves the files)
    workflow.add_node("render_docx", nodes.render_node)

    # 2. Set Entry Point
    workflow.set_entry_point("researcher")

    # 3. Define Logic: Researcher -> (Tools OR Generation)
    def route_research(state: DocAgentState):
        """Decides if we continue researching or start generating code."""
        
        # A. Check if the LLM wants to call a tool
        last_message = state["messages"][-1]
        if hasattr(last_message, 'tool_calls') and last_message.tool_calls:
            return "call_tools"
        
        # B. Check if research is explicitly marked complete (by limit or LLM)
        # Note: Your nodes.py sets "research_complete" when max iterations hit
        if state.get("research_complete"):
            mode = state.get("generation_mode", "ALL")
            
            if mode == "FUNCTIONAL":
                return "generate_functional"
            else:
                # "ALL" or "TECHNICAL" usually start with Technical analysis
                return "generate_technical"
        
        # C. Default Fallback (If LLM just talks without tools, force generation)
        # This prevents infinite loops of conversation
        return "generate_technical"

    workflow.add_conditional_edges(
        "researcher",
        route_research,
        {
            "call_tools": "call_tools",
            "generate_technical": "generate_technical",
            "generate_functional": "generate_functional"
        }
    )

    # 4. Tool Output -> Back to Researcher
    workflow.add_edge("call_tools", "researcher")

    # 5. Define Logic: Technical -> (Functional OR Render)
    def route_after_technical(state: DocAgentState):
        """After Technical is done, do we do Functional or finish?"""
        mode = state.get("generation_mode", "ALL")
        
        if mode == "ALL":
            return "generate_functional"
        elif mode == "TECHNICAL":
            return "render_docx"
        
        # Should not happen if logic above is correct, but safe fallback
        return "render_docx"

    workflow.add_conditional_edges(
        "generate_technical",
        route_after_technical,
        {
            "generate_functional": "generate_functional",
            "render_docx": "render_docx"
        }
    )

    # 6. Define Logic: Functional -> Render
    # Functional is the last step in the chain for "ALL" or "FUNCTIONAL" modes
    workflow.add_edge("generate_functional", "render_docx")

    # 7. Render -> End
    workflow.add_edge("render_docx", END)

    return workflow.compile()