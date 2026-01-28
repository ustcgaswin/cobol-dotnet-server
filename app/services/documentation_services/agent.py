from langgraph.graph import StateGraph, END
from langgraph.prebuilt import ToolNode
from .models import DocAgentState
from .nodes import DocAgentNodes

def create_documentation_graph(llm, analyzer, tools_dict):
    nodes = DocAgentNodes(llm, analyzer, tools_dict)
    workflow = StateGraph(DocAgentState)

    workflow.add_node("researcher", nodes.researcher_node)
    workflow.add_node("call_tools", ToolNode(list(tools_dict.values())))
    workflow.add_node("generate_functional", nodes.functional_gen_node)
    workflow.add_node("generate_technical", nodes.technical_gen_node)
    workflow.add_node("render_docx", nodes.render_node)

    workflow.set_entry_point("researcher")

    def should_continue(state: DocAgentState):
        if state.get("research_complete"):
            return "generate_technical"
            
        last_message = state["messages"][-1]
        if hasattr(last_message, 'tool_calls') and last_message.tool_calls:
            return "call_tools"
            
        return "generate_technical"

    workflow.add_conditional_edges(
        "researcher",
        should_continue,
        {
            "call_tools": "call_tools",
            "generate_technical": "generate_technical"
        }
    )

    workflow.add_edge("call_tools", "researcher")
    workflow.add_edge("generate_technical", "generate_functional")
    workflow.add_edge("generate_functional", "render_docx")
    workflow.add_edge("render_docx", END)

    return workflow.compile()