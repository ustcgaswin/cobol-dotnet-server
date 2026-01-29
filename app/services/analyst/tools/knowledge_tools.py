"""Knowledge access tools for the System Analyst Agent.

Provides access to static knowledge files like process_flow.md
when automated parsing (e.g., CA-7) is not available.
"""

from pathlib import Path

from langchain.tools import tool
from loguru import logger


# Knowledge folder is at project root
KNOWLEDGE_PATH = Path(__file__).parents[4] / "knowledge"


def create_knowledge_tools() -> list:
    """Factory to create knowledge access tools.
    
    Returns:
        List of LangChain tools for knowledge access
    """
    
    @tool("read_process_flow")
    def read_process_flow() -> str:
        """Read the process flow documentation from the knowledge folder.
        
        Use this tool when CA-7 scheduling data is not available in
        dependency_graph.md. The process flow document contains manual
        documentation of job chains, process dependencies, and scheduling
        information.
        
        Returns:
            The process flow markdown content, or a message if not available
        """
        try:
            process_flow_path = KNOWLEDGE_PATH / "process_flow.md"
            
            if not process_flow_path.exists():
                logger.info("No process_flow.md found in knowledge folder")
                return "No process flow documentation available. Rely on dependency_graph.md for job chain information."
            
            content = process_flow_path.read_text(encoding="utf-8")
            
            if not content.strip():
                return "Process flow document is empty. Rely on dependency_graph.md for job chain information."
            
            logger.info(f"Read process_flow.md ({len(content)} chars)")
            return content
            
        except Exception as e:
            logger.error(f"read_process_flow error: {e}")
            return f"Error reading process flow: {e}"
    
    return [read_process_flow]
