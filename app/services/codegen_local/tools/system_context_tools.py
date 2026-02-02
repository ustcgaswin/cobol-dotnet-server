"""System context tools for the code generation agent.

Provides access to Analyst agent outputs (functionality catalog, job chains, data flows).
"""

from pathlib import Path

from langchain.tools import tool
from loguru import logger

from app.config.settings import settings


def _get_system_context_path(project_id: str) -> Path:
    """Get the system_context directory for a project."""
    base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
    return base / project_id / "system_context"


def create_system_context_tools(project_id: str) -> list:
    """Create system context tools for accessing Analyst outputs.
    
    Args:
        project_id: Project ID for scoping file operations
        
    Returns:
        List of LangChain tools
    """
    context_path = _get_system_context_path(project_id)
    
    @tool("read_functionality_catalog")
    def read_functionality_catalog() -> str:
        """Read the functionality catalog from the Analyst phase.
        
        Contains business functionalities (F001, F002, etc.) that must be converted.
        Use this as a checklist to ensure all functionalities are implemented.
        
        Returns:
            The functionality catalog content, or error if not found
        """
        try:
            filepath = context_path / "functionality_catalog.md"
            if not filepath.exists():
                return "Error: functionality_catalog.md not found. Analyst phase may not have run."
            
            content = filepath.read_text(encoding="utf-8", errors="replace")
            return f"=== Functionality Catalog ===\n\n{content}"
            
        except Exception as e:
            logger.error(f"read_functionality_catalog error: {e}")
            return f"Error reading functionality catalog: {e}"
    
    @tool("read_job_chains")
    def read_job_chains() -> str:
        """Read job chain documentation from the Analyst phase.
        
        Contains job scheduling information, step sequences, and orchestration context.
        Useful for understanding how JCL jobs are scheduled and sequenced.
        
        Returns:
            The job chains content, or message if not found
        """
        try:
            filepath = context_path / "job_chains.md"
            if not filepath.exists():
                return "No job chains documented. JCL may need to be analyzed from source directly."
            
            content = filepath.read_text(encoding="utf-8", errors="replace")
            return f"=== Job Chains ===\n\n{content}"
            
        except Exception as e:
            logger.error(f"read_job_chains error: {e}")
            return f"Error reading job chains: {e}"
    
    @tool("read_data_flows")
    def read_data_flows() -> str:
        """Read data flow documentation from the Analyst phase.
        
        Contains information about data movement between systems, files, and databases.
        Useful for designing repositories and understanding I/O patterns.
        
        Returns:
            The data flows content, or message if not found
        """
        try:
            filepath = context_path / "data_flows.md"
            if not filepath.exists():
                return "No data flows documented. Analyze source code for I/O patterns."
            
            content = filepath.read_text(encoding="utf-8", errors="replace")
            return f"=== Data Flows ===\n\n{content}"
            
        except Exception as e:
            logger.error(f"read_data_flows error: {e}")
            return f"Error reading data flows: {e}"
    
    return [
        read_functionality_catalog,
        read_job_chains,
        read_data_flows,
    ]
