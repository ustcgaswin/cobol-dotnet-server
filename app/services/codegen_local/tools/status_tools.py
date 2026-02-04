"""Status tracking tools for the code generation agent.

Logs conversion progress, issues, and unknown utilities.
All status files go to code-migration/ folder with -local suffix.
"""

from datetime import datetime
from pathlib import Path

from langchain.tools import tool
from loguru import logger

from app.config.settings import settings


def _get_codegen_path(project_id: str) -> Path:
    """Get the code-migration directory for status files."""
    base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
    return base / project_id / "code-migration"


def create_status_tools(project_id: str) -> list:
    """Create status tracking tools.
    
    Args:
        project_id: Project ID for scoping status files
        
    Returns:
        List of LangChain tools
    """
    codegen_path = _get_codegen_path(project_id)
    
    @tool("log_component_status")
    def log_component_status(
        component_name: str,
        status: str,
        notes: str = "",
    ) -> str:
        """Log the conversion status of a component.
        
        Call this after converting each component to track progress.
        
        Args:
            component_name: Name of the component (e.g., "PAYROLL.cbl")
            status: One of: converted, skipped, failed, in_progress
            notes: Optional notes about the conversion
            
        Returns:
            Confirmation message
        """
        try:
            codegen_path.mkdir(parents=True, exist_ok=True)
            status_file = codegen_path / "conversion_status_local.md"
            
            # Create header if file doesn't exist
            if not status_file.exists():
                header = """# Conversion Status (Local)

| Component | Status | Notes | Timestamp |
|-----------|--------|-------|-----------|
"""
                status_file.write_text(header)
            
            # Append row
            timestamp = datetime.utcnow().strftime("%Y-%m-%d %H:%M")
            row = f"| {component_name} | {status} | {notes} | {timestamp} |\n"
            
            with open(status_file, "a") as f:
                f.write(row)
            
            logger.info(f"Status logged: {component_name} = {status}")
            return f"Logged status: {component_name} = {status}"
            
        except Exception as e:
            logger.error(f"log_component_status error: {e}")
            return f"Error logging status: {e}"
    
    @tool("log_issue")
    def log_issue(
        component_name: str,
        issue_type: str,
        description: str,
    ) -> str:
        """Log an issue encountered during conversion.
        
        Use this for errors, warnings, or things that need human review.
        
        Args:
            component_name: Component where issue occurred
            issue_type: Category (build_error, unknown_construct, needs_review, other)
            description: Detailed description of the issue
            
        Returns:
            Confirmation message
        """
        try:
            codegen_path.mkdir(parents=True, exist_ok=True)
            issues_file = codegen_path / "issues_local.md"
            
            if not issues_file.exists():
                header = """# Conversion Issues (Local)

Issues encountered during code generation that need attention.

---

"""
                issues_file.write_text(header)
            
            timestamp = datetime.utcnow().strftime("%Y-%m-%d %H:%M")
            entry = f"""## {component_name}
**Type**: {issue_type}
**Time**: {timestamp}
**Description**: {description}

---

"""
            with open(issues_file, "a") as f:
                f.write(entry)
            
            logger.info(f"Issue logged: {component_name} - {issue_type}")
            return f"Logged issue for {component_name}: {issue_type}"
            
        except Exception as e:
            logger.error(f"log_issue error: {e}")
            return f"Error logging issue: {e}"
    
    @tool("log_unknown_utility")
    def log_unknown_utility(
        utility_name: str,
        found_in: str,
        context: str,
    ) -> str:
        """Log an unknown utility for later research.
        
        Use this when lookup_utility() returns "not found".
        
        Args:
            utility_name: Name of the unknown utility
            found_in: File where it was found
            context: Surrounding code or JCL for context
            
        Returns:
            Confirmation message
        """
        try:
            codegen_path.mkdir(parents=True, exist_ok=True)
            utils_file = codegen_path / "unknown_utilities_local.md"
            
            if not utils_file.exists():
                header = """# Unknown Utilities (Local)

Utilities encountered that need research to find .NET equivalents.

---

"""
                utils_file.write_text(header)
            
            timestamp = datetime.utcnow().strftime("%Y-%m-%d %H:%M")
            entry = f"""## {utility_name}
**Found In**: {found_in}
**Time**: {timestamp}
**Context**:
```
{context}
```

---

"""
            with open(utils_file, "a") as f:
                f.write(entry)
            
            logger.info(f"Unknown utility logged: {utility_name}")
            return f"Logged unknown utility: {utility_name}"
            
        except Exception as e:
            logger.error(f"log_unknown_utility error: {e}")
            return f"Error logging utility: {e}"
    
    @tool("read_conversion_status")
    def read_conversion_status() -> str:
        """Read the current conversion status.
        
        Use this to check what's already been converted (for resuming).
        
        Returns:
            Contents of conversion_status_local.md or "no status yet"
        """
        try:
            status_file = codegen_path / "conversion_status_local.md"
            
            if not status_file.exists():
                return "No conversion status yet. This is a fresh start."
            
            content = status_file.read_text()
            return f"Current conversion status:\n\n{content}"
            
        except Exception as e:
            logger.error(f"read_conversion_status error: {e}")
            return f"Error reading status: {e}"
    
    @tool("search_session_logs")
    def search_session_logs(query: str) -> str:
        """Search session logs for specific information.
        
        Use this if you need details about a component that was summarized away
        from the conversation history.
        
        Args:
            query: Text to search for (component name, error message, etc.)
            
        Returns:
            Matching snippets from session logs or "No matches found"
        """
        import json
        
        try:
            logs_path = Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / project_id / "codegen_logs"
            
            if not logs_path.exists():
                return "No session logs found."
            
            matches = []
            for log_file in sorted(logs_path.glob("session_*.json")):
                try:
                    with open(log_file, 'r', encoding='utf-8') as f:
                        data = json.load(f)
                    
                    for i, msg in enumerate(data.get("messages", [])):
                        content = msg.get("content", "")
                        if query.lower() in content.lower():
                            # Truncate long content
                            snippet = content[:500]
                            if len(content) > 500:
                                snippet += "..."
                            
                            matches.append({
                                "file": log_file.name,
                                "index": i,
                                "role": msg.get("role", "unknown"),
                                "snippet": snippet
                            })
                            
                            if len(matches) >= 3:
                                break
                except Exception as e:
                    logger.warning(f"Failed to read {log_file}: {e}")
                
                if len(matches) >= 3:
                    break
            
            if not matches:
                return f"No matches found for '{query}' in session logs."
            
            result = f"Found {len(matches)} match(es) for '{query}':\n\n"
            for m in matches:
                result += f"**{m['file']}** (index {m['index']}, {m['role']}):\n{m['snippet']}\n\n---\n\n"
            
            return result
            
        except Exception as e:
            logger.error(f"search_session_logs error: {e}")
            return f"Error searching session logs: {e}"
    
    return [
        log_component_status,
        log_issue,
        log_unknown_utility,
        read_conversion_status,
        search_session_logs,
    ]
