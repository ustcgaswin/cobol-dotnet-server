"""Artifact access tools for agent use.

All artifact operations are scoped to project_artifacts/{project_id}/.
These tools provide access to generated analysis outputs like
dependency_graph.md and file_summaries.md.

Tools are created via factory functions that bind project_id.
"""

from pathlib import Path

from langchain.tools import tool
from loguru import logger

from app.config.settings import settings


def _get_artifacts_path(project_id: str) -> Path:
    """Get the absolute path to a project's artifacts directory."""
    base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
    return base / project_id


def _validate_artifact_path(project_id: str, filename: str) -> Path:
    """Validate and resolve an artifact path within project scope."""
    artifacts_path = _get_artifacts_path(project_id)
    target = (artifacts_path / filename).resolve()
    
    if not str(target).startswith(str(artifacts_path)):
        raise ValueError(f"Path '{filename}' is outside artifacts scope")
    
    return target


def create_artifact_tools(project_id: str) -> list:
    """Factory to create artifact tools bound to a specific project.
    
    Args:
        project_id: Project ID to scope all operations to
        
    Returns:
        List of LangChain tools
    """
    
    @tool("list_artifacts")
    def list_artifacts() -> str:
        """List all artifact files for the current project.
        
        Use this to discover what analysis outputs are available, such as
        dependency_graph.md, file_summaries.md, etc.
        
        Returns:
            List of artifact files with sizes
        """
        try:
            artifacts_path = _get_artifacts_path(project_id)
            
            if not artifacts_path.exists():
                return "Error: No artifacts directory found for this project"
            
            items = []
            for item in sorted(artifacts_path.rglob("*")):
                if item.is_file():
                    rel_path = item.relative_to(artifacts_path)
                    size = item.stat().st_size
                    items.append(f"[FILE] {rel_path} ({size} bytes)")
            
            if not items:
                return "Artifacts directory is empty"
            
            return f"Artifacts for project:\n" + "\n".join(items)
            
        except Exception as e:
            logger.error(f"list_artifacts error: {e}")
            return f"Error listing artifacts: {e}"
    
    @tool("read_artifact")
    def read_artifact(filename: str, start_line: int, end_line: int) -> str:
        """Read specific lines from a project artifact file.
        
        Use this to read portions of analysis outputs like dependency_graph.md
        or file_summaries.md. Line numbers are 1-indexed.
        
        Args:
            filename: Relative path to the artifact (e.g., "dependency_graph.md")
            start_line: First line to read (1-indexed, inclusive)
            end_line: Last line to read (1-indexed, inclusive)
            
        Returns:
            The requested lines with line numbers, or an error message
        """
        try:
            target = _validate_artifact_path(project_id, filename)
            
            if not target.exists():
                return f"Error: Artifact '{filename}' not found"
            
            if not target.is_file():
                return f"Error: '{filename}' is not a file"
            
            lines = target.read_text(encoding="utf-8", errors="replace").splitlines()
            total_lines = len(lines)
            
            if start_line < 1:
                start_line = 1
            if end_line > total_lines:
                end_line = total_lines
            if start_line > end_line:
                return f"Error: Invalid line range ({start_line}-{end_line})"
            
            selected = lines[start_line - 1:end_line]
            result_lines = [
                f"{i}: {line}" 
                for i, line in enumerate(selected, start=start_line)
            ]
            
            return f"Artifact: {filename} (lines {start_line}-{end_line} of {total_lines})\n" + "\n".join(result_lines)
            
        except ValueError as e:
            return f"Error: {e}"
        except Exception as e:
            logger.error(f"read_artifact error: {e}")
            return f"Error reading artifact: {e}"
    
    @tool("grep_artifact")
    def grep_artifact(pattern: str, filename: str) -> str:
        """Search for a pattern within a specific artifact file.
        
        Use this to find occurrences of text patterns (program names, 
        section headers, etc.) within analysis outputs.
        
        Args:
            pattern: The text pattern to search for (case-insensitive)
            filename: The artifact file to search in (e.g., "file_summaries.md")
            
        Returns:
            Matching lines with line numbers, or an error message
        """
        try:
            target = _validate_artifact_path(project_id, filename)
            
            if not target.exists():
                return f"Error: Artifact '{filename}' not found"
            
            if not target.is_file():
                return f"Error: '{filename}' is not a file"
            
            lines = target.read_text(encoding="utf-8", errors="replace").splitlines()
            
            matches = []
            pattern_lower = pattern.lower()
            for i, line in enumerate(lines, start=1):
                if pattern_lower in line.lower():
                    matches.append(f"{i}: {line}")
            
            if not matches:
                return f"No matches found for '{pattern}' in {filename}"
            
            if len(matches) > 50:
                output = "\n".join(matches[:50])
                output += f"\n... (truncated, {len(matches) - 50} more matches)"
            else:
                output = "\n".join(matches)
            
            return f"Search results for '{pattern}' in {filename}:\n{output}"
            
        except ValueError as e:
            return f"Error: {e}"
        except Exception as e:
            logger.error(f"grep_artifact error: {e}")
            return f"Error searching artifact: {e}"
    
    return [list_artifacts, read_artifact, grep_artifact]
