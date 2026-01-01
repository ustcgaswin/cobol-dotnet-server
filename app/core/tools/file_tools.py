"""File access tools for agent use.

All file operations are scoped to project_storage/{project_id}/.
"""

import subprocess
from pathlib import Path

from langchain.tools import tool, ToolRuntime
from loguru import logger

from app.config.settings import settings
from app.core.tools.context import ProjectContext


def _get_project_path(project_id: str) -> Path:
    """Get the absolute path to a project's storage directory."""
    base = Path(settings.PROJECT_STORAGE_PATH).resolve()
    return base / project_id


def _validate_path(project_id: str, relative_path: str) -> Path:
    """Validate and resolve a path within project scope.
    
    Raises:
        ValueError: If path escapes project directory
    """
    project_path = _get_project_path(project_id)
    
    # Resolve the full path
    target = (project_path / relative_path).resolve()
    
    # Ensure it's within the project directory
    if not str(target).startswith(str(project_path)):
        raise ValueError(f"Path '{relative_path}' is outside project scope")
    
    return target


@tool("view_file")
def view_file(
    filepath: str,
    start_line: int,
    end_line: int,
    runtime: ToolRuntime[ProjectContext],
) -> str:
    """View specific lines from a project file.
    
    Use this to read portions of source code files (COBOL, copybooks, JCL, etc.)
    within the current project. Line numbers are 1-indexed.
    
    Args:
        filepath: Relative path to the file within the project (e.g., "cobol/MAIN.cbl")
        start_line: First line to read (1-indexed, inclusive)
        end_line: Last line to read (1-indexed, inclusive)
        
    Returns:
        The requested lines with line numbers, or an error message
    """
    try:
        project_id = runtime.context.project_id
        target = _validate_path(project_id, filepath)
        
        if not target.exists():
            return f"Error: File '{filepath}' not found"
        
        if not target.is_file():
            return f"Error: '{filepath}' is not a file"
        
        lines = target.read_text(encoding="utf-8", errors="replace").splitlines()
        total_lines = len(lines)
        
        # Validate line numbers
        if start_line < 1:
            start_line = 1
        if end_line > total_lines:
            end_line = total_lines
        if start_line > end_line:
            return f"Error: Invalid line range ({start_line}-{end_line})"
        
        # Extract lines (convert to 0-indexed)
        selected = lines[start_line - 1:end_line]
        
        # Format with line numbers
        result_lines = [
            f"{i}: {line}" 
            for i, line in enumerate(selected, start=start_line)
        ]
        
        return f"File: {filepath} (lines {start_line}-{end_line} of {total_lines})\n" + "\n".join(result_lines)
        
    except ValueError as e:
        return f"Error: {e}"
    except Exception as e:
        logger.error(f"view_file error: {e}")
        return f"Error reading file: {e}"


@tool("grep_search")
def grep_search(
    pattern: str,
    file_pattern: str = "*",
    runtime: ToolRuntime[ProjectContext] = None,
) -> str:
    """Search for a pattern in project files using grep.
    
    Use this to find occurrences of text patterns (variable names, procedure names,
    CALL statements, etc.) across project files.
    
    Args:
        pattern: The text pattern to search for (supports regex)
        file_pattern: Optional glob pattern to filter files (e.g., "*.cbl", "*.cpy")
        
    Returns:
        Matching lines with file names and line numbers, or an error message
    """
    try:
        project_id = runtime.context.project_id
        project_path = _get_project_path(project_id)
        
        if not project_path.exists():
            return f"Error: Project directory not found"
        
        # Build grep command
        # Using findstr on Windows, grep on Unix
        import platform
        
        if platform.system() == "Windows":
            # Use findstr on Windows
            cmd = ["findstr", "/s", "/n", "/i", pattern]
            if file_pattern != "*":
                cmd.append(file_pattern)
            else:
                cmd.append("*.*")
        else:
            # Use grep on Unix
            cmd = ["grep", "-rn", "--include", file_pattern, pattern, "."]
        
        result = subprocess.run(
            cmd,
            cwd=str(project_path),
            capture_output=True,
            text=True,
            timeout=30,
        )
        
        output = result.stdout.strip()
        if not output:
            return f"No matches found for pattern '{pattern}'"
        
        # Limit output length
        lines = output.split("\n")
        if len(lines) > 100:
            output = "\n".join(lines[:100])
            output += f"\n... (truncated, {len(lines) - 100} more matches)"
        
        return f"Search results for '{pattern}':\n{output}"
        
    except subprocess.TimeoutExpired:
        return "Error: Search timed out"
    except Exception as e:
        logger.error(f"grep_search error: {e}")
        return f"Error searching files: {e}"


@tool("list_files")
def list_files(
    path: str = "",
    extension: str | None = None,
    runtime: ToolRuntime[ProjectContext] = None,
) -> str:
    """List files in a project directory.
    
    Use this to explore the project structure and find available source files.
    
    Args:
        path: Relative path within project (empty string for root)
        extension: Optional file extension filter (e.g., ".cbl", ".cpy")
        
    Returns:
        List of files and directories, or an error message
    """
    try:
        project_id = runtime.context.project_id
        target = _validate_path(project_id, path) if path else _get_project_path(project_id)
        
        if not target.exists():
            return f"Error: Path '{path}' not found"
        
        if not target.is_dir():
            return f"Error: '{path}' is not a directory"
        
        items = []
        for item in sorted(target.iterdir()):
            if item.is_dir():
                items.append(f"[DIR]  {item.name}/")
            else:
                if extension is None or item.suffix.lower() == extension.lower():
                    size = item.stat().st_size
                    items.append(f"[FILE] {item.name} ({size} bytes)")
        
        if not items:
            return f"Directory '{path or '/'}' is empty"
        
        header = f"Contents of '{path or '/'}'"
        if extension:
            header += f" (filtered: {extension})"
        
        return header + ":\n" + "\n".join(items)
        
    except ValueError as e:
        return f"Error: {e}"
    except Exception as e:
        logger.error(f"list_files error: {e}")
        return f"Error listing files: {e}"
