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
            return "Error: Project directory not found"
        
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
    file_type: str | None = None,
    runtime: ToolRuntime[ProjectContext] = None,
) -> str:
    """List source files for the current project from the database.
    
    Use this to discover available source files and their types.
    The file_type is the official type assigned during upload (e.g., cobol, copybook, jcl).
    
    Args:
        file_type: Optional filter by file type (e.g., "cobol", "copybook", "jcl", "assembly")
        
    Returns:
        List of source files with their types and sizes
    """
    import asyncio
    from sqlalchemy import select
    from app.db.base import async_session_factory
    from app.db.models.source_file import SourceFile
    
    async def _query_files():
        async with async_session_factory() as session:
            query = select(SourceFile).where(
                SourceFile.project_id == project_id
            )
            if file_type:
                query = query.where(SourceFile.file_type == file_type)
            
            result = await session.execute(query)
            return result.scalars().all()
    
    try:
        project_id = runtime.context.project_id
        
        # Run async query in sync context
        try:
            loop = asyncio.get_event_loop()
            if loop.is_running():
                # We're in an async context, create a new task
                import concurrent.futures
                with concurrent.futures.ThreadPoolExecutor() as pool:
                    files = pool.submit(asyncio.run, _query_files()).result()
            else:
                files = loop.run_until_complete(_query_files())
        except RuntimeError:
            files = asyncio.run(_query_files())
        
        if not files:
            msg = f"No source files found"
            if file_type:
                msg += f" with type '{file_type}'"
            return msg
        
        items = []
        for f in sorted(files, key=lambda x: (x.file_type, x.filename)):
            items.append(f"[{f.file_type.upper()}] {f.filename} ({f.size_bytes} bytes)")
        
        header = "Source files"
        if file_type:
            header += f" (type: {file_type})"
        
        return header + f" ({len(files)} total):\n" + "\n".join(items)
        
    except Exception as e:
        logger.error(f"list_files error: {e}")
        return f"Error listing files: {e}"

