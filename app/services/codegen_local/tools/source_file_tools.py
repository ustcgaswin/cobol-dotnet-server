"""Source file access tools for the code generation agent.

Wraps the core file_tools with project_id binding to avoid ToolRuntime context issues.
"""

import subprocess
from pathlib import Path

from langchain.tools import tool
from loguru import logger

from app.config.settings import settings


def _get_project_path(project_id: str) -> Path:
    """Get the absolute path to a project's storage directory."""
    base = Path(settings.PROJECT_STORAGE_PATH).resolve()
    return base / project_id



def create_source_file_tools(project_id: str) -> list:
    """Create source file access tools bound to a specific project.
    
    Args:
        project_id: Project ID for scoping file operations
        
    Returns:
        List of LangChain tools
    """
    
    @tool("view_source_file")
    def view_source_file(
        filename: str,
        start_line: int = 1,
        end_line: int = 100,
    ) -> str:
        """View a source file by filename (searches the entire project).
        
        Provide just the filename — the tool will find it automatically.
        Supports COBOL, PL/I, Assembly, REXX, JCL, copybooks, DCLGEN, parmlib, and all other source files.
        Line numbers are 1-indexed.
        
        Args:
            filename: Just the filename (e.g., "FSMAIN.cbl", "FSFEE.pli", "SETLJOB.jcl")
            start_line: First line to read (1-indexed, default 1)
            end_line: Last line to read (1-indexed, default 100)
            
        Returns:
            The requested lines with line numbers
        """
        try:
            project_path = _get_project_path(project_id)
            
            if not project_path.exists():
                return f"Error: Project directory not found"
            
            # Search for the file anywhere in the project (exact match first)
            matches = list(project_path.rglob(filename))
            
            # If no exact match, try case-insensitive via indexed scan
            if not matches:
                target_lower = filename.lower()
                matches = [
                    f for f in project_path.rglob("*.*")
                    if f.is_file() and f.name.lower() == target_lower
                ]
            
            if not matches:
                return f"Error: File '{filename}' not found in project. Use list_source_files() to see available files."
            
            if len(matches) > 1:
                listing = "\n".join(
                    str(m.relative_to(project_path)) for m in matches
                )
                return f"Multiple files named '{filename}' found. Specify the full relative path:\n{listing}"
            
            target = matches[0]
            rel_path = target.relative_to(project_path)
            
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
            
            return f"File: {rel_path} (lines {start_line}-{end_line} of {total_lines})\n" + "\n".join(result_lines)
            
        except ValueError as e:
            return f"Error: {e}"
        except Exception as e:
            logger.error(f"view_source_file error: {e}")
            return f"Error reading file: {e}"
    
    @tool("grep_source")
    def grep_source(
        pattern: str,
        file_pattern: str = "*",
    ) -> str:
        """Search for a pattern in source files.
        
        Use this to find occurrences of text patterns (variable names, procedure names,
        CALL statements, etc.) across project source files.
        
        Args:
            pattern: Text pattern to search for
            file_pattern: Glob pattern to filter files (e.g., "*.cbl", "*.cpy")
            
        Returns:
            Matching lines with file names and line numbers
        """
        try:
            project_path = _get_project_path(project_id)
            
            if not project_path.exists():
                return "Error: Project directory not found"
            
            import platform
            
            if platform.system() == "Windows":
                cmd = ["findstr", "/s", "/n", "/i", pattern]
                if file_pattern != "*":
                    cmd.append(file_pattern)
                else:
                    cmd.append("*.*")
            else:
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
            
            lines = output.split("\n")
            if len(lines) > 100:
                output = "\n".join(lines[:100])
                output += f"\n... (truncated, {len(lines) - 100} more matches)"
            
            return f"Search results for '{pattern}':\n{output}"
            
        except subprocess.TimeoutExpired:
            return "Error: Search timed out"
        except Exception as e:
            logger.error(f"grep_source error: {e}")
            return f"Error searching files: {e}"
    
    @tool("list_source_files")
    def list_source_files(file_type: str = None) -> str:
        """List source files for the current project.
        
        Use this to discover available source files and their types.
        Scans the project_storage directory for files organized by type.
        
        Args:
            file_type: Optional filter by type folder (cobol, copybook, jcl, assembly, etc.)
            
        Returns:
            List of source files with types and sizes
        """
        try:
            project_path = _get_project_path(project_id)
            
            if not project_path.exists():
                return "Error: Project directory not found"
            
            items = []
            
            # Scan subdirectories as file types
            for type_dir in sorted(project_path.iterdir()):
                if not type_dir.is_dir():
                    continue
                
                type_name = type_dir.name
                
                # Filter by file_type if specified
                if file_type and type_name.lower() != file_type.lower():
                    continue
                
                # List files in this type folder
                for file_path in sorted(type_dir.rglob("*")):
                    if file_path.is_file():
                        rel_path = file_path.relative_to(type_dir)
                        size = file_path.stat().st_size
                        items.append(f"[{type_name.upper()}] {rel_path} ({size} bytes)")
            
            if not items:
                msg = "No source files found"
                if file_type:
                    msg += f" with type '{file_type}'"
                return msg
            
            header = "Source files"
            if file_type:
                header += f" (type: {file_type})"
            
            return header + f" ({len(items)} total):\n" + "\n".join(items)
            
        except Exception as e:
            logger.error(f"list_source_files error: {e}")
            return f"Error listing files: {e}"
    
    return [view_source_file, grep_source, list_source_files]
