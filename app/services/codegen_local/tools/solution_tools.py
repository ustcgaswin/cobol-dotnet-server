"""Solution management tools for the code generation agent.

Handles .NET solution initialization and file writing.
"""

import re
import uuid
from pathlib import Path

from langchain.tools import tool
from loguru import logger

from app.services.codegen_local.tools.scaffold import scaffold_solution
from app.services.codegen_local.tools.source_file_tools import get_source_read_registry


def create_solution_tools(project_id: str, output_path: str, source_path: str) -> list:
    """Create solution management tools.
    
    Args:
        project_id: Project ID for scoping file operations
        output_path: Absolute path to the output directory for generated code
        source_path: Absolute path to the source directory (for listing components)
        
    Returns:
        List of LangChain tools
    """
    output_dir = Path(output_path).resolve()
    source_dir = Path(source_path).resolve()
    
    @tool("list_batch_components")
    def list_batch_components() -> str:
        """List all JCL Jobs and Procedures that need to be converted.

        Scans the source directory for .jcl (Jobs) and .proc/.prc (Procedures).

        Returns:
            Formatted list of batch components found in source.
        """
        try:
            if not source_dir.exists():
                return f"Error: Source directory not found at {source_dir}"

            jobs = []
            procs = []

            for item in sorted(source_dir.rglob("*")):
                if not item.is_file():
                    continue

                ext = item.suffix.lower()
                if ext == ".jcl":
                    jobs.append(item.name)
                elif ext in (".proc", ".prc"):
                    procs.append(item.name)

            if not jobs and not procs:
                return "No batch components (JCL/PROC) found in source directory."

            report = f"Found {len(jobs)} Jobs and {len(procs)} Procedures:\n"

            if jobs:
                report += "\nJOBS (Require .ps1 script AND Worker/Jobs code):\n"
                for j in jobs:
                    report += f"- {j}\n"

            if procs:
                report += "\nPROCEDURES (May be converted to shared code or scripts):\n"
                for p in procs:
                    report += f"- {p}\n"

            return report

        except Exception as e:
            logger.error(f"list_batch_components error: {e}")
            return f"Error listing batch components: {e}"
    
    @tool("initialize_solution")
    def initialize_solution(solution_name: str) -> str:
        """Initialize a .NET solution with the standard project structure.

        Note: The solution scaffold is pre-created by the system before
        the agent starts.  This tool verifies it exists and returns the
        current structure.

        Args:
            solution_name: Name for the solution (e.g., "ConvertedBatch")

        Returns:
            Success message with created structure
        """
        return scaffold_solution(output_dir, source_dir, solution_name)

    @tool("write_code_file")
    def write_code_file(relative_path: str, content: str) -> str:
        """Write a code file to the generated solution.
        
        Args:
            relative_path: Path relative to local-migration/ (e.g., "src/Core/Entities/Customer.cs")
            content: The complete file content
            
        Returns:
            Success or error message
        """
        ALLOWED_EXTENSIONS = {
            ".cs", ".csproj", ".sln",
            ".ps1", ".sh", ".bat",
            ".md",
            ".json", ".config", ".xml",
            ".gitignore", ".editorconfig"
        }

        # Filenames the agent must NEVER create (infra / devops files)
        FORBIDDEN_FILENAMES = {
            "dockerfile", "docker-compose.yml", "docker-compose.yaml",
            "config.yml", "config.yaml",
            ".dockerignore",
            "deploy.ps1", "deploy.sh", "deploy.yml", "deploy.yaml",
            "ci.yml", "ci.yaml", "pipeline.yml", "pipeline.yaml",
            "makefile",
        }

        try:
            # Validate path doesn't escape
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"

            # ----- Forbidden filename blocklist -----
            if target.name.lower() in FORBIDDEN_FILENAMES:
                return (f"Error: FORBIDDEN file '{target.name}'. "
                        "This is an infrastructure/DevOps file that must NOT be part of the generated solution.")

            # Validate extension
            ext = target.suffix.lower()
            if ext not in ALLOWED_EXTENSIONS:
                return (f"Error: Extension '{ext}' is not allowed. "
                        f"Allowed: {', '.join(sorted(ALLOWED_EXTENSIONS))}")

            # Block arbitrary markdown files (prevent pollution)
            if ext == ".md" and target.name.lower() not in ("readme.md", "setup.md", "process_flow.md"):
                return (f"Error: Content Policy Violation. Writing '{target.name}' is FORBIDDEN.\n"
                        "Allowed Markdown: 'README.md', 'setup.md', 'process_flow.md'.\n"
                        "DO NOT write intermediate plans, thoughts, or status files.")

            # Block intermediate PowerShell scripts (prevent ad-hoc fix scripts)
            if ext == ".ps1":
                parent_dir = target.parent
                # Check if path ends with scripts/jobs or scripts/common
                # Robust check across OS
                is_jobs_dir = parent_dir.name == "jobs" and parent_dir.parent.name == "scripts"
                is_common_dir = parent_dir.name == "common" and parent_dir.parent.name == "scripts"
                
                if is_jobs_dir:
                    if not target.name.startswith("run-"):
                        return (f"Error: Content Policy Violation. Writing '{target.name}' to 'scripts/jobs/' is FORBIDDEN.\n"
                                "Requirement: Scripts in 'scripts/jobs/' MUST start with 'run-' (e.g. 'run-job1.ps1').")
                elif is_common_dir:
                    pass # Allow common scripts
                else:
                    return (f"Error: Content Policy Violation. Writing '{target.name}' to '{parent_dir.name}' is FORBIDDEN.\n"
                            "Allowed Locations:\n"
                            "1. 'scripts/jobs/' (MUST start with 'run-')\n"
                            "2. 'scripts/common/' (Shared logic)\n"
                            "ALL other .ps1 files (e.g. ad-hoc inputs/tests) are PROHIBITED.")

            # Enforce directory structure (Agent cannot create new folders)
            if not target.parent.exists():
                return f"Error: Directory '{target.parent.name}' does not exist. You must use the existing folder structure."

            # === PROVENANCE ENFORCEMENT ===
            # Service and Job files MUST have a // Source: tag referencing a
            # source file that was actually read via view_source_file().
            is_service = "/Services/" in relative_path and ext == ".cs"
            is_job = "/Jobs/" in relative_path and ext == ".cs" and target.name != "IJob.cs"

            if is_service or is_job:
                source_tag = re.search(r'//\s*Source:\s*([\w.\-]+)', content)
                registry = get_source_read_registry(project_id)

                if not source_tag:
                    return (f"Error: {target.name} is missing a '// Source: FILENAME.ext' comment at the top. "
                            "Every Service and Job file MUST reference the mainframe source it was converted from.")

                source_name = source_tag.group(1).strip()
                source_key = source_name.lower()

                if source_key not in registry:
                    return (f"Error: {target.name} references Source: {source_name} but you have NOT read that file yet. "
                            f"Call view_source_file('{source_name}') first, then rewrite this file with real logic.")

                ri = registry[source_key]
                cov = (ri['lines_covered'] / max(ri['total_lines'], 1)) * 100
                logger.info(f"[Provenance] {target.name} ← {source_name} ({cov:.0f}% read)")

            # === JCL STEM VALIDATION FOR WORKER JOBS ===
            # Worker Job filenames must correspond to actual JCL source file stems.
            # e.g. SETLJOB.jcl → stem "setljob" → Worker class must be Setljob.cs
            if is_job and source_dir.exists():
                jcl_stems = {f.stem.lower() for f in source_dir.rglob("*.jcl") if f.is_file()}
                job_stem = target.stem.lower()
                if jcl_stems and job_stem not in jcl_stems:
                        return (f"Error: Worker Job '{target.name}' does not match any JCL source file. "
                                f"Available JCL stems: {', '.join(sorted(jcl_stems))}. "
                                "The Worker Job class name MUST match the JCL job name.")

            # Write file
            target.write_text(content, encoding="utf-8")

            logger.info(f"Wrote code file: {relative_path}")
            return f"Successfully wrote: {relative_path}"
            
        except Exception as e:
            logger.error(f"write_code_file error: {e}")
            return f"Error writing file: {e}"
    
    @tool("list_generated_files")
    def list_generated_files() -> str:
        """List all files generated so far in the solution.
        
        Use this to check what's already been created (useful for resuming).
        
        Returns:
            List of generated files with sizes
        """
        try:
            if not output_dir.exists():
                return "No files generated yet. Call initialize_solution() first."
            
            # Simple tree generator
            def tree(dir_path: Path, prefix: str = ""):
                lines = []
                # Helper to sort directories first, then files
                def sort_key(p): return (not p.is_dir(), p.name.lower())
                
                try:
                    contents = sorted(dir_path.iterdir(), key=sort_key)
                except Exception: return []

                for i, path in enumerate(contents):
                    is_last = (i == len(contents) - 1)
                    connector = "└── " if is_last else "├── "
                    
                    if path.is_dir():
                        if path.name in (".git", ".vs", "bin", "obj"): continue # Skip noise
                        lines.append(f"{prefix}{connector}{path.name}/")
                        extension = "    " if is_last else "│   "
                        lines.extend(tree(path, prefix + extension))
                    else:
                        size = path.stat().st_size
                        lines.append(f"{prefix}{connector}{path.name} ({size} B)")
                return lines

            tree_lines = tree(output_dir)
            
            if not tree_lines:
                return "Output directory exists but contains no files"
            
            return f"Generated Solution Structure ({len(tree_lines)} items):\nroot/\n" + "\n".join(tree_lines)
            
        except Exception as e:
            logger.error(f"list_generated_files error: {e}")
            return f"Error listing files: {e}"

    @tool("read_generated_file")
    def read_generated_file(relative_path: str, start_line: int = 1, end_line: int = 100) -> str:
        """Read a file from the generated solution. Use this to inspect your own output.

        Args:
            relative_path: Path relative to local-migration/ (e.g., "src/Core/Services/FsmainService.cs")
            start_line: First line to read (1-indexed, default 1)
            end_line: Last line to read (1-indexed, default 100)

        Returns:
            The requested lines with line numbers, or error message
        """
        try:
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"

            if not target.exists():
                return f"Error: File '{relative_path}' does not exist. Use list_generated_files() to see available files."

            if not target.is_file():
                return f"Error: Path '{relative_path}' is not a file"

            lines = target.read_text(encoding="utf-8", errors="replace").splitlines()
            total_lines = len(lines)

            if start_line < 1:
                start_line = 1
            if end_line > total_lines:
                end_line = total_lines
            if start_line > end_line:
                return f"Error: Invalid line range ({start_line}-{end_line})"

            selected = lines[start_line - 1:end_line]
            result_lines = [f"{i}: {line}" for i, line in enumerate(selected, start=start_line)]

            return f"File: {relative_path} (lines {start_line}-{end_line} of {total_lines})\n" + "\n".join(result_lines)

        except Exception as e:
            logger.error(f"read_generated_file error: {e}")
            return f"Error reading file: {e}"

    @tool("remove_file")
    def remove_file(relative_path: str) -> str:
        """Remove a specific file from the solution.
        
        Args:
            relative_path: Path relative to local-migration/ (e.g., "src/Core/OldClass.cs")
            
        Returns:
            Success or error message
        """
        try:
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"
            
            if not target.exists():
                return f"Error: File '{relative_path}' does not exist"
            
            if not target.is_file():
                return f"Error: Path '{relative_path}' is not a file"
            
            target.unlink()
            logger.info(f"Removed file: {relative_path}")
            return f"Successfully removed file: {relative_path}"
            
        except Exception as e:
            logger.error(f"remove_file error: {e}")
            return f"Error removing file: {e}"

    return [
        initialize_solution,
        write_code_file,
        list_generated_files,
        read_generated_file,
        remove_file,
        list_batch_components,
    ]
