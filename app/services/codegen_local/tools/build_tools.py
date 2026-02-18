"""Build and test tools for the code generation agent.

Executes dotnet build and dotnet test commands.
"""

import subprocess
from pathlib import Path

from langchain.tools import tool
from loguru import logger
from app.services.codegen_local.tools.java_build_tools import create_java_build_tools

def create_build_tools(output_path: str, target_language: str = "dotnet", project_id: str = "", include_test_tools: bool = False) -> list:
    """Create build and test tools.
    
    Args:
        output_path: Absolute path to the output directory
        target_language: "dotnet" or "java"
        project_id: Project ID (optional, but good for context)
        include_test_tools: Whether to include tools for running tests
    """
    
    if target_language == "java":
        return create_java_build_tools(project_id, output_path, include_test_tools=include_test_tools)
        
    # DOTNET LOGIC
    # ... (existing dotnet implementation)
    output_dir = Path(output_path)
    
    @tool("run_dotnet_build")
    def run_dotnet_build() -> str:
        """Run 'dotnet build' on the generated solution."""
        try:
            # Find .sln file
            sln_files = list(output_dir.glob("*.sln"))
            if not sln_files:
                return "Error: No .sln file found. Call initialize_solution() first."
            
            sln_path = sln_files[0]
            
            result = subprocess.run(
                ["dotnet", "build", str(sln_path)],
                cwd=str(output_dir),
                capture_output=True,
                text=True,
                timeout=120,
            )
            
            output = result.stdout + result.stderr
            
            if result.returncode == 0:
                logger.info("dotnet build succeeded")
                return "BUILD SUCCEEDED"
            else:
                lines = output.splitlines()
                # Filter for errors and warnings
                errors = [l for l in lines if ": error " in l or "Build FAILED." in l]
                warnings = [l for l in lines if ": warning " in l]
                
                # If no standard errors found, return tail of log
                if not errors:
                    log_tail = "\n".join(lines[-30:])
                    return f"BUILD FAILED (Exit Code {result.returncode})\n\nNo standard errors found. Log tail:\n{log_tail}"
                
                # Construct focused error report
                report = f"BUILD FAILED ({len(errors)} errors)\n\nERRORS:\n" + "\n".join(errors)
                
                if warnings:
                    report += f"\n\nWARNINGS (First 10 of {len(warnings)}):\n" + "\n".join(warnings[:10])
                
                logger.warning(f"dotnet build FAILED: {len(errors)} errors")
                return report
                
        except subprocess.TimeoutExpired:
            logger.error("dotnet build timed out")
            return "Error: Build timed out after 120 seconds"
            
        except FileNotFoundError:
            logger.error("dotnet command not found")
            return ("Error: 'dotnet' command not found. "
                   ".NET SDK may not be installed. "
                   "Continuing without build verification - the code will still be generated.")
            
        except Exception as e:
            logger.error(f"run_dotnet_build error: {e}")
            return f"Error running build: {e}"
    
    @tool("run_dotnet_test")
    def run_dotnet_test() -> str:
        """Run 'dotnet test' on the generated solution."""
        try:
            sln_files = list(output_dir.glob("*.sln"))
            if not sln_files:
                return "Error: No .sln file found"
            
            sln_path = sln_files[0]
            
            result = subprocess.run(
                ["dotnet", "test", str(sln_path), "--no-build"],
                cwd=str(output_dir),
                capture_output=True,
                text=True,
                timeout=180,
            )
            
            output = result.stdout + result.stderr
            
            if result.returncode == 0:
                logger.info("dotnet test passed")
                # Parse summary
                summary = [l for l in output.splitlines() if "Total tests:" in l]
                summary_text = summary[0] if summary else "Tests passed."
                return f"TESTS PASSED\n{summary_text}"
            else:
                # For tests, we want the stack trace of failures, which can be long.
                # But we can truncate if massive.
                if len(output) > 8000:
                    output = output[:8000] + "\n...(truncated)"
                
                logger.warning(f"dotnet test failed with code {result.returncode}")
                return f"TESTS FAILED (exit code {result.returncode})\n\n{output}"
                
        except subprocess.TimeoutExpired:
            logger.error("dotnet test timed out")
            return "Error: Tests timed out after 180 seconds"
            
        except FileNotFoundError:
            return ("Error: 'dotnet' command not found. "
                   ".NET SDK may not be installed.")
            
        except Exception as e:
            logger.error(f"run_dotnet_test error: {e}")
            return f"Error running tests: {e}"
    
    tools = [run_dotnet_build]
    if include_test_tools:
        tools.append(run_dotnet_test)
        
    return tools
