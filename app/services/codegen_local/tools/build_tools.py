"""Build and test tools for the code generation agent.

Executes dotnet build and dotnet test commands.
"""

import subprocess
from pathlib import Path

from langchain.tools import tool
from loguru import logger


def create_build_tools(project_id: str, output_path: str) -> list:
    """Create build and test tools.
    
    Args:
        project_id: Project ID for locating the solution
        output_path: Absolute path to the output directory with the solution
        
    Returns:
        List of LangChain tools
    """
    output_dir = Path(output_path)
    
    @tool("run_dotnet_build")
    def run_dotnet_build() -> str:
        """Run 'dotnet build' on the generated solution.
        
        Use this to compile the solution and check for errors.
        If there are errors, fix the code and try again.
        
        Returns:
            Build output including any errors or warnings
        """
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
        """Run 'dotnet test' on the generated solution.
        
        Use this to run unit tests after the build succeeds.
        
        Returns:
            Test results including passed/failed counts
        """
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
    
    return [run_dotnet_build, run_dotnet_test]
