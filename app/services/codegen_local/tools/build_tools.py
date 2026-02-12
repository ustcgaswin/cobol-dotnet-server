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
            
            # Truncate if too long
            if len(output) > 5000:
                output = output[:5000] + "\n...(truncated)"
            
            if result.returncode == 0:
                logger.info("dotnet build succeeded")
                return f"BUILD SUCCEEDED\n\n{output}"
            else:
                error_lines = [l for l in output.splitlines() if ": error " in l]
                logger.warning(f"dotnet build FAILED with {len(error_lines)} errors (exit code {result.returncode})")
                for err in error_lines[:10]:
                    logger.warning(f"  BUILD ERROR: {err.strip()}")
                return f"BUILD FAILED ({len(error_lines)} errors, exit code {result.returncode})\n\n{output}"
                
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
            
            if len(output) > 5000:
                output = output[:5000] + "\n...(truncated)"
            
            if result.returncode == 0:
                logger.info("dotnet test passed")
                return f"TESTS PASSED\n\n{output}"
            else:
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
