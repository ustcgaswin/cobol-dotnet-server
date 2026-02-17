import subprocess
import os
from pathlib import Path
from loguru import logger
from langchain.tools import tool

def create_java_build_tools(project_id: str, output_path: str) -> list:
    """Create Maven build and test tools.
    
    Args:
        project_id: Project ID 
        output_path: Absolute path to the output directory
    """
    output_dir = Path(output_path)
    
    def _get_mvn_command():
        """Get the Maven command (mvnw or mvn)."""
        # Try wrapper first
        if os.name == 'nt':
            wrapper = output_dir / "mvnw.cmd"
        else:
            wrapper = output_dir / "mvnw"
            
        if wrapper.exists():
            return str(wrapper)
        
        # Fallback to global maven
        return "mvn"

    @tool("run_maven_build")
    def run_maven_build() -> str:
        """Run 'mvn clean compile' on the project."""
        try:
            cmd = _get_mvn_command()
            # On Windows, shell=True might be needed for .cmd files if not strictly executed
            result = subprocess.run(
                [cmd, "clean", "compile"],
                cwd=str(output_dir),
                capture_output=True,
                text=True,
                timeout=300,
                shell=(os.name == 'nt')
            )
            
            output = result.stdout + result.stderr
            if result.returncode == 0:
                logger.info("Maven build succeeded")
                return "BUILD SUCCEEDED"
            else:
                return f"BUILD FAILED (Exit Code {result.returncode})\n\n{output}"
                
        except Exception as e:
            return f"Error running maven build: {e}"

    @tool("run_maven_test")
    def run_maven_test() -> str:
        """Run 'mvn test' on the project."""
        try:
            cmd = _get_mvn_command()
            result = subprocess.run(
                [cmd, "test"],
                cwd=str(output_dir),
                capture_output=True,
                text=True,
                timeout=300,
                shell=(os.name == 'nt')
            )
            
            output = result.stdout + result.stderr
            if result.returncode == 0:
                return f"TESTS PASSED\n{output[-500:]}"
            else:
                return f"TESTS FAILED (Exit Code {result.returncode})\n\n{output}"
                
        except Exception as e:
            return f"Error running maven tests: {e}"

    return [run_maven_build, run_maven_test]
