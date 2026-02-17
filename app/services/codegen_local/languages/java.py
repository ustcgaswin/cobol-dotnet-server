from pathlib import Path

from app.services.codegen_local.languages.base import LanguageStrategy
from app.services.codegen_local.languages.java_prompts import SYSTEM_PROMPT
from app.services.codegen_local.tools import java_build_tools, java_scaffold


class JavaStrategy(LanguageStrategy):
    """Strategy for generating Java Spring Boot code."""

    def get_system_prompt(self) -> str:
        return SYSTEM_PROMPT

    def scaffold_solution(self, output_path: Path, source_path: Path, project_name: str) -> str:
        return java_scaffold.scaffold_java_solution(output_path, source_path, project_name)

    def get_build_tools(self, project_id: str, output_path: Path) -> list:
        return java_build_tools.create_java_build_tools(project_id, str(output_path))
    
    def get_test_cmd(self) -> list[str]:
        # Logic to return check command for external validation if needed
        # Just returning list for consistency
        pass
