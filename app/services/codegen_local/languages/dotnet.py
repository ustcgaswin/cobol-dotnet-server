from pathlib import Path

from app.services.codegen_local.languages.base import LanguageStrategy
from app.services.codegen_local.languages.dotnet_prompt import SYSTEM_PROMPT
from app.services.codegen_local.tools import build_tools, scaffold


class DotNetStrategy(LanguageStrategy):
    """Strategy for generating .NET 8 code."""

    def get_system_prompt(self) -> str:
        return SYSTEM_PROMPT

    def scaffold_solution(self, output_path: Path, source_path: Path, project_name: str) -> str:
        return scaffold.scaffold_solution(output_path, source_path, project_name)

    def get_build_tools(self, project_id: str, output_path: Path) -> list:
        return build_tools.create_build_tools(project_id, str(output_path))
    
    def get_test_cmd(self) -> list[str]:
        return ["dotnet", "test"]
