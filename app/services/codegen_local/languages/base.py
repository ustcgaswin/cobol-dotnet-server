from abc import ABC, abstractmethod
from pathlib import Path

class LanguageStrategy(ABC):
    """Abstract base class for language-specific code generation strategies."""

    @abstractmethod
    def get_system_prompt(self) -> str:
        """Get the system prompt for the agent."""
        pass

    @abstractmethod
    def scaffold_solution(self, output_path: Path, source_path: Path, project_name: str) -> str:
        """Scaffold the project structure (e.g. .sln or pom.xml)."""
        pass

    @abstractmethod
    def get_build_tools(self, project_id: str, output_path: Path) -> list:
        """Get the build/test tools for this language."""
        pass

    @abstractmethod
    def get_test_cmd(self) -> list[str]:
        """Get the command to run tests."""
        pass
