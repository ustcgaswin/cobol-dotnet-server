"""Test Case Generator Service.

Analyzes the generated .NET solution and ensures every component has corresponding unit tests.
Reuses existing tests or generates new ones where coverage is missing.
Runs automatically after codegen or can be triggered via API.
"""

import operator
import re
from datetime import datetime
from pathlib import Path
from typing import Annotated, Literal
from uuid import UUID

from langchain.messages import (
    AnyMessage,
    HumanMessage,
    AIMessage,
    SystemMessage,
    ToolMessage,
)
from langchain.tools import tool
from langgraph.graph import StateGraph, START, END
from loguru import logger
from typing_extensions import TypedDict

from app.config.llm import get_llm, CODEGEN, LLMModel
from app.config.settings import settings


# ---------------------------------------------------------------------------
# State
# ---------------------------------------------------------------------------
class TestGeneratorState(TypedDict):
    """State for the test generator agent."""

    messages: Annotated[list[AnyMessage], operator.add]
    project_id: str
    output_path: str


# ---------------------------------------------------------------------------
# Prompt
# ---------------------------------------------------------------------------
TEST_GENERATOR_PROMPT = """\
You are a Senior .NET QA Automation Engineer. Your task is to ensure the generated .NET solution has 100% test coverage for its business logic.

## Workflow
1. Call `list_solution_files()` to see the full solution structure.
2. Identify all logic files in `src/Core/Services`, `src/Infrastructure/Repositories`, and `src/Worker/Jobs`.
3. Call `list_test_files()` to see what tests already exist.
4. For each logic file:
    a. Check if a corresponding test file exists in `tests/`.
    b. If it exists, call `read_file(test_path)` and `read_file(source_path)` to verify it actually tests the logic and uses the correct Mocks/Traits.
    c. If it's missing or incomplete, generate/update it using `write_test_file()`.
5. Every test file MUST:
    - Use xUnit, Moq, and FluentAssertions.
    - Include `[Trait("Functionality", "Fxxx")]` tags linking to the Functionality Catalog IDs.
    - Mock all dependencies injected via constructor.
    - Correctly place files in:
        - `tests/Core.Tests/Services/`
        - `tests/Infrastructure.Tests/Repositories/`
        - `tests/Worker.Tests/Jobs/`
6. When complete, call `run_tests()` to verify the new tests compile and pass.
7. If `run_tests()` fails, fix the tests and try again.
8. Finish by providing a summary of coverage.

## Rules
- Do NOT overwrite valid existing tests — only add missing cases or fix broken ones.
- Tag every test with its corresponding F-ID (refer to `read_functionality_catalog()`).
- Use file-scoped namespaces and .NET 8 standards.
"""


# ---------------------------------------------------------------------------
# Service
# ---------------------------------------------------------------------------
class TestGeneratorService:
    """ENSURES test coverage for the generated .NET solution."""

    def __init__(self, project_id: UUID):
        self.project_id = project_id
        self._files_read: set[str] = set()

    async def generate(self) -> str:
        """Run the test-generator agent and return a summary."""
        output_dir = self._get_output_dir()
        if not output_dir or not output_dir.exists():
            raise FileNotFoundError(
                f"No generated solution found for project {self.project_id}"
            )

        tools = self._create_tools(output_dir)
        agent = self._create_agent(tools)

        logger.info(f"[TestGenerator] Starting agent for project {self.project_id}")
        result = await agent.ainvoke(
            {
                "messages": [
                    HumanMessage(
                        content=(
                            "Verify and generate unit tests for the solution. "
                            "Ensure all Services, Repositories, and Jobs are covered."
                        )
                    )
                ],
                "project_id": str(self.project_id),
                "output_path": str(output_dir),
            },
            config={"recursion_limit": 500},
        )

        logger.info(f"[TestGenerator] Done for project {self.project_id}")
        return "Test generation/verification complete."

    def _get_output_dir(self) -> Path | None:
        """Locate the ``*-local`` solution directory."""
        codegen_dir = (
            Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
            / str(self.project_id)
            / "code-migration"
        )
        if not codegen_dir.exists():
            return None
        for child in codegen_dir.iterdir():
            if child.is_dir() and child.name.endswith("-local"):
                return child
        return None

    def _create_agent(self, tools: list):
        """Build 2-node LangGraph agent."""
        model = get_llm(CODEGEN, model=LLMModel.CLAUDE_SONNET_4_5)
        tools_by_name = {t.name: t for t in tools}
        model_with_tools = model.bind_tools(tools)

        def llm_call(state: TestGeneratorState) -> dict:
            system = [SystemMessage(content=TEST_GENERATOR_PROMPT)]
            response = model_with_tools.invoke(system + state["messages"])
            return {"messages": [response]}

        def tool_node(state: TestGeneratorState) -> dict:
            results: list[ToolMessage] = []
            last = state["messages"][-1]
            for tc in last.tool_calls:
                t = tools_by_name.get(tc["name"])
                if not t:
                    obs = f"Error: Unknown tool '{tc['name']}'"
                else:
                    try:
                        obs = t.invoke(tc["args"])
                    except Exception as e:
                        obs = f"Error: {e}"
                results.append(
                    ToolMessage(content=str(obs), tool_call_id=tc["id"])
                )
            return {"messages": results}

        def should_continue(state: TestGeneratorState) -> Literal["tool_node", "__end__"]:
            last = state["messages"][-1]
            if hasattr(last, "tool_calls") and last.tool_calls:
                return "tool_node"
            return "__end__"

        builder = StateGraph(TestGeneratorState)
        builder.add_node("llm_call", llm_call)
        builder.add_node("tool_node", tool_node)
        builder.add_edge(START, "llm_call")
        builder.add_conditional_edges("llm_call", should_continue, ["tool_node", END])
        builder.add_edge("tool_node", "llm_call")
        return builder.compile()

    def _create_tools(self, output_dir: Path) -> list:
        """Create tools for the test generator."""

        @tool("list_solution_files")
        def list_solution_files() -> str:
            """List all files in the solution."""
            def _get_tree(p, depth=0):
                res = []
                for child in sorted(p.iterdir()):
                    if child.name in ("bin", "obj", ".git", ".vs"): continue
                    res.append("  " * depth + ("/" if child.is_dir() else "") + child.name)
                    if child.is_dir(): res.extend(_get_tree(child, depth + 1))
                return res
            return "\n".join(_get_tree(output_dir))

        @tool("list_test_files")
        def list_test_files() -> str:
            """List all files in the tests/ directory."""
            test_dir = output_dir / "tests"
            if not test_dir.exists(): return "tests/ directory not found."
            def _get_tree(p, depth=0):
                res = []
                for child in sorted(p.iterdir()):
                    if child.name in ("bin", "obj"): continue
                    res.append("  " * depth + ("/" if child.is_dir() else "") + child.name)
                    if child.is_dir(): res.extend(_get_tree(child, depth + 1))
                return res
            return "\n".join(_get_tree(test_dir))

        @tool("read_file")
        def read_file(relative_path: str) -> str:
            """Read file content."""
            target = (output_dir / relative_path).resolve()
            if not target.exists(): return f"Error: {relative_path} not found"
            return target.read_text(encoding="utf-8", errors="replace")

        @tool("write_test_file")
        def write_test_file(relative_path: str, content: str) -> str:
            """Write a test file to the solution."""
            if not relative_path.startswith("tests/"):
                return "Error: You can only write to the tests/ directory."
            target = (output_dir / relative_path).resolve()
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(content, encoding="utf-8")
            return f"SUCCESS: Wrote {relative_path}"

        @tool("run_tests")
        def run_tests() -> str:
            """Run 'dotnet test' and return the output."""
            import subprocess
            try:
                result = subprocess.run(
                    ["dotnet", "test"],
                    cwd=str(output_dir),
                    capture_output=True,
                    text=True,
                    timeout=120
                )
                return result.stdout + result.stderr
            except Exception as e:
                return f"Error running tests: {e}"

        @tool("read_functionality_catalog")
        def read_functionality_catalog() -> str:
            """Read the functionality catalog to map tests to F-IDs."""
            catalog_path = output_dir.parent.parent / "system_context" / "functionality_catalog.md"
            if not catalog_path.exists():
                return "Functionality catalog not found."
            return catalog_path.read_text(encoding="utf-8", errors="replace")

        return [
            list_solution_files,
            list_test_files,
            read_file,
            write_test_file,
            run_tests,
            read_functionality_catalog
        ]
