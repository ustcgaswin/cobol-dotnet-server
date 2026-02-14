"""Process Flow Diagram Service.

Generates a Mermaid process flow diagram by analyzing the generated .NET solution.
Runs a focused LangGraph agent with 4 tools (3 read-only + 1 validated write).
Completely independent from the codegen agent.
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
class ProcessFlowState(TypedDict):
    """State for the process flow agent."""

    messages: Annotated[list[AnyMessage], operator.add]
    project_id: str
    output_path: str


# ---------------------------------------------------------------------------
# Prompt
# ---------------------------------------------------------------------------
PROCESS_FLOW_PROMPT = """\
You are a .NET Solution Architect. Analyze a generated .NET 8 solution and
produce a Mermaid process flow diagram showing the system architecture.

## Workflow
1. Call `list_solution_files()` to see the full solution structure.
2. Start with Worker/Jobs — these are the entry points (triggered by PS1 scripts).
3. For each Job, call `read_file_signatures(path)` to see its constructor
   dependencies and methods.
4. Trace into Services — read their signatures to discover their own dependencies.
5. Trace into Repositories — read their signatures for data-access patterns.
6. Read PS1 scripts with `read_script(path)` to understand job orchestration order.
7. When you have a **complete** picture, call `submit_process_flow(mermaid)`.

## Diagram Requirements
- Entry Points: PS1 Scripts → Worker Jobs
- Flow: Job → Service(s) → Repository → Database/External
- Use subgraphs: `subgraph Scripts`, `subgraph Workers`, `subgraph Services`,
  `subgraph Repositories`, `subgraph External`
- Show data-flow labels on edges where meaningful.
- Include external systems (DB2, file I/O, queues) as terminal nodes in the
  `External` subgraph.
- Name nodes by **actual class names** from the code you read.

## Rules
- You MUST call `read_file_signatures()` on EVERY Job and Service file.
  Do not skip any.
- Do NOT guess class names or methods — only include what you actually read.
- If `submit_process_flow()` returns validation errors, fix the mermaid and
  call it again.
- Do NOT include test files, bin/obj, or interfaces in the diagram.
"""


# ---------------------------------------------------------------------------
# Service
# ---------------------------------------------------------------------------
class ProcessFlowService:
    """Generates ``process_flow.md`` from the generated .NET solution."""

    def __init__(self, project_id: UUID):
        self.project_id = project_id
        self._files_read: set[str] = set()

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    async def generate(self) -> Path:
        """Run the process-flow agent and return the path to ``process_flow.md``."""
        output_dir = self._get_output_dir()
        if not output_dir or not output_dir.exists():
            raise FileNotFoundError(
                f"No generated solution found for project {self.project_id}"
            )

        tools = self._create_tools(output_dir)
        agent = self._create_agent(tools)

        logger.info(f"[ProcessFlow] Starting agent for project {self.project_id}")
        await agent.ainvoke(
            {
                "messages": [
                    HumanMessage(
                        content=(
                            "Analyze the generated .NET solution and produce "
                            "a Mermaid process flow diagram."
                        )
                    )
                ],
                "project_id": str(self.project_id),
                "output_path": str(output_dir),
            },
            config={"recursion_limit": 200},
        )

        # Soft completeness check
        self._check_completeness(output_dir)

        flow_file = output_dir / "process_flow.md"
        if not flow_file.exists():
            raise RuntimeError(
                "Agent completed but process_flow.md was not written"
            )
        logger.info(f"[ProcessFlow] Done — {flow_file}")
        return flow_file

    def get_existing(self) -> str | None:
        """Return ``process_flow.md`` content if it exists, else ``None``."""
        output_dir = self._get_output_dir()
        if not output_dir:
            return None
        flow_file = output_dir / "process_flow.md"
        if flow_file.exists():
            return flow_file.read_text(encoding="utf-8", errors="replace")
        return None

    # ------------------------------------------------------------------
    # Output directory resolution
    # ------------------------------------------------------------------

    def _get_output_dir(self) -> Path | None:
        """Locate the ``*-local`` solution directory for this project."""
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

    # ------------------------------------------------------------------
    # Agent factory
    # ------------------------------------------------------------------

    def _create_agent(self, tools: list):
        """Build a 2-node LangGraph agent (llm_call + tool_node)."""
        model = get_llm(CODEGEN, model=LLMModel.CLAUDE_SONNET_4_5)
        tools_by_name = {t.name: t for t in tools}
        model_with_tools = model.bind_tools(tools)

        def llm_call(state: ProcessFlowState) -> dict:
            system = [SystemMessage(content=PROCESS_FLOW_PROMPT)]
            response = model_with_tools.invoke(system + state["messages"])
            return {"messages": [response]}

        def tool_node(state: ProcessFlowState) -> dict:
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
                        logger.error(f"[ProcessFlow] Tool {tc['name']} failed: {e}")
                results.append(
                    ToolMessage(content=str(obs), tool_call_id=tc["id"])
                )
            return {"messages": results}

        def should_continue(
            state: ProcessFlowState,
        ) -> Literal["tool_node", "__end__"]:
            last = state["messages"][-1]
            if hasattr(last, "tool_calls") and last.tool_calls:
                return "tool_node"
            return "__end__"

        builder = StateGraph(ProcessFlowState)
        builder.add_node("llm_call", llm_call)
        builder.add_node("tool_node", tool_node)
        builder.add_edge(START, "llm_call")
        builder.add_conditional_edges(
            "llm_call", should_continue, ["tool_node", END]
        )
        builder.add_edge("tool_node", "llm_call")
        return builder.compile()

    # ------------------------------------------------------------------
    # Tools
    # ------------------------------------------------------------------

    def _create_tools(self, output_dir: Path) -> list:
        """Create the 4 process-flow tools scoped to *output_dir*."""
        service_ref = self  # Capture for closures

        @tool("list_solution_files")
        def list_solution_files() -> str:
            """List all files in the generated .NET solution (skips bin/obj).

            Returns:
                Tree view of the solution with file sizes.
            """

            def _tree(dir_path: Path, prefix: str = "") -> list[str]:
                lines: list[str] = []
                try:
                    contents = sorted(
                        dir_path.iterdir(),
                        key=lambda p: (not p.is_dir(), p.name.lower()),
                    )
                except Exception:
                    return []
                for i, path in enumerate(contents):
                    is_last = i == len(contents) - 1
                    connector = "└── " if is_last else "├── "
                    if path.is_dir():
                        if path.name in (".git", ".vs", "bin", "obj"):
                            continue
                        lines.append(f"{prefix}{connector}{path.name}/")
                        ext = "    " if is_last else "│   "
                        lines.extend(_tree(path, prefix + ext))
                    else:
                        size = path.stat().st_size
                        lines.append(
                            f"{prefix}{connector}{path.name} ({size} B)"
                        )
                return lines

            tree_lines = _tree(output_dir)
            if not tree_lines:
                return "Solution directory is empty."
            return (
                f"Solution Structure ({len(tree_lines)} items):\n"
                "root/\n" + "\n".join(tree_lines)
            )

        @tool("read_file_signatures")
        def read_file_signatures(relative_path: str) -> str:
            """Read class/method signatures from a .cs file (strips method bodies).

            Extracts: using statements, namespace, class declaration, constructor
            parameters (DI dependencies), method signatures, and ``// Source:``
            / ``// Implements:`` tags.

            Args:
                relative_path: Path relative to solution root
                    (e.g. ``src/Core/Services/FsmainService.cs``)

            Returns:
                Formatted signatures string, or an error message.
            """
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir.resolve())):
                return "Error: Path is outside the solution directory."
            if not target.exists():
                return f"Error: File not found: {relative_path}"
            if target.suffix != ".cs":
                return f"Error: Not a .cs file: {relative_path}"

            service_ref._files_read.add(
                relative_path.replace("\\", "/")
            )

            try:
                content = target.read_text(encoding="utf-8", errors="replace")
                return _extract_signatures(content, relative_path)
            except Exception as e:
                return f"Error reading {relative_path}: {e}"

        @tool("read_script")
        def read_script(relative_path: str) -> str:
            """Read a PowerShell script from the generated solution.

            Args:
                relative_path: Path relative to solution root
                    (e.g. ``scripts/jobs/run-setljob.ps1``)

            Returns:
                Full script content, or an error message.
            """
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir.resolve())):
                return "Error: Path is outside the solution directory."
            if not target.exists():
                return f"Error: File not found: {relative_path}"
            if target.suffix != ".ps1":
                return f"Error: Not a .ps1 file: {relative_path}"

            service_ref._files_read.add(
                relative_path.replace("\\", "/")
            )

            try:
                return target.read_text(encoding="utf-8", errors="replace")
            except Exception as e:
                return f"Error reading {relative_path}: {e}"

        @tool("submit_process_flow")
        def submit_process_flow(mermaid_content: str) -> str:
            """Validate and save the process flow diagram as ``process_flow.md``.

            Args:
                mermaid_content: Mermaid diagram content (with or without
                    triple-backtick fences).

            Returns:
                ``SUCCESS: …`` if saved, or a list of validation errors to fix.
            """
            # Extract bare mermaid block if fenced
            match = re.search(
                r"```mermaid\s*(.*?)\s*```", mermaid_content, re.DOTALL
            )
            diagram = match.group(1) if match else mermaid_content.strip()

            issues = _validate_mermaid(diagram)
            if issues:
                return (
                    "VALIDATION FAILED:\n"
                    + "\n".join(f"- {i}" for i in issues)
                    + "\n\nFix these issues and call submit_process_flow() again."
                )

            final = (
                "# Process Flow Diagram\n\n"
                f"> Generated: {datetime.utcnow().isoformat()}\n"
                f"> Project: {service_ref.project_id}\n\n"
                f"```mermaid\n{diagram}\n```\n"
            )
            flow_file = output_dir / "process_flow.md"
            flow_file.write_text(final, encoding="utf-8")
            logger.info(f"[ProcessFlow] Diagram written to {flow_file}")
            return (
                f"SUCCESS: Process flow saved to process_flow.md "
                f"({len(diagram)} chars, "
                f"{diagram.count('-->')} edges)."
            )

        return [
            list_solution_files,
            read_file_signatures,
            read_script,
            submit_process_flow,
        ]

    # ------------------------------------------------------------------
    # Completeness check (soft — warns, does not block)
    # ------------------------------------------------------------------

    def _check_completeness(self, output_dir: Path) -> None:
        """Warn if the agent skipped key files."""
        key_dirs = [
            output_dir / "src" / "Worker" / "Jobs",
            output_dir / "src" / "Core" / "Services",
        ]
        skip_filenames = {"IJob.cs", "Program.cs"}

        for d in key_dirs:
            if not d.exists():
                continue
            for cs in d.glob("*.cs"):
                if cs.name in skip_filenames:
                    continue
                # Interfaces (e.g. IFooService.cs) are optional
                if (
                    cs.name.startswith("I")
                    and len(cs.name) > 1
                    and cs.name[1].isupper()
                ):
                    continue
                rel = str(cs.relative_to(output_dir)).replace("\\", "/")
                if rel not in self._files_read:
                    logger.warning(
                        f"[ProcessFlow] Agent never read {rel} — "
                        "it may be missing from the diagram"
                    )


# ---------------------------------------------------------------------------
# Module-Level Helpers
# ---------------------------------------------------------------------------


def _extract_signatures(content: str, file_path: str) -> str:
    """Extract class/method signatures from C# source code.

    Pure regex — no LLM involved.  Returns a formatted string with:
    ``using`` statements, namespace, class declaration, constructor
    parameters, method signatures, and provenance tags.
    """
    lines = content.splitlines()
    result: list[str] = [f"--- {file_path} ---"]

    # 1. Provenance tags (// Source:, // Implements:)
    for line in lines[:30]:
        stripped = line.strip()
        if stripped.startswith("// Source:") or stripped.startswith(
            "// Implements:"
        ):
            result.append(stripped)

    # 2. Using statements (cap at 15)
    usings = [
        l.strip()
        for l in lines
        if l.strip().startswith("using ") and l.strip().endswith(";")
    ]
    if usings:
        result.append("")
        result.extend(usings[:15])

    # 3. Namespace
    for line in lines:
        ns_match = re.match(r"\s*namespace\s+([\w.]+)", line)
        if ns_match:
            result.append(f"\nnamespace {ns_match.group(1)};")
            break

    # 4. Class / interface / record declaration
    class_name: str | None = None
    for line in lines:
        cls_match = re.match(
            r"\s*(?:public|internal|private|protected)?\s*"
            r"(?:abstract|static|sealed|partial)?\s*"
            r"(?:class|interface|record|struct|enum)\s+(\w+)([^{]*)",
            line,
        )
        if cls_match:
            class_name = cls_match.group(1)
            decl = line.strip().rstrip("{").strip()
            result.append(f"\n{decl}")
            result.append("{")
            break

    # 5. Constructor → parameters (= DI dependencies)
    if class_name:
        for i, line in enumerate(lines):
            ctor_match = re.match(
                rf"\s*(?:public|internal|protected|private)?\s*{re.escape(class_name)}"
                r"\s*\(([^)]*)\)",
                line,
            )
            if ctor_match:
                params = ctor_match.group(1)
                # Handle multi-line constructors
                if ")" not in line:
                    for j in range(i + 1, min(i + 20, len(lines))):
                        params += " " + lines[j].strip()
                        if ")" in lines[j]:
                            break
                    # Clean up to just the params portion
                    paren_end = params.find(")")
                    if paren_end != -1:
                        params = params[:paren_end]
                params = re.sub(r"\s+", " ", params).strip()
                result.append(f"    ctor({params})")
                break

    # 6. Method signatures
    method_pattern = re.compile(
        r"\s*(public|internal|protected|private)\s+"
        r"(?:async\s+)?(?:virtual\s+|override\s+|static\s+|abstract\s+|sealed\s+)*"
        r"([\w<>\[\],\s?]+?)\s+(\w+)\s*\(([^)]*)\)",
    )
    result.append("")
    result.append("    // Methods")
    methods_found = 0
    for line in lines:
        m = method_pattern.match(line)
        if m:
            visibility = m.group(1)
            return_type = m.group(2).strip()
            name = m.group(3)
            params = m.group(4).strip()
            # Skip constructors (already handled above)
            if class_name and name == class_name:
                continue
            result.append(f"    {visibility} {return_type} {name}({params})")
            methods_found += 1

    if methods_found == 0:
        result.append("    // (no public methods found)")

    result.append("}")
    return "\n".join(result)


def _validate_mermaid(content: str) -> list[str]:
    """Basic mermaid syntax validation.

    Returns a list of issues (empty means valid).
    """
    issues: list[str] = []
    if not re.search(r"(graph|flowchart)\s+(TD|LR|TB|RL|BT)", content):
        issues.append(
            "Missing graph/flowchart declaration (e.g. 'graph TD')"
        )
    if "-->" not in content:
        issues.append(
            "No edges found (missing '-->'). A diagram must have connections."
        )
    if "subgraph" not in content.lower():
        issues.append(
            "No subgraphs found. Use 'subgraph Workers', "
            "'subgraph Services', etc."
        )
    # Bracket balance
    opens = content.count("[") + content.count("(") + content.count("{")
    closes = content.count("]") + content.count(")") + content.count("}")
    if abs(opens - closes) > 2:
        issues.append(
            f"Bracket imbalance: {opens} opens vs {closes} closes"
        )
    return issues
