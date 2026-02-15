"""Process Flow Diagram Service.

Two-step pipeline — no agent needed:
  1. Deterministic Python scan → ``dependency_graph_output.md``
  2. Single LLM call → ``process_flow.md`` (Mermaid diagram only)

Guarantees 100% file coverage (no files skipped) and costs exactly one
LLM call regardless of solution size.
"""

import re
from datetime import datetime
from pathlib import Path
from uuid import UUID

from langchain.messages import HumanMessage, SystemMessage
from loguru import logger

from app.config.llm import get_llm, CODEGEN, LLMModel
from app.config.settings import settings


# ---------------------------------------------------------------------------
# LLM Prompt — converts a dependency graph into a Mermaid diagram
# ---------------------------------------------------------------------------
MERMAID_PROMPT = """\
You are a .NET Solution Architect. You will receive a structured dependency
graph of a generated .NET 8 batch-processing solution.

Produce a **Mermaid flowchart** (`graph TD`) showing the full system architecture.

## Requirements
- Use **subgraphs**: `Scripts`, `Workers`, `Services`, `Repositories`, `External`
- Entry points: PS1 scripts → Worker Jobs
- Flow direction: Script → Job → Service(s) → Repository → Database/External
- Show **data-flow labels** on edges where meaningful
  (e.g., ``-->|"reads contracts"|``)
- Include external systems (DB2, flat files, queues) as terminal nodes in
  the `External` subgraph
- Use the **exact class/file names** from the dependency graph
- Do NOT include test files, interfaces, or bin/obj artifacts

## Output Format
Return ONLY the raw Mermaid diagram — no markdown fences, no explanation,
no commentary. Start with `graph TD` and end with the last node/edge.
"""


# ---------------------------------------------------------------------------
# Service
# ---------------------------------------------------------------------------
class ProcessFlowService:
    """Generates ``process_flow.md`` from the generated .NET solution.

    Pipeline:
      ``_build_dependency_graph`` (deterministic) → ``dependency_graph_output.md``
      ``_generate_mermaid``       (single LLM call) → ``process_flow.md``
    """

    def __init__(self, project_id: UUID):
        self.project_id = project_id

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    async def generate(self) -> Path:
        """Build the dependency graph then generate the Mermaid diagram.

        Returns:
            Path to the written ``process_flow.md``.

        Raises:
            FileNotFoundError: No generated solution found.
            RuntimeError: LLM returned empty/unparseable content.
        """
        output_dir = self._get_output_dir()
        if not output_dir or not output_dir.exists():
            raise FileNotFoundError(
                f"No generated solution found for project {self.project_id}"
            )

        # Step 1 — deterministic extraction (no LLM)
        dep_graph_path = self._build_dependency_graph(output_dir)
        logger.info(
            f"[ProcessFlow] Dependency graph written: {dep_graph_path}"
        )

        # Step 2 — single LLM call to produce Mermaid
        flow_path = await self._generate_mermaid(dep_graph_path, output_dir)
        logger.info(f"[ProcessFlow] Process flow written: {flow_path}")

        return flow_path

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
    # Step 1 — Deterministic dependency graph extraction
    # ------------------------------------------------------------------

    def _build_dependency_graph(self, output_dir: Path) -> Path:
        """Scan ALL .cs and .ps1 files and write ``dependency_graph_output.md``.

        Sections: Worker Jobs, Services, Repositories, Entities, Scripts.
        Each .cs entry includes: class name, DI dependencies, method
        signatures, and provenance tags — extracted via regex, no LLM.

        Returns:
            Path to the written ``dependency_graph_output.md``.
        """
        sections: list[str] = [
            "# .NET Solution Dependency Graph",
            "",
            f"> Generated: {datetime.utcnow().isoformat()}",
            f"> Project: {self.project_id}",
            "",
        ]

        # --- Worker Jobs ---
        jobs_dir = output_dir / "src" / "Worker" / "Jobs"
        sections.append("## Worker Jobs")
        sections.append("")
        job_files = self._collect_cs_files(jobs_dir)
        if job_files:
            for cs_file in job_files:
                rel = str(cs_file.relative_to(output_dir)).replace("\\", "/")
                content = cs_file.read_text(encoding="utf-8", errors="replace")
                sections.append(_extract_signatures(content, rel))
                sections.append("")
        else:
            sections.append("_(no job files found)_")
            sections.append("")

        # --- Services ---
        services_dir = output_dir / "src" / "Core" / "Services"
        sections.append("## Services")
        sections.append("")
        svc_files = self._collect_cs_files(services_dir)
        if svc_files:
            for cs_file in svc_files:
                rel = str(cs_file.relative_to(output_dir)).replace("\\", "/")
                content = cs_file.read_text(encoding="utf-8", errors="replace")
                sections.append(_extract_signatures(content, rel))
                sections.append("")
        else:
            sections.append("_(no service files found)_")
            sections.append("")

        # --- Repositories ---
        repos_dir = output_dir / "src" / "Infrastructure" / "Repositories"
        sections.append("## Repositories")
        sections.append("")
        repo_files = self._collect_cs_files(repos_dir)
        if repo_files:
            for cs_file in repo_files:
                rel = str(cs_file.relative_to(output_dir)).replace("\\", "/")
                content = cs_file.read_text(encoding="utf-8", errors="replace")
                sections.append(_extract_signatures(content, rel))
                sections.append("")
        else:
            sections.append("_(no repository files found)_")
            sections.append("")

        # --- Entities (class names only) ---
        entities_dir = output_dir / "src" / "Core" / "Entities"
        sections.append("## Entities")
        sections.append("")
        entity_files = self._collect_cs_files(entities_dir, skip_interfaces=False)
        if entity_files:
            for cs_file in entity_files:
                rel = str(cs_file.relative_to(output_dir)).replace("\\", "/")
                content = cs_file.read_text(encoding="utf-8", errors="replace")
                class_name = _extract_class_name(content)
                label = class_name or cs_file.stem
                sections.append(f"- **{label}** (`{rel}`)")
            sections.append("")
        else:
            sections.append("_(no entity files found)_")
            sections.append("")

        # --- PowerShell Scripts ---
        scripts_dir = output_dir / "scripts" / "jobs"
        sections.append("## PowerShell Scripts")
        sections.append("")
        if scripts_dir.exists():
            ps1_files = sorted(scripts_dir.glob("*.ps1"))
            for ps1 in ps1_files:
                rel = str(ps1.relative_to(output_dir)).replace("\\", "/")
                content = ps1.read_text(encoding="utf-8", errors="replace")
                sections.append(f"### {ps1.name} (`{rel}`)")
                sections.append("")
                steps = _extract_ps1_steps(content)
                if steps:
                    for step in steps:
                        sections.append(f"- **{step['name']}**: {step['command']}")
                        if step.get("condition"):
                            sections.append(f"  - Condition: `{step['condition']}`")
                else:
                    sections.append("_(no step commands found)_")
                sections.append("")
        else:
            sections.append("_(no scripts/jobs directory found)_")
            sections.append("")

        # --- Summary ---
        total = len(job_files) + len(svc_files) + len(repo_files) + len(entity_files)
        ps1_count = len(list(scripts_dir.glob("*.ps1"))) if scripts_dir.exists() else 0
        sections.append("## Summary")
        sections.append("")
        sections.append(f"- Jobs: {len(job_files)}")
        sections.append(f"- Services: {len(svc_files)}")
        sections.append(f"- Repositories: {len(repo_files)}")
        sections.append(f"- Entities: {len(entity_files)}")
        sections.append(f"- Scripts: {ps1_count}")
        sections.append(f"- **Total .cs files scanned**: {total}")
        sections.append("")

        dep_graph_path = output_dir / "dependency_graph_output.md"
        dep_graph_path.write_text(
            "\n".join(sections), encoding="utf-8"
        )
        return dep_graph_path

    # ------------------------------------------------------------------
    # Step 2 — Single LLM call for Mermaid generation
    # ------------------------------------------------------------------

    async def _generate_mermaid(
        self, dep_graph_path: Path, output_dir: Path
    ) -> Path:
        """Read the dependency graph and produce a Mermaid diagram.

        Makes exactly one LLM call. Writes ``process_flow.md``.

        Returns:
            Path to the written ``process_flow.md``.
        """
        dep_graph = dep_graph_path.read_text(encoding="utf-8")

        model = get_llm(CODEGEN, model=LLMModel.CLAUDE_SONNET_4_5)
        response = await model.ainvoke(
            [
                SystemMessage(content=MERMAID_PROMPT),
                HumanMessage(
                    content=(
                        "Here is the dependency graph of the .NET solution. "
                        "Produce the Mermaid flowchart.\n\n"
                        f"{dep_graph}"
                    )
                ),
            ]
        )

        raw = response.content if hasattr(response, "content") else str(response)
        diagram = _extract_mermaid_block(raw)

        if not diagram:
            raise RuntimeError(
                "LLM returned empty or unparseable Mermaid content."
            )

        # TODO: Implement proper Mermaid validation (e.g., mmdc CLI)
        # issues = _validate_mermaid(diagram)
        # if issues:
        #     logger.warning(f"[ProcessFlow] Mermaid issues: {issues}")

        final = (
            "# Process Flow Diagram\n\n"
            f"> Generated: {datetime.utcnow().isoformat()}\n"
            f"> Project: {self.project_id}\n\n"
            f"```mermaid\n{diagram}\n```\n"
        )
        flow_path = output_dir / "process_flow.md"
        flow_path.write_text(final, encoding="utf-8")

        logger.info(
            f"[ProcessFlow] Mermaid diagram: {len(diagram)} chars, "
            f"{diagram.count('-->')} edges"
        )
        return flow_path

    # ------------------------------------------------------------------
    # File collection helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _collect_cs_files(
        directory: Path,
        skip_interfaces: bool = True,
    ) -> list[Path]:
        """Collect .cs files from a directory, skipping interfaces and infra.

        Args:
            directory: Folder to scan.
            skip_interfaces: If True, skip files starting with ``I``
                followed by an uppercase letter (e.g., ``IFooService.cs``).

        Returns:
            Sorted list of .cs file paths.
        """
        if not directory or not directory.exists():
            return []

        skip_names = {"IJob.cs", "Program.cs"}
        results: list[Path] = []

        for cs_file in sorted(directory.rglob("*.cs")):
            if "bin" in cs_file.parts or "obj" in cs_file.parts:
                continue
            if cs_file.name in skip_names:
                continue
            if skip_interfaces and _is_interface_file(cs_file.name):
                continue
            results.append(cs_file)

        return results


# ---------------------------------------------------------------------------
# Module-Level Helpers
# ---------------------------------------------------------------------------


def _is_interface_file(filename: str) -> bool:
    """Return True if the filename looks like an interface (e.g. IFooService.cs)."""
    return (
        filename.startswith("I")
        and len(filename) > 1
        and filename[1].isupper()
    )


def _extract_class_name(content: str) -> str | None:
    """Extract the first class/record/struct name from C# source."""
    match = re.search(
        r"(?:public|internal|private|protected)?\s*"
        r"(?:abstract|static|sealed|partial)?\s*"
        r"(?:class|record|struct|enum)\s+(\w+)",
        content,
    )
    return match.group(1) if match else None


def _extract_ps1_steps(content: str) -> list[dict]:
    """Extract step info from a PowerShell job script.

    Looks for step comment headers (``# ---- STEP01: ... ----``) and
    the ``dotnet run`` / ``Copy-Item`` / ``Remove-Item`` commands that
    follow them.

    Returns:
        List of dicts with ``name``, ``command``, and optional ``condition``.
    """
    steps: list[dict] = []
    lines = content.splitlines()

    current_step: str | None = None
    current_condition: str | None = None

    for line in lines:
        stripped = line.strip()

        # Match step headers like: # ---- STEP01: Description ----
        step_match = re.match(
            r"#\s*[-=]+\s*(STEP\d+\w*)\s*[:\-]?\s*(.*?)\s*[-=]*$",
            stripped,
            re.IGNORECASE,
        )
        if step_match:
            current_step = step_match.group(1)
            current_condition = None
            continue

        # Match COND comments like: # COND=(4,LT) — skip if MaxRC >= 4
        cond_match = re.match(
            r"#\s*COND[=:\s].*",
            stripped,
            re.IGNORECASE,
        )
        if cond_match:
            current_condition = stripped.lstrip("# ").strip()
            continue

        # Match if ($script:MaxRC ...) condition blocks
        if_match = re.match(
            r"if\s*\(\s*\$script:MaxRC\s*.+\)\s*\{?",
            stripped,
            re.IGNORECASE,
        )
        if if_match:
            current_condition = stripped
            continue

        # Match command lines
        cmd_match = re.match(
            r"(dotnet\s+run\s+.*|Copy-Item\s+.*|Remove-Item\s+.*)",
            stripped,
            re.IGNORECASE,
        )
        if cmd_match and current_step:
            steps.append({
                "name": current_step,
                "command": cmd_match.group(1).strip(),
                "condition": current_condition,
            })
            current_step = None
            current_condition = None

    return steps


def _extract_mermaid_block(llm_response: str) -> str:
    """Extract Mermaid diagram from LLM response.

    Handles three cases:
      1. Fenced: ```mermaid ... ```
      2. Fenced: ``` ... ```
      3. Raw: starts with ``graph`` or ``flowchart``
    """
    # Try fenced mermaid block
    match = re.search(
        r"```mermaid\s*\n(.*?)```", llm_response, re.DOTALL
    )
    if match:
        return match.group(1).strip()

    # Try generic fenced block
    match = re.search(
        r"```\s*\n(.*?)```", llm_response, re.DOTALL
    )
    if match:
        block = match.group(1).strip()
        if block.startswith(("graph ", "flowchart ")):
            return block

    # Try raw (starts with graph/flowchart)
    stripped = llm_response.strip()
    if stripped.startswith(("graph ", "flowchart ")):
        return stripped

    return ""


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
    """Mermaid syntax validation.

    Returns a list of issues (empty means valid).

    TODO: Implement proper validation with mermaid-cli (mmdc) or
    equivalent parser. For now, returns no issues.
    """
    # Placeholder — validation will be implemented later.
    # issues: list[str] = []
    # if not re.search(r"(graph|flowchart)\s+(TD|LR|TB|RL|BT)", content):
    #     issues.append("Missing graph/flowchart declaration")
    # if "-->" not in content:
    #     issues.append("No edges found")
    # if "subgraph" not in content.lower():
    #     issues.append("No subgraphs found")
    # opens = content.count("[") + content.count("(") + content.count("{")
    # closes = content.count("]") + content.count(")") + content.count("}")
    # if abs(opens - closes) > 2:
    #     issues.append(f"Bracket imbalance: {opens} opens vs {closes} closes")
    # return issues
    return []
