"""LangGraph-based Code Generation Agent.

Converts mainframe components (COBOL, copybooks, JCL) to .NET code.
Uses LangGraph StateGraph for agent execution with tool calling.
Includes context management: pruning, summarization, and history dumping.
"""

import json
import re
from datetime import datetime
from pathlib import Path
from typing import Literal, Annotated

from langchain.messages import AnyMessage, SystemMessage, ToolMessage, HumanMessage, AIMessage
from langgraph.graph import StateGraph, START, END
from loguru import logger
from typing_extensions import TypedDict

from app.config.llm import get_llm, CODEGEN, LLMModel
from app.config.settings import settings
from app.services.codegen_local.prompts import SYSTEM_PROMPT

# Summarization configuration
SUMMARIZE_THRESHOLD = 60  # Trigger summarization at this message count
KEEP_RECENT_MESSAGES = 15  # Keep last N messages after compression
PRUNE_AFTER_EXCHANGES = 4  # Prune tool outputs older than N exchanges (N*2 messages)

# Tools whose outputs should be pruned (large outputs)
PRUNEABLE_TOOLS = {"view_source_file", "read_artifact", "run_dotnet_build", "run_dotnet_test"}

# Special marker to signal message replacement
_RESET_MARKER = "__RESET_MESSAGES__"


def message_reducer(existing: list[AnyMessage], new: list[AnyMessage]) -> list[AnyMessage]:
    """
    Custom reducer for messages that supports both ADD and REPLACE.
    
    If new list starts with a string === _RESET_MARKER, replace existing with rest of new.
    Otherwise, append new to existing (normal behavior).
    """
    if existing is None:
        existing = []
    if not new:
        return existing
    
    # Check for reset marker
    if isinstance(new[0], str) and new[0] == _RESET_MARKER:
        # Replace mode: return everything after the marker
        logger.info(f"Message reducer: REPLACING {len(existing)} messages with {len(new) - 1}")
        return list(new[1:])
    
    # Normal add mode
    return existing + new


class CodegenState(TypedDict):
    """State for the code generation agent."""
    messages: Annotated[list[AnyMessage], message_reducer]
    project_id: str
    iteration_count: int
    codegen_logs_path: str  # Path for dumping session history
    codegen_output_path: str # Path to generated code for verification
    codegen_source_path: str # Path to source code for verification checks
    verification_attempts: int # Track number of verification runs
    verification_step_failures: dict  # Per-step failure counts for budgets


def _prune_old_tool_outputs(messages: list[AnyMessage]) -> list[AnyMessage]:
    """
    Replace large tool outputs from old messages with lightweight markers.
    Keeps recent outputs intact so LLM can act on them.
    """
    if len(messages) <= PRUNE_AFTER_EXCHANGES * 2:
        return messages
    
    # Messages to keep unpruned (last N exchanges)
    cutoff = len(messages) - (PRUNE_AFTER_EXCHANGES * 2)
    pruned = []
    
    for i, msg in enumerate(messages):
        if i >= cutoff:
            # Recent message - keep as is
            pruned.append(msg)
        elif isinstance(msg, ToolMessage):
            content = str(msg.content)
            # Check if this is a large output from pruneable tool
            if len(content) > 500:
                lines = content.count('\n')
                marker = f"[Pruned: {lines} lines. Re-call tool if needed.]"
                pruned.append(ToolMessage(content=marker, tool_call_id=msg.tool_call_id))
            else:
                pruned.append(msg)
        else:
            pruned.append(msg)
    
    return pruned


def _extract_structured_state(messages: list[AnyMessage]) -> str:
    """
    Extract progress state from message history deterministically.
    Parses tool call patterns to find converted components and files.
    """
    files_created = set()
    files_deleted = set()
    components_status = {}
    
    for msg in messages:
        if isinstance(msg, AIMessage) and msg.tool_calls:
            for tool_call in msg.tool_calls:
                name = tool_call["name"]
                args = tool_call["args"]
                
                if name == "write_code_file":
                    target = args.get("target_path") or args.get("target_file")
                    if target:
                        files_created.add(str(target))
                        files_deleted.discard(str(target))
                        
                elif name == "remove_file":
                    target = args.get("target_path") or args.get("target_file")
                    if target:
                        files_deleted.add(str(target))
                        files_created.discard(str(target))
                        
                elif name == "log_component_status":
                    comp = args.get("component_name")
                    status = args.get("status")
                    if comp and status:
                        components_status[comp] = status

    # Format summary
    summary_lines = ["### Current Project State"]
    
    if components_status:
        summary_lines.append("\n**Component Status:**")
        for comp, status in components_status.items():
            summary_lines.append(f"- {comp}: {status}")
            
    if files_created:
        summary_lines.append(f"\n**Files Created ({len(files_created)}):**")
        # Show top 10 most recent
        for f in sorted(list(files_created))[-10:]:
            summary_lines.append(f"- {Path(f).name}")
            
    return "\n".join(summary_lines)


def _extract_structured_state_from_disk(project_id: str, output_path: Path) -> str:
    """
    Extract progress state from actual filesystem instead of message history.
    More accurate than message parsing — survives compression without info loss.
    """
    summary_lines = ["### Current Project State (from disk scan)"]
    
    if not output_path.exists():
        summary_lines.append("\nNo output directory yet.")
        return "\n".join(summary_lines)
    
    # Count files by category
    categories = {
        "Entities": "src/Core/Entities",
        "Services": "src/Core/Services",
        "Interfaces": "src/Core/Interfaces",
        "Repositories": "src/Infrastructure/Repositories",
        "Jobs": "src/Worker/Jobs",
        "Tests": "tests",
        "Scripts": "scripts/jobs",
    }
    
    summary_lines.append("\n**Generated Files by Category:**")
    for label, rel_path in categories.items():
        dir_path = output_path / rel_path
        if dir_path.exists():
            cs_files = [f.name for f in dir_path.rglob("*.cs") if "bin" not in f.parts and "obj" not in f.parts]
            ps1_files = [f.name for f in dir_path.rglob("*.ps1")] if "scripts" in rel_path else []
            files = cs_files + ps1_files
            if files:
                summary_lines.append(f"- {label}: {len(files)} files ({', '.join(sorted(files)[:5])}{'...' if len(files) > 5 else ''})")
            else:
                summary_lines.append(f"- {label}: 0 files")
        else:
            summary_lines.append(f"- {label}: directory not created")
    
    # F-ID progress
    try:
        catalog = _parse_functionality_catalog(project_id)
        if catalog:
            _, missing = _scan_fid_coverage(output_path, catalog)
            total = len(catalog)
            done = total - len(missing)
            missing_ids = [m["id"] for m in missing]
            summary_lines.append(f"\n**Functionality Progress: {done}/{total} implemented**")
            if missing_ids:
                summary_lines.append(f"Missing: {', '.join(missing_ids[:10])}{'...' if len(missing_ids) > 10 else ''}")
    except Exception:
        pass
    
    return "\n".join(summary_lines)


# =========================================================================
# Verification Helper Functions
# =========================================================================

def _parse_functionality_catalog(project_id: str) -> list[dict]:
    """Parse the functionality catalog into structured entries.
    
    Returns list of dicts with keys: id, title, programs, description, domain
    Returns empty list if catalog doesn't exist.
    """
    artifacts_base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / project_id
    catalog_path = artifacts_base / "system_context" / "functionality_catalog.md"
    
    if not catalog_path.exists():
        logger.info("[Catalog] No functionality_catalog.md found")
        return []
    
    try:
        content = catalog_path.read_text(encoding="utf-8", errors="replace")
    except Exception as e:
        logger.warning(f"[Catalog] Failed to read catalog: {e}")
        return []
    
    entries = []
    # Split by ### F0XX headers
    blocks = re.split(r"(?=^### F\d{3}:)", content, flags=re.MULTILINE)
    
    for block in blocks:
        block = block.strip()
        if not block.startswith("### F"):
            continue
        
        # Extract F-ID and title
        header_match = re.match(r"### (F\d{3}): (.+)", block)
        if not header_match:
            continue
        
        fid = header_match.group(1)
        title = header_match.group(2).strip()
        
        # Extract domain
        domain_match = re.search(r"\*\*Domain\*\*:\s*(.+)", block)
        domain = domain_match.group(1).strip() if domain_match else ""
        
        # Extract implemented by (program names)
        impl_match = re.search(r"\*\*Implemented By\*\*:\s*(.+)", block)
        programs = []
        if impl_match:
            programs = [p.strip() for p in impl_match.group(1).split(",")]
        
        # Extract description
        desc_match = re.search(r"\*\*Description\*\*:\s*(.+)", block, re.DOTALL)
        description = ""
        if desc_match:
            # Take text until next --- or end of block
            desc_text = desc_match.group(1).strip()
            # Remove trailing --- separator
            desc_text = re.split(r"\n---", desc_text)[0].strip()
            description = desc_text
        
        entries.append({
            "id": fid,
            "title": title,
            "domain": domain,
            "programs": programs,
            "description": description,
        })
    
    logger.info(f"[Catalog] Parsed {len(entries)} functionalities")
    return entries


def _scan_fid_coverage(
    output_path: Path, catalog: list[dict]
) -> tuple[list[dict], list[dict]]:
    """Scan generated .cs files for F-ID implementation tags.
    
    Returns (implemented, missing) where each is a list of catalog entries
    enriched with 'found_in' for implemented entries.
    """
    if not output_path.exists():
        return [], list(catalog)
    
    # Build F-ID → file mapping by scanning all .cs files
    fid_locations: dict[str, list[str]] = {}
    
    for cs_file in output_path.rglob("*.cs"):
        if "bin" in cs_file.parts or "obj" in cs_file.parts:
            continue
        try:
            content = cs_file.read_text(encoding="utf-8", errors="replace")
            rel_path = str(cs_file.relative_to(output_path)).replace("\\", "/")
            # Only match F-IDs inside "// Implements:" comment lines (not arbitrary text)
            for impl_match in re.finditer(r"//\s*Implements:\s*(.+)", content):
                for fid in re.findall(r"F\d{3}", impl_match.group(1)):
                    fid_locations.setdefault(fid, []).append(rel_path)
        except Exception:
            continue
    
    implemented = []
    missing = []
    
    for entry in catalog:
        fid = entry["id"]
        if fid in fid_locations:
            enriched = {**entry, "found_in": fid_locations[fid]}
            implemented.append(enriched)
        else:
            missing.append(entry)
    
    return implemented, missing


def _get_file_summary(project_id: str, program_name: str) -> str:
    """Get the file summary for a specific program from file_summaries.md.
    
    Args:
        project_id: Project ID
        program_name: Program name (e.g., 'FSFEE', 'FSMAIN')
    
    Returns:
        The summary section text, or empty string if not found.
    """
    artifacts_base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / project_id
    summaries_path = artifacts_base / "file_summaries.md"
    
    if not summaries_path.exists():
        return ""
    
    try:
        content = summaries_path.read_text(encoding="utf-8", errors="replace")
    except Exception:
        return ""
    
    # Find section for this program (case-insensitive)
    # Format: ### PROGRAM_ext.txt (TYPE)
    pattern = rf"(### {re.escape(program_name)}[\w_]*\.txt.*?)(?=\n### |\n## |\Z)"
    match = re.search(pattern, content, re.IGNORECASE | re.DOTALL)
    if match:
        section = match.group(1).strip()
        # Limit to ~500 chars to avoid bloating the message
        if len(section) > 500:
            section = section[:500] + "..."
        return section
    
    return ""


def _get_dependencies(project_id: str, program_name: str) -> str:
    """Get dependency information for a program from dependency_graph.md.
    
    Args:
        project_id: Project ID
        program_name: Program name (e.g., 'FSFEE')
    
    Returns:
        Formatted dependency string, or empty string if not found.
    """
    artifacts_base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / project_id
    dep_path = artifacts_base / "dependency_graph.md"
    
    if not dep_path.exists():
        return ""
    
    try:
        content = dep_path.read_text(encoding="utf-8", errors="replace")
    except Exception:
        return ""
    
    deps = {"calls": set(), "copybooks": set(), "tables": set()}
    name_upper = program_name.upper()
    
    for line in content.splitlines():
        # Look for table rows containing this program
        if f"| {name_upper}" in line.upper() or f"|{name_upper}" in line.upper():
            cells = [c.strip() for c in line.split("|") if c.strip()]
            if len(cells) >= 2:
                source = cells[0].upper().strip()
                target = cells[1].strip()
                
                if source == name_upper and target:
                    # Determine relationship type from surrounding context
                    if "CALL" in line.upper() or "→ Program" in content[max(0, content.find(line)-200):content.find(line)]:
                        deps["calls"].add(target)
                    elif "Copybook" in content[max(0, content.find(line)-200):content.find(line)]:
                        deps["copybooks"].add(target)
                    elif "Table" in content[max(0, content.find(line)-200):content.find(line)] or "SQL" in content[max(0, content.find(line)-200):content.find(line)]:
                        deps["tables"].add(target)
                    else:
                        deps["calls"].add(target)
    
    parts = []
    if deps["calls"]:
        parts.append(f"Calls: {', '.join(sorted(deps['calls']))}")
    if deps["copybooks"]:
        parts.append(f"Uses copybooks: {', '.join(sorted(deps['copybooks']))}")
    if deps["tables"]:
        parts.append(f"SQL tables: {', '.join(sorted(deps['tables']))}")
    
    return " | ".join(parts)


def _scan_placeholder_files(output_path: Path) -> list[dict]:
    """Scan generated source files for placeholder/stub patterns.
    
    Returns list of dicts with 'file', 'reason', and 'line_count' for each placeholder.
    """
    placeholders = []
    
    if not output_path.exists():
        return placeholders
    
    src_dir = output_path / "src"
    if not src_dir.exists():
        return placeholders
    
    for cs_file in src_dir.rglob("*.cs"):
        if "bin" in cs_file.parts or "obj" in cs_file.parts:
            continue
        # Skip interfaces and IJob.cs
        if cs_file.name.startswith("I") and cs_file.name[1].isupper():
            continue
        if cs_file.name == "Program.cs":
            continue
            
        try:
            content = cs_file.read_text(encoding="utf-8", errors="replace")
            lines = content.splitlines()
            line_count = len(lines)
            rel_path = str(cs_file.relative_to(output_path)).replace("\\", "/")
            
            # Check line count thresholds
            is_service = "Services" in rel_path
            is_job = "Jobs" in rel_path
            
            if is_service and line_count < 25:
                placeholders.append({
                    "file": rel_path,
                    "reason": f"Service file is suspiciously short ({line_count} lines)",
                    "line_count": line_count,
                })
                continue
            
            if is_job and line_count < 20:
                placeholders.append({
                    "file": rel_path,
                    "reason": f"Job file is suspiciously short ({line_count} lines)",
                    "line_count": line_count,
                })
                continue
            
            # Check if ALL methods are just NotImplementedException
            method_bodies = re.findall(
                r"\{[^{}]*\}", content, re.DOTALL
            )
            all_nie = True
            has_methods = False
            for body in method_bodies:
                body_stripped = body.strip().strip("{}")
                if not body_stripped.strip():
                    continue
                has_methods = True
                # Check if body is ONLY NotImplementedException or return default
                body_clean = body_stripped.strip()
                if not re.match(
                    r"^\s*(throw\s+new\s+NotImplementedException\s*\(\s*\)\s*;|return\s+(default|0|Task\.FromResult\s*\(\s*0\s*\))\s*;)\s*$",
                    body_clean, re.DOTALL
                ):
                    all_nie = False
                    break
            
            if has_methods and all_nie:
                placeholders.append({
                    "file": rel_path,
                    "reason": "All method bodies are NotImplementedException or return default — no real logic",
                    "line_count": line_count,
                })
                continue
            
        except Exception:
            continue
    
    return placeholders


def _dump_history(messages: list[AnyMessage], logs_path: Path) -> str:
    """
    Dump full conversation history to JSON file for recovery.
    Returns the filename of the created dump.
    """
    logs_path.mkdir(parents=True, exist_ok=True)
    
    # Find next session number
    existing = list(logs_path.glob("session_*.json"))
    counter = len(existing) + 1
    
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"session_{counter:03d}_{timestamp}.json"
    filepath = logs_path / filename
    
    # Serialize messages
    serialized = []
    for msg in messages:
        entry = {
            "role": msg.__class__.__name__.replace("Message", "").lower(),
            "content": str(getattr(msg, 'content', ''))
        }
        if hasattr(msg, 'tool_calls') and msg.tool_calls:
            entry["tool_calls"] = [
                {"name": tc["name"], "args": tc.get("args", {})}
                for tc in msg.tool_calls
            ]
        if hasattr(msg, 'tool_call_id'):
            entry["tool_call_id"] = msg.tool_call_id
        serialized.append(entry)
    
    dump = {
        "timestamp": datetime.now().isoformat(),
        "message_count": len(messages),
        "messages": serialized
    }
    
    try:
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(dump, f, indent=2, ensure_ascii=False)
        logger.info(f"Dumped session history to {filename}")
    except Exception as e:
        logger.warning(f"Failed to dump session history: {e}")
    
    return filename


def _compress_messages(
    messages: list[AnyMessage],
    logs_path: Path,
    project_id: str = "",
    output_path: str = "",
) -> list[AnyMessage]:
    """
    Compress message history by:
    1. Dumping full history to file
    2. Extracting structured state from messages AND disk
    3. Replacing old messages with summary + recent messages
    
    IMPORTANT: We must not split AIMessage/ToolMessage pairs.
    ToolMessages must have their corresponding AIMessage with tool_calls.
    """
    # Dump before compressing
    _dump_history(messages, logs_path)
    
    # Extract state summary from messages
    summary = _extract_structured_state(messages)
    
    # Enrich with disk-based state if available (more reliable than message parsing)
    if project_id and output_path:
        disk_state = _extract_structured_state_from_disk(project_id, Path(output_path))
        if disk_state:
            summary += "\n\n" + disk_state
    
    # Find a clean cut point - walk backward from target position
    # to find a message that's NOT a ToolMessage (which would need its AIMessage)
    target_keep = KEEP_RECENT_MESSAGES
    
    if len(messages) <= target_keep:
        # No compression needed
        return messages
    
    # Start from target position
    cut_idx = len(messages) - target_keep
    
    # Walk backward until we find a clean boundary (HumanMessage or AIMessage without pending tool calls)
    while cut_idx < len(messages):
        msg = messages[cut_idx]
        
        # ToolMessage needs its AIMessage - keep looking
        if isinstance(msg, ToolMessage):
            cut_idx += 1
            continue
        
        # Check if this is an AIMessage with tool_calls whose ToolMessages we'd cut
        if hasattr(msg, 'tool_calls') and msg.tool_calls:
            # Need to keep this AND its ToolMessages
            # Check if next messages are ToolMessages for these calls
            tool_call_ids = {tc["id"] for tc in msg.tool_calls}
            next_idx = cut_idx + 1
            while next_idx < len(messages):
                next_msg = messages[next_idx]
                if isinstance(next_msg, ToolMessage) and next_msg.tool_call_id in tool_call_ids:
                    tool_call_ids.discard(next_msg.tool_call_id)
                    next_idx += 1
                else:
                    break
            # If we haven't collected all ToolMessages, skip this AIMessage
            if tool_call_ids:
                cut_idx += 1
                continue
        
        # Clean boundary found
        break
    
    # Take from cut_idx onwards
    recent = messages[cut_idx:]
    
    # Preserve the first HumanMessage (contains source manifest + project context)
    # so it survives every compression cycle.
    first_human = None
    for msg in messages:
        if isinstance(msg, HumanMessage):
            first_human = msg
            break

    if first_human is not None and first_human not in recent:
        compressed = [first_human, HumanMessage(content=summary)] + list(recent)
    else:
        compressed = [HumanMessage(content=summary)] + list(recent)
    
    logger.info(f"Compressed {len(messages)} messages to {len(compressed)} (cut at index {cut_idx})")
    return compressed


def create_codegen_agent(tools: list, project_id: str):
    """Create the Code Generation LangGraph agent.
    
    Args:
        tools: List of tools to bind
        project_id: Project ID for scoping operations
        
    Returns:
        Compiled LangGraph agent
    """
    model = get_llm(CODEGEN, model=LLMModel.CLAUDE_SONNET_4_5)
    
    tools_by_name = {tool.name: tool for tool in tools}
    model_with_tools = model.bind_tools(tools)
    


    def llm_call(state: CodegenState) -> dict:
        """LLM decides whether to call a tool or respond."""
        messages = state["messages"]
        logs_path = Path(state.get("codegen_logs_path", ""))
        should_reset = False
        
        # Only prune and compress if summarization is enabled
        if settings.CODEGEN_ENABLE_SUMMARIZATION:
            # Step 1: Prune old tool outputs
            messages = _prune_old_tool_outputs(messages)
            
            # Step 2: Compress if above threshold
            if len(messages) > SUMMARIZE_THRESHOLD and logs_path:
                messages = _compress_messages(
                    messages, logs_path,
                    project_id=state.get("project_id", ""),
                    output_path=state.get("codegen_output_path", ""),
                )
                should_reset = True  # Need to replace state, not add
        
        # Invoke LLM with system prompt + processed messages
        
        system_instructions = [SystemMessage(content=SYSTEM_PROMPT)]
        
        # Conditionally add memory management instructions
        if settings.CODEGEN_ENABLE_SUMMARIZATION:
            memory_note = """
## Memory Management (System Automated)
- The system will automatically prune large tool outputs (like file reads) to save context.
- The system will summarize older history periodically.
- **DO NOT** try to summarize history yourself. 
- Rely on `read_conversion_status()` to know exactly what is done, rather than scrolling back in chat history.
- If you need details about a previously converted component that is no longer in chat history, use `search_session_logs(query)`.
"""
            system_instructions.append(SystemMessage(content=memory_note))

        full_messages = system_instructions + messages
        response = model_with_tools.invoke(full_messages)
        
        if should_reset:
            # Add marker to signal _reset_ needed
            return {
                "messages": [_RESET_MARKER] + messages + [response],
                "iteration_count": state.get("iteration_count", 0) + 1,
            }
        else:
            # Normal add mode
            return {
                "messages": [response],
                "iteration_count": state.get("iteration_count", 0) + 1,
            }
    
    def tool_node(state: CodegenState) -> dict:
        """Execute tool calls."""
        result = []
        last_message = state["messages"][-1]
        
        for tool_call in last_message.tool_calls:
            tool = tools_by_name.get(tool_call["name"])
            if not tool:
                observation = f"Error: Unknown tool '{tool_call['name']}'"
            else:
                try:
                    observation = tool.invoke(tool_call["args"])
                except Exception as e:
                    observation = f"Error: {e}"
                    logger.error(f"Tool {tool_call['name']} failed: {e}")
            
            result.append(
                ToolMessage(content=str(observation), tool_call_id=tool_call["id"])
            )
        
        return {"messages": result}
    
    def should_continue(state: CodegenState) -> Literal["tool_node", "verify_completion"]:
        """Decide if we should run tools or finish."""
        last_message = state["messages"][-1]
        
        # If LLM called tools, run them
        if hasattr(last_message, "tool_calls") and last_message.tool_calls:
            return "tool_node"
        
        # Otherwise, check if we are done (verify)
        return "verify_completion"

    def verify_completion(state: CodegenState) -> dict:
        """Verify that the generated solution is complete, correct, and high-quality.

        Runs structured verification steps with clear logging for each.

        Strategy:
          - Steps 0-5 (structural): BATCH — accumulate ALL failures, return them
            together so the agent can fix multiple issues in one pass.
          - Steps 6-7 (build/test): PER-STEP BUDGET — each step gets up to
            MAX_STEP_BUDGET failures before the system stops retrying that step.
            Actual compiler/test error output is included inline so the agent
            does not need to call build/test tools separately.

        Step order (cheapest/most impactful first):
          0. Output directory exists
          1. F-ID Catalog Coverage (progress gate)
          2. Placeholder/Stub Quality Scan
          3. JCL → PowerShell scripts existence
          4. 1:1 Job Mapping (.ps1 ↔ Worker Class)
          5. Test Existence
          6. dotnet build  (budget: MAX_STEP_BUDGET)
          7. dotnet test   (budget: MAX_STEP_BUDGET)
        """
        import subprocess

        MAX_STEP_BUDGET = 20  # max failures per build/test step before giving up

        output_path_str = state.get("codegen_output_path", "")
        source_path_str = state.get("codegen_source_path", "")
        pid = state.get("project_id", "")

        # Increment verification attempts
        current_attempts = state.get("verification_attempts", 0) + 1
        step_failures: dict = dict(state.get("verification_step_failures", {}))
        verification_update = {
            "verification_attempts": current_attempts,
            "verification_step_failures": step_failures,
        }

        # Hard cap: prevent infinite loops on genuinely unsolvable issues
        MAX_VERIFICATION_ATTEMPTS = 100_000
        if current_attempts > MAX_VERIFICATION_ATTEMPTS:
            logger.warning(f"[Verification] Hit max attempts ({MAX_VERIFICATION_ATTEMPTS}). Proceeding with current state.")
            return {"messages": [], **verification_update}

        logger.info("[Verification] ═══════════════════════════════════════")
        logger.info(f"[Verification] Starting verification check #{current_attempts}")
        logger.info(f"[Verification] Output: {output_path_str}")
        logger.info(f"[Verification] Source: {source_path_str}")
        logger.info("[Verification] ═══════════════════════════════════════")

        # Collects failures across Steps 0-5 for batched return
        batch_failures: list[str] = []

        def _fmt_batch(all_failures: list[str]) -> dict:
            """Return a single HumanMessage containing ALL batched failures."""
            logger.warning(f"[Verification] ═══ BATCH FAILED — {len(all_failures)} issue(s) across Steps 0-5 ═══")
            for f in all_failures:
                logger.warning(f"[Verification]   • {f}")
            return {
                "messages": [
                    HumanMessage(
                        content=f"CRITICAL: Verification (Attempt {current_attempts}) found {len(all_failures)} issues:\n" +
                                "\n".join([f"- {f}" for f in all_failures]) +
                                "\n\nYou MUST fix ALL of these issues before you can finish. "
                                "If a file is a placeholder, call view_source_file() on the original mainframe source and implement real logic. "
                                "Do NOT write stubs."
                    )
                ],
                **verification_update,
            }

        def _fmt_step(step_name: str, step_failures_list: list[str]) -> dict:
            """Return a HumanMessage for a single build/test step failure."""
            logger.warning(f"[Verification] ═══ FAILED at {step_name} — {len(step_failures_list)} issue(s) found ═══")
            for f in step_failures_list:
                logger.warning(f"[Verification]   • {f}")
            return {
                "messages": [
                    HumanMessage(
                        content=f"CRITICAL: Verification (Attempt {current_attempts}) — {step_name} found issues:\n" +
                                "\n".join([f"- {f}" for f in step_failures_list]) +
                                "\n\nFix these issues. The actual error output is shown above — "
                                "read it carefully and correct the code."
                    )
                ],
                **verification_update,
            }

        # === STEP 0: Output directory exists ===
        logger.info("[Verification Step 0] Checking output directory exists...")
        if not output_path_str:
            # Fatal — can't continue without output dir
            return _fmt_batch(["CRITICAL: No codegen_output_path in state. Cannot verify."])

        output_path = Path(output_path_str)
        if not output_path.exists():
            return _fmt_batch(["CRITICAL: Output directory does not exist. You cannot be done."])
        
        logger.info("[Verification Step 0] PASS — Output directory exists")

        # =================================================================
        # STEP 1: F-ID Catalog Coverage (PROGRESS GATE)
        # =================================================================
        logger.info("[Verification Step 1] Checking Functionality Catalog coverage...")
        catalog = _parse_functionality_catalog(pid)

        if catalog:
            implemented, missing = _scan_fid_coverage(output_path, catalog)
            total = len(catalog)
            done = len(implemented)
            logger.info(f"[Verification Step 1] Coverage: {done}/{total} functionalities implemented")

            if missing:
                fail_lines = [
                    f"[Step 1 — Functionality Coverage] INCOMPLETE: {done} of {total} implemented. "
                    f"{len(missing)} more to go."
                ]

                for entry in missing[:2]:
                    fid = entry["id"]
                    title = entry["title"]
                    programs = ", ".join(entry["programs"])
                    desc = entry["description"][:200] + ("..." if len(entry["description"]) > 200 else "")

                    detail = f"\n**Next: {fid} — {title}**"
                    detail += f"\n  Source programs: {programs}"
                    detail += f"\n  Description: {desc}"

                    for prog in entry["programs"][:1]:
                        summary = _get_file_summary(pid, prog)
                        if summary:
                            detail += f"\n  File Summary:\n  {summary[:300]}"

                    for prog in entry["programs"][:1]:
                        deps = _get_dependencies(pid, prog)
                        if deps:
                            detail += f"\n  Dependencies: {deps}"

                    detail += "\n  Action: Call view_source_file() for the source program(s) and implement this functionality."
                    fail_lines.append(detail)

                if len(missing) > 2:
                    remaining_ids = [m["id"] + ": " + m["title"] for m in missing[2:]]
                    fail_lines.append(f"\nAlso remaining: {', '.join(remaining_ids)}")

                logger.warning(f"[Verification Step 1] FAIL — {len(missing)} functionalities missing")
                batch_failures.extend(fail_lines)
            else:
                logger.info("[Verification Step 1] PASS — All functionalities tagged")
        else:
            logger.info("[Verification Step 1] SKIP — No functionality catalog found")

        # =================================================================
        # STEP 2: Placeholder / Stub Quality Scan
        # =================================================================
        logger.info("[Verification Step 2] Scanning for placeholder/stub files...")
        placeholders = _scan_placeholder_files(output_path)

        if placeholders:
            batch_failures.append(
                f"[Step 2 — Quality Scan] {len(placeholders)} file(s) are placeholders with no real implementation."
            )
            for ph in placeholders[:5]:
                batch_failures.append(
                    f"  • {ph['file']} ({ph['line_count']} lines): {ph['reason']}\n"
                    f"    → Read the original source with view_source_file() and implement real logic."
                )
            if len(placeholders) > 5:
                batch_failures.append(f"  ... and {len(placeholders) - 5} more placeholder files.")
            logger.warning(f"[Verification Step 2] FAIL — {len(placeholders)} placeholders found")
        else:
            logger.info("[Verification Step 2] PASS — No placeholders detected")

        # =================================================================
        # STEP 3: JCL → PowerShell scripts exist
        # =================================================================
        logger.info("[Verification Step 3] Checking JCL → PowerShell scripts...")
        scripts_dir = output_path / "scripts" / "jobs"
        step3_failures: list[str] = []

        if not scripts_dir.exists() or not any(scripts_dir.iterdir()):
            logger.warning("[Verification Step 3] FAIL — scripts/jobs is empty or missing")
            step3_failures.append("scripts/jobs directory is empty or missing. You MUST convert JCL jobs to PowerShell scripts.")
        else:
            if source_path_str:
                source_path = Path(source_path_str)
                if source_path.exists():
                    jcl_files = list(source_path.rglob("*.jcl"))
                    if jcl_files:
                        logger.info(f"[Verification Step 3] Found {len(jcl_files)} JCL source files")
                        for jcl_file in jcl_files:
                            job_name = jcl_file.stem.lower()
                            expected_script = scripts_dir / f"run-{job_name}.ps1"
                            if not expected_script.exists():
                                step3_failures.append(f"Missing Script: run-{job_name}.ps1 (from {jcl_file.name}). Read {jcl_file.name} with view_source_file() first.")
                            else:
                                try:
                                    ps_content = expected_script.read_text(encoding="utf-8", errors="replace")
                                    has_step_cmd = bool(re.search(r"dotnet run|Copy-Item|Remove-Item", ps_content))
                                    if not has_step_cmd and len(ps_content.splitlines()) < 8:
                                        step3_failures.append(
                                            f"Stub Script: run-{job_name}.ps1 has no step commands (no 'dotnet run', 'Copy-Item', etc.). "
                                            f"Read {jcl_file.name} with view_source_file() and convert its actual steps."
                                        )
                                except Exception:
                                    pass

            if not step3_failures:
                logger.info("[Verification Step 3] PASS — All JCL scripts present and substantive")

        if step3_failures:
            logger.warning(f"[Verification Step 3] FAIL — {len(step3_failures)} issue(s)")
            batch_failures.append(f"[Step 3 — JCL Scripts] {len(step3_failures)} issue(s):")
            batch_failures.extend(step3_failures)

        # =================================================================
        # STEP 4: 1:1 Job Mapping (.ps1 ↔ Worker Class)
        # =================================================================
        logger.info("[Verification Step 4] Checking 1:1 Job Mapping (.ps1 ↔ Worker logic)...")
        worker_jobs_dir = output_path / "src" / "Worker" / "Jobs"
        step4_failures: list[str] = []

        ps1_jobs = set()
        if scripts_dir.exists():
            for f in scripts_dir.glob("run-*.ps1"):
                job_name = f.stem[4:].lower()
                ps1_jobs.add(job_name)

        worker_classes = set()
        if worker_jobs_dir.exists():
            for f in worker_jobs_dir.glob("*.cs"):
                if f.name == "IJob.cs":
                    continue
                worker_classes.add(f.stem.lower())

        for job in ps1_jobs:
            if job not in worker_classes:
                step4_failures.append(f"Missing Worker Class: {job.title()}.cs (for run-{job}.ps1)")

        for cls in worker_classes:
            if cls not in ps1_jobs:
                step4_failures.append(f"Missing Script: run-{cls}.ps1 (for {cls.title()}.cs)")

        if step4_failures:
            logger.warning(f"[Verification Step 4] FAIL — {len(step4_failures)} mapping issue(s)")
            batch_failures.append(f"[Step 4 — Job Mapping] {len(step4_failures)} issue(s):")
            batch_failures.extend(step4_failures)
        else:
            logger.info("[Verification Step 4] PASS — All jobs mapped 1:1")

        # =================================================================
        # STEP 5: Test Existence (rule-based — deterministic, no LLM call)
        # =================================================================
        logger.info("[Verification Step 5] Checking test file existence...")
        step5_failures: list[str] = []

        test_mapping = [
            ("src/Core/Services", "tests/Core.Tests/Services"),
            ("src/Infrastructure/Repositories", "tests/Infrastructure.Tests/Repositories"),
            ("src/Worker/Jobs", "tests/Worker.Tests/Jobs"),
        ]

        skip_files = {"IJob.cs", "Program.cs"}

        for src_rel, test_rel in test_mapping:
            src_dir = output_path / src_rel
            if not src_dir.exists():
                continue
            for cs_file in src_dir.glob("*.cs"):
                if cs_file.name in skip_files:
                    continue
                if cs_file.name.startswith("I") and len(cs_file.name) > 1 and cs_file.name[1].isupper():
                    continue

                expected_test = output_path / test_rel / (cs_file.stem + "Tests.cs")
                if not expected_test.exists():
                    step5_failures.append(
                        f"Missing Test: {test_rel}/{cs_file.stem}Tests.cs (for {src_rel}/{cs_file.name})"
                    )

        if step5_failures:
            logger.warning(f"[Verification Step 5] FAIL — {len(step5_failures)} missing test files")
            batch_failures.append(f"[Step 5 — Test Existence] {len(step5_failures)} missing test file(s):")
            batch_failures.extend(step5_failures)
        else:
            logger.info("[Verification Step 5] PASS — All logic files have tests")

        # -----------------------------------------------------------------
        # Return ALL batched failures from Steps 0-5 at once
        # -----------------------------------------------------------------
        if batch_failures:
            return _fmt_batch(batch_failures)

        # =================================================================
        # STEP 6: Solution builds successfully (per-step budget)
        # =================================================================
        logger.info("[Verification Step 6] Checking dotnet build...")

        # Resolve sln files once for both Step 6 and Step 7
        sln_files = list(output_path.glob("*.sln"))

        build_fail_count = step_failures.get("build", 0)
        if build_fail_count >= MAX_STEP_BUDGET:
            logger.warning(f"[Verification Step 6] SKIP — build budget exhausted ({build_fail_count}/{MAX_STEP_BUDGET})")
        else:
            if not sln_files:
                logger.warning("[Verification Step 6] FAIL — No .sln file found")
                step_failures["build"] = build_fail_count + 1
                return _fmt_step("Step 6: Build", ["Missing .sln file — solution was never initialized"])
            else:
                try:
                    result = subprocess.run(
                        ["dotnet", "build", str(sln_files[0])],
                        cwd=str(output_path),
                        capture_output=True,
                        text=True,
                        timeout=120,
                    )
                    if result.returncode == 0:
                        logger.info("[Verification Step 6] PASS — Build succeeded")
                    else:
                        build_output = result.stdout + result.stderr
                        error_lines = [line for line in build_output.splitlines() if ": error " in line]
                        # Cap inline errors to avoid token explosion
                        shown_errors = error_lines[:30]
                        truncated_msg = f"\n... and {len(error_lines) - 30} more errors" if len(error_lines) > 30 else ""
                        logger.warning(f"[Verification Step 6] FAIL — Build failed with {len(error_lines)} errors")
                        step_failures["build"] = build_fail_count + 1
                        verification_update["verification_step_failures"] = step_failures
                        return _fmt_step("Step 6: Build", [
                            f"Build FAILED with {len(error_lines)} error(s) (attempt {build_fail_count + 1}/{MAX_STEP_BUDGET}).\n"
                            "Fix these errors by reading each message and correcting the code:\n" +
                            "\n".join(shown_errors) + truncated_msg
                        ])
                except Exception as e:
                    logger.warning(f"[Verification Step 6] SKIP — Build check error: {e}")

        # =================================================================
        # STEP 7: Tests pass AND >0 tests ran (per-step budget)
        # =================================================================
        logger.info("[Verification Step 7] Checking dotnet test...")

        test_fail_count = step_failures.get("test", 0)
        if test_fail_count >= MAX_STEP_BUDGET:
            logger.warning(f"[Verification Step 7] SKIP — test budget exhausted ({test_fail_count}/{MAX_STEP_BUDGET})")
        else:
            if sln_files:
                try:
                    result = subprocess.run(
                        ["dotnet", "test", str(sln_files[0]), "--no-build"],
                        cwd=str(output_path),
                        capture_output=True,
                        text=True,
                        timeout=180,
                    )
                    test_output = result.stdout + result.stderr

                    if result.returncode == 0:
                        total_match = re.search(r'Total tests: (\d+)', test_output)
                        passed_match = re.search(r'Passed: (\d+)', test_output)
                        executed_count = 0
                        if total_match:
                            executed_count = int(total_match.group(1))
                        elif passed_match:
                            executed_count = int(passed_match.group(1))

                        if executed_count > 0:
                            logger.info(f"[Verification Step 7] PASS — {executed_count} tests passed")
                        else:
                            logger.warning("[Verification Step 7] FAIL — Build passed but 0 tests were executed")
                            step_failures["test"] = test_fail_count + 1
                            verification_update["verification_step_failures"] = step_failures
                            return _fmt_step("Step 7: Tests", [
                                "Test suite passed but 0 tests were executed. "
                                "You MUST write unit tests with actual [Fact] or [Theory] attributes."
                            ])
                    else:
                        # Extract failed test details
                        fail_lines = [
                            line for line in test_output.splitlines()
                            if "Failed" in line or ": error " in line or "Assert." in line
                        ]
                        shown = fail_lines[:30]
                        truncated_msg = f"\n... and {len(fail_lines) - 30} more failures" if len(fail_lines) > 30 else ""
                        logger.warning("[Verification Step 7] FAIL — Tests failed")
                        step_failures["test"] = test_fail_count + 1
                        verification_update["verification_step_failures"] = step_failures
                        return _fmt_step("Step 7: Tests", [
                            f"Tests FAILED (attempt {test_fail_count + 1}/{MAX_STEP_BUDGET}).\n"
                            "Fix these failures by reading the error output and correcting the code:\n" +
                            "\n".join(shown) + truncated_msg
                        ])
                except Exception as e:
                    logger.warning(f"[Verification Step 7] SKIP — Test check error: {e}")
            else:
                logger.info("[Verification Step 7] SKIP — No solution to test")

        logger.info("[Verification] ═══ ALL CHECKS PASSED ═══")
        return {"messages": [], **verification_update}

    def post_verification_routing(state: CodegenState) -> Literal["llm_call", "__end__"]:
        """Route after verification: back to agent if CRITICAL, else done."""
        last_message = state["messages"][-1]
        if isinstance(last_message, HumanMessage) and "CRITICAL" in str(last_message.content):
            return "llm_call"
        logger.info("Verification PASSED. Codegen complete.")
        return "__end__"

    # Build the graph (3 nodes: llm_call, tool_node, verify_completion)
    builder = StateGraph(CodegenState)

    builder.add_node("llm_call", llm_call)
    builder.add_node("tool_node", tool_node)
    builder.add_node("verify_completion", verify_completion)

    # START -> LLM
    builder.add_edge(START, "llm_call")

    builder.add_conditional_edges(
        "llm_call",
        should_continue,
        ["tool_node", "verify_completion"],
    )
    builder.add_edge("tool_node", "llm_call")

    builder.add_conditional_edges(
        "verify_completion",
        post_verification_routing,
        ["llm_call", END],
    )

    agent = builder.compile()

    logger.info(f"Created Codegen Agent with {len(tools)} tools for project {project_id}")
    return agent
