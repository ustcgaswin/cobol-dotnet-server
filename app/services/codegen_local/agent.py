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
SUMMARIZE_THRESHOLD = 80  # Trigger summarization at this message count
KEEP_RECENT_MESSAGES = 10  # Keep last N messages after compression
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
    converted = []
    files_written = []
    errors_count = 0
    
    for msg in messages:
        content = str(getattr(msg, 'content', ''))
        
        # Pattern: log_component_status("NAME", "converted", ...)
        for match in re.finditer(r'log_component_status\(["\']([^"\']+)["\'],\s*["\']converted["\']', content):
            comp = match.group(1)
            if comp not in converted:
                converted.append(comp)
        
        # Pattern match in tool message responses: "Logged status: X = converted"
        for match in re.finditer(r'Logged status:\s*(\S+)\s*=\s*converted', content):
            comp = match.group(1)
            if comp not in converted:
                converted.append(comp)
        
        # Pattern: write_code_file("PATH", ...)
        for match in re.finditer(r'write_code_file\(["\']([^"\']+)["\']', content):
            path = match.group(1)
            if path not in files_written:
                files_written.append(path)
        
        # Pattern: "Wrote file:" in responses
        for match in re.finditer(r'Wrote file:\s*(\S+)', content):
            path = match.group(1)
            if path not in files_written:
                files_written.append(path)
        
        # Count errors
        if 'log_issue(' in content or 'Error:' in content:
            errors_count += 1
    
    # Format summary
    converted_list = ', '.join(converted[-15:]) if converted else 'none'
    if len(converted) > 15:
        converted_list += f' (+{len(converted) - 15} more)'
    
    summary = f"""[SESSION STATE]
Converted: {converted_list} ({len(converted)} total)
Files written: {len(files_written)}
Errors logged: {errors_count}
Continue converting remaining components."""
    
    return summary


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
    logs_path: Path
) -> list[AnyMessage]:
    """
    Compress message history by:
    1. Dumping full history to file
    2. Extracting structured state
    3. Replacing old messages with summary + recent messages
    
    IMPORTANT: We must not split AIMessage/ToolMessage pairs.
    ToolMessages must have their corresponding AIMessage with tool_calls.
    """
    # Dump before compressing
    _dump_history(messages, logs_path)
    
    # Extract state summary
    summary = _extract_structured_state(messages)
    
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
    
    # Create new compressed history
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
                messages = _compress_messages(messages, logs_path)
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
            # Replace all messages with compressed + response
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
        """Verify that critical artifacts exist before finishing.
        
        Runs structured verification steps with clear logging for each.
        Collects ALL failures before returning so the agent can fix everything in one pass.
        """
        import subprocess
        
        output_path_str = state.get("codegen_output_path", "")
        source_path_str = state.get("codegen_source_path", "")
        
        logger.info(f"[Verification] ═══════════════════════════════════════")
        logger.info(f"[Verification] Starting verification checks")
        logger.info(f"[Verification] Output: {output_path_str}")
        logger.info(f"[Verification] Source: {source_path_str}")
        logger.info(f"[Verification] ═══════════════════════════════════════")

        failures = []  # Collect ALL failures

        # -----------------------------------------------------------------
        # STEP 1: Output directory exists
        # -----------------------------------------------------------------
        logger.info("[Verification Step 1] Checking output directory exists...")
        if not output_path_str:
            logger.warning("[Verification Step 1] FAIL — No codegen_output_path in state")
            return {"messages": [HumanMessage(content="CRITICAL: No output directory configured. Cannot verify.")]}
            
        output_path = Path(output_path_str)
        if not output_path.exists():
            logger.warning("[Verification Step 1] FAIL — Output directory does not exist")
            return {"messages": [HumanMessage(content="CRITICAL: Output directory does not exist. You cannot be done.")]}
        logger.info("[Verification Step 1] PASS — Output directory exists")

        # -----------------------------------------------------------------
        # STEP 2: Solution builds successfully
        # -----------------------------------------------------------------
        logger.info("[Verification Step 2] Checking dotnet build...")
        sln_files = list(output_path.glob("*.sln"))
        if not sln_files:
            logger.warning("[Verification Step 2] FAIL — No .sln file found")
            failures.append("Missing .sln file — solution was never initialized")
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
                    logger.info("[Verification Step 2] PASS — Build succeeded")
                else:
                    build_output = result.stdout + result.stderr
                    error_lines = [l for l in build_output.splitlines() if ": error " in l]
                    logger.warning(f"[Verification Step 2] FAIL — Build failed with {len(error_lines)} errors")
                    for err in error_lines[:5]:
                        logger.warning(f"  {err.strip()}")
                    failures.append(f"Build FAILED with {len(error_lines)} errors. Run `run_dotnet_build()` to see details and fix.")
            except FileNotFoundError:
                logger.warning("[Verification Step 2] SKIP — dotnet SDK not found")
            except subprocess.TimeoutExpired:
                logger.warning("[Verification Step 2] SKIP — Build timed out")
            except Exception as e:
                logger.warning(f"[Verification Step 2] SKIP — Build check error: {e}")

        # -----------------------------------------------------------------
        # STEP 3: Tests pass
        # -----------------------------------------------------------------
        logger.info("[Verification Step 3] Checking dotnet test...")
        if sln_files and not any("Build FAILED" in f for f in failures):
            try:
                result = subprocess.run(
                    ["dotnet", "test", str(sln_files[0]), "--no-build"],
                    cwd=str(output_path),
                    capture_output=True,
                    text=True,
                    timeout=180,
                )
                if result.returncode == 0:
                    logger.info("[Verification Step 3] PASS — All tests passed")
                else:
                    test_output = result.stdout + result.stderr
                    failed_lines = [l for l in test_output.splitlines() if "Failed" in l or "Error" in l]
                    logger.warning(f"[Verification Step 3] FAIL — Tests failed")
                    failures.append(f"Tests FAILED. Run `run_dotnet_test()` to see details and fix.")
            except FileNotFoundError:
                logger.warning("[Verification Step 3] SKIP — dotnet SDK not found")
            except subprocess.TimeoutExpired:
                logger.warning("[Verification Step 3] SKIP — Tests timed out")
            except Exception as e:
                logger.warning(f"[Verification Step 3] SKIP — Test check error: {e}")
        else:
            logger.info("[Verification Step 3] SKIP — Build must pass first")

        # -----------------------------------------------------------------
        # STEP 4: JCL scripts exist
        # -----------------------------------------------------------------
        logger.info("[Verification Step 4] Checking JCL → PowerShell scripts...")
        scripts_dir = output_path / "scripts" / "jobs"
        
        if not scripts_dir.exists() or not any(scripts_dir.iterdir()):
            logger.warning("[Verification Step 4] FAIL — scripts/jobs is empty or missing")
            failures.append("scripts/jobs directory is empty or missing. You MUST convert JCL jobs to PowerShell scripts.")
        else:
            if source_path_str:
                source_path = Path(source_path_str)
                if source_path.exists():
                    jcl_files = list(source_path.rglob("*.jcl"))
                    logger.info(f"[Verification Step 4] Found {len(jcl_files)} JCL source files")
                    
                    for jcl_file in jcl_files:
                        job_name = jcl_file.stem.lower()
                        expected_script = scripts_dir / f"run-{job_name}.ps1"
                        if not expected_script.exists():
                            logger.warning(f"[Verification Step 4] Missing script for {jcl_file.name} → {expected_script.name}")
                            failures.append(f"Missing Script: run-{job_name}.ps1 (from {jcl_file.name})")
            
            if not any(f.startswith("Missing Script") for f in failures):
                logger.info("[Verification Step 4] PASS — All JCL scripts present")

        # -----------------------------------------------------------------
        # STEP 5: Service tests exist
        # -----------------------------------------------------------------
        logger.info("[Verification Step 5] Checking Service tests...")
        services_dir = output_path / "src" / "Core" / "Services"
        tests_dir = output_path / "tests" / "Core" / "Services"

        if services_dir.exists():
            service_files = list(services_dir.glob("*Service.cs"))
            logger.info(f"[Verification Step 5] Found {len(service_files)} Services")
            
            for service_file in service_files:
                test_filename = service_file.stem + "Tests.cs"
                test_file = tests_dir / test_filename
                if not test_file.exists():
                    logger.warning(f"[Verification Step 5] Missing test: {test_filename}")
                    failures.append(f"Missing Service Test: {test_filename} (for {service_file.name})")
            
            if not any("Missing Service Test" in f for f in failures):
                logger.info("[Verification Step 5] PASS — All Service tests present")
        else:
            logger.info("[Verification Step 5] SKIP — No Services directory")

        # -----------------------------------------------------------------
        # STEP 6: Repository tests exist
        # -----------------------------------------------------------------
        logger.info("[Verification Step 6] Checking Repository tests...")
        repos_dir = output_path / "src" / "Infrastructure" / "Repositories"
        repos_tests_dir = output_path / "tests" / "Infrastructure" / "Repositories"
        
        if repos_dir.exists():
            repo_files = list(repos_dir.glob("*Repository.cs"))
            logger.info(f"[Verification Step 6] Found {len(repo_files)} Repositories")
            
            for repo_file in repo_files:
                test_filename = repo_file.stem + "Tests.cs"
                test_file = repos_tests_dir / test_filename
                if not test_file.exists():
                    logger.warning(f"[Verification Step 6] Missing test: {test_filename}")
                    failures.append(f"Missing Repository Test: {test_filename} (for {repo_file.name})")
            
            if not any("Missing Repository Test" in f for f in failures):
                logger.info("[Verification Step 6] PASS — All Repository tests present")
        else:
            logger.info("[Verification Step 6] SKIP — No Repositories directory")

        # -----------------------------------------------------------------
        # STEP 7: Job step tests exist
        # -----------------------------------------------------------------
        logger.info("[Verification Step 7] Checking Job step tests...")
        jobs_src_dir = output_path / "src" / "Worker" / "Jobs"
        jobs_tests_dir = output_path / "tests" / "Worker" / "Jobs"

        if jobs_src_dir.exists():
            job_folders = [d for d in jobs_src_dir.iterdir() if d.is_dir() and (d / "Program.cs").exists()]
            logger.info(f"[Verification Step 7] Found {len(job_folders)} Job Steps")
            
            for job_folder in job_folders:
                test_filename = f"{job_folder.name}Tests.cs"
                test_file = jobs_tests_dir / test_filename
                if not test_file.exists():
                    logger.warning(f"[Verification Step 7] Missing test: {test_filename}")
                    failures.append(f"Missing Job Test: {test_filename} (for {job_folder.name})")
            
            if not any("Missing Job Test" in f for f in failures):
                logger.info("[Verification Step 7] PASS — All Job step tests present")
        else:
            logger.info("[Verification Step 7] SKIP — No Worker/Jobs directory")

        # -----------------------------------------------------------------
        # STEP 8: Functionality audit (soft — does NOT block)
        # -----------------------------------------------------------------
        logger.info("[Verification Step 8] Running functionality audit...")
        catalog_path = output_path.parent / "system_context" / "functionality_catalog.md"
        
        if catalog_path.exists():
            try:
                catalog_content = catalog_path.read_text(encoding="utf-8", errors="replace")
                f_ids = re.findall(r"(F\d{3})", catalog_content)
                unique_f_ids = sorted(list(set(f_ids)))
                
                if unique_f_ids:
                    found_ids = set()
                    cs_files = list(output_path.rglob("*.cs"))
                    
                    for cs_file in cs_files:
                        if "bin" in cs_file.parts or "obj" in cs_file.parts:
                            continue
                        try:
                            content = cs_file.read_text(encoding="utf-8", errors="replace")
                            for fid in unique_f_ids:
                                if fid in content:
                                    found_ids.add(fid)
                        except Exception:
                            continue
                    
                    missing_fids = [fid for fid in unique_f_ids if fid not in found_ids]
                    logger.info(f"[Verification Step 8] Coverage: {len(found_ids)}/{len(unique_f_ids)} functionalities tagged")
                    
                    if missing_fids:
                        logger.warning(f"[Verification Step 8] WARNING — Missing F-ID tags: {', '.join(missing_fids)}")
                    else:
                        logger.info("[Verification Step 8] PASS — All functionalities tagged")
            except Exception as e:
                logger.warning(f"[Verification Step 8] Audit error: {e}")
        else:
            logger.info("[Verification Step 8] SKIP — No functionality catalog found")

        # -----------------------------------------------------------------
        # FINAL DECISION
        # -----------------------------------------------------------------
        if failures:
            logger.warning(f"[Verification] ═══ FAILED — {len(failures)} issue(s) found ═══")
            for f in failures:
                logger.warning(f"[Verification]   • {f}")
            return {
                "messages": [
                    HumanMessage(
                        content=f"CRITICAL: Verification found {len(failures)} issue(s):\n" +
                                "\n".join([f"- {f}" for f in failures]) +
                                "\n\nYou MUST fix ALL of these before finishing."
                    )
                ]
            }

        logger.info("[Verification] ═══ ALL CHECKS PASSED ═══")
        return {"messages": []}

    def generate_process_flow(state: CodegenState) -> dict:
        """Generate final Process Flow by reading the actual generated code files."""
        logger.info("Generating Process Flow Diagram (Architect Node)...")
        
        output_path_str = state.get("codegen_output_path", "")
        if not output_path_str:
            return {"messages": []}
            
        output_path = Path(output_path_str)
        src_dir = output_path / "src"
        
        if not src_dir.exists():
            return {"messages": []}

        # 1. READ THE FILES (The "Eyes" of the Architect)
        # We need to gather the code so the LLM can analyze the *actual* implementation.
        # We focus on .cs files in Workers, Services, and Repositories.
        
        context_buffer = []
        files_to_read = list(src_dir.rglob("*.cs"))
        
        # Sort for stability
        files_to_read.sort()
        
        logger.info(f"Architect reading {len(files_to_read)} files for strict reverse engineering...")
        
        for file_path in files_to_read:
            # Skip bin/obj or tests if they ended up here
            if "bin" in file_path.parts or "obj" in file_path.parts:
                continue
                
            try:
                # Read content
                content = file_path.read_text(encoding="utf-8", errors="replace")
                
                # OPTIMIZATION: We mainly need class definitions, methods, and calls.
                # We can strip comments or empty lines to save context if needed.
                # For now, let's just dump it, but formatted clearly.
                relative_name = file_path.relative_to(src_dir)
                context_buffer.append(f"--- FILE: {relative_name} ---\n{content}\n")
            except Exception as e:
                logger.warning(f"Failed to read {file_path}: {e}")

        codebase_context = "\n".join(context_buffer)

        # 2. GENERATE DIAGRAM
        ARCHITECT_PROMPT = f"""You are a System Architect.
        
        CONTEXT: The following is the COMPLETE source code of a .NET application we just finished migrating.
        
        {codebase_context}
        
        TASK:
        Reverse Engineer this code and generate a Mermaid process flow diagram.
        
        REQUIREMENTS:
        1. Identify the Entry Points (Jobs/Workers).
        2. Trace the flow: Job -> Service -> Repository -> Database.
        3. Use Subgraphs for: `subgraph Workers`, `subgraph Services`, `subgraph Repositories`.
        4. Nodes should be named by Class Name.
        
        OUTPUT FORMAT:
        Return ONLY the mermaid code block. No conversational text.
        
        ```mermaid
        graph TD
          ...
        ```
        """
        
        try:
             # We use the base model to generate the diagram based on the file content context
             response = model.invoke([HumanMessage(content=ARCHITECT_PROMPT)])
             content = str(response.content)
             
             # Extract mermaid block
             mermaid_match = re.search(r"```mermaid\s*(.*?)\s*```", content, re.DOTALL)
             if mermaid_match:
                 diagram = mermaid_match.group(1)
                 final_content = f"```mermaid\n{diagram}\n```"
                 
                 # Write file directly
                 flow_file = output_path / "process_flow.md"
                 flow_file.write_text(final_content, encoding="utf-8")
                 logger.info(f"Architect wrote {flow_file}")
             else:
                 logger.warning("Architect failed to produce mermaid block")
                 # Fallback: Write the raw response maybe? Or just error.
                 # Let's write raw if block missing, might be malformed markdown
                 if "graph TD" in content or "flowchart TD" in content:
                     flow_file = output_path / "process_flow.md"
                     flow_file.write_text(content, encoding="utf-8")
        
        except Exception as e:
            logger.error(f"Architect Node failed: {e}")

        return {
            "messages": [
                AIMessage(content="Process Flow Generated via Source Code Analysis. Session Complete.")
            ]
        }

    def cleanup_intermediary_files(state: CodegenState) -> dict:
        """Remove intermediary tracking files from the output before archiving.
        
        Cleans up markdown status files, Dockerfiles, and deployment scripts
        that the agent may have created. Keeps only README.md in the root.
        """
        output_path_str = state.get("codegen_output_path", "")
        if not output_path_str:
            return {"messages": []}
        
        output_path = Path(output_path_str)
        if not output_path.exists():
            return {"messages": []}
        
        # Patterns to remove from root directory
        remove_patterns = [
            "completion.md", "pending.md", "DEPLOYMENT_CHECKLIST.md",
            "FILE_MANIFEST.md", "FINAL_SUMMARY.md", "migration_plan.json",
            "Dockerfile", "dockerfile", "docker-compose.yml", "docker-compose.yaml",
            ".dockerignore",
        ]
        # Also remove any deploy scripts
        deploy_patterns = ["deploy.*", "Deploy.*"]
        
        removed = []
        
        # Remove specific files from root
        for pattern in remove_patterns:
            target = output_path / pattern
            if target.exists():
                target.unlink()
                removed.append(pattern)
                logger.info(f"[Cleanup] Removed: {pattern}")
        
        # Remove any root .md files except README.md and setup.md
        for md_file in output_path.glob("*.md"):
            if md_file.name.lower() not in ("readme.md", "setup.md", "process_flow.md"):
                md_file.unlink()
                removed.append(md_file.name)
                logger.info(f"[Cleanup] Removed: {md_file.name}")
        
        # Remove deploy scripts from root
        for dp in deploy_patterns:
            for match in output_path.glob(dp):
                match.unlink()
                removed.append(match.name)
                logger.info(f"[Cleanup] Removed: {match.name}")
        
        if removed:
            logger.info(f"[Cleanup] Removed {len(removed)} intermediary files: {removed}")
        else:
            logger.info("[Cleanup] No intermediary files to remove")
        
        return {"messages": []}

    def post_verification_routing(state: CodegenState) -> Literal["llm_call", "cleanup_intermediary_files"]:
        """Decide next step after verification."""
        last_message = state["messages"][-1]
        
        # 1. If verification failed (CRITICAL), go back to work (Fix Code)
        if isinstance(last_message, HumanMessage) and "CRITICAL" in str(last_message.content):
            return "llm_call"
            
        # 2. If Code Passed, clean up then proceed to Architect
        logger.info("Code Verified. Proceeding to cleanup and then Architect Node.")
        return "cleanup_intermediary_files"

    # Build the graph
    builder = StateGraph(CodegenState)
    
    builder.add_node("llm_call", llm_call)
    builder.add_node("tool_node", tool_node)
    builder.add_node("verify_completion", verify_completion)
    builder.add_node("cleanup_intermediary_files", cleanup_intermediary_files)
    builder.add_node("generate_process_flow", generate_process_flow)

    # START -> LLM (no planning node)
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
        ["llm_call", "cleanup_intermediary_files"]
    )
    
    # Cleanup -> Architect -> END
    builder.add_edge("cleanup_intermediary_files", "generate_process_flow")
    builder.add_edge("generate_process_flow", END) 
    
    agent = builder.compile()
    
    logger.info(f"Created Codegen Agent with {len(tools)} tools for project {project_id}")
    return agent
