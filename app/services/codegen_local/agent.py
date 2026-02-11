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
    model = get_llm(CODEGEN, model=LLMModel.GPT4_1_MINI)
    
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
        full_messages = [SystemMessage(content=SYSTEM_PROMPT)] + messages
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
        """Verify that critical artifacts exist before finishing."""
        output_path_str = state.get("codegen_output_path", "")
        if not output_path_str:
            # If path not provided, we can't verify, so assume partial success but warn log
            logger.warning("No codegen_output_path in state, skipping verification")
            return {"messages": []}
            
        output_path = Path(output_path_str)
        if not output_path.exists():
            return {"messages": [HumanMessage(content="CRITICAL ERROR: Output directory does not exist. You cannot be done.")]}
        
        # Check for generated jobs/scripts
        jobs_dir = output_path / "scripts" / "jobs"
        if not jobs_dir.exists() or not any(jobs_dir.iterdir()):
             return {
                "messages": [
                    HumanMessage(
                        content="CRITICAL: `scripts/jobs` directory is empty or missing. "
                                "You have not converted any JCL jobs. "
                                "You MUST convert the jobs listed in dependency_graph.md."
                    )
                ]
            }

        # Check for missing tests
        services_dir = output_path / "src" / "Core" / "Services"
        tests_dir = output_path / "tests" / "Core" / "Services"
        missing_tests = []

        if services_dir.exists():
            for service_file in services_dir.glob("*Service.cs"):
                # Expecting ServiceNameService.cs -> ServiceNameServiceTests.cs
                test_filename = service_file.stem + "Tests.cs"
                test_file = tests_dir / test_filename
                
                if not test_file.exists():
                    missing_tests.append(f"Service: {service_file.name}")

        # Check for missing Repository tests
        repos_dir = output_path / "src" / "Infrastructure" / "Repositories"
        repos_tests_dir = output_path / "tests" / "Infrastructure" / "Repositories"
        
        if repos_dir.exists():
            for repo_file in repos_dir.glob("*Repository.cs"):
                test_filename = repo_file.stem + "Tests.cs"
                test_file = repos_tests_dir / test_filename
                
                if not test_file.exists():
                     missing_tests.append(f"Repository: {repo_file.name}")

        # Check for missing Job tests
        # Jobs are typically src/Worker/Jobs/StepName/Program.cs
        # We expect tests/Worker/Jobs/StepNameTests.cs
        jobs_src_dir = output_path / "src" / "Worker" / "Jobs"
        jobs_tests_dir = output_path / "tests" / "Worker" / "Jobs"
        scripts_dir = output_path / "scripts" / "jobs"

        # STRICT Verification: Every .jcl file in Source must have a .ps1 script
        source_path_str = state.get("codegen_source_path", "")
        if source_path_str:
            source_path = Path(source_path_str)
            if source_path.exists():
                # Only look for TOP-LEVEL JOBS (.jcl), not Procedures (.proc/.prc)
                # Procedures are dependencies, not executable entry points.
                for jcl_file in source_path.rglob("*.jcl"):
                    # Expected script name: run-{filename_lower}.ps1
                    # e.g., JOB01.jcl -> run-job01.ps1
                    job_name = jcl_file.stem.lower()
                    expected_script = scripts_dir / f"run-{job_name}.ps1"
                    
                    if not expected_script.exists():
                         missing_tests.append(f"Missing Script: scripts/jobs/run-{job_name}.ps1 (for {jcl_file.name})")

        # Check for missing Job tests (Worker/Jobs)
        if jobs_src_dir.exists():
            # Walk through job directories
            for job_folder in jobs_src_dir.iterdir():
                if job_folder.is_dir():
                    # Check if this folder contains a Program.cs (meaning it's a job step)
                    if (job_folder / "Program.cs").exists():
                        test_filename = f"{job_folder.name}Tests.cs"
                        test_file = jobs_tests_dir / test_filename
                        
                        if not test_file.exists():
                            missing_tests.append(f"Job Test: {job_folder.name}")
        
        # ---------------------------------------------------------------------
        # FUNCTIONALITY AUDIT (Soft Check)
        # ---------------------------------------------------------------------
        # 1. Parse Functionality Catalog for F-IDs
        # 2. Scan Codebase for Tags
        # 3. Report Missing Tags
        
        project_id = state.get("project_id")
        catalog_path = output_path.parent / "system_context" / "functionality_catalog.md"
        
        missing_functionalities = []
        
        if catalog_path.exists():
            try:
                import re
                catalog_content = catalog_path.read_text(encoding="utf-8", errors="replace")
                # Regex to find IDs like "### F001:" or "**F001**"
                # Looking for standard pattern from Analyst prompt
                f_ids = re.findall(r"(F\d{3})", catalog_content)
                unique_f_ids = sorted(list(set(f_ids)))
                
                if unique_f_ids:
                    logger.info(f"Auditing coverage for functionalities: {unique_f_ids}")
                    
                    found_ids = set()
                    
                    # Scan all generated .cs files
                    for valid_file in output_path.rglob("*.cs"):
                        if "bin" in valid_file.parts or "obj" in valid_file.parts:
                            continue
                        
                        try:
                            content = valid_file.read_text(encoding="utf-8", errors="replace")
                            # Look for "// Implements: F001" or '[Trait("Functionality", "F001")]'
                            for fid in unique_f_ids:
                                if fid in content:
                                    found_ids.add(fid)
                        except Exception:
                            continue
                    
                    # Calculate Gap
                    for fid in unique_f_ids:
                        if fid not in found_ids:
                            missing_functionalities.append(fid)
            except Exception as e:
                logger.warning(f"Functionality audit failed: {e}")
        
        audit_warnings = []
        if missing_functionalities:
            msg = f"WARNING: The following functionalities are not explicitly tagged in the codebase: {', '.join(missing_functionalities)}"
            audit_warnings.append(msg)
            logger.warning(msg) 
            # We do NOT block on this, but we append it to the final message if we were returning one
        
        # ---------------------------------------------------------------------
        # FINAL DECISION
        # ---------------------------------------------------------------------
        
        if missing_tests:
            logger.warning(f"Verification failed: Missing tests/scripts for {missing_tests}")
            return {
                "messages": [
                    HumanMessage(
                        content=f"CRITICAL: Missing artifacts detected:\n" + 
                                "\n".join([f"- {m}" for m in missing_tests]) + 
                                "\nYou MUST generate these before finishing."
                    )
                ]
            }

        if audit_warnings:
             # If we passed strict checks but have audit warnings, we add a gentle nudge 
             # OR we just let it pass and log it. 
             # "Senior Engineer" decision: Let it pass, but maybe add a final note in logs.
             pass

        logger.info("Completion verification passed (Codebase is complete)")
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

    def post_verification_routing(state: CodegenState) -> Literal["llm_call", "generate_process_flow"]:
        """Decide next step after verification."""
        last_message = state["messages"][-1]
        
        # 1. If verification failed (CRITICAL), go back to work (Fix Code)
        if isinstance(last_message, HumanMessage) and "CRITICAL" in str(last_message.content):
            return "llm_call"
            
        # 2. If Code Passed, Always go to Architect for final documentation
        logger.info("Code Verified. Proceeding to Architect Node.")
        return "generate_process_flow"

    # Build the graph
    builder = StateGraph(CodegenState)
    
    builder.add_node("llm_call", llm_call)
    builder.add_node("tool_node", tool_node)
    builder.add_node("verify_completion", verify_completion)
    builder.add_node("generate_process_flow", generate_process_flow) # New Node

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
        ["llm_call", "generate_process_flow"]
    )
    
    # Architect is TERMINAL
    builder.add_edge("generate_process_flow", END) 
    
    agent = builder.compile()
    
    logger.info(f"Created Codegen Agent with {len(tools)} tools for project {project_id}")
    return agent
