
"""Markdown generator for file summaries."""

def generate_file_summaries_md(summaries: list[dict]) -> str:
    """Generate markdown content from summaries list.
    
    Args:
        summaries: List of summary dicts
        
    Returns:
        Markdown string
    """
    lines = ["# File Summaries", ""]
    
    # 1. Group by type
    cobol_summaries = [s for s in summaries if s.get("type") == "cobol"]
    pli_summaries = [s for s in summaries if s.get("type") == "pli"]
    assembly_summaries = [s for s in summaries if s.get("type") == "assembly"]
    
    copybook_summaries = [s for s in summaries if s.get("type") == "copybook"]
    jcl_summaries = [s for s in summaries if s.get("type") in ["jcl", "proc"]]
    pli_summaries = [s for s in summaries if s.get("type") == "pli"]
    pli_copybook_summaries = [s for s in summaries if s.get("type") == "pli_copybook"]
    dclgen_summaries = [s for s in summaries if s.get("type") == "dclgen"] 
    ca7_summaries = [s for s in summaries if s.get("type") == "ca7"]
    
    # --- Helper to render program-style summaries (COBOL/PLI/Assembly) ---
    def _render_program_section(title: str, items: list[dict]):
        if not items:
            return
        lines.append(f"## {title}")
        lines.append("")
        
        for s in sorted(items, key=lambda x: x.get("filename", "")):
            lines.append(f"### {s.get('filename')}")
            lines.append(f"**Purpose**: {s.get('purpose', 'N/A')}")
            lines.append("")
            
            # --- ASSEMBLY SPECIFIC: Register Usage ---
            if s.get("register_usage"):
                lines.append("**Register Usage**:")
                for item in s.get("register_usage", []):
                    lines.append(f"- {item}")
                lines.append("")
            
            if s.get("functionalities"):
                lines.append("**Functionalities**:")
                for item in s.get("functionalities", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            if s.get("key_operations"):
                lines.append("**Key Operations**:")
                for item in s.get("key_operations", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            if s.get("notes"):
                lines.append("**Notes**:")
                for item in s.get("notes", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            lines.append("---")
            lines.append("")

    # --- Helper to render DCLGEN summaries ---
    def _render_dclgen_section(title: str, items: list[dict]):
        if not items: return
        lines.append(f"## {title}")
        lines.append("")
        for s in sorted(items, key=lambda x: x.get("filename", "")):
            lines.append(f"### {s.get('filename')}")
            lines.append(f"**Table**: `{s.get('table_name', 'UNKNOWN')}`")
            lines.append(f"**Host Structure**: `{s.get('host_structure', 'N/A')}`")
            lines.append(f"**Purpose**: {s.get('purpose', 'N/A')}")
            lines.append("")
            
            # New: Render the Table Structure as a Markdown Table
            if s.get("table_structure"):
                lines.append("| Column Name | DB2 Type | Nullable? |")
                lines.append("| :--- | :--- | :--- |")
                for col in s.get("table_structure", []):
                    # Expecting: "COL_NAME | TYPE | YES/NO"
                    parts = col.split('|')
                    if len(parts) == 3:
                        lines.append(f"| {parts[0].strip()} | {parts[1].strip()} | {parts[2].strip()} |")
                    else:
                        # Fallback if LLM misses a pipe
                        lines.append(f"| {col} | - | - |")
                lines.append("")

            if s.get("notes"):
                lines.append("**Notes**:")
                for item in s.get("notes", []): lines.append(f"- {item}")
                lines.append("")
            lines.append("---")
            lines.append("")
    # --- Helper to render copybook-style summaries ---
    def _render_copybook_section(title: str, items: list[dict]):
        if not items:
            return
        lines.append(f"## {title}")
        lines.append("")
        
        for s in sorted(items, key=lambda x: x.get("filename", "")):
            lines.append(f"### {s.get('filename')}")
            lines.append(f"**Purpose**: {s.get('purpose', 'N/A')}")
            
            if s.get("entity"):
                lines.append(f"**Entity**: {s.get('entity')}")
            
            lines.append("")
            
            if s.get("key_fields"):
                lines.append("**Key Fields**:")
                for item in s.get("key_fields", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            lines.append("---")
            lines.append("")
    
    # --- Helper to render CA-7 Workload summaries ---
    def _render_ca7_section(title: str, items: list[dict]):
        if not items:
            return
        lines.append(f"## {title}")
        lines.append("")
        
        for s in sorted(items, key=lambda x: x.get("filename", "")):
            lines.append(f"### {s.get('filename')}")
            lines.append(f"**Purpose**: {s.get('purpose', 'N/A')}")
            lines.append("")
            
            if s.get("workload_identity"):
                lines.append("**Workload Identity**:")
                for item in s.get("workload_identity", []):
                    lines.append(f"- {item}")
                lines.append("")

            if s.get("dependencies_triggers"):
                lines.append("**Dependencies & Triggers**:")
                for item in s.get("dependencies_triggers", []):
                    lines.append(f"- {item}")
                lines.append("")

            if s.get("operational_rules"):
                lines.append("**Operational Rules**:")
                for item in s.get("operational_rules", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            if s.get("notes"):
                lines.append("**Notes**:")
                for item in s.get("notes", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            lines.append("---")
            lines.append("")
    

    if jcl_summaries:
        lines.append("## JCL & Procedures")
        lines.append("")
        for s in sorted(jcl_summaries, key=lambda x: x.get("filename", "")):
            file_label = "PROC" if s.get("type") == "proc" else "JOB"
            lines.append(f"### {s.get('filename')} ({file_label})")
            lines.append(f"**Purpose**: {s.get('purpose', 'N/A')}")
            lines.append("")
            
            # Workflow Steps (Programs/PROCs called)
            if s.get("steps"):
                lines.append("**Workflow Steps**:")
                for item in s.get("steps", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            # Main Datasets (Inputs/Outputs)
            if s.get("main_datasets"):
                lines.append("**Main Datasets**:")
                for item in s.get("main_datasets", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            # Operational Notes (Frequency, restart logic, etc)
            if s.get("notes"):
                lines.append("**Notes**:")
                for item in s.get("notes", []):
                    lines.append(f"- {item}")
                lines.append("")
                
            lines.append("---")
            lines.append("")

    # 2. Render Sections (Calls helpers for all 5 types)
    _render_program_section("COBOL Programs", cobol_summaries)
    _render_program_section("PL/I Programs", pli_summaries)
    _render_program_section("Assembly Modules", assembly_summaries)
    _render_dclgen_section("Database Table Definitions (DCLGEN)", dclgen_summaries)
    _render_copybook_section("COBOL Copybooks", copybook_summaries)
    _render_copybook_section("PL/I Include Files", pli_copybook_summaries)
    _render_ca7_section("CA-7 Workload Orchestration", ca7_summaries)
            
    return '\n'.join(lines)