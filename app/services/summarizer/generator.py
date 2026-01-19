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
    copybook_summaries = [s for s in summaries if s.get("type") == "copybook"]
    jcl_summaries = [s for s in summaries if s.get("type") == "jcl"]
    pli_summaries = [s for s in summaries if s.get("type") == "pli"]
    pli_copybook_summaries = [s for s in summaries if s.get("type") == "pli_copybook"]
    
    # --- Helper to render program-style summaries (COBOL/PLI) ---
    def _render_program_section(title: str, items: list[dict]):
        if not items:
            return
        lines.append(f"## {title}")
        lines.append("")
        
        for s in sorted(items, key=lambda x: x.get("filename", "")):
            lines.append(f"### {s.get('filename')}")
            lines.append(f"**Purpose**: {s.get('purpose', 'N/A')}")
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
    
    if jcl_summaries:
        lines.append("## JCL / Jobs")
        lines.append("")
        for s in sorted(jcl_summaries, key=lambda x: x.get("filename", "")):
            lines.append(f"### {s.get('filename')}")
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

    # 2. Render Sections
    _render_program_section("COBOL Programs", cobol_summaries)
    _render_program_section("PL/I Programs", pli_summaries)
    
    _render_copybook_section("COBOL Copybooks", copybook_summaries)
    _render_copybook_section("PL/I Include Files", pli_copybook_summaries)
            
    return '\n'.join(lines)