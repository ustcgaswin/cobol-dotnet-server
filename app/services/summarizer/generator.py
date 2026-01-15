"""Markdown generator for file summaries."""

def generate_file_summaries_md(summaries: list[dict]) -> str:
    """Generate markdown content from summaries list.
    
    Args:
        summaries: List of summary dicts
        
    Returns:
        Markdown string
    """
    lines = ["# File Summaries", ""]
    
    # Group by type
    cobol_summaries = [s for s in summaries if s.get("type") == "cobol"]
    copybook_summaries = [s for s in summaries if s.get("type") == "copybook"]
    
    # 1. COBOL Programs
    if cobol_summaries:
        lines.append("## COBOL Programs")
        lines.append("")
        
        for s in sorted(cobol_summaries, key=lambda x: x.get("filename", "")):
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

    # 2. Copybooks
    if copybook_summaries:
        lines.append("## Copybooks")
        lines.append("")
        
        for s in sorted(copybook_summaries, key=lambda x: x.get("filename", "")):
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
            
    return '\n'.join(lines)
