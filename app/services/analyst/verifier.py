"""Hybrid coverage verification for System Analyst output.

Combines:
1. Programmatic metrics (fast, deterministic)
2. LLM semantic analysis (understands meaning)
"""

import re
from pathlib import Path

from loguru import logger

from app.config.settings import settings
from app.config.llm_config import get_llm


VERIFICATION_PROMPT = """You are a verification agent reviewing documentation coverage.

## File Summaries (Source of Truth)
{summaries_content}

## Generated Functionality Catalog
{catalog_content}

## Your Task
Analyze whether the catalog fully captures the functionalities from the summaries.

Respond in this exact JSON format:
{{
    "coverage_rating": <1-10 rating>,
    "all_functionalities_covered": <true/false>,
    "missing_functionalities": ["list of functionalities from summaries not in catalog"],
    "extra_in_catalog": ["functionalities in catalog not clearly from summaries"],
    "suggestions": ["improvements to catalog"],
    "summary": "Brief assessment of coverage quality"
}}

Be strict but fair. A 10/10 means every functionality is clearly represented."""


def verify_coverage(project_id: str) -> dict:
    """Verify coverage using hybrid approach.
    
    Returns:
        Report with programmatic metrics and LLM analysis
    """
    base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
    artifacts_path = base / project_id
    system_context_path = artifacts_path / "system_context"
    
    summaries_file = artifacts_path / "file_summaries.md"
    catalog_file = system_context_path / "functionality_catalog.md"
    
    # === PROGRAMMATIC METRICS ===
    programmatic = _get_programmatic_metrics(summaries_file, catalog_file)
    
    # === LLM SEMANTIC ANALYSIS ===
    llm_analysis = _get_llm_analysis(summaries_file, catalog_file)
    
    return {
        "programmatic_metrics": programmatic,
        "llm_analysis": llm_analysis,
    }


def _get_programmatic_metrics(summaries_file: Path, catalog_file: Path) -> dict:
    """Fast programmatic checks."""
    summary_files = _extract_files_from_summaries(summaries_file)
    catalog_programs = _extract_programs_from_catalog(catalog_file)
    functionality_count = _count_functionalities_in_summaries(summaries_file)
    catalog_entry_count = _count_catalog_entries(catalog_file)
    
    # Normalize for comparison
    summary_set = set(f.lower().replace('.cbl', '').replace('.cpy', '').replace('.jcl', '') 
                     for f in summary_files)
    catalog_set = set(p.lower() for p in catalog_programs)
    
    covered = summary_set & catalog_set
    coverage_pct = len(covered) / len(summary_set) * 100 if summary_set else 100
    
    return {
        "files_in_summaries": len(summary_files),
        "files_covered": len(covered),
        "functionalities_in_summaries": functionality_count,
        "entries_in_catalog": catalog_entry_count,
        "file_coverage_percent": round(coverage_pct, 1),
    }


def _get_llm_analysis(summaries_file: Path, catalog_file: Path) -> dict:
    """LLM-based semantic analysis."""
    try:
        if not summaries_file.exists() or not catalog_file.exists():
            return {"error": "Required files not found"}
        
        summaries_content = summaries_file.read_text(encoding="utf-8", errors="replace")
        catalog_content = catalog_file.read_text(encoding="utf-8", errors="replace")
        
        # Truncate if too long
        max_chars = 12000
        if len(summaries_content) > max_chars:
            summaries_content = summaries_content[:max_chars] + "\n... (truncated)"
        if len(catalog_content) > max_chars:
            catalog_content = catalog_content[:max_chars] + "\n... (truncated)"
        
        prompt = VERIFICATION_PROMPT.format(
            summaries_content=summaries_content,
            catalog_content=catalog_content,
        )
        
        llm = get_llm()
        response = llm.invoke(prompt)
        
        # Try to parse JSON from response
        import json
        content = response.content
        
        # Extract JSON from response
        json_match = re.search(r'\{[\s\S]*\}', content)
        if json_match:
            return json.loads(json_match.group())
        
        return {"raw_response": content}
        
    except Exception as e:
        logger.error(f"LLM verification failed: {e}")
        return {"error": str(e)}


# === Helper Functions ===

def _extract_files_from_summaries(filepath: Path) -> list[str]:
    """Extract file names from file_summaries.md."""
    if not filepath.exists():
        return []
    content = filepath.read_text(encoding="utf-8", errors="replace")
    pattern = r"^### ([\w\-]+\.\w+)$"
    return re.findall(pattern, content, re.MULTILINE)


def _count_functionalities_in_summaries(filepath: Path) -> int:
    """Count bullet points under **Functionalities**: sections."""
    if not filepath.exists():
        return 0
    content = filepath.read_text(encoding="utf-8", errors="replace")
    count = 0
    in_func_section = False
    for line in content.splitlines():
        if "**Functionalities**" in line:
            in_func_section = True
            continue
        if in_func_section:
            if line.startswith("- "):
                count += 1
            elif line.startswith("**") or line.startswith("##"):
                in_func_section = False
    return count


def _extract_programs_from_catalog(filepath: Path) -> list[str]:
    """Extract program names from 'Implemented By' fields."""
    if not filepath.exists():
        return []
    content = filepath.read_text(encoding="utf-8", errors="replace")
    pattern = r"\*\*Implemented By\*\*:\s*(.+)$"
    matches = re.findall(pattern, content, re.MULTILINE)
    programs = set()
    for match in matches:
        for prog in match.split(","):
            prog = prog.strip()
            if prog:
                programs.add(prog)
    return list(programs)


def _count_catalog_entries(filepath: Path) -> int:
    """Count ### FXXX: entries in catalog."""
    if not filepath.exists():
        return 0
    content = filepath.read_text(encoding="utf-8", errors="replace")
    pattern = r"^### F\d+:"
    return len(re.findall(pattern, content, re.MULTILINE))
