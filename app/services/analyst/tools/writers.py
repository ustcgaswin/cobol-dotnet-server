"""Structured writer tools for the System Analyst Agent.

These tools guarantee valid Markdown output by accepting structured data
and formatting it in Python. Tools are created via factory functions.
"""

from pathlib import Path
from typing import Literal

from langchain.tools import tool
from loguru import logger

from app.config.settings import settings


def _get_output_path(project_id: str) -> Path:
    """Get the system_context output directory for a project."""
    base = Path(settings.PROJECT_ARTIFACTS_PATH).resolve()
    output_dir = base / project_id / "system_context"
    output_dir.mkdir(parents=True, exist_ok=True)
    return output_dir


def _append_to_file(filepath: Path, content: str) -> None:
    """Append content to a file, creating it if it doesn't exist."""
    with open(filepath, "a", encoding="utf-8") as f:
        f.write(content)


def create_writer_tools(project_id: str) -> list:
    """Factory to create writer tools bound to a specific project.
    
    Args:
        project_id: Project ID to scope all operations to
        
    Returns:
        List of LangChain tools
    """
    output_path = _get_output_path(project_id)
    
    @tool("submit_catalog_entry")
    def submit_catalog_entry(
        domain: str,
        functionality_id: str,
        name: str,
        description: str,
        implemented_by: list[str],
    ) -> str:
        """Submit a functionality catalog entry.
        
        Use this to document a business functionality discovered in the system.
        
        Args:
            domain: Business domain (e.g., "Payroll", "HR", "Reporting")
            functionality_id: Unique ID (e.g., "F001", "F002")
            name: Short name of the functionality
            description: What this functionality does
            implemented_by: List of program names that implement this
        """
        try:
            filepath = output_path / "functionality_catalog.md"
            
            if not filepath.exists():
                _append_to_file(filepath, "# Functionality Catalog\n\n")
            
            programs = ", ".join(implemented_by)
            content = f"""### {functionality_id}: {name}
**Domain**: {domain}
**Implemented By**: {programs}
**Description**: {description}

---

"""
            _append_to_file(filepath, content)
            logger.info(f"Submitted catalog entry: {functionality_id}")
            return f"Successfully submitted: {functionality_id} - {name}"
            
        except Exception as e:
            logger.error(f"submit_catalog_entry error: {e}")
            return f"Error: {e}"

    @tool("submit_job_chain")
    def submit_job_chain(
        chain_name: str,
        steps: list[dict],
        trigger: str,
        purpose: str,
    ) -> str:
        """Submit a job chain description.
        
        Args:
            chain_name: Name of the chain
            steps: List of steps, each with {job: str, description: str}
            trigger: When/how the chain is triggered
            purpose: Business purpose
        """
        try:
            filepath = output_path / "job_chains.md"
            
            if not filepath.exists():
                _append_to_file(filepath, "# Job Chains\n\n")
            
            steps_text = ""
            for i, step in enumerate(steps, 1):
                job = step.get("job", "Unknown")
                desc = step.get("description", "")
                steps_text += f"{i}. **{job}**: {desc}\n"
            
            content = f"""## {chain_name}

**Trigger**: {trigger}
**Purpose**: {purpose}

### Steps:
{steps_text}
---

"""
            _append_to_file(filepath, content)
            logger.info(f"Submitted job chain: {chain_name}")
            return f"Successfully submitted job chain: {chain_name}"
            
        except Exception as e:
            logger.error(f"submit_job_chain error: {e}")
            return f"Error: {e}"

    @tool("submit_gap")
    def submit_gap(
        component: str,
        gap_type: str,
        reason: str,
        remediation: str,
    ) -> str:
        """Submit a gap (something we don't fully understand).
        
        Args:
            component: The component with the gap
            gap_type: Category (missing_source, dynamic_call, unknown_utility, external_interface, other)
            reason: Why this is a gap
            remediation: What information would fix it
        """
        try:
            filepath = output_path / "gaps.md"
            
            if not filepath.exists():
                _append_to_file(filepath, "# Gaps\n\nItems requiring additional information.\n\n")
            
            content = f"""## {component}
**Type**: {gap_type}
**Reason**: {reason}
**Remediation**: {remediation}

---

"""
            _append_to_file(filepath, content)
            logger.info(f"Submitted gap: {component}")
            return f"Successfully submitted gap: {component}"
            
        except Exception as e:
            logger.error(f"submit_gap error: {e}")
            return f"Error: {e}"

    @tool("submit_overview")
    def submit_overview(section: str, content: str) -> str:
        """Submit a system overview section.
        
        Args:
            section: Section name (e.g., "Business Purpose")
            content: The section content
        """
        try:
            filepath = output_path / "system_overview.md"
            
            if not filepath.exists():
                _append_to_file(filepath, "# System Overview\n\n")
            
            formatted = f"""## {section}

{content}

---

"""
            _append_to_file(filepath, formatted)
            logger.info(f"Submitted overview: {section}")
            return f"Successfully submitted overview section: {section}"
            
        except Exception as e:
            logger.error(f"submit_overview error: {e}")
            return f"Error: {e}"

    @tool("submit_inventory_item")
    def submit_inventory_item(
        name: str,
        component_type: str,
        status: str,
        notes: str,
    ) -> str:
        """Submit a component to the inventory.
        
        Args:
            name: Component name
            component_type: Type (program, copybook, job, proc, rexx, assembly)
            status: Analysis status (understood, partial, gap)
            notes: Additional notes
        """
        try:
            filepath = output_path / "component_inventory.md"
            
            if not filepath.exists():
                _append_to_file(filepath, """# Component Inventory

| Component | Type | Status | Notes |
|-----------|------|--------|-------|
""")
            
            _append_to_file(filepath, f"| {name} | {component_type} | {status} | {notes} |\n")
            logger.info(f"Submitted inventory: {name}")
            return f"Successfully submitted inventory item: {name}"
            
        except Exception as e:
            logger.error(f"submit_inventory_item error: {e}")
            return f"Error: {e}"

    @tool("submit_data_flow")
    def submit_data_flow(
        source: str,
        target: str,
        data_description: str,
        direction: str,
    ) -> str:
        """Submit a data flow relationship.
        
        Args:
            source: Where data comes from
            target: Where data goes to
            data_description: What data is being transferred
            direction: Flow direction (input, output, bidirectional)
        """
        try:
            filepath = output_path / "data_flows.md"
            
            if not filepath.exists():
                _append_to_file(filepath, """# Data Flows

| Source | Target | Data | Direction |
|--------|--------|------|-----------|
""")
            
            _append_to_file(filepath, f"| {source} | {target} | {data_description} | {direction} |\n")
            logger.info(f"Submitted data flow: {source} -> {target}")
            return f"Successfully submitted data flow: {source} -> {target}"
            
        except Exception as e:
            logger.error(f"submit_data_flow error: {e}")
            return f"Error: {e}"
    
    return [
        submit_catalog_entry,
        submit_job_chain,
        submit_gap,
        submit_overview,
        submit_inventory_item,
        submit_data_flow,
    ]
