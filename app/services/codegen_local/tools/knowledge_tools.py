"""Knowledge file access tools for the code generation agent.

Provides access to conversion patterns, style guides, and utility mappings.
"""

import json
from pathlib import Path

from langchain.tools import tool
from loguru import logger


def _get_knowledge_path() -> Path:
    """Get the path to the knowledge directory."""
    # Knowledge files are at project root
    return Path(__file__).resolve().parents[4] / "knowledge"


def create_knowledge_tools() -> list:
    """Create knowledge access tools.
    
    Returns:
        List of LangChain tools for knowledge file access
    """
    knowledge_path = _get_knowledge_path()
    
    @tool("read_conversion_guide")
    def read_conversion_guide() -> str:
        """Read the COBOL to C# conversion guide.
        
        Contains patterns, anti-patterns, and data type mappings for converting
        mainframe code to .NET. Use this to understand how to convert specific
        COBOL constructs.
        
        Returns:
            The complete conversion guide content
        """
        try:
            filepath = knowledge_path / "conversion_guide.md"
            if not filepath.exists():
                return "Error: conversion_guide.md not found"
            
            content = filepath.read_text(encoding="utf-8")
            return f"=== Conversion Guide ===\n\n{content}"
            
        except Exception as e:
            logger.error(f"read_conversion_guide error: {e}")
            return f"Error reading conversion guide: {e}"
    
    @tool("read_style_guide")
    def read_style_guide() -> str:
        """Read the C# code style guide.
        
        Contains naming conventions, file structure, documentation requirements.
        Use this to ensure generated code follows consistent standards.
        
        Returns:
            The complete style guide content
        """
        try:
            filepath = knowledge_path / "code_style_guide.md"
            if not filepath.exists():
                return "Error: code_style_guide.md not found"
            
            content = filepath.read_text(encoding="utf-8")
            return f"=== Code Style Guide ===\n\n{content}"
            
        except Exception as e:
            logger.error(f"read_style_guide error: {e}")
            return f"Error reading style guide: {e}"
    
    @tool("lookup_utility")
    def lookup_utility(name: str) -> str:
        """Look up the .NET equivalent for an IBM or third-party utility.
        
        Args:
            name: Utility name (e.g., "SORT", "IDCAMS", "IEBGENER")
            
        Returns:
            The .NET equivalent and notes, or "not found" message
        """
        try:
            filepath = knowledge_path / "utility_mappings.json"
            if not filepath.exists():
                return "Error: utility_mappings.json not found"
            
            with open(filepath, "r") as f:
                data = json.load(f)
            
            utilities = data.get("utilities", [])
            name_upper = name.upper()
            
            for util in utilities:
                if util.get("name", "").upper() == name_upper:
                    return (
                        f"Utility: {util['name']}\n"
                        f"Description: {util.get('description', 'N/A')}\n"
                        f".NET Equivalent: {util.get('dotnet_equivalent', 'N/A')}\n"
                        f"Notes: {util.get('notes', 'N/A')}"
                    )
            
            return f"Utility '{name}' not found in mappings. Log this with log_issue() for later research."
            
        except Exception as e:
            logger.error(f"lookup_utility error: {e}")
            return f"Error looking up utility: {e}"
    
    @tool("search_knowledge")
    def search_knowledge(pattern: str, filename: str) -> str:
        """Search for a pattern within a knowledge file.
        
        Args:
            pattern: Text pattern to search for (case-insensitive)
            filename: Knowledge file to search (conversion_guide.md, code_style_guide.md)
            
        Returns:
            Matching lines with line numbers
        """
        try:
            filepath = knowledge_path / filename
            if not filepath.exists():
                return f"Error: {filename} not found"
            
            lines = filepath.read_text(encoding="utf-8").splitlines()
            pattern_lower = pattern.lower()
            
            matches = []
            for i, line in enumerate(lines, start=1):
                if pattern_lower in line.lower():
                    matches.append(f"{i}: {line}")
            
            if not matches:
                return f"No matches for '{pattern}' in {filename}"
            
            if len(matches) > 30:
                output = "\n".join(matches[:30])
                output += f"\n... ({len(matches) - 30} more matches)"
            else:
                output = "\n".join(matches)
            
            return f"Search results for '{pattern}' in {filename}:\n{output}"
            
        except Exception as e:
            logger.error(f"search_knowledge error: {e}")
            return f"Error searching knowledge: {e}"
    
    @tool("read_process_flow")
    def read_process_flow() -> str:
        """Read the process flow documentation from the knowledge folder.
        
        Contains the overall system architecture, job chains, program inventory,
        and processing flow. Use this to understand the complete system being
        converted before starting code generation.
        
        Returns:
            The process flow content, or message if not available
        """
        try:
            filepath = knowledge_path / "process_flow.md"
            if not filepath.exists():
                logger.info("No process_flow.md found in knowledge folder")
                return "No process flow documentation available. Rely on dependency_graph.md and job_chains for system context."
            
            content = filepath.read_text(encoding="utf-8")
            
            if not content.strip():
                return "Process flow document is empty. Rely on dependency_graph.md for system context."
            
            logger.info(f"Read process_flow.md ({len(content)} chars)")
            return f"=== Process Flow ===\n\n{content}"
            
        except Exception as e:
            logger.error(f"read_process_flow error: {e}")
            return f"Error reading process flow: {e}"
    
    return [
        read_conversion_guide,
        read_style_guide,
        read_process_flow,
        lookup_utility,
        search_knowledge,
    ]
