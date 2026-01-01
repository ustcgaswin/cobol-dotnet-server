"""Agent tools module.

This module provides LangChain tools for agent use:
- RAG tools for documentation search
- File tools for project file access
"""

from app.core.tools.context import ProjectContext
from app.core.tools.file_tools import grep_search, list_files, view_file
from app.core.tools.rag_tools import search_cobol_docs, search_docs

# All available tools for agent binding
ALL_TOOLS = [
    # Documentation search
    search_docs,
    search_cobol_docs,
    # File operations
    view_file,
    grep_search,
    list_files,
]

__all__ = [
    "ProjectContext",
    "ALL_TOOLS",
    # RAG tools
    "search_docs",
    "search_cobol_docs",
    # File tools
    "view_file",
    "grep_search",
    "list_files",
]
