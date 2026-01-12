"""RAG documentation search tools for agent use."""

from langchain.tools import tool

from app.services.rag import rag_service


@tool("search_docs")
def search_docs(question: str) -> str:
    """Search ALL documentation to answer questions.
    
    Use this tool to find information about any topic in the project documentation,
    including COBOL, JCL, DB2, and other mainframe technologies.
    
    Args:
        question: The question or topic to search for
        
    Returns:
        An answer synthesized from relevant documentation
    """
    result = rag_service.ask(question, source_folder=None)
    return result["answer"]


@tool("search_cobol_docs")
def search_cobol_docs(question: str) -> str:
    """Search COBOL documentation to answer questions about COBOL.
    
    Use this tool specifically for questions about:
    - COBOL syntax and statements (MOVE, PERFORM, IF, EVALUATE, etc.)
    - COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
    - COBOL data types and PICTURE clauses
    - COBOL file handling and I/O
    - COBOL program structure and copybooks
    
    Args:
        question: The COBOL-related question to answer
        
    Returns:
        An answer synthesized from COBOL documentation
    """
    result = rag_service.ask(question, source_folder="cobol")
    return result["answer"]


@tool("search_flatfile_docs")
def search_flatfile_docs(question: str) -> str:
    """Search flat file format documentation to answer questions.
    
    Use this tool specifically for questions about:
    - CSV format specifications, delimiters, and parsing rules
    - Fixed-length file layouts and record structures
    - COBOL copybook formats and field definitions
    - Common mainframe flat file patterns and EBCDIC encoding
    - Data type inference and validation rules
    - Industry-standard file formats (banking, healthcare, retail)
    - Flat file migration best practices
    
    Args:
        question: The flat file related question to answer
        
    Returns:
        An answer synthesized from flat file documentation
    """
    result = rag_service.ask(question, source_folder="flat_files")
    return result["answer"]

@tool("search_parmlib_docs")
def search_parmlib_docs(question: str) -> str:
    """Search PARMLIB documentation to answer questions about PARMLIB control members.
    
    Use this tool specifically for questions about:
    - PARMLIB parameter syntax and meanings
    - Utility commands (SORT, IDCAMS, IEBGENER)
    - Dataset and program dependencies
    - Control statement purposes
    - Migration patterns to .NET
    
    Args:
        question: The PARMLIB-related question to answer
        
    Returns:
        An answer synthesized from PARMLIB documentation
    """
    result = rag_service.ask(question, source_folder="Parmlib")
    return result["answer"]


@tool("search_rexx_docs")
def search_rexx_docs(question: str) -> str:
    """Search REXX documentation to answer questions about REXX scripting.
    
    Use this tool specifically for questions about:
    - REXX syntax and built-in functions
    - Control flow structures (IF, DO, SELECT)
    - TSO and ISPF commands
    - External program calls (COBOL, JCL)
    - File and dataset operations
    - Migration patterns to PowerShell/.NET
    
    Args:
        question: The REXX-related question to answer
        
    Returns:
        An answer synthesized from REXX documentation
    """
    result = rag_service.ask(question, source_folder="rexx")
    return result["answer"]