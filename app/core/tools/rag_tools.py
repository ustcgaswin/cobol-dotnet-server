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
