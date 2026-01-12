"""
REXX Parser for Mainframe Modernization

Comprehensive parsing of REXX scripts including:
- Variables and assignments
- Control flow structures (IF, DO, SELECT)
- Procedures and subroutines
- External calls (COBOL, JCL, TSO, ISPF)
- File/dataset operations
- Environment variables

Enhanced with:
- Structured exception handling
- Comprehensive logging
- EBCDIC encoding support
- Unrecognized content tracking
- Parse error reporting
- LLM-powered file analysis and documentation
- RAG-enhanced understanding of REXX patterns
"""

import json
import logging
import re
import asyncio
from dataclasses import asdict, dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple
from tenacity import retry, stop_after_attempt, wait_exponential

from app.core.exceptions.parser import (
    InvalidREXXSyntaxError,
    REXXParseError,
    UnsupportedREXXFeatureError,
)
from app.core.parsers.base import BaseParser

# Configure logging to match JCL and PL/I parsers
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('rexx_parser')


# =============================================================================
# Data Structures
# =============================================================================

class REXXTokenType(Enum):
    """Token types for REXX syntax elements"""
    COMMENT = "COMMENT"
    VARIABLE = "VARIABLE"
    ASSIGNMENT = "ASSIGNMENT"
    CONTROL_FLOW = "CONTROL_FLOW"
    PROCEDURE = "PROCEDURE"
    EXTERNAL_CALL = "EXTERNAL_CALL"
    FILE_OPERATION = "FILE_OPERATION"
    UTILITY_CALL = "UTILITY_CALL"
    ENVIRONMENT_VAR = "ENVIRONMENT_VAR"
    STRING_LITERAL = "STRING_LITERAL"
    FUNCTION_CALL = "FUNCTION_CALL"


@dataclass
class Variable:
    """Represents a REXX variable"""
    name: str
    initial_value: Optional[str] = None
    line_number: int = 0
    scope: str = "global"
    data_type: str = "string"
    
    def to_dict(self) -> Dict:
        return {
            "name": self.name,
            "initialValue": self.initial_value,
            "lineNumber": self.line_number,
            "scope": self.scope,
            "dataType": self.data_type
        }


@dataclass
class ControlFlowNode:
    """Represents control flow constructs (IF, DO, SELECT, etc.)"""
    type: str
    condition: Optional[str] = None
    start_line: int = 0
    end_line: int = 0
    nested_level: int = 0
    children: List['ControlFlowNode'] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            "type": self.type,
            "condition": self.condition,
            "startLine": self.start_line,
            "endLine": self.end_line,
            "nestedLevel": self.nested_level,
            "children": [child.to_dict() for child in self.children]
        }


@dataclass
class Procedure:
    """Represents a REXX procedure or subroutine"""
    name: str
    start_line: int
    end_line: int
    parameters: List[str] = field(default_factory=list)
    local_variables: List[str] = field(default_factory=list)
    calls_made: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            "name": self.name,
            "startLine": self.start_line,
            "endLine": self.end_line,
            "parameters": self.parameters,
            "localVariables": self.local_variables,
            "callsMade": self.calls_made
        }


@dataclass
class ExternalCall:
    """Represents calls to external programs (COBOL, utilities, etc.)"""
    call_type: str
    program_name: str
    line_number: int
    parameters: List[str] = field(default_factory=list)
    return_code_check: Optional[str] = None
    context: str = ""
    
    def to_dict(self) -> Dict:
        return {
            "callType": self.call_type,
            "programName": self.program_name,
            "lineNumber": self.line_number,
            "parameters": self.parameters,
            "returnCodeCheck": self.return_code_check,
            "context": self.context
        }


@dataclass
class FileAccess:
    """Represents file or dataset access operations"""
    operation: str
    file_name: str
    line_number: int
    file_type: str = "UNKNOWN"
    access_mode: str = "UNKNOWN"
    ddname: Optional[str] = None
    
    def to_dict(self) -> Dict:
        return {
            "operation": self.operation,
            "fileName": self.file_name,
            "lineNumber": self.line_number,
            "fileType": self.file_type,
            "accessMode": self.access_mode,
            "ddname": self.ddname
        }


@dataclass
class EnvironmentVariable:
    """Represents environment variable usage"""
    name: str
    line_number: int
    operation: str
    value: Optional[str] = None
    
    def to_dict(self) -> Dict:
        return {
            "name": self.name,
            "lineNumber": self.line_number,
            "operation": self.operation,
            "value": self.value
        }


# =============================================================================
# REXX Parser Implementation
# =============================================================================

class REXXParser(BaseParser):
    """
    Enterprise REXX Parser for Mainframe Modernization
    
    Parses REXX scripts and extracts:
    - Variables and control flow
    - Procedures and external calls
    - File operations and environment variables
    - Dependencies for migration planning
    """
    
    FILE_TYPE = "rexx"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    
    CONTROL_KEYWORDS = {
        'IF', 'THEN', 'ELSE', 'DO', 'END', 'SELECT', 'WHEN', 'OTHERWISE',
        'LEAVE', 'ITERATE', 'EXIT', 'RETURN', 'CALL', 'SIGNAL', 'PROCEDURE'
    }
    
    BUILTIN_FUNCTIONS = {
        'ABBREV', 'ABS', 'ADDRESS', 'ARG', 'BITAND', 'BITOR', 'BITXOR',
        'B2X', 'CENTER', 'CENTRE', 'CHANGESTR', 'COMPARE', 'CONDITION',
        'COPIES', 'C2D', 'C2X', 'DATATYPE', 'DATE', 'DELSTR', 'DELWORD',
        'DIGITS', 'D2C', 'D2X', 'ERRORTEXT', 'FORM', 'FORMAT', 'FUZZ',
        'INSERT', 'LASTPOS', 'LEFT', 'LENGTH', 'LINESIZE', 'MAX', 'MIN',
        'OVERLAY', 'POS', 'QUEUED', 'RANDOM', 'REVERSE', 'RIGHT', 'SIGN',
        'SOURCELINE', 'SPACE', 'STRIP', 'SUBSTR', 'SUBWORD', 'SYMBOL',
        'TIME', 'TRACE', 'TRANSLATE', 'TRUNC', 'VALUE', 'VERIFY', 'WORD',
        'WORDINDEX', 'WORDLENGTH', 'WORDPOS', 'WORDS', 'XRANGE', 'X2B',
        'X2C', 'X2D'
    }
    
    TSO_COMMANDS = {
        'ALLOC', 'ALLOCATE', 'FREE', 'DELETE', 'LISTDS', 'LISTCAT',
        'SUBMIT', 'TRANSMIT', 'RECEIVE', 'SEND', 'ATTRIB', 'RENAME'
    }
    
    ISPF_COMMANDS = {
        'ISPEXEC', 'ISREDIT', 'BROWSE', 'EDIT', 'VIEW', 'LMINIT',
        'LMOPEN', 'LMCLOSE', 'LMFREE', 'LMMOVE', 'LMCOPY'
    }
    
    AUGMENTATION_MAX_RETRIES = 3
    AUGMENTATION_TIMEOUT = 60  # seconds per LLM call

    def __init__(self):
        """Initialize REXX parser"""
        self.lines: List[str] = []
        self.current_line: int = 0
        self.variables: List[Variable] = []
        self.control_flow: List[ControlFlowNode] = []
        self.procedures: List[Procedure] = []
        self.external_calls: List[ExternalCall] = []
        self.files_accessed: List[FileAccess] = []
        self.environment_vars: List[EnvironmentVariable] = []
        self.comments: List[Dict[str, Any]] = []
        self.file_name: str = ""
        self._unrecognized: List[Dict[str, Any]] = []
        self._warnings: List[str] = []
        self._source: str = ""
        
        logger.info("Initialized REXXParser")
    
    def parse_file(self, filepath: str) -> dict:
        """Parse a REXX file from filesystem.
        
        Required by BaseParser interface for FastAPI integration.
        
        Args:
            filepath: Path to the REXX file
            
        Returns:
            Dictionary containing parsed data
            
        Raises:
            FileNotFoundError: If file doesn't exist
            REXXParseError: If parsing fails
        """
        path = Path(filepath)
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            raise FileNotFoundError(f"File not found: {filepath}")
        
        logger.info(f"Parsing REXX file: {filepath} (size: {path.stat().st_size} bytes)")
        
        try:
            # Read file with encoding detection
            raw_bytes = path.read_bytes()
            content, encoding = self._detect_and_decode(raw_bytes)
            logger.info(f"Detected encoding: {encoding}")
            
            # Parse content
            result = self.parse_string(content, path.name)
            
            # Add framework-required fields
            result['encoding'] = encoding
            result['file_type'] = self.FILE_TYPE
            result['source_file'] = path.name
            
            logger.info(
                f"Successfully parsed {filepath}: {result['metadata']['totalLines']} lines, "
                f"{len(self._warnings)} warnings, {len(self._unrecognized)} unrecognized patterns"
            )
            
            return result
            
        except Exception as e:
            logger.error(f"Failed to parse {filepath}: {e}", exc_info=True)
            raise REXXParseError(
                message=f"REXX parsing failed: {e}",
                filename=path.name
            )
    
    def parse_string(self, content: str, file_name: str = "unknown.rexx") -> Dict[str, Any]:
        """Parse REXX content from string.
        
        Core parsing logic - useful for testing and backward compatibility.
        
        Args:
            content: REXX script content
            file_name: Name of the file for metadata
            
        Returns:
            Dictionary containing parsed data
        """
        self.file_name = file_name
        self.lines = content.split('\n')
        self._source = content  # NEW: Store raw source
        self._reset_state()
        
        logger.debug(f"Parsing REXX content: {len(self.lines)} lines")
        
        try:
            self._extract_comments()
            logger.debug(f"Extracted {len(self.comments)} comments")
            
            self._extract_procedures()
            logger.debug(f"Extracted {len(self.procedures)} procedures")
            
            self._extract_variables()
            logger.debug(f"Extracted {len(self.variables)} variables")
            
            self._extract_control_flow()
            logger.debug(f"Extracted {len(self.control_flow)} control flow structures")
            
            self._extract_external_calls()
            logger.debug(f"Extracted {len(self.external_calls)} external calls")
            
            self._extract_file_operations()
            logger.debug(f"Extracted {len(self.files_accessed)} file operations")
            
            self._extract_environment_variables()
            logger.debug(f"Extracted {len(self.environment_vars)} environment variables")
            
            return self._generate_output()
            
        except Exception as e:
            logger.error(f"Error during REXX parsing: {e}", exc_info=True)
            raise REXXParseError(
                message=f"REXX parsing failed: {e}",
                filename=file_name
            )

    def _create_augmentation_agent(self):
        """Create LangGraph ReAct agent with REXX RAG tool access.
        
        The agent can decide when to search REXX documentation for context.
        """
        from langgraph.prebuilt import create_react_agent
        from app.config.llm_config import llm
        from app.core.tools.rag_tools import search_rexx_docs
        
        return create_react_agent(model=llm, tools=[search_rexx_docs])
    
    async def augment(self, parsed_data: dict) -> dict:
        """Augment parsed REXX data with LLM-generated metadata.
        
        Adds:
        - description: Script-level description of the REXX script's purpose
        - llm_summary: Procedure-level summaries
        - llm_orchestration_pattern: Overall orchestration pattern analysis
        
        Args:
            parsed_data: The parsed REXX JSON structure
            
        Returns:
            The augmented parsed data with LLM-generated fields
        """
        agent = self._create_augmentation_agent()
        
        # 1. Add script-level description
        try:
            parsed_data["description"] = await self._augment_rexx_script(agent, parsed_data)
        except Exception as e:
            logger.error(f"Failed to augment script description: {e}")
            parsed_data["description"] = None
            parsed_data["_augmentation_errors"] = parsed_data.get("_augmentation_errors", [])
            parsed_data["_augmentation_errors"].append(f"Script description failed: {e}")
        
        # 2. Add procedure summaries
        for proc in parsed_data.get('procedures', []):
            try:
                # Extract procedure code
                proc_code = self._extract_procedure_code(
                    proc.get("name", ""),
                    proc.get("startLine", 0),
                    proc.get("endLine", 0)
                )
                
                proc["llm_summary"] = await self._augment_procedure(
                    agent, proc, parsed_data, proc_code
                )
            except Exception as e:
                logger.error(f"Failed to augment procedure {proc.get('name')}: {e}")
                proc["llm_summary"] = None
                parsed_data["_augmentation_errors"] = parsed_data.get("_augmentation_errors", [])
                parsed_data["_augmentation_errors"].append(
                    f"Procedure {proc.get('name')} failed: {e}"
                )
        
        # 3. Add orchestration pattern analysis (if external calls exist)
        external_calls = parsed_data.get('externalCalls', {}).get('calls', [])
        if external_calls and len(external_calls) > 0:
            try:
                parsed_data["llm_orchestration_pattern"] = await self._analyze_orchestration(
                    agent, parsed_data
                )
            except Exception as e:
                logger.error(f"Failed to analyze orchestration pattern: {e}")
                parsed_data["llm_orchestration_pattern"] = None
                parsed_data["_augmentation_errors"] = parsed_data.get("_augmentation_errors", [])
                parsed_data["_augmentation_errors"].append(f"Orchestration analysis failed: {e}")
        
        return parsed_data
    
    async def _augment_rexx_script(self, agent, parsed_data: dict) -> str:
        """Generate script-level description using LangGraph agent.
        
        The agent may use RAG tool to understand REXX constructs.
        """
        file_name = parsed_data.get("fileName", "UNKNOWN")
        metadata = parsed_data.get("metadata", {})
        deps = parsed_data.get("dependencies", {})
        
        # Build context from parsed data
        total_lines = metadata.get("totalLines", 0)
        total_procs = metadata.get("totalProcedures", 0)
        
        # Extract key dependencies
        cobol_programs = deps.get("cobolPrograms", [])[:5]
        jcl_jobs = deps.get("jclJobs", [])[:3]
        datasets = deps.get("datasets", [])[:5]
        
        prompt = f"""Analyze this REXX script and provide a concise business description.

Script: {file_name}
Total Lines: {total_lines}
Procedures: {total_procs}
COBOL Programs Called: {', '.join(cobol_programs) if cobol_programs else 'None'}
JCL Jobs Submitted: {', '.join(jcl_jobs) if jcl_jobs else 'None'}
Datasets Accessed: {', '.join(datasets) if datasets else 'None'}

If you need to understand specific REXX constructs or commands, use the search tool.

Provide a 1-2 sentence description of what this script does and its business purpose.
Return ONLY the description, no additional text."""

        @retry(
            stop=stop_after_attempt(self.AUGMENTATION_MAX_RETRIES),
            wait=wait_exponential(multiplier=1, min=2, max=10),
        )
        async def invoke_with_retry():
            result = await asyncio.wait_for(
                agent.ainvoke({"messages": [("user", prompt)]}),
                timeout=self.AUGMENTATION_TIMEOUT,
            )
            return result["messages"][-1].content.strip()
        
        return await invoke_with_retry()
    
    def _extract_procedure_code(
        self, proc_name: str, start_line: int, end_line: int
    ) -> str:
        """Extract the code for a procedure from the stored source.
        
        Args:
            proc_name: Name of the procedure to extract
            start_line: Starting line number (1-indexed)
            end_line: Ending line number (1-indexed)
            
        Returns:
            The code content of the procedure
        """
        if not self._source or start_line == 0:
            return ""
        
        lines = self._source.split('\n')
        
        # Adjust for 1-indexed line numbers
        start_idx = max(0, start_line - 1)
        end_idx = min(len(lines), end_line)
        
        if start_idx >= len(lines):
            return ""
        
        proc_lines = lines[start_idx:end_idx]
        return '\n'.join(proc_lines)
    
    async def _augment_procedure(
        self, agent, procedure: dict, parsed_data: dict, proc_code: str
    ) -> str:
        """Generate procedure summary using LangGraph agent.
        
        The agent may use RAG tool for unfamiliar REXX statements.
        """
        proc_name = procedure.get("name", "UNKNOWN")
        file_name = parsed_data.get("fileName", "UNKNOWN")
        params = procedure.get("parameters", [])
        
        prompt = f"""Analyze this REXX procedure and provide a brief summary.

Script: {file_name}
Procedure: {proc_name}
Parameters: {', '.join(params) if params else 'None'}

Code:
{proc_code if proc_code else "(Code not available)"}

If you need to understand specific REXX statements or commands, use the search tool.

Provide a 1-sentence summary of what this procedure does.
This will be used for documentation, so be concise and action-oriented.
Return ONLY the summary, no questions or additional text."""

        @retry(
            stop=stop_after_attempt(self.AUGMENTATION_MAX_RETRIES),
            wait=wait_exponential(multiplier=1, min=2, max=10),
        )
        async def invoke_with_retry():
            result = await asyncio.wait_for(
                agent.ainvoke({"messages": [("user", prompt)]}),
                timeout=self.AUGMENTATION_TIMEOUT,
            )
            return result["messages"][-1].content.strip()
        
        return await invoke_with_retry()
    
    async def _analyze_orchestration(
        self, agent, parsed_data: dict
    ) -> str:
        """Analyze the orchestration pattern of external calls.
        
        The agent may use RAG tool for understanding orchestration patterns.
        """
        file_name = parsed_data.get("fileName", "UNKNOWN")
        external_calls = parsed_data.get('externalCalls', {})
        summary = external_calls.get('summary', {})
        calls = external_calls.get('calls', [])
        
        # Build call sequence context
        call_sequence = []
        for call in calls[:10]:  # Limit to first 10 for context
            call_sequence.append(
                f"Line {call.get('lineNumber')}: {call.get('callType')} - {call.get('programName')}"
            )
        
        prompt = f"""Analyze this REXX script's orchestration pattern and describe how it coordinates external programs.

Script: {file_name}
COBOL Programs: {summary.get('cobolPrograms', 0)}
JCL Jobs: {summary.get('jclJobs', 0)}
TSO Commands: {summary.get('tsoCommands', 0)}
ISPF Commands: {summary.get('ispfCommands', 0)}

Call Sequence:
{chr(10).join(call_sequence) if call_sequence else "(No external calls)"}

If you need to understand orchestration patterns or command sequences, use the search tool.

Provide a 1-sentence description of the orchestration pattern this script follows.
Focus on the workflow: what gets executed in what order and why.
Return ONLY the pattern description, no additional text."""

        @retry(
            stop=stop_after_attempt(self.AUGMENTATION_MAX_RETRIES),
            wait=wait_exponential(multiplier=1, min=2, max=10),
        )
        async def invoke_with_retry():
            result = await asyncio.wait_for(
                agent.ainvoke({"messages": [("user", prompt)]}),
                timeout=self.AUGMENTATION_TIMEOUT,
            )
            return result["messages"][-1].content.strip()
        
        return await invoke_with_retry()
  
    
    def _detect_and_decode(self, raw_bytes: bytes) -> Tuple[str, str]:
        """Detect encoding with mainframe EBCDIC support.
        
        Args:
            raw_bytes: Raw file bytes
            
        Returns:
            Tuple of (decoded_content, encoding_name)
        """
        # Try UTF-8 first (most common)
        try:
            content = raw_bytes.decode('utf-8')
            logger.debug("Encoding detected: utf-8")
            return content, 'utf-8'
        except UnicodeDecodeError:
            pass
        
        # Try ASCII
        try:
            content = raw_bytes.decode('ascii')
            logger.debug("Encoding detected: ascii")
            return content, 'ascii'
        except UnicodeDecodeError:
            pass
        
        # Try EBCDIC codepages (mainframe files)
        for codepage in self.EBCDIC_CODEPAGES:
            try:
                content = raw_bytes.decode(codepage)
                # Heuristic: Check if content looks reasonable for REXX
                if any(keyword in content.upper() for keyword in ['IF', 'DO', 'END', 'SAY', 'CALL']):
                    logger.debug(f"Encoding detected: {codepage} (EBCDIC)")
                    return content, codepage
            except (UnicodeDecodeError, LookupError):
                continue
        
        # Fallback with error replacement
        logger.warning("Using UTF-8 with error replacement as fallback")
        content = raw_bytes.decode('utf-8', errors='replace')
        return content, 'utf-8'
    
    def _reset_state(self):
        """Reset parser state for new file"""
        self.variables = []
        self.control_flow = []
        self.procedures = []
        self.external_calls = []
        self.files_accessed = []
        self.environment_vars = []
        self.comments = []
        self._unrecognized = []
        self._warnings = []
    
    def _extract_comments(self):
        """Extract all comments from REXX script"""
        for i, line in enumerate(self.lines, 1):
            comment_pattern = r'/\*(.*?)\*/'
            matches = re.finditer(comment_pattern, line)
            for match in matches:
                self.comments.append({
                    "lineNumber": i,
                    "text": match.group(1).strip(),
                    "type": "block"
                })
    
    def _extract_procedures(self):
        """Extract procedure definitions"""
        procedure_stack = []
        
        for i, line in enumerate(self.lines, 1):
            line_upper = line.strip().upper()
            
            # Match: LABEL: PROCEDURE [EXPOSE ...]
            proc_match = re.match(r'^(\w+):\s*PROCEDURE', line_upper)
            if proc_match:
                proc_name = proc_match.group(1)
                params = []
                param_match = re.search(r'PROCEDURE\s+EXPOSE\s+([\w\s]+)', line_upper)
                if param_match:
                    params = param_match.group(1).split()
                
                proc = Procedure(
                    name=proc_name,
                    start_line=i,
                    end_line=0,
                    parameters=params
                )
                procedure_stack.append(proc)
                logger.debug(f"Found procedure '{proc_name}' at line {i}")
            
            # Match: RETURN (end of procedure)
            if line_upper.strip().startswith('RETURN') and procedure_stack:
                proc = procedure_stack[-1]
                if proc.end_line == 0:
                    proc.end_line = i
        
        # Close any unclosed procedures
        for proc in procedure_stack:
            if proc.end_line == 0:
                proc.end_line = len(self.lines)
                self._warnings.append(
                    f"Procedure '{proc.name}' at line {proc.start_line} has no explicit RETURN"
                )
            self.procedures.append(proc)
    
    def _extract_variables(self):
        """Extract variable declarations and assignments"""
        for i, line in enumerate(self.lines, 1):
            line_clean = self._remove_comments(line).strip()
            if not line_clean:
                continue
            
            # Pattern: VAR = VALUE
            assign_pattern = r'^([A-Za-z_][\w.]*)\s*=\s*(.+)$'
            match = re.match(assign_pattern, line_clean)
            
            if match:
                var_name = match.group(1)
                var_value = match.group(2).strip()
                
                # Only add if not already tracked
                existing = next((v for v in self.variables if v.name == var_name), None)
                if not existing:
                    data_type = self._infer_data_type(var_value)
                    
                    var = Variable(
                        name=var_name,
                        initial_value=var_value,
                        line_number=i,
                        data_type=data_type
                    )
                    self.variables.append(var)
    
    def _extract_control_flow(self):
        """Extract control flow structures"""
        flow_stack = []
        nesting_level = 0
        
        for i, line in enumerate(self.lines, 1):
            line_clean = self._remove_comments(line).strip().upper()
            if not line_clean:
                continue
            
            try:
                # IF ... THEN
                if_match = re.match(r'IF\s+(.+?)\s+THEN', line_clean)
                if if_match:
                    condition = if_match.group(1)
                    node = ControlFlowNode(
                        type="IF",
                        condition=condition,
                        start_line=i,
                        nested_level=nesting_level
                    )
                    flow_stack.append(node)
                    nesting_level += 1
                
                # ELSE
                elif line_clean.startswith('ELSE'):
                    if flow_stack and flow_stack[-1].type == "IF":
                        else_node = ControlFlowNode(
                            type="ELSE",
                            start_line=i,
                            nested_level=nesting_level - 1
                        )
                        flow_stack[-1].children.append(else_node)
                
                # DO loops
                elif line_clean.startswith('DO'):
                    condition = None
                    do_patterns = [
                        r'DO\s+WHILE\s+\((.+?)\)',
                        r'DO\s+UNTIL\s+\((.+?)\)',
                        r'DO\s+(\w+)\s*=\s*(.+?)\s+TO\s+(.+)',
                        r'DO\s+FOREVER'
                    ]
                    
                    for pattern in do_patterns:
                        match = re.search(pattern, line_clean)
                        if match:
                            condition = match.group(0)
                            break
                    
                    node = ControlFlowNode(
                        type="DO",
                        condition=condition,
                        start_line=i,
                        nested_level=nesting_level
                    )
                    flow_stack.append(node)
                    nesting_level += 1
                
                # SELECT
                elif line_clean.startswith('SELECT'):
                    node = ControlFlowNode(
                        type="SELECT",
                        start_line=i,
                        nested_level=nesting_level
                    )
                    flow_stack.append(node)
                    nesting_level += 1
                
                # WHEN
                elif line_clean.startswith('WHEN'):
                    when_match = re.match(r'WHEN\s+(.+)', line_clean)
                    if when_match and flow_stack:
                        condition = when_match.group(1)
                        when_node = ControlFlowNode(
                            type="WHEN",
                            condition=condition,
                            start_line=i,
                            nested_level=nesting_level
                        )
                        if flow_stack[-1].type == "SELECT":
                            flow_stack[-1].children.append(when_node)
                
                # OTHERWISE
                elif line_clean.startswith('OTHERWISE'):
                    if flow_stack and flow_stack[-1].type == "SELECT":
                        otherwise_node = ControlFlowNode(
                            type="OTHERWISE",
                            start_line=i,
                            nested_level=nesting_level
                        )
                        flow_stack[-1].children.append(otherwise_node)
                
                # END
                elif line_clean == 'END' or line_clean.startswith('END '):
                    if flow_stack:
                        node = flow_stack.pop()
                        node.end_line = i
                        nesting_level = max(0, nesting_level - 1)
                        
                        if nesting_level == 0:
                            self.control_flow.append(node)
            
            except Exception as e:
                logger.warning(f"Line {i}: Failed to parse control flow: {e}")
                self._unrecognized.append({
                    "line": i,
                    "content": line[:100],
                    "error": f"Control flow parsing error: {e}"
                })
        
        # Close any unclosed structures
        for node in flow_stack:
            node.end_line = len(self.lines)
            self.control_flow.append(node)
            self._warnings.append(
                f"Unclosed {node.type} structure starting at line {node.start_line}"
            )
    
    def _extract_external_calls(self):
        """Extract calls to external programs, utilities, and jobs"""
        for i, line in enumerate(self.lines, 1):
            line_clean = self._remove_comments(line).strip()
            line_upper = line_clean.upper()
            if not line_clean:
                continue
            
            try:
                # CALL program
                call_match = re.match(r'CALL\s+["\']?(\w+)["\']?\s*(.*)', line_upper)
                if call_match:
                    program_name = call_match.group(1)
                    params_str = call_match.group(2).strip()
                    
                    params = []
                    if params_str:
                        params_str = params_str.strip('()')
                        params = [p.strip().strip('"\'') for p in params_str.split(',')]
                    
                    call = ExternalCall(
                        call_type="COBOL_PROGRAM",
                        program_name=program_name,
                        line_number=i,
                        parameters=params,
                        context=line_clean[:100]
                    )
                    self.external_calls.append(call)
                
                # SUBMIT job
                submit_match = re.search(r'SUBMIT\s+["\']?([A-Za-z0-9.()]+)["\']?', line_upper)
                if submit_match:
                    job_name = submit_match.group(1)
                    call = ExternalCall(
                        call_type="JCL_JOB",
                        program_name=job_name,
                        line_number=i,
                        context=line_clean[:100]
                    )
                    self.external_calls.append(call)
                
                # TSO commands
                for cmd in self.TSO_COMMANDS:
                    if cmd in line_upper:
                        call = ExternalCall(
                            call_type="TSO_COMMAND",
                            program_name=cmd,
                            line_number=i,
                            context=line_clean[:100]
                        )
                        self.external_calls.append(call)
                        break
                
                # ISPF commands
                for cmd in self.ISPF_COMMANDS:
                    if cmd in line_upper:
                        call = ExternalCall(
                            call_type="ISPF_COMMAND",
                            program_name=cmd,
                            line_number=i,
                            context=line_clean[:100]
                        )
                        self.external_calls.append(call)
                        break
                
                # ADDRESS TSO/ISPEXEC
                address_match = re.match(r'ADDRESS\s+(TSO|ISPEXEC)\s+["\'](.+?)["\']', line_upper)
                if address_match:
                    environment = address_match.group(1)
                    command = address_match.group(2)
                    call = ExternalCall(
                        call_type=f"{environment}_COMMAND",
                        program_name=command.split()[0],
                        line_number=i,
                        context=line_clean[:100]
                    )
                    self.external_calls.append(call)
                
                # OUTTRAP utility
                if 'OUTTRAP' in line_upper:
                    call = ExternalCall(
                        call_type="UTILITY",
                        program_name="OUTTRAP",
                        line_number=i,
                        context=line_clean[:100]
                    )
                    self.external_calls.append(call)
            
            except Exception as e:
                logger.debug(f"Line {i}: Could not parse external call: {e}")
    
    def _extract_file_operations(self):
        """Extract file and dataset access operations"""
        for i, line in enumerate(self.lines, 1):
            line_clean = self._remove_comments(line).strip()
            line_upper = line_clean.upper()
            if not line_clean:
                continue
            
            try:
                # ALLOCATE dataset
                if 'ALLOCATE' in line_upper or 'ALLOC' in line_upper:
                    ds_patterns = [
                        r'DATASET\(([A-Za-z0-9.()]+)\)',
                        r'DA\(([A-Za-z0-9.()]+)\)',
                        r'DSN\(([A-Za-z0-9.()]+)\)'
                    ]
                    
                    for pattern in ds_patterns:
                        match = re.search(pattern, line_upper)
                        if match:
                            dataset = match.group(1)
                            
                            file_type = "FLAT"
                            if '(+' in dataset or '(0)' in dataset:
                                file_type = "GDG"
                            
                            access_mode = "UNKNOWN"
                            if 'SHR' in line_upper:
                                access_mode = "SHARED"
                            elif 'OLD' in line_upper:
                                access_mode = "EXCLUSIVE"
                            elif 'NEW' in line_upper:
                                access_mode = "CREATE"
                            
                            ddname = None
                            dd_match = re.search(r'DD\((\w+)\)', line_upper)
                            if dd_match:
                                ddname = dd_match.group(1)
                            
                            file_access = FileAccess(
                                operation="ALLOCATE",
                                file_name=dataset,
                                line_number=i,
                                file_type=file_type,
                                access_mode=access_mode,
                                ddname=ddname
                            )
                            self.files_accessed.append(file_access)
                            break
                
                # FREE dataset
                if 'FREE' in line_upper:
                    free_match = re.search(r'FREE\s+(FI|FILE|DD)\((\w+)\)', line_upper)
                    if free_match:
                        ddname = free_match.group(2)
                        file_access = FileAccess(
                            operation="FREE",
                            file_name=ddname,
                            line_number=i,
                            ddname=ddname
                        )
                        self.files_accessed.append(file_access)
                
                # DELETE dataset
                if 'DELETE' in line_upper:
                    del_match = re.search(r'DELETE\s+["\']?([A-Za-z0-9.()]+)["\']?', line_upper)
                    if del_match:
                        dataset = del_match.group(1)
                        file_access = FileAccess(
                            operation="DELETE",
                            file_name=dataset,
                            line_number=i
                        )
                        self.files_accessed.append(file_access)
                
                # EXECIO (read/write)
                execio_match = re.match(
                    r'["\']?EXECIO\s+(\*|\d+)\s+(DISKR|DISKW|DISKRU|DISKWU)\s+(\w+)',
                    line_upper
                )
                if execio_match:
                    operation_code = execio_match.group(2)
                    ddname = execio_match.group(3)
                    
                    operation = "READ" if 'DISKR' in operation_code else "WRITE"
                    access_mode = "UPDATE" if 'U' in operation_code else "SEQUENTIAL"
                    
                    file_access = FileAccess(
                        operation=operation,
                        file_name=ddname,
                        line_number=i,
                        access_mode=access_mode,
                        ddname=ddname
                    )
                    self.files_accessed.append(file_access)
                
                # LISTDS
                listds_match = re.search(r'LISTDS\s+["\']?([A-Za-z0-9.()]+)["\']?', line_upper)
                if listds_match:
                    dataset = listds_match.group(1)
                    file_access = FileAccess(
                        operation="LIST",
                        file_name=dataset,
                        line_number=i
                    )
                    self.files_accessed.append(file_access)
            
            except Exception as e:
                logger.debug(f"Line {i}: Could not parse file operation: {e}")
    
    def _extract_environment_variables(self):
        """Extract environment variable usage"""
        for i, line in enumerate(self.lines, 1):
            line_clean = self._remove_comments(line).strip()
            line_upper = line_clean.upper()
            if not line_clean:
                continue
            
            try:
                # GETENV-like patterns (custom or via ADDRESS TSO)
                getenv_match = re.search(r'(\w+)\s*=\s*GETENV\(["\'](\w+)["\']\)', line_upper)
                if getenv_match:
                    var_name = getenv_match.group(2)
                    env_var = EnvironmentVariable(
                        name=var_name,
                        line_number=i,
                        operation="GET"
                    )
                    self.environment_vars.append(env_var)
                
                # SETENV patterns
                setenv_patterns = [
                    r'SETENV\(["\'](\w+)["\']\s*,\s*["\']?(.+?)["\']?\)',
                    r'ADDRESS\s+TSO\s+["\']SET\s+(\w+)=(.+?)["\']'
                ]
                
                for pattern in setenv_patterns:
                    match = re.search(pattern, line_upper)
                    if match:
                        var_name = match.group(1)
                        var_value = match.group(2).strip()
                        env_var = EnvironmentVariable(
                            name=var_name,
                            line_number=i,
                            operation="SET",
                            value=var_value
                        )
                        self.environment_vars.append(env_var)
                        break
            
            except Exception as e:
                logger.debug(f"Line {i}: Could not parse environment variable usage: {e}")
    
    def _remove_comments(self, line: str) -> str:
        """Remove REXX block comments from a line (inline /* ... */ only)"""
        return re.sub(r'/\*.*?\*/', '', line)
    
    def _infer_data_type(self, value: str) -> str:
        """Infer data type from assignment value"""
        value = value.strip().strip('"\'')
        
        if re.match(r'^-?\d+$', value):
            return "integer"
        if re.match(r'^-?\d+\.\d+$', value):
            return "decimal"
        if value.upper() in ('TRUE', 'FALSE', '1', '0'):
            return "boolean"
        
        return "string"
    
    def _generate_output(self) -> Dict[str, Any]:
        """Generate final structured output with both framework and REXX-specific fields"""
        rexx_specific_output = {
            "fileType": "REXX",
            "fileName": self.file_name,
            "metadata": {
                "totalLines": len(self.lines),
                "totalComments": len(self.comments),
                "totalVariables": len(self.variables),
                "totalProcedures": len(self.procedures),
                "totalExternalCalls": len(self.external_calls),
                "totalFileOperations": len(self.files_accessed)
            },
            "comments": [
                {
                    "lineNumber": c["lineNumber"],
                    "text": c["text"],
                    "type": c["type"]
                }
                for c in self.comments
            ],
            "variables": [v.to_dict() for v in self.variables],
            "controlFlow": [cf.to_dict() for cf in self.control_flow],
            "procedures": [p.to_dict() for p in self.procedures],
            "externalCalls": {
                "summary": {
                    "cobolPrograms": len([c for c in self.external_calls if c.call_type == "COBOL_PROGRAM"]),
                    "jclJobs": len([c for c in self.external_calls if c.call_type == "JCL_JOB"]),
                    "tsoCommands": len([c for c in self.external_calls if c.call_type == "TSO_COMMAND"]),
                    "ispfCommands": len([c for c in self.external_calls if c.call_type == "ISPF_COMMAND"]),
                    "utilities": len([c for c in self.external_calls if c.call_type == "UTILITY"])
                },
                "calls": [ec.to_dict() for ec in self.external_calls]
            },
            "filesAccessed": {
                "summary": {
                    "totalFiles": len(self.files_accessed),
                    "allocations": len([f for f in self.files_accessed if f.operation == "ALLOCATE"]),
                    "reads": len([f for f in self.files_accessed if f.operation == "READ"]),
                    "writes": len([f for f in self.files_accessed if f.operation == "WRITE"]),
                    "deletes": len([f for f in self.files_accessed if f.operation == "DELETE"])
                },
                "files": [fa.to_dict() for fa in self.files_accessed]
            },
            "environmentVariables": [ev.to_dict() for ev in self.environment_vars],
            "dependencies": self._generate_dependency_summary()
        }
        
        # Add framework-required fields
        result = {
            "file_type": self.FILE_TYPE,
            "source_file": self.file_name,
            "encoding": getattr(self, "_detected_encoding", "unknown"),
            "_warnings": self._warnings,
            "_unrecognized": self._unrecognized,
            **rexx_specific_output
        }
        
        return result
    
    def _generate_dependency_summary(self) -> Dict[str, Any]:
        """Generate dependency summary for migration planning"""
        return {
            "cobolPrograms": list(set([
                ec.program_name for ec in self.external_calls 
                if ec.call_type == "COBOL_PROGRAM"
            ])),
            "jclJobs": list(set([
                ec.program_name for ec in self.external_calls 
                if ec.call_type == "JCL_JOB"
            ])),
            "datasets": list(set([
                fa.file_name for fa in self.files_accessed
            ])),
            "utilities": list(set([
                ec.program_name for ec in self.external_calls 
                if ec.call_type in ("TSO_COMMAND", "ISPF_COMMAND", "UTILITY")
            ])),
            "environmentVariables": list(set([
                ev.name for ev in self.environment_vars
            ]))
        }


def parse_rexx_file(file_path: str) -> Dict[str, Any]:
    """Convenience function to parse a REXX file (backward compatible)"""
    parser = REXXParser()
    return parser.parse_file(file_path)


def parse_rexx_string(content: str, file_name: str = "inline.rexx") -> Dict[str, Any]:
    """Convenience function to parse REXX content from string (backward compatible)"""
    parser = REXXParser()
    return parser.parse_string(content, file_name)