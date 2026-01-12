"""
PARMLIB Parser - Mainframe Control Member Parser
Integrated with the mainframe modernization parser framework.

Parses PARMLIB control member files and extracts:
- Parameters and control statements
- Utility commands (SORT, IDCAMS, etc.)
- Dataset and program dependencies
- Sections and execution flow
- Conditional logic

Enhanced with:
- Structured exception handling
- Comprehensive logging
- EBCDIC encoding support
- Unrecognized content tracking
- Parse error reporting
- LLM-powered file analysis and documentation
- RAG-enhanced understanding of PARMLIB

Usage:
    python parmlib_parser.py <parmlib_file> [-o output.json]
"""

import argparse
import asyncio
import json
import logging
import re
import sys
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple
from tenacity import retry, stop_after_attempt, wait_exponential

from app.core.exceptions import (
    EmptyFileError,
    EncodingDetectionError,
    InvalidStatementError,
    PARMLIBParseError,
    SectionParseError,
    UtilityCommandError,
)
from app.core.parsers.base import BaseParser

# ============================================================================
# Logging Configuration (Dual Setup: Standard + Loguru-Ready)
# ============================================================================

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('parmlib_parser')

# Optional loguru integration
try:
    from loguru import logger as loguru_logger
    LOGURU_AVAILABLE = True
except ImportError:
    LOGURU_AVAILABLE = False
    loguru_logger = None


# ============================================================================
# Statement Type Enumeration
# ============================================================================

class StatementType(Enum):
    """Types of statements found in PARMLIB members"""
    PARAMETER = "parameter"
    CONTROL_STATEMENT = "control_statement"
    UTILITY_COMMAND = "utility_command"
    DATASET_REFERENCE = "dataset_reference"
    PROGRAM_REFERENCE = "program_reference"
    CONDITIONAL = "conditional"
    SECTION_HEADER = "section_header"
    CONTINUATION = "continuation"
    COMMENT = "comment"
    UNKNOWN = "unknown"


# ============================================================================
# Main PARMLIB Parser
# ============================================================================

class PARMLIBParser(BaseParser):
    """
    Enterprise-grade parser for PARMLIB control member files.
    
    Extracts parameters, control statements, dependencies, and execution flow
    with comprehensive error tracking and mainframe encoding support.
    """
    
    FILE_TYPE = "parmlib"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']

    AUGMENTATION_MAX_RETRIES = 3
    AUGMENTATION_TIMEOUT = 60  # seconds per LLM call
    
    # Regex Patterns
    DATASET_PATTERN = re.compile(r'\b([A-Z0-9$#@]{1,8}(?:\.[A-Z0-9$#@]{1,8}){0,21})\b')
    PARAM_PATTERNS = [
        re.compile(r'^([A-Z_][A-Z0-9_]*)\s*=\s*(.+)$', re.IGNORECASE),
        re.compile(r'^([A-Z_][A-Z0-9_]*)\s+(.+)$', re.IGNORECASE),
    ]
    SORT_PATTERN = re.compile(r'^\s*SORT\s+FIELDS\s*=\s*\((.+)\)', re.IGNORECASE)
    IDCAMS_PATTERN = re.compile(r'^\s*(DEFINE|DELETE|ALTER|LISTCAT|PRINT|REPRO)\s+', re.IGNORECASE)
    PROGRAM_PATTERNS = [
        re.compile(r'(?:PGM|PROGRAM)\s*=\s*([A-Z0-9]+)', re.IGNORECASE),
        re.compile(r'(?:EXEC)\s+([A-Z0-9]+)', re.IGNORECASE),
        re.compile(r'(?:CALL)\s+["\']?([A-Z0-9]+)["\']?', re.IGNORECASE),
    ]
    DATASET_KEYWORDS = ['DSN', 'DSNAME', 'FILE', 'INPUT', 'OUTPUT', 'DATASET', 'DD']
    CONDITIONAL_PATTERN = re.compile(r'^\s*(IF|WHEN|ELSE|ENDIF|END-IF)\b', re.IGNORECASE)
    CONTINUATION_CHARS = [',', '-', '+']
    
    def __init__(self):
        """Initialize PARMLIB parser."""
        self.current_section = "MAIN"
        self.line_number = 0
        
        # Error tracking for parse_errors.json integration
        self._warnings: List[str] = []
        self._unrecognized: List[Dict[str, Any]] = []
        
        logger.info("Initialized PARMLIBParser")
    
    def parse_file(self, filepath: str, **kwargs) -> dict:
        """Parse a PARMLIB file from filesystem.
        
        Args:
            filepath: Path to the PARMLIB file
            **kwargs: Additional parameters
            
        Returns:
            Dictionary containing parsed data with structure:
            {
                "file_type": "parmlib",
                "source_file": filename,
                "encoding": detected encoding,
                "metadata": {...},
                "parameters": [...],
                "control_statements": [...],
                "sections": {...},
                "dependencies": [...],
                "_warnings": [...],
                "_unrecognized": [...]
            }
            
        Raises:
            FileNotFoundError: If file doesn't exist
            EmptyFileError: If file is empty
            EncodingDetectionError: If encoding cannot be detected
            PARMLIBParseError: If parsing fails critically
        """
        path = Path(filepath)
        
        logger.info(f"Starting PARMLIB parse: {filepath}")
        
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            raise FileNotFoundError(f"File not found: {filepath}")
        
        if not path.is_file():
            logger.error(f"Path is not a file: {filepath}")
            raise PARMLIBParseError(f"Path is not a file: {filepath}", filename=path.name)
        
        file_size = path.stat().st_size
        logger.debug(f"File size: {file_size} bytes")
        
        if file_size == 0:
            logger.error(f"Empty file: {filepath}")
            raise EmptyFileError("PARMLIB file is empty", filename=path.name)
        
        # Reset tracking for each parse
        self._warnings = []
        self._unrecognized = []
        self.current_section = "MAIN"
        self.line_number = 0
        
        try:
            # Read file with EBCDIC-aware encoding detection
            raw_bytes = path.read_bytes()
            content, encoding = self._detect_and_decode(raw_bytes, path.name)
            logger.info(f"Detected encoding: {encoding}")
            
            # Parse using string parsing logic
            result = self.parse_string(content, source_file=path.name)
            
            # Add framework-required fields
            result['encoding'] = encoding
            result['file_type'] = self.FILE_TYPE
            result['source_file'] = path.name
            
            # Add error tracking fields
            result['_warnings'] = self._warnings
            result['_unrecognized'] = self._unrecognized
            
            logger.info(
                f"Successfully parsed {filepath}: "
                f"{result['metadata']['total_lines']} lines, "
                f"{len(result.get('parameters', []))} parameters, "
                f"{len(result.get('control_statements', []))} control statements, "
                f"{len(self._warnings)} warnings, "
                f"{len(self._unrecognized)} unrecognized"
            )
            
            return result
            
        except EmptyFileError:
            logger.error(f"Empty file: {filepath}")
            raise
        except EncodingDetectionError:
            logger.error(f"Encoding detection failed: {filepath}")
            raise
        except Exception as e:
            logger.error(f"Failed to parse {filepath}: {e}", exc_info=True)
            raise PARMLIBParseError(
                f"PARMLIB parsing failed: {str(e)}",
                filename=path.name
            ) from e

    def _create_augmentation_agent(self):
        """Create LangGraph ReAct agent with PARMLIB RAG tool access.
        
        The agent can decide when to search PARMLIB documentation for context.
        """
        from langgraph.prebuilt import create_react_agent
        from app.config.llm_config import llm
        from app.core.tools.rag_tools import search_parmlib_docs
        
        return create_react_agent(model=llm, tools=[search_parmlib_docs])
    
    async def augment(self, parsed_data: dict) -> dict:
        """Augment parsed PARMLIB data with LLM-generated metadata.
        
        Adds:
        - description: File-level description of the control member's purpose
        - llm_summary: Section-level summaries (for non-MAIN sections)
        - llm_explanation: Utility command explanations
        
        Args:
            parsed_data: The parsed PARMLIB JSON structure
            
        Returns:
            The augmented parsed data with LLM-generated fields
        """
        agent = self._create_augmentation_agent()
        
        # 1. Add file-level description
        try:
            parsed_data["description"] = await self._augment_parmlib_file(agent, parsed_data)
        except Exception as e:
            logger.error(f"Failed to augment file description: {e}")
            parsed_data["description"] = None
            parsed_data["_augmentation_errors"] = parsed_data.get("_augmentation_errors", [])
            parsed_data["_augmentation_errors"].append(f"File description failed: {e}")
        
        # 2. Add section summaries (only for non-MAIN sections with content)
        if "sections" in parsed_data:
            for section_name, section_stmts in parsed_data['sections'].items():
                if section_name != "MAIN" and len(section_stmts) > 0:
                    try:
                        # Extract section code
                        section_code = self._extract_section_code(section_name, section_stmts)
                        
                        section_summary = await self._augment_section(
                            agent, section_name, section_stmts, parsed_data, section_code
                        )
                        
                        # Transform section to dict with summary
                        parsed_data['sections'][section_name] = {
                            'statements': section_stmts,
                            'llm_summary': section_summary
                        }
                    except Exception as e:
                        logger.error(f"Failed to augment section {section_name}: {e}")
                        parsed_data['sections'][section_name] = {
                            'statements': section_stmts,
                            'llm_summary': None
                        }
                        parsed_data["_augmentation_errors"] = parsed_data.get("_augmentation_errors", [])
                        parsed_data["_augmentation_errors"].append(
                            f"Section {section_name} failed: {e}"
                        )
        
        # 3. Add utility command explanations
        for stmt in parsed_data.get('control_statements', []):
            if stmt.get('utility'):
                try:
                    stmt["llm_explanation"] = await self._augment_utility_command(
                        agent, stmt, parsed_data
                    )
                except Exception as e:
                    logger.error(f"Failed to augment utility {stmt.get('utility')}: {e}")
                    stmt["llm_explanation"] = None
                    parsed_data["_augmentation_errors"] = parsed_data.get("_augmentation_errors", [])
                    parsed_data["_augmentation_errors"].append(
                        f"Utility {stmt.get('utility')} at line {stmt.get('line_number')} failed: {e}"
                    )
        
        return parsed_data
    
    async def _augment_parmlib_file(self, agent, parsed_data: dict) -> str:
        """Generate file-level description using LangGraph agent.
        
        The agent may use RAG tool to understand PARMLIB constructs.
        """
        source_file = parsed_data.get("source_file", "UNKNOWN")
        metadata = parsed_data.get("metadata", {})
        deps = parsed_data.get("dependencies", [])
        
        # Build context from parsed data
        param_count = len(parsed_data.get("parameters", []))
        control_count = len(parsed_data.get("control_statements", []))
        
        # Extract utilities used
        utilities = list(set([
            stmt.get("utility") for stmt in parsed_data.get("control_statements", [])
            if stmt.get("utility")
        ]))
        
        # Extract datasets referenced
        datasets = list(set([
            dep.get("name") for dep in deps if dep.get("type") == "DATASET"
        ]))[:5]  # Limit to first 5
        
        # Extract programs referenced
        programs = list(set([
            dep.get("name") for dep in deps if dep.get("type") == "PROGRAM"
        ]))[:5]  # Limit to first 5
        
        prompt = f"""Analyze this PARMLIB control member and provide a concise business description.

File: {source_file}
Parameters: {param_count}
Control Statements: {control_count}
Utilities Used: {', '.join(utilities) if utilities else 'None'}
Datasets Referenced: {', '.join(datasets) if datasets else 'None'}
Programs Referenced: {', '.join(programs) if programs else 'None'}

If you need to understand specific PARMLIB constructs or utility commands, use the search tool.

Provide a 1-2 sentence description of what this control member does and its business purpose.
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
    
    def _extract_section_code(
        self, section_name: str, section_stmts: list
    ) -> str:
        """Extract the code for a section from section statements.
        
        Args:
            section_name: Name of the section
            section_stmts: List of statement dictionaries in the section
            
        Returns:
            The code content of the section as text
        """
        if not section_stmts:
            return ""
        
        # Combine raw_text from all statements
        code_lines = []
        for stmt in section_stmts:
            raw_text = stmt.get("raw_text", "")
            if raw_text:
                code_lines.append(raw_text)
        
        return '\n'.join(code_lines)
    
    async def _augment_section(
        self, agent, section_name: str, section_stmts: list, 
        parsed_data: dict, section_code: str
    ) -> str:
        """Generate section summary using LangGraph agent.
        
        The agent may use RAG tool for unfamiliar PARMLIB constructs.
        """
        source_file = parsed_data.get("source_file", "UNKNOWN")
        
        prompt = f"""Analyze this PARMLIB section and provide a brief summary.

File: {source_file}
Section: {section_name}
Statements: {len(section_stmts)}

Code:
{section_code if section_code else "(Code not available)"}

If you need to understand specific PARMLIB parameters or commands, use the search tool.

Provide a 1-sentence summary of what this section accomplishes.
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
    
    async def _augment_utility_command(
        self, agent, stmt: dict, parsed_data: dict
    ) -> str:
        """Generate utility command explanation using LangGraph agent.
        
        The agent may use RAG tool for utility-specific documentation.
        """
        utility = stmt.get("utility", "UNKNOWN")
        command = stmt.get("command", "")
        line_number = stmt.get("line_number", 0)
        
        prompt = f"""Analyze this {utility} utility command and explain its purpose.

Utility: {utility}
Line: {line_number}
Command:
{command}

If you need to understand specific {utility} syntax or parameters, use the search tool.

Provide a 1-sentence explanation of what this command does.
Return ONLY the explanation, no questions or additional text."""

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
    
    def _detect_and_decode(self, raw_bytes: bytes, filename: str) -> Tuple[str, str]:
        """Mainframe-aware encoding detection with EBCDIC support.
        
        Args:
            raw_bytes: Raw file bytes
            filename: Name of file (for logging)
            
        Returns:
            Tuple of (decoded_content, encoding_name)
            
        Raises:
            EncodingDetectionError: If no encoding works
        """
        logger.debug(f"Detecting encoding for {filename}")
        
        # Try UTF-8 first (most common)
        try:
            content = raw_bytes.decode('utf-8')
            logger.debug(f"Successfully decoded as UTF-8: {filename}")
            return content, 'utf-8'
        except UnicodeDecodeError:
            logger.debug(f"UTF-8 decode failed, trying other encodings: {filename}")
        
        # Try ASCII
        try:
            content = raw_bytes.decode('ascii')
            logger.debug(f"Decoded as ASCII: {filename}")
            return content, 'ascii'
        except UnicodeDecodeError:
            pass
        
        # Try EBCDIC codepages (mainframe context)
        for encoding in self.EBCDIC_CODEPAGES:
            try:
                content = raw_bytes.decode(encoding)
                # Heuristic: Check for reasonable text content
                if len(content.strip()) > 0:
                    logger.info(f"Decoded as {encoding}: {filename}")
                    return content, encoding
            except (UnicodeDecodeError, LookupError):
                continue
        
        # Fallback to latin-1 with error replacement
        logger.warning(f"Using fallback encoding (latin-1) for {filename}")
        self._warnings.append(f"Could not reliably detect encoding, using latin-1 fallback")
        content = raw_bytes.decode('latin-1', errors='replace')
        return content, 'latin-1'
    
    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        """Parse PARMLIB content string and return structured data.
        
        Args:
            content: Raw PARMLIB content as string
            source_file: Name of the source file
            
        Returns:
            Structured dictionary with parsed data
            
        Raises:
            EmptyFileError: If content is empty
            PARMLIBParseError: If parsing fails
        """
        if not content or not content.strip():
            raise EmptyFileError("PARMLIB content is empty", filename=source_file)
        
        logger.debug(f"Parsing PARMLIB content from {source_file}")
        
        lines = content.split('\n')
        
        # Extract metadata
        metadata = self._extract_metadata(lines, source_file)
        logger.debug(f"Extracted metadata: {metadata['total_lines']} lines")
        
        # Parse content
        try:
            parsed_content = self._parse_content(lines)
            logger.debug(
                f"Parsed content: {len(parsed_content.get('parameters', []))} parameters, "
                f"{len(parsed_content.get('control_statements', []))} control statements"
            )
        except Exception as e:
            logger.error(f"Content parsing failed: {e}", exc_info=True)
            raise PARMLIBParseError(
                f"Failed to parse PARMLIB content: {str(e)}",
                filename=source_file
            ) from e
        
        # Extract dependencies
        try:
            dependencies = self._extract_dependencies(parsed_content)
            logger.debug(f"Extracted {len(dependencies)} dependencies")
        except Exception as e:
            logger.warning(f"Dependency extraction failed: {e}")
            self._warnings.append(f"Failed to extract dependencies: {str(e)}")
            dependencies = []
        
        # Update metadata with section info
        metadata['sections'] = list(parsed_content.get('sections', {}).keys())
        
        return {
            'metadata': metadata,
            'parameters': parsed_content.get('parameters', []),
            'control_statements': parsed_content.get('control_statements', []),
            'sections': parsed_content.get('sections', {}),
            'dependencies': dependencies,
        }
    
    def _extract_metadata(self, lines: List[str], source_file: str) -> Dict[str, Any]:
        """Extract file-level metadata."""
        total_lines = len(lines)
        non_empty_lines = sum(1 for line in lines if line.strip())
        comment_lines = sum(1 for line in lines if self._is_comment(line))
        
        return {
            'source_file': source_file,
            'total_lines': total_lines,
            'non_empty_lines': non_empty_lines,
            'comment_lines': comment_lines,
            'code_lines': non_empty_lines - comment_lines,
            'sections': []
        }
    
    def _parse_content(self, lines: List[str]) -> Dict[str, Any]:
        """Main parsing logic for PARMLIB content.
        
        Args:
            lines: List of content lines
            
        Returns:
            Dictionary with parsed statements organized by type
        """
        content = {
            'parameters': [],
            'control_statements': [],
            'sections': {},
            'raw_statements': []
        }
        
        self.current_section = "MAIN"
        content['sections'][self.current_section] = []
        
        i = 0
        while i < len(lines):
            self.line_number = i + 1
            line = lines[i]
            
            # Handle line continuation
            full_statement, lines_consumed = self._handle_continuation(lines, i)
            
            # Parse the complete statement
            try:
                parsed_stmt = self._parse_statement(full_statement, self.line_number)
                
                if parsed_stmt:
                    content['raw_statements'].append(parsed_stmt)
                    content['sections'][self.current_section].append(parsed_stmt)
                    
                    # Categorize statement
                    if parsed_stmt['type'] == StatementType.PARAMETER.value:
                        content['parameters'].append(parsed_stmt)
                    elif parsed_stmt['type'] in [StatementType.CONTROL_STATEMENT.value, 
                                                 StatementType.UTILITY_COMMAND.value]:
                        content['control_statements'].append(parsed_stmt)
                    elif parsed_stmt['type'] == StatementType.SECTION_HEADER.value:
                        self.current_section = parsed_stmt.get('section_name', 'UNNAMED')
                        if self.current_section not in content['sections']:
                            content['sections'][self.current_section] = []
                            logger.debug(f"Started new section: {self.current_section}")
            
            except InvalidStatementError as e:
                logger.warning(f"Line {self.line_number}: Invalid statement - {e}")
                self._unrecognized.append({
                    "line": self.line_number,
                    "content": full_statement[:100],
                    "error": str(e)
                })
            except Exception as e:
                logger.error(f"Line {self.line_number}: Unexpected error - {e}")
                self._unrecognized.append({
                    "line": self.line_number,
                    "content": full_statement[:100],
                    "error": f"Unexpected error: {str(e)}"
                })
            
            i += lines_consumed
        
        return content
    
    def _handle_continuation(self, lines: List[str], start_idx: int) -> Tuple[str, int]:
        """Handle line continuation in PARMLIB files.
        
        Args:
            lines: List of all lines
            start_idx: Starting line index
            
        Returns:
            Tuple of (complete_statement, lines_consumed)
        """
        statement_parts = []
        idx = start_idx
        lines_consumed = 0
        
        while idx < len(lines):
            line = lines[idx]
            clean_line = line.rstrip()
            
            # Skip empty lines and comments at start
            if not clean_line or self._is_comment(clean_line):
                idx += 1
                lines_consumed += 1
                if not statement_parts:
                    return clean_line, lines_consumed
                continue
            
            # Check for continuation character
            has_continuation = any(clean_line.endswith(char) 
                                  for char in self.CONTINUATION_CHARS)
            
            if has_continuation:
                # Remove continuation character
                for char in self.CONTINUATION_CHARS:
                    if clean_line.endswith(char):
                        clean_line = clean_line[:-1].rstrip()
                        break
            
            statement_parts.append(clean_line)
            idx += 1
            lines_consumed += 1
            
            if not has_continuation:
                break
        
        if not lines_consumed:
            lines_consumed = 1
        
        return ' '.join(statement_parts), lines_consumed
    
    def _parse_statement(self, statement: str, line_number: int) -> Optional[Dict[str, Any]]:
        """Parse a single PARMLIB statement.
        
        Args:
            statement: Complete statement text
            line_number: Line number in source file
            
        Returns:
            Dictionary with parsed statement data, or None
            
        Raises:
            InvalidStatementError: If statement has invalid syntax
        """
        # Handle comments
        if not statement or self._is_comment(statement):
            if statement.strip():
                return {
                    'type': StatementType.COMMENT.value,
                    'line_number': line_number,
                    'raw_text': statement,
                    'content': statement.lstrip('*/ \t')
                }
            return None
        
        parsed = {
            'type': StatementType.UNKNOWN.value,
            'line_number': line_number,
            'raw_text': statement,
        }
        
        # Section header
        if self._is_section_header(statement):
            parsed['type'] = StatementType.SECTION_HEADER.value
            parsed['section_name'] = self._extract_section_name(statement)
            return parsed
        
        # Conditional statement
        if self.CONDITIONAL_PATTERN.match(statement):
            parsed['type'] = StatementType.CONDITIONAL.value
            match = self.CONDITIONAL_PATTERN.match(statement)
            parsed['conditional_type'] = match.group(1).upper()
            parsed['condition'] = statement
            return parsed
        
        # Utility command
        utility_type = self._identify_utility_command(statement)
        if utility_type:
            parsed['type'] = StatementType.UTILITY_COMMAND.value
            parsed['utility'] = utility_type
            parsed['command'] = statement
            
            try:
                utility_details = self._parse_utility_command(statement, utility_type)
                parsed.update(utility_details)
            except UtilityCommandError as e:
                logger.warning(f"Line {line_number}: Utility command parse failed - {e}")
                self._warnings.append(f"Line {line_number}: Failed to parse {utility_type} command")
            
            return parsed
        
        # Parameter assignment
        param_match = self._match_parameter(statement)
        if param_match:
            parsed['type'] = StatementType.PARAMETER.value
            parsed['parameter_name'] = param_match['name']
            parsed['parameter_value'] = param_match['value']
            parsed['quoted'] = param_match['quoted']
            
            if self._contains_dataset(param_match['value']):
                parsed['contains_dataset'] = True
                parsed['referenced_datasets'] = self._extract_datasets(param_match['value'])
            
            return parsed
        
        # Dataset reference
        if self._contains_dataset(statement):
            parsed['type'] = StatementType.DATASET_REFERENCE.value
            parsed['referenced_datasets'] = self._extract_datasets(statement)
            return parsed
        
        # Default: control statement
        parsed['type'] = StatementType.CONTROL_STATEMENT.value
        parsed['statement'] = statement
        
        return parsed
    
    def _is_comment(self, line: str) -> bool:
        """Check if a line is a comment."""
        stripped = line.lstrip()
        if not stripped:
            return True
        return any(stripped.startswith(char) for char in ['*', '//', '#'])
    
    def _is_section_header(self, statement: str) -> bool:
        """Check if statement is a section header."""
        patterns = [
            r'^\s*\/\*\s*[A-Z_][A-Z0-9_\s]*\*\/$',
            r'^\s*\*+\s*[A-Z_][A-Z0-9_\s]*\*+$',
            r'^\s*={3,}.*={3,}$',
            r'^\s*-{3,}.*-{3,}$',
        ]
        return any(re.match(pattern, statement, re.IGNORECASE) for pattern in patterns)
    
    def _extract_section_name(self, statement: str) -> str:
        """Extract section name from header."""
        name = re.sub(r'[/*=\-]+', '', statement)
        return name.strip()
    
    def _match_parameter(self, statement: str) -> Optional[Dict[str, Any]]:
        """Match parameter assignment patterns."""
        for pattern in self.PARAM_PATTERNS:
            match = pattern.match(statement.strip())
            if match:
                name = match.group(1).strip()
                value = match.group(2).strip()
                
                quoted = False
                if value:
                    if (value.startswith("'") and value.endswith("'")) or \
                       (value.startswith('"') and value.endswith('"')):
                        quoted = True
                        value = value[1:-1]
                
                return {'name': name, 'value': value, 'quoted': quoted}
        return None
    
    def _identify_utility_command(self, statement: str) -> Optional[str]:
        """Identify utility command type."""
        statement_upper = statement.upper().strip()
        
        # SORT utility
        if self.SORT_PATTERN.match(statement) or \
           statement_upper.startswith('SORT ') or \
           statement_upper.startswith('OUTREC') or \
           statement_upper.startswith('INREC') or \
           statement_upper.startswith('INCLUDE') or \
           statement_upper.startswith('OMIT'):
            return 'SORT'
        
        # IDCAMS utility
        if self.IDCAMS_PATTERN.match(statement):
            return 'IDCAMS'
        
        # IEBGENER
        if statement_upper.startswith('GENERATE'):
            return 'IEBGENER'
        
        # IEFBR14
        if 'IEFBR14' in statement_upper:
            return 'IEFBR14'
        
        return None
    
    def _parse_utility_command(self, statement: str, utility_type: str) -> Dict[str, Any]:
        """Parse utility-specific commands.
        
        Args:
            statement: Statement text
            utility_type: Type of utility (SORT, IDCAMS, etc.)
            
        Returns:
            Dictionary with utility-specific details
            
        Raises:
            UtilityCommandError: If parsing fails
        """
        details = {}
        
        try:
            if utility_type == 'SORT':
                match = self.SORT_PATTERN.match(statement)
                if match:
                    fields_str = match.group(1)
                    field_specs = re.findall(r'(\d+),(\d+),([A-Z]+),([AD])', fields_str, re.IGNORECASE)
                    details['sort_fields'] = []
                    for spec in field_specs:
                        details['sort_fields'].append({
                            'position': int(spec[0]),
                            'length': int(spec[1]),
                            'format': spec[2].upper(),
                            'order': 'ASCENDING' if spec[3].upper() == 'A' else 'DESCENDING'
                        })
            
            elif utility_type == 'IDCAMS':
                match = self.IDCAMS_PATTERN.match(statement)
                if match:
                    details['idcams_command'] = match.group(1).upper()
                    obj_match = re.search(r'([A-Z0-9.]+)\s*(?:\(|PURGE|CLUSTER)', 
                                         statement, re.IGNORECASE)
                    if obj_match:
                        details['object_name'] = obj_match.group(1)
        
        except Exception as e:
            raise UtilityCommandError(
                f"Failed to parse {utility_type} command: {str(e)}",
                filename=None,
                line_number=self.line_number,
                content=statement[:100]
            ) from e
        
        return details
    
    def _contains_dataset(self, text: str) -> bool:
        """Check if text contains dataset reference."""
        if any(keyword in text.upper() for keyword in self.DATASET_KEYWORDS):
            return True
        return bool(self.DATASET_PATTERN.search(text))
    
    def _extract_datasets(self, text: str) -> List[str]:
        """Extract dataset names from text."""
        datasets = []
        matches = self.DATASET_PATTERN.findall(text)
        for match in matches:
            # Filter out common false positives
            if match.upper() not in ['SORT', 'FIELDS', 'OUTREC', 'INREC']:
                datasets.append(match)
        return list(set(datasets))
    
    def _extract_dependencies(self, parsed_content: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract dependencies from parsed content.
        
        Args:
            parsed_content: Parsed content dictionary
            
        Returns:
            List of dependency dictionaries
        """
        dependencies = []
        
        # Extract dataset dependencies
        for stmt in parsed_content.get('raw_statements', []):
            if stmt.get('type') == StatementType.DATASET_REFERENCE.value or \
               stmt.get('contains_dataset'):
                for ds_name in stmt.get('referenced_datasets', []):
                    dependencies.append({
                        'type': 'DATASET',
                        'name': ds_name,
                        'line_number': stmt['line_number'],
                        'context': stmt.get('raw_text', '')[:100]
                    })
        
        # Extract program references
        for stmt in parsed_content.get('raw_statements', []):
            programs = self._extract_program_references(stmt.get('raw_text', ''))
            for prog in programs:
                dependencies.append({
                    'type': 'PROGRAM',
                    'name': prog,
                    'line_number': stmt['line_number'],
                    'context': stmt.get('raw_text', '')[:100]
                })
        
        # Extract utility dependencies
        for stmt in parsed_content.get('control_statements', []):
            if stmt.get('utility'):
                dependencies.append({
                    'type': 'UTILITY',
                    'name': stmt['utility'],
                    'line_number': stmt['line_number'],
                    'command': stmt.get('command', '')[:100]
                })
        
        return dependencies
    
    def _extract_program_references(self, text: str) -> List[str]:
        """Extract program references from text."""
        programs = []
        for pattern in self.PROGRAM_PATTERNS:
            matches = pattern.findall(text)
            programs.extend(matches)
        return list(set(programs))


# ============================================================================
# CLI Entry Point
# ============================================================================

def main():
    """Command-line interface for PARMLIB parser."""
    parser = argparse.ArgumentParser(
        description='Parse PARMLIB control member files to JSON',
        epilog='Part of COBOL-to-.NET migration toolkit'
    )
    parser.add_argument('input', help='Input PARMLIB file or folder path')
    parser.add_argument('-o', '--output', help='Output JSON file (default: stdout)')
    parser.add_argument('--indent', type=int, default=2, help='JSON indent level')
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable verbose logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger('parmlib_parser').setLevel(logging.DEBUG)
    
    try:
        parmlib_parser = PARMLIBParser()
        input_path = Path(args.input)
        results = []
        
        # Handle file or folder
        if input_path.is_file():
            result = parmlib_parser.parse_file(str(input_path))
            results.append(result)
        elif input_path.is_dir():
            logger.info(f"Processing folder: {input_path}")
            for file_path in input_path.rglob('*'):
                if file_path.is_file() and not file_path.name.startswith('.'):
                    try:
                        result = parmlib_parser.parse_file(str(file_path))
                        results.append(result)
                    except Exception as e:
                        logger.error(f"Failed to parse {file_path}: {e}")
                        results.append({
                            "file": str(file_path),
                            "error": str(e)
                        })
        else:
            print(f"Error: {args.input} is neither a file nor a directory", file=sys.stderr)
            return 1
        
        # Output results
        output_json = json.dumps(results if len(results) > 1 else results[0], 
                                indent=args.indent, ensure_ascii=False)
        
        if args.output:
            Path(args.output).write_text(output_json, encoding='utf-8')
            logger.info(f"Output written to: {args.output}")
            print(f"Output written to: {args.output}")
        else:
            print(output_json)
        
        return 0
        
    except FileNotFoundError as e:
        logger.error(str(e))
        print(f"Error: {e}", file=sys.stderr)
        return 1
    except EmptyFileError as e:
        logger.error(str(e))
        print(f"Error: {e}", file=sys.stderr)
        return 2
    except PARMLIBParseError as e:
        logger.error(str(e))
        print(f"Parse Error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 4
    except Exception as e:
        logger.exception(f"Unexpected error: {e}")
        print(f"Error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 4


if __name__ == '__main__':
    sys.exit(main())