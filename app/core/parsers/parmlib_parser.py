"""
PARMLIB Parser - Mainframe Control Member Parser
Enhanced dependency extraction for proper integration with dependency graph.

Key improvements:
- Structured dependency extraction matching framework expectations
- Better program/utility detection
- JCL job reference tracking
- System parameter categorization
- Symbolic substitution tracking
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
# Logging Configuration
# ============================================================================

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('parmlib_parser')

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
    AUGMENTATION_TIMEOUT = 60
    
    # Enhanced Regex Patterns
    DATASET_PATTERN = re.compile(
        r'\b([A-Z][A-Z0-9$#@]{0,7}(?:\.[A-Z0-9$#@]{1,8}){1,21})\b'
    )
    
    PARAM_PATTERNS = [
        re.compile(r'^([A-Z_][A-Z0-9_]*)\s*=\s*(.+)$', re.IGNORECASE),
        re.compile(r'^([A-Z_][A-Z0-9_]*)\s+(.+)$', re.IGNORECASE),
    ]
    
    # Utility detection patterns
    SORT_PATTERN = re.compile(r'^\s*SORT\s+FIELDS\s*=\s*\((.+)\)', re.IGNORECASE)
    IDCAMS_PATTERN = re.compile(
        r'^\s*(DEFINE|DELETE|ALTER|LISTCAT|PRINT|REPRO|EXPORT|IMPORT)\s+', 
        re.IGNORECASE
    )
    IEBGENER_PATTERN = re.compile(r'^\s*GENERATE\b', re.IGNORECASE)
    IEFBR14_PATTERN = re.compile(r'\bIEFBR14\b', re.IGNORECASE)
    IEBCOPY_PATTERN = re.compile(r'^\s*COPY\s+(OUTDD|INDD)\s*=', re.IGNORECASE)
    
    # Program reference patterns (enhanced)
    PROGRAM_PATTERNS = [
        re.compile(r'(?:PGM|PROGRAM)\s*=\s*([A-Z][A-Z0-9]{0,7})', re.IGNORECASE),
        re.compile(r'(?:EXEC)\s+([A-Z][A-Z0-9]{0,7})(?:\s|,|$)', re.IGNORECASE),
        re.compile(r'(?:CALL)\s+["\']?([A-Z][A-Z0-9]{0,7})["\']?', re.IGNORECASE),
        re.compile(r'(?:LOAD|ATTACH)\s+["\']?([A-Z][A-Z0-9]{0,7})["\']?', re.IGNORECASE),
    ]
    
    # JCL job reference patterns
    JCL_PATTERNS = [
        re.compile(r'(?:JOB|JOBNAME)\s*=\s*([A-Z][A-Z0-9]{0,7})', re.IGNORECASE),
        re.compile(r'(?:SUBMIT)\s+([A-Z][A-Z0-9.]+)', re.IGNORECASE),
        re.compile(r'(?:PROC)\s*=\s*([A-Z][A-Z0-9]{0,7})', re.IGNORECASE),
    ]
    
    # System parameter categories
    SYSTEM_PARAM_CATEGORIES = {
        'REGION': 'RESOURCE',
        'TIME': 'RESOURCE',
        'SPACE': 'RESOURCE',
        'UNIT': 'RESOURCE',
        'VOL': 'RESOURCE',
        'VOLUME': 'RESOURCE',
        'CLASS': 'SCHEDULING',
        'MSGCLASS': 'SCHEDULING',
        'PRTY': 'SCHEDULING',
        'COND': 'CONTROL',
        'RESTART': 'CONTROL',
        'TYPRUN': 'CONTROL',
        'SYSOUT': 'OUTPUT',
        'OUTLIM': 'OUTPUT',
    }
    
    # Symbolic substitution pattern
    SYMBOL_PATTERN = re.compile(r'&([A-Z][A-Z0-9_]*)', re.IGNORECASE)
    SYMBOL_DEFINE_PATTERN = re.compile(
        r'(?:SET|SYMBOL)\s+&?([A-Z][A-Z0-9_]*)\s*=\s*(.+)', 
        re.IGNORECASE
    )
    
    DATASET_KEYWORDS = [
        'DSN', 'DSNAME', 'FILE', 'INPUT', 'OUTPUT', 'DATASET', 'DD',
        'SYSIN', 'SYSPRINT', 'SYSOUT', 'SORTIN', 'SORTOUT'
    ]
    
    CONDITIONAL_PATTERN = re.compile(
        r'^\s*(IF|WHEN|ELSE|ENDIF|END-IF|THEN)\b', 
        re.IGNORECASE
    )
    
    CONTINUATION_CHARS = [',', '-', '+']
    
    def __init__(self):
        """Initialize PARMLIB parser."""
        self.current_section = "MAIN"
        self.line_number = 0
        
        # Error tracking
        self._warnings: List[str] = []
        self._unrecognized: List[Dict[str, Any]] = []
        
        # Dependency tracking during parse
        self._temp_dependencies = {
            'programs': [],
            'datasets': [],
            'jcl_references': [],
            'utilities': [],
            'system_parameters': [],
            'symbols': []
        }
        
        logger.info("Initialized PARMLIBParser")
    
    def parse_file(self, filepath: str, **kwargs) -> dict:
        """Parse a PARMLIB file from filesystem.
        
        Returns structured data with proper dependency extraction for framework.
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
        
        # Reset tracking
        self._warnings = []
        self._unrecognized = []
        self._temp_dependencies = {
            'programs': [],
            'datasets': [],
            'jcl_references': [],
            'utilities': [],
            'system_parameters': [],
            'symbols': []
        }
        self.current_section = "MAIN"
        self.line_number = 0
        
        try:
            raw_bytes = path.read_bytes()
            content, encoding = self._detect_and_decode(raw_bytes, path.name)
            logger.info(f"Detected encoding: {encoding}")
            
            result = self.parse_string(content, source_file=path.name)
            
            # Add framework-required fields
            result['encoding'] = encoding
            result['file_type'] = self.FILE_TYPE
            result['source_file'] = path.name
            result['member_name'] = path.stem  # Add member_name for extractor
            
            # Add error tracking
            result['_warnings'] = self._warnings
            result['_unrecognized'] = self._unrecognized
            
            logger.info(
                f"Successfully parsed {filepath}: "
                f"{result['metadata']['total_lines']} lines, "
                f"{len(result.get('parameters', []))} parameters, "
                f"{len(result.get('dependencies', {}).get('programs', []))} programs, "
                f"{len(result.get('dependencies', {}).get('datasets', []))} datasets"
            )
            
            return result
            
        except (EmptyFileError, EncodingDetectionError):
            raise
        except Exception as e:
            logger.error(f"Failed to parse {filepath}: {e}", exc_info=True)
            raise PARMLIBParseError(
                f"PARMLIB parsing failed: {str(e)}",
                filename=path.name
            ) from e

    def _detect_and_decode(self, raw_bytes: bytes, filename: str) -> Tuple[str, str]:
        """Mainframe-aware encoding detection with EBCDIC support."""
        logger.debug(f"Detecting encoding for {filename}")
        
        # Try UTF-8 first
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
        
        # Try EBCDIC codepages
        for encoding in self.EBCDIC_CODEPAGES:
            try:
                content = raw_bytes.decode(encoding)
                if len(content.strip()) > 0:
                    logger.info(f"Decoded as {encoding}: {filename}")
                    return content, encoding
            except (UnicodeDecodeError, LookupError):
                continue
        
        # Fallback
        logger.warning(f"Using fallback encoding (latin-1) for {filename}")
        self._warnings.append("Could not reliably detect encoding, using latin-1 fallback")
        content = raw_bytes.decode('latin-1', errors='replace')
        return content, 'latin-1'
    
    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        """Parse PARMLIB content string and return structured data."""
        if not content or not content.strip():
            raise EmptyFileError("PARMLIB content is empty", filename=source_file)
        
        logger.debug(f"Parsing PARMLIB content from {source_file}")
        
        lines = content.split('\n')
        
        metadata = self._extract_metadata(lines, source_file)
        
        try:
            parsed_content = self._parse_content(lines)
        except Exception as e:
            logger.error(f"Content parsing failed: {e}", exc_info=True)
            raise PARMLIBParseError(
                f"Failed to parse PARMLIB content: {str(e)}",
                filename=source_file
            ) from e
        
        # Build structured dependencies from collected data
        dependencies = self._build_dependencies()
        
        metadata['sections'] = list(parsed_content.get('sections', {}).keys())
        
        return {
            'metadata': metadata,
            'parameters': parsed_content.get('parameters', []),
            'control_statements': parsed_content.get('control_statements', []),
            'sections': parsed_content.get('sections', {}),
            'dependencies': dependencies,  # Structured for extractor
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
        """Main parsing logic for PARMLIB content."""
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
            
            full_statement, lines_consumed = self._handle_continuation(lines, i)
            
            try:
                parsed_stmt = self._parse_statement(full_statement, self.line_number)
                
                if parsed_stmt:
                    content['raw_statements'].append(parsed_stmt)
                    content['sections'][self.current_section].append(parsed_stmt)
                    
                    # Categorize statement
                    if parsed_stmt['type'] == StatementType.PARAMETER.value:
                        content['parameters'].append(parsed_stmt)
                        # Track system parameters
                        self._track_system_parameter(parsed_stmt)
                        
                    elif parsed_stmt['type'] in [
                        StatementType.CONTROL_STATEMENT.value, 
                        StatementType.UTILITY_COMMAND.value
                    ]:
                        content['control_statements'].append(parsed_stmt)
                        # Track utility
                        if parsed_stmt.get('utility'):
                            self._track_utility(parsed_stmt)
                    
                    elif parsed_stmt['type'] == StatementType.SECTION_HEADER.value:
                        self.current_section = parsed_stmt.get('section_name', 'UNNAMED')
                        if self.current_section not in content['sections']:
                            content['sections'][self.current_section] = []
                    
                    # Track dependencies from any statement
                    self._extract_statement_dependencies(parsed_stmt)
            
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
        """Handle line continuation in PARMLIB files."""
        statement_parts = []
        idx = start_idx
        lines_consumed = 0
        
        while idx < len(lines):
            line = lines[idx]
            clean_line = line.rstrip()
            
            if not clean_line or self._is_comment(clean_line):
                idx += 1
                lines_consumed += 1
                if not statement_parts:
                    return clean_line, lines_consumed
                continue
            
            has_continuation = any(
                clean_line.endswith(char) for char in self.CONTINUATION_CHARS
            )
            
            if has_continuation:
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
    
    def _parse_statement(
        self, statement: str, line_number: int
    ) -> Optional[Dict[str, Any]]:
        """Parse a single PARMLIB statement."""
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
        
        # Utility command (check BEFORE parameter matching)
        utility_type = self._identify_utility_command(statement)
        if utility_type:
            parsed['type'] = StatementType.UTILITY_COMMAND.value
            parsed['utility'] = utility_type
            parsed['command'] = statement
            
            try:
                utility_details = self._parse_utility_command(statement, utility_type)
                parsed.update(utility_details)
            except UtilityCommandError as e:
                logger.warning(f"Line {line_number}: Utility parse failed - {e}")
                self._warnings.append(
                    f"Line {line_number}: Failed to parse {utility_type} command"
                )
            
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
                parsed['referenced_datasets'] = self._extract_datasets(
                    param_match['value']
                )
            
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
           any(statement_upper.startswith(kw) for kw in [
               'SORT ', 'OUTREC', 'INREC', 'INCLUDE', 'OMIT'
           ]):
            return 'SORT'
        
        # IDCAMS utility
        if self.IDCAMS_PATTERN.match(statement):
            return 'IDCAMS'
        
        # IEBGENER
        if self.IEBGENER_PATTERN.match(statement):
            return 'IEBGENER'
        
        # IEBCOPY
        if self.IEBCOPY_PATTERN.match(statement):
            return 'IEBCOPY'
        
        # IEFBR14
        if self.IEFBR14_PATTERN.search(statement):
            return 'IEFBR14'
        
        return None
    
    def _parse_utility_command(
        self, statement: str, utility_type: str
    ) -> Dict[str, Any]:
        """Parse utility-specific commands."""
        details = {}
        
        try:
            if utility_type == 'SORT':
                match = self.SORT_PATTERN.match(statement)
                if match:
                    fields_str = match.group(1)
                    field_specs = re.findall(
                        r'(\d+),(\d+),([A-Z]+),([AD])', 
                        fields_str, 
                        re.IGNORECASE
                    )
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
                    obj_match = re.search(
                        r'([A-Z][A-Z0-9.]+)\s*(?:\(|PURGE|CLUSTER)', 
                        statement, 
                        re.IGNORECASE
                    )
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
        
        # Common false positives to filter
        FILTER_OUT = {
            'SORT', 'FIELDS', 'OUTREC', 'INREC', 'INCLUDE', 'OMIT',
            'DEFINE', 'DELETE', 'ALTER', 'LISTCAT', 'PRINT', 'REPRO'
        }
        
        for match in matches:
            if match.upper() not in FILTER_OUT:
                datasets.append(match)
        
        return list(set(datasets))
    
    def _extract_statement_dependencies(self, stmt: Dict[str, Any]) -> None:
        """Extract dependencies from a parsed statement and track them."""
        raw_text = stmt.get('raw_text', '')
        line_num = stmt.get('line_number', 0)
        
        # Extract datasets
        if stmt.get('referenced_datasets'):
            for ds in stmt['referenced_datasets']:
                self._temp_dependencies['datasets'].append({
                    'name': ds,
                    'line': line_num,
                    'context': raw_text[:100],
                    'type': self._classify_dataset_type(ds)
                })
        
        # Extract programs
        programs = self._extract_program_references(raw_text)
        for prog in programs:
            self._temp_dependencies['programs'].append({
                'name': prog,
                'line': line_num,
                'purpose': self._infer_program_purpose(raw_text),
                'context': raw_text[:100]
            })
        
        # Extract JCL references
        jcl_refs = self._extract_jcl_references(raw_text)
        for jcl in jcl_refs:
            self._temp_dependencies['jcl_references'].append({
                'job_name': jcl,
                'line': line_num,
                'usage': self._infer_jcl_usage(raw_text),
                'context': raw_text[:100]
            })
        
        # Extract symbolic substitutions
        symbols = self._extract_symbolic_substitutions(raw_text)
        for symbol in symbols:
            if symbol['type'] == 'DEFINITION':
                self._temp_dependencies['symbols'].append({
                    'name': symbol['name'],
                    'value': symbol.get('value', ''),
                    'line': line_num,
                    'scope': 'GLOBAL'  # Default for PARMLIB
                })
    
    def _track_system_parameter(self, param_stmt: Dict[str, Any]) -> None:
        """Track system parameters for dependency extraction."""
        param_name = param_stmt.get('parameter_name', '').upper()
        param_value = param_stmt.get('parameter_value', '')
        
        # Check if this is a system-level parameter
        category = None
        for key_prefix, cat in self.SYSTEM_PARAM_CATEGORIES.items():
            if param_name.startswith(key_prefix):
                category = cat
                break
        
        if category:
            self._temp_dependencies['system_parameters'].append({
                'name': param_name,
                'value': param_value,
                'category': category,
                'line': param_stmt.get('line_number')
            })
    
    def _track_utility(self, stmt: Dict[str, Any]) -> None:
        """Track utility usage for dependency extraction."""
        utility = stmt.get('utility')
        if utility:
            # Check if already tracked
            existing = next(
                (u for u in self._temp_dependencies['utilities'] 
                 if u['name'] == utility and u['line'] == stmt.get('line_number')),
                None
            )
            
            if not existing:
                self._temp_dependencies['utilities'].append({
                    'name': utility,
                    'line': stmt.get('line_number'),
                    'command': stmt.get('command', '')[:100]
                })
    
    def _extract_program_references(self, text: str) -> List[str]:
        """Extract program references from text."""
        programs = []
        for pattern in self.PROGRAM_PATTERNS:
            matches = pattern.findall(text)
            programs.extend(matches)
        
        # Filter out common false positives
        FILTER_OUT = {'SORT', 'IDCAMS', 'IEBGENER', 'IEBCOPY', 'GENERATE'}
        return [p for p in set(programs) if p.upper() not in FILTER_OUT]
    
    def _extract_jcl_references(self, text: str) -> List[str]:
        """Extract JCL job/proc references from text."""
        jcl_refs = []
        for pattern in self.JCL_PATTERNS:
            matches = pattern.findall(text)
            jcl_refs.extend(matches)
        return list(set(jcl_refs))
    
    def _extract_symbolic_substitutions(self, text: str) -> List[Dict[str, Any]]:
        """Extract symbolic parameter definitions and references."""
        symbols = []
        
        # Look for definitions (SET &VAR = value)
        define_match = self.SYMBOL_DEFINE_PATTERN.search(text)
        if define_match:
            symbols.append({
                'type': 'DEFINITION',
                'name': define_match.group(1),
                'value': define_match.group(2).strip()
            })
        
        # Look for references (&VAR usage)
        ref_matches = self.SYMBOL_PATTERN.findall(text)
        for ref in ref_matches:
            symbols.append({
                'type': 'REFERENCE',
                'name': ref,
                'value': None
            })
        
        return symbols
    
    def _classify_dataset_type(self, dataset_name: str) -> str:
        """Classify dataset type based on naming conventions."""
        upper_ds = dataset_name.upper()
        
        # Check for common patterns
        if '.GDG' in upper_ds or upper_ds.endswith('G0000V00'):
            return 'GDG'
        elif '.VSAM.' in upper_ds or '.CLUSTER.' in upper_ds:
            return 'VSAM'
        elif any(kw in upper_ds for kw in ['.CSV', '.TXT', '.DAT']):
            return 'FLAT'
        elif '.PDS' in upper_ds or '.LOAD' in upper_ds:
            return 'PDS'
        else:
            return 'SEQUENTIAL'
    
    def _infer_program_purpose(self, context: str) -> Optional[str]:
        """Infer program purpose from context."""
        context_upper = context.upper()
        
        if 'EXEC' in context_upper:
            return 'EXECUTION'
        elif 'CALL' in context_upper:
            return 'SUBROUTINE_CALL'
        elif 'LOAD' in context_upper or 'ATTACH' in context_upper:
            return 'DYNAMIC_LOAD'
        else:
            return 'REFERENCE'
    
    def _infer_jcl_usage(self, context: str) -> str:
        """Infer how JCL is being used from context."""
        context_upper = context.upper()
        
        if 'SUBMIT' in context_upper:
            return 'SUBMIT'
        elif 'PROC' in context_upper:
            return 'PROCEDURE'
        elif 'JOB' in context_upper:
            return 'JOB_REFERENCE'
        else:
            return 'REFERENCE'
    
    def _build_dependencies(self) -> Dict[str, List[Dict[str, Any]]]:
        """Build final structured dependencies for extractor consumption.
        
        This structure matches what the extractor expects.
        """
        # Deduplicate and clean up collected dependencies
        
        # Programs - deduplicate by name
        programs_map = {}
        for prog in self._temp_dependencies['programs']:
            key = prog['name'].upper()
            if key not in programs_map:
                programs_map[key] = prog
        
        # Datasets - deduplicate by name
        datasets_map = {}
        for ds in self._temp_dependencies['datasets']:
            key = ds['name'].upper()
            if key not in datasets_map:
                datasets_map[key] = ds
            else:
                # Keep the one with more context
                if len(ds.get('context', '')) > len(datasets_map[key].get('context', '')):
                    datasets_map[key] = ds
        
        # JCL References - deduplicate by job_name
        jcl_map = {}
        for jcl in self._temp_dependencies['jcl_references']:
            key = jcl['job_name'].upper()
            if key not in jcl_map:
                jcl_map[key] = jcl
        
        # System Parameters - deduplicate by name
        sysparm_map = {}
        for param in self._temp_dependencies['system_parameters']:
            key = param['name'].upper()
            if key not in sysparm_map:
                sysparm_map[key] = param
        
        # Symbols - deduplicate by name, prefer definitions over references
        symbols_map = {}
        for symbol in self._temp_dependencies['symbols']:
            key = symbol['name'].upper()
            if key not in symbols_map:
                symbols_map[key] = symbol
            elif symbol.get('value') and not symbols_map[key].get('value'):
                # Prefer definition over reference
                symbols_map[key] = symbol
        
        # Utilities are already unique enough
        
        return {
            'programs': list(programs_map.values()),
            'datasets': list(datasets_map.values()),
            'jcl_references': list(jcl_map.values()),
            'system_parameters': list(sysparm_map.values()),
            'symbols': list(symbols_map.values()),
            'utilities': self._temp_dependencies['utilities']
        }
    
    # ========================================================================
    # LLM Augmentation Methods (Keep your existing implementation)
    # ========================================================================
    
    def _create_augmentation_agent(self):
        """Create LangGraph ReAct agent with PARMLIB RAG tool access."""
        from langgraph.prebuilt import create_react_agent
        from app.config.llm import get_llm, DOCGEN
        from app.core.tools.rag_tools import search_parmlib_docs
        
        return create_react_agent(model=get_llm(DOCGEN), tools=[search_parmlib_docs])
    
    async def augment(self, parsed_data: dict) -> dict:
        """Augment parsed PARMLIB data with LLM-generated metadata."""
        agent = self._create_augmentation_agent()
        
        # 1. Add file-level description
        try:
            parsed_data["description"] = await self._augment_parmlib_file(
                agent, parsed_data
            )
        except Exception as e:
            logger.error(f"Failed to augment file description: {e}")
            parsed_data["description"] = None
            parsed_data["_augmentation_errors"] = parsed_data.get(
                "_augmentation_errors", []
            )
            parsed_data["_augmentation_errors"].append(
                f"File description failed: {e}"
            )
        
        # 2. Add section summaries
        if "sections" in parsed_data:
            for section_name, section_stmts in parsed_data['sections'].items():
                if section_name != "MAIN" and len(section_stmts) > 0:
                    try:
                        section_code = self._extract_section_code(
                            section_name, section_stmts
                        )
                        
                        section_summary = await self._augment_section(
                            agent, section_name, section_stmts, 
                            parsed_data, section_code
                        )
                        
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
                        parsed_data["_augmentation_errors"] = parsed_data.get(
                            "_augmentation_errors", []
                        )
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
                    parsed_data["_augmentation_errors"] = parsed_data.get(
                        "_augmentation_errors", []
                    )
                    parsed_data["_augmentation_errors"].append(
                        f"Utility {stmt.get('utility')} at line "
                        f"{stmt.get('line_number')} failed: {e}"
                    )
        
        return parsed_data
    
    async def _augment_parmlib_file(self, agent, parsed_data: dict) -> str:
        """Generate file-level description using LangGraph agent."""
        source_file = parsed_data.get("source_file", "UNKNOWN")
        deps = parsed_data.get("dependencies", {})
        
        param_count = len(parsed_data.get("parameters", []))
        control_count = len(parsed_data.get("control_statements", []))
        
        utilities = list(set([
            u['name'] for u in deps.get('utilities', [])
        ]))
        
        datasets = list(set([
            d['name'] for d in deps.get('datasets', [])
        ]))[:5]
        
        programs = list(set([
            p['name'] for p in deps.get('programs', [])
        ]))[:5]
        
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
        """Extract the code for a section from section statements."""
        if not section_stmts:
            return ""
        
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
        """Generate section summary using LangGraph agent."""
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
        """Generate utility command explanation using LangGraph agent."""
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
            print(f"Error: {args.input} is neither a file nor a directory", 
                  file=sys.stderr)
            return 1
        
        output_json = json.dumps(
            results if len(results) > 1 else results[0], 
            indent=args.indent, 
            ensure_ascii=False
        )
        
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