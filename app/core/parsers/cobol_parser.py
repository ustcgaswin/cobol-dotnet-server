"""
COBOL Program Parser for IBM Enterprise COBOL for z/OS Version 4

Parses COBOL source files and outputs comprehensive JSON including:
- IDENTIFICATION DIVISION (program-id, author, etc.)
- ENVIRONMENT DIVISION (configuration, file-control)
- DATA DIVISION (file section, working-storage, linkage section)
- PROCEDURE DIVISION (sections, paragraphs, statements)
- Dependencies (CALLs, PERFORMs, SQL, file I/O, copybooks)

Uses legacylens-cobol-parser for dependency extraction and custom
parsing for complete division coverage.

Usage:
    python cobol-parser.py <cobol_file> [-o output.json]
"""

import re
import json
import sys
import logging
import argparse
from pathlib import Path
from abc import ABC, abstractmethod

from app.core.parsers.base import BaseParser
from app.core.exceptions import CobolParseError, InvalidSyntaxError

# Configure structured logging
logging.basicConfig(
    level=logging.WARNING,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger('cobol_parser')


# =============================================================================
# Error Codes
# =============================================================================

class ExitCode:
    """Exit codes for the parser"""
    SUCCESS = 0
    FILE_NOT_FOUND = 1
    EMPTY_FILE = 2
    ENCODING_ERROR = 3
    PARSE_ERROR = 4
    INVALID_INPUT = 5
    OUTPUT_ERROR = 6




# Try to import legacylens-cobol-parser
try:
    from cobol_parser import parse_string as legacylens_parse
    HAS_LEGACYLENS = True
except ImportError:
    HAS_LEGACYLENS = False
    logger.info("legacylens-cobol-parser not available, using custom dependency extraction")


# =============================================================================
# Parser Registry - Extensible Plugin Architecture
# =============================================================================

class ParserRegistry:
    """Registry for division parsers - enables easy extension"""
    _parsers: dict[str, type] = {}
    
    @classmethod
    def register(cls, name: str):
        """Decorator to register a parser"""
        def decorator(parser_class):
            cls._parsers[name] = parser_class
            return parser_class
        return decorator
    
    @classmethod
    def get_parser(cls, name: str):
        """Get a registered parser by name"""
        return cls._parsers.get(name)
    
    @classmethod
    def all_parsers(cls) -> dict[str, type]:
        """Get all registered parsers"""
        return cls._parsers.copy()


class BaseDivisionParser(ABC):
    """Base class for all division parsers"""
    
    def __init__(self):
        self._unrecognized: list[dict] = []
    
    @abstractmethod
    def parse(self, source: str, lines: list[tuple]) -> dict:
        """Parse the division and return a dictionary of extracted data"""
        pass
    
    @property
    def unrecognized(self) -> list[dict]:
        """Return list of unrecognized patterns"""
        return self._unrecognized


# =============================================================================
# IDENTIFICATION DIVISION Parser
# =============================================================================

@ParserRegistry.register("identification")
class IdentificationDivisionParser(BaseDivisionParser):
    """Parser for IDENTIFICATION DIVISION"""
    
    CLAUSES = [
        ('program_id', r'PROGRAM-ID\.\s*([A-Za-z][A-Za-z0-9_-]*)'),
        ('author', r'AUTHOR\.\s*(.+?)(?:\.|$)'),
        ('installation', r'INSTALLATION\.\s*(.+?)(?:\.|$)'),
        ('date_written', r'DATE-WRITTEN\.\s*(.+?)(?:\.|$)'),
        ('date_compiled', r'DATE-COMPILED\.\s*(.+?)(?:\.|$)'),
        ('security', r'SECURITY\.\s*(.+?)(?:\.|$)'),
        ('class_id', r'CLASS-ID\.\s*([A-Za-z][A-Za-z0-9_-]*)'),  # OO COBOL
        ('method_id', r'METHOD-ID\.\s*([A-Za-z][A-Za-z0-9_-]*)'),  # OO COBOL
        ('function_id', r'FUNCTION-ID\.\s*([A-Za-z][A-Za-z0-9_-]*)'),  # Intrinsic func
    ]
    def parse(self, source: str, lines: list[tuple]) -> dict:
        result = {}
        # Improved Regex: Handles spaces, tabs, and trailing periods
        match = re.search(r'PROGRAM-ID\.\s+([A-Za-z0-9_-]+)', source, re.IGNORECASE)
        if match:
            result['program_id'] = match.group(1).upper()
        else:
            # THIS IS HOW YOU POPULATE UNRECOGNIZED
            self._unrecognized.append({
                "type": "MISSING_HEADER",
                "message": "PROGRAM-ID not found or improperly formatted"
            })
        return result

# ... (inside CobolParser class)
    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        self._source = content
        self._lines = self._preprocess(content)
        self._unrecognized = []
        self._warnings = [] 
        
        normalized = self._normalize_source(content)
        
        # Heuristic Warning: Check if file is suspiciously short
        if len(content.splitlines()) < 10:
            self._warnings.append("File is very short; verify if this is a complete COBOL program.")

        result = {
            'source_file': source_file,
            'program_name': 'UNKNOWN',
            'dependencies': {},
            '_warnings': self._warnings,
            '_unrecognized': []
        }
        
        for parser_name, parser_class in ParserRegistry.all_parsers().items():
            # Only run the two essential parsers
            if parser_name not in ['identification', 'dependencies']:
                continue

            parser = parser_class()
            parser_result = parser.parse(normalized, self._lines)
            
            if parser_result:
                if parser_name == 'identification':
                    result['program_name'] = parser_result.get('program_id', 'UNKNOWN')
                elif parser_name == 'dependencies':
                    result['dependencies'] = parser_result
            
            # Transfer errors from the sub-parser to the main result
            if parser.unrecognized:
                self._unrecognized.extend(parser.unrecognized)
        
        # Ensure these fields are ALWAYS in the output, even if empty
        result['_unrecognized'] = self._unrecognized
        result['_warnings'] = self._warnings
        
        return result

# =============================================================================
# ENVIRONMENT DIVISION Parser
# =============================================================================


# =============================================================================
# DATA DIVISION Parser
# =============================================================================


# =============================================================================
# PROCEDURE DIVISION Parser
# =============================================================================


# =============================================================================
# Dependencies Parser (using legacylens + custom)
# =============================================================================

@ParserRegistry.register("dependencies")
class DependenciesParser(BaseDivisionParser):
    """Parser for extracting program dependencies"""
    
    def parse(self, source: str, lines: list[tuple]) -> dict:
        result = {}
        
        # Use legacylens if available
        if HAS_LEGACYLENS:
            try:
                legacylens_result = legacylens_parse(source)
                if legacylens_result:
                    if legacylens_result.get('calls'):
                        # Post-process to add call_type classification
                        result['calls'] = self._classify_calls(
                            legacylens_result['calls'], source
                        )
                    if legacylens_result.get('performs'):
                        result['performs'] = legacylens_result['performs']
                    if legacylens_result.get('sql_queries'):
                        result['sql_queries'] = legacylens_result['sql_queries']
                    if legacylens_result.get('io_files'):
                        result['file_operations'] = self._fix_file_operations(
                            legacylens_result['io_files']
                        )
                    if legacylens_result.get('copybooks'):
                        result['copybooks'] = legacylens_result['copybooks']
            except Exception as e:
                self._unrecognized.append({'error': f'legacylens error: {str(e)}'})
        
        # Custom extraction for items legacylens might miss
        
        # Extract CALL statements (fallback if legacylens didn't provide)
        if 'calls' not in result:
            calls = self._extract_calls(source)
            if calls:
                result['calls'] = calls
        
        # Extract COPY statements
        if 'copybooks' not in result:
            copybooks = self._extract_copybooks(source)
            if copybooks:
                result['copybooks'] = copybooks
        
        # Extract EXEC SQL statements
        if 'sql_queries' not in result:
            sql = self._extract_sql(source)
            if sql:
                result['sql_queries'] = sql
        
        # Extract file operations (fallback if legacylens didn't provide)
        if 'file_operations' not in result:
            file_ops = self._extract_file_operations(source)
            if file_ops:
                result['file_operations'] = file_ops
        
        # Extract EXEC CICS statements
        cics = self._extract_cics(source)
        if cics:
            result['cics_commands'] = cics
        
        # Extract XML PARSE statements
        xml = self._extract_xml_parse(source)
        if xml:
            result['xml_parse_statements'] = xml
        
        # Extract JSON GENERATE/PARSE statements (IBM COBOL v4.2+)
        json_stmts = self._extract_json_statements(source)
        if json_stmts:
            result['json_statements'] = json_stmts
        
        # Extract INVOKE statements (OO COBOL)
        invokes = self._extract_invoke_statements(source)
        if invokes:
            result['invoke_statements'] = invokes
        
        # Extract EVALUATE structure
        evaluates = self._extract_evaluate_statements(source)
        if evaluates:
            result['evaluate_statements'] = evaluates
        
        # Extract nested programs
        nested = self._extract_nested_programs(source)
        if nested:
            result['nested_programs'] = nested
        
        # Extract DECLARATIVES section
        declaratives = self._extract_declaratives(source)
        if declaratives:
            result['declaratives'] = declaratives
        
        return result
    
    def _classify_calls(self, calls: list, source: str) -> list[dict]:
        """Post-process calls to add call_type classification (static/dynamic).
        
        Static calls: CALL 'PROGRAM-NAME'
        Dynamic calls: CALL variable-name
        """
        classified = []
        for call in calls:
            call_copy = dict(call) if isinstance(call, dict) else {'raw': call}
            
            # Check if program_name exists and is a literal (static)
            program_name = call_copy.get('program_name', '')
            
            if program_name and not program_name.startswith("'"):
                # Check if the match contains a quoted literal
                match_text = call_copy.get('match', '')
                if "'" in match_text or '"' in match_text:
                    call_copy['call_type'] = 'static'
                else:
                    # This is a dynamic call - variable name
                    call_copy['call_type'] = 'dynamic'
                    call_copy['variable'] = program_name
                    call_copy['program_name'] = None
                    
                    # Try to resolve the dynamic call
                    resolved, possible_values = self._resolve_dynamic_call(
                        program_name, source
                    )
                    call_copy['resolved'] = resolved
                    if possible_values:
                        call_copy['possible_values'] = possible_values
                        if len(possible_values) == 1:
                            call_copy['program_name'] = possible_values[0]
            else:
                call_copy['call_type'] = 'static'
            
            classified.append(call_copy)
        
        return classified
    
    def _extract_calls(self, source: str) -> list[dict]:
        """Extract CALL statements with static/dynamic classification.
        
        This is a fallback when legacylens is not available.
        """
        calls = []
        
        # Pattern for CALL statements
        # Matches both: CALL 'PROG-NAME' and CALL WS-PROG-VAR
        call_pattern = re.compile(
            r"CALL\s+(['\"][A-Za-z][A-Za-z0-9_-]*['\"]|[A-Za-z][A-Za-z0-9_-]*)"
            r"(?:\s+USING\s+(.+?))?(?:\s*\.|(?:\s+END-CALL))",
            re.IGNORECASE | re.DOTALL
        )
        
        # Find line numbers for matches
        lines = source.split('\n')
        line_offset = 0
        
        for match in call_pattern.finditer(source):
            target = match.group(1)
            using_clause = match.group(2)
            
            # Calculate line number
            pos = match.start()
            line_num = source[:pos].count('\n') + 1
            
            call_entry = {
                'match': match.group(0).strip(),
                'line': line_num,
            }
            
            # Determine if static or dynamic
            if target.startswith("'") or target.startswith('"'):
                # Static call
                call_entry['call_type'] = 'static'
                call_entry['program_name'] = target.strip("'\"").upper()
            else:
                # Dynamic call
                call_entry['call_type'] = 'dynamic'
                call_entry['variable'] = target.upper()
                call_entry['program_name'] = None
                
                # Try to resolve
                resolved, possible_values = self._resolve_dynamic_call(target, source)
                call_entry['resolved'] = resolved
                if possible_values:
                    call_entry['possible_values'] = possible_values
                    if len(possible_values) == 1:
                        call_entry['program_name'] = possible_values[0]
            
            # Extract parameters
            if using_clause:
                params = re.findall(r'([A-Za-z][A-Za-z0-9_-]*)', using_clause)
                call_entry['parameters'] = [p.upper() for p in params]
            
            calls.append(call_entry)
        
        return calls
    
    def _resolve_dynamic_call(
        self, variable_name: str, source: str
    ) -> tuple[bool, list[str]]:
        """Attempt to resolve a dynamic CALL target by searching for assignments.
        
        Searches for:
        - MOVE 'VALUE' TO variable-name
        - VALUE 'LITERAL' in variable definition
        
        Returns:
            (resolved: bool, possible_values: list[str])
        """
        possible_values = []
        var_upper = variable_name.upper()
        
        # Pattern 1: MOVE 'LITERAL' TO variable
        move_pattern = re.compile(
            rf"MOVE\s+['\"]([A-Za-z][A-Za-z0-9_-]*)['\"]"
            rf"\s+TO\s+{re.escape(var_upper)}",
            re.IGNORECASE
        )
        
        for match in move_pattern.finditer(source):
            value = match.group(1).upper()
            if value not in possible_values:
                possible_values.append(value)
        
        # Pattern 2: VALUE 'LITERAL' in variable definition
        # e.g., 05 WS-PROG PIC X(8) VALUE 'MYPROG'.
        value_pattern = re.compile(
            rf"\d{{2}}\s+{re.escape(var_upper)}[^.]*"
            rf"VALUE\s+['\"]([A-Za-z][A-Za-z0-9_-]*)['\"]",
            re.IGNORECASE
        )
        
        for match in value_pattern.finditer(source):
            value = match.group(1).upper()
            if value not in possible_values:
                possible_values.append(value)
        
        resolved = len(possible_values) > 0
        return resolved, possible_values
    
    def _fix_file_operations(self, file_ops: list) -> list[dict]:
        """Fix common issues in file operations extraction from legacylens.
        
        Known issues:
        - 'files' list may contain non-file entries like 'EXIT'
        - OPEN operations may capture unrelated tokens
        """
        fixed = []
        
        # Known non-file keywords that may be incorrectly captured
        non_file_keywords = {
            'EXIT', 'END', 'PERFORM', 'CALL', 'MOVE', 'IF', 'ELSE',
            'END-IF', 'END-PERFORM', 'END-READ', 'END-WRITE', 'STOP',
            'GOBACK', 'CONTINUE', 'AT', 'NOT', 'INTO', 'FROM'
        }
        
        for op in file_ops:
            op_copy = dict(op) if isinstance(op, dict) else {'raw': op}
            
            # Fix 'files' list
            if 'files' in op_copy and isinstance(op_copy['files'], list):
                cleaned_files = [
                    f for f in op_copy['files']
                    if f.upper() not in non_file_keywords
                    and not f.isdigit()
                    and len(f) > 1
                ]
                op_copy['files'] = cleaned_files if cleaned_files else op_copy['files']
            
            fixed.append(op_copy)
        
        return fixed
    
    def _extract_file_operations(self, source: str) -> list[dict]:
        """Extract file operations when legacylens is unavailable.
        
        Extracts:
        - SELECT statements (file definitions)
        - OPEN/CLOSE/READ/WRITE/DELETE/REWRITE operations
        """
        operations = []
        
        # Pattern for SELECT statements (file definitions)
        select_pattern = re.compile(
            r'SELECT\s+([A-Za-z][A-Za-z0-9_-]*)\s+'
            r'ASSIGN\s+(?:TO\s+)?([A-Za-z][A-Za-z0-9_-]*)'
            r'(?:.*?ORGANIZATION\s+(?:IS\s+)?([A-Za-z]+))?'
            r'(?:.*?ACCESS\s+(?:MODE\s+)?(?:IS\s+)?([A-Za-z]+))?',
            re.IGNORECASE | re.DOTALL
        )
        
        for match in select_pattern.finditer(source):
            line_num = source[:match.start()].count('\n') + 1
            operations.append({
                'operation': 'SELECT',
                'file_name': match.group(1).upper(),
                'assign_to': match.group(2).upper(),
                'organization': match.group(3).upper() if match.group(3) else None,
                'access_mode': match.group(4).upper() if match.group(4) else None,
                'line': line_num,
            })
        
        # Pattern for OPEN statements
        open_pattern = re.compile(
            r'OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+([A-Za-z][A-Za-z0-9_-]*)',
            re.IGNORECASE
        )
        
        for match in open_pattern.finditer(source):
            line_num = source[:match.start()].count('\n') + 1
            operations.append({
                'operation': 'OPEN',
                'mode': match.group(1).upper(),
                'files': [match.group(2).upper()],
                'line': line_num,
            })
        
        # Pattern for CLOSE statements
        close_pattern = re.compile(
            r'CLOSE\s+([A-Za-z][A-Za-z0-9_-]*(?:\s+[A-Za-z][A-Za-z0-9_-]*)*)',
            re.IGNORECASE
        )
        
        for match in close_pattern.finditer(source):
            line_num = source[:match.start()].count('\n') + 1
            files = [f.strip().upper() for f in match.group(1).split()]
            # Filter out keywords
            files = [f for f in files if f not in ('WITH', 'LOCK', 'NO', 'REWIND')]
            if files:
                operations.append({
                    'operation': 'CLOSE',
                    'files': files,
                    'line': line_num,
                })
        
        # Pattern for READ statements
        read_pattern = re.compile(
            r'READ\s+([A-Za-z][A-Za-z0-9_-]*)(?:\s+INTO\s+([A-Za-z][A-Za-z0-9_-]*))?',
            re.IGNORECASE
        )
        
        for match in read_pattern.finditer(source):
            line_num = source[:match.start()].count('\n') + 1
            file_name = match.group(1).upper()
            # Skip if it matches a keyword
            if file_name not in ('EXIT', 'END', 'NEXT'):
                operations.append({
                    'operation': 'READ',
                    'files': [file_name],
                    'into': match.group(2).upper() if match.group(2) else None,
                    'line': line_num,
                })
        
        # Pattern for WRITE statements
        write_pattern = re.compile(
            r'WRITE\s+([A-Za-z][A-Za-z0-9_-]*)(?:\s+FROM\s+([A-Za-z][A-Za-z0-9_-]*))?',
            re.IGNORECASE
        )
        
        for match in write_pattern.finditer(source):
            line_num = source[:match.start()].count('\n') + 1
            operations.append({
                'operation': 'WRITE',
                'files': [match.group(1).upper()],
                'from': match.group(2).upper() if match.group(2) else None,
                'line': line_num,
            })
        
        # Pattern for DELETE statements
        delete_pattern = re.compile(
            r'DELETE\s+([A-Za-z][A-Za-z0-9_-]*)(?:\s+RECORD)?',
            re.IGNORECASE
        )
        
        for match in delete_pattern.finditer(source):
            line_num = source[:match.start()].count('\n') + 1
            file_name = match.group(1).upper()
            if file_name not in ('FROM',):
                operations.append({
                    'operation': 'DELETE',
                    'files': [file_name],
                    'line': line_num,
                })
        
        # Pattern for REWRITE statements
        rewrite_pattern = re.compile(
            r'REWRITE\s+([A-Za-z][A-Za-z0-9_-]*)(?:\s+FROM\s+([A-Za-z][A-Za-z0-9_-]*))?',
            re.IGNORECASE
        )
        
        for match in rewrite_pattern.finditer(source):
            line_num = source[:match.start()].count('\n') + 1
            operations.append({
                'operation': 'REWRITE',
                'files': [match.group(1).upper()],
                'from': match.group(2).upper() if match.group(2) else None,
                'line': line_num,
            })
        
        return operations
    
    
    def _extract_copybooks(self, source: str) -> list[dict]:
        """Extract COPY statements"""
        copybooks = []
        pattern = re.compile(
            r'COPY\s+([A-Za-z][A-Za-z0-9_-]*)(?:\s+(?:OF|IN)\s+([A-Za-z][A-Za-z0-9_-]*))?',
            re.IGNORECASE
        )
        
        for match in pattern.finditer(source):
            entry = {'copybook_name': match.group(1).upper()}
            if match.group(2):
                entry['library'] = match.group(2).upper()
            copybooks.append(entry)
        
        return copybooks
    
    def _extract_sql(self, source: str) -> list[dict]:
        """Extract EXEC SQL statements"""
        sql_stmts = []
        pattern = re.compile(
            r'EXEC\s+SQL(.*?)END-EXEC',
            re.IGNORECASE | re.DOTALL
        )
        
        for match in pattern.finditer(source):
            sql_content = match.group(1).strip()
            # Determine SQL type
            sql_type = 'UNKNOWN'
            if re.match(r'\s*SELECT', sql_content, re.IGNORECASE):
                sql_type = 'SELECT'
            elif re.match(r'\s*INSERT', sql_content, re.IGNORECASE):
                sql_type = 'INSERT'
            elif re.match(r'\s*UPDATE', sql_content, re.IGNORECASE):
                sql_type = 'UPDATE'
            elif re.match(r'\s*DELETE', sql_content, re.IGNORECASE):
                sql_type = 'DELETE'
            elif re.match(r'\s*INCLUDE', sql_content, re.IGNORECASE):
                sql_type = 'INCLUDE'
            elif re.match(r'\s*DECLARE', sql_content, re.IGNORECASE):
                sql_type = 'DECLARE'
            
            sql_stmts.append({
                'type': sql_type,
                'statement': ' '.join(sql_content.split())  # Normalize whitespace
            })
        
        return sql_stmts
    
    def _extract_cics(self, source: str) -> list[dict]:
        """Extract EXEC CICS statements"""
        cics_stmts = []
        pattern = re.compile(
            r'EXEC\s+CICS\s+([A-Za-z]+)(.*?)END-EXEC',
            re.IGNORECASE | re.DOTALL
        )
        
        for match in pattern.finditer(source):
            cics_stmts.append({
                'command': match.group(1).upper(),
                'full_statement': f"EXEC CICS {match.group(1)} {match.group(2).strip()} END-EXEC"
            })
        
        return cics_stmts
    
    def _extract_xml_parse(self, source: str) -> list[dict]:
        """Extract XML PARSE statements (IBM COBOL v4 feature)"""
        xml_stmts = []
        pattern = re.compile(
            r'XML\s+PARSE\s+([A-Za-z][A-Za-z0-9_-]*)(.*?)END-XML',
            re.IGNORECASE | re.DOTALL
        )
        
        for match in pattern.finditer(source):
            entry = {'document': match.group(1).upper()}
            
            # Check for PROCESSING PROCEDURE
            proc_match = re.search(r'PROCESSING\s+PROCEDURE\s+([A-Za-z][A-Za-z0-9_-]*)', 
                                  match.group(2), re.IGNORECASE)
            if proc_match:
                entry['processing_procedure'] = proc_match.group(1).upper()
            
            xml_stmts.append(entry)
        
        return xml_stmts
    
    def _extract_json_statements(self, source: str) -> list[dict]:
        """Extract JSON GENERATE and JSON PARSE statements (IBM COBOL v4.2+)"""
        json_stmts = []
        
        # JSON GENERATE
        gen_pattern = re.compile(
            r'JSON\s+GENERATE\s+([A-Za-z][A-Za-z0-9_-]*)\s+FROM\s+([A-Za-z][A-Za-z0-9_-]*)',
            re.IGNORECASE
        )
        for match in gen_pattern.finditer(source):
            json_stmts.append({
                'type': 'GENERATE',
                'target': match.group(1).upper(),
                'source': match.group(2).upper()
            })
        
        # JSON PARSE
        parse_pattern = re.compile(
            r'JSON\s+PARSE\s+([A-Za-z][A-Za-z0-9_-]*)\s+INTO\s+([A-Za-z][A-Za-z0-9_-]*)',
            re.IGNORECASE
        )
        for match in parse_pattern.finditer(source):
            json_stmts.append({
                'type': 'PARSE',
                'source': match.group(1).upper(),
                'target': match.group(2).upper()
            })
        
        return json_stmts
    
    def _extract_invoke_statements(self, source: str) -> list[dict]:
        """Extract INVOKE statements (OO COBOL method calls)"""
        invokes = []
        pattern = re.compile(
            r'INVOKE\s+([A-Za-z][A-Za-z0-9_-]*)\s+"([^"]+)"',
            re.IGNORECASE
        )
        
        for match in pattern.finditer(source):
            invokes.append({
                'object': match.group(1).upper(),
                'method': match.group(2)
            })
        
        return invokes
    
    def _extract_evaluate_statements(self, source: str) -> list[dict]:
        """Extract EVALUATE statements structure"""
        evaluates = []
        pattern = re.compile(
            r'EVALUATE\s+([A-Za-z][A-Za-z0-9_-]*|TRUE|FALSE)(.*?)END-EVALUATE',
            re.IGNORECASE | re.DOTALL
        )
        
        for match in pattern.finditer(source):
            entry = {'subject': match.group(1).upper()}
            
            # Extract WHEN clauses
            when_pattern = re.compile(r'WHEN\s+([^W]+?)(?=WHEN|END-EVALUATE)', re.IGNORECASE | re.DOTALL)
            whens = when_pattern.findall(match.group(2))
            if whens:
                entry['when_count'] = len(whens)
            
            evaluates.append(entry)
        
        return evaluates
    
    def _extract_nested_programs(self, source: str) -> list[dict]:
        """Extract nested program definitions"""
        nested = []
        
        # Find all PROGRAM-ID entries after the first one
        pattern = re.compile(
            r'IDENTIFICATION\s+DIVISION\.(.*?)PROGRAM-ID\.\s*([A-Za-z][A-Za-z0-9_-]*)',
            re.IGNORECASE | re.DOTALL
        )
        
        matches = list(pattern.finditer(source))
        if len(matches) > 1:
            # First one is main program, rest are nested
            for match in matches[1:]:
                nested.append({
                    'program_id': match.group(2).upper(),
                    'type': 'nested'
                })
        
        return nested
    
    def _extract_declaratives(self, source: str) -> list[dict]:
        """Extract DECLARATIVES section and USE statements"""
        declaratives = []
        
        # Find DECLARATIVES section
        decl_match = re.search(
            r'DECLARATIVES\.(.*?)END\s+DECLARATIVES',
            source, re.IGNORECASE | re.DOTALL
        )
        
        if decl_match:
            decl_content = decl_match.group(1)
            
            # Extract USE statements
            use_pattern = re.compile(
                r'USE\s+(?:GLOBAL\s+)?(?:AFTER\s+)?(?:STANDARD\s+)?'
                r'(?:ERROR|EXCEPTION|DEBUGGING)\s+(?:PROCEDURE\s+)?'
                r'(?:ON\s+)?([A-Za-z][A-Za-z0-9_-]*)?',
                re.IGNORECASE
            )
            
            for match in use_pattern.finditer(decl_content):
                entry = {'type': 'USE'}
                if match.group(1):
                    entry['on_file'] = match.group(1).upper()
                declaratives.append(entry)
        
        return declaratives


# =============================================================================
# Main COBOL Parser
# =============================================================================

class CobolParser(BaseParser):
    """
    Main parser for COBOL programs.
    
    Combines multiple division parsers to create comprehensive JSON output.
    """
    
    FILE_TYPE = "cobol"
    
    # Common EBCDIC codepages for IBM mainframes
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    
    # Limits
    MAX_FILE_SIZE = 50 * 1024 * 1024  # 50 MB (COBOL programs can be large)
    
    def __init__(self, strict: bool = False):
        self.strict = strict
        self._source: str = ""
        self._lines: list[tuple] = []
        self._unrecognized: list[dict] = []
        self._warnings: list[str] = []
    
    def parse_file(self, filepath: str) -> dict:
        """Parse a COBOL file and return structured JSON"""
        logger.info(f"Starting to parse COBOL file: {filepath}")
        
        path = Path(filepath)
        
        # Input validation
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            raise FileNotFoundError(f"COBOL file not found: {filepath}")
        
        if not path.is_file():
            logger.error(f"Path is not a file: {filepath}")
            raise InvalidSyntaxError(f"Path is not a file: {filepath}")
        
        file_size = path.stat().st_size
        logger.debug(f"File size: {file_size} bytes")
        
        if file_size == 0:
            logger.error(f"Empty file: {filepath}")
            raise InvalidSyntaxError("File is empty")
        
        if file_size > self.MAX_FILE_SIZE:
            logger.error(f"File too large: {file_size} bytes")
            raise InvalidSyntaxError(
                f"File exceeds maximum size of {self.MAX_FILE_SIZE // 1024 // 1024} MB"
            )
        
        # Read file as bytes first to detect encoding
        logger.debug("Reading file as bytes for encoding detection")
        raw_bytes = path.read_bytes()
        
        # Detect and convert encoding
        content, detected_encoding = self._detect_and_decode(raw_bytes)
        logger.info(f"Detected encoding: {detected_encoding}")
        
        result = self.parse_string(content, source_file=str(path.name))
        
        # Add encoding info if not standard
        if detected_encoding and detected_encoding not in ('utf-8', 'ascii'):
            result['detected_encoding'] = detected_encoding
        
        logger.info(f"Successfully parsed COBOL file: {filepath}")
        return result
    
    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        """Detect file encoding and decode to string"""
        # Try UTF-8 first
        try:
            text = raw_bytes.decode('utf-8')
            if self._looks_like_cobol(text):
                return text, 'utf-8'
        except UnicodeDecodeError:
            pass
        
        # Try ASCII
        try:
            text = raw_bytes.decode('ascii')
            if self._looks_like_cobol(text):
                return text, 'ascii'
        except UnicodeDecodeError:
            pass
        
        # Try EBCDIC codepages
        for codepage in self.EBCDIC_CODEPAGES:
            try:
                text = raw_bytes.decode(codepage)
                if self._looks_like_cobol(text):
                    logger.info(f"Detected EBCDIC encoding: {codepage}")
                    return text, codepage
            except (UnicodeDecodeError, LookupError):
                continue
        
        # Fallback
        logger.info("Could not detect encoding, falling back to UTF-8")
        return raw_bytes.decode('utf-8', errors='replace'), 'utf-8-fallback'
    
    def _looks_like_cobol(self, text: str) -> bool:
        """Heuristic check for valid COBOL source"""
        unprintable_count = sum(1 for c in text[:1000] if ord(c) < 32 and c not in '\n\r\t')
        if unprintable_count > 10:
            return False
        
        cobol_keywords = ['DIVISION', 'SECTION', 'PROCEDURE', 'WORKING-STORAGE',
                          'PERFORM', 'MOVE', 'IF', 'END-IF', 'CALL', 'COPY']
        text_upper = text.upper()
        keyword_count = sum(1 for kw in cobol_keywords if kw in text_upper)
        return keyword_count >= 2
    
    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        """Parse COBOL content string and return structured JSON"""
        self._source = content
        self._lines = self._preprocess(content)
        self._unrecognized = []
        self._warnings = [] # Initialize/clear warnings for this run
        
        # Normalize source for parsing (join continuation lines)
        normalized = self._normalize_source(content)
        
        # Initialize result with mandatory metadata fields
        result = {
            'source_file': source_file,
            'program_name': 'UNKNOWN',
            'dependencies': {},
            '_warnings': self._warnings,      # <--- ADDED THIS
            '_unrecognized': []               # <--- INITIALIZED EMPTY
        }
        
        # Run all registered parsers
        for parser_name, parser_class in ParserRegistry.all_parsers().items():
            # Optimization: Only run the parsers you want to keep
            if parser_name not in ['identification', 'dependencies']:
                continue

            parser = parser_class()
            parser_result = parser.parse(normalized, self._lines)
            
            if parser_result:
                if parser_name == 'identification':
                    # Only keep program_id from identification
                    prog_id = parser_result.get('program_id')
                    if prog_id:
                        result['program_name'] = prog_id
                
                elif parser_name == 'dependencies':
                    result['dependencies'] = parser_result
            
            # Collect unrecognized patterns from the parser
            if parser.unrecognized:
                self._unrecognized.extend(parser.unrecognized)
        
        # Final assignment to ensure lists are updated
        result['_unrecognized'] = self._unrecognized
        result['_warnings'] = self._warnings
        
        return result
    def _preprocess(self, content: str) -> list[tuple]:
        """Preprocess COBOL source into (line_number, text, original) tuples"""
        lines = content.split('\n')
        processed = []
        
        for i, line in enumerate(lines):
            if len(line) >= 7:
                indicator = line[6] if len(line) > 6 else ' '
                if indicator in ('*', '/'):  # Comment
                    continue
                text = line[7:72] if len(line) >= 72 else line[7:]
            else:
                text = line
                
            if text.strip():
                processed.append((i + 1, text, line))
                
        return processed
    
    def _normalize_source(self, content: str) -> str:
        """Normalize source: handle continuations, remove sequence numbers"""
        lines = content.split('\n')
        result_lines = []
        
        for line in lines:
            if len(line) >= 7:
                indicator = line[6] if len(line) > 6 else ' '
                if indicator == '*' or indicator == '/':
                    continue
                if indicator == '-' and result_lines:
                    # Continuation - append to previous line
                    continuation = line[7:72] if len(line) >= 72 else line[7:]
                    result_lines[-1] = result_lines[-1].rstrip() + continuation.lstrip()
                    continue
                text = line[7:72] if len(line) >= 72 else line[7:]
            else:
                text = line
                
            result_lines.append(text)
        
        return '\n'.join(result_lines)


def main():
    parser = argparse.ArgumentParser(
        description='Parse COBOL program files to JSON',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('input', help='Input COBOL file path')
    parser.add_argument('-o', '--output', help='Output JSON file (default: stdout)')
    parser.add_argument('--indent', type=int, default=2, help='JSON indent level')
    parser.add_argument('--strict', action='store_true', help='Raise errors on parse failures')
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable verbose logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger('cobol_parser').setLevel(logging.DEBUG)
    
    try:
        cobol_parser = CobolParser(strict=args.strict)
        result = cobol_parser.parse_file(args.input)
        
        output_json = json.dumps(result, indent=args.indent, ensure_ascii=False)
        
        if args.output:
            try:
                Path(args.output).write_text(output_json, encoding='utf-8')
                logger.info(f"Output written to: {args.output}")
                print(f"Output written to: {args.output}")
            except IOError as e:
                logger.error(f"Failed to write output file: {e}")
                print(f"Error writing output: {e}", file=sys.stderr)
                return ExitCode.OUTPUT_ERROR
        else:
            print(output_json)
        
        return ExitCode.SUCCESS
            
    except FileNotFoundError as e:
        logger.error(str(e))
        print(f"Error: {e}", file=sys.stderr)
        return ExitCode.FILE_NOT_FOUND
    
    except InvalidSyntaxError as e:
        logger.error(str(e))
        print(f"Syntax Error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback
            traceback.print_exc()
        return ExitCode.INVALID_INPUT
    
    except CobolParseError as e:
        logger.error(str(e))
        print(f"Parse Error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback
            traceback.print_exc()
        return ExitCode.PARSE_ERROR
    
    except Exception as e:
        logger.exception(f"Unexpected error: {e}")
        print(f"Error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback
            traceback.print_exc()
        return ExitCode.PARSE_ERROR


if __name__ == '__main__':
    exit(main())

