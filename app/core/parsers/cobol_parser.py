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
    logger.warning("legacylens-cobol-parser not available, dependency extraction limited")


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
        
        # Extract IDENTIFICATION DIVISION content
        id_match = re.search(
            r'IDENTIFICATION\s+DIVISION\.(.*?)(?=ENVIRONMENT\s+DIVISION\.|DATA\s+DIVISION\.|PROCEDURE\s+DIVISION\.|$)',
            source, re.IGNORECASE | re.DOTALL
        )
        
        if not id_match:
            return result
            
        id_content = id_match.group(1)
        
        for key, pattern in self.CLAUSES:
            match = re.search(pattern, id_content, re.IGNORECASE)
            if match:
                value = match.group(1).strip().rstrip('.')
                if value:
                    result[key] = value
        
        return result


# =============================================================================
# ENVIRONMENT DIVISION Parser
# =============================================================================

@ParserRegistry.register("environment")
class EnvironmentDivisionParser(BaseDivisionParser):
    """Parser for ENVIRONMENT DIVISION"""
    
    def parse(self, source: str, lines: list[tuple]) -> dict:
        result = {}
        
        # Extract ENVIRONMENT DIVISION content
        env_match = re.search(
            r'ENVIRONMENT\s+DIVISION\.(.*?)(?=DATA\s+DIVISION\.|PROCEDURE\s+DIVISION\.|$)',
            source, re.IGNORECASE | re.DOTALL
        )
        
        if not env_match:
            return result
            
        env_content = env_match.group(1)
        
        # Parse CONFIGURATION SECTION
        config = self._parse_configuration(env_content)
        if config:
            result['configuration_section'] = config
        
        # Parse INPUT-OUTPUT SECTION
        io = self._parse_input_output(env_content)
        if io:
            result['input_output_section'] = io
        
        return result
    
    def _parse_configuration(self, content: str) -> dict:
        """Parse CONFIGURATION SECTION"""
        config = {}
        
        # SOURCE-COMPUTER
        match = re.search(r'SOURCE-COMPUTER\.\s*([A-Za-z][A-Za-z0-9_-]*)', content, re.IGNORECASE)
        if match:
            config['source_computer'] = match.group(1).upper()
        
        # OBJECT-COMPUTER
        match = re.search(r'OBJECT-COMPUTER\.\s*([A-Za-z][A-Za-z0-9_-]*)', content, re.IGNORECASE)
        if match:
            config['object_computer'] = match.group(1).upper()
        
        # SPECIAL-NAMES
        special_names = self._parse_special_names(content)
        if special_names:
            config['special_names'] = special_names
        
        return config
    
    def _parse_special_names(self, content: str) -> dict:
        """Parse SPECIAL-NAMES paragraph"""
        special = {}
        
        # DECIMAL-POINT IS COMMA
        if re.search(r'DECIMAL-POINT\s+IS\s+COMMA', content, re.IGNORECASE):
            special['decimal_point_is_comma'] = True
        
        # CURRENCY SIGN
        currency_matches = re.findall(
            r"CURRENCY\s+SIGN\s+IS\s+'([^']+)'(?:\s+WITH\s+PICTURE\s+SYMBOL\s+'([^']+)')?",
            content, re.IGNORECASE
        )
        if currency_matches:
            special['currency_signs'] = [
                {'sign': m[0], 'symbol': m[1] if m[1] else m[0]}
                for m in currency_matches
            ]
        
        return special
    
    def _parse_input_output(self, content: str) -> dict:
        """Parse INPUT-OUTPUT SECTION"""
        io_section = {}
        
        # Parse FILE-CONTROL
        file_control = self._parse_file_control(content)
        if file_control:
            io_section['file_control'] = file_control
        
        return io_section
    
    def _parse_file_control(self, content: str) -> list[dict]:
        """Parse FILE-CONTROL paragraph"""
        files = []
        
        # Find all SELECT statements
        select_pattern = re.compile(
            r'SELECT\s+([A-Za-z][A-Za-z0-9_-]*)(.*?)(?=SELECT\s+|$)',
            re.IGNORECASE | re.DOTALL
        )
        
        for match in select_pattern.finditer(content):
            file_entry = {'select_name': match.group(1).upper()}
            clauses = match.group(2)
            
            # ASSIGN TO
            assign_match = re.search(r'ASSIGN\s+(?:TO\s+)?([A-Za-z][A-Za-z0-9_-]*)', clauses, re.IGNORECASE)
            if assign_match:
                file_entry['assign_to'] = assign_match.group(1).upper()
            
            # ORGANIZATION
            org_match = re.search(r'ORGANIZATION\s+(?:IS\s+)?(SEQUENTIAL|INDEXED|RELATIVE|LINE\s+SEQUENTIAL)', 
                                 clauses, re.IGNORECASE)
            if org_match:
                file_entry['organization'] = org_match.group(1).upper().replace(' ', '_')
            
            # ACCESS MODE
            access_match = re.search(r'ACCESS\s+(?:MODE\s+(?:IS\s+)?)?(SEQUENTIAL|RANDOM|DYNAMIC)', 
                                    clauses, re.IGNORECASE)
            if access_match:
                file_entry['access_mode'] = access_match.group(1).upper()
            
            # RECORD KEY
            key_match = re.search(r'RECORD\s+KEY\s+(?:IS\s+)?([A-Za-z][A-Za-z0-9_-]*)', clauses, re.IGNORECASE)
            if key_match:
                file_entry['record_key'] = key_match.group(1).upper()
            
            # ALTERNATE RECORD KEY
            alt_keys = re.findall(
                r'ALTERNATE\s+RECORD\s+KEY\s+(?:IS\s+)?([A-Za-z][A-Za-z0-9_-]*)(?:\s+WITH\s+DUPLICATES)?',
                clauses, re.IGNORECASE
            )
            if alt_keys:
                file_entry['alternate_keys'] = [k.upper() for k in alt_keys]
            
            # FILE STATUS
            status_match = re.search(r'FILE\s+STATUS\s+(?:IS\s+)?([A-Za-z][A-Za-z0-9_-]*)', 
                                    clauses, re.IGNORECASE)
            if status_match:
                file_entry['file_status'] = status_match.group(1).upper()
            
            files.append(file_entry)
        
        return files


# =============================================================================
# DATA DIVISION Parser
# =============================================================================

@ParserRegistry.register("data")
class DataDivisionParser(BaseDivisionParser):
    """Parser for DATA DIVISION - extracts file section, working-storage, linkage"""
    
    def parse(self, source: str, lines: list[tuple]) -> dict:
        result = {}
        
        # Extract DATA DIVISION content
        data_match = re.search(
            r'DATA\s+DIVISION\.(.*?)(?=PROCEDURE\s+DIVISION\.|$)',
            source, re.IGNORECASE | re.DOTALL
        )
        
        if not data_match:
            return result
            
        data_content = data_match.group(1)
        
        # Parse FILE SECTION
        file_section = self._parse_section(data_content, 'FILE SECTION')
        if file_section:
            result['file_section'] = file_section
        
        # Parse WORKING-STORAGE SECTION
        ws_section = self._parse_section(data_content, 'WORKING-STORAGE SECTION')
        if ws_section:
            result['working_storage_section'] = ws_section
        
        # Parse LINKAGE SECTION
        linkage_section = self._parse_section(data_content, 'LINKAGE SECTION')
        if linkage_section:
            result['linkage_section'] = linkage_section
        
        # Parse LOCAL-STORAGE SECTION (for threads)
        local_section = self._parse_section(data_content, 'LOCAL-STORAGE SECTION')
        if local_section:
            result['local_storage_section'] = local_section
        
        return result
    
    def _parse_section(self, content: str, section_name: str) -> list[dict]:
        """Parse a specific section of DATA DIVISION"""
        # Find section boundaries
        section_pattern = re.compile(
            rf'{section_name}\.(.*?)(?=FILE\s+SECTION\.|WORKING-STORAGE\s+SECTION\.|'
            rf'LINKAGE\s+SECTION\.|LOCAL-STORAGE\s+SECTION\.|PROCEDURE\s+DIVISION\.|$)',
            re.IGNORECASE | re.DOTALL
        )
        
        match = section_pattern.search(content)
        if not match:
            return []
            
        section_content = match.group(1)
        
        # Parse FD entries for FILE SECTION
        if 'FILE' in section_name.upper():
            return self._parse_file_descriptions(section_content)
        
        # Parse data items for other sections
        return self._parse_data_items(section_content)
    
    def _parse_file_descriptions(self, content: str) -> list[dict]:
        """Parse FD (File Description) entries"""
        fds = []
        
        # Find all FD entries
        fd_pattern = re.compile(
            r'FD\s+([A-Za-z][A-Za-z0-9_-]*)(.*?)(?=FD\s+|SD\s+|\d{2}\s+|$)',
            re.IGNORECASE | re.DOTALL
        )
        
        for match in fd_pattern.finditer(content):
            fd_entry = {'file_name': match.group(1).upper()}
            clauses = match.group(2)
            
            # LABEL RECORDS
            label_match = re.search(r'LABEL\s+RECORDS?\s+(?:ARE?\s+)?(STANDARD|OMITTED)', 
                                   clauses, re.IGNORECASE)
            if label_match:
                fd_entry['label_records'] = label_match.group(1).upper()
            
            # BLOCK CONTAINS
            block_match = re.search(r'BLOCK\s+CONTAINS\s+(\d+)(?:\s+TO\s+(\d+))?\s+(?:RECORDS?|CHARACTERS?)', 
                                   clauses, re.IGNORECASE)
            if block_match:
                fd_entry['block_contains'] = int(block_match.group(1))
            
            # RECORD CONTAINS
            record_match = re.search(r'RECORD\s+CONTAINS\s+(\d+)(?:\s+TO\s+(\d+))?\s+CHARACTERS?', 
                                    clauses, re.IGNORECASE)
            if record_match:
                fd_entry['record_contains'] = int(record_match.group(1))
                if record_match.group(2):
                    fd_entry['record_contains_max'] = int(record_match.group(2))
            
            # RECORDING MODE
            mode_match = re.search(r'RECORDING\s+MODE\s+(?:IS\s+)?([FVUS])', clauses, re.IGNORECASE)
            if mode_match:
                fd_entry['recording_mode'] = mode_match.group(1).upper()
            
            # DATA RECORD(S)
            data_rec_match = re.search(r'DATA\s+RECORDS?\s+(?:ARE?\s+)?([A-Za-z][A-Za-z0-9_\s-]+?)(?:\.|$)', 
                                      clauses, re.IGNORECASE)
            if data_rec_match:
                fd_entry['data_records'] = [r.strip().upper() for r in data_rec_match.group(1).split()]
            
            fds.append(fd_entry)
        
        return fds
    
    def _parse_data_items(self, content: str) -> list[dict]:
        """Parse data items (01-49 levels)"""
        items = []
        
        # Simple extraction of 01-level items with their names
        level_pattern = re.compile(
            r'^\s*(\d{2})\s+([A-Za-z][A-Za-z0-9_-]*|FILLER)',
            re.MULTILINE | re.IGNORECASE
        )
        
        current_01 = None
        for match in level_pattern.finditer(content):
            level = int(match.group(1))
            name = match.group(2).upper()
            
            if level == 1:
                current_01 = {'level': level, 'name': name, 'fields': []}
                items.append(current_01)
            elif current_01 and level in (5, 10, 15, 20, 25, 30, 35, 40, 45, 49, 66, 77, 88):
                current_01['fields'].append({'level': level, 'name': name})
        
        return items


# =============================================================================
# PROCEDURE DIVISION Parser
# =============================================================================

@ParserRegistry.register("procedure")
class ProcedureDivisionParser(BaseDivisionParser):
    """Parser for PROCEDURE DIVISION structure"""
    
    def parse(self, source: str, lines: list[tuple]) -> dict:
        result = {}
        
        # Extract PROCEDURE DIVISION content
        proc_match = re.search(
            r'PROCEDURE\s+DIVISION(.*?)\.',
            source, re.IGNORECASE
        )
        
        if not proc_match:
            return result
        
        # Check for USING clause
        header = proc_match.group(1)
        using_match = re.search(r'USING\s+(.*)', header, re.IGNORECASE)
        if using_match:
            params = re.findall(r'([A-Za-z][A-Za-z0-9_-]*)', using_match.group(1))
            result['using_clause'] = [p.upper() for p in params]
        
        # Extract full procedure division
        proc_full_match = re.search(
            r'PROCEDURE\s+DIVISION.*?\.(.*?)$',
            source, re.IGNORECASE | re.DOTALL
        )
        
        if proc_full_match:
            proc_content = proc_full_match.group(1)
            
            # Parse sections and paragraphs
            structure = self._parse_structure(proc_content)
            if structure.get('sections'):
                result['sections'] = structure['sections']
            if structure.get('paragraphs'):
                result['paragraphs'] = structure['paragraphs']
        
        return result
    
    def _parse_structure(self, content: str) -> dict:
        """Parse sections and paragraphs"""
        result = {'sections': [], 'paragraphs': []}
        
        # Find sections (name followed by SECTION)
        section_pattern = re.compile(
            r'^(\s*)([A-Za-z][A-Za-z0-9_-]*)\s+SECTION\s*\.',
            re.MULTILINE | re.IGNORECASE
        )
        
        # Find paragraphs (name at margin A followed by period)
        # Paragraph names start in column 8-11 (Area A)
        # COBOL paragraph names can start with digits (e.g., 0000-MAIN-PARA)
        paragraph_pattern = re.compile(
            r'^(\s{0,4})([A-Za-z0-9][A-Za-z0-9_-]*)\s*\.',
            re.MULTILINE
        )
        
        sections = list(section_pattern.finditer(content))
        paragraphs = list(paragraph_pattern.finditer(content))
        
        # Build section list
        for match in sections:
            result['sections'].append({
                'name': match.group(2).upper()
            })
        
        # Build paragraph list (excluding section names)
        section_names = {s['name'] for s in result['sections']}
        for match in paragraphs:
            para_name = match.group(2).upper()
            # Skip if it's a section name or a COBOL keyword
            if para_name not in section_names and para_name not in (
                'IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
                'DIVISION', 'SECTION', 'COPY', 'END', 'EXIT', 'STOP',
                'GOBACK', 'CONTINUE', 'NEXT', 'PERFORM', 'IF', 'ELSE',
                'EVALUATE', 'WHEN', 'MOVE', 'COMPUTE', 'ADD', 'SUBTRACT',
                'MULTIPLY', 'DIVIDE', 'CALL', 'OPEN', 'CLOSE', 'READ',
                'WRITE', 'REWRITE', 'DELETE', 'START', 'RETURN',
            ):
                result['paragraphs'].append({
                    'name': para_name
                })
        
        return result


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
                        result['calls'] = legacylens_result['calls']
                    if legacylens_result.get('performs'):
                        result['performs'] = legacylens_result['performs']
                    if legacylens_result.get('sql_queries'):
                        result['sql_queries'] = legacylens_result['sql_queries']
                    if legacylens_result.get('io_files'):
                        result['file_operations'] = legacylens_result['io_files']
                    if legacylens_result.get('copybooks'):
                        result['copybooks'] = legacylens_result['copybooks']
            except Exception as e:
                self._unrecognized.append({'error': f'legacylens error: {str(e)}'})
        
        # Custom extraction for items legacylens might miss
        
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
    
    # Augmentation configuration
    AUGMENTATION_MAX_RETRIES = 3
    AUGMENTATION_TIMEOUT = 60  # seconds per LLM call
    
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
        logger.warning("Could not detect encoding, falling back to UTF-8")
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
        
        # Normalize source for parsing (join continuation lines)
        normalized = self._normalize_source(content)
        
        # Initialize result
        result = {
            'source_file': source_file,
        }
        
        # Run all registered parsers
        for parser_name, parser_class in ParserRegistry.all_parsers().items():
            parser = parser_class()
            parser_result = parser.parse(normalized, self._lines)
            
            if parser_result:
                # Map parser names to output keys
                key_mapping = {
                    'identification': 'identification_division',
                    'environment': 'environment_division',
                    'data': 'data_division',
                    'procedure': 'procedure_division',
                    'dependencies': 'dependencies',
                }
                key = key_mapping.get(parser_name, parser_name)
                result[key] = parser_result
            
            # Collect unrecognized patterns
            if parser.unrecognized:
                self._unrecognized.extend(parser.unrecognized)
        
        # Add program_name at top level if available
        if 'identification_division' in result:
            id_div = result['identification_division']
            if 'program_id' in id_div:
                result['program_name'] = id_div['program_id']
            elif 'class_id' in id_div:
                result['program_name'] = id_div['class_id']
        
        # Add unrecognized patterns if any
        if self._unrecognized:
            result['_unrecognized'] = self._unrecognized
        
        return result
    
    # =========================================================================
    # LLM AUGMENTATION
    # =========================================================================
    
    def _create_augmentation_agent(self):
        """Create LangGraph ReAct agent with RAG tool access.
        
        The agent can decide when to search COBOL documentation for context.
        """
        from langgraph.prebuilt import create_react_agent
        from app.config.llm_config import llm
        from app.core.tools.rag_tools import search_cobol_docs
        
        return create_react_agent(model=llm, tools=[search_cobol_docs])
    
    async def augment(self, parsed_data: dict) -> dict:
        """Augment parsed COBOL data with LLM-generated metadata.
        
        Adds:
        - description: Program-level description for C# service class naming
        - summary: Paragraph-level summary for C# method naming
        
        Args:
            parsed_data: The parsed COBOL JSON structure
            
        Returns:
            The augmented parsed data with description and summary fields
        """
        import asyncio
        from tenacity import retry, stop_after_attempt, wait_exponential
        
        agent = self._create_augmentation_agent()
        
        # Add program description
        try:
            parsed_data["description"] = await self._augment_program(agent, parsed_data)
        except Exception as e:
            logger.error(f"Failed to augment program description: {e}")
            parsed_data["description"] = None
            parsed_data["_augmentation_errors"] = parsed_data.get("_augmentation_errors", [])
            parsed_data["_augmentation_errors"].append(f"Program description failed: {e}")
        
        # Add paragraph summaries (sequential within file)
        if "procedure_division" in parsed_data:
            paragraphs = parsed_data["procedure_division"].get("paragraphs", [])
            # Get list of paragraph names for code extraction
            para_names = [p.get("name", "") for p in paragraphs]
            
            for i, para in enumerate(paragraphs):
                try:
                    # Extract code for this paragraph
                    para_code = self._extract_paragraph_code(
                        para.get("name", ""),
                        para_names[i + 1] if i + 1 < len(para_names) else None
                    )
                    para["summary"] = await self._augment_paragraph(
                        agent, para, parsed_data, para_code
                    )
                except Exception as e:
                    logger.error(f"Failed to augment paragraph {para.get('name')}: {e}")
                    para["summary"] = None
                    parsed_data["_augmentation_errors"] = parsed_data.get("_augmentation_errors", [])
                    parsed_data["_augmentation_errors"].append(
                        f"Paragraph {para.get('name')} failed: {e}"
                    )
        
        return parsed_data
    
    async def _augment_program(self, agent, parsed_data: dict) -> str:
        """Generate program description using LangGraph agent.
        
        The agent may use RAG tool to understand COBOL constructs.
        """
        import asyncio
        from tenacity import retry, stop_after_attempt, wait_exponential
        
        program_name = parsed_data.get("program_name", "UNKNOWN")
        id_div = parsed_data.get("identification_division", {})
        deps = parsed_data.get("dependencies", {})
        
        # Build context from parsed data
        copybooks = [c.get("copybook_name", "") for c in deps.get("copybooks", [])]
        calls = [c.get("program", "") if isinstance(c, dict) else c for c in deps.get("calls", [])]
        
        prompt = f"""Analyze this COBOL program and provide a concise business description.

Program ID: {program_name}
Author: {id_div.get('author', 'Unknown')}
Copybooks used: {', '.join(copybooks) if copybooks else 'None'}
Programs called: {', '.join(calls) if calls else 'None'}

If you need to understand specific COBOL constructs, use the search tool.

Provide a 1-2 sentence description of what this program does, suitable for naming a C# service class.
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
    
    def _extract_paragraph_code(
        self, para_name: str, next_para_name: str | None
    ) -> str:
        """Extract the code for a paragraph from the stored source.
        
        Args:
            para_name: Name of the paragraph to extract
            next_para_name: Name of the next paragraph (to know where to stop)
            
        Returns:
            The code content of the paragraph
        """
        if not self._source:
            return ""
        
        lines = self._source.split('\n')
        in_paragraph = False
        para_lines = []
        
        # Pattern to match paragraph start
        para_start_pattern = re.compile(
            rf'^\s{{0,7}}{re.escape(para_name)}\s*\.',
            re.IGNORECASE
        )
        
        # Pattern to match next paragraph or end markers
        if next_para_name:
            next_para_pattern = re.compile(
                rf'^\s{{0,7}}{re.escape(next_para_name)}\s*\.',
                re.IGNORECASE
            )
        else:
            next_para_pattern = None
        
        for line in lines:
            # Check if this line starts our paragraph
            if not in_paragraph and para_start_pattern.search(line):
                in_paragraph = True
                para_lines.append(line)
                continue
            
            if in_paragraph:
                # Check if we've hit the next paragraph
                if next_para_pattern and next_para_pattern.search(line):
                    break
                # Check for end of procedure division or another division
                if re.match(r'^\s{0,7}[A-Za-z0-9]+-', line.strip()):
                    # Might be another paragraph without us knowing
                    if len(para_lines) > 2:  # We have some content
                        break
                
                para_lines.append(line)
        
        return '\n'.join(para_lines)
    
    async def _augment_paragraph(
        self, agent, paragraph: dict, parsed_data: dict, para_code: str
    ) -> str:
        """Generate paragraph summary using LangGraph agent.
        
        The agent may use RAG tool for unfamiliar COBOL statements.
        """
        import asyncio
        from tenacity import retry, stop_after_attempt, wait_exponential
        
        para_name = paragraph.get("name", "UNKNOWN")
        program_name = parsed_data.get("program_name", "UNKNOWN")
        
        prompt = f"""Analyze this COBOL paragraph and provide a brief summary.

Program: {program_name}
Paragraph: {para_name}

Code:
{para_code if para_code else "(Code not available)"}

If you need to understand specific COBOL statements, use the search tool.

Provide a 1-sentence summary of what this paragraph does.
This will be used to name a C# method, so be concise and action-oriented.
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

