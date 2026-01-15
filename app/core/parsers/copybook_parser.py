"""
COBOL Copybook Parser for IBM Enterprise COBOL for z/OS Version 4

Parses COBOL copybook files and outputs comprehensive JSON including:
- Field hierarchy (levels 01-49, 66, 77, 88)
- PIC clauses with computed lengths
- OCCURS clauses (fixed and DEPENDING ON)
- REDEFINES relationships
- RENAMES (66-level)
- Condition names (88-level)
- USAGE/COMP types
- VALUE clauses
- Special clauses (JUSTIFIED, SYNCHRONIZED, BLANK WHEN ZERO)
- IBM extensions (EXTERNAL, GLOBAL, SAME AS, GROUP-USAGE)

Usage:
    python copybook-parser.py <copybook_file> [-o output.json]
"""

import re
import json
import sys
import logging
import argparse
from pathlib import Path
from dataclasses import dataclass, field
from typing import Optional
from enum import Enum

from app.core.parsers.base import BaseParser
from app.core.exceptions import (
    CopybookParseError,
    InvalidSyntaxError,
    UnsupportedFeatureError,
)

# Configure structured logging
logging.basicConfig(
    level=logging.WARNING,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger('copybook_parser')


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




class UsageType(Enum):
    """COBOL USAGE types"""
    DISPLAY = "DISPLAY"
    DISPLAY_1 = "DISPLAY-1"  # DBCS
    NATIONAL = "NATIONAL"
    BINARY = "BINARY"
    COMP = "COMP"
    COMP_1 = "COMP-1"  # Single-precision floating point
    COMP_2 = "COMP-2"  # Double-precision floating point
    COMP_3 = "COMP-3"  # Packed decimal
    COMP_4 = "COMP-4"  # Same as BINARY
    COMP_5 = "COMP-5"  # Native binary
    POINTER = "POINTER"
    INDEX = "INDEX"
    PROCEDURE_POINTER = "PROCEDURE-POINTER"
    FUNCTION_POINTER = "FUNCTION-POINTER"
    OBJECT_REFERENCE = "OBJECT REFERENCE"


@dataclass
class Condition:
    """88-level condition name"""
    name: str
    values: list[str]
    through_values: list[tuple[str, str]] = field(default_factory=list)
    false_value: Optional[str] = None  # For WHEN SET TO FALSE


@dataclass  
class CopybookField:
    """Represents a single field in the copybook"""
    level: int
    name: str
    line_number: int
    picture: Optional[str] = None
    usage: Optional[str] = None
    occurs: Optional[int] = None
    occurs_min: Optional[int] = None
    occurs_max: Optional[int] = None
    occurs_depending_on: Optional[str] = None
    indexed_by: list[str] = field(default_factory=list)
    ascending_key: Optional[str] = None
    descending_key: Optional[str] = None
    redefines: Optional[str] = None
    renames: Optional[str] = None
    renames_through: Optional[str] = None
    value: Optional[str] = None
    justified: bool = False
    synchronized: bool = False
    blank_when_zero: bool = False
    sign_clause: Optional[str] = None
    computed_length: Optional[int] = None
    computed_decimals: Optional[int] = None
    # Position tracking
    start_position: Optional[int] = None  # 1-based byte offset
    end_position: Optional[int] = None    # 1-based end byte
    # IBM Extensions
    is_external: bool = False
    is_global: bool = False
    is_volatile: bool = False  # VOLATILE clause
    same_as: Optional[str] = None
    type_def: Optional[str] = None  # TYPEDEF clause
    group_usage: Optional[str] = None  # GROUP-USAGE NATIONAL
    property_clause: Optional[str] = None  # PROPERTY clause for OO COBOL
    # Additional
    conditions: list[Condition] = field(default_factory=list)
    children: list['CopybookField'] = field(default_factory=list)
    
    def to_dict(self) -> dict:
        """Convert to dictionary, omitting None/empty values"""
        result = {"level": self.level, "name": self.name}
        
        if self.picture:
            result["picture"] = self.picture
        if self.usage:
            result["usage"] = self.usage
        if self.occurs:
            result["occurs"] = self.occurs
        if self.occurs_min is not None:
            result["occurs_min"] = self.occurs_min
        if self.occurs_max is not None:
            result["occurs_max"] = self.occurs_max
        if self.occurs_depending_on:
            result["occurs_depending_on"] = self.occurs_depending_on
        if self.indexed_by:
            result["indexed_by"] = self.indexed_by
        if self.ascending_key:
            result["ascending_key"] = self.ascending_key
        if self.descending_key:
            result["descending_key"] = self.descending_key
        if self.redefines:
            result["redefines"] = self.redefines
        if self.renames:
            result["renames"] = self.renames
            if self.renames_through:
                result["renames_through"] = self.renames_through
        if self.value:
            result["value"] = self.value
        if self.justified:
            result["justified"] = True
        if self.synchronized:
            result["synchronized"] = True
        if self.blank_when_zero:
            result["blank_when_zero"] = True
        if self.sign_clause:
            result["sign_clause"] = self.sign_clause
        if self.computed_length:
            result["computed_length"] = self.computed_length
        if self.computed_decimals:
            result["computed_decimals"] = self.computed_decimals
        # Position tracking
        if self.start_position is not None:
            result["start_position"] = self.start_position
        if self.end_position is not None:
            result["end_position"] = self.end_position
        # IBM Extensions
        if self.is_external:
            result["is_external"] = True
        if self.is_global:
            result["is_global"] = True
        if self.is_volatile:
            result["is_volatile"] = True
        if self.same_as:
            result["same_as"] = self.same_as
        if self.type_def:
            result["type_def"] = self.type_def
        if self.group_usage:
            result["group_usage"] = self.group_usage
        if self.property_clause:
            result["property_clause"] = self.property_clause
        # Conditions
        if self.conditions:
            result["conditions"] = [
                {"name": c.name, "values": c.values, 
                 **({"through_values": c.through_values} if c.through_values else {}),
                 **({"false_value": c.false_value} if c.false_value else {})}
                for c in self.conditions
            ]
        if self.children:
            result["children"] = [child.to_dict() for child in self.children]
            
        result["line_number"] = self.line_number
        return result


class CopybookParser(BaseParser):
    """
    Parser for COBOL copybook files.
    
    Extensible design - add new clause parsers by adding methods named _parse_<clause>
    """
    
    FILE_TYPE = "copybook"
    
    # Regex patterns for COBOL constructs
    PATTERNS = {
        'level': r'^\s*(\d{2})\s+',
        'name': r'([A-Za-z][A-Za-z0-9_-]*|FILLER)',
        'pic': r'(?:PIC(?:TURE)?)\s+(?:IS\s+)?([^\s.]+(?:\([^)]+\)[^\s.]*)*)',
        'usage': r'(?:USAGE\s+(?:IS\s+)?)?'
                 r'(DISPLAY-1|DISPLAY|NATIONAL|BINARY|COMP-[1-5]|COMP|POINTER|INDEX|'
                 r'PROCEDURE-POINTER|FUNCTION-POINTER|OBJECT\s+REFERENCE)',
        'occurs': r'OCCURS\s+(\d+)\s*(?:TIMES)?',
        'occurs_depending': r'OCCURS\s+(\d+)\s+TO\s+(\d+)\s*(?:TIMES)?\s+'
                           r'DEPENDING\s+(?:ON\s+)?([A-Za-z][A-Za-z0-9_-]*)',
        'indexed_by': r'INDEXED\s+(?:BY\s+)?([A-Za-z][A-Za-z0-9_-]*(?:\s+[A-Za-z][A-Za-z0-9_-]*)*)',
        'ascending_key': r'ASCENDING\s+(?:KEY\s+(?:IS\s+)?)?([A-Za-z][A-Za-z0-9_-]*)',
        'descending_key': r'DESCENDING\s+(?:KEY\s+(?:IS\s+)?)?([A-Za-z][A-Za-z0-9_-]*)',
        'redefines': r'REDEFINES\s+([A-Za-z][A-Za-z0-9_-]*)',
        'renames': r'RENAMES\s+([A-Za-z][A-Za-z0-9_-]*)'
                   r'(?:\s+(?:THRU|THROUGH)\s+([A-Za-z][A-Za-z0-9_-]*))?',
        'value': r'VALUE\s+(?:IS\s+)?(.+?)(?=\s+PIC|\s+USAGE|\s+OCCURS|\s+REDEFINES|'
                 r'\s+EXTERNAL|\s+GLOBAL|\s+SAME|\s+GROUP-USAGE|\s*\.|$)',
        'justified': r'(?:JUST(?:IFIED)?)\s*(?:RIGHT)?',
        'synchronized': r'(?:SYNC(?:HRONIZED)?)\s*(?:LEFT|RIGHT)?',
        'blank_zero': r'BLANK\s+(?:WHEN\s+)?ZERO',
        'sign': r'SIGN\s+(?:IS\s+)?(LEADING|TRAILING)(?:\s+SEPARATE(?:\s+CHARACTER)?)?',
        'condition_88': r'^\s*88\s+([A-Za-z][A-Za-z0-9_-]*)\s+VALUES?\s+(?:ARE?\s+)?(.+?)(?:\s+WHEN\s+SET\s+TO\s+FALSE\s+(?:IS\s+)?(.+?))?\..*$',
        # IBM Extensions
        'external': r'\bEXTERNAL\b',
        'global': r'\bGLOBAL\b',
        'same_as': r'SAME\s+AS\s+([A-Za-z][A-Za-z0-9_-]*)',
        'type_def': r'TYPE(?:DEF)?\s+([A-Za-z][A-Za-z0-9_-]*)',
        'group_usage': r'GROUP-USAGE\s+(?:IS\s+)?(NATIONAL|BIT)',
        'property': r'PROPERTY\s+(?:IS\s+)?(.+?)(?=\s+PIC|\s+USAGE|\s*\.|$)',
        'volatile': r'\bVOLATILE\b',
    }
    
    def __init__(self, strict: bool = False):
        """
        Initialize parser.
        
        Args:
            strict: If True, raise errors on unrecognized patterns. If False, log warnings.
        """
        self.strict = strict
        self.fields: list[CopybookField] = []
        self.root_fields: list[CopybookField] = []
        self._current_line = 0
        self._source_lines: list[str] = []
        self._unrecognized: list[dict] = []
        self._warnings: list[str] = []
    # Common EBCDIC codepages for IBM mainframes
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    
    # Limits
    MAX_FILE_SIZE = 10 * 1024 * 1024  # 10 MB
        
    def parse_file(self, filepath: str) -> dict:
        """Parse a copybook file and return structured JSON"""
        logger.info(f"Starting to parse file: {filepath}")
        
        path = Path(filepath)
        
        # Input validation
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            raise FileNotFoundError(f"Copybook file not found: {filepath}")
        
        if not path.is_file():
            logger.error(f"Path is not a file: {filepath}")
            raise InvalidSyntaxError(f"Path is not a file: {filepath}")
        
        file_size = path.stat().st_size
        logger.debug(f"File size: {file_size} bytes")
        
        if file_size == 0:
            logger.error(f"Empty file: {filepath}")
            raise InvalidSyntaxError("File is empty", content=filepath)
        
        if file_size > self.MAX_FILE_SIZE:
            logger.error(f"File too large: {file_size} bytes (max: {self.MAX_FILE_SIZE})")
            raise InvalidSyntaxError(
                f"File exceeds maximum size of {self.MAX_FILE_SIZE // 1024 // 1024} MB"
            )
        
        # Read file as bytes first to detect encoding
        logger.debug("Reading file as bytes for encoding detection")
        raw_bytes = path.read_bytes()
        
        # Try to detect and convert encoding
        content, detected_encoding = self._detect_and_decode(raw_bytes)
        logger.info(f"Detected encoding: {detected_encoding}")
        
        result = self.parse_string(content, source_file=str(path.name))
        
        # Add encoding info to result if not standard UTF-8
        if detected_encoding and detected_encoding not in ('utf-8', 'ascii'):
            result['detected_encoding'] = detected_encoding
        
        logger.info(f"Successfully parsed {len(self.fields)} fields from {filepath}")
        return result
    
    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        """
        Detect file encoding and decode to string.
        Returns (decoded_string, detected_encoding).
        """
        # First, try UTF-8 (most common for transferred files)
        try:
            text = raw_bytes.decode('utf-8')
            # Check if the decoded text looks like valid COBOL
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
        
        # Try common EBCDIC codepages
        for codepage in self.EBCDIC_CODEPAGES:
            try:
                text = raw_bytes.decode(codepage)
                if self._looks_like_cobol(text):
                    logger.info(f"Detected EBCDIC encoding: {codepage}")
                    return text, codepage
            except (UnicodeDecodeError, LookupError):
                continue
        
        # Fallback: force UTF-8 with error replacement
        logger.info("Could not detect encoding, falling back to UTF-8 with replacement")
        return raw_bytes.decode('utf-8', errors='replace'), 'utf-8-fallback'
    
    def _looks_like_cobol(self, text: str) -> bool:
        """
        Heuristic to check if decoded text looks like valid COBOL.
        Checks for common COBOL keywords and structure.
        """
        # Check for unprintable characters (sign of wrong encoding)
        unprintable_count = sum(1 for c in text[:1000] if ord(c) < 32 and c not in '\n\r\t')
        if unprintable_count > 10:
            return False
        
        # Check for common COBOL keywords
        cobol_keywords = ['PIC', 'PICTURE', 'OCCURS', 'REDEFINES', 'VALUE', 
                          'COMP', 'BINARY', 'FILLER', 'SECTION', 'DIVISION']
        text_upper = text.upper()
        keyword_count = sum(1 for kw in cobol_keywords if kw in text_upper)
        
        # If at least 2 keywords found, likely COBOL
        return keyword_count >= 2
    
    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        """Parse copybook content string and return structured JSON"""
        self.fields = []
        self.root_fields = []
        self._unrecognized = []
        self._warnings = []
        self._copy_references = []  # Track COPY statements
        self._constants = []  # Track CONSTANT declarations
        
        # Preprocess: handle continuation, remove comments, join lines
        lines = self._preprocess(content)
        self._source_lines = lines
        
        # Parse each statement
        statements = self._split_statements(lines)
        
        for stmt_info in statements:
            try:
                self._parse_statement(stmt_info)
            except Exception as e:
                error_msg = f"Error parsing line {stmt_info['line']}: {e}"
                if self.strict:
                    raise ValueError(error_msg)
                logger.warning(error_msg)
                self._unrecognized.append({
                    'line': stmt_info['line'], 
                    'content': stmt_info['text'],
                    'error': str(e)
                })
        
        # Build hierarchy
        self._build_hierarchy()
        
        # Calculate positions and total length
        total_length = self._calculate_positions()
        
        # Construct output
        result = {
            "copybook_name": Path(source_file).stem.upper(),
            "source_file": source_file,
            "total_length": total_length,
            "record_layouts": [f.to_dict() for f in self.root_fields],
        }
        
        # Add COPY references if any
        if self._copy_references:
            result["copy_references"] = self._copy_references
        
        # Add constants if any
        if self._constants:
            result["constants"] = self._constants
        
        if self._unrecognized:
            result["_unrecognized"] = self._unrecognized
        
        if self._warnings:
            result["_warnings"] = self._warnings
            
        return result
    
    def _preprocess(self, content: str) -> list[tuple]:
        """Preprocess COBOL source: handle columns, continuations, comments"""
        lines = content.split('\n')
        processed = []
        
        for i, line in enumerate(lines):
            # Handle fixed-format COBOL (columns 7-72)
            if len(line) >= 7:
                indicator = line[6] if len(line) > 6 else ' '
                
                # Skip comment lines
                if indicator in ('*', '/', 'D', 'd'):  # D/d for debugging lines
                    continue
                    
                # Handle continuation
                if indicator == '-' and processed:
                    # Continuation line - append to previous
                    continuation = line[7:72] if len(line) >= 72 else line[7:]
                    # For string continuations, handle the leading quote
                    cont_text = continuation.lstrip()
                    if cont_text and cont_text[0] in ("'", '"'):
                        cont_text = cont_text[1:]  # Remove leading quote
                    processed[-1] = (processed[-1][0], 
                                    processed[-1][1].rstrip() + cont_text,
                                    processed[-1][2])
                    continue
                
                # Extract Area A and B (columns 8-72)
                text = line[7:72] if len(line) >= 72 else line[7:]
            else:
                text = line
                
            # Skip empty lines
            if text.strip():
                processed.append((i + 1, text, line))  # (line_number, text, original)
                
        return processed
    
    def _split_statements(self, lines: list[tuple]) -> list[dict]:
        """Split preprocessed lines into COBOL statements (period-terminated)"""
        statements = []
        current_stmt = []
        start_line = 0
        
        for line_num, text, original in lines:
            if not current_stmt:
                start_line = line_num
                
            current_stmt.append(text)
            
            # Check if statement ends with period
            # Be careful with periods inside quoted strings
            if self._has_terminating_period(text):
                full_text = ' '.join(current_stmt)
                # Handle the full statement
                statements.append({
                    'line': start_line,
                    'text': full_text,
                })
                current_stmt = []
        
        # Handle unterminated statement at end
        if current_stmt:
            full_text = ' '.join(current_stmt)
            if full_text.strip():
                self._warnings.append(f"Unterminated statement at line {start_line}")
                statements.append({
                    'line': start_line,
                    'text': full_text + '.',  # Add period
                })
                
        return statements
    
    def _has_terminating_period(self, text: str) -> bool:
        """Check if text ends with a terminating period (not inside quotes)"""
        in_quote = False
        quote_char = None
        
        for i, char in enumerate(text):
            if char in ("'", '"') and (i == 0 or text[i-1] != '\\'):
                if not in_quote:
                    in_quote = True
                    quote_char = char
                elif char == quote_char:
                    in_quote = False
                    quote_char = None
            elif char == '.' and not in_quote:
                # Check if this is end of statement (not decimal point in PIC)
                remaining = text[i+1:].strip()
                if not remaining or remaining[0].isupper() or remaining[0].isdigit():
                    return True
        
        # Check if last non-whitespace char is period
        stripped = text.rstrip()
        return stripped and stripped[-1] == '.'
    
    def _parse_statement(self, stmt_info: dict):
        """Parse a single COBOL statement"""
        text = stmt_info['text']
        line_num = stmt_info['line']
        
        # Check for level number
        level_match = re.match(self.PATTERNS['level'], text)
        if not level_match:
            # Not a data definition line, skip
            return
            
        level = int(level_match.group(1))
        remaining = text[level_match.end():].strip()
        
        # Handle 88-level conditions separately
        if level == 88:
            self._parse_condition(text, line_num)
            return
        
        # Handle 77-level (independent items) same as regular
        # Handle 66-level (RENAMES) - special handling in _parse_clauses
        
        # Extract field name
        name_match = re.match(self.PATTERNS['name'], remaining, re.IGNORECASE)
        if not name_match:
            self._unrecognized.append({'line': line_num, 'content': text, 'error': 'No field name found'})
            return
            
        name = name_match.group(1).upper()
        remaining = remaining[name_match.end():].strip()
        
        # Create field
        fld = CopybookField(level=level, name=name, line_number=line_num)
        
        # Parse clauses
        self._parse_clauses(fld, remaining, line_num)
        
        # Compute length from PIC
        if fld.picture:
            try:
                fld.computed_length, fld.computed_decimals = self._compute_pic_length(fld.picture, fld.usage)
            except Exception as e:
                self._warnings.append(f"Line {line_num}: Could not compute length for PIC {fld.picture}: {e}")
        
        self.fields.append(fld)
    
    def _parse_clauses(self, fld: CopybookField, text: str, line_num: int):
        """Parse all clauses in a field definition"""
        # Remove trailing period for cleaner parsing
        text = text.rstrip('.')
        
        # PIC clause - improved pattern for complex PICs
        pic_match = re.search(self.PATTERNS['pic'], text, re.IGNORECASE)
        if pic_match:
            fld.picture = pic_match.group(1).upper()
        
        # USAGE clause
        usage_match = re.search(self.PATTERNS['usage'], text, re.IGNORECASE)
        if usage_match:
            usage = usage_match.group(1)
            if usage:
                fld.usage = usage.upper().replace(' ', '-')
        
        # OCCURS DEPENDING ON (must check before simple OCCURS)
        occurs_dep_match = re.search(self.PATTERNS['occurs_depending'], text, re.IGNORECASE)
        if occurs_dep_match:
            fld.occurs_min = int(occurs_dep_match.group(1))
            fld.occurs_max = int(occurs_dep_match.group(2))
            fld.occurs_depending_on = occurs_dep_match.group(3).upper()
        else:
            # Simple OCCURS
            occurs_match = re.search(self.PATTERNS['occurs'], text, re.IGNORECASE)
            if occurs_match:
                fld.occurs = int(occurs_match.group(1))
        
        # INDEXED BY
        indexed_match = re.search(self.PATTERNS['indexed_by'], text, re.IGNORECASE)
        if indexed_match:
            fld.indexed_by = indexed_match.group(1).upper().split()
        
        # ASCENDING KEY
        asc_match = re.search(self.PATTERNS['ascending_key'], text, re.IGNORECASE)
        if asc_match:
            fld.ascending_key = asc_match.group(1).upper()
            
        # DESCENDING KEY
        desc_match = re.search(self.PATTERNS['descending_key'], text, re.IGNORECASE)
        if desc_match:
            fld.descending_key = desc_match.group(1).upper()
        
        # REDEFINES
        redef_match = re.search(self.PATTERNS['redefines'], text, re.IGNORECASE)
        if redef_match:
            fld.redefines = redef_match.group(1).upper()
        
        # RENAMES (for 66-level)
        if fld.level == 66:
            renames_match = re.search(self.PATTERNS['renames'], text, re.IGNORECASE)
            if renames_match:
                fld.renames = renames_match.group(1).upper()
                if renames_match.group(2):
                    fld.renames_through = renames_match.group(2).upper()
        
        # VALUE clause - improved to handle multi-value and complex values
        value_match = re.search(self.PATTERNS['value'], text, re.IGNORECASE)
        if value_match:
            value_str = value_match.group(1).strip()
            # Clean up the value
            value_str = value_str.rstrip('.')
            # Handle quoted strings
            if value_str.startswith("'") or value_str.startswith('"'):
                # Find matching end quote
                quote_char = value_str[0]
                end_idx = value_str.find(quote_char, 1)
                if end_idx > 0:
                    fld.value = value_str[1:end_idx]
                else:
                    fld.value = value_str.strip("'\"")
            else:
                # Numeric or figurative constant
                fld.value = value_str.split()[0] if value_str else None
        
        # JUSTIFIED
        if re.search(self.PATTERNS['justified'], text, re.IGNORECASE):
            fld.justified = True
        
        # SYNCHRONIZED
        if re.search(self.PATTERNS['synchronized'], text, re.IGNORECASE):
            fld.synchronized = True
        
        # BLANK WHEN ZERO
        if re.search(self.PATTERNS['blank_zero'], text, re.IGNORECASE):
            fld.blank_when_zero = True
        
        # SIGN clause
        sign_match = re.search(self.PATTERNS['sign'], text, re.IGNORECASE)
        if sign_match:
            fld.sign_clause = sign_match.group(0).upper()
        
        # ===== IBM EXTENSIONS =====
        
        # EXTERNAL
        if re.search(self.PATTERNS['external'], text, re.IGNORECASE):
            fld.is_external = True
        
        # GLOBAL
        if re.search(self.PATTERNS['global'], text, re.IGNORECASE):
            fld.is_global = True
        
        # SAME AS
        same_as_match = re.search(self.PATTERNS['same_as'], text, re.IGNORECASE)
        if same_as_match:
            fld.same_as = same_as_match.group(1).upper()
        
        # TYPEDEF
        type_match = re.search(self.PATTERNS['type_def'], text, re.IGNORECASE)
        if type_match:
            fld.type_def = type_match.group(1).upper()
        
        # GROUP-USAGE
        group_usage_match = re.search(self.PATTERNS['group_usage'], text, re.IGNORECASE)
        if group_usage_match:
            fld.group_usage = group_usage_match.group(1).upper()
        
        # PROPERTY (OO COBOL)
        property_match = re.search(self.PATTERNS['property'], text, re.IGNORECASE)
        if property_match:
            fld.property_clause = property_match.group(1).strip().upper()
        
        # VOLATILE (thread safety)
        if re.search(self.PATTERNS['volatile'], text, re.IGNORECASE):
            fld.is_volatile = True
        
        # ===== UNSUPPORTED FEATURE DETECTION =====
        self._check_unsupported_features(text, line_num)
    
    def _check_unsupported_features(self, text: str, line_num: int):
        """Extract additional features and check for truly unsupported patterns"""
        
        # ===== PARSE COPY STATEMENTS =====
        copy_match = re.search(
            r'\bCOPY\s+([A-Za-z][A-Za-z0-9_-]*)'
            r'(?:\s+(?:OF|IN)\s+([A-Za-z][A-Za-z0-9_-]*))?'
            r'(?:\s+REPLACING\s+(.+?))?(?:\.|$)',
            text, re.IGNORECASE
        )
        if copy_match:
            copy_ref = {
                'copybook_name': copy_match.group(1).upper(),
                'line_number': line_num
            }
            if copy_match.group(2):
                copy_ref['library'] = copy_match.group(2).upper()
            if copy_match.group(3):
                # Parse REPLACING clause
                replacing_text = copy_match.group(3)
                replacements = self._parse_replacing_clause(replacing_text)
                if replacements:
                    copy_ref['replacing'] = replacements
            self._copy_references.append(copy_ref)
            return  # COPY statement fully handled
        
        # ===== PARSE CONSTANT DECLARATIONS =====
        const_match = re.search(
            r'\b([A-Za-z][A-Za-z0-9_-]*)\s+CONSTANT\s+(?:AS\s+)?(.+?)(?:\.|$)',
            text, re.IGNORECASE
        )
        if const_match:
            self._constants.append({
                'name': const_match.group(1).upper(),
                'value': const_match.group(2).strip().strip("'\""),
                'line_number': line_num
            })
            return  # CONSTANT fully handled
        
        # ===== STILL UNSUPPORTED FEATURES =====
        # Only truly unsupported features that we cannot handle
        unsupported_patterns = [
            (r'\bANY\s+LENGTH\b', 'ANY LENGTH clause - field has dynamic length'),
            (r'\bBASED\b', 'BASED clause - pointer-based storage'),
            (r'\bSELECT\s+WHEN\b', 'SELECT WHEN - COBOL 2002+ feature'),
        ]
        
        for pattern, message in unsupported_patterns:
            if re.search(pattern, text, re.IGNORECASE):
                error = UnsupportedFeatureError(message, line_num, text)
                if self.strict:
                    raise error
                self._warnings.append(str(error))
                self._unrecognized.append({
                    'line': line_num,
                    'content': text,
                    'error': message,
                    'type': 'unsupported_feature'
                })
                break
    
    def _parse_replacing_clause(self, replacing_text: str) -> list[dict]:
        """Parse COPY REPLACING clause into structured replacements"""
        replacements = []
        
        # Pattern: old-text BY new-text
        pattern = re.compile(
            r'([^\s]+|==.+?==)\s+BY\s+([^\s]+|==.+?==)',
            re.IGNORECASE
        )
        
        for match in pattern.finditer(replacing_text):
            old_val = match.group(1).strip('=')
            new_val = match.group(2).strip('=')
            replacements.append({
                'from': old_val,
                'to': new_val
            })
        
        return replacements
    
    def _parse_condition(self, text: str, line_num: int):
        """Parse 88-level condition and attach to previous field"""
        match = re.match(self.PATTERNS['condition_88'], text, re.IGNORECASE)
        if not match:
            # Try simpler pattern
            simple_match = re.match(r'^\s*88\s+([A-Za-z][A-Za-z0-9_-]*)\s+VALUES?\s+(?:ARE?\s+)?(.+?)\.', 
                                   text, re.IGNORECASE)
            if not simple_match:
                self._unrecognized.append({'line': line_num, 'content': text, 'error': '88-level parse failed'})
                return
            match = simple_match
            
        name = match.group(1).upper()
        values_str = match.group(2).strip()
        false_value = match.group(3).strip() if len(match.groups()) > 2 and match.group(3) else None
        
        # Parse values (handle THRU/THROUGH)
        values = []
        through_values = []
        
        # Split by spaces/commas, handling quoted strings
        value_pattern = r"'[^']*'|\"[^\"]*\"|[^\s,]+"
        tokens = re.findall(value_pattern, values_str)
        
        i = 0
        while i < len(tokens):
            val = tokens[i].strip("'\"")
            if i + 2 < len(tokens) and tokens[i + 1].upper() in ('THRU', 'THROUGH'):
                through_val = tokens[i + 2].strip("'\"")
                through_values.append((val, through_val))
                i += 3
            else:
                values.append(val)
                i += 1
        
        condition = Condition(
            name=name, 
            values=values, 
            through_values=through_values,
            false_value=false_value.strip("'\"") if false_value else None
        )
        
        # Attach to most recent non-88 field
        if self.fields:
            self.fields[-1].conditions.append(condition)
        else:
            self._warnings.append(f"Line {line_num}: 88-level {name} has no parent field")
    
    def _compute_pic_length(self, pic: str, usage: Optional[str] = None) -> tuple[int, int]:
        """
        Compute byte length and decimal places from PIC clause.
        
        Handles:
        - Standard symbols: 9, X, A, S, V, P
        - Editing symbols: Z, *, $, +, -, ., ,
        - Floating editing: $$$, +++, ZZZ
        - CR/DB indicators
        - National/DBCS: N, G
        """
        length = 0
        decimals = 0
        in_decimal = False
        digit_count = 0
        pic_upper = pic.upper()
        
        # Handle CR and DB as single 2-character symbols first
        # They represent "CR" or "DB" printed, so 2 bytes
        cr_count = pic_upper.count('CR')
        db_count = pic_upper.count('DB')
        length += (cr_count + db_count) * 2
        
        # Remove CR/DB for further processing
        pic_cleaned = pic_upper.replace('CR', '').replace('DB', '')
        
        # Expand repeat counts: X(10) -> XXXXXXXXXX
        expanded = re.sub(
            r'([A-Za-z9$€£¥+\-.,ZB*/0])(?:\((\d+)\))?', 
            lambda m: m.group(1) * int(m.group(2) or 1), 
            pic_cleaned
        )
        
        logger.debug(f"PIC '{pic}' expanded to '{expanded}' (len before loop: {length})")
        
        i = 0
        while i < len(expanded):
            char = expanded[i]
            
            if char == 'V':
                in_decimal = True
            elif char == '9':
                length += 1
                digit_count += 1
                if in_decimal:
                    decimals += 1
            elif char in 'XA':
                length += 1
            elif char in 'Z':  # Zero suppression (still takes space)
                length += 1
                digit_count += 1
                if in_decimal:
                    decimals += 1
            elif char == '*':  # Check protection
                length += 1
                digit_count += 1
            elif char == 'B':  # Blank insertion
                length += 1
            elif char == '/':  # Slash insertion
                length += 1
            elif char == 'S':
                # Sign - handling depends on SIGN clause and USAGE
                # For DISPLAY with SIGN SEPARATE, add 1 byte
                # For DISPLAY with overpunch (default), no extra byte
                pass  # Default: overpunched, no extra space
            elif char in '$€£¥':  # Currency symbols (floating or fixed)
                length += 1
            elif char in '+-':  # Sign symbols
                length += 1
            elif char in '.,':  # Decimal point / comma
                length += 1
            elif char == 'P':
                # P = assumed decimal position, doesn't take storage
                decimals += 1
            elif char in 'N':  # National character
                length += 2  # 2 bytes per character
            elif char == 'G':  # DBCS
                length += 2  # 2 bytes per character
            elif char == '0':  # Zero insertion
                length += 1
            elif char == '1':  # Literal insertion
                length += 1
            
            i += 1
        
        # Adjust for COMP types (overrides the DISPLAY length)
        if usage in ('COMP', 'BINARY', 'COMP-4'):
            if digit_count <= 4:
                length = 2
            elif digit_count <= 9:
                length = 4
            else:
                length = 8
        elif usage in ('COMP-3', 'PACKED-DECIMAL'):
            # Packed decimal: (digits + 1) / 2, rounded up
            length = (digit_count + 2) // 2
        elif usage == 'COMP-1':
            length = 4  # Single precision float
        elif usage == 'COMP-2':
            length = 8  # Double precision float
        elif usage == 'COMP-5':
            if digit_count <= 4:
                length = 2
            elif digit_count <= 9:
                length = 4
            else:
                length = 8
        elif usage == 'POINTER':
            length = 4  # 32-bit pointer (8 on 64-bit)
        elif usage == 'INDEX':
            length = 4
        
        logger.debug(f"PIC '{pic}' -> length={length}, decimals={decimals}")
        return length, decimals
    
    def _build_hierarchy(self):
        """Build parent-child hierarchy based on level numbers"""
        if not self.fields:
            return
            
        stack: list[CopybookField] = []
        
        for fld in self.fields:
            # Handle 66-level (RENAMES) - stays at top level
            if fld.level == 66:
                self.root_fields.append(fld)
                continue
            
            # Handle 77-level (independent) - stays at top level
            if fld.level == 77:
                self.root_fields.append(fld)
                continue
                
            # Find parent: first field in stack with lower level number
            while stack and stack[-1].level >= fld.level:
                stack.pop()
            
            if stack:
                stack[-1].children.append(fld)
            else:
                self.root_fields.append(fld)
            
            # Push current field as potential parent
            stack.append(fld)
    
    def _calculate_positions(self) -> int:
        """
        Calculate byte positions for all fields.
        Returns the total record length.
        """
        # Build a map of field names to positions for REDEFINES lookup
        field_positions: dict[str, int] = {}
        
        def calc_field_positions(field: CopybookField, start_pos: int, siblings: list = None) -> int:
            """Recursively calculate positions. Returns next available position."""
            current_pos = start_pos
            
            # If field redefines another, it starts at same position as redefined field
            if field.redefines:
                redef_name = field.redefines.upper()
                if redef_name in field_positions:
                    current_pos = field_positions[redef_name]
                    logger.debug(f"Field {field.name} REDEFINES {redef_name} at position {current_pos}")
                else:
                    # Look in siblings if present
                    if siblings:
                        for sib in siblings:
                            if sib.name.upper() == redef_name and sib.start_position:
                                current_pos = sib.start_position
                                break
                    logger.warning(f"REDEFINES target '{redef_name}' not found, using current position")
            
            field.start_position = current_pos
            field_positions[field.name.upper()] = current_pos
            
            if field.children:
                # Group field - calculate children positions
                child_pos = current_pos
                for child in field.children:
                    child_pos = calc_field_positions(child, child_pos, field.children)
                field.end_position = child_pos - 1 if child_pos > current_pos else current_pos
                field.computed_length = child_pos - current_pos
                
                # REDEFINES doesn't advance position
                if not field.redefines:
                    current_pos = child_pos
            else:
                # Elementary field - use computed length
                field_len = field.computed_length or 0
                
                # Handle OCCURS - multiply by occurrence count
                if field.occurs:
                    field_len *= field.occurs
                elif field.occurs_max:
                    # For DEPENDING ON, use max occurrences for total length
                    field_len *= field.occurs_max
                
                field.end_position = current_pos + field_len - 1 if field_len > 0 else current_pos
                
                # REDEFINES doesn't advance position
                if not field.redefines:
                    current_pos += field_len
            
            return current_pos
        
        total_length = 0
        for root in self.root_fields:
            # Skip 66-level (RENAMES) - they don't add to length
            if root.level == 66:
                continue
            # Each root (01-level) starts at position 1
            end_pos = calc_field_positions(root, 1)
            root_len = end_pos - 1
            if root_len > total_length:
                total_length = root_len
        
        logger.debug(f"Total record length: {total_length}")
        return total_length


def main():
    parser = argparse.ArgumentParser(
        description='Parse COBOL copybook files to JSON',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('input', help='Input copybook file path')
    parser.add_argument('-o', '--output', help='Output JSON file (default: stdout)')
    parser.add_argument('--indent', type=int, default=2, help='JSON indent level')
    parser.add_argument('--strict', action='store_true', help='Raise errors on parse failures')
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable verbose logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger('copybook_parser').setLevel(logging.DEBUG)
    
    try:
        copybook_parser = CopybookParser(strict=args.strict)
        result = copybook_parser.parse_file(args.input)
        
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
    
    except CopybookParseError as e:
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
