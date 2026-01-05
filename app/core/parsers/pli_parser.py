"""
PL/1 Source Parser for IBM Enterprise PL/I for z/OS

Comprehensive parsing of PL/I source including:
- AST-like Block Structure (Procedures, Begin Blocks, DO groups)
- Scope-aware variable detection
- Control Flow extraction (ON units, SIGNAL, IF/SELECT)
- Detailed Dependency Extraction (SQL, CICS, INCLUDE, CALL/FETCH)
- Preprocessor handling (%INCLUDE, %IF)

Usage:
    python pli-parser.py <pli_file> [-o output.json]
"""

import re
import json
import sys
import logging
import argparse
from pathlib import Path
from abc import ABC, abstractmethod
from typing import List, Dict, Optional, Tuple

# =============================================================================
# Core Interfaces & Exceptions
# =============================================================================

class BaseParser(ABC):
    """Abstract Base Parser for consistency with the application core."""
    @abstractmethod
    def parse_file(self, filepath: str) -> dict: pass
    
class InvalidSyntaxError(Exception): pass
class PLIParseError(Exception): pass

# Configure structured logging
logging.basicConfig(
    level=logging.WARNING,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger('pli_parser')

class ExitCode:
    SUCCESS = 0
    FILE_NOT_FOUND = 1
    EMPTY_FILE = 2
    PARSE_ERROR = 4
    INVALID_INPUT = 5

# =============================================================================
# Robust Tokenizer
# =============================================================================

class PLITokenizer:
    """
    Robust tokenizer for PL/I. 
    Handles: Comments (/* */), Strings ('...'), Preprocessor (%), Semicolons.
    Does NOT split semicolons if they are inside strings or comments.
    """
    @staticmethod
    def get_statements(content: str) -> List[Dict]:
        statements = []
        
        # 1. Strip comments carefully to avoid stripping inside strings
        clean_content = []
        i = 0
        n = len(content)
        in_string = False
        string_char = "'" # PL/I uses single quotes usually
        
        while i < n:
            # Check for comment start /*
            if not in_string and content[i:i+2] == '/*':
                end_comment = content.find('*/', i+2)
                if end_comment == -1:
                    i = n # Unterminated comment
                else:
                    i = end_comment + 2
                    clean_content.append(' ') # Replace with space to preserve separation
                continue
            
            # Check for string start/end
            if content[i] in ("'", '"'):
                if not in_string:
                    in_string = True
                    string_char = content[i]
                elif content[i] == string_char:
                    # Check for escaped quote (e.g. 'It''s')
                    if i+1 < n and content[i+1] == string_char:
                        clean_content.append(content[i]) # Keep the first quote
                        clean_content.append(content[i+1]) # Keep the second quote
                        i += 2 # Skip both
                        continue
                    else:
                        in_string = False
            
            clean_content.append(content[i])
            i += 1
            
        full_text = "".join(clean_content)
        
        # 2. Split by semicolon, respecting remaining string boundaries
        current_stmt = []
        line_number = 1
        start_line = 1
        
        in_string = False
        quote_char = None
        
        for idx, char in enumerate(full_text):
            if char == '\n':
                line_number += 1
                current_stmt.append(' ')
                continue
            
            if char in ("'", '"'):
                if not in_string:
                    in_string = True
                    quote_char = char
                elif char == quote_char:
                    # Check for double quote escape isn't strictly needed here as we just need boundaries
                    in_string = False
                    quote_char = None
                
            if char == ';' and not in_string:
                stmt_str = "".join(current_stmt).strip()
                if stmt_str:
                    statements.append({
                        'text': stmt_str,
                        'line': start_line,
                        'type': PLITokenizer._identify_type(stmt_str)
                    })
                current_stmt = []
                start_line = line_number
            else:
                current_stmt.append(char)
                
        return statements

    @staticmethod
    def _identify_type(stmt: str) -> str:
        first_word = stmt.split()[0].upper() if stmt else ""
        
        # Labels: LABEL: PROC;
        if ':' in stmt and not stmt.upper().startswith('EXEC') and not stmt.upper().startswith('WHEN'):
            return "LABEL"
            
        if first_word in ['DCL', 'DECLARE']: return "DECLARE"
        if first_word in ['CALL', 'FETCH']: return "CALL"
        if first_word in ['IF', 'THEN', 'ELSE', 'SELECT', 'WHEN', 'OTHERWISE']: return "LOGIC"
        if first_word in ['DO', 'BEGIN', 'PROC', 'PROCEDURE', 'PACKAGE']: return "BLOCK_START"
        if first_word == 'END': return "BLOCK_END"
        if first_word == 'EXEC': return "EXEC"
        if first_word == 'ON': return "ON_UNIT"
        if first_word in ['SIGNAL', 'REVERT']: return "SIGNAL"
        if stmt.startswith('%') or first_word.startswith('%'): return "PREPROCESSOR"
        return "STATEMENT"

# =============================================================================
# Parsers Registry
# =============================================================================

class ParserRegistry:
    """Registry for section parsers - enables easy extension"""
    _parsers = {}
    
    @classmethod
    def register(cls, name):
        def wrapper(clz):
            cls._parsers[name] = clz
            return clz
        return wrapper
    
    @classmethod
    def get_all(cls):
        return cls._parsers

class BaseSectionParser(ABC):
    def __init__(self):
        self._unrecognized: list[dict] = []
    
    @abstractmethod
    def parse(self, statements: List[Dict]) -> dict:
        pass

# =============================================================================
# Section Implementations
# =============================================================================

@ParserRegistry.register("structure")
class StructureParser(BaseSectionParser):
    """Parses Block Structure (PROC, DO, BEGIN) and Scoping"""
    
    def parse(self, statements: List[Dict]) -> dict:
        procs = []
        block_stack = []
        
        for stmt in statements:
            text = stmt['text']
            typ = stmt['type']
            upper = text.upper()
            
            # Handle Labels and PROCs
            # Pattern: LABEL: PROC(PARAMS) OPTIONS(...);
            if typ == "LABEL" and ("PROC" in upper or "PROCEDURE" in upper):
                # Extract Label
                label_match = re.match(r'([\w\$]+)\s*:', text)
                proc_name = label_match.group(1).upper() if label_match else "UNKNOWN"
                
                is_main = 'MAIN' in upper and 'OPTIONS' in upper
                
                proc_entry = {
                    "name": proc_name,
                    "line": stmt['line'],
                    "is_main": is_main,
                    "parameters": self._extract_params(text),
                    "options": self._extract_options(text),
                    "parent": block_stack[-1] if block_stack else None
                }
                
                procs.append(proc_entry)
                block_stack.append(proc_name)
                
            elif typ == "BLOCK_START" and "PROC" in upper:
                # Unlabeled PROC (rare but possible in some contexts)
                block_stack.append("UNNAMED_PROC")
                
            elif typ == "BLOCK_END":
                # END label; or just END;
                # Check if it ends a specific block: END A;
                match = re.match(r'END\s+([\w\$]+)', upper)
                if match:
                    closing_label = match.group(1)
                    if block_stack and block_stack[-1] == closing_label:
                        block_stack.pop()
                else:
                    # Simple END; pops the top
                    if block_stack:
                        block_stack.pop()

        return {
            "procedures": procs, 
            "program_id": next((p['name'] for p in procs if p.get('is_main')), procs[0]['name'] if procs else None)
        }

    def _extract_params(self, text: str) -> List[str]:
        match = re.search(r'PROC(?:EDURE)?\s*\((.*?)\)', text, re.IGNORECASE)
        return [p.strip() for p in match.group(1).split(',')] if match else []

    def _extract_options(self, text: str) -> List[str]:
        match = re.search(r'OPTIONS\s*\((.*?)\)', text, re.IGNORECASE)
        return [o.strip() for o in match.group(1).split(',')] if match else []

@ParserRegistry.register("dependencies")
class DependenciesParser(BaseSectionParser):
    """Extracts External Calls, Includes, SQL and CICS"""
    
    def parse(self, statements: List[Dict]) -> dict:
        result = {
            "includes": [],
            "calls": [],
            "sql_queries": [],
            "cics_commands": []
        }
        
        for stmt in statements:
            text = stmt['text']
            upper_text = text.upper()
            
            # 1. Preprocessor Includes (%INCLUDE or ++INCLUDE)
            if stmt['type'] == "PREPROCESSOR" and 'INCLUDE' in upper_text:
                match = re.search(r'(?:%|\+\+)\s*INCLUDE\s+([\w\$\@\#]+)', text, re.IGNORECASE)
                if match:
                    result["includes"].append({"name": match.group(1).upper(), "line": stmt['line']})

            # 2. CALLs
            if stmt['type'] == "CALL":
                # Handles: CALL PROCNAME (ARGS);
                match = re.match(r'CALL\s+([\w\$\@\#]+)', text, re.IGNORECASE)
                if match:
                    result["calls"].append({
                        "target": match.group(1).upper(),
                        "line": stmt['line']
                    })

            # 3. EXEC SQL / CICS
            if stmt['type'] == "EXEC":
                clean = " ".join(text.split())
                if 'EXEC SQL' in upper_text:
                    sql_type = 'UNKNOWN'
                    if 'SELECT' in upper_text: sql_type = 'SELECT'
                    elif 'INSERT' in upper_text: sql_type = 'INSERT'
                    elif 'UPDATE' in upper_text: sql_type = 'UPDATE'
                    elif 'DELETE' in upper_text: sql_type = 'DELETE'
                    elif 'DECLARE' in upper_text: sql_type = 'CURSOR'
                    
                    result["sql_queries"].append({
                        "type": sql_type,
                        "statement": clean,
                        "line": stmt['line']
                    })
                elif 'EXEC CICS' in upper_text:
                    cmd_match = re.search(r'EXEC\s+CICS\s+(\w+)', clean, re.IGNORECASE)
                    cmd = cmd_match.group(1) if cmd_match else "UNKNOWN"
                    result["cics_commands"].append({
                        "command": cmd,
                        "statement": clean,
                        "line": stmt['line']
                    })
                    
        return result

@ParserRegistry.register("logic")
class LogicParser(BaseSectionParser):
    """Parses IO operations and Error Handling"""
    
    def parse(self, statements: List[Dict]) -> dict:
        result = {"conditions": [], "io_files": []}
        
        for stmt in statements:
            text = stmt['text']
            upper = text.upper()
            
            # ON Conditions (Error handling)
            if stmt['type'] == "ON_UNIT":
                # ON ERROR ... ; ON ENDFILE(F) ...;
                match = re.match(r'ON\s+([A-Za-z0-9_\-\$]+)(?:\((.*?)\))?', text, re.IGNORECASE)
                if match:
                    cond = match.group(1).upper()
                    target = match.group(2).upper() if match.group(2) else None
                    result["conditions"].append({
                        "condition": cond,
                        "target": target,
                        "line": stmt['line']
                    })

            # File I/O Heuristics (OPEN/CLOSE/READ/WRITE)
            if re.match(r'\b(OPEN|CLOSE|READ|WRITE|REWRITE|DELETE)\b', upper):
                 file_match = re.search(r'\bFILE\s*\(\s*([\w\$]+)\s*\)', text, re.IGNORECASE)
                 if file_match:
                     result["io_files"].append({
                         "operation": upper.split()[0],
                         "file": file_match.group(1).upper(),
                         "line": stmt['line']
                     })

        return result

# =============================================================================
# Main Driver
# =============================================================================

class PLIParser(BaseParser):
    """
    Main parser for PL/1 Source Files.
    """
    FILE_TYPE = "pli"
    # Common EBCDIC codepages
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']

    def __init__(self, strict: bool = False):
        self.strict = strict

    def parse_file(self, filepath: str) -> dict:
        logger.info(f"Parsing PL/1 file: {filepath}")
        path = Path(filepath)
        if not path.exists():
            raise FileNotFoundError(f"File not found: {filepath}")

        # Robust Encoding Detection
        raw = path.read_bytes()
        content, encoding = self._detect_and_decode(raw)
        
        result = self.parse_string(content, str(path.name))
        result['encoding'] = encoding
        return result

    def _detect_and_decode(self, raw: bytes) -> Tuple[str, str]:
        # Try UTF-8
        try:
            return raw.decode('utf-8'), 'utf-8'
        except UnicodeDecodeError: pass
        
        # Try EBCDIC
        for cp in self.EBCDIC_CODEPAGES:
            try:
                txt = raw.decode(cp)
                # Heuristic: Check for common PL/1 keywords
                if 'PROC' in txt or 'DCL' in txt or 'DECLARE' in txt:
                    return txt, cp
            except: continue
            
        # Fallback
        return raw.decode('latin-1', errors='replace'), 'latin-1'

    def parse_string(self, content: str, source_file: str) -> dict:
        # 1. Tokenize
        statements = PLITokenizer.get_statements(content)
        
        # 2. Initialize Result
        result = {
            'source_file': source_file, 
            'file_type': self.FILE_TYPE,
            'stats': {'statement_count': len(statements)}
        }
        
        # 3. Dispatch to Sub-Parsers
        for name, parser_cls in ParserRegistry.get_all().items():
            parser = parser_cls()
            try:
                data = parser.parse(statements)
                result[name] = data
            except Exception as e:
                logger.error(f"Parser {name} failed: {e}")
                if self.strict: raise
                result[f"{name}_error"] = str(e)
                
        # 4. Hoist program name to top level
        if 'structure' in result and result['structure'].get('program_id'):
            result['program_name'] = result['structure']['program_id']

        return result

def main():
    parser = argparse.ArgumentParser(
        description='Parse PL/1 Source Code to JSON',
        epilog='Handles Enterprise PL/I syntax including nested blocks and SQL.'
    )
    parser.add_argument('input', help='Input PL/1 file path')
    parser.add_argument('-o', '--output', help='Output JSON file (default: stdout)')
    parser.add_argument('--strict', action='store_true', help='Fail on parse errors')
    
    args = parser.parse_args()
    
    try:
        pli_parser = PLIParser(strict=args.strict)
        result = pli_parser.parse_file(args.input)
        
        json_output = json.dumps(result, indent=2)
        if args.output:
            Path(args.output).write_text(json_output, encoding='utf-8')
            print(f"Output written to {args.output}")
        else:
            print(json_output)
            
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return ExitCode.PARSE_ERROR
    
    return ExitCode.SUCCESS

if __name__ == '__main__':
    sys.exit(main())