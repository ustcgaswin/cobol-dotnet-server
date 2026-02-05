"""
PL/1 Source Parser for Dependency Graph Generation (v3.1 - Dependency Focused)

EXPECTATIONS FROM THIS PARSER:
------------------------------
1. INPUT: Raw PL/I Source Code (EBCDIC or ASCII text).
2. OUTPUT: JSON structure optimized for Graph Nodes and Edges.

DEPENDENCY GRAPH MAPPING LOGIC:
------------------------------
The output of this parser is designed to create the following Graph Edges:

| Source Node (PL/I) | Relationship      | Target Node (Component) | Extracted Via Field               |
|--------------------|-------------------|-------------------------|-----------------------------------|
| Program A          | INCLUDES          | PL/I Copybook           | dependencies.includes (Preproc)   |
| Program A          | INCLUDES          | DB2 DCLGEN              | dependencies.includes (SQL)       |
| Program A          | CALLS_STATIC      | COBOL/ASM/PLI Program   | dependencies.calls (Type=STATIC)  |
| Program A          | CALLS_DYNAMIC     | COBOL/ASM/PLI Program   | dependencies.calls (Type=DYNAMIC) |
| Program A          | READS/WRITES      | JCL DDNAME              | io.file_descriptors.ddname        |
| Program A          | ACCESSES          | DB2 Table               | dependencies.sql_tables           |
| Program A          | EXECUTES          | CICS Transaction        | dependencies.cics_commands        |

USAGE:
    python pli_parser.py <path_to_pli_file> [-o output.json]
"""

import re
import json
import logging
import argparse
import os
import sys
from pathlib import Path
from abc import ABC, abstractmethod
from typing import List, Dict
from dataclasses import dataclass, field

# --- Framework Imports ---
# Assuming these exist in your environment, otherwise they act as placeholders
from app.db.enums import SourceFileType
from app.core.parsers.base import BaseParser

# =============================================================================
# CONFIGURATION
# =============================================================================

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger('pli_parser')

# =============================================================================
# SHARED CONTEXT & TOKENIZER
# =============================================================================

@dataclass
class ParseContext:
    source_file: str
    errors: List[Dict] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    def record_error(self, content: str, msg: str, line: int):
        self.errors.append({"line": line, "content": content[:100], "error": msg})
    
    def record_warning(self, msg: str):
        if msg not in self.warnings:
            self.warnings.append(msg)

class PLITokenizer:
    @staticmethod
    def get_statements(content: str) -> List[Dict]:
        clean_content = []
        i = 0
        n = len(content)
        in_string = False
        string_char = "'" 
        
        while i < n:
            # Strip comments
            if not in_string and content[i:i+2] == '/*':
                end = content.find('*/', i+2)
                if end == -1: i = n 
                else:
                    comment_body = content[i+2:end]
                    newlines = comment_body.count('\n')
                    clean_content.append('\n' * newlines + ' ')
                    i = end + 2
                continue
            
            # Handle Strings
            if content[i] in ("'", '"'):
                if not in_string:
                    in_string = True
                    string_char = content[i]
                elif content[i] == string_char:
                    if i+1 < n and content[i+1] == string_char:
                        clean_content.append(content[i] + content[i+1])
                        i += 2
                        continue
                    else:
                        in_string = False
            clean_content.append(content[i])
            i += 1
            
        full_text = "".join(clean_content)
        
        statements = []
        current_stmt = []
        start_line = 1
        line_number = 1
        in_string = False
        
        for char in full_text:
            if char == '\n':
                line_number += 1
                current_stmt.append(' ')
                continue
            if char in ("'", '"'):
                if not in_string: in_string = True
                elif in_string: in_string = False 
            
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
                if not current_stmt: start_line = line_number
                current_stmt.append(char)
        return statements

    @staticmethod
    def _identify_type(stmt: str) -> str:
        first = stmt.split()[0].upper() if stmt else ""
        if ':' in stmt and first not in ['EXEC', 'WHEN']: return "LABEL"
        if first in ['DCL', 'DECLARE']: return "DECLARE"
        if first in ['CALL', 'FETCH']: return "CALL"
        if first in ['IF', 'THEN', 'ELSE', 'SELECT', 'WHEN', 'OTHERWISE', 'END']: return "LOGIC"
        if first in ['DO', 'BEGIN', 'PROC', 'PROCEDURE', 'PACKAGE']: return "BLOCK_START"
        if first == 'END': return "BLOCK_END"
        if first == 'EXEC': return "EXEC"
        if first == 'ON': return "ON_UNIT"
        if first in ['SIGNAL', 'REVERT']: return "SIGNAL"
        if first.startswith('%') or stmt.startswith('%'): return "PREPROCESSOR"
        return "STATEMENT"

# =============================================================================
# SUB-PARSERS
# =============================================================================

class BaseSectionParser(ABC):
    @abstractmethod
    def parse(self, statements: List[Dict], ctx: ParseContext) -> dict: pass

class StructureParser(BaseSectionParser):
    def parse(self, statements: List[Dict], ctx: ParseContext) -> dict:
        program_id = None
        first_proc = None

        for stmt in statements:
            text = stmt['text']
            # We only care about Labels that define a Procedure
            if stmt['type'] == "LABEL" and ("PROC" in text.upper() or "PROCEDURE" in text.upper()):
                label_match = re.match(r'([\w\$]+)\s*:', text)
                if label_match:
                    current_name = label_match.group(1).upper()
                    if not first_proc:
                        first_proc = current_name
                    
                    # If this procedure is marked MAIN, it's the definitive Program ID
                    if re.search(r'OPTIONS\s*\(\s*MAIN\s*\)', text, re.IGNORECASE):
                        program_id = current_name
                        break # Optimization: found the main entry point

        # Return only the program_id; use the first procedure found as a fallback
        return {"program_id": program_id or first_proc}

class DependenciesParser(BaseSectionParser):
    """
    Handles Includes, Calls, SQL, and CICS using Deterministic Regex.
    """
    def parse(self, statements: List[Dict], ctx: ParseContext) -> dict:
        result = {"includes": [], "calls": [], "sql_tables": [], "cics_commands": []}
        
        for stmt in statements:
            text = stmt['text']
            upper = text.upper()
            line = stmt['line']
            
            if stmt['type'] == "PREPROCESSOR" and 'INCLUDE' in upper:
                match = re.search(r'(?:%|\+\+)\s*INCLUDE\s+([\w\$\@\#]+)', text, re.IGNORECASE)
                if match: 
                    result["includes"].append({"name": match.group(1).upper(), "type": "PREPROCESSOR", "line": line})

            if stmt['type'] == "CALL":
                is_fetch = text.lstrip().upper().startswith("FETCH ")
                if is_fetch:
                     match = re.match(r'FETCH\s+([\w\$\@\#]+)', text, re.IGNORECASE)
                     if match: result["calls"].append({"target": match.group(1).upper(), "type": "DYNAMIC_FETCH", "line": line})
                else:
                    match = re.match(r'CALL\s+([\w\$\@\#]+)', text, re.IGNORECASE)
                    if match:
                        target = match.group(1).upper()
                        ctype = "IMS_CALL" if target == 'PLITDLI' else "STATIC_CALL"
                        result["calls"].append({"target": target, "type": ctype, "line": line})

            if stmt['type'] == "EXEC":
                clean = " ".join(text.split())
                if 'EXEC SQL' in upper: self._parse_sql(clean, line, result, ctx)
                elif 'EXEC CICS' in upper: self._parse_cics(clean, line, result["cics_commands"], ctx)
                    
        return result

    def _parse_sql(self, text: str, line: int, result: dict, ctx: ParseContext):
        upper = text.upper()
        
        # 1. Heuristic: DCLGEN Include
        include_match = re.search(r'INCLUDE\s+([\w\$\@\#]+)', upper)
        if include_match and "SQLCA" not in include_match.group(1):
            result["includes"].append({"name": include_match.group(1).upper(), "type": "SQL_INCLUDE", "line": line})
            return

        # 2. Heuristic: Tables (FROM)
        from_matches = re.findall(r'\bFROM\s+([\w\$\@\#\.]+)', upper)
        for tbl in from_matches:
            if tbl not in ['(', 'SELECT']: result["sql_tables"].append({"table": tbl, "access": "READ", "line": line})

        # 3. Heuristic: Tables (UPDATE)
        update_match = re.search(r'UPDATE\s+([\w\$\@\#\.]+)', upper)
        if update_match: result["sql_tables"].append({"table": update_match.group(1), "access": "WRITE", "line": line})

        # 4. Heuristic: Tables (INSERT INTO)
        insert_match = re.search(r'INSERT\s+INTO\s+([\w\$\@\#\.]+)', upper)
        if insert_match: result["sql_tables"].append({"table": insert_match.group(1), "access": "WRITE", "line": line})

    def _parse_cics(self, text: str, line: int, output_list: list, ctx: ParseContext):
        match = re.search(r'EXEC\s+CICS\s+(\w+)', text, re.IGNORECASE)
        cmd = match.group(1).upper() if match else "UNKNOWN"
        output_list.append({"command": cmd, "statement": text, "line": line})

class IOParser(BaseSectionParser):
    """
    Parses File Declarations and I/O Operations for Graph Dependency mapping.
    (Business Logic parsing has been removed).
    """
    def parse(self, statements: List[Dict], ctx: ParseContext) -> dict:
        data = {
            "file_descriptors": [],
            "operations": []
        }
        
        for stmt in statements:
            text = stmt['text']
            upper = text.upper()
            line = stmt['line']
            typ = stmt['type']
            
            # 1. JCL Dependency (DCL FILE)
            if typ == "DECLARE" and "FILE" in upper:
                var_match = re.search(r'(?:DCL|DECLARE)\s+([\w\$]+)', text, re.IGNORECASE)
                internal_name = var_match.group(1).upper() if var_match else "UNKNOWN"
                title_match = re.search(r"TITLE\s*\(\s*'(.*?)'", text, re.IGNORECASE)
                
                # Logic: If TITLE('DD:MYFILE') exists, link to MYFILE. Else use internal name.
                dd_name = title_match.group(1).upper().replace('DD:', '').strip() if title_match else internal_name
                
                data["file_descriptors"].append({
                    "internal_name": internal_name,
                    "ddname": dd_name,
                    "line": line
                })

            # 2. I/O Usage (OPEN, READ, WRITE)
            elif re.match(r'\b(OPEN|CLOSE|READ|WRITE|REWRITE|DELETE)\b', upper):
                 file_match = re.search(r'\bFILE\s*\(\s*([\w\$]+)\s*\)', text, re.IGNORECASE)
                 if file_match:
                     data["operations"].append({
                         "op": upper.split()[0], 
                         "file": file_match.group(1).upper(), 
                         "line": line
                     })
                     
        return data

# =============================================================================
# MAIN ORCHESTRATOR
# =============================================================================

class PLIParser(BaseParser):
    FILE_TYPE = "pli"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']

    def parse(self, content: str, source_name: str) -> dict:
        return self._parse_string_logic(content, source_name)

    def parse_file(self, filepath: str) -> dict:
        path = Path(filepath)
        if not path.exists(): return {"error": "File not found"}
        raw = path.read_bytes()
        content = None
        encoding = 'utf-8'
        try: content = raw.decode('utf-8')
        except UnicodeDecodeError: pass
        if content is None:
            for cp in self.EBCDIC_CODEPAGES:
                try:
                    txt = raw.decode(cp)
                    if 'PROC' in txt or 'DCL' in txt:
                        content = txt; encoding = cp; break
                except: continue
        if content is None: content = raw.decode('latin-1', errors='replace')
        return self._parse_string_logic(content, path.name, encoding)

    def _parse_string_logic(self, content: str, source_name: str, encoding: str = 'utf-8') -> dict:
        ctx = ParseContext(source_file=source_name)
        statements = PLITokenizer.get_statements(content)
        
        structure_data = {}
        dependencies_data = {}
        io_data = {}

        parsers = [
            ("structure", StructureParser()),
            ("dependencies", DependenciesParser()),
            ("io", IOParser())
        ]
        
        for key, parser in parsers:
            try:
                if key == "structure": structure_data = parser.parse(statements, ctx)
                elif key == "dependencies": dependencies_data = parser.parse(statements, ctx)
                elif key == "io": io_data = parser.parse(statements, ctx)
            except Exception as e:
                logger.error(f"Parser {key} failed: {e}")
                ctx.record_error("Whole File", f"Parser {key} crashed: {str(e)}", 0)

        return {
            'meta': {
                'source_file': source_name, 
                'file_type': SourceFileType.PLI.value
            },
            'structure': structure_data,
            'dependencies': dependencies_data,
            'io': {
                "file_descriptors": io_data.get("file_descriptors", []),
                "operations": io_data.get("operations", [])
            },
            '_unrecognized': ctx.errors,
            '_warnings': ctx.warnings
        }

# =============================================================================
# CLI
# =============================================================================

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('input', help='Input PL/1 file path')
    parser.add_argument('-o', '--output', help='Output JSON file')
    args = parser.parse_args()
    if not os.path.exists(args.input): sys.exit(1)
    pli_parser = PLIParser()
    result = pli_parser.parse_file(args.input)
    json_output = json.dumps(result, indent=2)
    if args.output: Path(args.output).write_text(json_output, encoding='utf-8')
    else: print(json_output)

if __name__ == '__main__':
    main()