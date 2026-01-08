"""
PL/1 Source Parser - Hybrid AI/Deterministic Engine (v2.2)

A production-grade parser for IBM Enterprise PL/I for z/OS.
Combines deterministic regex strategies with selective AI enrichment.

Updates in v2.2:
- FIXED: LangChain Prompt Template escaping (passed variables via invoke).
- FIXED: Line number drifting. Tokenizer now preserves newlines inside comments.
- RETAINED: [AI_CORRECTION] logging strategy.

Usage:
    python pli_parser.py <pli_file> [-o output.json]
"""
import re
import json
import sys
import logging
import argparse
import os
from pathlib import Path
from abc import ABC, abstractmethod
from typing import List, Dict, Optional, Tuple, Any
from dataclasses import dataclass, field

# =============================================================================
# CONFIGURATION & AI SETUP
# =============================================================================

MAX_AI_RETRIES_PER_FILE = 15  # Circuit Breaker

# Centralized LLM Import
ENABLE_AI = False
_llm_model = None

# Logger
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger('pli_parser')

try:
    # Adjust import to match your application structure
    from app.config.llm_config import get_llm as app_llm
    from langchain_core.prompts import ChatPromptTemplate
    from langchain_core.output_parsers import JsonOutputParser
    
    # Placeholder for actual import success check
    if 'app_llm' in globals() and app_llm:
        _llm_model = app_llm
        ENABLE_AI = True
        logger.info("AI Model initialized successfully.")
except ImportError:
    pass  # Run in deterministic mode
except Exception as e:
    logging.getLogger('pli_parser').error(f"Failed to initialize AI: {e}")

# =============================================================================
# SHARED CONTEXT & TOKENIZER
# =============================================================================

@dataclass
class ParseContext:
    """Shared state passed between sub-parsers."""
    source_file: str
    ai_calls: int = 0
    errors: List[Dict] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    def record_error(self, content: str, msg: str, line: int):
        """Records a hard failure."""
        self.errors.append({
            "line": line,
            "content": content[:100],
            "error": msg
        })
    
    def record_correction(self, content: str, reason: str, line: int):
        """
        Records a successful AI intervention.
        """
        self.errors.append({
            "line": line,
            "content": content[:100],
            "error": f"[AI_CORRECTION] Regex failed ({reason}). AI successfully inferred structure."
        })
    
    def record_warning(self, msg: str):
        if msg not in self.warnings:
            self.warnings.append(msg)

    def can_call_ai(self) -> bool:
        return ENABLE_AI and self.ai_calls < MAX_AI_RETRIES_PER_FILE

class PLITokenizer:
    """
    Robust tokenizer for PL/I. Handles comments, strings, semicolons.
    """
    @staticmethod
    def get_statements(content: str) -> List[Dict]:
        statements = []
        clean_content = []
        i = 0
        n = len(content)
        in_string = False
        string_char = "'" 
        
        # 1. Strip comments (/* ... */) but PRESERVE NEWLINES to keep line numbers accurate
        while i < n:
            if not in_string and content[i:i+2] == '/*':
                end = content.find('*/', i+2)
                if end == -1:
                    i = n # Unterminated
                else:
                    # Capture the comment content
                    comment_body = content[i+2:end]
                    # Count newlines inside the comment
                    newlines = comment_body.count('\n')
                    # Replace comment with equal newlines + space to maintain line count
                    clean_content.append('\n' * newlines + ' ')
                    i = end + 2
                continue
            
            if content[i] in ("'", '"'):
                if not in_string:
                    in_string = True
                    string_char = content[i]
                elif content[i] == string_char:
                    # Handle escaped quotes (e.g. 'It''s')
                    if i+1 < n and content[i+1] == string_char:
                        clean_content.append(content[i] + content[i+1])
                        i += 2
                        continue
                    else:
                        in_string = False
            
            clean_content.append(content[i])
            i += 1
            
        full_text = "".join(clean_content)
        
        # 2. Split by semicolon
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
                # Update start_line to current line for the NEXT statement
                start_line = line_number
            else:
                if not current_stmt:
                    # If this is the first char of a new stmt, record the start line
                    start_line = line_number
                current_stmt.append(char)
                
        return statements

    @staticmethod
    def _identify_type(stmt: str) -> str:
        first = stmt.split()[0].upper() if stmt else ""
        if ':' in stmt and first not in ['EXEC', 'WHEN']: return "LABEL"
        if first in ['DCL', 'DECLARE']: return "DECLARE"
        if first in ['CALL', 'FETCH']: return "CALL"
        if first in ['IF', 'THEN', 'ELSE', 'SELECT', 'WHEN', 'OTHERWISE']: return "LOGIC"
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
    """
    Parses Block Structure (PROC, DO, BEGIN), Scoping, and Program Meta.
    """
    def parse(self, statements: List[Dict], ctx: ParseContext) -> dict:
        procs = []
        block_stack = []
        
        for stmt in statements:
            text = stmt['text']
            typ = stmt['type']
            upper = text.upper()
            
            if typ == "LABEL" and ("PROC" in upper or "PROCEDURE" in upper):
                label_match = re.match(r'([\w\$]+)\s*:', text)
                proc_name = label_match.group(1).upper() if label_match else "UNKNOWN"
                
                params = self._extract_params(text)
                options = self._extract_options(text)
                is_main = 'MAIN' in [o.upper() for o in options]
                
                procs.append({
                    "name": proc_name,
                    "line": stmt['line'],
                    "is_main": is_main,
                    "parameters": params,
                    "options": options,
                    "parent": block_stack[-1] if block_stack else None
                })
                block_stack.append(proc_name)
                
            elif typ == "BLOCK_END":
                match = re.match(r'END\s+([\w\$]+)', upper)
                if match:
                    closing_label = match.group(1)
                    if block_stack and block_stack[-1] == closing_label:
                        block_stack.pop()
                    else:
                        ctx.record_warning(f"Mismatched END at line {stmt['line']}: Found {closing_label}, expected {block_stack[-1] if block_stack else 'None'}")
                else:
                    if block_stack: block_stack.pop()

        program_name = next((p['name'] for p in procs if p.get('is_main')), procs[0]['name'] if procs else None)
        
        return {
            "procedures": procs,
            "program_id": program_name
        }

    def _extract_params(self, text: str) -> List[str]:
        match = re.search(r'PROC(?:EDURE)?\s*\((.*?)\)', text, re.IGNORECASE)
        return [p.strip() for p in match.group(1).split(',')] if match else []

    def _extract_options(self, text: str) -> List[str]:
        match = re.search(r'OPTIONS\s*\((.*?)\)', text, re.IGNORECASE)
        return [o.strip() for o in match.group(1).split(',')] if match else []

class LogicParser(BaseSectionParser):
    """
    Extracts File I/O and Error Handling (ON units).
    """
    def parse(self, statements: List[Dict], ctx: ParseContext) -> dict:
        result = {"conditions": [], "io_files": []}
        
        for stmt in statements:
            text = stmt['text']
            upper = text.upper()
            line = stmt['line']
            
            # 1. ON Conditions
            if stmt['type'] == "ON_UNIT" or text.lstrip().upper().startswith("ON "):
                match = re.match(r'ON\s+([A-Za-z0-9_\-\$]+)(?:\((.*?)\))?', text, re.IGNORECASE)
                if match:
                    result["conditions"].append({
                        "condition": match.group(1).upper(),
                        "target": match.group(2).upper() if match.group(2) else None,
                        "line": line
                    })

            # 2. File I/O Heuristics
            if re.match(r'\b(OPEN|CLOSE|READ|WRITE|REWRITE|DELETE)\b', upper):
                 file_match = re.search(r'\bFILE\s*\(\s*([\w\$]+)\s*\)', text, re.IGNORECASE)
                 if file_match:
                     result["io_files"].append({
                         "operation": upper.split()[0],
                         "file": file_match.group(1).upper(),
                         "line": line
                     })
        return result

class DependenciesParser(BaseSectionParser):
    """
    Extracts External Calls, SQL, and CICS. 
    Uses Hybrid AI Fallback and records successful interventions.
    """
    
    def parse(self, statements: List[Dict], ctx: ParseContext) -> dict:
        result = {
            "includes": [],
            "calls": [],
            "sql_queries": [],
            "cics_commands": []
        }
        
        for stmt in statements:
            text = stmt['text']
            upper = text.upper()
            line = stmt['line']
            
            # 1. Includes
            if stmt['type'] == "PREPROCESSOR" and 'INCLUDE' in upper:
                match = re.search(r'(?:%|\+\+)\s*INCLUDE\s+([\w\$\@\#]+)', text, re.IGNORECASE)
                if match: result["includes"].append({"name": match.group(1).upper(), "line": line})

            # 2. Calls
            if stmt['type'] == "CALL":
                match = re.match(r'CALL\s+([\w\$\@\#]+)', text, re.IGNORECASE)
                if match: result["calls"].append({"target": match.group(1).upper(), "line": line})

            # 3. EXEC SQL / CICS (Hybrid Parsing)
            if stmt['type'] == "EXEC":
                clean = " ".join(text.split())
                
                if 'EXEC SQL' in upper:
                    self._parse_sql(clean, line, result["sql_queries"], ctx)
                elif 'EXEC CICS' in upper:
                    self._parse_cics(clean, line, result["cics_commands"], ctx)
                    
        return result

    def _parse_sql(self, text: str, line: int, output_list: list, ctx: ParseContext):
        # 1. Regex Heuristic
        upper = text.upper()
        sql_type = "UNKNOWN"
        
        if 'SELECT' in upper: sql_type = 'SELECT'
        elif 'INSERT' in upper: sql_type = 'INSERT'
        elif 'UPDATE' in upper: sql_type = 'UPDATE'
        elif 'DELETE' in upper: sql_type = 'DELETE'
        elif 'DECLARE' in upper: sql_type = 'CURSOR'
        
        # 2. AI Fallback check
        if sql_type == "UNKNOWN" or (sql_type == "SELECT" and "FROM" not in upper):
            if ctx.can_call_ai():
                ctx.ai_calls += 1
                try:
                    ai_res = self._ai_extract(text, "SQL")
                    output_list.append({
                        "type": ai_res.get('type', 'UNKNOWN'),
                        "tables": ai_res.get('tables', []),
                        "statement": text,
                        "line": line,
                        "ai_inferred": True
                    })
                    ctx.record_correction(text, "Unparseable SQL Pattern", line)
                    return
                except Exception as e:
                    ctx.record_error(text, f"AI SQL Parse Failed: {e}", line)
            else:
                 pass

        output_list.append({"type": sql_type, "statement": text, "line": line})

    def _parse_cics(self, text: str, line: int, output_list: list, ctx: ParseContext):
        # 1. Regex Heuristic
        match = re.search(r'EXEC\s+CICS\s+(\w+)', text, re.IGNORECASE)
        cmd = match.group(1).upper() if match else "UNKNOWN"
        
        # 2. AI Fallback
        if cmd == "UNKNOWN" and ctx.can_call_ai():
            ctx.ai_calls += 1
            try:
                ai_res = self._ai_extract(text, "CICS")
                output_list.append({
                    "command": ai_res.get('command', 'UNKNOWN'),
                    "options": ai_res.get('options', []),
                    "statement": text,
                    "line": line,
                    "ai_inferred": True
                })
                ctx.record_correction(text, "Unparseable CICS Command", line)
                return
            except Exception as e:
                ctx.record_error(text, f"AI CICS Parse Failed: {e}", line)

        output_list.append({"command": cmd, "statement": text, "line": line})

    def _ai_extract(self, text: str, mode: str) -> dict:
        # Requires external dependencies
        from langchain_core.prompts import ChatPromptTemplate
        from langchain_core.output_parsers import JsonOutputParser

        parser = JsonOutputParser()
        prompt_text = ""
        
        # NOTE: Using Templates instead of F-Strings to avoid JSON brace conflicts
        if mode == "SQL":
            prompt_text = """
            Analyze this embedded SQL statement.
            Statement: "{statement}"
            Return JSON: {{ "type": "SELECT/INSERT/UPDATE/DELETE/CURSOR", "tables": ["table_names"] }}
            """
        else:
            prompt_text = """
            Analyze this CICS command.
            Statement: "{statement}"
            Return JSON: {{ "command": "LINK/XCTL/READ...", "options": ["key_options"] }}
            """

        # Pass variables via invoke to handle escaping properly
        chain = ChatPromptTemplate.from_template(prompt_text) | _llm_model | parser
        return chain.invoke({"statement": text})

# =============================================================================
# MAIN PARSER
# =============================================================================

class PLIParser:
    FILE_TYPE = "pli"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']

    def parse_file(self, filepath: str) -> dict:
        path = Path(filepath)
        if not path.exists():
            return {"error": "File not found"}

        # Robust EBCDIC detection logic
        raw = path.read_bytes()
        content = None
        encoding = 'utf-8'

        try:
            content = raw.decode('utf-8')
        except UnicodeDecodeError:
            pass

        if content is None:
            for cp in self.EBCDIC_CODEPAGES:
                try:
                    txt = raw.decode(cp)
                    if 'PROC' in txt or 'DCL' in txt or 'DECLARE' in txt or '/*' in txt:
                        content = txt
                        encoding = cp
                        break
                except:
                    continue
        
        if content is None:
            content = raw.decode('latin-1', errors='replace')
            encoding = 'latin-1 (fallback)'
            
        return self.parse_string(content, path.name, encoding)

    def parse_string(self, content: str, source_name: str, encoding: str = 'utf-8') -> dict:
        ctx = ParseContext(source_file=source_name)
        statements = PLITokenizer.get_statements(content)
        
        result = {
            'meta': {
                'source_file': source_name, 
                'file_type': self.FILE_TYPE,
                'encoding': encoding,
                'stmts': len(statements)
            }
        }
        
        parsers = [
            ("structure", StructureParser()),
            ("dependencies", DependenciesParser()),
            ("logic", LogicParser())
        ]
        
        for key, parser in parsers:
            try:
                result[key] = parser.parse(statements, ctx)
            except Exception as e:
                logger.error(f"Parser {key} failed: {e}")
                ctx.record_error("Whole File", f"Parser {key} crashed: {str(e)}", 0)

        # 3. Standardized Error Reporting
        # Both true errors and [AI_CORRECTION] entries appear here
        result['parse_errors'] = ctx.errors
        result['_warnings'] = ctx.warnings
        
        if ctx.ai_calls > 0:
            result['meta']['ai_calls'] = ctx.ai_calls
            
        return result

# =============================================================================
# CLI
# =============================================================================

def main():
    parser = argparse.ArgumentParser(description='Hybrid PL/1 Source Parser (v2.2)')
    parser.add_argument('input', help='Input PL/1 file path')
    parser.add_argument('-o', '--output', help='Output JSON file')
    
    args = parser.parse_args()
    
    if not os.path.exists(args.input):
        print(f"Error: File {args.input} not found.")
        sys.exit(1)
        
    pli_parser = PLIParser()
    result = pli_parser.parse_file(args.input)
    
    json_output = json.dumps(result, indent=2)
    
    if args.output:
        Path(args.output).write_text(json_output, encoding='utf-8')
        print(f"Parsed {args.input} -> {args.output}")
        if result.get('parse_errors'):
            print(f"WARNING: {len(result['parse_errors'])} errors/corrections recorded.")
    else:
        print(json_output)

if __name__ == '__main__':
    main()