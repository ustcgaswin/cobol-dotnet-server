import os
import re
import json
import logging
import argparse
from pathlib import Path
from abc import ABC, abstractmethod
from typing import Dict, List, Optional, Any

from app.core.parsers.base import BaseParser

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('jcl_parser')

class ParseError(Exception):
    """Custom exception for parsing failures."""
    pass

class StatementRegistry:
    _parsers: Dict[str, Any] = {}
    
    @classmethod
    def register(cls, op_code: str):
        def decorator(parser_class):
            cls._parsers[op_code] = parser_class
            return parser_class
        return decorator
    
    @classmethod
    def get_parser(cls, op_code: str):
        return cls._parsers.get(op_code)

class BaseStatementParser(ABC):
    @abstractmethod
    def parse(self, label: str, params: str, context: dict):
        pass

@StatementRegistry.register("JOB")
class JobParser(BaseStatementParser):
    def parse(self, label: str, params: str, context: dict):
        context["result"]["type"] = "JCL"
        context["result"]["job_name"] = label

@StatementRegistry.register("SET")
class SetParser(BaseStatementParser):
    def parse(self, label: str, params: str, context: dict):
        pairs = re.findall(r'(\w+)=([^\s,]+)', params)
        for k, v in pairs:
            context["parser_instance"].symbols[k] = v

@StatementRegistry.register("EXEC")
class ExecParser(BaseStatementParser):
    def parse(self, label: str, params: str, context: dict):
        target_match = re.search(r'(?:PGM|PROC)=([\w\d]+)', params)
        exec_name = target_match.group(1) if target_match else params.split(',')[0]
        
        exec_params = {}
        param_pairs = re.findall(r'(\w+)=([^\s,]+)', params)
        for k, v in param_pairs:
            if k not in ['PGM', 'PROC']: exec_params[k] = v

        step_obj = {
            "step_name": label,
            "exec_target": exec_name,
            "parameters": exec_params,
            "is_pgm": "PGM=" in params or ("," not in params and "=" not in params),
            "dd_statements": [],
            "overrides": []
        }
        
        if context["in_proc_def"]:
            context["result"]["definitions"][context["current_proc_name"]]["steps"].append(step_obj)
        else:
            context["result"]["steps"].append(step_obj)
        context["current_step"] = step_obj

@StatementRegistry.register("DD")
class DdParser(BaseStatementParser):
    def parse(self, label: str, params: str, context: dict):
        dd_data = {"dd_name": label, "content": params}
        current_step = context.get("current_step")

        # Handle Concatenation
        if label == "" and current_step and current_step["dd_statements"]:
            last_dd = current_step["dd_statements"][-1]
            if "concatenations" not in last_dd: last_dd["concatenations"] = []
            last_dd["concatenations"].append(params)
        
        # Handle Overrides
        elif "." in label:
            label_parts = label.split('.', 1)
            step_ref, real_dd = label_parts[0], label_parts[1]
            dd_data["dd_name"] = real_dd
            for s in context["result"]["steps"]:
                if s["step_name"] == step_ref:
                    s["overrides"].append(dd_data)
        
        elif current_step:
            current_step["dd_statements"].append(dd_data)

class JCLParser(BaseParser):
    FILE_TYPE = "jcl"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037']

    def __init__(self, library_path: str = "."):
        self.library_path = library_path
        self.symbols = {}
        self._unrecognized = []

    def _detect_and_decode(self, raw_bytes: bytes) -> str:
        """Mainframe-style encoding detection."""
        for encoding in ['utf-8', 'ascii'] + self.EBCDIC_CODEPAGES:
            try:
                text = raw_bytes.decode(encoding)
                if "//" in text: return text
            except: continue
        return raw_bytes.decode('utf-8', errors='replace')

    def _resolve_symbols(self, text: str) -> str:
        for var, val in self.symbols.items():
            text = re.sub(f"&{var}\\.?", val, text)
        return text

    def _get_logical_lines(self, filepath: str):
        raw_bytes = Path(filepath).read_bytes()
        content = self._detect_and_decode(raw_bytes)
        lines = content.splitlines()

        logical_lines = []
        current_stmt = ""
        
        for line in lines:
            # Mainframe JCL columns: ignore anything past column 72
            line = line[:72]
            if line.startswith('//*') or not line.startswith('//'): continue
            
            content = line[2:].rstrip()
            is_concatenation = content.startswith(' ')
            content = " " + content.lstrip() if is_concatenation else content.lstrip()

            if current_stmt.endswith(','):
                current_stmt += content
            else:
                if current_stmt: logical_lines.append(current_stmt)
                current_stmt = content
        
        if current_stmt: logical_lines.append(current_stmt)
        return logical_lines

    def parse_file(self, filepath: str) -> dict:
        if not self.library_path:
            self.library_path = os.path.dirname(filepath)

        try:
            logger.info(f"Parsing: {filepath}")
            lines = self._get_logical_lines(filepath)
            
            context = {
                "result": {
                    "file_name": os.path.basename(filepath),
                    "type": "UNKNOWN",
                    "definitions": {}, 
                    "steps": [],
                    "includes": [],
                    "_unrecognized": []
                },
                "current_step": None,
                "in_proc_def": False,
                "current_proc_name": "",
                "parser_instance": self
            }

            for line in lines:
                line = self._resolve_symbols(line)
                is_concatenation = line.startswith(' ')
                parts = re.split(r'\s+', line.strip(), maxsplit=2)
                
                if not parts: continue
                
                # Identify Statement Components
                label = ""
                op = ""
                params = ""

                if is_concatenation:
                    op = "DD"
                    params = parts[1] if len(parts) > 1 else parts[0]
                elif len(parts) == 1:
                    op = parts[0]
                else:
                    label, op, params = (parts[0], parts[1], parts[2] if len(parts) > 2 else "")

                # Handle PROC/PEND State directly (affects logic flow)
                if op == 'PROC' and label != "":
                    context["result"]["type"] = "PROC"
                    context["in_proc_def"] = True
                    context["current_proc_name"] = label
                    context["result"]["definitions"][label] = {"steps": []}
                    continue
                elif op == 'PEND':
                    context["in_proc_def"] = False
                    continue

                # Execute Registered Parser
                stmt_parser_class = StatementRegistry.get_parser(op)
                if stmt_parser_class:
                    stmt_parser_class().parse(label, params, context)
                else:
                    self._unrecognized.append({"line": line, "reason": f"Unknown Op: {op}"})
            
            context["result"]["_unrecognized"] = self._unrecognized
            return context["result"]

        except Exception as e:
            logger.error(f"Failed to parse {filepath}: {e}")
            return {"file": filepath, "error": str(e)}

def main():
    parser = argparse.ArgumentParser(description='JCL Parser for Folder Locations')
    parser.add_argument('path', help='Path to JCL file or folder')
    parser.add_argument('-o', '--output', help='Output JSON file', default='output.json')
    args = parser.parse_args()

    input_path = Path(args.path)
    jcl_parser = JCLParser(library_path=str(input_path))
    results = []

    if input_path.is_file():
        results.append(jcl_parser.parse_file(str(input_path)))
    else:
        for file in input_path.rglob('*'):
            if file.is_file() and not file.name.startswith('.'):
                results.append(jcl_parser.parse_file(str(file)))

    with open(args.output, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"Parsing complete. Results saved to {args.output}")

if __name__ == '__main__':
    main()