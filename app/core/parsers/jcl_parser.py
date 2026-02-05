import os
import re
import json
import logging
import argparse
from pathlib import Path
from typing import Dict, List, Optional

from app.core.parsers.base import BaseParser

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('jcl_parser')

class ParseError(Exception):
    """Custom exception for parsing failures."""
    pass


class JCLParser(BaseParser):
    FILE_TYPE = "jcl"

    def __init__(self, library_path: str = "."):
        self.library_path = library_path
        self._unrecognized = []
        self._warnings = []

    def _get_logical_lines(self, filepath: str):
        """Read and process JCL file into logical lines."""
        try:
            content = Path(filepath).read_text(encoding='utf-8', errors='replace')
        except Exception as e:
            logger.error(f"Failed to read {filepath}: {e}")
            raise ParseError(f"Failed to read file: {e}")
        
        lines = content.splitlines()
        logical_lines = []
        current_stmt = ""
        
        for line in lines:
            # Strip sequence numbers (columns 73-80)
            line = line[:72]
            
            # Skip comments and non-JCL lines
            if line.startswith('//*') or not line.startswith('//'):
                continue
            
            content = line[2:].rstrip()
            is_continuation = content.startswith(' ')
            content = " " + content.lstrip() if is_continuation else content.lstrip()

            # Handle continuation lines
            if current_stmt.endswith(','):
                current_stmt += content
            else:
                if current_stmt:
                    logical_lines.append(current_stmt)
                current_stmt = content
        
        if current_stmt:
            logical_lines.append(current_stmt)
        
        return logical_lines

    def _parse_exec_statement(self, label: str, params: str, result: dict, current_step: dict):
        """Parse EXEC statement and extract dependency information."""
        # Extract Program or Procedure name
        target_match = re.search(r'(?:PGM|PROC)=([\w\d]+)', params, re.IGNORECASE)
        exec_name = target_match.group(1) if target_match else params.split(',')[0].strip()
        
        # Determine if it's a program or procedure
        is_pgm = "PGM=" in params.upper() or ("," not in params and "=" not in params)
        
        step_obj = {
            "step_name": label,
            "dependency": {
                "name": exec_name,
                "type": "PROGRAM" if is_pgm else "PROCEDURE"
            },
            "dd_statements": []
        }
        
        # Add to top-level dependencies list
        result["dependencies"].append(step_obj["dependency"])
        result["steps"].append(step_obj)
        
        return step_obj

    def _parse_dd_statement(self, label: str, params: str, current_step: Optional[dict]):
        """Parse DD statement and extract DSN and DISP information."""
        if not current_step:
            self._warnings.append(f"DD statement '{label}' found without preceding EXEC step")
            return
        
        dd_data = {"dd_name": label}
        
        # Extract DSN for data dependency tracking
        dsn_match = re.search(r'DSN=([A-Z0-9#@$.()+-]+)', params, re.IGNORECASE)
        if dsn_match:
            dd_data["dsn"] = dsn_match.group(1)
        
        # Extract DISP (disposition)
        disp_match = re.search(r'DISP=\(([^)]+)\)', params, re.IGNORECASE)
        if disp_match:
            dd_data["disp"] = disp_match.group(1).split(',')[0].upper()
        elif "DISP=" in params.upper():
            # Handle simple DISP=SHR
            simple_disp = re.search(r'DISP=([\w]+)', params, re.IGNORECASE)
            if simple_disp:
                dd_data["disp"] = simple_disp.group(1).upper()
        
        # Only add DD if it has DSN (for dependency tracking)
        if "dsn" in dd_data:
            current_step["dd_statements"].append(dd_data)
        elif label:  # Named DD without DSN
            current_step["dd_statements"].append(dd_data)

    def parse_file(self, filepath: str) -> dict:
        """Parse JCL file and extract dependency-relevant information."""
        if not self.library_path:
            self.library_path = os.path.dirname(filepath)

        try:
            logger.info(f"Parsing: {filepath}")
            lines = self._get_logical_lines(filepath)
            
            result = {
                "file_name": os.path.basename(filepath),
                "type": "UNKNOWN",
                "job_name": None,
                "steps": [],
                "dependencies": [],
                "_unrecognized": [],
                "_warnings": []
            }
            
            current_step = None
            keywords = ['JOB', 'EXEC', 'DD', 'PROC', 'PEND']

            for line in lines:
                # Parse line into label, operation, and parameters
                raw_parts = re.split(r'\s+', line.strip(), maxsplit=2)
                if not raw_parts or not raw_parts[0]:
                    continue

                label, op, params = "", "", ""
                
                if len(raw_parts) >= 2 and raw_parts[1].upper() in keywords:
                    label = raw_parts[0]
                    op = raw_parts[1].upper()
                    params = raw_parts[2] if len(raw_parts) > 2 else ""
                elif raw_parts[0].upper() in keywords:
                    op = raw_parts[0].upper()
                    params = raw_parts[1] if len(raw_parts) > 1 else ""
                else:
                    # Assume DD statement without label
                    op = "DD"
                    label = ""
                    params = line.strip()

                # Process statements
                if op == 'JOB':
                    result["type"] = "JCL"
                    result["job_name"] = label
                
                elif op == 'PROC':
                    if result["type"] != "JCL":
                        result["type"] = "PROC"
                    # Skip PROC definition tracking
                    continue
                
                elif op == 'PEND':
                    # Skip PROC end marker
                    continue
                
                elif op == 'EXEC':
                    current_step = self._parse_exec_statement(label, params, result, current_step)
                
                elif op == 'DD':
                    self._parse_dd_statement(label, params, current_step)
                
                else:
                    self._unrecognized.append({"line": line, "reason": f"Unknown operation: {op}"})

            result["_unrecognized"] = self._unrecognized
            result["_warnings"] = self._warnings
            
            return result

        except Exception as e:
            logger.error(f"Failed to parse {filepath}: {e}")
            return {
                "file_name": os.path.basename(filepath),
                "type": "ERROR",
                "error": str(e),
                "_unrecognized": self._unrecognized,
                "_warnings": self._warnings
            }


def main():
    parser = argparse.ArgumentParser(description='Simplified JCL Parser for Dependency Extraction')
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