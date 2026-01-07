import re
import json
import sys
import logging
import argparse
from pathlib import Path
from dataclasses import dataclass, field
from typing import Optional, Dict, List, Any

from app.core.parsers.base import BaseParser
from app.core.exceptions import (
    BindParseError,
    InvalidSyntaxError,
)

# =============================================================================
# Logging and Exit Codes
# =============================================================================

logging.basicConfig(
    level=logging.WARNING,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger('bind_parser')

class ExitCode:
    SUCCESS = 0
    FILE_NOT_FOUND = 1
    PARSE_ERROR = 4
    INVALID_INPUT = 5
    OUTPUT_ERROR = 6

# =============================================================================
# Models
# =============================================================================

@dataclass
class BindObject:
    """Represents a single BIND PACKAGE or BIND PLAN command"""
    object_type: str  # PACKAGE or PLAN
    object_name: str
    owner: Optional[str] = None
    qualifier: Optional[str] = None
    member: Optional[str] = None
    action: str = "REPLACE"
    isolation: str = "CS"
    release: str = "COMMIT"
    explain: str = "YES"
    pklist: List[str] = field(default_factory=list)
    additional_params: Dict[str, str] = field(default_factory=dict)
    raw_content: str = ""

    def to_dict(self) -> dict:
        return {
            "metadata": {
                "object_type": self.object_type,
                "object_name": self.object_name
            },
            "config": {
                "owner": self.owner,
                "qualifier": self.qualifier,
                "member_dbrm": self.member,
                "action": self.action,
                "isolation": self.isolation,
                "release": self.release,
                "explain": self.explain,
                "pklist": self.pklist
            },
            "additional_params": self.additional_params,
            "raw_snippet": self.raw_content
        }

# =============================================================================
# Parser Logic
# =============================================================================

class BindParser(BaseParser):
    FILE_TYPE = "bind"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500']

    # Regex to extract any KEYWORD(VALUE) pair
    PARAM_RE = r"(\w+)\s*\(\s*([^)]+?)\s*\)"

    def __init__(self, strict: bool = False):
        self.strict = strict
        self.objects: List[BindObject] = []
        self._unrecognized: List[Dict] = []

    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        self.objects = []
        self._unrecognized = []

        # 1. Pre-process: Handle Mainframe Continuation dashes ('-')
        # This joins lines that end with a dash so the regex sees one continuous command
        clean_content = self._preprocess_continuations(content)

        # 2. Split into blocks by BIND command
        blocks = re.split(r'(?im)(?=BIND\s+(?:PACKAGE|PLAN))', clean_content)

        for block_text in blocks:
            if not block_text.strip() or "BIND " not in block_text.upper():
                continue

            try:
                bind_obj = self._parse_block(block_text)
                if bind_obj:
                    self.objects.append(bind_obj)
            except Exception as e:
                if self.strict:
                    raise BindParseError(f"BIND block parse failed: {str(e)}", content=block_text[:200])
                logger.warning(f"Skipping malformed BIND block: {e}")
                self._unrecognized.append({"error": str(e), "snippet": block_text[:100]})

        return {
            "metadata": {"source": source_file, "object_count": len(self.objects)},
            "bind_commands": [obj.to_dict() for obj in self.objects],
            "parsing_issues": self._unrecognized
        }

    def _preprocess_continuations(self, text: str) -> str:
        """Removes the '-' continuation character at the end of lines and joins them."""
        # Remove JCL inline comments (--) if any exist
        lines = [line.split('--')[0].rstrip() for line in text.splitlines()]
        
        processed_text = ""
        for line in lines:
            if line.endswith('-'):
                processed_text += line[:-1] + " "  # Remove '-' and add space
            else:
                processed_text += line + "\n"
        return processed_text

    def _parse_block(self, block: str) -> Optional[BindObject]:
        # Identify Object Type and Name
        header_match = re.search(r"BIND\s+(PACKAGE|PLAN)\s*\(([^)]+)\)", block, re.IGNORECASE)
        if not header_match:
            return None

        obj_type = header_match.group(1).upper()
        obj_name = header_match.group(2).strip()
        
        bind_obj = BindObject(
            object_type=obj_type,
            object_name=obj_name,
            raw_content=block.strip()
        )

        # Extract all KEYWORD(VALUE) pairs into a temporary dict
        params = {}
        for key, val in re.findall(self.PARAM_RE, block):
            params[key.upper()] = val.strip()

        # Map known BIND keywords
        bind_obj.owner = params.pop("OWNER", None)
        bind_obj.qualifier = params.pop("QUALIFIER", None)
        bind_obj.member = params.pop("MEMBER", None)
        bind_obj.action = params.pop("ACTION", "REPLACE")
        bind_obj.isolation = params.pop("ISOLATION", "CS")
        bind_obj.release = params.pop("RELEASE", "COMMIT")
        bind_obj.explain = params.pop("EXPLAIN", "YES")

        # Special handling for PKLIST (comma separated list)
        if "PKLIST" in params:
            pk_val = params.pop("PKLIST")
            bind_obj.pklist = [item.strip() for item in pk_val.split(',')]

        # Anything remaining goes into additional_params (Robustness)
        bind_obj.additional_params = params

        return bind_obj

    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        for enc in ['utf-8', 'ascii'] + self.EBCDIC_CODEPAGES:
            try:
                text = raw_bytes.decode(enc)
                if "BIND " in text.upper(): return text, enc
            except: continue
        return raw_bytes.decode('utf-8', errors='replace'), 'fallback'

    def parse_file(self, filepath: str) -> dict:
        path = Path(filepath)
        if not path.exists():
            raise FileNotFoundError(f"BIND file not found: {filepath}")
        
        raw_bytes = path.read_bytes()
        content, encoding = self._detect_and_decode(raw_bytes)
        return self.parse_string(content, source_file=path.name)

# =============================================================================
# Main Execution Block
# =============================================================================

def main():
    parser = argparse.ArgumentParser(description='Parse DB2 BIND Control Cards to JSON')
    parser.add_argument('input', help='Input BIND file path')
    parser.add_argument('-o', '--output', help='Output JSON file')
    parser.add_argument('--strict', action='store_true', help='Raise errors on failure')
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable debug logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    
    try:
        bind_parser = BindParser(strict=args.strict)
        result = bind_parser.parse_file(args.input)
        
        output_json = json.dumps(result, indent=2)
        
        if args.output:
            Path(args.output).write_text(output_json)
            print(f"Success: Results saved to {args.output}")
        else:
            print(output_json)
        
        return ExitCode.SUCCESS

    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        return ExitCode.FILE_NOT_FOUND
    except BindParseError as e:
        print(f"BIND Parse Error: {e.message}", file=sys.stderr)
        return ExitCode.PARSE_ERROR
    except Exception as e:
        print(f"Unexpected Error: {e}", file=sys.stderr)
        return ExitCode.PARSE_ERROR

if __name__ == '__main__':
    sys.exit(main())