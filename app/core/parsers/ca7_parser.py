import re
import json
import sys
import logging
import argparse
from pathlib import Path
from dataclasses import dataclass, field
from typing import Optional, Dict, List, Any

# Assuming these are available in your environment
from app.core.parsers.base import BaseParser
from app.core.exceptions import (
    CA7ParseError,
    InvalidSyntaxError,
)

logger = logging.getLogger('ca7_parser')

@dataclass
class CA7Job:
    job_name: str
    system: Optional[str] = None
    schedule_id: Optional[str] = None
    owner: Optional[str] = None
    execution_class: Optional[str] = None
    requirements: Dict[str, List[str]] = field(default_factory=lambda: {
        "job_dependencies": [],
        "dataset_dependencies": [],
        "user_requirements": [],
        "network_requirements": []
    })
    flags: Dict[str, bool] = field(default_factory=dict)
    raw_content: str = ""

    def to_dict(self) -> dict:
        return {
            "job_name": self.job_name,
            "header": {
                "system": self.system,
                "schedule_id": self.schedule_id,
                "owner": self.owner,
                "class": self.execution_class
            },
            "requirements": self.requirements,
            "status_flags": self.flags,
            "raw_snippet": self.raw_content
        }

class CA7Parser(BaseParser):
    FILE_TYPE = "ca7"

    PATTERNS = {
        'flags': r"([YN])\s+--\s+([^.\n\-]+)",
        'dep_job': r"DEP-JOB:\s+([A-Z0-9#@$]+)",
        'dep_dsn': r"DSN:\s+([A-Z0-9#@$.]+)",
        'dep_usr': r"USR-REQ:\s+([A-Z0-9#@$]+)"
    }

    def __init__(self, strict: bool = False):
        self.strict = strict
        self.jobs: List[CA7Job] = []
        self._unrecognized: List[Dict] = []

    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        self.jobs = []
        self._unrecognized = []
        
        # 1. Split content into blocks by 'JOB:' 
        # We use (?m)^\s* to ensure we only split when 'JOB:' is at the start of a line
        # This prevents accidental splitting on 'DEP-JOB:'
        blocks = re.split(r'(?im)^.*?JOB:\s+', content)
        
        for block_text in blocks:
            if not block_text.strip():
                continue

            # Re-attach 'JOB: ' to the front because re.split removes the delimiter
            full_block = "JOB: " + block_text

            try:
                job_obj = self._parse_block(full_block)
                if job_obj:
                    # Filter out wildcard summary lines like 'JOB: FIN*'
                    if '*' not in job_obj.job_name:
                        self.jobs.append(job_obj)
            except Exception as e:
                if self.strict:
                    raise CA7ParseError(f"Block parse failed: {str(e)}", content=full_block[:200])
                logger.warning(f"Skipping malformed block: {e}")
                self._unrecognized.append({"error": str(e), "snippet": full_block[:100]})

        return {
            "metadata": {"source": source_file, "job_count": len(self.jobs)},
            "jobs": [j.to_dict() for j in self.jobs],
            "parsing_issues": self._unrecognized
        }

    def _parse_block(self, block: str) -> Optional[CA7Job]:
        # Extract the Job Name
        name_match = re.search(r"JOB:\s+([A-Z0-9#@$]+)", block, re.IGNORECASE)
        if not name_match:
            return None
        
        job_name = name_match.group(1)
        job = CA7Job(job_name=job_name, raw_content=block.strip())
        
        # Header field extraction
        job.system = self._find(r"SYSTEM:\s+([A-Z0-9#@$]+)", block)
        job.schedule_id = self._find(r"SCHID:\s+(\d+)", block)
        job.owner = self._find(r"OWNER:\s+([A-Z0-9#@$]+)", block)
        job.execution_class = self._find(r"CLASS:\s+(\S+)", block)

        # Requirements - FIXED: Added re.findall back into the set calls
        job.requirements["job_dependencies"] = sorted(list(set(re.findall(self.PATTERNS['dep_job'], block))))
        job.requirements["dataset_dependencies"] = sorted(list(set(re.findall(self.PATTERNS['dep_dsn'], block))))
        job.requirements["user_requirements"] = sorted(list(set(re.findall(self.PATTERNS['dep_usr'], block))))

        # Flags (Matrix extraction)
        for val, desc in re.findall(self.PATTERNS['flags'], block):
            clean_desc = desc.split('  ')[0].strip()
            job.flags[clean_desc] = (val == 'Y')

        return job

    def _find(self, pattern: str, text: str) -> Optional[str]:
        match = re.search(pattern, text, re.IGNORECASE)
        return match.group(1) if match else None

    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        try:
            return raw_bytes.decode('utf-8'), 'utf-8'
        except:
            return raw_bytes.decode('cp1047', errors='replace'), 'ebcdic'

    def parse_file(self, filepath: str) -> dict:
        path = Path(filepath)
        raw_bytes = path.read_bytes()
        content, encoding = self._detect_and_decode(raw_bytes)
        return self.parse_string(content, source_file=path.name)

# =============================================================================
# Main Execution Block
# =============================================================================

def main():
    parser = argparse.ArgumentParser(description='Parse CA-7 LJOB Reports to JSON')
    parser.add_argument('input', help='Input CA-7 report file path')
    parser.add_argument('-o', '--output', help='Output JSON file')
    parser.add_argument('--strict', action='store_true', help='Raise errors on failure')
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable debug logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    
    try:
        ca7_parser = CA7Parser(strict=args.strict)
        result = ca7_parser.parse_file(args.input)
        
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
    except CA7ParseError as e:
        print(f"CA-7 Parse Error: {e.message}", file=sys.stderr)
        return ExitCode.PARSE_ERROR
    except Exception as e:
        print(f"Unexpected Error: {e}", file=sys.stderr)
        return ExitCode.PARSE_ERROR

if __name__ == '__main__':
    sys.exit(main())