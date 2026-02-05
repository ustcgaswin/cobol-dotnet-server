"""
CA-7 Parser - Simplified for Dependency Extraction Only

This parser extracts only job names and their dependencies (job, dataset, user requirements).
All unnecessary fields (owner, class, flags, raw content) have been removed.
"""

import re
import json
import sys
import logging
import argparse
from pathlib import Path
from dataclasses import dataclass, field
from typing import Optional, Dict, List

from app.core.parsers.base import BaseParser

logger = logging.getLogger('ca7_parser')


@dataclass
class CA7Job:
    """Simplified CA-7 job representation - dependencies only."""
    job_name: str
    system: Optional[str] = None
    schedule_id: Optional[str] = None
    requirements: Dict[str, List[str]] = field(default_factory=lambda: {
        "job_dependencies": [],
        "dataset_dependencies": [],
        "user_requirements": []
    })

    def to_dict(self) -> dict:
        """Convert to dictionary with only dependency-relevant fields."""
        return {
            "job_name": self.job_name,
            "header": {
                "system": self.system,
                "schedule_id": self.schedule_id
            },
            "requirements": self.requirements
        }


class CA7Parser(BaseParser):
    """Simplified CA-7 parser for dependency extraction."""
    
    FILE_TYPE = "ca7"

    # Only patterns needed for dependency extraction
    PATTERNS = {
        'dep_job': r"DEP-JOB:\s+([A-Z0-9#@$]+)",
        'dep_dsn': r"DSN:\s+([A-Z0-9#@$.]+)",
        'dep_usr': r"USR-REQ:\s+([A-Z0-9#@$]+)"
    }

    def __init__(self, strict: bool = False):
        self.strict = strict
        self.jobs: List[CA7Job] = []
        self._unrecognized: List[Dict] = []
        self._warnings: List[str] = []

    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        """Parse CA-7 report content and extract dependencies."""
        self.jobs = []
        self._unrecognized = []
        self._warnings = []
        
        # Split content into blocks by 'JOB:' 
        # Ensures we only split when 'JOB:' is at the start of a line
        # This prevents accidental splitting on 'DEP-JOB:'
        blocks = re.split(r'(?im)^(?:(?!\s*DEP-JOB:).)*?JOB:\s+', content)
        
        for block_text in blocks:
            if not block_text.strip():
                continue

            # Re-attach 'JOB: ' because re.split removes the delimiter
            full_block = "JOB: " + block_text

            try:
                job_obj = self._parse_block(full_block)
                if job_obj:
                    # Filter out wildcard summary lines like 'JOB: FIN*'
                    if '*' not in job_obj.job_name:
                        self.jobs.append(job_obj)
            except Exception as e:
                logger.warning(f"Skipping malformed block: {e}")
                self._unrecognized.append({
                    "error": str(e), 
                    "snippet": full_block[:100]
                })

        return {
            "meta": {
                "source_file": source_file,
                "file_type": self.FILE_TYPE,
                "job_count": len(self.jobs)
            },
            "jobs": [j.to_dict() for j in self.jobs],
            "_unrecognized": self._unrecognized,
            "_warnings": self._warnings
        }

    def _parse_block(self, block: str) -> Optional[CA7Job]:
        """Parse a single job block and extract dependencies."""
        # Extract the Job Name
        name_match = re.search(r"JOB:\s+([A-Z0-9#@$]+)", block, re.IGNORECASE)
        if not name_match:
            return None
        
        job_name = name_match.group(1)
        job = CA7Job(job_name=job_name)
        
        # Extract header fields needed for dependency context
        job.system = self._find(r"SYSTEM:\s+([A-Z0-9#@$]+)", block)
        job.schedule_id = self._find(r"SCHID:\s+(\d+)", block)

        # Extract all dependency types
        job.requirements["job_dependencies"] = sorted(list(set(
            re.findall(self.PATTERNS['dep_job'], block)
        )))
        job.requirements["dataset_dependencies"] = sorted(list(set(
            re.findall(self.PATTERNS['dep_dsn'], block)
        )))
        job.requirements["user_requirements"] = sorted(list(set(
            re.findall(self.PATTERNS['dep_usr'], block)
        )))

        return job

    def _find(self, pattern: str, text: str) -> Optional[str]:
        """Helper to find a single pattern match."""
        match = re.search(pattern, text, re.IGNORECASE)
        return match.group(1) if match else None

    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        """Detect encoding and decode bytes to string."""
        try:
            return raw_bytes.decode('utf-8'), 'utf-8'
        except:
            return raw_bytes.decode('cp1047', errors='replace'), 'ebcdic'

    def parse_file(self, filepath: str) -> dict:
        """Parse CA-7 file and extract dependencies."""
        path = Path(filepath)
        if not path.exists():
            return {
                "meta": {
                    "source_file": path.name,
                    "file_type": self.FILE_TYPE,
                    "job_count": 0
                },
                "jobs": [],
                "_unrecognized": [],
                "_warnings": ["File not found"]
            }
        
        raw_bytes = path.read_bytes()
        content, encoding = self._detect_and_decode(raw_bytes)
        return self.parse_string(content, source_file=path.name)


def main():
    """Simple CLI test for the parser."""
    parser = argparse.ArgumentParser(description='Simplified CA-7 Parser for Dependency Extraction')
    parser.add_argument('input', help='Input CA-7 report file path')
    parser.add_argument('-o', '--output', help='Output JSON file')
    parser.add_argument('--strict', action='store_true', help='Raise errors on failure')
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable debug logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    
    try:
        ca7_parser = CA7Parser(strict=args.strict)
        result = ca7_parser.parse_file(args.input)
        
        output_json = json.dumps(result, indent=2)
        
        if args.output:
            Path(args.output).write_text(output_json)
            print(f"Success: Results saved to {args.output}")
        else:
            print(output_json)
        
        return 0

    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    except Exception as e:
        print(f"Unexpected Error: {e}", file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())