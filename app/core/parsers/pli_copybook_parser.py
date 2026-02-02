"""
PL/I Copybook Parser - Simplified for Dependency Extraction Only

This parser extracts only %INCLUDE statements for dependency graph generation.
All structure parsing logic has been removed as it's not needed for dependencies.
"""

import re
import logging
import json
from pathlib import Path
from typing import Dict, List

# --- Framework Imports ---
from app.core.parsers.base import BaseParser
from app.db.enums import SourceFileType

logger = logging.getLogger(__name__)


class PLICopybookParser(BaseParser):
    """
    Simplified parser for PL/I Copybooks.
    Extracts only %INCLUDE statements for dependency tracking.
    """
    
    def parse(self, content: str, source_name: str) -> dict:
        """Parse PL/I copybook content and extract includes."""
        return self._parse_string_logic(content, source_name)

    def parse_file(self, filepath: str) -> dict:
        """Parse PL/I copybook file and extract includes."""
        path = Path(filepath)
        if not path.exists():
            return {
                "meta": {"source_file": path.name, "file_type": SourceFileType.PLI_COPYBOOK.value},
                "dependencies": {"includes": []},
                "_unrecognized": [],
                "_warnings": ["File not found"]
            }
        
        content = path.read_text(encoding='utf-8', errors='replace')
        return self._parse_string_logic(content, path.name)

    def _parse_string_logic(self, content: str, source_name: str) -> dict:
        """Main parsing logic - extracts only INCLUDE statements."""
        self.original_content = content
        self._warnings = []
        self._unrecognized = []
        
        # Clean comments (preserve whitespace for line counts)
        clean = re.sub(r'/\*.*?\*/', lambda m: ' ' * len(m.group(0)), content, flags=re.DOTALL)
        
        # Extract includes
        includes = self._extract_includes(clean)
        
        return {
            "meta": {
                "source_file": source_name,
                "file_type": SourceFileType.PLI_COPYBOOK.value,
            },
            "dependencies": {
                "includes": includes
            },
            "_unrecognized": self._unrecognized,
            "_warnings": self._warnings
        }

    def _extract_includes(self, content: str) -> List[Dict]:
        """Extract %INCLUDE statements for dependency tracking."""
        includes = []
        
        # Pattern matches:
        # %INCLUDE NAME
        # %INCLUDE LIBRARY(NAME)
        # ++INCLUDE NAME (alternative syntax)
        # With optional REPLACING clause
        pattern = r'(?:%|\+\+)\s*INCLUDE\s+(?:(\w+)\s*\()?(\w+)\)?(?:\s+REPLACING\s*\([^)]+\))?'
        
        matches = re.finditer(pattern, content, re.IGNORECASE)
        
        for m in matches:
            line = self._get_line_number(m.start())
            library = m.group(1)  # Optional library name
            name = m.group(2) if m.group(1) else m.group(2)  # Copybook name
            
            # If library exists, the actual copybook is in group(2)
            # Otherwise, copybook name is in group(2)
            if library:
                # Format: %INCLUDE LIBRARY(NAME)
                copybook_name = m.group(2)
            else:
                # Format: %INCLUDE NAME
                copybook_name = name
            
            includes.append({
                "name": copybook_name.upper(),
                "type": "NESTED_INCLUDE",
                "line": line
            })
        
        return includes

    def _get_line_number(self, index: int) -> int:
        """Get line number for a given character index in the original content."""
        return self.original_content.count('\n', 0, index) + 1


def main():
    """Simple CLI test for the parser."""
    import argparse
    import sys
    
    parser = argparse.ArgumentParser(description='Simplified PL/I Copybook Parser')
    parser.add_argument('input', help='PL/I Copybook file')
    parser.add_argument('-o', '--output', help='Output JSON file')
    args = parser.parse_args()
    
    if Path(args.input).exists():
        result = PLICopybookParser().parse_file(args.input)
        json_output = json.dumps(result, indent=2)
        
        if args.output:
            Path(args.output).write_text(json_output)
            print(f"Output written to {args.output}")
        else:
            print(json_output)
    else:
        print(f"File not found: {args.input}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()