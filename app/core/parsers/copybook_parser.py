"""
COBOL Copybook Parser - Simplified for Dependency Extraction Only

This parser extracts only COPY statements for dependency graph generation.
All field parsing, hierarchy building, and position calculation removed.
"""

import re
import json
import sys
import logging
import argparse
from pathlib import Path
from typing import Optional

from app.core.parsers.base import BaseParser

logging.basicConfig(
    level=logging.WARNING,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger('copybook_parser')


class CopybookParser(BaseParser):
    """
    Simplified parser for COBOL copybooks.
    Extracts only COPY statements for dependency tracking.
    """
    
    FILE_TYPE = "copybook"
    
    # Common EBCDIC codepages for IBM mainframes
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    
    # File size limit
    MAX_FILE_SIZE = 10 * 1024 * 1024  # 10 MB
    
    def __init__(self, strict: bool = False):
        """Initialize parser."""
        self.strict = strict
        self._unrecognized: list[dict] = []
        self._warnings: list[str] = []
        self._copy_references: list[dict] = []
        
    def parse_file(self, filepath: str) -> dict:
        """Parse a copybook file and return dependencies."""
        logger.info(f"Starting to parse file: {filepath}")
        
        path = Path(filepath)
        
        # Input validation
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            return {
                "meta": {
                    "copybook_name": path.stem.upper(),
                    "source_file": path.name,
                    "file_type": self.FILE_TYPE
                },
                "copy_references": [],
                "_unrecognized": [],
                "_warnings": ["File not found"]
            }
        
        if not path.is_file():
            logger.error(f"Path is not a file: {filepath}")
            raise ValueError(f"Path is not a file: {filepath}")
        
        file_size = path.stat().st_size
        
        if file_size == 0:
            logger.warning(f"Empty file: {filepath}")
            return {
                "meta": {
                    "copybook_name": path.stem.upper(),
                    "source_file": path.name,
                    "file_type": self.FILE_TYPE
                },
                "copy_references": [],
                "_unrecognized": [],
                "_warnings": ["File is empty"]
            }
        
        if file_size > self.MAX_FILE_SIZE:
            raise ValueError(
                f"File exceeds maximum size of {self.MAX_FILE_SIZE // 1024 // 1024} MB"
            )
        
        # Read file as bytes first to detect encoding
        raw_bytes = path.read_bytes()
        
        # Detect and convert encoding
        content, detected_encoding = self._detect_and_decode(raw_bytes)
        logger.info(f"Detected encoding: {detected_encoding}")
        
        result = self.parse_string(content, source_file=str(path.name))
        
        logger.info(f"Successfully parsed {len(self._copy_references)} COPY references from {filepath}")
        return result
    
    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        """Detect file encoding and decode to string."""
        # Try UTF-8 first
        try:
            text = raw_bytes.decode('utf-8')
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
        logger.warning("Could not detect encoding, falling back to UTF-8 with replacement")
        return raw_bytes.decode('utf-8', errors='replace'), 'utf-8-fallback'
    
    def _looks_like_cobol(self, text: str) -> bool:
        """Heuristic to check if decoded text looks like valid COBOL."""
        # Check for unprintable characters
        unprintable_count = sum(1 for c in text[:1000] if ord(c) < 32 and c not in '\n\r\t')
        if unprintable_count > 10:
            return False
        
        # Check for common COBOL keywords
        cobol_keywords = ['PIC', 'PICTURE', 'OCCURS', 'REDEFINES', 'VALUE', 
                          'COMP', 'BINARY', 'FILLER', 'COPY']
        text_upper = text.upper()
        keyword_count = sum(1 for kw in cobol_keywords if kw in text_upper)
        
        return keyword_count >= 2
    
    def parse_string(self, content: str, source_file: str = "unknown") -> dict:
        """Parse copybook content string and extract COPY statements."""
        self._copy_references = []
        self._unrecognized = []
        self._warnings = []
        
        # Preprocess: handle continuation, remove comments, join lines
        lines = self._preprocess(content)
        
        # Parse each statement looking for COPY
        statements = self._split_statements(lines)
        
        for stmt_info in statements:
            try:
                self._parse_statement(stmt_info)
            except Exception as e:
                error_msg = f"Error parsing line {stmt_info['line']}: {e}"
                logger.warning(error_msg)
                self._unrecognized.append({
                    'line': stmt_info['line'], 
                    'content': stmt_info['text'][:100],
                    'error': str(e)
                })
        
        # Construct output
        result = {
            "meta": {
                "copybook_name": Path(source_file).stem.upper(),
                "source_file": source_file,
                "file_type": self.FILE_TYPE
            },
            "copy_references": self._copy_references,
            "_unrecognized": self._unrecognized,
            "_warnings": self._warnings
        }
            
        return result
    
    def _preprocess(self, content: str) -> list[tuple]:
        """Preprocess COBOL source: handle columns, continuations, comments."""
        lines = content.split('\n')
        processed = []
        
        for i, line in enumerate(lines):
            # Handle fixed-format COBOL (columns 7-72)
            if len(line) >= 7:
                indicator = line[6] if len(line) > 6 else ' '
                
                # Skip comment lines
                if indicator in ('*', '/', 'D', 'd'):
                    continue
                    
                # Handle continuation
                if indicator == '-' and processed:
                    continuation = line[7:72] if len(line) >= 72 else line[7:]
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
                processed.append((i + 1, text, line))
                
        return processed
    
    def _split_statements(self, lines: list[tuple]) -> list[dict]:
        """Split preprocessed lines into COBOL statements (period-terminated)."""
        statements = []
        current_stmt = []
        start_line = 0
        
        for line_num, text, original in lines:
            if not current_stmt:
                start_line = line_num
                
            current_stmt.append(text)
            
            # Check if statement ends with period
            if self._has_terminating_period(text):
                full_text = ' '.join(current_stmt)
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
                    'text': full_text + '.',
                })
                
        return statements
    
    def _has_terminating_period(self, text: str) -> bool:
        """Check if text ends with a terminating period."""
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
                remaining = text[i+1:].strip()
                if not remaining or remaining[0].isupper() or remaining[0].isdigit():
                    return True
        
        stripped = text.rstrip()
        return stripped and stripped[-1] == '.'
    
    def _parse_statement(self, stmt_info: dict):
        """Parse a single COBOL statement looking for COPY."""
        text = stmt_info['text']
        line_num = stmt_info['line']
        
        # Check for COPY statement
        copy_match = re.match(
            r'COPY\s+([A-Za-z][A-Za-z0-9_-]*)(?:\s+(?:OF|IN)\s+([A-Za-z][A-Za-z0-9_-]*))?',
            text,
            re.IGNORECASE
        )
        
        if copy_match:
            copy_ref = {
                'copybook_name': copy_match.group(1).upper(),
                'line': line_num,
            }
            
            # Add library if present
            if copy_match.group(2):
                copy_ref['library'] = copy_match.group(2).upper()
            
            # Check for REPLACING clause
            replacing_match = re.search(r'REPLACING\s+(.+?)(?:\.|$)', text, re.IGNORECASE)
            if replacing_match:
                copy_ref['replacing'] = self._parse_replacing_clause(replacing_match.group(1))
            
            self._copy_references.append(copy_ref)
            logger.debug(f"Found COPY: {copy_ref}")
    
    def _parse_replacing_clause(self, replacing_text: str) -> str:
        """Parse REPLACING clause (simplified - just store the text)."""
        # For dependency tracking, we just need to know there's a REPLACING clause
        # The actual replacements don't affect dependencies
        return replacing_text.strip()


def main():
    """CLI interface for the parser."""
    parser = argparse.ArgumentParser(
        description='Ultra-Simplified COBOL Copybook Parser for Dependency Extraction',
        formatter_class=argparse.RawDescriptionHelpFormatter
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
                return 1
        else:
            print(output_json)
        
        return 0
            
    except FileNotFoundError as e:
        logger.error(str(e))
        print(f"Error: {e}", file=sys.stderr)
        return 1
    
    except Exception as e:
        logger.exception(f"Unexpected error: {e}")
        print(f"Error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == '__main__':
    exit(main())