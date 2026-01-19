"""
PL/1 Copybook Parser for Dependency Graph Generation (v4.1 - Deterministic)

EXPECTATIONS FROM THIS PARSER:
------------------------------
1. INPUT: Raw PL/I Copybook content (often just DCL statements or structure members).
2. OUTPUT: JSON structure with 'meta', 'structure' (variables), and 'dependencies'.

DEPENDENCY GRAPH MAPPING LOGIC:
------------------------------
| Source Node (Copybook) | Relationship | Target Node (Copybook) | Extracted Via Field             |
|------------------------|--------------|------------------------|---------------------------------|
| Copybook A             | INCLUDES     | Copybook B             | dependencies.includes (Preproc) |

USAGE:
    Used by ParserService via SourceFileType.PLI_COPYBOOK
"""

import re
import logging
import json
from dataclasses import dataclass, field
from typing import List, Optional, Dict
from pathlib import Path

# --- Framework Imports ---
from app.core.parsers.base import BaseParser
from app.db.enums import SourceFileType

logger = logging.getLogger(__name__)

# =============================================================================
# DATA STRUCTURES
# =============================================================================

@dataclass
class PLIField:
    level: int
    name: str
    attributes_raw: str = ""
    data_type: str = "UNKNOWN"
    length: int = 0
    scale: int = 0
    dimension: Optional[str] = None
    is_array: bool = False
    is_varying: bool = False
    initial_value: Optional[str] = None
    storage_class: Optional[str] = None
    is_ai_inferred: bool = False # Always False in deterministic mode
    children: List['PLIField'] = field(default_factory=list)
    line_number: Optional[int] = None
    length_bytes: int = 0 

    def to_dict(self):
        d = {"level": self.level, "name": self.name}
        if self.line_number: d["line"] = self.line_number
        if self.data_type != "UNKNOWN": d["data_type"] = self.data_type
        if self.length: d["length"] = self.length
        if self.scale: d["scale"] = self.scale
        if self.dimension: d["dimension"] = self.dimension
        if self.is_array: d["is_array"] = True
        if self.is_varying: d["is_varying"] = True
        if self.initial_value: d["initial_value"] = self.initial_value
        if self.storage_class: d["storage_class"] = self.storage_class
        if self.is_ai_inferred: d["ai_inferred"] = True
        if self.length_bytes: d["length_bytes"] = self.length_bytes
        if self.children:
            d["children"] = [child.to_dict() for child in self.children]
        return d

# =============================================================================
# ATTRIBUTE PARSER (Deterministic)
# =============================================================================

class PLIAttributeParser:
    TYPES = {
        'CHAR', 'CHARACTER', 'BIT', 'BIN', 'BINARY', 'FIXED', 
        'DEC', 'DECIMAL', 'PTR', 'POINTER', 'FLOAT', 'GRAPHIC', 'PICTURE', 'PIC'
    }
    KEYWORDS = {
        'DIM', 'DIMENSION', 'INIT', 'INITIAL', 'VAR', 'VARYING', 
        'STATIC', 'BASED', 'DEFINED', 'DEF', 'AUTO', 'AUTOMATIC',
        'ALIGNED', 'UNALIGNED'
    }
    
    def __init__(self, raw_attributes: str):
        self.tokens = re.findall(r"\'[^\']*\'|\"[^\"]*\"|[\w\$\@\#]+|[(),]", raw_attributes)
        self.parsed_data = {
            'data_type': None, 'length': None, 'scale': None,
            'dimension': None, 'initial': None, 'storage_class': None,
            'is_varying': False
        }

    def parse(self) -> dict:
        i = 0
        n = len(self.tokens)
        while i < n:
            token = self.tokens[i].upper()
            if token in [',', '(', ')']: 
                i += 1; continue

            if token in self.TYPES:
                norm_token = {'CHARACTER':'CHAR', 'BINARY':'BIN', 'DECIMAL':'DEC', 'POINTER':'PTR'}.get(token, token)
                curr = self.parsed_data['data_type']
                self.parsed_data['data_type'] = f"{curr} {norm_token}" if curr else norm_token
                
                if i + 1 < n and self.tokens[i+1] == '(':
                    params, i = self._consume_parens(i + 1)
                    if norm_token in ('DEC', 'FIXED', 'FLOAT', 'BIN'):
                        if len(params) >= 1: self.parsed_data['length'] = params[0]
                        if len(params) >= 2: self.parsed_data['scale'] = params[1]
                    elif params:
                        self.parsed_data['length'] = params[0]
                else:
                    i += 1
                continue
            
            if token in self.KEYWORDS:
                if token in ('VAR', 'VARYING'): self.parsed_data['is_varying'] = True
                elif token in ('DIM', 'DIMENSION'):
                     if i+1 < n and self.tokens[i+1] == '(':
                         params, i = self._consume_parens(i+1)
                         self.parsed_data['dimension'] = ",".join(params)
                     else: i += 1
                elif token in ('INIT', 'INITIAL'):
                     if i+1 < n and self.tokens[i+1] == '(':
                         params, i = self._consume_parens(i+1)
                         self.parsed_data['initial'] = ",".join(params)
                     else: i += 1
                elif token in ('STATIC', 'BASED', 'DEFINED', 'DEF', 'AUTO', 'AUTOMATIC'):
                    self.parsed_data['storage_class'] = token
                    i += 1
                else:
                    i += 1
                continue
            i += 1
        return self.parsed_data

    def _consume_parens(self, start_index: int):
        i = start_index + 1
        depth = 1
        content = []
        while i < len(self.tokens) and depth > 0:
            t = self.tokens[i]
            if t == '(': depth += 1
            elif t == ')': depth -= 1
            if depth > 0: content.append(t)
            i += 1
        return [p.strip() for p in "".join(content).split(',') if p.strip()], i

# =============================================================================
# MAIN PARSER LOGIC
# =============================================================================

class PLICopybookParser(BaseParser):
    """
    Parses PL/I structure definitions (Copybooks).
    Handles DCL blocks, implicit structures, and nested Includes.
    """
    
    def parse(self, content: str, source_name: str) -> dict:
        return self._parse_string_logic(content, source_name)

    def parse_file(self, filepath: str) -> dict:
        path = Path(filepath)
        if not path.exists(): return {"error": "File not found"}
        content = path.read_text(encoding='utf-8', errors='replace')
        return self._parse_string_logic(content, path.name)

    def _parse_string_logic(self, content: str, source_name: str) -> dict:
        self.root_fields = []
        self._current_level_stack = []
        self.parse_errors = []
        self.original_content = content
        self.dependencies = {"includes": []}

        # 1. Clean Comments (Preserve whitespace for line counts)
        clean = re.sub(r'/\*.*?\*/', lambda m: ' ' * len(m.group(0)), content, flags=re.DOTALL)
        
        # 2. Extract Nested Includes (Dependency Graph)
        self._extract_includes(clean)

        # 3. Find DCL blocks
        dcl_iterator = re.finditer(r'\b(?:DCL|DECLARE)\s+([\s\S]*?);', clean, re.IGNORECASE)
        found = False
        
        for match in dcl_iterator:
            found = True
            body_start_index = match.start(1) 
            body = match.group(1)
            base_line = self._get_line_number(body_start_index)
            self._process_dcl_body(body, base_line)

        # 4. Fallback for implicit structures (Common in copybooks: "1 MEMBER, 3 SUB ...")
        if not found:
             stripped = clean.strip()
             if re.match(r'^\d+', stripped):
                 self._process_dcl_body(stripped, 1)

        return {
            "meta": {
                "source_file": source_name,
                "file_type": SourceFileType.PLI_COPYBOOK.value,
            },
            "structure": {
                "fields": [f.to_dict() for f in self.root_fields]
            },
            "dependencies": self.dependencies,
            "_unrecognized": self.parse_errors,
            "_warnings": []
        }

    def _extract_includes(self, content: str):
        """Finds %INCLUDE statements inside this copybook."""
        matches = re.finditer(r'(?:%|\+\+)\s*INCLUDE\s+([\w\$\@\#]+)', content, re.IGNORECASE)
        for m in matches:
            line = self._get_line_number(m.start())
            self.dependencies["includes"].append({
                "name": m.group(1).upper(),
                "type": "NESTED_INCLUDE",
                "line": line
            })

    def _get_line_number(self, index: int) -> int:
        return self.original_content.count('\n', 0, index) + 1

    def _process_dcl_body(self, body: str, base_line: int):
        curr = []
        depth = 0
        current_field_start_line = base_line
        newlines_encountered = 0
        
        for char in body:
            if char == '\n': newlines_encountered += 1
            if char == '(': depth += 1
            elif char == ')': depth -= 1
            
            if char == ',' and depth == 0:
                token_str = "".join(curr).strip()
                if token_str:
                    clean_token = " ".join(token_str.split())
                    self._parse_node(clean_token, current_field_start_line)
                
                curr = []
                current_field_start_line = base_line + newlines_encountered
            else:
                curr.append(char)
        
        if curr:
            token_str = "".join(curr).strip()
            if token_str:
                self._parse_node(" ".join(token_str.split()), current_field_start_line)

    def _parse_node(self, token: str, line_num: int):
        # Matches: "1 NAME ATTRS" or "NAME ATTRS" or "1 NAME"
        match = re.match(r'^(\d+)?\s*([\w\$\#\@\-]+|\([\w\s,]+\))\s*(.*)', token)
        if not match: return

        level = int(match.group(1)) if match.group(1) else 1
        name_part = match.group(2)
        attrs = match.group(3)

        # Handle Factorization: DCL (A, B) CHAR(10);
        names = [n.strip() for n in name_part.strip('()').split(',')] if name_part.startswith('(') else [name_part]

        for name in names:
            field_obj = PLIField(level=level, name=name.upper(), line_number=line_num)
            try:
                self._analyze_attributes_standard(field_obj, attrs)
            except Exception as e:
                self._handle_fallback(field_obj, attrs, str(e), line_num)
            
            self._add_to_hierarchy(field_obj)

    def _analyze_attributes_standard(self, field_obj: PLIField, attrs: str):
        field_obj.attributes_raw = attrs
        parsed = PLIAttributeParser(attrs).parse()
        
        if parsed['length']: field_obj.length = int(parsed['length'])
        if parsed['scale']: field_obj.scale = int(parsed['scale'])
        field_obj.data_type = parsed['data_type'] if parsed['data_type'] else "UNKNOWN"
        
        # Implicit Typing Rule (I-N)
        if field_obj.data_type == "UNKNOWN" and field_obj.level > 1 and not parsed['dimension']:
            if 'I' <= field_obj.name[0] <= 'N':
                field_obj.data_type = "FIXED BIN"
                field_obj.length = 15

        # Byte Size Estimation (For Migration sizing)
        if 'FIXED' in field_obj.data_type and 'BIN' in field_obj.data_type:
             if field_obj.length <= 15: field_obj.length_bytes = 2
             elif field_obj.length <= 31: field_obj.length_bytes = 4
             else: field_obj.length_bytes = 8
        elif 'PTR' in field_obj.data_type or 'POINTER' in field_obj.data_type:
            field_obj.length_bytes = 4
        elif 'DEC' in field_obj.data_type:
             if field_obj.length: 
                 field_obj.length_bytes = (field_obj.length + 1) // 2
        
        field_obj.is_varying = parsed['is_varying']
        field_obj.initial_value = parsed['initial']
        if parsed['dimension']:
            field_obj.dimension = parsed['dimension']
            field_obj.is_array = True

    def _handle_fallback(self, field_obj: PLIField, attrs: str, error_msg: str, line_num: int):
        # AI Logic Removed: Just record the error for external handling
        self._record_error(field_obj.name, attrs, f"Standard parsing failed: {error_msg}", line_num)

    def _add_to_hierarchy(self, new_field: PLIField):
        if new_field.level == 1:
            self.root_fields.append(new_field)
            self._current_level_stack = [new_field]
            return
        while self._current_level_stack:
            if self._current_level_stack[-1].level < new_field.level:
                self._current_level_stack[-1].children.append(new_field)
                self._current_level_stack.append(new_field)
                return
            self._current_level_stack.pop()
        self.root_fields.append(new_field)
        self._current_level_stack = [new_field]

    def _record_error(self, name: str, content: str, error_msg: str, line: int):
        self.parse_errors.append({
            "line": line,
            "content": f"Field: {name} | {content[:50]}",
            "error": error_msg
        })

if __name__ == '__main__':
    # Simple CLI test
    import argparse
    import sys
    import os
    parser = argparse.ArgumentParser()
    parser.add_argument('input', help='PL/1 Copybook file')
    parser.add_argument('-o', '--output', help='Output JSON file')
    args = parser.parse_args()
    
    if os.path.exists(args.input):
        res = PLICopybookParser().parse_file(args.input)
        json_output = json.dumps(res, indent=2)
        if args.output:
            Path(args.output).write_text(json_output)
        else:
            print(json_output)
    else:
        print(f"File not found: {args.input}", file=sys.stderr)