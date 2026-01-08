"""
PL/1 Copybook Parser - Hybrid AI/Deterministic Engine (v3.3)

Updates:
- FIXED: Line number calculation.
  1. Removed destructive whitespace collapsing.
  2. Implemented per-field newline tracking inside DCL blocks.
- FIXED: LangChain Prompt Template escaping (JSON braces vs Prompt variables).
- RETAINED: Strict validation, I-N rules, and AI Correction logging.

Usage:
    python pli_copybook_parser.py <copybook> [-o output.json]
"""

import re
import json
import sys
import logging
import argparse
import os
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Optional, Tuple

# =============================================================================
# CONFIGURATION & AI SETUP
# =============================================================================

MAX_AI_RETRIES_PER_FILE = 15
ENABLE_AI = False
_llm_model = None

# Logger Setup
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger('pli_copybook_parser')

try:
    from app.config.llm_config import get_llm
    from langchain_core.prompts import ChatPromptTemplate
    from langchain_core.output_parsers import JsonOutputParser
    
    _llm_model = get_llm()
    ENABLE_AI = True
    logger.info("AI Model initialized successfully.")
    
except ImportError:
    logger.warning("Could not import 'get_llm'. Running in Deterministic Mode.")
except Exception as e:
    logger.error(f"Failed to initialize AI context: {e}")

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
    is_ai_inferred: bool = False
    children: List['PLIField'] = field(default_factory=list)
    line_number: Optional[int] = None
    _parent: Optional['PLIField'] = field(default=None, repr=False)

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
        if self.children:
            d["children"] = [child.to_dict() for child in self.children]
        return d

# =============================================================================
# DETERMINISTIC PARSER (STRICT MODE)
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
        self.raw = raw_attributes
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
            
            if not token.isdigit() and not token.startswith("'") and not token.startswith('"'):
                 raise ValueError(f"Unknown attribute token: {token}")

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
# MAIN CONTROLLER
# =============================================================================

class PLICopybookParser:
    def __init__(self):
        self.root_fields = []
        self._current_level_stack = []
        self.parse_errors = []
        self.ai_calls_made = 0 
        self.original_content = ""

    def parse_file(self, filepath: str) -> dict:
        content = Path(filepath).read_text(encoding='utf-8')
        return self.parse_string(content, Path(filepath).name)

    def parse_string(self, content: str, source_name: str) -> dict:
        self.root_fields = []
        self._current_level_stack = []
        self.parse_errors = []
        self.ai_calls_made = 0 
        self.original_content = content
        
        # 1. Clean Comments ONLY (Preserve newlines and whitespace length)
        # Using lambda to replace comment content with spaces to keep index alignment exact
        clean = re.sub(r'/\*.*?\*/', lambda m: ' ' * len(m.group(0)), content, flags=re.DOTALL)
        
        # 2. Find DCL blocks
        dcl_iterator = re.finditer(r'\b(?:DCL|DECLARE)\s+([\s\S]*?);', clean, re.IGNORECASE)
        found = False
        
        for match in dcl_iterator:
            found = True
            body_start_index = match.start(1) # Start of the capturing group (the body)
            body = match.group(1)
            
            # Calculate absolute starting line of the body
            base_line = self._get_line_number(body_start_index)
            self._process_dcl_body(body, base_line)

        # 3. Fallback for implicit structures
        if not found:
             stripped = clean.strip()
             if re.match(r'^\d+', stripped):
                 self._process_dcl_body(stripped, 1)

        return {
            "meta": {"file": source_name, "ai_calls": self.ai_calls_made},
            "fields": [f.to_dict() for f in self.root_fields],
            "_unrecognized": self.parse_errors,
            "_warnings": []
        }

    def _get_line_number(self, index: int) -> int:
        """Calculates line number from character index in original content."""
        return self.original_content.count('\n', 0, index) + 1

    def _process_dcl_body(self, body: str, base_line: int):
        """
        Splits by comma while tracking newline counts to assign accurate line numbers.
        """
        curr = []
        depth = 0
        
        # Track relative line offset inside the DCL body
        current_field_start_line = base_line
        newlines_encountered = 0
        
        for char in body:
            if char == '\n':
                newlines_encountered += 1
            
            if char == '(': depth += 1
            elif char == ')': depth -= 1
            
            if char == ',' and depth == 0:
                token_str = "".join(curr).strip()
                if token_str:
                    # Clean up token (remove newlines inside attributes for Regex parser)
                    clean_token = " ".join(token_str.split())
                    self._parse_node(clean_token, current_field_start_line)
                
                curr = []
                # The NEXT field starts roughly where we are now
                current_field_start_line = base_line + newlines_encountered
            else:
                curr.append(char)
        
        # Process last token
        if curr:
            token_str = "".join(curr).strip()
            clean_token = " ".join(token_str.split())
            self._parse_node(clean_token, current_field_start_line)

    def _parse_node(self, token: str, line_num: int):
        match = re.match(r'^(\d+)?\s*([\w\$\#\@\-]+|\([\w\s,]+\))\s*(.*)', token)
        if not match: return

        level = int(match.group(1)) if match.group(1) else 1
        name_part = match.group(2)
        attrs = match.group(3)

        names = [n.strip() for n in name_part.strip('()').split(',')] if name_part.startswith('(') else [name_part]

        for name in names:
            field_obj = PLIField(level=level, name=name.upper(), line_number=line_num)
            
            try:
                self._analyze_attributes_standard(field_obj, attrs)
            except Exception as e:
                self._handle_fallback(field_obj, attrs, str(e), line_num)
            
            self._add_to_hierarchy(field_obj)

    def _handle_fallback(self, field_obj: PLIField, attrs: str, error_msg: str, line_num: int):
        if self.ai_calls_made >= MAX_AI_RETRIES_PER_FILE:
            self._record_error(field_obj.name, attrs, f"Standard failed: {error_msg} (Circuit Breaker HIT)", line_num)
            return

        if not ENABLE_AI:
            self._record_error(field_obj.name, attrs, f"Standard failed: {error_msg} (AI Disabled)", line_num)
            return

        self.ai_calls_made += 1
        try:
            prompt_text = """
            You are a PL/1 Compiler Expert. The standard parser failed on this field.
            
            Variable: "{name}"
            Attributes: "{attrs}"
            Error: "{error}"
            
            Analyze the attributes. Return strictly valid JSON with this schema:
            {{
                "data_type": "string (normalized)",
                "length": 0,
                "scale": 0,
                "dimension": null,
                "is_varying": false,
                "initial_value": null,
                "storage_class": null
            }}
            If length/scale are invalid (e.g. 'XYZ'), infer a standard default.
            """
            
            parser = JsonOutputParser()
            prompt = ChatPromptTemplate.from_template(prompt_text)
            chain = prompt | _llm_model | parser
            
            result = chain.invoke({
                "name": field_obj.name,
                "attrs": attrs,
                "error": error_msg
            })
            
            field_obj.data_type = result.get('data_type', 'UNKNOWN')
            field_obj.length = result.get('length', 0)
            field_obj.scale = result.get('scale', 0)
            field_obj.dimension = result.get('dimension')
            if field_obj.dimension: field_obj.is_array = True
            field_obj.is_varying = result.get('is_varying', False)
            field_obj.initial_value = result.get('initial_value')
            field_obj.storage_class = result.get('storage_class')
            field_obj.is_ai_inferred = True
            field_obj.attributes_raw = attrs
            
            self._record_error(
                field_obj.name, 
                attrs, 
                f"[AI_CORRECTION] Regex failed ({error_msg}).",
                line_num
            )
            
        except Exception as e:
            self._record_error(field_obj.name, attrs, f"Fatal: Standard & AI failed. {str(e)}", line_num)

    def _analyze_attributes_standard(self, field_obj: PLIField, attrs: str):
        field_obj.attributes_raw = attrs
        parsed = PLIAttributeParser(attrs).parse()
        
        if parsed['length']:
            if not str(parsed['length']).isdigit():
                raise ValueError(f"Invalid length value: {parsed['length']}")
            field_obj.length = int(parsed['length'])
            
        if parsed['scale']:
            if not str(parsed['scale']).isdigit():
                raise ValueError(f"Invalid scale value: {parsed['scale']}")
            field_obj.scale = int(parsed['scale'])

        field_obj.data_type = parsed['data_type'] if parsed['data_type'] else "UNKNOWN"

        # IMPLICIT TYPING (I-N Rule)
        if field_obj.data_type == "UNKNOWN" and field_obj.level > 1 and not parsed['dimension']:
            first_char = field_obj.name[0]
            if 'I' <= first_char <= 'N':
                field_obj.data_type = "FIXED BIN"
                field_obj.length = 15

        # BYTE ALIGNMENT
        if 'FIXED' in field_obj.data_type and 'BIN' in field_obj.data_type:
             if field_obj.length <= 15: field_obj.length = 2
             elif field_obj.length <= 31: field_obj.length = 4
             else: field_obj.length = 8
        elif 'PTR' in field_obj.data_type or 'POINTER' in field_obj.data_type:
            field_obj.length = 4
        elif 'DEC' in field_obj.data_type:
             if field_obj.length: 
                 field_obj.length = (field_obj.length + 1) // 2

        field_obj.is_varying = parsed['is_varying']
        field_obj.initial_value = parsed['initial']
        if parsed['dimension']:
            field_obj.dimension = parsed['dimension']
            field_obj.is_array = True

    def _record_error(self, name: str, content: str, error_msg: str, line: int):
        self.parse_errors.append({
            "line": line,
            "content": f"Field: {name} | Attrs: {content[:100]}",
            "error": error_msg
        })

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

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('input', help='PL/1 Copybook file')
    parser.add_argument('-o', '--output', help='Output JSON file')
    args = parser.parse_args()
    
    if os.path.exists(args.input):
        res = PLICopybookParser().parse_file(args.input)
        print(json.dumps(res, indent=2))
    else:
        print(f"File not found: {args.input}", file=sys.stderr)