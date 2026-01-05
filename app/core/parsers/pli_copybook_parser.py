"""
PL/1 Copybook Parser - Enterprise Grade

Features:
- Token-Based Attribute Parsing (No fragile Regex for nested parens)
- Handles Complex Factoring: DCL (A, B) CHAR(5); -> A CHAR(5), B CHAR(5)
- Hierarchy Reconstruction: Maps Level 1 -> 2 -> 3 correctly.
- Robust Attribute Parsing: Handles BIT, BIN FIXED(31), DEC(15,3), VARYING.
- Size Calculation: Approximates storage bytes for mainframe types.

Usage:
    python pli-copybook-parser.py <copybook> [-o output.json]
"""

import re
import json
import sys
import logging
import argparse
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Optional, Tuple

# Core Mock for consistency
class InvalidSyntaxError(Exception): pass

# Logger
logging.basicConfig(level=logging.WARNING, format='%(levelname)s: %(message)s')
logger = logging.getLogger('pli_cpy')

# =============================================================================
# Data Structures
# =============================================================================

@dataclass
class PLIField:
    """Represents a PL/I Variable or Structure Member"""
    level: int
    name: str
    attributes_raw: str = ""
    data_type: str = "UNKNOWN"
    length: int = 0
    scale: int = 0
    dimension: Optional[str] = None
    is_array: bool = False
    initial_value: Optional[str] = None
    storage_class: Optional[str] = None # STATIC, BASED, AUTOMATIC
    children: List['PLIField'] = field(default_factory=list)
    
    # Internal usage for tree building
    _parent: Optional['PLIField'] = None

    def to_dict(self):
        """
        Manually construct dictionary to avoid infinite recursion 
        caused by dataclasses.asdict() traversing the _parent link.
        """
        d = {
            "level": self.level,
            "name": self.name,
        }
        
        # Only add fields if they have values to keep JSON clean
        if self.attributes_raw: d["attributes_raw"] = self.attributes_raw
        if self.data_type != "UNKNOWN": d["data_type"] = self.data_type
        if self.length: d["length"] = self.length
        if self.scale: d["scale"] = self.scale
        if self.dimension: d["dimension"] = self.dimension
        if self.is_array: d["is_array"] = True
        if self.initial_value: d["initial_value"] = self.initial_value
        if self.storage_class: d["storage_class"] = self.storage_class

        if self.children:
            d["children"] = [child.to_dict() for child in self.children]
            
        return d

# =============================================================================
# Attribute Parser (The "Lexer")
# =============================================================================

class PLIAttributeParser:
    """
    Parses PL/I attribute strings using tokenization instead of Regex.
    Handles order-independence, nested parens, and complex types.
    """
    
    # PL/I Keywords
    TYPES = {
        'CHAR', 'CHARACTER', 'BIT', 'BIN', 'BINARY', 'FIXED', 
        'DEC', 'DECIMAL', 'PTR', 'POINTER', 'FLOAT', 'GRAPHIC', 'PICTURE', 'PIC'
    }
    
    KEYWORDS = {
        'DIM', 'DIMENSION', 'INIT', 'INITIAL', 'VARYING', 'VAR', 
        'ALIGNED', 'UNALIGNED', 'STATIC', 'BASED', 'DEFINED', 'DEF', 'AUTO', 'AUTOMATIC'
    }

    def __init__(self, raw_attributes: str):
        self.raw = raw_attributes
        self.tokens = self._tokenize(raw_attributes)
        self.parsed_data = {
            'data_type': None,
            'length': None,
            'scale': None,     # Decimal precision
            'dimension': None, # Array bounds
            'initial': None,
            'storage_class': None,
            'is_varying': False
        }

    def _tokenize(self, text: str) -> List[str]:
        """
        Splits string into tokens: words, numbers, parens, commas, quoted strings.
        Example: "CHAR(5) INIT('A')" -> ['CHAR', '(', '5', ')', 'INIT', '(', "'A'", ')']
        """
        # Regex explanation:
        # 1. Quoted strings (keeping quotes)
        # 2. Alphanumeric words (identifiers/numbers)
        # 3. Individual symbols: ( ) ,
        pattern = r"\'[^\']*\'|\"[^\"]*\"|[\w\$\@\#]+|[(),]"
        return re.findall(pattern, text)

    def parse(self) -> dict:
        i = 0
        n = len(self.tokens)
        
        while i < n:
            token = self.tokens[i].upper()
            
            # --- 1. Handle Data Types ---
            if token in self.TYPES:
                # Map synonmys
                if token == 'CHARACTER': token = 'CHAR'
                if token == 'BINARY': token = 'BIN'
                if token == 'DECIMAL': token = 'DEC'
                if token == 'POINTER': token = 'PTR'
                if token == 'PICTURE': token = 'PIC'
                
                # Combine types (e.g. FIXED BIN)
                current_type = self.parsed_data['data_type']
                if current_type:
                    # If we already have FIXED and now see BIN, make it FIXED BIN
                    self.parsed_data['data_type'] = f"{current_type} {token}"
                else:
                    self.parsed_data['data_type'] = token
                
                # Check for Length/Precision: CHAR(5) or DEC(10,2)
                # Must check if next token is '('
                if i + 1 < n and self.tokens[i+1] == '(':
                    params, new_index = self._consume_parens(i + 1)
                    i = new_index
                    
                    if token in ('CHAR', 'BIT', 'PIC', 'GRAPHIC'):
                        # Only length: (5)
                        if params: self.parsed_data['length'] = params[0]
                    elif token in ('DEC', 'FIXED', 'FLOAT', 'BIN'):
                        # Precision: (15, 3)
                        if len(params) >= 1: self.parsed_data['length'] = params[0] # Precision
                        if len(params) >= 2: self.parsed_data['scale'] = params[1]  # Scale
                else:
                    i += 1
                continue

            # --- 2. Handle Explicit Dimensions (DIM) ---
            if token in ('DIM', 'DIMENSION'):
                if i + 1 < n and self.tokens[i+1] == '(':
                    params, new_index = self._consume_parens(i + 1)
                    self.parsed_data['dimension'] = ",".join(params)
                    i = new_index
                else:
                    i += 1
                continue

            # --- 3. Handle Implicit Dimensions ---
            # If we see (...) and haven't seen a Type yet, it's an array definition
            # Example: DCL A (10) CHAR(5);
            # Limitation: DCL A (10); (Default type logic not handled here)
            if token == '(' and self.parsed_data['data_type'] is None:
                params, new_index = self._consume_parens(i)
                self.parsed_data['dimension'] = ",".join(params)
                i = new_index
                continue

            # --- 4. Handle Initialization ---
            if token in ('INIT', 'INITIAL'):
                if i + 1 < n and self.tokens[i+1] == '(':
                    params, new_index = self._consume_parens(i + 1)
                    self.parsed_data['initial'] = ",".join(params)
                    i = new_index
                else:
                    i += 1
                continue

            # --- 5. Other Keywords ---
            if token in ('VAR', 'VARYING'):
                self.parsed_data['is_varying'] = True
            elif token in ('STATIC', 'BASED', 'DEFINED', 'AUTO', 'AUTOMATIC'):
                self.parsed_data['storage_class'] = token
            elif token == 'ALIGNED' or token == 'UNALIGNED':
                 pass # Ignored for now
            else:
                # Unknown token, skip
                pass
            
            # Move to next token
            i += 1
            
        return self.parsed_data

    def _consume_parens(self, start_index: int) -> Tuple[List[str], int]:
        """
        Reads content inside parens (...) starting at start_index.
        Returns (List of params, new_index_after_closing_paren)
        """
        i = start_index + 1 # Skip opening (
        n = len(self.tokens)
        depth = 1
        content = []
        
        while i < n and depth > 0:
            t = self.tokens[i]
            if t == '(': depth += 1
            elif t == ')': depth -= 1
            
            if depth > 0:
                content.append(t)
            i += 1
            
        # Join content and split by comma to get params
        # E.g. ['10', ',', '2'] -> ['10', '2']
        raw_str = "".join(content)
        # Simple split by comma (doesn't handle nested commas in logic expressions, usually fine for copybooks)
        params = [p.strip() for p in raw_str.split(',') if p.strip()]
        
        return params, i

# =============================================================================
# Main Parser Logic
# =============================================================================

class PLICopybookParser:
    """
    Main Parser for PL/1 Copybooks.
    Uses PLIAttributeParser for analyzing individual fields.
    """

    def __init__(self, strict=False):
        self.strict = strict
        self.root_fields = []
        self._current_level_stack = []

    def parse_file(self, filepath: str) -> dict:
        path = Path(filepath)
        if not path.exists():
            raise FileNotFoundError(f"File not found: {filepath}")

        # EBCDIC Fallback
        try:
            content = path.read_text('utf-8')
            encoding = 'utf-8'
        except:
            content = path.read_text('cp037', errors='replace') # EBCDIC US
            encoding = 'cp037'
            
        result = self.parse_string(content, path.name)
        result['encoding'] = encoding
        return result

    def parse_string(self, content: str, source_name: str) -> dict:
        self.root_fields = []
        self._current_level_stack = [] # Reset stack for new file
        
        # 1. Preprocess: Remove comments, normalize spaces
        clean_code = re.sub(r'/\*.*?\*/', ' ', content, flags=re.DOTALL)
        clean_code = " ".join(clean_code.split())
        
        # 2. Extract DECLARE statements
        # Regex finds "DCL" or "DECLARE" followed by content until ";"
        dcl_iterator = re.finditer(r'\b(?:DCL|DECLARE)\s+(.*?);', clean_code, re.IGNORECASE)
        
        count = 0
        has_explicit_dcl = False
        
        for match in dcl_iterator:
            has_explicit_dcl = True
            body = match.group(1)
            self._process_dcl_body(body)
            count += 1

        # 3. Fallback: Implicit Structure (No 'DCL' keyword)
        # If no DCL keywords were found, but the file looks like a structure (starts with a number)
        if not has_explicit_dcl:
            # Remove trailing semicolon if present
            stripped_body = clean_code.strip().rstrip(';')
            
            # Check if it starts with a number (Level number)
            # Example: "1 MASTER_POLICY_REC ..."
            if re.match(r'^\d+', stripped_body):
                logger.info(f"No explicit DCL found in {source_name}, parsing as implicit structure.")
                self._process_dcl_body(stripped_body)
                count = 1

        return {
            "copybook_name": source_name,
            "dcl_count": count,
            "fields": [f.to_dict() for f in self.root_fields]
        }

    def _process_dcl_body(self, body: str):
        """
        Parses the body of a DCL statement. 
        Handles factoring: DCL 1 A, 2 B CHAR(5);
        """
        # Split by comma, BUT respect parentheses to avoid splitting CHAR(5,2)
        tokens = self._smart_split(body)
        
        for token in tokens:
            self._parse_node(token)

    def _smart_split(self, text: str) -> List[str]:
        """Splits by comma not inside parens."""
        parts = []
        curr = []
        depth = 0
        for c in text:
            if c == '(': depth += 1
            if c == ')': depth -= 1
            if c == ',' and depth == 0:
                parts.append("".join(curr).strip())
                curr = []
            else:
                curr.append(c)
        if curr: parts.append("".join(curr).strip())
        return parts

    def _parse_node(self, token: str):
        """
        Parses a single variable declaration fragment.
        Pattern: (Level)? (Name) (Attributes)
        """
        # Regex to capture: Level? Name Attributes
        # Name might be factored like (A, B)
        # Update: Allow name to contain dashes or underscores
        match = re.match(r'^(\d+)?\s*([\w\$\#\@\-]+|\([\w\s,]+\))\s*(.*)', token)
        if not match: return

        level_str = match.group(1)
        name_part = match.group(2)
        attrs = match.group(3)

        level = int(level_str) if level_str else 1
        
        # Check for Factored Names: DCL (A,B) CHAR(10);
        names = []
        if name_part.startswith('('):
            # Strip parens and split
            names = [n.strip() for n in name_part.strip('()').split(',')]
        else:
            names = [name_part]

        for name in names:
            field_obj = PLIField(level=level, name=name.upper(), attributes_raw=attrs)
            self._analyze_attributes(field_obj, attrs)
            self._add_to_hierarchy(field_obj)

    def _analyze_attributes(self, field: PLIField, attrs: str):
        """
        Uses the Token Parser to extract type, length, etc.
        """
        if not attrs.strip():
            # If no attributes and it's Level 1, likely a Structure or Implicit
            if field.level == 1:
                field.data_type = "STRUCTURE"
            return

        # 1. Parse using the Lexer
        attr_parser = PLIAttributeParser(attrs)
        parsed = attr_parser.parse()
        
        # 2. Map parsed data
        if parsed['data_type']:
            field.data_type = parsed['data_type']
        elif field.level > 1:
             # Level > 1 without type is often a substructure, 
             # unless it inherits (PL/I inheritance rules are complex, defaulting to STRUCT here)
             field.data_type = "STRUCTURE"

        # Length / Scale
        if parsed['length']:
            try:
                field.length = int(parsed['length'])
            except ValueError:
                field.length = 0 
        
        if parsed['scale']:
            try:
                field.scale = int(parsed['scale'])
            except ValueError:
                field.scale = 0

        # 3. Enterprise PL/I Byte Alignment Heuristics
        if 'FIXED' in field.data_type and 'BIN' in field.data_type:
            # For FIXED BIN, length=precision. Map to bytes.
            precision = field.length
            if precision <= 15: field.length = 2
            elif precision <= 31: field.length = 4
            else: field.length = 8
        elif 'PTR' in field.data_type:
            field.length = 4
        elif 'BIT' in field.data_type:
            # Length is bits
            pass
        elif 'DEC' in field.data_type:
            # Packed Decimal (approx bytes: (digits+1)/2)
            if field.length:
                 field.length = (field.length + 1) // 2

        # 4. Arrays
        if parsed['dimension']:
            field.is_array = True
            field.dimension = parsed['dimension']
            
        # 5. Other
        field.initial_value = parsed['initial']
        field.storage_class = parsed['storage_class']
        
        if parsed['is_varying']:
            field.attributes_raw += " VARYING"

    def _add_to_hierarchy(self, new_field: PLIField):
        """Maintains the tree structure based on levels."""
        # Level 1 is always root
        if new_field.level == 1:
            self.root_fields.append(new_field)
            self._current_level_stack = [new_field]
            return

        # Find parent: Look backwards in stack for first node with level < new_field.level
        while self._current_level_stack:
            top = self._current_level_stack[-1]
            if top.level < new_field.level:
                # Found parent
                top.children.append(new_field)
                new_field._parent = top
                self._current_level_stack.append(new_field)
                return
            else:
                # This node is a sibling or child of a previous branch, pop
                self._current_level_stack.pop()
        
        # Fallback (Should rarely happen in valid PL/I)
        self.root_fields.append(new_field)
        self._current_level_stack = [new_field]

def main():
    parser = argparse.ArgumentParser(
        description='Parse PL/1 Copybook/Include Files to JSON',
        epilog='Features: DCL Factoring, Token-based parsing, Byte alignment heuristics.'
    )
    parser.add_argument('input', help='Input PL/1 copybook file')
    parser.add_argument('-o', '--output', help='Output JSON file')
    parser.add_argument('--indent', type=int, default=2, help='JSON indentation')
    
    args = parser.parse_args()
    
    try:
        pli_parser = PLICopybookParser()
        result = pli_parser.parse_file(args.input)
        
        json_output = json.dumps(result, indent=args.indent)
        if args.output:
            Path(args.output).write_text(json_output, encoding='utf-8')
            print(f"Output written to {args.output}")
        else:
            print(json_output)
            
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == '__main__':
    main()