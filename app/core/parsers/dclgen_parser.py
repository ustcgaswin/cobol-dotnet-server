"""
DCLGEN Parser for IBM DB2 COBOL Declarations.

Extracts both SQL Table Metadata and COBOL Host Variable Structures.
Features:
- Resilient PIC parsing (prevents truncation).
- DB2 VARCHAR collapsing (collapses 49-level pairs).
- Mainframe-accurate byte length calculation (COMP-3/COMP).
- Nullable Indicator linking (INDNULL- linkage).
- .NET Type mapping.
"""

import re
import logging
from pathlib import Path
from dataclasses import dataclass, field
from typing import Optional, List, Dict, Any

from app.core.parsers.base import BaseParser
from app.core.exceptions import ParseError, InvalidSyntaxError

logger = logging.getLogger('dclgen_parser')

# =============================================================================
# Data Models
# =============================================================================

@dataclass
class SqlMetadata:
    """Stores metadata from the EXEC SQL DECLARE TABLE section."""
    name: str
    db2_type: str
    is_nullable: bool

@dataclass
class DclgenField:
    """Represents a combined SQL/COBOL field definition."""
    level: int
    name: str
    line_number: int
    picture: Optional[str] = None
    usage: Optional[str] = None
    computed_length: int = 0
    start_position: int = 0
    end_position: int = 0
    db2_type: Optional[str] = None
    is_nullable: bool = False
    is_varchar: bool = False
    indicator_variable: Optional[str] = None
    dotnet_type: str = "object"
    children: List['DclgenField'] = field(default_factory=list)

    def to_dict(self) -> dict:
        """Serializes the field for JSON output."""
        res = {
            "level": self.level,
            "name": self.name,
            "line_number": self.line_number,
            "pic": self.picture,
            "usage": self.usage,
            "length": self.computed_length,
            "start": self.start_position,
            "end": self.end_position,
            "db2_type": self.db2_type,
            "dotnet_type": self.dotnet_type,
            "is_nullable": self.is_nullable,
            "is_varchar": self.is_varchar,
            "indicator": self.indicator_variable
        }
        if self.children:
            res["children"] = [c.to_dict() for c in self.children]
        # Return only non-None values for a cleaner JSON
        return {k: v for k, v in res.items() if v is not None}

# =============================================================================
# Dclgen Parser Implementation
# =============================================================================

class DclgenParser(BaseParser):
    """
    Parser for DB2 DCLGEN files.
    Identifies SQL column metadata and maps them to COBOL host variables.
    """
    FILE_TYPE = "dclgen"
    
    # EBCDIC and Search Patterns
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500']

    def __init__(self, strict: bool = False):
        self.strict = strict
        self._sql_meta: Dict[str, SqlMetadata] = {}

    def parse_file(self, filepath: str) -> dict:
        """Main entry point: loads, cleans, and parses the DCLGEN."""
        path = Path(filepath)
        logger.info(f"Processing DCLGEN: {path.name}")
        
        content = self._detect_and_load(path)
        processed_lines = self._preprocess(content)
        full_text = "\n".join([text for _, text, _ in processed_lines])

        # 1. Extract SQL Table Definition
        table_name = self._parse_sql_metadata(full_text)

        # 2. Extract COBOL Data Structure
        raw_fields = self._parse_cobol_structure(processed_lines)

        # 3. Handle VARCHAR Collapsing and Hierarchy
        roots = self._process_structure(raw_fields)

        # 4. Final Stitching (SQL Metadata + Indicator Linkage)
        self._stitch_metadata(roots, raw_fields)

        return {
            "table_name": table_name,
            "source": path.name,
            "record_layout": [f.to_dict() for f in roots]
        }

    def _detect_and_load(self, path: Path) -> str:
        """Detects encoding (EBCDIC/UTF-8) and ensures it's a DCLGEN."""
        raw = path.read_bytes()
        for enc in self.EBCDIC_CODEPAGES + ['utf-8', 'ascii']:
            try:
                text = raw.decode(enc)
                if "EXEC SQL" in text.upper():
                    return text
            except (UnicodeDecodeError, LookupError):
                continue
        return raw.decode('utf-8', errors='replace')

    def _preprocess(self, content: str):
        """Standard COBOL preprocessing for Area A/B."""
        lines = content.splitlines()
        processed = []
        for i, line in enumerate(lines, 1):
            if len(line) < 7: continue
            if line[6] in ('*', '/'): continue # Skip comments
            # Extract cols 8-72 (Standard COBOL text)
            text = line[7:72].rstrip()
            if text.strip():
                processed.append((i, text, line))
        return processed

    def _parse_sql_metadata(self, text: str) -> str:
        """Parses the EXEC SQL DECLARE TABLE block."""
        # Find the block inside the parentheses
        block_match = re.search(r"DECLARE\s+([\w-]+)\s+TABLE\s*\((.*?)\)\s*END-EXEC", text, re.I | re.S)
        if not block_match:
            return "UNKNOWN"

        table_name = block_match.group(1).upper()
        col_content = block_match.group(2)

        # Matches: COLUMN_NAME  TYPE(DIMS)  NOT NULL
        col_re = re.compile(r"([A-Z0-9_-]+)\s+([A-Z0-9]+(?:\([\d\s,]+\))?)\s*(NOT\s+NULL)?", re.I)
        
        for m in col_re.finditer(col_content):
            name = m.group(1).upper()
            db2_type = m.group(2).upper()
            is_nullable = not bool(m.group(3))
            self._sql_meta[name] = SqlMetadata(name, db2_type, is_nullable)
        
        return table_name

    def _parse_cobol_structure(self, lines) -> List[DclgenField]:
        """Parses Host Structure with resilient PIC and Usage detection."""
        fields = []
        level_re = re.compile(r"^\s*(\d{2})\s+([\w-]+)", re.I)
        
        # Capture PIC until a period (terminal COBOL statement)
        pic_re = re.compile(r"PIC(?:TURE)?\s+(?:IS\s+)?([^\.]+)", re.I)
        usage_re = re.compile(r"USAGE\s+(?:IS\s+)?([\w-]+)", re.I)

        for line_num, text, _ in lines:
            m = level_re.search(text)
            if not m: continue
            
            level = int(m.group(1))
            name = m.group(2).upper()

            # Fix for PIC Truncation
            pic_match = pic_re.search(text)
            pic = pic_match.group(1).strip().upper() if pic_match else None

            usage_match = usage_re.search(text)
            usage = usage_match.group(1).upper() if usage_match else None
            
            # Explicit COMP-3 detection for DCLGEN decimals
            if not usage and pic and "COMP-3" in text.upper():
                usage = "COMP-3"

            fields.append(DclgenField(
                level=level, name=name, line_number=line_num, 
                picture=pic, usage=usage
            ))
        return fields

    def _process_structure(self, raw_fields: List[DclgenField]) -> List[DclgenField]:
        """Builds field hierarchy and collapses DB2 VARCHARs."""
        roots = []
        stack: List[DclgenField] = []
        i = 0
        while i < len(raw_fields):
            curr = raw_fields[i]
            
            # DB2 VARCHAR Logic: Group + Level 49 -LEN + Level 49 -TEXT
            if i + 2 < len(raw_fields):
                n1, n2 = raw_fields[i+1], raw_fields[i+2]
                if n1.level == 49 and n2.level == 49 and "-LEN" in n1.name.upper():
                    curr.is_varchar = True
                    curr.picture = n2.picture # Use TEXT portion for length
                    curr.usage = "DISPLAY"
                    i += 2 # Consume child fields

            while stack and stack[-1].level >= curr.level:
                stack.pop()
            
            if stack:
                stack[-1].children.append(curr)
            else:
                roots.append(curr)
            
            stack.append(curr)
            i += 1

        # Recursive offset calculation
        self._calculate_offsets(roots, 1)
        return roots

    def _calculate_offsets(self, fields: List[DclgenField], pos: int) -> int:
        """Mainframe-standard sequential offset calculation."""
        for f in fields:
            f.start_position = pos
            if f.children and not f.is_varchar:
                new_pos = self._calculate_offsets(f.children, pos)
                f.computed_length = new_pos - pos
                f.end_position = new_pos - 1
                pos = new_pos
            else:
                f.computed_length = self._get_mainframe_length(f.picture, f.usage)
                f.end_position = pos + f.computed_length - 1
                pos += f.computed_length
        return pos

    def _get_mainframe_length(self, pic: Optional[str], usage: Optional[str]) -> int:
        """Calculates bytes based on IBM COMP/COMP-3 rules."""
        if not pic: return 0
        
        # Expand PIC 9(5) -> 99999
        expanded = re.sub(r"(\w)\((\d+)\)", lambda m: m.group(1) * int(m.group(2)), pic)
        # Remove COBOL symbols that occupy no storage
        clean = expanded.replace("V", "").replace("S", "").replace(" ", "")
        digits = len(clean)

        usage = (usage or "").upper()
        # COMP-3: (n+1)//2
        if "COMP-3" in usage:
            return (digits + 1) // 2
        
        # BINARY / COMP: 2, 4, or 8 bytes
        if "COMP" in usage or "BINARY" in usage:
            if digits <= 4: return 2
            if digits <= 9: return 4
            return 8
            
        # Standard DISPLAY
        return digits

    def _stitch_metadata(self, roots: List[DclgenField], all_fields: List[DclgenField]):
        """Marries SQL truth with COBOL structure and links Indicators."""
        flat_map = {f.name: f for f in all_fields}
        
        def walk(f: DclgenField):
            # 1. Link SQL Metadata
            if f.name in self._sql_meta:
                meta = self._sql_meta[f.name]
                f.db2_type = meta.db2_type
                f.is_nullable = meta.is_nullable
                f.dotnet_type = self._map_to_dotnet(meta.db2_type, meta.is_nullable)

            # 2. Indicator Linking (Standard DCLGEN pattern: INDNULL-[NAME])
            ind_name = f"INDNULL-{f.name}"
            if ind_name in flat_map:
                f.indicator_variable = ind_name
                # Note: Presence of indicator implies nullability even if SQL says otherwise
                f.is_nullable = True 
            
            for child in f.children:
                walk(child)

        for r in roots: walk(r)

    def _map_to_dotnet(self, db2_type_str: str, is_nullable: bool) -> str:
        """Determines the C# / .NET type based on DB2 column type."""
        base_type = db2_type_str.split('(')[0].strip().upper()
        
        mapping = {
            "CHAR": "string", "VARCHAR": "string", "DECIMAL": "decimal",
            "NUMERIC": "decimal", "INTEGER": "int", "SMALLINT": "short",
            "BIGINT": "long", "DATE": "DateTime", "TIME": "TimeSpan",
            "TIMESTAMP": "DateTime", "FLOAT": "double", "REAL": "float"
        }
        
        dotnet_type = mapping.get(base_type, "object")
        
        # Nullable value types
        if is_nullable and dotnet_type not in ["string", "object"]:
            return dotnet_type + "?"
        
        return dotnet_type