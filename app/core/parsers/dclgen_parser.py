"""
DCLGEN Parser for IBM DB2 COBOL Declarations.
Simplified output: only required fields for migration.
"""

import re
import logging
from pathlib import Path
from dataclasses import dataclass, field
from typing import Optional, List, Dict

from app.core.parsers.base import BaseParser

logger = logging.getLogger('dclgen_parser')

# =============================================================================
# Data Models
# =============================================================================

@dataclass
class SqlMetadata:
    name: str
    db2_type: str
    is_nullable: bool

@dataclass
class DclgenField:
    level: int
    name: str
    line_number: int
    db2_type: Optional[str] = None
    is_nullable: bool = False
    indicator_variable: Optional[str] = None
    dotnet_type: str = "object"
    children: List['DclgenField'] = field(default_factory=list)

    def to_dict(self) -> dict:
        """Only the fields we actually want in the JSON."""
        res = {
            "name": self.name,
            "line_number": self.line_number,
            "db2_type": self.db2_type,
            "dotnet_type": self.dotnet_type,
            "is_nullable": self.is_nullable,
            "indicator_variable": self.indicator_variable
        }
        if self.children:
            res["children"] = [c.to_dict() for c in self.children]
        return {k: v for k, v in res.items() if v is not None}

# =============================================================================
# Dclgen Parser Implementation
# =============================================================================

class DclgenParser(BaseParser):
    FILE_TYPE = "dclgen"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500']

    def __init__(self, strict: bool = False):
        self.strict = strict
        self._sql_meta: Dict[str, SqlMetadata] = {}

    def parse_file(self, filepath: str) -> dict:
        path = Path(filepath)
        logger.info(f"Processing DCLGEN: {path.name}")

        content = self._detect_and_load(path)
        processed_lines = self._preprocess(content)
        full_text = "\n".join([text for _, text, _ in processed_lines])

        table_name = self._parse_sql_metadata(full_text)
        raw_fields = self._parse_cobol_structure(processed_lines)
        roots = self._process_structure(raw_fields)          # hierarchy only
        self._stitch_metadata(roots, raw_fields)             # SQL + indicators only

        return {
            "source": path.name,
            "file_type": self.FILE_TYPE,
            "table_name": table_name,
            "record_layout": [f.to_dict() for f in roots]
        }

    def _detect_and_load(self, path: Path) -> str:
        raw = path.read_bytes()
        for enc in self.EBCDIC_CODEPAGES + ['utf-8', 'ascii']:
            try:
                text = raw.decode(enc)
                if "EXEC SQL" in text.upper():
                    return text
            except:
                continue
        return raw.decode('utf-8', errors='replace')

    def _preprocess(self, content: str):
        lines = content.splitlines()
        processed = []
        for i, line in enumerate(lines, 1):
            if len(line) < 7:
                continue
            if line[6] in ('*', '/'):
                continue
            text = line[7:72].rstrip()
            if text.strip():
                processed.append((i, text, line))
        return processed

    def _parse_sql_metadata(self, text: str) -> str:
        block_match = re.search(r"DECLARE\s+([\w-]+)\s+TABLE\s*\((.*?)\)\s*END-EXEC",
                               text, re.I | re.S)
        if not block_match:
            return "UNKNOWN"

        table_name = block_match.group(1).upper()
        col_content = block_match.group(2)
        cols = [c.strip() for c in re.split(r'(?<!\d)\s*,\s*', col_content) if c.strip()]

        self._sql_meta = {}
        for col in cols:
            m = re.match(r"([A-Z0-9_-]+)\s+([A-Z0-9]+(?:\([\d\s,]+\))?)(\s+NOT\s+NULL(?:\s+WITH\s+DEFAULT.*)?)?",
                         col, re.I)
            if m:
                name = m.group(1).upper()
                db2_type = m.group(2).upper()
                is_nullable = not bool(m.group(3))
                self._sql_meta[name] = SqlMetadata(name, db2_type, is_nullable)

        return table_name

    def _parse_cobol_structure(self, lines) -> List[DclgenField]:
        fields = []
        level_re = re.compile(r"^\s*(\d{2})\s+([\w-]+)", re.I)
        i = 0
        while i < len(lines):
            line_num, text, _ = lines[i]
            m = level_re.search(text)
            if not m:
                i += 1
                continue
            level = int(m.group(1))
            if level == 88:
                i += 1
                continue
            name = m.group(2).upper()

            full_text = text
            i += 1
            while i < len(lines):
                next_num, next_text, _ = lines[i]
                next_m = level_re.search(next_text)
                if next_m:
                    break
                full_text += ' ' + next_text
                i += 1

            fields.append(DclgenField(level=level, name=name, line_number=line_num))
        return fields

    def _process_structure(self, raw_fields: List[DclgenField]) -> List[DclgenField]:
        roots = []
        stack: List[DclgenField] = []
        i = 0
        while i < len(raw_fields):
            curr = raw_fields[i]
            # Simple VARCHAR skip (we no longer store is_varchar)
            if i + 2 < len(raw_fields) and raw_fields[i+1].level == 49 and "-LEN" in raw_fields[i+1].name.upper():
                i += 2
                continue

            while stack and stack[-1].level >= curr.level:
                stack.pop()
            if stack:
                stack[-1].children.append(curr)
            else:
                roots.append(curr)
            stack.append(curr)
            i += 1
        return roots

    def _stitch_metadata(self, roots: List[DclgenField], all_fields: List[DclgenField]):
        flat_map = {f.name: f for f in all_fields}

        def walk(f: DclgenField):
            if "FILLER" in f.name:
                for child in f.children:
                    walk(child)
                return

            norm_name = re.sub(r'^[A-Z0-9]{1,3}-', '', f.name).replace('-', '_')
            if norm_name in self._sql_meta:
                meta = self._sql_meta[norm_name]
                f.db2_type = meta.db2_type
                f.is_nullable = meta.is_nullable
                f.dotnet_type = self._map_to_dotnet(meta.db2_type, meta.is_nullable)
            elif f.name in self._sql_meta:
                meta = self._sql_meta[f.name]
                f.db2_type = meta.db2_type
                f.is_nullable = meta.is_nullable
                f.dotnet_type = self._map_to_dotnet(meta.db2_type, meta.is_nullable)

            # Indicator linking
            for prefix in ["INDNULL-", "NULL-", "IND-", "N_"]:
                ind_name = prefix + f.name
                if ind_name in flat_map:
                    f.indicator_variable = ind_name
                    f.is_nullable = True
                    break

            for child in f.children:
                walk(child)

        for r in roots:
            walk(r)

    def _map_to_dotnet(self, db2_type_str: str, is_nullable: bool) -> str:
        base_type = db2_type_str.split('(')[0].strip().upper()
        mapping = {
            "CHAR": "string", "VARCHAR": "string", "DECIMAL": "decimal",
            "NUMERIC": "decimal", "INTEGER": "int", "SMALLINT": "short",
            "BIGINT": "long", "DATE": "DateTime", "TIME": "TimeSpan",
            "TIMESTAMP": "DateTime", "FLOAT": "double", "REAL": "float",
            "GRAPHIC": "string", "VARGRAPHIC": "string"
        }
        dotnet_type = mapping.get(base_type, "object")
        if is_nullable and dotnet_type not in ["string", "object"]:
            return dotnet_type + "?"
        return dotnet_type