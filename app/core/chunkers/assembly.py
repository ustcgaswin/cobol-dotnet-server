
"""Production-grade HLASM chunker for structure-aware chunking."""

import re
from pathlib import Path
from loguru import logger
from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError

class AssemblyChunker(BaseChunker):
    """
    Chunks HLASM files at logical boundaries:
    - Control Sections (CSECT/DSECT/RSECT/COM/LOCTR)
    - Major functional labels (ignores small DS/DC)
    - Handles continuations (col 72)
    - Groups data areas at end
    """
    SUPPORTED_EXTENSIONS = [".asm", ".hlasm", ".s", ".txt"]
    MIN_CHUNK_LINES = 4  # Ignore tiny chunks

    SECTION_PATTERN = re.compile(
        r'^\s*([A-Z0-9_@#$]{1,63})?\s+(CSECT|DSECT|RSECT|COM|LOCTR)\b',
        re.IGNORECASE
    )

    # Major label: only if followed by instruction (not DS/DC/EQU)
    LABEL_PATTERN = re.compile(
        r'^([A-Z0-9_@#$]{1,8})\s+'
        r'(?!(DS|DC|EQU|ORG|LTORG|EXTRN|WXTRN|SPACE|EJECT|PRINT|TITLE|COPY|MACRO|MEND))',
        re.IGNORECASE
    )

    def chunk(self, filepaths: list[str]) -> list[dict]:
        all_chunks = []
        for fp in filepaths:
            try:
                chunks = self._chunk_file(fp)
                all_chunks.extend(chunks)
            except Exception as e:
                logger.error(f"Chunking failed {fp}: {e}")
                raise ChunkError(f"Assembly chunking failed: {e}")
        return all_chunks

    def _chunk_file(self, filepath: str) -> list[dict]:
        content = Path(filepath).read_text(encoding='utf-8', errors='replace')
        content = self._preprocess(content)
        lines = content.splitlines()

        chunks = []
        current = []
        section = "HEADER"
        chunk_name = "INITIALIZATION"
        is_data_section = False

        i = 0
        while i < len(lines):
            line = lines[i].rstrip()

            # Collect continuation
            stmt_lines = [line]
            j = i + 1
            while j < len(lines) and len(lines[j]) >= 72 and lines[j][71].strip():
                cont = lines[j][15:].lstrip() if len(lines[j]) > 15 else lines[j]
                stmt_lines.append(cont)
                j += 1

            stmt = ' '.join(s.strip() for s in stmt_lines if s.strip()).strip()
            if not stmt:
                i = j
                continue

            # New section?
            if (m := self.SECTION_PATTERN.search(stmt)):
                if current and len(current) >= self.MIN_CHUNK_LINES:
                    chunks.append(self._make_chunk(current, section, chunk_name, filepath))
                section = (m.group(1) or "UNNAMED").upper()
                chunk_name = m.group(2).upper()
                current = stmt_lines
                is_data_section = False
                i = j
                continue

            # Major functional label?
            if (m := self.LABEL_PATTERN.match(line)):
                if current and len(current) >= self.MIN_CHUNK_LINES:
                    chunks.append(self._make_chunk(current, section, chunk_name, filepath))
                chunk_name = m.group(1).upper()
                current = stmt_lines
                is_data_section = False
                i = j
                continue

            # Data area? (DS/DC/EQU/EXTRN)
            if re.match(r'^\s*[A-Z0-9_@#$]{1,8}\s+(DS|DC|EQU|EXTRN|WXTRN)', stmt):
                if not is_data_section and current and len(current) >= self.MIN_CHUNK_LINES:
                    chunks.append(self._make_chunk(current, section, chunk_name, filepath))
                    current = []
                is_data_section = True
                chunk_name = "DATA_AREAS"
                current.extend(stmt_lines)
                i = j
                continue

            # Normal code line
            current.extend(stmt_lines)
            i = j

        # Final chunk
        if current and len(current) >= self.MIN_CHUNK_LINES:
            chunks.append(self._make_chunk(current, section, chunk_name, filepath))

        logger.info(f"Chunked {Path(filepath).name} → {len(chunks)} meaningful chunks")
        return chunks

    def _make_chunk(self, lines: list[str], section: str, name: str, source: str) -> dict:
        content = '\n'.join(lines)
        return {
            "content": content,
            "type": "section" if section != "HEADER" else "header",
            "name": f"[{section}] {name}".strip(),
            "source": source,
            "line_count": len(lines)
        }

    def _preprocess(self, content: str) -> str:
        lines = []
        for line in content.splitlines():
            if len(line) >= 72:
                line = line[:72].rstrip()
            if line.strip() and line.lstrip()[0] == '*':
                continue
            if line.strip():
                lines.append(line)
        return '\n'.join(lines)