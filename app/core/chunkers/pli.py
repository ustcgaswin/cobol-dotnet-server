"""PL/I chunker for structure-aware chunking."""

import re
from pathlib import Path
from typing import List, Dict

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class PliChunker(BaseChunker):
    """Structure-aware PL/I chunker.
    
    Chunks PL/I code at logical boundaries:
    1. Procedures (external and internal) identified by 'LABEL: PROC'.
    2. Major Labels identified by 'LABEL:'.
    
    This flattens the nested structure into sequential chunks for the LLM,
    allowing it to focus on one logical block (Procedure/Section) at a time.
    """

    SUPPORTED_EXTENSIONS = [".pli", ".pl1"]

    # Regex to find Procedure definitions: "MYPROC: PROC(ARGS) OPTIONS(MAIN);"
    # Group 1: Label Name
    PROC_PATTERN = re.compile(
        r'^\s*([\w#$@]+)\s*:\s*(?:PROC|PROCEDURE)\b', 
        re.IGNORECASE | re.MULTILINE
    )

    # Regex to find standard Labels: "MYLABEL: STATEMENT;"
    # Look for a word followed by a colon, ensuring it's NOT a PROC definition.
    LABEL_PATTERN = re.compile(
        r'^\s*([\w#$@]+)\s*:\s*(?!PROC|PROCEDURE)', 
        re.IGNORECASE | re.MULTILINE
    )

    # Regex for PL/I comments /* ... */
    COMMENT_PATTERN = re.compile(r'/\*.*?\*/', re.DOTALL)

    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Chunk PL/I files based on Procedures and Labels."""
        all_chunks = []
        for filepath in filepaths:
            try:
                chunks = self._chunk_file(filepath)
                all_chunks.extend(chunks)
            except Exception as e:
                logger.error(f"Failed to chunk {filepath}: {e}")
                raise ChunkError(f"Failed to chunk {filepath}: {e}") from e
        return all_chunks

    def get_whole(self, filepaths: list[str]) -> list[dict]:
        """Return entire PL/I file(s) as single chunks (cleaned)."""
        chunks = []
        for filepath in filepaths:
            try:
                content = Path(filepath).read_text(encoding='utf-8', errors='replace')
                content = self._preprocess_source(content)
                chunks.append({
                    'content': content,
                    'type': 'whole',
                    'name': Path(filepath).name,
                    'source': filepath,
                })
            except Exception as e:
                logger.error(f"Failed to read {filepath}: {e}")
                raise ChunkError(f"Failed to read {filepath}: {e}") from e
        return chunks

    def _chunk_file(self, filepath: str) -> list[dict]:
        """Parse file content and split into chunks based on regex boundaries."""
        raw_content = Path(filepath).read_text(encoding='utf-8', errors='replace')
        
        # Preprocess: Remove comments and sequence numbers
        content = self._preprocess_source(raw_content)
        lines = content.split('\n')
        
        chunks = []
        current_chunk_lines = []
        current_chunk_name = "HEADER / DECLARATIONS"
        current_chunk_type = "header"

        for i, line in enumerate(lines):
            # 1. Check for Procedure Start
            proc_match = self.PROC_PATTERN.match(line)
            if proc_match:
                # Save previous chunk if valid
                if self._has_content(current_chunk_lines):
                    chunks.append(self._make_chunk(
                        current_chunk_lines, current_chunk_name, current_chunk_type, filepath
                    ))
                
                # Start new chunk
                current_chunk_name = f"PROC {proc_match.group(1).upper()}"
                current_chunk_type = "procedure"
                current_chunk_lines = [line]
                continue

            # 2. Check for Label (only if we aren't at the very top header)
            # This mimics COBOL paragraphs. We assume labeled blocks are significant.
            label_match = self.LABEL_PATTERN.match(line)
            if label_match:
                # Save previous chunk
                if self._has_content(current_chunk_lines):
                    chunks.append(self._make_chunk(
                        current_chunk_lines, current_chunk_name, current_chunk_type, filepath
                    ))
                
                # Start new chunk
                current_chunk_name = f"LABEL {label_match.group(1).upper()}"
                current_chunk_type = "label_block"
                current_chunk_lines = [line]
                continue

            # 3. Accumulate line
            current_chunk_lines.append(line)

        # Append the final chunk
        if self._has_content(current_chunk_lines):
            chunks.append(self._make_chunk(
                current_chunk_lines, current_chunk_name, current_chunk_type, filepath
            ))

        if not chunks:
             # Fallback if no structure found
            return [{
                'content': content,
                'type': 'whole',
                'name': Path(filepath).name,
                'source': filepath,
            }]

        logger.debug(f"Chunked {Path(filepath).name} into {len(chunks)} sections.")
        return chunks

    def _make_chunk(self, lines: List[str], name: str, type_: str, source: str) -> Dict:
        return {
            'content': '\n'.join(lines),
            'type': type_,
            'name': name,
            'source': source
        }

    def _has_content(self, lines: List[str]) -> bool:
        """Check if lines contain anything other than whitespace."""
        return any(line.strip() for line in lines)

    def _preprocess_source(self, content: str) -> str:
        """Cleanup PL/I source code."""
        # 1. Strip sequence numbers if detected (similar to COBOL)
        lines = content.split('\n')
        if self._has_sequence_numbers(lines):
            # PL/I margins vary, but often 2-72 or 1-72. 
            # If sequence numbers exist in 1-6 or 73-80, we try to strip column 1-1 (if numbered there)
            # or strictly rely on the logic that sequence numbers look numeric.
            # Simplified approach: If lines start with 8 digits or 6 digits consistently, strip them.
            processed_lines = []
            for line in lines:
                if len(line) > 8 and line[:6].isdigit() and line[6] == ' ':
                     processed_lines.append(line[7:]) # Strip standard seq area
                elif len(line) > 72 and line[:6].isdigit() is False: 
                    # Handle right-margin sequence numbers? (Optional, skipping for safety)
                    processed_lines.append(line)
                else:
                    processed_lines.append(line)
            content = '\n'.join(processed_lines)

        # 2. Remove Block Comments /* ... */
        # We replace with a space to avoid joining tokens accidentally (e.g. A/*comment*/B -> AB)
        content = self.COMMENT_PATTERN.sub(' ', content)

        # 3. Clean up empty lines
        return re.sub(r'\n\s*\n', '\n', content).strip()

    def _has_sequence_numbers(self, lines: list[str]) -> bool:
        """Detect presence of sequence numbers in columns 1-6."""
        sample_count = 0
        numeric_count = 0
        for line in lines[:50]:
            if len(line) >= 7: # Need at least col 1-6 and a separator
                sample_count += 1
                prefix = line[:6]
                if prefix.isdigit():
                    numeric_count += 1
        return sample_count > 0 and (numeric_count / sample_count) > 0.8