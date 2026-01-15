"""COBOL chunker for structure-aware chunking at division/paragraph boundaries."""

import re
from pathlib import Path
from typing import Optional

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class CobolChunker(BaseChunker):
    """Self-contained COBOL chunker that parses structure via regex.
    
    Chunks COBOL at logical boundaries:
    - Each division (IDENTIFICATION, ENVIRONMENT, DATA) as separate chunks
    - Each paragraph in PROCEDURE DIVISION as separate chunk
    """
    
    SUPPORTED_EXTENSIONS = [".cbl", ".cob", ".cobol"]
    
    # Division patterns
    DIVISION_PATTERN = re.compile(
        r'^[\s\d]*(\w+)\s+DIVISION\s*\.', 
        re.IGNORECASE | re.MULTILINE
    )
    
    # Paragraph pattern (name followed by period at Area A)
    PARAGRAPH_PATTERN = re.compile(
        r'^[\s\d]*([A-Z][A-Z0-9_-]*)\s*\.\s*$',
        re.IGNORECASE | re.MULTILINE
    )
    
    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Chunk COBOL files at division/paragraph boundaries.
        
        Args:
            filepaths: List of COBOL file paths
            
        Returns:
            List of chunk dicts with 'content', 'type', 'name', 'source' keys
        """
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
        """Return entire file(s) as single chunk(s).
        
        Args:
            filepaths: List of COBOL file paths
            
        Returns:
            List of chunk dicts, one per file
        """
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
        """Chunk a single COBOL file."""
        content = Path(filepath).read_text(encoding='utf-8', errors='replace')
        content = self._preprocess_source(content)
        lines = content.split('\n')
        
        chunks = []
        filename = Path(filepath).name
        
        # Find all divisions
        divisions = self._find_divisions(lines)
        
        if not divisions:
            # No divisions found, return whole file as single chunk
            logger.warning(f"No divisions found in {filepath}, returning as single chunk")
            return [{
                'content': content,
                'type': 'whole',
                'name': filename,
                'source': filepath,
            }]
        
        # Process each division
        for i, div in enumerate(divisions):
            div_name = div['name']
            start_line = div['start']
            end_line = divisions[i + 1]['start'] - 1 if i + 1 < len(divisions) else len(lines)
            
            if div_name.upper() == 'PROCEDURE':
                # Split PROCEDURE DIVISION by paragraphs
                proc_chunks = self._chunk_procedure_division(
                    lines, start_line, end_line, filepath
                )
                chunks.extend(proc_chunks)
            else:
                # Other divisions as single chunks
                div_content = '\n'.join(lines[start_line:end_line])
                chunks.append({
                    'content': div_content,
                    'type': 'division',
                    'name': f"{div_name} DIVISION",
                    'source': filepath,
                })
        
        logger.info(f"Chunked {filepath} into {len(chunks)} chunks")
        return chunks
    
    def _preprocess_source(self, content: str) -> str:
        """Strip COBOL sequence numbers (columns 1-6) if present.
        
        Also handles indicator column (7) by removing comment lines.
        """
        lines = content.split('\n')
        processed = []
        
        # Check if file has sequence numbers (columns 1-6 are numeric)
        has_sequence_numbers = self._has_sequence_numbers(lines)
        
        for line in lines:
            if has_sequence_numbers and len(line) >= 6:
                # Strip columns 1-6
                line = line[6:]
            
            # Check for comment indicator in column 7 (now column 1 after stripping)
            if line and line[0] in ('*', '/'):
                continue  # Skip comment lines
            
            processed.append(line)
        
        return '\n'.join(processed)
    
    def _has_sequence_numbers(self, lines: list[str]) -> bool:
        """Detect if file has sequence numbers in columns 1-6."""
        sample_count = 0
        numeric_count = 0
        
        for line in lines[:50]:  # Check first 50 lines
            if len(line) >= 6:
                sample_count += 1
                # Check if columns 1-6 are numeric or spaces
                prefix = line[:6]
                if prefix.strip() == '' or prefix.strip().isdigit():
                    numeric_count += 1
        
        # If >80% of sampled lines have numeric/blank prefix, likely has seq numbers
        return sample_count > 0 and (numeric_count / sample_count) > 0.8
    
    def _find_divisions(self, lines: list[str]) -> list[dict]:
        """Find all division boundaries in the source."""
        divisions = []
        
        for i, line in enumerate(lines):
            match = self.DIVISION_PATTERN.match(line)
            if match:
                div_name = match.group(1).upper()
                divisions.append({
                    'name': div_name,
                    'start': i,
                })
        
        return divisions
    
    def _chunk_procedure_division(
        self, 
        lines: list[str], 
        start: int, 
        end: int,
        filepath: str
    ) -> list[dict]:
        """Chunk PROCEDURE DIVISION by paragraphs."""
        chunks = []
        
        # Find all paragraphs within PROCEDURE DIVISION
        paragraphs = self._find_paragraphs(lines, start, end)
        
        if not paragraphs:
            # No paragraphs found, return whole PROCEDURE DIVISION
            content = '\n'.join(lines[start:end])
            return [{
                'content': content,
                'type': 'division',
                'name': 'PROCEDURE DIVISION',
                'source': filepath,
            }]
        
        # First chunk: from PROCEDURE DIVISION header to first paragraph
        if paragraphs[0]['start'] > start:
            pre_content = '\n'.join(lines[start:paragraphs[0]['start']])
            if pre_content.strip():
                chunks.append({
                    'content': pre_content,
                    'type': 'division_header',
                    'name': 'PROCEDURE DIVISION (header)',
                    'source': filepath,
                })
        
        # Each paragraph as a chunk
        for i, para in enumerate(paragraphs):
            para_start = para['start']
            para_end = paragraphs[i + 1]['start'] if i + 1 < len(paragraphs) else end
            
            para_content = '\n'.join(lines[para_start:para_end])
            chunks.append({
                'content': para_content,
                'type': 'paragraph',
                'name': para['name'],
                'source': filepath,
            })
        
        return chunks
    
    def _find_paragraphs(self, lines: list[str], start: int, end: int) -> list[dict]:
        """Find paragraph boundaries within a range."""
        paragraphs = []
        
        # Skip the PROCEDURE DIVISION line itself
        search_start = start + 1
        
        for i in range(search_start, end):
            line = lines[i]
            
            # Skip empty lines and lines that don't start in Area A
            if not line.strip():
                continue
            
            # Check if this looks like a paragraph name (starts in Area A, ends with period)
            match = self.PARAGRAPH_PATTERN.match(line)
            if match:
                para_name = match.group(1).upper()
                # Exclude known division/section keywords
                if para_name not in ('IDENTIFICATION', 'ENVIRONMENT', 'DATA', 
                                     'PROCEDURE', 'FILE', 'WORKING-STORAGE',
                                     'LINKAGE', 'LOCAL-STORAGE', 'SCREEN',
                                     'REPORT', 'INPUT-OUTPUT', 'CONFIGURATION'):
                    paragraphs.append({
                        'name': para_name,
                        'start': i,
                    })
        
        return paragraphs
