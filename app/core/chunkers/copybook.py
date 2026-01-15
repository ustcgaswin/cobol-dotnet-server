"""Copybook chunker."""

from pathlib import Path

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class CopybookChunker(BaseChunker):
    """Chunker for COBOL copybooks.
    
    Copybooks are typically small enough to be treated as a single chunk.
    This chunker mainly handles preprocessing (stripping sequence numbers).
    """
    
    SUPPORTED_EXTENSIONS = [".cpy", ".copy"]
    
    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Return entire copybook as single chunk.
        
        Args:
            filepaths: List of copybook file paths
            
        Returns:
            List of chunk dicts
        """
        # For copybooks, chunk() behaves same as get_whole()
        return self.get_whole(filepaths)
    
    def get_whole(self, filepaths: list[str]) -> list[dict]:
        """Return entire copybook as single chunk.
        
        Args:
            filepaths: List of copybook file paths
            
        Returns:
            List of chunk dicts
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
