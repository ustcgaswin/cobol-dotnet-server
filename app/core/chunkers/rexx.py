"""REXX chunker."""

from pathlib import Path

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class RexxChunker(BaseChunker):
    """Chunker for REXX scripts.
    
    REXX (Restructured Extended Executor) scripts are procedural scripting
    language files commonly used in mainframe environments for automation,
    batch processing, and system utilities.
    
    REXX scripts are typically small to medium-sized and can be treated as
    single chunks. This chunker handles preprocessing to clean up mainframe-
    specific formatting.
    """
    
    SUPPORTED_EXTENSIONS = [".rexx", ".rex", ".exec", ".rx"]
    
    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Return entire REXX script as single chunk.
        
        Args:
            filepaths: List of REXX file paths
            
        Returns:
            List of chunk dicts
        """
        # For REXX scripts, chunk() behaves same as get_whole()
        return self.get_whole(filepaths)
    
    def get_whole(self, filepaths: list[str]) -> list[dict]:
        """Return entire REXX script as single chunk.
        
        Args:
            filepaths: List of REXX file paths
            
        Returns:
            List of chunk dicts containing:
                - content: Preprocessed REXX script content
                - type: Always 'whole'
                - name: Filename
                - source: Full file path
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
        """Preprocess REXX source code.
        
        Handles mainframe-specific formatting:
        - Strips sequence numbers (columns 1-8) if present
        - Preserves REXX comments (/* */ and --)
        - Removes trailing whitespace
        - Handles continuation characters
        """
        lines = content.split('\n')
        processed = []
        
        # Check if file has sequence numbers (columns 1-8 are numeric)
        has_sequence_numbers = self._has_sequence_numbers(lines)
        
        for line in lines:
            if has_sequence_numbers and len(line) >= 8:
                # Strip columns 1-8 (mainframe sequence numbers)
                line = line[8:]
            
            # Remove trailing whitespace but preserve the line
            line = line.rstrip()
            
            # Keep all lines including comments - REXX comments are part of logic
            # REXX uses /* */ for block comments and -- for line comments
            processed.append(line)
        
        return '\n'.join(processed)
    
    def _has_sequence_numbers(self, lines: list[str]) -> bool:
        """Detect if file has sequence numbers in columns 1-8.
        
        Mainframe REXX files often have sequence numbers in the first 8 columns.
        This method samples the first 50 lines to detect this pattern.
        
        Returns:
            True if sequence numbers are detected, False otherwise
        """
        sample_count = 0
        numeric_count = 0
        
        for line in lines[:50]:  # Check first 50 lines
            if len(line) >= 8:
                sample_count += 1
                # Check if columns 1-8 are numeric or spaces
                prefix = line[:8]
                if prefix.strip() == '' or prefix.strip().isdigit():
                    numeric_count += 1
        
        # If >80% of sampled lines have numeric/blank prefix, likely has seq numbers
        return sample_count > 0 and (numeric_count / sample_count) > 0.8