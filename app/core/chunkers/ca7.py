import re
from pathlib import Path
from typing import Optional, List

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class Ca7Chunker(BaseChunker):
    """Chunker for CA-7 LJOB reports.
    
    Splits large CA-7 report files into individual job definitions.
    Mirrors the logic in CA7Parser to ensure consistency.
    """
    
    SUPPORTED_EXTENSIONS = [".ca7", ".txt", ".ljob"]
    
    # Matches the specific splitter used in CA7Parser
    # This splits on 'JOB:' but ignores 'DEP-JOB:'
    JOB_SPLIT_PATTERN = re.compile(
        r'(?im)^(?:(?!\s*DEP-JOB:).)*?JOB:\s+', 
        re.MULTILINE
    )
    
    # Pattern to extract job name from the chunk for metadata naming
    JOB_NAME_PATTERN = re.compile(r'([A-Z0-9#@$]+)', re.IGNORECASE)

    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Chunk CA-7 reports into individual job blocks.
        
        Args:
            filepaths: List of file paths to CA-7 reports
            
        Returns:
            List of chunk dicts with 'content', 'type', 'name', 'source'
        """
        all_chunks = []
        
        for filepath in filepaths:
            try:
                chunks = self._chunk_file(filepath)
                all_chunks.extend(chunks)
            except Exception as e:
                logger.error(f"Failed to chunk CA-7 file {filepath}: {e}")
                raise ChunkError(f"Failed to chunk {filepath}: {e}") from e
        
        return all_chunks

    def get_whole(self, filepaths: list[str]) -> list[dict]:
        """Return entire CA-7 file as single chunk."""
        chunks = []
        for filepath in filepaths:
            try:
                content = Path(filepath).read_text(encoding='utf-8', errors='replace')
                processed = self._preprocess_source(content)
                chunks.append({
                    'content': processed,
                    'type': 'whole',
                    'name': Path(filepath).name,
                    'source': filepath,
                })
            except Exception as e:
                logger.error(f"Failed to read {filepath}: {e}")
                raise ChunkError(f"Failed to read {filepath}: {e}") from e
        return chunks

    def _chunk_file(self, filepath: str) -> list[dict]:
        """Split a CA-7 file into job blocks."""
        content = Path(filepath).read_text(encoding='utf-8', errors='replace')
        content = self._preprocess_source(content)
        
        # Split by JOB: pattern
        # The re.split will remove the 'JOB: ' part if not using a capture group,
        # so we split and then reconstruct if necessary.
        blocks = self.JOB_SPLIT_PATTERN.split(content)
        
        chunks = []
        filename = Path(filepath).name
        
        for block in blocks:
            cleaned_block = block.strip()
            if not cleaned_block:
                continue
                
            # Re-attach 'JOB: ' because the split removes it
            full_content = "JOB: " + cleaned_block
            
            # Extract job name for the chunk metadata
            job_name = self._extract_job_name(cleaned_block) or "UNKNOWN_JOB"
            
            # Filter out wildcard headers (like JOB: FIN*) as per Parser logic
            if '*' in job_name:
                continue

            chunks.append({
                'content': full_content,
                'type': 'job_definition',
                'name': job_name,
                'source': filepath
            })
            
        logger.info(f"Chunked {filename} into {len(chunks)} job blocks")
        return chunks

    def _preprocess_source(self, content: str) -> str:
        """Clean CA-7 report noise.
        
        1. Removes report headers (DATE, TIME, PAGE).
        2. Removes LIST: LJOB metadata.
        3. Preserves internal spacing for regex compatibility.
        """
        lines = content.splitlines()
        processed = []
        
        # Pattern to identify report noise
        noise_pattern = re.compile(r'^(DATE:|LIST:|TIME:|PAGE:|--+)', re.IGNORECASE)
        
        for line in lines:
            # Skip report headers and separator lines
            if noise_pattern.match(line.strip()):
                continue
            
            processed.append(line)
            
        return '\n'.join(processed)

    def _extract_job_name(self, block_text: str) -> Optional[str]:
        """Helper to find the job name in a block of text."""
        # Typically the job name is the first word in the first line of the block
        # after 'JOB: ' has been removed.
        match = self.JOB_NAME_PATTERN.search(block_text)
        return match.group(1) if match else None