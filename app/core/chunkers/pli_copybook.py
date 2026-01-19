"""PL/I copybook chunker."""

import re
from pathlib import Path

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class PliCopybookChunker(BaseChunker):
    """Chunker for PL/I copybooks (Includes).
    
    Treats the copybook as a single chunk but performs cleaning 
    (comment stripping) to optimize LLM context usage.
    """

    SUPPORTED_EXTENSIONS = [".inc", ".mac"]
    COMMENT_PATTERN = re.compile(r'/\*.*?\*/', re.DOTALL)

    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Return the entire copybook as a single chunk."""
        return self.get_whole(filepaths)

    def get_whole(self, filepaths: list[str]) -> list[dict]:
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
        """Remove PL/I comments to clean up declarations."""
        # Remove block comments
        content = self.COMMENT_PATTERN.sub(' ', content)
        # Squeeze extra whitespace
        return re.sub(r'\n\s*\n', '\n', content).strip()