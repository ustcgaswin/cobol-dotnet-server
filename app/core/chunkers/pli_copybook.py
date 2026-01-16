"""PL/I copybook chunker.

Copybooks (PL/I include files) are typically small and best treated as a
single chunk. This module mirrors CopybookChunker API so higher layers can
reuse the same summarization/extraction flows.
"""

from pathlib import Path

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class PliCopybookChunker(BaseChunker):
    """Chunker for PL/I copybooks/includes (fallback)."""

    SUPPORTED_EXTENSIONS = [".inc", ".mac"]

    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Return the entire copybook as a single chunk (same as get_whole)."""
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
        """Minimal preprocessing for PL/I copybooks.

        We avoid aggressive rewriting; this preserves include directives and
        lets the PL/I parser or LLM handle structure-specific concerns.
        """
        return content.strip('\n')
