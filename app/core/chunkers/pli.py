"""PL/I chunker.

This chunker provides the same public API as other chunkers (chunk, get_whole)
but keeps the chunking logic intentionally simple for a safe fallback. The goal
is to provide consistent function signatures and preprocessing hooks so the
rest of the pipeline can call a PL/I chunker just like COBOL or copybook.
"""

from pathlib import Path

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class PliChunker(BaseChunker):
    """Basic PL/I chunker (fallback implementation).

    SUPPORTED_EXTENSIONS lists the common PL/I program file extensions.
    The chunk() method currently returns the entire file as a single chunk
    (same behavior as CopybookChunker.get_whole) to keep behavior predictable.
    """

    SUPPORTED_EXTENSIONS = [".pli", ".pl1"]

    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Chunk PL/I files. Fallback: return whole file(s) as single chunks."""
        return self.get_whole(filepaths)

    def get_whole(self, filepaths: list[str]) -> list[dict]:
        """Return entire PL/I file(s) as single chunk dicts."""
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
        """Minimal preprocessing for PL/I sources.

        Current behavior:
        - Preserve content encoding replacements
        - Strip common leading/trailing whitespace
        - Keep internal structure untouched (parser will handle statements)
        """
        # For PL/I, we keep things simple; parsers handle statement tokenization.
        return content.strip('\n')
