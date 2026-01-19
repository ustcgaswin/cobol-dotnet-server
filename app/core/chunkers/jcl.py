from pathlib import Path
from loguru import logger
from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError

class JclChunker(BaseChunker):
    """Chunker for JCL (Job Control Language) and PROCs.
    
    This chunker mirrors the logic in JCLParser to ensure consistency 
    between stored chunks and parsed dependencies.
    """
    
    SUPPORTED_EXTENSIONS = [".jcl", ".proc", ".cntl", ".prc"]
    
    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Return entire JCL/PROC as single chunk."""
        return self.get_whole(filepaths)
    
    def get_whole(self, filepaths: list[str]) -> list[dict]:
        """Return entire JCL file as a single chunk after preprocessing."""
        chunks = []
        
        for filepath in filepaths:
            try:
                content = Path(filepath).read_text(encoding='utf-8', errors='replace')
                processed_content = self._preprocess_source(content)
                
                chunks.append({
                    'content': processed_content,
                    'type': 'whole',
                    'name': Path(filepath).name,
                    'source': filepath,
                })
            except Exception as e:
                logger.error(f"Failed to read JCL {filepath}: {e}")
                raise ChunkError(f"Failed to read {filepath}: {e}") from e
        
        return chunks
    
    def _preprocess_source(self, content: str) -> str:
        """Clean JCL source code to match Parser logic.
        
        1. Strips sequence numbers (cols 73-80).
        2. Removes comments (//*).
        3. Only keeps valid JCL statements (//).
        """
        lines = content.splitlines()
        processed = []
        
        for line in lines:
            line = line[:72]

            if line.startswith('//*') or not line.startswith('//'):
                continue

            processed.append(line.rstrip())
                
        return '\n'.join(processed)