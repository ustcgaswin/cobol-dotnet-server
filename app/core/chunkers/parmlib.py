"""Parmlib chunker for mainframe parameter library member chunking.

Parmlib files contain system initialization parameters, typically organized as:
- Comment blocks (headers, descriptions)
- Parameter groups (related settings)
- Individual parameter statements

Chunking strategy:
- Group by logical parameter sections (delimited by comment headers)
- Keep related parameters together
- Preserve comment context
"""

import re
from pathlib import Path
from typing import Optional

from loguru import logger

from app.core.chunkers.base import BaseChunker
from app.core.exceptions import ChunkError


class ParmlibChunker(BaseChunker):
    """Chunks parmlib members at logical parameter group boundaries.
    
    Recognizes common parmlib patterns:
    - Section headers (comment blocks with asterisks)
    - Parameter groups (related NAME=VALUE pairs)
    - Subsystem-specific syntax (IEASYSxx, PROGxx, etc.)
    """
    
    SUPPORTED_EXTENSIONS = [".txt", ".parm", ".prm", ".parmlib"]
    
    # Comment line patterns
    COMMENT_PATTERN = re.compile(r'^\s*\*.*$')
    HEADER_COMMENT_PATTERN = re.compile(r'^\s*\*{3,}.*$')  # Multiple asterisks indicate headers
    
    # Parameter patterns
    PARAM_PATTERN = re.compile(
        r'^[\s]*([A-Z][A-Z0-9_]*)\s*=\s*(.+)$',
        re.IGNORECASE
    )
    
    # Common parmlib section keywords
    SECTION_KEYWORDS = [
        'SYSTEM', 'IPL', 'SUBSYSTEM', 'DATASET', 'DEVICE', 'STORAGE',
        'PROGRAM', 'LIBRARY', 'EXIT', 'SMF', 'JES', 'CATALOG',
        'SECURITY', 'NETWORK', 'VTAM', 'CICS', 'DB2', 'IMS',
        'ALLOCATION', 'SCHEDULER', 'CONSOLE', 'OPERATOR'
    ]
    
    def chunk(self, filepaths: list[str]) -> list[dict]:
        """Chunk parmlib files at logical parameter group boundaries.
        
        Args:
            filepaths: List of parmlib file paths
            
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
            filepaths: List of parmlib file paths
            
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
        """Chunk a single parmlib file by parameter groups."""
        content = Path(filepath).read_text(encoding='utf-8', errors='replace')
        content = self._preprocess_source(content)
        lines = content.split('\n')
        
        chunks = []
        filename = Path(filepath).name
        
        # Find section boundaries
        sections = self._find_sections(lines)
        
        if not sections:
            # No clear sections, try to chunk by parameter density
            logger.info(f"No clear sections in {filepath}, chunking by parameter groups")
            sections = self._chunk_by_parameter_groups(lines)
        
        if not sections:
            # Still no sections, return whole file
            logger.warning(f"Could not identify sections in {filepath}, returning as single chunk")
            return [{
                'content': content,
                'type': 'whole',
                'name': filename,
                'source': filepath,
            }]
        
        # Create chunks from sections
        for i, section in enumerate(sections):
            start_line = section['start']
            end_line = sections[i + 1]['start'] if i + 1 < len(sections) else len(lines)
            
            section_content = '\n'.join(lines[start_line:end_line])
            
            # Skip empty sections
            if not section_content.strip():
                continue
            
            chunks.append({
                'content': section_content,
                'type': section['type'],
                'name': section['name'],
                'source': filepath,
            })
        
        logger.info(f"Chunked {filepath} into {len(chunks)} chunks")
        return chunks
    
    def _preprocess_source(self, content: str) -> str:
        """Normalize parmlib content.
        
        - Remove trailing whitespace
        - Normalize line continuations (if any)
        - Preserve comments for context
        """
        lines = content.split('\n')
        processed = []
        
        for line in lines:
            # Remove trailing whitespace but preserve leading indent
            line = line.rstrip()
            
            # Handle continuation lines (lines ending with comma or hyphen)
            # For now, just preserve them as-is; LLM can understand context
            processed.append(line)
        
        return '\n'.join(processed)
    
    def _find_sections(self, lines: list[str]) -> list[dict]:
        """Find logical sections based on comment headers.
        
        Sections are identified by:
        1. Major comment headers (*** SECTION NAME ***)
        2. Blank lines followed by comment blocks
        3. Parameter keyword patterns
        """
        sections = []
        
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            
            # Check for header comment (section marker)
            if self.HEADER_COMMENT_PATTERN.match(lines[i]):
                section_name = self._extract_section_name(lines[i])
                sections.append({
                    'name': section_name,
                    'start': i,
                    'type': 'parameter_group',
                })
                i += 1
                continue
            
            # Check for section keyword at start of comment block
            if line.startswith('*') and any(keyword in line.upper() for keyword in self.SECTION_KEYWORDS):
                section_name = self._extract_section_name(lines[i])
                sections.append({
                    'name': section_name,
                    'start': i,
                    'type': 'parameter_group',
                })
                i += 1
                continue
            
            i += 1
        
        return sections
    
    def _extract_section_name(self, comment_line: str) -> str:
        """Extract meaningful section name from comment line."""
        # Remove leading asterisks and whitespace
        clean = comment_line.lstrip('* \t').rstrip('* \t')
        
        if not clean:
            return "Parameter Group"
        
        # Truncate if too long
        if len(clean) > 50:
            clean = clean[:47] + "..."
        
        return clean
    
    def _chunk_by_parameter_groups(self, lines: list[str]) -> list[dict]:
        """Fallback chunking by identifying parameter density patterns.
        
        Groups consecutive parameter statements together, separated by
        comment blocks or blank lines.
        """
        sections = []
        current_group_start = None
        current_group_params = []
        blank_line_count = 0
        
        for i, line in enumerate(lines):
            stripped = line.strip()
            
            # Track blank lines
            if not stripped:
                blank_line_count += 1
                continue
            
            # Check if this is a parameter line
            if self.PARAM_PATTERN.match(line):
                # Start new group if needed
                if current_group_start is None:
                    current_group_start = i
                    current_group_params = []
                
                # Extract parameter name
                match = self.PARAM_PATTERN.match(line)
                if match:
                    current_group_params.append(match.group(1))
                
                blank_line_count = 0
            
            # Check if this is a comment (potential group separator)
            elif self.COMMENT_PATTERN.match(line):
                # If we have accumulated parameters and hit 2+ blank lines before this comment,
                # close the current group
                if current_group_start is not None and blank_line_count >= 2:
                    # Save current group
                    group_name = self._generate_group_name(current_group_params)
                    sections.append({
                        'name': group_name,
                        'start': current_group_start,
                        'type': 'parameter_group',
                    })
                    current_group_start = None
                    current_group_params = []
                
                blank_line_count = 0
        
        # Save final group if exists
        if current_group_start is not None:
            group_name = self._generate_group_name(current_group_params)
            sections.append({
                'name': group_name,
                'start': current_group_start,
                'type': 'parameter_group',
            })
        
        return sections
    
    def _generate_group_name(self, param_names: list[str]) -> str:
        """Generate descriptive name for a parameter group."""
        if not param_names:
            return "Parameter Group"
        
        # Take first 1-3 parameter names
        sample = param_names[:3]
        
        if len(sample) == 1:
            return f"{sample[0]} Configuration"
        elif len(sample) == 2:
            return f"{sample[0]} and {sample[1]} Settings"
        else:
            return f"{sample[0]}, {sample[1]}, and Others"