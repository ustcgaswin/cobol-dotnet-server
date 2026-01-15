"""Dependency Extractor Service for generating dependency graphs from parsed outputs."""

import json
import uuid
from pathlib import Path
from typing import Any

from loguru import logger

from app.config.settings import settings
from app.services.dependency_extractor.extractors import (
    extract_cobol_dependencies,
    extract_copybook_dependencies,
    extract_jcl_dependencies
)
from app.services.dependency_extractor.generator import generate_dependency_graph_md

EXPECTED_FILE_TYPES = {
    'cobol': 'COBOL programs',
    'copybook': 'Copybooks',
    'jcl': 'JCL jobs',
    'proc': 'JCL procedures',
    'pli': 'PL/I programs',
    'rexx': 'REXX scripts',
}

class DependencyExtractorService:
    """Service for generating dependency graphs from parsed outputs."""
    
    def __init__(self, project_id: uuid.UUID):
        """Initialize the service.
        
        Args:
            project_id: Project UUID
        """
        self.project_id = project_id
        self.parsed_outputs_path = (
            settings.get_artifacts_path() / str(project_id) / "parsed_outputs"
        )
        self.output_path = settings.get_artifacts_path() / str(project_id)
    
    async def generate(self) -> dict:
        """Generate dependency_graph.md for the project.
        
        Returns:
            Dictionary with output path and relationship counts
        """
        logger.info(f"Generating dependency graph for project {self.project_id}")
        
        # Discover available parsed outputs
        available_types = self._discover_available_types()
        logger.info(f"Found parsed outputs for: {list(available_types.keys())}")
        
        # Track missing file types
        missing_types = [
            ft for ft in EXPECTED_FILE_TYPES.keys() 
            if ft not in available_types
        ]
        
        # Extract dependencies from each available type
        cobol_deps = {'program_calls': [], 'unresolved_calls': [], 
                      'copybooks': [], 'sql_tables': [], 
                      'file_definitions': [], 'file_io': []}
        copybook_deps = {'copybook_to_copybook': []}
        jcl_deps = {'jcl_program_calls': [], 'jcl_proc_calls': [], 
                    'jcl_includes': [], 'jcl_files': []}
        
        if 'cobol' in available_types:
            cobol_data = self._read_consolidated(available_types['cobol'])
            if cobol_data:
                cobol_deps = extract_cobol_dependencies(cobol_data)
                logger.info(f"Extracted {len(cobol_deps['program_calls'])} program calls, "
                           f"{len(cobol_deps['copybooks'])} copybook refs")
        
        if 'copybook' in available_types:
            copybook_data = self._read_consolidated(available_types['copybook'])
            if copybook_data:
                copybook_deps = extract_copybook_dependencies(copybook_data)
                logger.info(f"Extracted {len(copybook_deps['copybook_to_copybook'])} nested copybook refs")

        if 'jcl' in available_types:
            jcl_data = self._read_consolidated(available_types['jcl'])
            if jcl_data:
                jcl_deps = extract_jcl_dependencies(jcl_data)
                logger.info(f"Extracted {len(jcl_deps['jcl_program_calls'])} JCL-to-Program calls")
        
        # Generate markdown
        markdown_content = generate_dependency_graph_md(
            project_id=str(self.project_id),
            cobol_deps=cobol_deps,
            copybook_deps=copybook_deps,
            jcl_deps=jcl_deps,
            missing_file_types=missing_types,
        )
        
        # Save to file
        output_file = self.output_path / "dependency_graph.md"
        output_file.parent.mkdir(parents=True, exist_ok=True)
        output_file.write_text(markdown_content, encoding='utf-8')
        
        logger.info(f"Dependency graph saved to {output_file}")
        
        # Calculate counts for response
        relationship_counts = {
            'program_to_program': len(cobol_deps['program_calls']),
            'program_to_copybook': len(cobol_deps['copybooks']),
            'program_to_table': len(cobol_deps['sql_tables']),
            'program_to_file_definition': len(cobol_deps['file_definitions']),
            'program_to_file_io': len(cobol_deps['file_io']),
            'jcl_to_program': len(jcl_deps['jcl_program_calls']),
            'jcl_to_proc': len(jcl_deps['jcl_proc_calls']),
            'jcl_to_file': len(jcl_deps['jcl_files']),
            'copybook_to_copybook': len(copybook_deps['copybook_to_copybook']),
            'unresolved_calls': len(cobol_deps['unresolved_calls']),
        }
        
        return {
            'output_path': str(output_file),
            'relationship_counts': relationship_counts,
            'missing_file_types': missing_types,
        }
    
    def _discover_available_types(self) -> dict[str, Path]:
        """Discover which file types have parsed outputs.
        
        Returns:
            Dictionary mapping file type to consolidated JSON path
        """
        available = {}
        
        if not self.parsed_outputs_path.exists():
            logger.warning(f"Parsed outputs directory not found: {self.parsed_outputs_path}")
            return available
        
        for type_dir in self.parsed_outputs_path.iterdir():
            if type_dir.is_dir():
                consolidated_file = type_dir / "_consolidated.json"
                if consolidated_file.exists():
                    available[type_dir.name] = consolidated_file
        
        return available
    
    def _read_consolidated(self, path: Path) -> list[dict]:
        """Read consolidated JSON file.
        
        Args:
            path: Path to _consolidated.json
            
        Returns:
            List of parsed file dictionaries
        """
        try:
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
                # Handle both list and dict formats
                if isinstance(data, list):
                    return data
                elif isinstance(data, dict):
                    return [data]
                else:
                    logger.warning(f"Unexpected data format in {path}")
                    return []
        except Exception as e:
            logger.error(f"Failed to read {path}: {e}")
            return []
