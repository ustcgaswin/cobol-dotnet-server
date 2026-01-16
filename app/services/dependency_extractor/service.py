# """Dependency Extractor Service for generating dependency graphs from parsed outputs."""

# import json
# import uuid
# from pathlib import Path
# from typing import Any

# from loguru import logger

# from app.config.settings import settings
# from app.core.exceptions import DependencyExtractionError
# from app.services.dependency_extractor.extractors import (
#     extract_cobol_dependencies,
#     extract_copybook_dependencies,
#     extract_jcl_dependencies,
#     extract_assembly_dependencies,
# )
# from app.services.dependency_extractor.generator import generate_dependency_graph_md

# EXPECTED_FILE_TYPES = {
#     'cobol': 'COBOL programs',
#     'copybook': 'Copybooks',
#     'jcl': 'JCL jobs',
#     'proc': 'JCL procedures',
#     'pli': 'PL/I programs',
#     'rexx': 'REXX scripts',
# }

# class DependencyExtractorService:
#     """Service for generating dependency graphs from parsed outputs."""
    
#     def __init__(self, project_id: uuid.UUID):
#         """Initialize the service.
        
#         Args:
#             project_id: Project UUID
#         """
#         self.project_id = project_id
#         self.parsed_outputs_path = (
#             settings.get_artifacts_path() / str(project_id) / "parsed_outputs"
#         )
#         self.output_path = settings.get_artifacts_path() / str(project_id)
    
#     async def generate(self) -> dict:
#         """Generate dependency_graph.md for the project.
        
#         Returns:
#             Dictionary with output path and relationship counts
            
#         Raises:
#             DependencyExtractionError: If generation fails
#         """
#         logger.info(f"Generating dependency graph for project {self.project_id}")
        
#         try:
#             # Discover available parsed outputs
#             available_types = self._discover_available_types()
#             logger.info(f"Found parsed outputs for: {list(available_types.keys())}")
            
#             # Track missing file types
#             missing_types = [
#                 ft for ft in EXPECTED_FILE_TYPES.keys() 
#                 if ft not in available_types
#             ]
            
#             # Extract dependencies from each available type
#             cobol_deps = {'program_calls': [], 'unresolved_calls': [], 
#                           'copybooks': [], 'sql_tables': [], 
#                           'file_definitions': [], 'file_io': []}
#             copybook_deps = {'copybook_to_copybook': []}
#             jcl_deps = {'jcl_program_calls': [], 'jcl_proc_calls': [], 
#                         'jcl_includes': [], 'jcl_files': []}
#             assembly_deps = {'program_calls': [], 'copybooks': [], 'file_io': [], 
#                      'db2_usage': [], 'externals': []}

#             if 'cobol' in available_types:
#                 cobol_data = self._read_consolidated(available_types['cobol'])
#                 if cobol_data:
#                     cobol_deps = extract_cobol_dependencies(cobol_data)
#                     logger.info(f"Extracted {len(cobol_deps['program_calls'])} program calls, "
#                                f"{len(cobol_deps['copybooks'])} copybook refs")
            
#             if 'copybook' in available_types:
#                 copybook_data = self._read_consolidated(available_types['copybook'])
#                 if copybook_data:
#                     copybook_deps = extract_copybook_dependencies(copybook_data)
#                     logger.info(f"Extracted {len(copybook_deps['copybook_to_copybook'])} nested copybook refs")

#             if 'jcl' in available_types:
#                 jcl_data = self._read_consolidated(available_types['jcl'])
#                 if jcl_data:
#                     jcl_deps = extract_jcl_dependencies(jcl_data)
#                     logger.info(f"Extracted {len(jcl_deps['jcl_program_calls'])} JCL program calls")
            
#             # Generate markdown
#             markdown_content = generate_dependency_graph_md(
#                 project_id=str(self.project_id),
#                 cobol_deps=cobol_deps,
#                 copybook_deps=copybook_deps,
#                 jcl_deps=jcl_deps,
#                 missing_file_types=missing_types,
#             )
            
#             # Save to file
#             output_file = self.output_path / "dependency_graph.md"
#             output_file.parent.mkdir(parents=True, exist_ok=True)
#             output_file.write_text(markdown_content, encoding='utf-8')
            
#             logger.info(f"Dependency graph saved to {output_file}")
            
#             # Calculate counts for response
#             relationship_counts = {
#                 'program_to_program': len(cobol_deps['program_calls']),
#                 'program_to_copybook': len(cobol_deps['copybooks']),
#                 'program_to_table': len(cobol_deps['sql_tables']),
#                 'program_to_file_definition': len(cobol_deps['file_definitions']),
#                 'program_to_file_io': len(cobol_deps['file_io']),
#                 'copybook_to_copybook': len(copybook_deps['copybook_to_copybook']),
#                 'unresolved_calls': len(cobol_deps['unresolved_calls']),
#             }
            
#             return {
#                 'output_path': str(output_file),
#                 'relationship_counts': relationship_counts,
#                 'missing_file_types': missing_types,
#             }
            
#         except Exception as e:
#             logger.error(f"Dependency extraction failed: {e}")
#             raise DependencyExtractionError(f"Failed to generate dependency graph: {e}") from e
    
#     def _discover_available_types(self) -> dict[str, Path]:
#         """Discover which file types have parsed outputs."""
#         available = {}
        
#         if not self.parsed_outputs_path.exists():
#             # This is critical - without parsed outputs we can't do anything
#             raise DependencyExtractionError(f"Parsed outputs directory not found: {self.parsed_outputs_path}")
        
#         for type_dir in self.parsed_outputs_path.iterdir():
#             if type_dir.is_dir():
#                 consolidated_file = type_dir / "_consolidated.json"
#                 if consolidated_file.exists():
#                     available[type_dir.name] = consolidated_file
        
#         return available
    
#     def _read_consolidated(self, path: Path) -> list[dict]:
#         """Read consolidated JSON file."""
#         try:
#             with open(path, 'r', encoding='utf-8') as f:
#                 data = json.load(f)
#                 # Handle both list and dict formats
#                 if isinstance(data, list):
#                     return data
#                 elif isinstance(data, dict):
#                     return [data]
#                 else:
#                     logger.warning(f"Unexpected data format in {path}")
#                     return []
#         except Exception as e:
#             logger.error(f"Failed to read {path}: {e}")
#             raise DependencyExtractionError(f"Failed to read parsed output {path}: {e}") from e




































"""Dependency Extractor Service for generating dependency graphs from parsed outputs."""

import json
import uuid
from pathlib import Path
from typing import Any

from loguru import logger

from app.config.settings import settings
from app.core.exceptions import DependencyExtractionError
from app.services.dependency_extractor.extractors import (
    extract_cobol_dependencies,
    extract_copybook_dependencies,
    extract_jcl_dependencies,
    extract_assembly_dependencies,
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
            
        Raises:
            DependencyExtractionError: If generation fails
        """
        logger.info(f"Generating dependency graph for project {self.project_id}")
        
        try:
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
            assembly_deps = {'program_calls': [], 'copybooks': [], 'file_io': [], 
                     'db2_usage': [], 'externals': []}

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
                    logger.info(f"Extracted {len(jcl_deps['jcl_program_calls'])} JCL program calls")
            
            if 'assembly' in available_types:
                assembly_data = self._read_consolidated(available_types['assembly'])
                if assembly_data:
                    assembly_deps = extract_assembly_dependencies(assembly_data)
                    logger.info(f"Extracted {len(assembly_deps['program_calls'])} Assembly program calls")

            # UPDATE THIS: Pass assembly_deps to the generator
            markdown_content = generate_dependency_graph_md(
                project_id=str(self.project_id),
                cobol_deps=cobol_deps,
                copybook_deps=copybook_deps,
                jcl_deps=jcl_deps,
                assembly_deps=assembly_deps, # <-- New parameter
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
                'copybook_to_copybook': len(copybook_deps['copybook_to_copybook']),
                'unresolved_calls': len(cobol_deps['unresolved_calls']),
                'assembly_db2_calls': len(assembly_deps['db2_usage']),
            }
            
            return {
                'output_path': str(output_file),
                'relationship_counts': relationship_counts,
                'missing_file_types': missing_types,
            }
            
        except Exception as e:
            logger.error(f"Dependency extraction failed: {e}")
            raise DependencyExtractionError(f"Failed to generate dependency graph: {e}") from e
    
    def _discover_available_types(self) -> dict[str, Path]:
        """Discover which file types have parsed outputs."""
        available = {}
        
        if not self.parsed_outputs_path.exists():
            # This is critical - without parsed outputs we can't do anything
            raise DependencyExtractionError(f"Parsed outputs directory not found: {self.parsed_outputs_path}")
        
        for type_dir in self.parsed_outputs_path.iterdir():
            if type_dir.is_dir():
                consolidated_file = type_dir / "_consolidated.json"
                if consolidated_file.exists():
                    available[type_dir.name] = consolidated_file
        
        return available
    
    def _read_consolidated(self, path: Path) -> list[dict]:
        """Read consolidated JSON file."""
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
            raise DependencyExtractionError(f"Failed to read parsed output {path}: {e}") from e
