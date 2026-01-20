"""Dependency Extractor Service for generating dependency graphs from parsed outputs."""

import json
import uuid
from pathlib import Path
from typing import Any, Callable, Dict

from loguru import logger

from app.config.settings import settings
from app.core.exceptions import DependencyExtractionError
from app.db.enums import SourceFileType
from app.services.dependency_extractor.extractors import (
    extract_cobol_dependencies,
    extract_copybook_dependencies,
    extract_jcl_dependencies,
    extract_assembly_dependencies,
    extract_ca7_dependencies,
    extract_pli_dependencies,           
    extract_pli_copybook_dependencies,  
    extract_rexx_dependencies,
)
from app.services.dependency_extractor.generator import generate_dependency_graph_md

# 1. Expected Types
EXPECTED_FILE_TYPES = {
    SourceFileType.COBOL,
    SourceFileType.COPYBOOK,
    SourceFileType.JCL,
    SourceFileType.PROC,
    SourceFileType.PLI,
    SourceFileType.PLI_COPYBOOK,      
    SourceFileType.REXX,
    SourceFileType.ASSEMBLY,
    SourceFileType.CA7,
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

        # 2. Strategy Mapping: Enum -> Extractor Function
        self.strategies: Dict[SourceFileType, Callable[[list], dict]] = {
            SourceFileType.COBOL: extract_cobol_dependencies,
            SourceFileType.COPYBOOK: extract_copybook_dependencies,
            SourceFileType.JCL: extract_jcl_dependencies,
            SourceFileType.ASSEMBLY: extract_assembly_dependencies,
            SourceFileType.CA7: extract_ca7_dependencies,
            SourceFileType.PLI: extract_pli_dependencies,                   
            SourceFileType.PLI_COPYBOOK: extract_pli_copybook_dependencies, 
            SourceFileType.REXX: extract_rexx_dependencies,
        }

        # 3. Default structures (Empty state)
        self.default_deps = {
            SourceFileType.COBOL: {'program_calls': [], 'unresolved_calls': [], 'copybooks': [], 'sql_tables': [], 'file_definitions': [], 'file_io': []},
            SourceFileType.COPYBOOK: {'copybook_to_copybook': []},
            SourceFileType.JCL: {'jcl_program_calls': [], 'jcl_proc_calls': [], 'jcl_includes': [], 'jcl_files': []},
            SourceFileType.ASSEMBLY: {'program_calls': [], 'copybooks': [], 'file_io': [], 'db2_usage': [], 'externals': []},
            SourceFileType.CA7: {'ca7_job_flow': [], 'ca7_dataset_triggers': [], 'ca7_user_requirements': [], 'ca7_nodes': []},
            SourceFileType.PLI: {'program_calls': [], 'unresolved_calls': [], 'copybooks': [], 'sql_tables': [], 'file_definitions': [], 'file_io': []},
            SourceFileType.PLI_COPYBOOK: {'copybook_to_copybook': []},
            SourceFileType.REXX: {  # <--- ADD THIS
                'cobol_calls': [],
                'jcl_submissions': [],
                'dataset_operations': [],
                'tso_utilities': [],
                'environment_vars': []
            },
        }
    
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
            logger.info(f"Found parsed outputs for: {[t.value for t in available_types.keys()]}")
            
            # Calculate missing types using Enums
            missing_types = [
                ft.value for ft in EXPECTED_FILE_TYPES 
                if ft not in available_types
            ]
            
            # Initialize results with defaults
            # We copy to avoid modifying the class-level defaults structure
            results = {k: v.copy() for k, v in self.default_deps.items()}

            # ---------------------------------------------------------
            # STRATEGY LOOP: Replace individual IF blocks
            # ---------------------------------------------------------
            for file_type, extractor_func in self.strategies.items():
                if file_type in available_types:
                    try:
                        data = self._read_consolidated(available_types[file_type])
                        if data:
                            # Run the extraction strategy
                            extracted_deps = extractor_func(data)
                            
                            # Store result
                            results[file_type] = extracted_deps
                            
                            # Generic logging based on dictionary size
                            count = sum(len(v) for v in extracted_deps.values() if isinstance(v, list))
                            logger.info(f"Extracted {count} total dependencies for {file_type.value}")
                    except Exception as e:
                        logger.error(f"Error processing dependencies for {file_type.value}: {e}")
            # ---------------------------------------------------------
                        # Accessing specifically from the results dictionary
            cobol_deps = results[SourceFileType.COBOL]
            copybook_deps = results[SourceFileType.COPYBOOK]
            jcl_deps = results[SourceFileType.JCL]
            assembly_deps = results[SourceFileType.ASSEMBLY]
            ca7_deps = results[SourceFileType.CA7]
            pli_deps = results[SourceFileType.PLI]                  
            pli_copybook_deps = results[SourceFileType.PLI_COPYBOOK] 
            rexx_deps = results[SourceFileType.REXX]

            # Generate Markdown
            markdown_content = generate_dependency_graph_md(
                project_id=str(self.project_id),
                cobol_deps=cobol_deps,
                copybook_deps=copybook_deps,
                jcl_deps=jcl_deps,
                assembly_deps=assembly_deps,
                ca7_deps=ca7_deps,
                pli_deps=pli_deps,                  
                pli_copybook_deps=pli_copybook_deps,
                rexx_deps=rexx_deps,
                missing_file_types=missing_types,
            )
            
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
                'ca7_job_dependencies': len(ca7_deps['ca7_job_flow']),
                'ca7_dataset_triggers': len(ca7_deps['ca7_dataset_triggers']),
                'unresolved_calls': len(cobol_deps['unresolved_calls']),
                'assembly_db2_calls': len(assembly_deps['db2_usage']),
                'rexx_cobol_calls': len(rexx_deps['cobol_calls']), 
                'rexx_jcl_submissions': len(rexx_deps['jcl_submissions']),  
                'rexx_dataset_operations': len(rexx_deps['dataset_operations']), 
                'rexx_tso_utilities': len(rexx_deps['tso_utilities']),
        }
            
            
            return {
                'output_path': str(output_file),
                'relationship_counts': relationship_counts,
                'missing_file_types': missing_types,
            }
            
        except Exception as e:
            logger.error(f"Dependency extraction failed: {e}")
            raise DependencyExtractionError(f"Failed to generate dependency graph: {e}") from e
    
    def _discover_available_types(self) -> dict[SourceFileType, Path]:
        """Discover which file types have parsed outputs using Enums."""
        available = {}
        
        if not self.parsed_outputs_path.exists():
            # This is critical - without parsed outputs we can't do anything
            raise DependencyExtractionError(f"Parsed outputs directory not found: {self.parsed_outputs_path}")
        
        for type_dir in self.parsed_outputs_path.iterdir():
            if type_dir.is_dir():
                try:
                    # Match directory name to Enum
                    file_type = SourceFileType(type_dir.name)
                    consolidated_file = type_dir / "_consolidated.json"
                    if consolidated_file.exists():
                        available[file_type] = consolidated_file
                except ValueError:
                    logger.debug(f"Skipping unknown directory type: {type_dir.name}")
                    continue
        
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
