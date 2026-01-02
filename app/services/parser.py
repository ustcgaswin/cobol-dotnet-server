"""Parser service for orchestrating file parsing."""

import json
import uuid
from dataclasses import dataclass
from pathlib import Path

from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession

from app.config.settings import settings
from app.core.exceptions import ParserNotFoundError
from app.core.parsers import PARSER_REGISTRY, get_parser
from app.core.storage import FileStorage
from app.db.repositories.source_file import SourceFileRepository


@dataclass
class ParseResult:
    """Result of parsing a single file."""
    
    filename: str
    success: bool
    output_path: str | None = None
    error: str | None = None


@dataclass
class ParseSummary:
    """Summary of parsing operation."""
    
    project_id: uuid.UUID
    file_type: str
    total_files: int
    successful: int
    failed: int
    results: list[ParseResult]
    consolidated_path: str | None = None


class ParserService:
    """Service for parsing source files and saving outputs."""
    
    def __init__(self, session: AsyncSession):
        self.repository = SourceFileRepository(session)
        self.storage = FileStorage()
        self.artifacts_path = settings.get_artifacts_path()
    
    def _get_output_dir(self, project_id: uuid.UUID, file_type: str) -> Path:
        """Get output directory for parsed files."""
        return self.artifacts_path / str(project_id) / "parsed_outputs" / file_type
    
    async def parse_project(
        self,
        project_id: uuid.UUID,
        file_types: list[str] | None = None,
    ) -> list[ParseSummary]:
        """Parse all source files for a project.
        
        Args:
            project_id: Project UUID
            file_types: Optional list of file types to parse. 
                       If None, parses all types with registered parsers.
        
        Returns:
            List of ParseSummary for each file type processed
        """
        if file_types is None:
            file_types = list(PARSER_REGISTRY.keys())
        
        summaries = []
        for file_type in file_types:
            try:
                summary = await self.parse_file_type(project_id, file_type)
                summaries.append(summary)
            except ParserNotFoundError:
                logger.warning(f"No parser for file type: {file_type}")
                continue
        
        return summaries
    
    async def parse_file_type(
        self,
        project_id: uuid.UUID,
        file_type: str,
    ) -> ParseSummary:
        """Parse all files of a specific type for a project.
        
        Args:
            project_id: Project UUID
            file_type: Type of files to parse (e.g., "cobol", "copybook")
            
        Returns:
            ParseSummary with results for each file
        """
        # Get parser for this file type
        parser = get_parser(file_type)
        
        # Get all files of this type
        source_files = await self.repository.get_by_project_and_type(
            project_id, file_type
        )
        
        if not source_files:
            logger.info(f"No {file_type} files found for project {project_id}")
            return ParseSummary(
                project_id=project_id,
                file_type=file_type,
                total_files=0,
                successful=0,
                failed=0,
                results=[],
            )
        
        # Prepare output directory
        output_dir = self._get_output_dir(project_id, file_type)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        results: list[ParseResult] = []
        all_parsed: list[dict] = []
        
        for source_file in source_files:
            # Get file path from storage
            file_path = (
                settings.get_storage_path() 
                / str(project_id) 
                / file_type 
                / source_file.filename
            )
            
            try:
                # Parse the file
                parsed_data = parser.parse_file(str(file_path))
                
                # Save individual output
                output_filename = f"{Path(source_file.filename).stem}.json"
                output_path = output_dir / output_filename
                
                with open(output_path, "w", encoding="utf-8") as f:
                    json.dump(parsed_data, f, indent=2, ensure_ascii=False)
                
                logger.info(f"Parsed {source_file.filename} -> {output_path}")
                
                results.append(ParseResult(
                    filename=source_file.filename,
                    success=True,
                    output_path=str(output_path),
                ))
                
                all_parsed.append(parsed_data)
                
            except Exception as e:
                logger.error(f"Failed to parse {source_file.filename}: {e}")
                results.append(ParseResult(
                    filename=source_file.filename,
                    success=False,
                    error=str(e),
                ))
        
        # Save consolidated output if any files were parsed successfully
        consolidated_path = None
        if all_parsed:
            consolidated_file = output_dir / "_consolidated.json"
            with open(consolidated_file, "w", encoding="utf-8") as f:
                json.dump(all_parsed, f, indent=2, ensure_ascii=False)
            consolidated_path = str(consolidated_file)
            logger.info(f"Saved consolidated output: {consolidated_path}")
        
        successful = sum(1 for r in results if r.success)
        
        return ParseSummary(
            project_id=project_id,
            file_type=file_type,
            total_files=len(source_files),
            successful=successful,
            failed=len(source_files) - successful,
            results=results,
            consolidated_path=consolidated_path,
        )
