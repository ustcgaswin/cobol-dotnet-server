"""Parser service for orchestrating file parsing."""

import asyncio
import json
import uuid
from dataclasses import dataclass, asdict
from datetime import datetime, timezone
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
class FileParseError:
    """Represents a single parsing error or warning."""
    
    filename: str
    error_type: str  # "fatal", "warning", "unrecognized"
    message: str
    line_number: int | None = None
    content: str | None = None  # offending line content


@dataclass
class ParseErrorReport:
    """Complete error report for a file type."""
    
    file_type: str
    project_id: str
    timestamp: str
    total_files: int
    files_with_errors: int
    errors: list[FileParseError]


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
    
    # Maximum concurrent files to process in parallel
    MAX_CONCURRENT_FILES = 5
    
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
        all_errors: list[FileParseError] = []
        files_with_errors: set[str] = set()
        
        # Create semaphore for limiting concurrent file processing
        semaphore = asyncio.Semaphore(self.MAX_CONCURRENT_FILES)
        
        async def process_single_file(source_file) -> tuple[ParseResult, dict | None]:
            """Process a single file with semaphore-controlled concurrency."""
            async with semaphore:
                file_path = (
                    settings.get_storage_path() 
                    / str(project_id) 
                    / file_type 
                    / source_file.filename
                )
                
                try:
                    # Parse the file (sync operation)
                    parsed_data = parser.parse_file(str(file_path))
                    
                    # Augment with LLM-generated metadata
                    if hasattr(parser, 'augment'):
                        parsed_data = await parser.augment(parsed_data)
                    
                    # Save individual output
                    output_filename = f"{Path(source_file.filename).stem}.json"
                    output_path = output_dir / output_filename
                    
                    with open(output_path, "w", encoding="utf-8") as f:
                        json.dump(parsed_data, f, indent=2, ensure_ascii=False)
                    
                    logger.info(
                        f"Parsed and augmented {source_file.filename} -> {output_path}"
                    )
                    
                    return (
                        ParseResult(
                            filename=source_file.filename,
                            success=True,
                            output_path=str(output_path),
                        ),
                        parsed_data,
                    )
                    
                except Exception as e:
                    logger.error(f"Failed to parse {source_file.filename}: {e}")
                    return (
                        ParseResult(
                            filename=source_file.filename,
                            success=False,
                            error=str(e),
                        ),
                        None,
                    )
        
        # Process all files in parallel (limited by semaphore)
        logger.info(
            f"Processing {len(source_files)} {file_type} files "
            f"(max {self.MAX_CONCURRENT_FILES} concurrent)"
        )
        file_results = await asyncio.gather(
            *[process_single_file(sf) for sf in source_files],
            return_exceptions=True,
        )
        
        # Collect results
        for i, result in enumerate(file_results):
            if isinstance(result, Exception):
                # Unexpected exception during gather
                source_file = source_files[i]
                results.append(ParseResult(
                    filename=source_file.filename,
                    success=False,
                    error=str(result),
                ))
                all_errors.append(FileParseError(
                    filename=source_file.filename,
                    error_type="fatal",
                    message=str(result),
                ))
                files_with_errors.add(source_file.filename)
            else:
                parse_result, parsed_data = result
                results.append(parse_result)
                
                if parsed_data:
                    all_parsed.append(parsed_data)
                    # Extract warnings and unrecognized patterns
                    file_errors = self._extract_errors_from_parsed_data(
                        parse_result.filename, parsed_data
                    )
                    if file_errors:
                        all_errors.extend(file_errors)
                        files_with_errors.add(parse_result.filename)
                elif not parse_result.success:
                    all_errors.append(FileParseError(
                        filename=parse_result.filename,
                        error_type="fatal",
                        message=parse_result.error or "Unknown error",
                    ))
                    files_with_errors.add(parse_result.filename)
        
        # Save consolidated output if any files were parsed successfully
        consolidated_path = None
        if all_parsed:
            consolidated_file = output_dir / "_consolidated.json"
            with open(consolidated_file, "w", encoding="utf-8") as f:
                json.dump(all_parsed, f, indent=2, ensure_ascii=False)
            consolidated_path = str(consolidated_file)
            logger.info(f"Saved consolidated output: {consolidated_path}")
        
        # Save parse errors report (always, even if empty)
        self._save_parse_errors(
            output_dir=output_dir,
            project_id=project_id,
            file_type=file_type,
            total_files=len(source_files),
            files_with_errors=len(files_with_errors),
            errors=all_errors,
        )
        
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
    
    def _extract_errors_from_parsed_data(
        self,
        filename: str,
        parsed_data: dict,
    ) -> list[FileParseError]:
        """Extract warnings and unrecognized patterns from parsed output.
        
        Works with COBOL and Copybook parsers that store errors in
        '_warnings' and '_unrecognized' keys.
        """
        errors: list[FileParseError] = []
        
        # Extract warnings (list of strings)
        warnings = parsed_data.get("_warnings", [])
        for warning in warnings:
            errors.append(FileParseError(
                filename=filename,
                error_type="warning",
                message=str(warning),
            ))
        
        # Extract unrecognized patterns (list of dicts with line, content, error)
        unrecognized = parsed_data.get("_unrecognized", [])
        for item in unrecognized:
            errors.append(FileParseError(
                filename=filename,
                error_type="unrecognized",
                message=item.get("error", "Unknown parsing issue"),
                line_number=item.get("line"),
                content=item.get("content"),
            ))
        
        return errors
    
    def _save_parse_errors(
        self,
        output_dir: Path,
        project_id: uuid.UUID,
        file_type: str,
        total_files: int,
        files_with_errors: int,
        errors: list[FileParseError],
    ) -> None:
        """Save parse_errors.json report to the output directory.
        
        Always creates the file, even if there are no errors.
        """
        report = ParseErrorReport(
            file_type=file_type,
            project_id=str(project_id),
            timestamp=datetime.now(timezone.utc).isoformat(),
            total_files=total_files,
            files_with_errors=files_with_errors,
            errors=errors,
        )
        
        # Convert to dict for JSON serialization
        report_dict = {
            "file_type": report.file_type,
            "project_id": report.project_id,
            "timestamp": report.timestamp,
            "total_files": report.total_files,
            "files_with_errors": report.files_with_errors,
            "errors": [asdict(e) for e in report.errors],
        }
        
        error_file = output_dir / "parse_errors.json"
        with open(error_file, "w", encoding="utf-8") as f:
            json.dump(report_dict, f, indent=2, ensure_ascii=False)
        
        logger.info(
            f"Saved parse errors report: {error_file} "
            f"({files_with_errors} files with errors)"
        )

