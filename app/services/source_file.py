"""Source file service for business logic."""

import uuid
from dataclasses import dataclass
from datetime import datetime
from typing import Sequence

from fastapi import UploadFile
from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession

from app.config.settings import settings
from app.core.exceptions import (
    FileSizeExceededError,
    InvalidFileTypeError,
    SourceFileNotFoundException,
)
from app.core.storage import FileStorage
from app.db.models.source_file import SourceFile, SourceFileType
from app.db.repositories.source_file import SourceFileRepository


@dataclass
class UploadResult:
    """Result of a single file upload."""
    
    id: uuid.UUID
    filename: str
    file_type: str
    size_bytes: int
    overwritten: bool


class SourceFileService:
    """Service layer for source file operations."""
    
    def __init__(self, session: AsyncSession):
        self.repository = SourceFileRepository(session)
        self.storage = FileStorage()
    
    def _validate_file_type(self, file_type: str) -> None:
        """Validate that file type is supported."""
        valid_types = [t.value for t in SourceFileType]
        if file_type not in valid_types:
            raise InvalidFileTypeError(file_type, valid_types)
    
    async def _validate_files(
        self,
        files: list[UploadFile],
        file_types: list[str],
    ) -> list[tuple[UploadFile, str, bytes]]:
        """Validate all files and their types. Returns list of (file, type, content).
        
        Validates:
        - Number of files matches number of types
        - All file types are valid
        - All file sizes are within limits
        """
        # Validate count matches
        if len(files) != len(file_types):
            raise InvalidFileTypeError(
                "mismatch",
                [f"Number of files ({len(files)}) must match number of file_types ({len(file_types)})"]
            )
        
        # Validate all types first
        valid_types = [t.value for t in SourceFileType]
        invalid_types = [ft for ft in file_types if ft not in valid_types]
        if invalid_types:
            raise InvalidFileTypeError(", ".join(set(invalid_types)), valid_types)
        
        # Read all files and validate sizes
        file_data: list[tuple[UploadFile, str, bytes]] = []
        violations = []
        
        for file, file_type in zip(files, file_types):
            content = await file.read()
            size_bytes = len(content)
            
            limit_bytes = settings.get_file_size_limit_bytes(file_type)
            limit_mb = limit_bytes / (1024 * 1024)
            size_mb = size_bytes / (1024 * 1024)
            
            if size_bytes > limit_bytes:
                violations.append({
                    "filename": file.filename,
                    "file_type": file_type,
                    "size_mb": round(size_mb, 2),
                    "limit_mb": limit_mb,
                })
            
            file_data.append((file, file_type, content))
        
        if violations:
            raise FileSizeExceededError(violations)
        
        return file_data
    
    async def upload_files(
        self,
        project_id: uuid.UUID,
        files: list[UploadFile],
        file_types: list[str],
    ) -> list[UploadResult]:
        """Upload multiple files to a project with individual types.
        
        All-or-nothing: validates all files before saving any.
        Overwrites duplicates with logging.
        """
        # Validate all files first
        file_data = await self._validate_files(files, file_types)
        
        results = []
        
        for file, file_type, content in file_data:
            filename = file.filename
            size_bytes = len(content)
            
            # Check for existing file (duplicate)
            existing = await self.repository.get_by_project_and_filename(
                project_id, file_type, filename
            )
            overwritten = existing is not None
            
            if overwritten:
                logger.warning(
                    f"Overwriting existing file: {filename} "
                    f"(project={project_id}, type={file_type})"
                )
                # Update existing record
                existing.size_bytes = size_bytes
                existing.uploaded_at = datetime.utcnow()
                source_file = await self.repository.update(existing)
            else:
                # Create new record
                source_file = SourceFile(
                    project_id=project_id,
                    filename=filename,
                    file_type=file_type,
                    size_bytes=size_bytes,
                )
                source_file = await self.repository.create(source_file)
                logger.info(
                    f"Uploaded new file: {filename} "
                    f"(project={project_id}, type={file_type}, size={size_bytes})"
                )
            
            # Save to storage
            await self.storage.save(project_id, file_type, filename, content)
            
            results.append(UploadResult(
                id=source_file.id,
                filename=filename,
                file_type=file_type,
                size_bytes=size_bytes,
                overwritten=overwritten,
            ))
        
        return results
    
    async def get_file(self, file_id: uuid.UUID) -> SourceFile | None:
        """Get source file by ID."""
        return await self.repository.get_by_id(file_id)
    
    async def get_project_files(self, project_id: uuid.UUID) -> Sequence[SourceFile]:
        """Get all source files for a project."""
        return await self.repository.get_by_project(project_id)
    
    async def delete_file(self, file_id: uuid.UUID) -> None:
        """Delete a source file."""
        source_file = await self.repository.get_by_id(file_id)
        if not source_file:
            raise SourceFileNotFoundException(str(file_id))
        
        project_id = source_file.project_id
        file_type = source_file.file_type
        
        # Delete from storage
        await self.storage.delete(
            project_id,
            file_type,
            source_file.filename,
        )
        
        # Delete from database
        await self.repository.delete(source_file)
        logger.info(f"Deleted source file: {source_file.filename} (id={file_id})")
        
        # Cleanup empty folders
        await self.storage.cleanup_empty_folders(project_id, file_type)


    async def read_file_content(self, file_id: uuid.UUID) -> tuple[bytes, str]:
        """
        Fetch the actual content of a file from storage.
        Returns: (content_bytes, filename)
        """
        # 1. Get DB Record to find out where the file is
        source_file = await self.repository.get_by_id(file_id)
        if not source_file:
            raise SourceFileNotFoundException(str(file_id))
            
        # 2. Fetch bytes from physical storage
        # Note: self.storage.get takes (project_id, file_type, filename)
        try:
            content = await self.storage.get(
                source_file.project_id,
                source_file.file_type,
                source_file.filename
            )
            return content, source_file.filename
        except FileNotFoundError:
            # Handle case where DB record exists but file is missing on disk
            raise SourceFileNotFoundException(f"Physical file missing for {file_id}")

