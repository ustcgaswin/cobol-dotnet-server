"""SourceFile repository for database operations."""

import uuid
from typing import Sequence

from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession

from app.db.models.source_file import SourceFile


class SourceFileRepository:
    """Repository for SourceFile CRUD operations."""
    
    def __init__(self, session: AsyncSession):
        self.session = session
    
    async def create(self, source_file: SourceFile) -> SourceFile:
        """Create a new source file record."""
        self.session.add(source_file)
        await self.session.flush()
        await self.session.refresh(source_file)
        return source_file
    
    async def get_by_id(self, file_id: uuid.UUID) -> SourceFile | None:
        """Get source file by ID."""
        result = await self.session.execute(
            select(SourceFile).where(SourceFile.id == file_id)
        )
        return result.scalar_one_or_none()
    
    async def get_by_project(self, project_id: uuid.UUID) -> Sequence[SourceFile]:
        """Get all source files for a project."""
        result = await self.session.execute(
            select(SourceFile)
            .where(SourceFile.project_id == project_id)
            .order_by(SourceFile.file_type, SourceFile.filename)
        )
        return result.scalars().all()
    
    async def get_by_project_and_filename(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
    ) -> SourceFile | None:
        """Get source file by project, type, and filename (for duplicate detection)."""
        result = await self.session.execute(
            select(SourceFile).where(
                SourceFile.project_id == project_id,
                SourceFile.file_type == file_type,
                SourceFile.filename == filename,
            )
        )
        return result.scalar_one_or_none()
    
    async def update(self, source_file: SourceFile) -> SourceFile:
        """Update an existing source file record."""
        await self.session.flush()
        await self.session.refresh(source_file)
        return source_file
    
    async def delete(self, source_file: SourceFile) -> None:
        """Delete a source file record."""
        await self.session.delete(source_file)
        await self.session.flush()
    
    async def get_by_project_and_type(
        self,
        project_id: uuid.UUID,
        file_type: str,
    ) -> Sequence[SourceFile]:
        """Get all source files for a project of a specific type."""
        result = await self.session.execute(
            select(SourceFile).where(
                SourceFile.project_id == project_id,
                SourceFile.file_type == file_type,
            ).order_by(SourceFile.filename)
        )
        return result.scalars().all()

