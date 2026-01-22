"""Project repository for database operations."""

import uuid
from typing import Sequence, Tuple

from sqlalchemy import select,func
from sqlalchemy.ext.asyncio import AsyncSession

from app.db.models.project import Project
from app.db.models.source_file import SourceFile 

class ProjectRepository:
    """Repository for Project CRUD operations."""
    
    def __init__(self, session: AsyncSession):
        self.session = session
    
    async def create(self, project: Project) -> Project:
        """Create a new project."""
        self.session.add(project)
        await self.session.flush()
        await self.session.refresh(project)
        return project
    
    # REPLACE get_by_id WITH THIS
    async def get_by_id(self, id: uuid.UUID) -> Tuple[Project, int] | None:
        """Get project by ID with file count."""
        stmt = (
            select(Project, func.count(SourceFile.id))
            .outerjoin(SourceFile, SourceFile.project_id == Project.id)
            .where(Project.id == id)
            .group_by(Project.id)
        )
        result = await self.session.execute(stmt)
        # Returns a tuple (Project, count) or None
        return result.first()

    # REPLACE get_all WITH THIS
    async def get_all(self) -> Sequence[Tuple[Project, int]]:
        """Get all projects with file count."""
        stmt = (
            select(Project, func.count(SourceFile.id))
            .outerjoin(SourceFile, SourceFile.project_id == Project.id)
            .group_by(Project.id)
            .order_by(Project.created_at.desc())
        )
        result = await self.session.execute(stmt)
        return result.all()

    async def update(self, project: Project) -> Project:
        """Update an existing project."""
        await self.session.flush()
        await self.session.refresh(project)
        return project
    
    async def delete(self, project: Project) -> None:
        """Delete a project."""
        await self.session.delete(project)
        await self.session.flush()
