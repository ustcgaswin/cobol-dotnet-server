"""Project repository for database operations."""

import uuid
from typing import Sequence

from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession

from app.db.models.project import Project


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
    
    async def get_by_id(self, project_id: uuid.UUID) -> Project | None:
        """Get project by ID."""
        result = await self.session.execute(
            select(Project).where(Project.id == project_id)
        )
        return result.scalar_one_or_none()
    
    async def get_all(self) -> Sequence[Project]:
        """Get all projects."""
        result = await self.session.execute(
            select(Project).order_by(Project.created_at.desc())
        )
        return result.scalars().all()
    
    async def update(self, project: Project) -> Project:
        """Update an existing project."""
        await self.session.flush()
        await self.session.refresh(project)
        return project
    
    async def delete(self, project: Project) -> None:
        """Delete a project."""
        await self.session.delete(project)
        await self.session.flush()
