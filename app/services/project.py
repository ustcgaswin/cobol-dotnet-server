"""Project service for business logic."""

import uuid
from typing import Sequence

from sqlalchemy.ext.asyncio import AsyncSession

from app.db.models.project import Project, ProjectStatus
from app.db.repositories.project import ProjectRepository


class ProjectService:
    """Service layer for Project operations."""
    
    def __init__(self, session: AsyncSession):
        self.repository = ProjectRepository(session)
    
    async def create_project(
        self,
        name: str,
        description: str | None = None,
    ) -> Project:
        """Create a new project with default statuses."""
        project = Project(
            name=name,
            description=description,
            functional_document_status=ProjectStatus.PENDING.value,
            technical_document_status=ProjectStatus.PENDING.value,
            code_migration_status=ProjectStatus.PENDING.value,
        )
        return await self.repository.create(project)
    
    async def get_project(self, project_id: uuid.UUID) -> Project | None:
        """Get project by ID."""
        return await self.repository.get_by_id(project_id)
    
    async def get_all_projects(self) -> Sequence[Project]:
        """Get all projects."""
        return await self.repository.get_all()
    
    async def update_project(
        self,
        project: Project,
        name: str | None = None,
        description: str | None = None,
        functional_document_status: ProjectStatus | None = None,
        technical_document_status: ProjectStatus | None = None,
        code_migration_status: ProjectStatus | None = None,
    ) -> Project:
        """Update project fields."""
        if name is not None:
            project.name = name
        if description is not None:
            project.description = description
        if functional_document_status is not None:
            project.functional_document_status = functional_document_status.value
        if technical_document_status is not None:
            project.technical_document_status = technical_document_status.value
        if code_migration_status is not None:
            project.code_migration_status = code_migration_status.value
        
        return await self.repository.update(project)
    
    async def delete_project(self, project: Project) -> None:
        """Delete a project."""
        await self.repository.delete(project)
