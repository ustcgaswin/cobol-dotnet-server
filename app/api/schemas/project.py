"""Project API schemas."""

import uuid
from datetime import datetime

from pydantic import BaseModel, Field

from app.db.models.project import ProjectStatus


class ProjectCreate(BaseModel):
    """Schema for creating a project."""
    
    name: str = Field(..., min_length=1, max_length=255, description="Project name")
    description: str | None = Field(default=None, description="Project description")


class ProjectUpdate(BaseModel):
    """Schema for updating a project."""
    
    name: str | None = Field(default=None, min_length=1, max_length=255)
    description: str | None = Field(default=None)
    functional_document_status: ProjectStatus | None = Field(default=None)
    technical_document_status: ProjectStatus | None = Field(default=None)
    code_migration_status: ProjectStatus | None = Field(default=None)


class ProjectResponse(BaseModel):
    """Schema for project response."""
    
    id: uuid.UUID
    name: str
    description: str | None
    created_at: datetime
    functional_document_status: str
    technical_document_status: str
    code_migration_status: str

    file_count: int = 0
    
    model_config = {"from_attributes": True}