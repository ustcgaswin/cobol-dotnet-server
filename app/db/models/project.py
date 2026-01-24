"""Project model and status enum."""

import uuid
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING

from sqlalchemy import DateTime, String, Text
from sqlalchemy.dialects.postgresql import UUID
from sqlalchemy.orm import Mapped, mapped_column, relationship

from app.db.base import Base

if TYPE_CHECKING:
    from app.db.models.source_file import SourceFile


class ProjectStatus(str, Enum):
    """Status enum for project phases."""
    
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"


class Project(Base):
    """Project model for tracking COBOL to .NET migration projects."""
    
    __tablename__ = "projects"
    
    id: Mapped[uuid.UUID] = mapped_column(
        UUID(as_uuid=True),
        primary_key=True,
        default=uuid.uuid4,
    )
    name: Mapped[str] = mapped_column(String(255), nullable=False)
    description: Mapped[str | None] = mapped_column(Text, nullable=True)
    created_at: Mapped[datetime] = mapped_column(
        DateTime,
        nullable=False,
        default=datetime.utcnow,
    )
    functional_document_status: Mapped[str] = mapped_column(
        String(20),
        nullable=False,
        default=ProjectStatus.PENDING.value,
    )
    technical_document_status: Mapped[str] = mapped_column(
        String(20),
        nullable=False,
        default=ProjectStatus.PENDING.value,
    )
    code_migration_status: Mapped[str] = mapped_column(
        String(20),
        nullable=False,
        default=ProjectStatus.PENDING.value,
    )
    
    # Relationships
    source_files: Mapped[list["SourceFile"]] = relationship(
        "SourceFile",
        back_populates="project",
        cascade="all, delete-orphan",
    )

