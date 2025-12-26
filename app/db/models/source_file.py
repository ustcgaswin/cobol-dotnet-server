"""SourceFile model and file type enum."""

import uuid
from datetime import datetime
from enum import Enum

from sqlalchemy import DateTime, ForeignKey, Integer, String
from sqlalchemy.dialects.postgresql import UUID
from sqlalchemy.orm import Mapped, mapped_column, relationship

from app.db.base import Base


class SourceFileType(str, Enum):
    """Supported source file types."""
    
    COBOL = "cobol"
    COPYBOOK = "copybook"
    JCL = "jcl"
    REXX = "rexx"
    CATPROC = "catproc"
    PROC = "proc"
    PLI = "pli"


class SourceFile(Base):
    """Model for tracking uploaded source files."""
    
    __tablename__ = "source_files"
    
    id: Mapped[uuid.UUID] = mapped_column(
        UUID(as_uuid=True),
        primary_key=True,
        default=uuid.uuid4,
    )
    project_id: Mapped[uuid.UUID] = mapped_column(
        UUID(as_uuid=True),
        ForeignKey("projects.id", ondelete="CASCADE"),
        nullable=False,
        index=True,
    )
    filename: Mapped[str] = mapped_column(String(255), nullable=False)
    file_type: Mapped[str] = mapped_column(String(20), nullable=False)
    size_bytes: Mapped[int] = mapped_column(Integer, nullable=False)
    uploaded_at: Mapped[datetime] = mapped_column(
        DateTime,
        nullable=False,
        default=datetime.utcnow,
    )
    
    # Relationship to Project
    project = relationship("Project", back_populates="source_files")
