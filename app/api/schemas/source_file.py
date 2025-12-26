"""Source file API schemas."""

import uuid
from datetime import datetime

from pydantic import BaseModel


class SourceFileResponse(BaseModel):
    """Schema for source file metadata response."""
    
    id: uuid.UUID
    project_id: uuid.UUID
    filename: str
    file_type: str
    size_bytes: int
    uploaded_at: datetime
    
    model_config = {"from_attributes": True}


class UploadedFileInfo(BaseModel):
    """Info for a successfully uploaded file."""
    
    id: uuid.UUID
    filename: str
    file_type: str
    size_bytes: int
    overwritten: bool


class UploadResponse(BaseModel):
    """Response for file upload operation."""
    
    uploaded: list[UploadedFileInfo]
    total_count: int
    overwritten_count: int


class FileSizeViolation(BaseModel):
    """Details of a file size violation."""
    
    filename: str
    size_mb: float
    limit_mb: float


class DeleteSourceFileResponse(BaseModel):
    """Response for delete operation."""
    
    message: str
    id: uuid.UUID
