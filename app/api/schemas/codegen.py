"""Pydantic schemas for codegen API responses."""

from typing import Optional
from pydantic import BaseModel


class CodegenFileNode(BaseModel):
    """A node in the file tree (file or directory)."""
    
    id: Optional[str] = None  # Only files have IDs
    name: str
    type: str  # "file" or "directory"
    children: Optional[list["CodegenFileNode"]] = None  # Only directories have children


class CodegenFileTreeResponse(BaseModel):
    """Response for GET /projects/{project_id}/codegen/local/files."""
    
    project_id: str
    project_name: str
    tree: CodegenFileNode


class CodegenFileContentResponse(BaseModel):
    """Response for GET /projects/{project_id}/codegen/local/files/{file_id}."""
    
    id: str
    name: str
    path: str
    content: str
