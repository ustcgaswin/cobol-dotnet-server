"""Data Transfer Objects (DTOs) for the Documentation Engine."""

from typing import List, Optional, Any, Dict
from pydantic import BaseModel, Field

class DependencyEdge(BaseModel):
    source: str
    target: str
    type: str
    line: Optional[int] = None

class FileSummary(BaseModel):
    """Unified model for file summaries. 
    Using Optional fields allows this single model to handle COBOL, JCL, etc.
    """
    filename: str
    file_type: str = Field(alias="type")
    purpose: str = "No purpose provided."
    
    # Logic & Code Fields (COBOL/PLI/Assembly/REXX)
    functionalities: List[str] = Field(default_factory=list)
    key_operations: List[str] = Field(default_factory=list)
    register_usage: List[str] = Field(default_factory=list)
    
    # Workflow Fields (JCL/CA7)
    steps: List[str] = Field(default_factory=list)
    main_datasets: List[str] = Field(default_factory=list)
    dependencies_triggers: List[str] = Field(default_factory=list) # CA7
    
    # Data Fields (Copybook/DCLGEN/SQL/Bind)
    table_name: Optional[str] = None
    table_structure: List[str] = Field(default_factory=list)
    key_fields: List[str] = Field(default_factory=list)
    
    # Config Fields (Parmlib/Bind/REXX Env Vars)
    configuration_areas: List[str] = Field(default_factory=list)
    key_parameters: List[str] = Field(default_factory=list)

    notes: List[str] = Field(default_factory=list)

    class Config:
        populate_by_name = True
        extra = "ignore"

class SystemMetrics(BaseModel):
    total_files: int
    total_lines_of_code: Optional[int] = 0
    files_by_type: Dict[str, int]
    top_complex_modules: List[str]