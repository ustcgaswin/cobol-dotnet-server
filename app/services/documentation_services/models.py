"""Data Transfer Objects (DTOs) for the Documentation Engine."""

from pydantic import BaseModel, Field
import operator
from typing import Annotated, List, TypedDict, Dict, Any, Optional
from langchain_core.messages import BaseMessage
from typing_extensions import Annotated
from langgraph.graph import add_messages

class DependencyEdge(BaseModel):
    source: str
    target: str
    type: str
    line: Optional[int] = None

class FileSummary(BaseModel):
    filename: str
    file_type: str = Field(alias="type")
    
    # Nested structures for Technical Documents
    business_overview: Dict[str, Any] = Field(default_factory=dict)
    technical_analysis: Dict[str, Any] = Field(default_factory=dict)
    
    # Flat fields for Functional Documents (Executive Summary)
    business_purpose: Optional[str] = None
    business_scope: List[str] = Field(default_factory=list)
    process_flow_steps: List[str] = Field(default_factory=list)
    schedule_frequency: Dict[str, Any] = Field(default_factory=dict)
    data_overview: Dict[str, Any] = Field(default_factory=dict)
    business_risks: List[str] = Field(default_factory=list)
    external_interfaces: List[Dict[str, Any]] = Field(default_factory=list)
    ownership: Dict[str, str] = Field(default_factory=dict)

    class Config:
        populate_by_name = True
        extra = "ignore"

class SystemMetrics(BaseModel):
    total_files: int
    total_lines_of_code: Optional[int] = 0
    files_by_type: Dict[str, int]
    top_complex_modules: List[str]

class DocAgentState(TypedDict):
    project_id: str
    target_file: str
    file_type: str
    mermaid_graph: str
    code_snippets: str
    functional_json: Dict[str, Any]
    technical_json: Dict[str, Any]
    
    # Use add_messages to properly handle message list updates
    messages: Annotated[List, add_messages]
    
    research_iterations: int
    research_complete: bool