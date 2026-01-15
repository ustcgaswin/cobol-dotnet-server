from pydantic import BaseModel, Field
from typing import List, Optional

class ParagraphSummary(BaseModel):
    paragraph_name: str
    logic_type: str = Field(description="e.g., Data Validation, Calculation, I/O")
    business_logic: str = Field(description="2-sentence summary of what this does")
    technical_steps: List[str] = Field(description="Key technical operations performed")
    variables_modified: List[str]

class ProgramReport(BaseModel):
    program_id: str
    business_purpose: str
    high_level_flow: List[str]
    external_dependencies: List[str]
    potential_risk_areas: List[str] = Field(description="Complex logic or legacy hacks detected")