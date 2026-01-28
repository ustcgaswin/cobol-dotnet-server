import uuid
from typing import Optional, Dict
from fastapi import APIRouter, Depends, BackgroundTasks, HTTPException
from loguru import logger
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.core.exceptions import ProjectNotFoundException
from app.services.project import ProjectService
from app.services.documentation_services.service import DocumentationService

router = APIRouter(prefix="/projects/{project_id}/documentation", tags=["Documentation"])

# --- Response Schemas ---

class DocumentationGenerateResponse(BaseModel):
    """Response when agentic documentation generation is triggered."""
    job_id: str
    status: str
    base_output_path: str
    message: str

class DocumentationStatusResponse(BaseModel):
    """Response for checking progress of per-file requirement generation."""
    project_id: uuid.UUID
    status: str
    is_complete: bool
    progress_percentage: float
    technical_folder: Optional[str] = None
    functional_folder: Optional[str] = None

# --- Dependencies ---

def get_doc_service(db: AsyncSession = Depends(get_db)) -> DocumentationService:
    return DocumentationService(db)

def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    return ProjectService(db)

# --- Routes ---

@router.post("/generate", response_model=APIResponse[DocumentationGenerateResponse])
async def generate_requirements_documentation(
    project_id: uuid.UUID,
    background_tasks: BackgroundTasks,
    doc_service: DocumentationService = Depends(get_doc_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[DocumentationGenerateResponse]:
    """
    Trigger the Agentic Documentation Pipeline.
    
    This process:
    1. Builds a System Graph (NetworkX) from parsed data.
    2. Runs a LangGraph Agent for EACH file to perform surgical code research.
    3. Generates separate Technical and Functional PDFs using specialized Renderers.
    4. Stores files in 'technical_requirements' and 'functional_requirements' folders.
    """
    # 1. Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))
    
    logger.info(f"Triggering Agentic Research and Documentation for project {project_id}")

    # 2. Define the base artifact path
    base_output_path = f"project_artifacts/{project_id}/"

    # 3. Queue the Agentic Pipeline in the background
    # Note: New service only needs project_id as it handles internal folder logic
    background_tasks.add_task(doc_service.run_full_pipeline, project_id)

    response_data = DocumentationGenerateResponse(
        job_id=str(uuid.uuid4()),
        status="Agentic Research Started",
        base_output_path=base_output_path,
        message="The AI Agent is researching files individually. PDFs will be generated per file."
    )

    return APIResponse.ok(response_data)

@router.get("/status", response_model=APIResponse[DocumentationStatusResponse])
async def check_documentation_status(
    project_id: uuid.UUID,
    doc_service: DocumentationService = Depends(get_doc_service),
) -> APIResponse[DocumentationStatusResponse]:
    """Check progress of the technical and functional requirement generation."""
    
    # Calls the updated service method that checks for folder existence
    status_data = await doc_service.get_job_status(project_id)
    
    return APIResponse.ok(
        DocumentationStatusResponse(
            project_id=project_id,
            status=status_data["status"],
            is_complete=(status_data["status"] == "COMPLETED"),
            progress_percentage=status_data["progress"],
            technical_folder=status_data["paths"]["technical"],
            functional_folder=status_data["paths"]["functional"]
        )
    )

@router.get("/metadata", response_model=APIResponse[Dict])
async def get_requirements_metadata(
    project_id: uuid.UUID,
    doc_service: DocumentationService = Depends(get_doc_service),
) -> APIResponse[Dict]:
    """Retrieve metadata and storage locations for the generated PDF sets."""
    
    # Reuse the metadata logic to return paths for both requirement sets
    status_data = await doc_service.get_job_status(project_id)
    
    if status_data["status"] != "COMPLETED":
        raise HTTPException(
            status_code=404, 
            detail="Requirements documentation not yet generated or incomplete."
        )

    return APIResponse.ok({
        "project_id": str(project_id),
        "folders": status_data["paths"],
        "description": "Technical and Functional requirement folders containing per-file PDFs."
    })