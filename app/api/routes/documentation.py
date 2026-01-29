import uuid
from typing import Optional, Dict, Literal
from fastapi import APIRouter, Depends, BackgroundTasks, HTTPException, Query
from loguru import logger
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from fastapi.responses import FileResponse
from app.config.settings import settings

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
    mode: str  # Added to track what was requested

class DocumentationStatusResponse(BaseModel):
    """Response for checking progress of requirement generation."""
    project_id: uuid.UUID
    status: str # PENDING, PROCESSING, COMPLETED
    mode_requested: str
    is_complete: bool
    technical_document: Optional[str] = None
    functional_document: Optional[str] = None

# --- Dependencies ---

def get_doc_service(db: AsyncSession = Depends(get_db)) -> DocumentationService:
    return DocumentationService(db)

def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    return ProjectService(db)

# --- Helper for Code Reuse ---

async def _trigger_pipeline(
    project_id: uuid.UUID,
    mode: Literal["ALL", "TECHNICAL", "FUNCTIONAL"],
    background_tasks: BackgroundTasks,
    doc_service: DocumentationService,
    project_service: ProjectService
) -> DocumentationGenerateResponse:
    """
    Internal helper to validate project and trigger the background pipeline.
    """
    # 1. Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))
    
    logger.info(f"Triggering {mode} Agentic Documentation for project {project_id}")

    # 2. Define the base artifact path (for reference in response)
    base_output_path = f"project_artifacts/{project_id}/"

    # 3. Queue the Agentic Pipeline in the background
    background_tasks.add_task(doc_service.run_full_pipeline, project_id, mode)

    return DocumentationGenerateResponse(
        job_id=str(uuid.uuid4()), # Tracking ID
        status="Started",
        base_output_path=base_output_path,
        message=f"Agentic Research Started ({mode} mode). Documents will be generated per file.",
        mode=mode
    )

# --- Routes ---

@router.post("/generate", response_model=APIResponse[DocumentationGenerateResponse])
async def generate_all_documentation(
    project_id: uuid.UUID,
    background_tasks: BackgroundTasks,
    doc_service: DocumentationService = Depends(get_doc_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[DocumentationGenerateResponse]:
    """
    Trigger generation for BOTH Technical and Functional requirements.
    """
    response = await _trigger_pipeline(project_id, "ALL", background_tasks, doc_service, project_service)
    return APIResponse.ok(response)


@router.post("/generate/technical", response_model=APIResponse[DocumentationGenerateResponse])
async def generate_technical_documentation(
    project_id: uuid.UUID,
    background_tasks: BackgroundTasks,
    doc_service: DocumentationService = Depends(get_doc_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[DocumentationGenerateResponse]:
    """
    Trigger generation for TECHNICAL requirements only.
    """
    response = await _trigger_pipeline(project_id, "TECHNICAL", background_tasks, doc_service, project_service)
    return APIResponse.ok(response)


@router.post("/generate/functional", response_model=APIResponse[DocumentationGenerateResponse])
async def generate_functional_documentation(
    project_id: uuid.UUID,
    background_tasks: BackgroundTasks,
    doc_service: DocumentationService = Depends(get_doc_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[DocumentationGenerateResponse]:
    """
    Trigger generation for FUNCTIONAL requirements only.
    """
    response = await _trigger_pipeline(project_id, "FUNCTIONAL", background_tasks, doc_service, project_service)
    return APIResponse.ok(response)


@router.get("/status", response_model=APIResponse[DocumentationStatusResponse])
async def check_documentation_status(
    project_id: uuid.UUID,
    type: str = Query("ALL", regex="^(ALL|TECHNICAL|FUNCTIONAL)$"), # Optional filter
    doc_service: DocumentationService = Depends(get_doc_service),
) -> APIResponse[DocumentationStatusResponse]:
    """
    Check progress. Frontend polls this. 
    Use 'type' param to check specific folders (e.g. ?type=FUNCTIONAL).
    """
    
    # Calls the service method that checks for folder existence
    status_data = await doc_service.get_job_status(project_id, type)
    
    # Extract paths safely from the dictionary returned by service
    artifacts = status_data.get("artifacts", {})
    
    return APIResponse.ok(
        DocumentationStatusResponse(
            project_id=project_id,
            status=status_data["status"],
            mode_requested=status_data["mode_requested"],
            is_complete=(status_data["status"] == "COMPLETED"),
            # Map the nested 'artifacts' dict to the flat response model
            technical_document=artifacts.get("technical_path"),
            functional_document=artifacts.get("functional_path")
        )
    )

@router.get("/metadata", response_model=APIResponse[Dict])
async def get_requirements_metadata(
    project_id: uuid.UUID,
    doc_service: DocumentationService = Depends(get_doc_service),
) -> APIResponse[Dict]:
    """Retrieve metadata and storage locations for completed documentation."""
    
    # Check status for "ALL" to see what is available
    status_data = await doc_service.get_job_status(project_id, "ALL")
    
    # If nothing is generated yet
    if not status_data["artifacts"]["technical_path"] and not status_data["artifacts"]["functional_path"]:
         raise HTTPException(
            status_code=404, 
            detail="No requirements documentation found."
        )

    return APIResponse.ok({
        "project_id": str(project_id),
        "folders": status_data["artifacts"],
        "status": status_data["status"],
        "description": "Technical and Functional requirement folders containing per-file documents."
    })

@router.get("/download", response_class=FileResponse)
async def download_documentation(
    project_id: uuid.UUID,
    doc_type: str = Query(..., regex="^(technical|functional)$"), # Force valid choice
    project_service: ProjectService = Depends(get_project_service), # Ensure project exists
):
    """
    Download the generated PDF file.
    """
    # 1. Verify Project Exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))

    # 2. Construct Path
    # Matches the filenames created in service.py _generate_consolidated_placeholders
    filename = "Technical_Specifications.pdf" if doc_type == "technical" else "Functional_Specifications.pdf"
    
    file_path = settings.get_artifacts_path() / str(project_id) / filename

    # 3. Verify File Exists
    if not file_path.exists():
        raise HTTPException(
            status_code=404, 
            detail=f"{doc_type.title()} documentation not found. Please generate it first."
        )

    # 4. Stream File
    return FileResponse(
        path=file_path,
        filename=filename,
        media_type="application/pdf",
        content_disposition_type="attachment" # Forces browser 'Save As' dialog
    )

@router.get("/preview", response_class=FileResponse)
async def preview_documentation(
    project_id: uuid.UUID,
    doc_type: str = Query(..., regex="^(technical|functional)$"),
    project_service: ProjectService = Depends(get_project_service),
):
    """
    Stream the PDF file for browser display (Inline).
    Used for embedding in IFrames or PDF Viewers on the frontend.
    """
    # 1. Verify Project Exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))

    # 2. Construct Path
    filename = "Technical_Specifications.pdf" if doc_type == "technical" else "Functional_Specifications.pdf"
    file_path = settings.get_artifacts_path() / str(project_id) / filename

    # 3. Verify File Exists
    if not file_path.exists():
        raise HTTPException(
            status_code=404, 
            detail=f"{doc_type.title()} documentation not found. Please generate it first."
        )

    # 4. Stream File (Inline)
    return FileResponse(
        path=file_path,
        filename=filename,
        media_type="application/pdf",
        content_disposition_type="inline"  # <--- THIS IS THE KEY CHANGE
    )