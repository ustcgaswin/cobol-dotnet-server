import uuid
from typing import Optional
from fastapi import APIRouter, Depends, BackgroundTasks, HTTPException
from loguru import logger
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.core.exceptions import ProjectNotFoundException
from app.services.project import ProjectService
from app.services.documentation import DocumentationService

router = APIRouter(prefix="/projects/{project_id}/documentation", tags=["Documentation"])

class DocumentationGenerateResponse(BaseModel):
    """Response when documentation generation is triggered."""
    job_id: str
    status: str
    output_path: str
    estimated_completion: str

class DocumentationStatusResponse(BaseModel):
    """Response for checking the status of the documentation job."""
    project_id: uuid.UUID
    is_complete: bool
    progress_percentage: float
    master_report_path: Optional[str] = None
    files_processed: int
    total_files: int

def get_doc_service(db: AsyncSession = Depends(get_db)) -> DocumentationService:
    """Dependency for documentation service."""
    return DocumentationService(db)

def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    """Dependency for project service."""
    return ProjectService(db)

@router.post("/generate", response_model=APIResponse[DocumentationGenerateResponse])
async def generate_master_manual(
    project_id: uuid.UUID,
    background_tasks: BackgroundTasks,
    doc_service: DocumentationService = Depends(get_doc_service),
    project_service: ProjectService = Depends(get_project_service),
) -> APIResponse[DocumentationGenerateResponse]:
    """
    Trigger the AI-powered documentation pipeline.
    
    This will:
    1. Build the Dependency Graph (NetworkX).
    2. Chunk large files (10k+ lines) using Paragraph-level analysis.
    3. Use RAG (FAISS) to maintain context between files.
    4. Generate a Consolidated Master Manual.
    """
    # 1. Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))

    # 2. Define the artifact path (where report.md will live)
    # Usually project.artifacts_path / "documentation"
    output_path = f"artifacts/{project_id}/documentation/"
    
    logger.info(f"Triggering Master Manual generation for project {project_id}")

    # 3. Queue the long-running process in the background
    background_tasks.add_task(doc_service.run_full_pipeline, project_id, output_path)

    response_data = DocumentationGenerateResponse(
        job_id=str(uuid.uuid4()),
        status="Processing started",
        output_path=output_path,
        estimated_completion="Processing time depends on code volume (approx. 1 min per 1k lines)"
    )

    return APIResponse.ok(response_data)

@router.get("/status", response_model=APIResponse[DocumentationStatusResponse])
async def check_documentation_status(
    project_id: uuid.UUID,
    doc_service: DocumentationService = Depends(get_doc_service),
) -> APIResponse[DocumentationStatusResponse]:
    """Check if the Master Manual and FAISS indexing are complete."""
    
    status = await doc_service.get_job_status(project_id)
    
    return APIResponse.ok(
        DocumentationStatusResponse(
            project_id=project_id,
            is_complete=status.is_complete,
            progress_percentage=status.progress,
            master_report_path=status.file_path,
            files_processed=status.files_processed,
            total_files=status.total_files
        )
    )

@router.get("/download", response_model=APIResponse[dict])
async def get_documentation_metadata(
    project_id: uuid.UUID,
    doc_service: DocumentationService = Depends(get_doc_service),
) -> APIResponse[dict]:
    """Retrieve the path and metadata of the generated Master Manual."""
    
    report_metadata = await doc_service.get_report_metadata(project_id)
    if not report_metadata:
        raise HTTPException(status_code=404, detail="Documentation not found or not yet generated.")

    return APIResponse.ok(report_metadata)