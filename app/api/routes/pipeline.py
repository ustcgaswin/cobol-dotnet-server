"""API routes for Full Pipeline orchestration."""

import asyncio
import uuid

from fastapi import APIRouter, Depends, HTTPException
from loguru import logger
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.core.exceptions import ProjectNotFoundException
from app.services.project import ProjectService
from app.services.analyst.service import AnalystService
from app.services.codegen_local import CodegenLocalService
from app.services.documentation_services.service import DocumentationService


router = APIRouter(tags=["Pipeline"])


class FullPipelineResponse(BaseModel):
    """Response when full pipeline is triggered."""
    project_id: str
    status: str
    message: str
    poll_endpoints: dict


def get_project_service(db: AsyncSession = Depends(get_db)) -> ProjectService:
    return ProjectService(db)


def get_doc_service(db: AsyncSession = Depends(get_db)) -> DocumentationService:
    return DocumentationService(db)


@router.post("/projects/{project_id}/full-pipeline", response_model=FullPipelineResponse)
async def start_full_pipeline(
    project_id: uuid.UUID,
    project_service: ProjectService = Depends(get_project_service),
    doc_service: DocumentationService = Depends(get_doc_service),
) -> FullPipelineResponse:
    """
    Start the full migration pipeline for a project.
    
    This endpoint:
    1. Ensures prerequisites exist (parsing, summarization, dependency extraction, analyst)
       - Uses self-healing: only generates missing artifacts
    2. Starts codegen and documentation generation in parallel
    
    Poll individual status endpoints to track progress:
    - Codegen: GET /projects/{project_id}/codegen/local/status
    - Documentation: GET /projects/{project_id}/documentation/status
    
    If one service fails, the other will continue to completion.
    """
    # 1. Verify project exists
    project = await project_service.get_project(project_id)
    if not project:
        raise ProjectNotFoundException(str(project_id))
    
    logger.info(f"Starting full pipeline for project {project_id}")
    
    # 2. Kick off the pipeline in background
    asyncio.create_task(_run_full_pipeline(project_id, doc_service))
    
    return FullPipelineResponse(
        project_id=str(project_id),
        status="started",
        message="Full pipeline started. Codegen and Documentation running in parallel.",
        poll_endpoints={
            "codegen": f"/api/v1/projects/{project_id}/codegen/local/status",
            "documentation": f"/api/v1/projects/{project_id}/documentation/status"
        }
    )


async def _run_full_pipeline(project_id: uuid.UUID, doc_service: DocumentationService):
    """
    Background task that orchestrates the full pipeline.
    
    1. Runs prerequisites (self-healing)
    2. Runs codegen and documentation in parallel
    """
    try:
        # 1. Ensure prerequisites via AnalystService (self-healing)
        # This handles: Parser -> Summarizer -> DependencyExtractor -> Analyst
        logger.info(f"[Pipeline] Ensuring prerequisites for project {project_id}")
        analyst_service = AnalystService(project_id)
        await analyst_service.ensure_prerequisites()
        
        # Also ensure the Analyst agent has run (for functionality_catalog.md)
        from pathlib import Path
        from app.config.settings import settings
        
        system_context_path = Path(settings.PROJECT_ARTIFACTS_PATH).resolve() / str(project_id) / "system_context"
        functionality_catalog = system_context_path / "functionality_catalog.md"
        
        if not functionality_catalog.exists():
            logger.info("[Pipeline] functionality_catalog.md missing. Running Analyst agent...")
            run_id = await analyst_service.run()
            
            # Wait for Analyst to complete
            max_wait = 600  # 10 minutes
            poll_interval = 5
            waited = 0
            
            while waited < max_wait:
                status = analyst_service.get_status(run_id)
                if status and status.get("status") in ("complete", "failed", "cancelled"):
                    break
                await asyncio.sleep(poll_interval)
                waited += poll_interval
            
            if not status or status.get("status") != "complete":
                logger.warning(f"[Pipeline] Analyst did not complete successfully: {status}")
            else:
                logger.info("[Pipeline] Analyst completed successfully")
        
        logger.info(f"[Pipeline] Prerequisites ready. Starting parallel execution...")
        
        # 2. Run codegen and documentation in parallel
        codegen_service = CodegenLocalService(project_id)
        
        async def run_codegen():
            try:
                logger.info("[Pipeline] Starting codegen...")
                await codegen_service.run()
            except Exception as e:
                logger.error(f"[Pipeline] Codegen failed: {e}")
        
        async def run_documentation():
            try:
                logger.info("[Pipeline] Starting documentation...")
                await doc_service.run_full_pipeline(project_id, mode="ALL")
            except Exception as e:
                logger.error(f"[Pipeline] Documentation failed: {e}")
        
        # Run both in parallel, don't fail if one errors
        await asyncio.gather(
            run_codegen(),
            run_documentation(),
            return_exceptions=True
        )
        
        logger.info(f"[Pipeline] Full pipeline completed for project {project_id}")
        
    except Exception as e:
        logger.exception(f"[Pipeline] Failed for project {project_id}: {e}")
