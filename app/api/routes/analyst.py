"""API routes for System Analyst Agent."""

import uuid

from fastapi import APIRouter, HTTPException
from loguru import logger

from app.services.analyst import AnalystService
from app.services.analyst.verifier import verify_coverage


router = APIRouter(tags=["Analyst"])


@router.post("/projects/{project_id}/analyst/run")
async def start_analyst(project_id: uuid.UUID):
    """Start the System Analyst Agent.
    
    Runs asynchronously - returns immediately with a run_id.
    Use GET /analyst/status/{run_id} to check progress.
    """
    try:
        service = AnalystService(project_id)
        run_id = await service.run()
        
        return {
            "run_id": run_id,
            "project_id": str(project_id),
            "status": "started",
            "message": "Analyst agent started. Poll /analyst/status/{run_id} for progress.",
        }
        
    except Exception as e:
        logger.error(f"Failed to start analyst: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/analyst/status/{run_id}")
async def get_analyst_status(run_id: str):
    """Get the status of an analyst run.
    
    Uses run_id only - no project_id needed.
    """
    status = AnalystService.get_status_by_run_id(run_id)
    
    if not status:
        raise HTTPException(status_code=404, detail="Run not found")
    
    return status


@router.post("/projects/{project_id}/analyst/verify")
async def verify_analyst_output(project_id: uuid.UUID):
    """Verify coverage of analyst output.
    
    Compares functionalities in file_summaries.md against
    the generated functionality_catalog.md.
    """
    try:
        report = verify_coverage(str(project_id))
        return report
        
    except Exception as e:
        logger.error(f"Verification failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))
