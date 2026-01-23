"""API routes for Code Generation (Local)."""

import uuid

from fastapi import APIRouter, HTTPException
from loguru import logger

from app.services.codegen_local import CodegenLocalService


router = APIRouter(tags=["Codegen"])


@router.post("/projects/{project_id}/codegen/local")
async def start_codegen_local(project_id: uuid.UUID):
    """Start local code generation for a project.
    
    Converts mainframe components (COBOL, copybooks, JCL) to .NET code.
    Runs asynchronously - returns immediately with a run_id.
    
    Prerequisites:
    - Phase A must be complete (file_summaries.md, dependency_graph.md)
    
    Output:
    - Generated code in project_artifacts/{project_id}/local-migration/
    """
    try:
        service = CodegenLocalService(project_id)
        result = await service.run()
        
        if not result.get("success"):
            raise HTTPException(status_code=400, detail=result.get("error"))
        
        return {
            "run_id": result["run_id"],
            "project_id": str(project_id),
            "status": "started",
            "message": "Code generation started. Poll /codegen/status/{run_id} for progress.",
            "output_path": f"project_artifacts/{project_id}/local-migration/",
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Failed to start codegen: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/codegen/status/{run_id}")
async def get_codegen_status(run_id: str):
    """Get the status of a code generation run.
    
    Uses run_id only - no project_id needed.
    """
    status = CodegenLocalService.get_status_by_run_id(run_id)
    
    if not status:
        raise HTTPException(status_code=404, detail="Run not found")
    
    return status
