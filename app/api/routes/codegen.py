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


@router.get("/projects/{project_id}/codegen/local/files")
async def get_codegen_file_tree(project_id: uuid.UUID):
    """Get the file tree of generated code.
    
    Returns a nested structure of directories and files.
    File IDs are URL-safe Base64 encoded paths.
    """
    service = CodegenLocalService(project_id)
    
    # Get project name for folder lookup
    project_name = await service._get_project_name()
    
    tree = service.get_file_tree(project_name)
    
    return {
        "project_id": str(project_id),
        "project_name": project_name,
        "tree": tree,
    }


@router.get("/projects/{project_id}/codegen/local/files/{file_id}")
async def get_codegen_file_content(project_id: uuid.UUID, file_id: str):
    """Get the content of a generated file.
    
    Args:
        project_id: UUID of the project
        file_id: URL-safe Base64 encoded file path (from file tree)
    """
    service = CodegenLocalService(project_id)
    
    # Get project name for folder lookup
    project_name = await service._get_project_name()
    
    content = service.get_file_content(project_name, file_id)
    
    return content

