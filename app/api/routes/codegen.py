"""API routes for Code Generation (Local)."""

import uuid

from fastapi import APIRouter, HTTPException
from loguru import logger

from app.services.codegen_local import CodegenLocalService
from app.services.codegen_local.process_flow import ProcessFlowService


router = APIRouter(tags=["Codegen"])


@router.post("/projects/{project_id}/codegen/local")
async def start_codegen_local(project_id: uuid.UUID):
    """Start local code generation for a project.
    
    Converts mainframe components (COBOL, copybooks, JCL) to .NET code.
    Runs asynchronously - returns immediately with a run_id.
    
    Prerequisites:
    - Phase A artifacts (file_summaries.md, dependency_graph.md).
      If missing, they will be automatically generated (Self-Healing).
    
    Output:
    - Generated code in project_artifacts/{project_id}/code-migration/{project_name}-local/
    """
    service = CodegenLocalService(project_id)
    
    # run() will auto-generate prerequisites if missing
    result = await service.run()
    
    return {
        "project_id": str(project_id),
        "status": "started",
        "message": "Code generation started.",
    }





@router.get("/projects/{project_id}/codegen/local/status")
async def get_latest_codegen_status(project_id: uuid.UUID):
    """Get the latest code generation status for a project.
    
    Returns the most recent status from the database.
    Useful for restoring UI state on page load.
    
    Status codes:
    - pending: Not started / Failed (allows retry)
    - in_progress: Running
    - completed: Finished successfully
    - failed: Error occurred
    """
    from app.db.base import async_session_factory
    from app.db.models.project import Project
    from app.services.codegen_local import CodegenLocalService
    
    async with async_session_factory() as session:
        project = await session.get(Project, project_id)
        
        if not project:
            raise HTTPException(status_code=404, detail="Project not found")
            
        status = project.code_migration_status
        
        # If in_progress, strict UX might want running task ID's details.
        # But for restoration, just knowing "in_progress" is enough to show spinner.
        # Use existing logic to see if we can get granular details if user wants?
        # For now, stick to simple DB status.
        
        return {
            "status": status,
            "project_id": str(project_id),
        }


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


@router.get("/projects/{project_id}/codegen/local/download")
async def download_codegen_zip(project_id: uuid.UUID):
    """Download the generated code as a zip file.
    
    Only available when code_migration_status is 'completed'.
    Returns 409 if status is not completed.
    Returns 404 if project not found.
    """
    from fastapi.responses import Response
    from app.db.base import async_session_factory
    from app.db.models.project import Project
    
    async with async_session_factory() as session:
        project = await session.get(Project, project_id)
        if not project:
            raise HTTPException(status_code=404, detail="Project not found")
        if project.code_migration_status != "completed":
            raise HTTPException(
                status_code=409,
                detail=f"Download unavailable. Status: {project.code_migration_status}"
            )
    
    service = CodegenLocalService(project_id)
    project_name = await service._get_project_name()
    zip_bytes = service.create_zip(project_name)
    safe_name = project_name.replace(" ", "_")
    
    return Response(
        content=zip_bytes,
        media_type="application/zip",
        headers={"Content-Disposition": f'attachment; filename="{safe_name}-local.zip"'}
    )


@router.post("/projects/{project_id}/codegen/local/process-flow")
async def generate_process_flow(project_id: uuid.UUID):
    """Generate or regenerate the process flow diagram.

    Analyzes the generated .NET solution and produces a Mermaid process
    flow diagram saved as ``process_flow.md`` in the solution directory.

    Can be called independently of code generation (e.g. to regenerate).
    Requires that code generation has completed — returns 404 if no solution exists.
    """
    service = ProcessFlowService(project_id)

    output_dir = service._get_output_dir()
    if not output_dir or not output_dir.exists():
        raise HTTPException(
            status_code=404,
            detail=(
                f"No generated solution found for project {project_id}. "
                "Run code generation first."
            ),
        )

    try:
        flow_path = await service.generate()
        return {
            "status": "success",
            "project_id": str(project_id),
            "path": flow_path.name,
            "message": "Process flow diagram generated successfully.",
        }
    except Exception as e:
        logger.error(f"Process flow generation failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/projects/{project_id}/codegen/local/process-flow")
async def get_process_flow(project_id: uuid.UUID):
    """Get the process flow diagram if it exists.

    Returns the mermaid markdown content of ``process_flow.md``.
    Returns 404 if not yet generated — POST to ``/process-flow`` to create it.
    """
    service = ProcessFlowService(project_id)
    content = service.get_existing()

    if content is None:
        raise HTTPException(
            status_code=404,
            detail="Process flow not yet generated. POST to /process-flow to create it.",
        )

    return {
        "project_id": str(project_id),
        "content": content,
    }

