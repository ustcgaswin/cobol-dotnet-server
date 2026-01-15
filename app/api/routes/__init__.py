"""API routes module."""

from app.api.routes.analyst import router as analyst_router
from app.api.routes.codegen import router as codegen_router
from app.api.routes.dependency import router as dependency_router
from app.api.routes.health import router as health_router
from app.api.routes.parsers import router as parsers_router
from app.api.routes.parsers import parsers_metadata_router
from app.api.routes.projects import router as projects_router
from app.api.routes.rag import router as rag_router
from app.api.routes.source_files import router as source_files_router
from app.api.routes.summarizer import router as summarizer_router
from app.api.routes.documentation import router as documentation_router

__all__ = [
    "analyst_router",
    "codegen_router",
    "dependency_router",
    "health_router",
    "parsers_router",
    "parsers_metadata_router",
    "projects_router",
    "rag_router",
    "source_files_router",
    "summarizer_router",
    "documentation_router"
]

