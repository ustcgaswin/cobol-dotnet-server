"""API routes module."""

from app.api.routes.health import router as health_router
from app.api.routes.parsers import router as parsers_router
from app.api.routes.parsers import parsers_metadata_router
from app.api.routes.projects import router as projects_router
from app.api.routes.rag import router as rag_router
from app.api.routes.source_files import router as source_files_router

__all__ = [
    "health_router",
    "parsers_router",
    "parsers_metadata_router",
    "projects_router",
    "rag_router",
    "source_files_router",
]



