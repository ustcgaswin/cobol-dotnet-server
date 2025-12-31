"""API routes module."""

from app.api.routes.health import router as health_router
from app.api.routes.parsers import router as parsers_router
from app.api.routes.projects import router as projects_router
from app.api.routes.source_files import router as source_files_router

__all__ = ["health_router", "parsers_router", "projects_router", "source_files_router"]

