"""API routes module."""

from app.api.routes.health import router as health_router
from app.api.routes.projects import router as projects_router

__all__ = ["health_router", "projects_router"]
