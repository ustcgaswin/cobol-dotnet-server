"""Health check endpoint for API monitoring."""

from datetime import datetime

from fastapi import APIRouter, Depends
from loguru import logger
from pydantic import BaseModel
from sqlalchemy import text
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.core.exceptions import DatabaseHealthCheckError

router = APIRouter(tags=["Health"])


class HealthStatus(BaseModel):
    """Health check response data."""
    
    status: str
    database: str
    timestamp: datetime


@router.get("/health", response_model=APIResponse[HealthStatus])
async def health_check(db: AsyncSession = Depends(get_db)) -> APIResponse[HealthStatus]:
    """
    Health check endpoint.
    
    Verifies API is running and database is accessible.
    """
    try:
        # Test database connectivity
        await db.execute(text("SELECT 1"))
        db_status = "connected"
    except Exception as e:
        logger.error(f"Database health check failed: {str(e)}")
        raise DatabaseHealthCheckError(f"Database connection failed: {str(e)}")
    
    health_data = HealthStatus(
        status="healthy",
        database=db_status,
        timestamp=datetime.utcnow(),
    )
    
    return APIResponse.ok(health_data)
