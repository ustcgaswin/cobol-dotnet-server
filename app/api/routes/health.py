"""Health check endpoints for API monitoring."""

from datetime import datetime
import traceback
from typing import Optional

from fastapi import APIRouter, Depends
from loguru import logger
from pydantic import BaseModel
from sqlalchemy import text
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.config.settings import settings
from app.core.exceptions import DatabaseHealthCheckError
from app.config.llm import get_llm, get_llm_manager, CODEGEN
from app.config.llm.models import DEFAULT_LLM_MODEL, DEFAULT_EMBEDDINGS_MODEL

router = APIRouter(tags=["Health"])


class HealthStatus(BaseModel):
    """Health check response data."""
    
    status: str
    database: str
    timestamp: datetime


class LLMHealth(BaseModel):
    """LLM instance health status."""
    status: str
    instance_name: str
    temperature: float
    max_tokens: int
    timeout: int


class EmbeddingHealth(BaseModel):
    """Embeddings instance health status."""
    status: str
    instance_name: str
    dimension: int


class LLMHealthStatus(BaseModel):
    """Overall LLM health status."""
    status: str
    llm: LLMHealth
    embeddings: EmbeddingHealth
    available_instances: list[str]
    timestamp: datetime


# ============================================
# Stats Models
# ============================================

class InstanceStatsResponse(BaseModel):
    """Statistics for a single LLM/Embeddings instance."""
    instance_name: str
    resource_type: str  # "llm" or "embeddings"
    first_request_time: Optional[datetime]
    last_request_time: Optional[datetime]
    total_requests: int
    successful_requests: int
    failed_requests: int


class AggregateStats(BaseModel):
    """Aggregate statistics across all instances."""
    total_requests: int
    successful_requests: int
    failed_requests: int
    success_rate: float


class LLMStatsResponse(BaseModel):
    """Full LLM stats response."""
    instances: list[InstanceStatsResponse]
    aggregate: AggregateStats
    available_instances: list[str]
    timestamp: datetime


# ============================================
# Endpoints
# ============================================

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


@router.get("/health/llm", response_model=APIResponse[LLMHealthStatus])
async def llm_health_check() -> APIResponse[LLMHealthStatus]:
    """
    Deep health check for LLM and embeddings.
    
    Tests the first available instance by making actual API calls.
    """
    manager = get_llm_manager()
    available = manager.get_available_instances()
    
    if not available:
        return APIResponse.ok(
            LLMHealthStatus(
                status="not_configured",
                llm=LLMHealth(
                    status="not_configured",
                    instance_name="none",
                    temperature=DEFAULT_LLM_MODEL.config.temperature,
                    max_tokens=DEFAULT_LLM_MODEL.config.max_tokens,
                    timeout=int(settings.LLM_TIMEOUT),
                ),
                embeddings=EmbeddingHealth(
                    status="not_configured",
                    instance_name="none",
                    dimension=DEFAULT_EMBEDDINGS_MODEL.config.dimensions,
                ),
                available_instances=[],
                timestamp=datetime.utcnow(),
            )
        )
    
    # Test first available instance
    test_instance = available[0]
    
    # ---- LLM (REAL CALL) ----
    try:
        llm = manager.get_llm(test_instance)
        response = llm.invoke("Reply with OK only.")
        content = getattr(response, "content", "")
        llm_status = "connected" if "OK" in str(content) else "unhealthy"
    except Exception:
        logger.error("LLM health check failed")
        logger.error(traceback.format_exc())
        llm_status = "unhealthy"

    # ---- Embeddings ----
    try:
        embeddings = manager.get_embeddings(test_instance)
        embeddings.embed_query("ping")
        embeddings_status = "connected"
    except Exception:
        logger.error("Embeddings health check failed")
        logger.error(traceback.format_exc())
        embeddings_status = "unhealthy"

    overall_status = (
        "healthy"
        if llm_status == "connected" and embeddings_status == "connected"
        else "degraded"
    )

    return APIResponse.ok(
        LLMHealthStatus(
            status=overall_status,
            llm=LLMHealth(
                status=llm_status,
                instance_name=test_instance,
                temperature=llm.temperature,
                max_tokens=llm.max_tokens,
                timeout=int(llm.timeout),
            ),
            embeddings=EmbeddingHealth(
                status=embeddings_status,
                instance_name=test_instance,
                dimension=DEFAULT_EMBEDDINGS_MODEL.config.dimensions,
            ),
            available_instances=available,
            timestamp=datetime.utcnow(),
        )
    )


@router.get("/health/llm-stats", response_model=APIResponse[LLMStatsResponse])
async def get_llm_stats() -> APIResponse[LLMStatsResponse]:
    """
    Get usage statistics for all LLM and Embeddings instances.
    
    Returns per-instance stats (first/last request time, counts) and 
    aggregate stats across all instances.
    
    Stats are in-memory and reset on server restart.
    """
    manager = get_llm_manager()
    all_stats = manager.stats.get_all_stats()
    aggregate = manager.stats.get_aggregate_stats()
    
    return APIResponse.ok(
        LLMStatsResponse(
            instances=[
                InstanceStatsResponse(
                    instance_name=s.instance_name,
                    resource_type=s.resource_type,
                    first_request_time=s.first_request_time,
                    last_request_time=s.last_request_time,
                    total_requests=s.total_requests,
                    successful_requests=s.successful_requests,
                    failed_requests=s.failed_requests,
                )
                for s in all_stats
            ],
            aggregate=AggregateStats(**aggregate),
            available_instances=manager.get_available_instances(),
            timestamp=datetime.utcnow(),
        )
    )
