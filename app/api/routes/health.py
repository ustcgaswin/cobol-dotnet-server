"""Health check endpoints for API monitoring."""

from datetime import datetime
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
from app.config.llm import get_llm_manager

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


class InstanceHealth(BaseModel):
    """Health status for a single instance (LLM + Embeddings)."""
    name: str
    llm: str  # "connected", "unhealthy", "skipped"
    embeddings: str  # "connected", "unhealthy", "skipped"


class LLMHealthStatus(BaseModel):
    """Overall LLM health status with all instances."""
    status: str  # "healthy", "degraded", "unhealthy", "not_configured"
    instances: list[InstanceHealth]
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
    successful_requests: int
    failed_requests: int
    total_tokens: int
    prompt_tokens: int
    completion_tokens: int


class AggregateStats(BaseModel):
    """Aggregate statistics across all instances."""
    total_requests: int
    successful_requests: int
    failed_requests: int
    failed_requests: int
    success_rate: float
    total_tokens: int
    prompt_tokens: int
    completion_tokens: int


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
    
    Tests ALL available instances by making actual API calls.
    Returns per-instance health status.
    """
    manager = get_llm_manager()
    available = manager.get_available_instances()
    
    if not available:
        return APIResponse.ok(
            LLMHealthStatus(
                status="not_configured",
                instances=[],
                available_instances=[],
                timestamp=datetime.utcnow(),
            )
        )
    
    instance_results: list[InstanceHealth] = []
    
    for instance_name in available:
        llm_status = "skipped"
        embeddings_status = "skipped"
        
        # ---- Test LLM ----
        try:
            llm = manager.get_llm(instance_name)
            response = llm.invoke("Reply with OK only.")
            content = getattr(response, "content", "")
            llm_status = "connected" if "OK" in str(content).upper() else "unhealthy"
        except Exception as e:
            logger.warning(f"LLM health check failed for {instance_name}: {e}")
            llm_status = "unhealthy"
        
        # ---- Test Embeddings ----
        try:
            embeddings = manager.get_embeddings(instance_name)
            embeddings.embed_query("ping")
            embeddings_status = "connected"
        except Exception as e:
            logger.warning(f"Embeddings health check failed for {instance_name}: {e}")
            embeddings_status = "unhealthy"
        
        instance_results.append(InstanceHealth(
            name=instance_name,
            llm=llm_status,
            embeddings=embeddings_status,
        ))
    
    # Determine overall status
    all_connected = all(
        i.llm == "connected" and i.embeddings == "connected" 
        for i in instance_results
    )
    any_connected = any(
        i.llm == "connected" or i.embeddings == "connected" 
        for i in instance_results
    )
    
    if all_connected:
        overall_status = "healthy"
    elif any_connected:
        overall_status = "degraded"
    else:
        overall_status = "unhealthy"
    
    return APIResponse.ok(
        LLMHealthStatus(
            status=overall_status,
            instances=instance_results,
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
                    successful_requests=s.successful_requests,
                    failed_requests=s.failed_requests,
                    total_tokens=s.total_tokens,
                    prompt_tokens=s.prompt_tokens,
                    completion_tokens=s.completion_tokens,
                )
                for s in all_stats
            ],
            aggregate=AggregateStats(**aggregate),
            available_instances=manager.get_available_instances(),
            timestamp=datetime.utcnow(),
        )
    )
