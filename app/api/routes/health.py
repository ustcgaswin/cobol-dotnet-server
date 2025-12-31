"""Health check endpoint for API monitoring."""

from datetime import datetime
import traceback

from fastapi import APIRouter, Depends
from loguru import logger
from pydantic import BaseModel
from sqlalchemy import text
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies import get_db
from app.api.schemas.common import APIResponse
from app.config.settings import settings
from app.core.exceptions import DatabaseHealthCheckError
from app.config.llm_config import llm, embeddings

router = APIRouter(tags=["Health"])


class HealthStatus(BaseModel):
    """Health check response data."""
    
    status: str
    database: str
    timestamp: datetime


class LLMHealth(BaseModel):
    status: str
    provider: str
    model: str
    temperature: float
    max_tokens: int
    timeout: int


class EmbeddingHealth(BaseModel):
    status: str
    deployment: str
    dimension: int



class LLMHealthStatus(BaseModel):
    status: str
    llm: LLMHealth
    embeddings: EmbeddingHealth
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




@router.get("/health/llm", response_model=APIResponse[LLMHealthStatus])
async def llm_health_check() -> APIResponse[LLMHealthStatus]:
    """
    Deep health check for LLM and embeddings.
    Calls the actual LLM.
    """

    # ---- LLM (REAL CALL) ----
    try:
        response = llm.invoke("Reply with OK only.", max_tokens=5)
        content = getattr(response, "content", "")
        llm_status = "connected" if "OK" in str(content) else "unhealthy"
    except Exception:
        logger.error("LLM health check failed")
        logger.error(traceback.format_exc())
        llm_status = "unhealthy"

    # ---- Embeddings ----
    try:
        embeddings.embed_query("ping")
        embeddings_status = "connected"
    except Exception:
        logger.error("Embeddings health check failed")
        logger.error(traceback.format_exc())
        embeddings_status = "unhealthy"

    overall_status = (
        "healthy"
        if llm_status == "connected"
        and embeddings_status == "connected"
        else "degraded"
    )

    return APIResponse.ok(
        LLMHealthStatus(
            status=overall_status,
            llm=LLMHealth(
                status=llm_status,
                provider=settings.LLM_PROVIDER,
                model=settings.AZURE_ANTHROPIC_MODEL,
                temperature=settings.LLM_TEMPERATURE,
                max_tokens=settings.LLM_MAX_TOKENS,
                timeout=settings.LLM_TIMEOUT,
            ),
            embeddings=EmbeddingHealth(
            status=embeddings_status,
            deployment=settings.AZURE_OPENAI_EMBED_DEPLOYMENT_NAME,
            dimension=settings.EMBEDDING_DIMENSION,
        ),

            timestamp=datetime.utcnow(),
        )
    )




















