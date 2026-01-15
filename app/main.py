from contextlib import asynccontextmanager

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from loguru import logger

from app.api.exception_handlers import register_exception_handlers
from app.api.routes import dependency_router, health_router, parsers_router, parsers_metadata_router, projects_router, rag_router, source_files_router
from app.config.logging_config import configure_logging
from app.config.mlflow_config import configure_mlflow
from app.config.settings import settings
from app.db.base import engine


configure_logging()
configure_mlflow()


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Application lifespan manager for startup and shutdown events."""
    logger.info("Starting Cobol Converter API...")
    
    # Check database connectivity
    try:
        from sqlalchemy import text
        from app.db.base import async_session_factory
        async with async_session_factory() as session:
            await session.execute(text("SELECT 1"))
        logger.info("Database connection: OK")
    except Exception as e:
        logger.error(f"Database connection: FAILED - {str(e)}")
    
    # Load or build RAG index
    try:
        from app.services.rag import rag_service
        if rag_service.load_if_exists():
            logger.info(f"RAG index loaded: {rag_service.store.document_count} documents")
        else:
            logger.info("RAG index not found, building...")
            result = rag_service.build_index()
            if result["status"] == "success":
                logger.info(f"RAG index built: {result['document_count']} documents from {result['files_processed']} files")
            elif result["status"] == "no_documents":
                logger.warning(f"RAG index: {result['message']}")
            else:
                logger.warning(f"RAG index build: {result['message']}")
    except Exception as e:
        logger.error(f"RAG index initialization: FAILED - {str(e)}")
    
    yield
    logger.info("Shutting down Cobol Converter API...")
    await engine.dispose()




app = FastAPI(
    title=settings.APP_NAME,
    version=settings.APP_VERSION,
    description="A FastAPI application for COBOL to .NET conversion.",
    docs_url="/docs",
    redoc_url="/redoc",
    openapi_url="/openapi.json",
    lifespan=lifespan,
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Register exception handlers
register_exception_handlers(app)

# Register routers
app.include_router(health_router)
app.include_router(projects_router, prefix="/api/v1")
app.include_router(source_files_router, prefix="/api/v1")
app.include_router(parsers_router, prefix="/api/v1")
app.include_router(parsers_metadata_router, prefix="/api/v1")
app.include_router(rag_router, prefix="/api/v1")
app.include_router(dependency_router, prefix="/api/v1")


