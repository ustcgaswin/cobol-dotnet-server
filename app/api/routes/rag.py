"""RAG API routes for indexing and search."""

from fastapi import APIRouter
from loguru import logger
from pydantic import BaseModel, Field

from app.api.schemas.common import APIResponse
from app.config.settings import settings
from app.services.rag import rag_service


router = APIRouter(prefix="/rag", tags=["RAG"])


class BuildRequest(BaseModel):
    force_rebuild: bool = False


class SearchRequest(BaseModel):
    query: str
    k: int = Field(default_factory=lambda: settings.RAG_DEFAULT_K)
    source_folder: str | None = None


class IndexStatus(BaseModel):
    exists: bool
    document_count: int
    index_path: str
    data_path: str


class BuildResult(BaseModel):
    status: str
    message: str
    document_count: int
    files_processed: int | None = None


class SearchResult(BaseModel):
    content: str
    metadata: dict


@router.post("/index/build", response_model=APIResponse[BuildResult])
async def build_index(request: BuildRequest) -> APIResponse[BuildResult]:
    """Build the RAG index from documents."""
    logger.info(f"Building index (force_rebuild={request.force_rebuild})")
    result = rag_service.build_index(force_rebuild=request.force_rebuild)
    return APIResponse.ok(BuildResult(**result))


@router.post("/index/rebuild", response_model=APIResponse[BuildResult])
async def rebuild_index() -> APIResponse[BuildResult]:
    """Force rebuild the RAG index."""
    logger.info("Force rebuilding index")
    result = rag_service.build_index(force_rebuild=True)
    return APIResponse.ok(BuildResult(**result))


@router.delete("/index", response_model=APIResponse[dict])
async def delete_index() -> APIResponse[dict]:
    """Delete the RAG index."""
    logger.info("Deleting index")
    result = rag_service.delete_index()
    return APIResponse.ok(result)


@router.get("/index/status", response_model=APIResponse[IndexStatus])
async def get_index_status() -> APIResponse[IndexStatus]:
    """Get the RAG index status."""
    status = rag_service.get_status()
    return APIResponse.ok(IndexStatus(**status))


@router.post("/search", response_model=APIResponse[list[SearchResult]])
async def search(request: SearchRequest) -> APIResponse[list[SearchResult]]:
    """Search the RAG index. Returns raw chunks for inspection."""
    documents = rag_service.search(
        query=request.query,
        k=request.k,
        source_folder=request.source_folder,
    )
    results = [
        SearchResult(content=doc.page_content, metadata=doc.metadata)
        for doc in documents
    ]
    return APIResponse.ok(results)


class AskRequest(BaseModel):
    query: str
    k: int = Field(default_factory=lambda: settings.RAG_DEFAULT_K)
    source_folder: str | None = None


class Source(BaseModel):
    source: str
    content_preview: str


class AskResponse(BaseModel):
    answer: str
    sources: list[Source]
    chunks_used: int


@router.post("/ask", response_model=APIResponse[AskResponse])
async def ask(request: AskRequest) -> APIResponse[AskResponse]:
    """Ask a question and get an LLM-synthesized answer."""
    logger.info(f"RAG ask: {request.query[:100]}...")
    result = rag_service.ask(
        query=request.query,
        k=request.k,
        source_folder=request.source_folder,
    )
    return APIResponse.ok(AskResponse(**result))



