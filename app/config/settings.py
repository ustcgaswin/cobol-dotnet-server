from pathlib import Path
from typing import Optional

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):

    # Application
    APP_NAME: str = Field(default="Cobol Converter API")
    APP_VERSION: str = Field(default="0.1.0")
    
    # Environment
    ENV: str = Field(default="development")

    # Storage
    PROJECT_STORAGE_PATH: str = Field(
        default="project_storage",
        description="Base path for project file storage"
    )
    PROJECT_ARTIFACTS_PATH: str = Field(
        default="project_artifacts",
        description="Base path for project artifacts (parsed outputs, etc.)"
    )
    
    # File size limits in MB (based on production mainframe standards)
    FILE_SIZE_LIMITS_MB: dict[str, int] = Field(
        default={
            "cobol": 10,
            "copybook": 5,
            "jcl": 2,
            "rexx": 5,
            "catproc": 2,
            "proc": 2,
            "pli": 10,
        },
        description="Maximum file size in MB per file type"
    )

    # Database
    DATABASE_URL: Optional[str] = Field(default=None)
    DB_POOL_SIZE: int = Field(default=10, description="Database connection pool size")
    DB_MAX_OVERFLOW: int = Field(default=20, description="Maximum connections beyond pool_size")
    DB_POOL_TIMEOUT: int = Field(default=30, description="Seconds to wait for connection from pool")
    DB_POOL_RECYCLE: int = Field(default=3600, description="Recycle connections after N seconds")

    # LLM Provider Selection
    LLM_PROVIDER: str = Field(
        default="azure_anthropic",
        description="LLM provider: 'azure_anthropic' or 'azure_openai'"
    )
    
    # Anthropic Azure Foundry settings
    AZURE_ANTHROPIC_ENDPOINT: Optional[str] = Field(default=None)
    AZURE_ANTHROPIC_API_KEY: Optional[str] = Field(default=None)
    AZURE_ANTHROPIC_MODEL: str = Field(default="claude-sonnet-4-5-2")
    

    LLM_MAX_TOKENS: int = Field(default=16384)
    LLM_TEMPERATURE: float = Field(default=0.0)
    LLM_TIMEOUT: float = Field(default=600.0, description="LLM request timeout in seconds")
    

    # Azure OpenAI embed settings
    AZURE_OPENAI_EMBED_API_ENDPOINT: Optional[str] = Field(default=None)
    AZURE_OPENAI_EMBED_API_KEY: Optional[str] = Field(default=None)
    AZURE_OPENAI_EMBED_MODEL: Optional[str] = Field(default=None)
    AZURE_OPENAI_EMBED_VERSION: Optional[str] = Field(default=None)
    AZURE_OPENAI_EMBED_DEPLOYMENT_NAME: Optional[str] = Field(default=None)
    EMBEDDING_DIMENSION: int = Field(default=1536, description="Embedding vector dimension")
    EMBEDDING_ENCODING: str = Field(
        default="cl100k_base",
        description="tiktoken encoding for chunking (must match embedding model)"
    )

    # RAG settings
    RAG_DATA_PATH: str = Field(
        default="app/data",
        description="Path to RAG data folder containing documents"
    )
    FAISS_INDEX_PATH: str = Field(
        default="faiss_index",
        description="Path to store FAISS index"
    )
    RAG_DEFAULT_K: int = Field(
        default=4,
        description="Default number of chunks to retrieve for RAG queries"
    )

    # MLflow
    MLFLOW_ENABLED: bool = Field(
        default=False,
        description="Enable MLflow tracing for LangChain/LangGraph"
    )
    MLFLOW_TRACKING_URI: str | None = Field(
        default=None,
        description="MLflow tracking server URI (e.g., http://localhost:5000)"
    )
    MLFLOW_EXPERIMENT_NAME: str = Field(
        default="cobol-converter",
        description="MLflow experiment name for organizing traces"
    )

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=True,
    )
    
    def get_file_size_limit_bytes(self, file_type: str) -> int:
        """Get file size limit in bytes for a given file type."""
        limit_mb = self.FILE_SIZE_LIMITS_MB.get(file_type, 5)  # Default 5MB
        return limit_mb * 1024 * 1024
    
    def get_storage_path(self) -> Path:
        """Get the storage path as a Path object."""
        return Path(self.PROJECT_STORAGE_PATH)
    
    def get_artifacts_path(self) -> Path:
        """Get the artifacts path as a Path object."""
        return Path(self.PROJECT_ARTIFACTS_PATH)


settings = Settings()