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

    # LLM settings (used by OAuth LLM manager)
    LLM_MAX_TOKENS: int = Field(default=16384, description="Default max tokens for LLM (fallback)")
    LLM_TEMPERATURE: float = Field(default=0.0, description="Default temperature for LLM (fallback)")
    LLM_TIMEOUT: float = Field(default=600.0, description="LLM request timeout in seconds")

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

    # Code Generation settings
    CODEGEN_ENABLE_SUMMARIZATION: bool = Field(
        default=True,
        description="Enable context summarization for codegen agent to prevent context overflow on large projects"
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

    # ============================================
    # OAuth2 LLM Configuration
    # ============================================
    
    # Shared OAuth2 Auth URL
    OAUTH_AUTH_URL: str = Field(
        default="",
        description="OAuth2 token endpoint URL"
    )
    
    # Separate API Endpoints
    LLM_ENDPOINT_URL: str = Field(
        default="",
        description="LLM chat completion API endpoint"
    )
    EMBEDDINGS_ENDPOINT_URL: str = Field(
        default="",
        description="Embeddings API endpoint"
    )
    
    # OAuth settings
    OAUTH_SCOPE: str = Field(
        default="customscope",
        description="OAuth2 scope for token request"
    )
    OAUTH_SSL_VERIFY: bool = Field(
        default=False,
        description="Whether to verify SSL certificates for OAuth/LLM calls"
    )
    
    # Model names
    LLM_MODEL: str = Field(
        default="gpt-40-dev",
        description="LLM model name to use"
    )
    EMBEDDINGS_MODEL: str = Field(
        default="text-embedding-ada-002-2-gs",
        description="Embeddings model name to use"
    )
    
    # Codegen Instance Credentials
    CODEGEN_CLIENT_ID: str = Field(default="")
    CODEGEN_CLIENT_SECRET: str = Field(default="")
    
    # Docgen Instance Credentials
    DOCGEN_CLIENT_ID: str = Field(default="")
    DOCGEN_CLIENT_SECRET: str = Field(default="")
    
    # Token settings
    OAUTH_TOKEN_REFRESH_BUFFER_SECONDS: int = Field(
        default=60,
        description="Refresh token this many seconds before actual expiry"
    )

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=True,
        extra="ignore",
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