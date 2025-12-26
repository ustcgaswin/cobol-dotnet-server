from typing import Optional

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):

    # Application
    APP_NAME: str = Field(default="Cobol Converter API")
    APP_VERSION: str = Field(default="0.1.0")
    
    # Environment
    ENV: str = Field(default="development")


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

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=True,
    )


settings = Settings()