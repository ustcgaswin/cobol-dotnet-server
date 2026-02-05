"""LLM Model configurations.

Centralized definitions for all LLM models. Services select models by enum
rather than hardcoding model names, ensuring consistency and easy management.

Usage:
    from app.config.llm import get_llm, CODEGEN
    from app.config.llm.models import LLMModel
    
    # Use specific model
    llm = get_llm(CODEGEN, model=LLMModel.GPT4O_DEV)
    
    # Use default model for instance
    llm = get_llm(CODEGEN)
"""

from dataclasses import dataclass
from enum import Enum
from typing import Optional


@dataclass(frozen=True)
class ModelConfig:
    """Configuration for a specific LLM model.
    
    Attributes:
        name: API model identifier (sent in requests)
        max_tokens: Maximum output tokens for this model
        temperature: Default temperature (can be overridden)
        description: Human-readable description for documentation
    """
    name: str
    max_tokens: int
    temperature: float
    description: str


class LLMModel(Enum):
    """Available LLM models.
    
    Each enum value contains a ModelConfig with the model's settings.
    Add new models here as they become available.
    """
    
    # Primary models
    GPT4O_DEV = ModelConfig(
        name="gpt-40-dev",
        max_tokens=16384,
        temperature=0.1,
        description="Primary model for complex code generation tasks"
    )
    
    GPT35_TURBO = ModelConfig(
        name="gpt-35-turbo",
        max_tokens=4096,
        temperature=0.0,
        description="Fast model for simple tasks and summarization"
    )
    
    # Add more models as needed:
    # GPT4_TURBO = ModelConfig(
    #     name="gpt-4-turbo",
    #     max_tokens=8192,
    #     temperature=0.1,
    #     description="Balanced model for general use"
    # )
    
    @property
    def config(self) -> ModelConfig:
        """Get the model's configuration."""
        return self.value
    
    @property
    def model_name(self) -> str:
        """Get the API model name."""
        return self.value.name


@dataclass(frozen=True)
class EmbeddingsModelConfig:
    """Configuration for embeddings models."""
    name: str
    dimensions: int
    description: str


class EmbeddingsModel(Enum):
    """Available embeddings models."""
    
    ADA_002 = EmbeddingsModelConfig(
        name="text-embedding-ada-002-2-gs",
        dimensions=1536,
        description="Primary embeddings model"
    )
    
    @property
    def config(self) -> EmbeddingsModelConfig:
        """Get the model's configuration."""
        return self.value
    
    @property
    def model_name(self) -> str:
        """Get the API model name."""
        return self.value.name


# Default models for each instance type
DEFAULT_LLM_MODEL = LLMModel.GPT4O_DEV
DEFAULT_EMBEDDINGS_MODEL = EmbeddingsModel.ADA_002
