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
        max_tokens=4096,  # 128k context / 4k output
        temperature=0.1,
        description="Primary model for complex code generation tasks"
    )
    
    GPT35_TURBO = ModelConfig(
        name="gpt-35-turbo",
        max_tokens=4096,
        temperature=0.0,
        description="Fast model for simple tasks and summarization"
    )
    
    # Superior model for complex tasks
    GPT4_1 = ModelConfig(
        name="gpt-4.1-20250414-gs",
        max_tokens=32768,  # 1M context / 32k output
        temperature=0.1,
        description="Superior model for complex reasoning and coding"
    )

    # Efficient model for standard tasks (Default for services)
    GPT4_1_MINI = ModelConfig(
        name="gpt-4.1-mini-20250414-gs",
        max_tokens=32768,  # 1M context / 32k output
        temperature=0.1,
        description="Efficient and capable model for standard tasks"
    )

    GPT4_1_NANO = ModelConfig(
        name="gpt-4.1-nano-20250414-gs",
        max_tokens=32768,  # 1M context / 32k output
        temperature=0.1,
        description="Ultra-efficient model for simple tasks"
    )

    GPT4_TURBO = ModelConfig(
        name="gpt-4-turbo",
        max_tokens=4096,  # 128k context / 4k output
        temperature=0.1,
        description="Balanced model for general use"
    )

    CLAUDE_3_SONNET = ModelConfig(
        name="anthropic.claude-3-sonnet-20240229-v1:0",
        max_tokens=4096,  # 200k context / 4k output
        temperature=0.1,
        description="High capability model (Anthropic)"
    )

    CLAUDE_SONNET_4_5 = ModelConfig(
        name="global.anthropic.claude-sonnet-4-5-20250929-v1:0",
        max_tokens=16384,
        temperature=0.1,
        description="Next-gen Anthropic model"
    )

    GPT5 = ModelConfig(
        name="gpt-5-20250807-gs",
        max_tokens=32768,
        temperature=0.1,
        description="Next generation GPT model"
    )

    GPT5_NANO = ModelConfig(
        name="gpt-5-nano-20250807-gs",
        max_tokens=32768,
        temperature=0.1,
        description="Ultra-efficient next-gen model"
    )

    GPT4O_MINI = ModelConfig(
        name="gpt-4o-mini-20240718-dzs",
        max_tokens=16384,
        temperature=0.1,
        description="Efficient GPT-4o variant"
    )

    GPT4O_STD = ModelConfig(
        name="gpt-4o-20240806-std",
        max_tokens=16384,
        temperature=0.1,
        description="Standard GPT-4o model"
    )

    O4_MINI = ModelConfig(
        name="o4-mini-20250416-gs",
        max_tokens=32768,
        temperature=1.0, # Reasoning models often use higher temp or fixed
        description="Reasoning model mini variant"
    )

    O3 = ModelConfig(
        name="o3-20250416-gs",
        max_tokens=100000,
        temperature=1.0,
        description="Reasoning model O3"
    )

    O3_MINI = ModelConfig(
        name="o3-mini",
        max_tokens=100000,
        temperature=1.0,
        description="Reasoning model O3 Mini"
    )
    
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

    TEXT_3_LARGE = EmbeddingsModelConfig(
        name="text-embedding-3-large-1-gs",
        dimensions=3072,
        description="Large embeddings model v3"
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
