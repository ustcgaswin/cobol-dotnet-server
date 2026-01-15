"""Summarization exceptions."""

class SummarizationException(Exception):
    """Base exception for summarization errors."""
    pass


class SummarizationError(SummarizationException):
    """General error during summarization."""
    pass


class LLMContextError(SummarizationException):
    """Error related to LLM context window or processing."""
    pass
