"""Common API schemas for request/response formatting."""

from datetime import datetime
from typing import Generic, TypeVar

from pydantic import BaseModel, Field

T = TypeVar("T")


class APIError(BaseModel):
    """Standard error detail format."""
    
    code: str = Field(..., description="Error code for programmatic handling")
    message: str = Field(..., description="Human-readable error message")
    details: str | None = Field(default=None, description="Additional error details")
    field: str | None = Field(default=None, description="Field that caused the error, if applicable")


class APIResponse(BaseModel, Generic[T]):
    """Standard API response wrapper.
    
    All API responses are wrapped in this format for consistency.
    
    Success Response:
    {
        "success": true,
        "data": { ... },
        "error": null,
        "timestamp": "2025-12-02T10:30:00Z"
    }
    
    Error Response:
    {
        "success": false,
        "data": null,
        "error": {
            "code": "PROJECT_NOT_FOUND",
            "message": "Project not found: abc-123",
            "details": null
        },
        "timestamp": "2025-12-02T10:30:00Z"
    }
    """
    
    success: bool = Field(..., description="Whether the request was successful")
    data: T | None = Field(default=None, description="Response data on success")
    error: APIError | None = Field(default=None, description="Error details on failure")
    timestamp: datetime = Field(default_factory=datetime.utcnow, description="Response timestamp")
    
    @classmethod
    def ok(cls, data: T) -> "APIResponse[T]":
        """Create a successful response."""
        return cls(success=True, data=data, error=None)
    
    @classmethod
    def fail(
        cls,
        code: str,
        message: str,
        details: str | None = None,
        field: str | None = None,
    ) -> "APIResponse[None]":
        """Create a failure response."""
        return cls(
            success=False,
            data=None,
            error=APIError(code=code, message=message, details=details, field=field),
        )
