"""In-memory usage statistics tracking for LLM and Embeddings."""

from dataclasses import dataclass
from datetime import datetime, timezone
from threading import Lock


@dataclass
class InstanceStats:
    """Statistics for a single LLM/Embeddings instance.
    
    Attributes:
        instance_name: Name of the instance (e.g., "codegen", "docgen")
        resource_type: Type of resource ("llm" or "embeddings")
        first_request_time: Timestamp of first request (UTC)
        last_request_time: Timestamp of most recent request (UTC)
        total_requests: Total number of requests made
        successful_requests: Number of successful requests
        failed_requests: Number of failed requests
    """
    instance_name: str
    resource_type: str  # "llm" or "embeddings"
    first_request_time: datetime | None = None
    last_request_time: datetime | None = None
    total_requests: int = 0
    successful_requests: int = 0
    failed_requests: int = 0
    
    # Token usage
    total_tokens: int = 0
    prompt_tokens: int = 0
    completion_tokens: int = 0


class LLMStats:
    """Thread-safe usage statistics tracker.
    
    Tracks request counts, timing, and token usage for all LLM and Embeddings instances.
    Stats are stored in-memory and reset on server restart.
    
    Usage:
        stats = LLMStats()
        stats.record_request("codegen", "llm", success=True, tokens={"total": 100, "prompt": 80, "completion": 20})
        all_stats = stats.get_all_stats()
        aggregate = stats.get_aggregate_stats()
    """
    
    def __init__(self):
        self._stats: dict[str, InstanceStats] = {}
        self._lock = Lock()
    
    def record_request(
        self, 
        instance_name: str, 
        resource_type: str,  # "llm" or "embeddings"
        success: bool,
        tokens: dict[str, int] | None = None
    ) -> None:
        """Record a request for an instance.
        
        This method is called internally by OAuthLLMClient and OAuthEmbeddings.
        Services do not need to call this directly.
        
        Args:
            instance_name: Name of the instance (e.g., "codegen")
            resource_type: "llm" or "embeddings"
            success: Whether the request succeeded
            tokens: Optional dict with "total", "prompt", "completion" token counts
        """
        with self._lock:
            key = f"{instance_name}:{resource_type}"
            
            if key not in self._stats:
                self._stats[key] = InstanceStats(
                    instance_name=instance_name,
                    resource_type=resource_type,
                )
            
            stats = self._stats[key]
            now = datetime.now(timezone.utc)
            
            if stats.first_request_time is None:
                stats.first_request_time = now
            stats.last_request_time = now
            stats.total_requests += 1
            
            if success:
                stats.successful_requests += 1
            else:
                stats.failed_requests += 1
            
            # Record tokens if provided
            if tokens:
                stats.total_tokens += tokens.get("total", 0)
                stats.prompt_tokens += tokens.get("prompt", 0)
                stats.completion_tokens += tokens.get("completion", 0)
    
    def get_all_stats(self) -> list[InstanceStats]:
        """Get stats for all instances.
        
        Returns:
            List of InstanceStats objects, one per instance/resource_type combo
        """
        with self._lock:
            return list(self._stats.values())
    
    def get_instance_stats(self, instance_name: str) -> list[InstanceStats]:
        """Get stats for a specific instance (both LLM and embeddings).
        
        Args:
            instance_name: Name of the instance (e.g., "codegen")
            
        Returns:
            List of InstanceStats for this instance (LLM and embeddings)
        """
        with self._lock:
            return [
                s for s in self._stats.values() 
                if s.instance_name == instance_name
            ]
    
    def get_aggregate_stats(self) -> dict:
        """Get aggregate stats across all instances.
        
        Returns:
            Dictionary with:
            - total_requests: Total across all instances
            - successful_requests: Total successful
            - failed_requests: Total failed
            - success_rate: Percentage of successful requests (0-100)
            - total_tokens: Sum of all tokens used
            - prompt_tokens: Sum of prompt tokens
            - completion_tokens: Sum of completion tokens
        """
        with self._lock:
            total = sum(s.total_requests for s in self._stats.values())
            successful = sum(s.successful_requests for s in self._stats.values())
            failed = sum(s.failed_requests for s in self._stats.values())
            
            total_tokens = sum(s.total_tokens for s in self._stats.values())
            prompt_tokens = sum(s.prompt_tokens for s in self._stats.values())
            completion_tokens = sum(s.completion_tokens for s in self._stats.values())
            
            return {
                "total_requests": total,
                "successful_requests": successful,
                "failed_requests": failed,
                "success_rate": round((successful / total * 100), 2) if total > 0 else 0.0,
                "total_tokens": total_tokens,
                "prompt_tokens": prompt_tokens,
                "completion_tokens": completion_tokens,
            }
