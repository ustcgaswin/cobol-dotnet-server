"""Project context for agent tools."""

from dataclasses import dataclass


@dataclass
class ProjectContext:
    """Context injected into tools at runtime.
    
    This is passed when invoking the agent and is NOT visible to the LLM.
    Tools access it via `runtime.context.project_id`.
    """
    project_id: str
