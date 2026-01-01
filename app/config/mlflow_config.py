"""MLflow tracing configuration.

Provides automatic tracing for LangChain/LangGraph when enabled.
Uses lazy import to avoid overhead when disabled.
"""

from loguru import logger

from app.config.settings import settings


def configure_mlflow() -> None:
    """Configure MLflow tracing if enabled.
    
    When MLFLOW_ENABLED=false:
        - No mlflow import happens
        - No overhead
        
    When MLFLOW_ENABLED=true:
        - Sets tracking URI and experiment
        - Enables autolog for LangChain/LangGraph
    """
    if not settings.MLFLOW_ENABLED:
        logger.info("MLflow tracing: DISABLED")
        return
    
    # Lazy import - only when enabled
    import mlflow
    
    if settings.MLFLOW_TRACKING_URI:
        mlflow.set_tracking_uri(settings.MLFLOW_TRACKING_URI)
    
    mlflow.set_experiment(settings.MLFLOW_EXPERIMENT_NAME)
    mlflow.langchain.autolog()
    
    logger.info(
        f"MLflow tracing: ENABLED "
        f"(experiment={settings.MLFLOW_EXPERIMENT_NAME}, "
        f"uri={settings.MLFLOW_TRACKING_URI or 'local'})"
    )
