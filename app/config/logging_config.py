from __future__ import annotations

import logging
import sys
from datetime import datetime
from pathlib import Path

from loguru import logger

from app.config.settings import settings

# Hardcoded list of libraries to silence
SILENCED_LIBRARIES = ("litellm", "httpx", "httpcore", "openai", "anthropic")

# Track if logging has been configured to prevent re-initialization
_configured = False

# Log directory
LOG_DIR = Path("logs")


def _get_log_filename() -> str:
    """Generate a log filename with current date and time."""
    timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    return f"{timestamp}.log"


def configure_logging(level: str | None = None) -> None:
    """
    Configure logging with loguru, redirect standard logging to loguru,
    and silence hardcoded third-party libraries.
    - level: optional override for the minimum log level; if None the level
      is inferred from settings.ENV ("development" -> DEBUG; else INFO).
    """
    global _configured
    if _configured:
        return
    
    # Determine default level from settings if not provided
    if level is None:
        env = getattr(settings, "ENV", "development").lower()
        level = "DEBUG" if env == "development" else "INFO"
    
    # Remove default loguru handlers
    logger.remove()
    
    # Determine format and serialization based on environment
    env = getattr(settings, "ENV", "development").lower()
    is_production = env == "production"
    
    # Create logs directory if it doesn't exist
    LOG_DIR.mkdir(exist_ok=True)
    log_file_path = LOG_DIR / _get_log_filename()
    
    # File format (always human-readable, no colors)
    file_fmt = (
        "{time:YYYY-MM-DD HH:mm:ss.SSS} | "
        "{level: <8} | "
        "{name}:{function}:{line} | "
        "{message}"
    )
    
    # Add file handler (new file each server restart)
    logger.add(
        log_file_path,
        level=level,
        format=file_fmt,
        enqueue=True,
        backtrace=True,
        diagnose=True,
        encoding="utf-8",
    )
    
    if is_production:
        # Production: JSON structured logs to stdout
        logger.add(
            sys.stdout,
            level=level,
            serialize=True,  # JSON output
            enqueue=True,
            backtrace=False,
            diagnose=False,
        )
    else:
        # Development: Human-readable format to stdout
        fmt = (
            "<green>{time:YYYY-MM-DD HH:mm:ss.SSS}</green> | "
            "<level>{level: <8}</level> | "
            "<cyan>{name}</cyan>:<cyan>{function}</cyan>:<cyan>{line}</cyan> | "
            "<level>{message}</level>"
        )
        logger.add(
            sys.stdout,
            level=level,
            format=fmt,
            enqueue=True,
            backtrace=True,
            diagnose=True,
            colorize=True,
        )
    
    # Handler to forward stdlib logging to loguru
    class InterceptHandler(logging.Handler):
        def emit(self, record: logging.LogRecord) -> None:
            try:
                level_name = logger.level(record.levelname).name
            except ValueError:
                level_name = record.levelno
            
            # Find caller from where the logging call originated
            frame = logging.currentframe()
            depth = 2
            while frame and frame.f_code.co_filename == logging.__file__:
                frame = frame.f_back
                depth += 1
            
            logger.opt(depth=depth, exception=record.exc_info).log(
                level_name, record.getMessage()
            )
    
    # Convert log level string to logging level constant
    stdlib_level = logging.getLevelName(level.upper())
    
    # Replace the root handlers with InterceptHandler
    logging.root.handlers = [InterceptHandler()]
    logging.root.setLevel(stdlib_level)
    
    # Ensure uvicorn loggers also funnel through loguru
    for name in ("uvicorn", "uvicorn.error", "uvicorn.access"):
        uvicorn_logger = logging.getLogger(name)
        uvicorn_logger.handlers = [InterceptHandler()]
        uvicorn_logger.setLevel(stdlib_level)
        uvicorn_logger.propagate = False
    
    # Silence the hardcoded libraries
    for lib_name in SILENCED_LIBRARIES:
        noisy_logger = logging.getLogger(lib_name)
        noisy_logger.setLevel(logging.CRITICAL)
        noisy_logger.propagate = False
        noisy_logger.handlers = []
    
    _configured = True
    logger.info(f"Logging configured: level={level}, environment={env}, log_file={log_file_path}")
