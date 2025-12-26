"""Local file system storage implementation."""

import shutil
import uuid
from pathlib import Path

import aiofiles
import aiofiles.os
from loguru import logger

from app.config.settings import settings
from app.core.storage.base import BaseStorage


class FileStorage(BaseStorage):
    """File system storage implementation.
    
    Stores files in: {base_path}/{project_id}/{file_type}/{filename}
    """
    
    def __init__(self, base_path: Path | None = None):
        """Initialize file storage.
        
        Args:
            base_path: Base directory for storage. Defaults to settings.PROJECT_STORAGE_PATH
        """
        self.base_path = base_path or settings.get_storage_path()
    
    def _get_file_path(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
    ) -> Path:
        """Get full path for a file."""
        return self.base_path / str(project_id) / file_type / filename
    
    def _get_project_path(self, project_id: uuid.UUID) -> Path:
        """Get path for a project folder."""
        return self.base_path / str(project_id)
    
    def _get_file_type_path(self, project_id: uuid.UUID, file_type: str) -> Path:
        """Get path for a file type folder within a project."""
        return self.base_path / str(project_id) / file_type
    
    async def _ensure_directory(self, path: Path) -> None:
        """Ensure directory exists."""
        path.parent.mkdir(parents=True, exist_ok=True)
    
    async def save(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
        content: bytes,
    ) -> str:
        """Save file to local filesystem."""
        file_path = self._get_file_path(project_id, file_type, filename)
        await self._ensure_directory(file_path)
        
        async with aiofiles.open(file_path, "wb") as f:
            await f.write(content)
        
        logger.debug(f"Saved file: {file_path}")
        return str(file_path)
    
    async def get(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
    ) -> bytes:
        """Retrieve file from local filesystem."""
        file_path = self._get_file_path(project_id, file_type, filename)
        
        if not file_path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")
        
        async with aiofiles.open(file_path, "rb") as f:
            return await f.read()
    
    async def delete(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
    ) -> bool:
        """Delete file from local filesystem."""
        file_path = self._get_file_path(project_id, file_type, filename)
        
        if not file_path.exists():
            return False
        
        await aiofiles.os.remove(file_path)
        logger.debug(f"Deleted file: {file_path}")
        return True
    
    async def exists(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
    ) -> bool:
        """Check if file exists in local filesystem."""
        file_path = self._get_file_path(project_id, file_type, filename)
        return file_path.exists()
    
    async def delete_project_folder(self, project_id: uuid.UUID) -> bool:
        """Delete entire project folder and all its contents."""
        project_path = self._get_project_path(project_id)
        
        if not project_path.exists():
            return False
        
        # Use shutil.rmtree for recursive deletion (sync, but fast for local fs)
        shutil.rmtree(project_path)
        logger.info(f"Deleted project storage folder: {project_path}")
        return True
    
    async def cleanup_empty_folders(
        self,
        project_id: uuid.UUID,
        file_type: str,
    ) -> None:
        """Remove empty file type folder and project folder if empty."""
        file_type_path = self._get_file_type_path(project_id, file_type)
        project_path = self._get_project_path(project_id)
        
        # Remove file type folder if empty
        if file_type_path.exists() and not any(file_type_path.iterdir()):
            file_type_path.rmdir()
            logger.debug(f"Removed empty folder: {file_type_path}")
        
        # Remove project folder if empty
        if project_path.exists() and not any(project_path.iterdir()):
            project_path.rmdir()
            logger.debug(f"Removed empty project folder: {project_path}")
