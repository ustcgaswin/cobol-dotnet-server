"""Abstract base class for storage implementations."""

import uuid
from abc import ABC, abstractmethod


class BaseStorage(ABC):
    """Abstract storage interface for file operations.
    
    This abstract class defines the contract for storage implementations.
    Implement this class to support different storage backends (filesystem, S3, etc.).
    """
    
    @abstractmethod
    async def save(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
        content: bytes,
    ) -> str:
        """Save file content to storage.
        
        Args:
            project_id: Project UUID
            file_type: Type of file (cobol, copybook, etc.)
            filename: Original filename
            content: File content as bytes
            
        Returns:
            Storage path/key where file was saved
        """
        pass
    
    @abstractmethod
    async def get(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
    ) -> bytes:
        """Retrieve file content from storage.
        
        Args:
            project_id: Project UUID
            file_type: Type of file
            filename: Filename to retrieve
            
        Returns:
            File content as bytes
            
        Raises:
            FileNotFoundError: If file doesn't exist
        """
        pass
    
    @abstractmethod
    async def delete(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
    ) -> bool:
        """Delete file from storage.
        
        Args:
            project_id: Project UUID
            file_type: Type of file
            filename: Filename to delete
            
        Returns:
            True if deleted, False if file didn't exist
        """
        pass
    
    @abstractmethod
    async def exists(
        self,
        project_id: uuid.UUID,
        file_type: str,
        filename: str,
    ) -> bool:
        """Check if file exists in storage.
        
        Args:
            project_id: Project UUID
            file_type: Type of file
            filename: Filename to check
            
        Returns:
            True if file exists, False otherwise
        """
        pass
    
    @abstractmethod
    async def delete_project_folder(self, project_id: uuid.UUID) -> bool:
        """Delete entire project folder and all its contents.
        
        Args:
            project_id: Project UUID
            
        Returns:
            True if deleted, False if folder didn't exist
        """
        pass
    
    @abstractmethod
    async def cleanup_empty_folders(
        self,
        project_id: uuid.UUID,
        file_type: str,
    ) -> None:
        """Remove empty file type folder after file deletion.
        
        Args:
            project_id: Project UUID
            file_type: Type folder to check and potentially remove
        """
        pass
