"""File utility functions."""

from pathlib import Path


def find_files(directory: str, extensions: list[str]) -> list[str]:
    """Scan directory recursively for files with given extensions.
    
    Args:
        directory: Directory path to scan
        extensions: List of extensions to match (e.g., [".pdf", ".md"])
    
    Returns:
        Sorted list of absolute file paths matching the extensions
    """
    path = Path(directory)
    if not path.exists():
        return []
    
    files: list[str] = []
    for ext in extensions:
        # Normalize extension to have leading dot
        if not ext.startswith("."):
            ext = f".{ext}"
        files.extend(str(f.resolve()) for f in path.rglob(f"*{ext}"))
    
    return sorted(files)
