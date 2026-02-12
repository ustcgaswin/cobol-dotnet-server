"""File processing tracker tool."""

import json
from pathlib import Path
from typing import List, Dict, Any
from langchain.tools import tool
from app.config.settings import settings
from loguru import logger

def _get_tracker_path(project_id: str) -> Path:
    """Get path to the processing tracker JSON file."""
    return Path(settings.PROJECT_ARTIFACTS_PATH) / project_id / "system_context" / "_processing_tracker.json"

def _load_tracker(path: Path) -> Dict[str, Any]:
    """Load the tracker data or return default structure."""
    if path.exists():
        try:
            return json.loads(path.read_text(encoding="utf-8"))
        except Exception as e:
            logger.error(f"Failed to load tracker: {e}")
    
    return {
        "total_files": 0,
        "processed_files": {},  # filename -> {timestamp, has_functionality, notes}
        "pending_files": []
    }

def _save_tracker(path: Path, data: Dict[str, Any]):
    """Save the tracker data to JSON."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2), encoding="utf-8")

def create_file_tracker_tools(project_id: str) -> List[Any]:
    """Factory to create file tracker tools bound to a project."""
    
def _initialize_tracker_logic(project_id: str) -> str:
    """Core logic for initializing the file tracker."""
    try:
        tracker_path = _get_tracker_path(project_id)
        summaries_path = Path(settings.PROJECT_ARTIFACTS_PATH) / project_id / "file_summaries.md"
        
        if not summaries_path.exists():
            return "Error: file_summaries.md not found. Cannot initialize tracker."
            
        # Parse file_summaries.md to get list of files
        content = summaries_path.read_text(encoding="utf-8", errors="replace")
        filenames = []
        for line in content.splitlines():
            if line.startswith("# File:"):
                fname = line.replace("# File:", "").strip()
                filenames.append(fname)
        
        data = _load_tracker(tracker_path)
        
        # Update pending files (add new ones, keep existing processed ones)
        existing_processed = set(data["processed_files"].keys())
        all_files = set(filenames)
        
        data["total_files"] = len(all_files)
        data["pending_files"] = sorted(list(all_files - existing_processed))
        
        _save_tracker(tracker_path, data)
        
        return f"Tracker initialized. {len(filenames)} total files found. {len(data['pending_files'])} pending."
        
    except Exception as e:
        logger.error(f"Tracker initialization error: {e}")
        return f"Error: {e}"

def create_file_tracker_tools(project_id: str) -> List[Any]:
    """Factory to create file tracker tools bound to a project."""
    
    @tool("initialize_file_tracker")
    def initialize_file_tracker() -> str:
        """
        Initialize the file processing tracker by reading file_summaries.md.
        Call this at the START of the analysis (Phase 2).
        """
        return _initialize_tracker_logic(project_id)

    @tool("mark_file_processed")
    def mark_file_processed(filename: str, has_functionality: bool, notes: str = "") -> str:
        """
        Mark a file as processed.
        
        Args:
            filename: The name of the file (as listed in file_summaries.md).
            has_functionality: True if business logic/functionality was cataloged. False if utility/copybook only.
            notes: Optional notes about the file.
        """
        try:
            tracker_path = _get_tracker_path(project_id)
            data = _load_tracker(tracker_path)
            
            from datetime import datetime
            
            # Normalize filename matching (simple containment check if full path mismatch)
            target = filename.strip()
            
            # If explicit match not found in pending, try to find by basename
            if target not in data["pending_files"] and target not in data["processed_files"]:
                 for p in data["pending_files"]:
                     if p.endswith(target) or target.endswith(p):
                         target = p
                         break
            
            data["processed_files"][target] = {
                "timestamp": datetime.now().isoformat(),
                "has_functionality": has_functionality,
                "notes": notes
            }
            
            if target in data["pending_files"]:
                data["pending_files"].remove(target)
                
            _save_tracker(tracker_path, data)
            
            status = "Functionality Cataloged" if has_functionality else "No Functionality (Skipped)"
            return f"Marked '{target}' as processed. Status: {status}"
            
        except Exception as e:
            logger.error(f"mark_file_processed error: {e}")
            return f"Error marking file: {e}"

    @tool("get_unprocessed_files")
    def get_unprocessed_files(limit: int = 20) -> str:
        """
        Get a list of files that have NOT yet been processed.
        Use this to find what to analyze next.
        """
        try:
            tracker_path = _get_tracker_path(project_id)
            data = _load_tracker(tracker_path)
            
            pending = data.get("pending_files", [])
            
            if not pending:
                return "No unprocessed files remaining."
                
            count = len(pending)
            preview = pending[:limit]
            
            msg = f"Found {count} unprocessed files. Showing first {len(preview)}:\n"
            msg += "\n".join([f"- {f}" for f in preview])
            
            if count > limit:
                msg += f"\n... ({count - limit} more)"
                
            return msg
            
        except Exception as e:
            logger.error(f"get_unprocessed_files error: {e}")
            return f"Error retrieving files: {e}"

    @tool("get_processing_stats")
    def get_processing_stats() -> str:
        """Get current processing statistics (total, processed, remaining)."""
        try:
            tracker_path = _get_tracker_path(project_id)
            data = _load_tracker(tracker_path)
            
            total = data.get("total_files", 0)
            processed = len(data.get("processed_files", {}))
            pending = len(data.get("pending_files", []))
            
            # Count functional vs non-functional
            functional = sum(1 for v in data["processed_files"].values() if v.get("has_functionality"))
            non_functional = processed - functional
            
            return (
                f"Processing Stats:\n"
                f"- Total Files: {total}\n"
                f"- Processed: {processed} ({functional} Functional, {non_functional} Utility/Skipped)\n"
                f"- Remaining: {pending}"
            )
            
        except Exception as e:
            return f"Error getting stats: {e}"

    return [initialize_file_tracker, mark_file_processed, get_unprocessed_files, get_processing_stats]
