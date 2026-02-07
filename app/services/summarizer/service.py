"""Service for generating file summaries via LLM with async support and crash recovery."""

import asyncio
import json
import uuid
from datetime import datetime
from pathlib import Path
from dataclasses import dataclass
from typing import Type, Any

from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession

from app.config.llm import get_llm, DOCGEN, LLMModel
from app.config.settings import settings
from app.core.chunkers.assembly import AssemblyChunker
from app.core.chunkers.rexx import RexxChunker
from app.db.enums import SourceFileType
from app.db.repositories.source_file import SourceFileRepository
from app.core.exceptions import SummarizationError
# Chunkers
from app.core.chunkers.cobol import CobolChunker
from app.core.chunkers.copybook import CopybookChunker
from app.core.chunkers.pli import PliChunker
from app.core.chunkers.pli_copybook import PliCopybookChunker
from app.core.chunkers.jcl import JclChunker
from app.core.chunkers.ca7 import Ca7Chunker
from app.core.chunkers.parmlib import ParmlibChunker

# Prompts
from app.services.summarizer.prompts import (
    ASSEMBLY_CHUNK_PROMPT,
    COBOL_CHUNK_PROMPT,
    COPYBOOK_PROMPT,
    PLI_CHUNK_PROMPT,
    PLI_COPYBOOK_PROMPT,
    JCL_PROMPT,
    DCLGEN_PROMPT,
    CA7_PROMPT,
    REXX_CHUNK_PROMPT,
    PARMLIB_CHUNK_PROMPT,
)
from app.services.summarizer.generator import generate_file_summaries_md


@dataclass
class ProcessingStrategy:
    """Configuration for handling specific file types."""
    chunker_cls: Type[Any]
    prompt_template: str
    is_rolling: bool  # True = Chunk loop with history, False = Single pass
    parser_type: str  # String identifier for the parsing logic


class SummarizerService:
    """Service for generating file summaries via LLM using DB-driven strategies.
    
    Supports two call patterns:
    - run(): Non-blocking, returns immediately (for direct API calls)
    - generate(): Blocking, waits for completion (for pipeline/internal calls)
    
    Features:
    - Per-file JSON storage for crash recovery
    - Automatic skip of already-summarized files
    - Progress tracking via _status.json
    """
    
    MAX_RETRIES = 3

    def __init__(self, project_id: uuid.UUID, session: AsyncSession):
        """
        Initialize service.
        
        Args:
            project_id: The UUID of the project.
            session: SQLAlchemy AsyncSession for DB access.
        """
        self.project_id = project_id
        self.session = session
        self.repo = SourceFileRepository(session)
        self.project_storage = settings.get_storage_path() / str(project_id)
        self.output_dir = settings.get_artifacts_path() / str(project_id)
        self.summaries_dir = self.output_dir / "summaries"
        self.llm = get_llm(DOCGEN, model=LLMModel.GPT4_1_MINI)

        # Define the strategies for supported Enums
        self.strategies = {
            SourceFileType.COBOL: ProcessingStrategy(
                chunker_cls=CobolChunker,
                prompt_template=COBOL_CHUNK_PROMPT,
                is_rolling=True,
                parser_type="cobol"
            ),
            SourceFileType.COPYBOOK: ProcessingStrategy(
                chunker_cls=CopybookChunker,
                prompt_template=COPYBOOK_PROMPT,
                is_rolling=False,
                parser_type="copybook"
            ),
            SourceFileType.PLI: ProcessingStrategy(
                chunker_cls=PliChunker,
                prompt_template=PLI_CHUNK_PROMPT,
                is_rolling=True,
                parser_type="pli"
            ),
            SourceFileType.PLI_COPYBOOK: ProcessingStrategy(
                chunker_cls=PliCopybookChunker,
                prompt_template=PLI_COPYBOOK_PROMPT,
                is_rolling=False,
                parser_type="pli_copybook"
            ),
            SourceFileType.JCL: ProcessingStrategy(
                chunker_cls=JclChunker,
                prompt_template=JCL_PROMPT,
                is_rolling=False,
                parser_type="jcl"
            ),
            SourceFileType.PROC: ProcessingStrategy(
                chunker_cls=JclChunker,
                prompt_template=JCL_PROMPT,
                is_rolling=False,
                parser_type="proc"
            ),
            SourceFileType.ASSEMBLY: ProcessingStrategy(
                chunker_cls=AssemblyChunker,
                prompt_template=ASSEMBLY_CHUNK_PROMPT,
                is_rolling=True,
                parser_type="assembly"
            ),
            SourceFileType.DCLGEN: ProcessingStrategy(
                chunker_cls=CopybookChunker,
                prompt_template=DCLGEN_PROMPT,
                is_rolling=False,
                parser_type="dclgen"
            ),
            SourceFileType.CA7: ProcessingStrategy(
                chunker_cls=Ca7Chunker,
                prompt_template=CA7_PROMPT,
                is_rolling=True,
                parser_type="ca7"
            ),
            SourceFileType.REXX: ProcessingStrategy(
                chunker_cls=RexxChunker,
                prompt_template=REXX_CHUNK_PROMPT,
                is_rolling=True,
                parser_type="rexx"
            ),
            SourceFileType.PARMLIB: ProcessingStrategy(
                chunker_cls=ParmlibChunker,
                prompt_template=PARMLIB_CHUNK_PROMPT,
                is_rolling=True,
                parser_type="parmlib"
            ),
        }

    # -------------------------------------------------------------------------
    # Public API
    # -------------------------------------------------------------------------

    async def run(self) -> None:
        """Start summarization asynchronously (non-blocking).
        
        Use this for direct API calls where you don't want to block.
        Poll get_status() to check progress.
        """
        asyncio.create_task(self._execute())
        logger.info(f"Started async summarization for project {self.project_id}")

    async def generate(self) -> dict:
        """Generate file_summaries.md (blocking).
        
        Use this for pipeline/internal calls where you need to wait for completion.
        
        Returns:
            dict with output_path and file_count
        """
        return await self._execute()

    def get_status(self) -> dict:
        """Get current summarization status.
        
        Returns:
            dict with status, progress info, and any errors
        """
        final_output = self.output_dir / "file_summaries.md"
        status_file = self.summaries_dir / "_status.json"
        
        # 1. Final output exists = completed (definitive)
        if final_output.exists():
            return {
                "status": "completed",
                "progress_percent": 100,
                "total_files": self._count_summary_files(),
                "completed_files": self._count_summary_files(),
            }
        
        # 2. No status file = never started
        if not status_file.exists():
            return {"status": "pending", "progress_percent": 0}
        
        # 3. Read status file for in_progress or failed
        try:
            data = json.loads(status_file.read_text(encoding="utf-8"))
            completed_count = self._count_summary_files()
            total = data.get("total_files", 0)
            
            return {
                "status": data.get("status", "in_progress"),
                "total_files": total,
                "completed_files": completed_count,
                "progress_percent": int(completed_count / total * 100) if total > 0 else 0,
                "current_file": data.get("current_file"),
                "error": data.get("error"),
                "started_at": data.get("started_at"),
            }
        except Exception as e:
            logger.warning(f"Failed to read status file: {e}")
            return {"status": "unknown", "error": str(e)}

    # -------------------------------------------------------------------------
    # Internal Methods
    # -------------------------------------------------------------------------

    def _count_summary_files(self) -> int:
        """Count JSON files in summaries folder (excluding _status.json)."""
        if not self.summaries_dir.exists():
            return 0
        return len([f for f in self.summaries_dir.glob("*.json") if f.name != "_status.json"])

    def _get_summary_filename(self, file_type: str, filename: str) -> str:
        """Generate unique summary filename: {file_type}_{filename}.json"""
        return f"{file_type}_{filename}.json"

    def _update_status(
        self,
        status: str,
        total_files: int = None,
        current_file: str = None,
        error: str = None
    ) -> None:
        """Update _status.json in summaries folder."""
        self.summaries_dir.mkdir(parents=True, exist_ok=True)
        status_file = self.summaries_dir / "_status.json"
        
        # Read existing data to preserve fields
        existing = {}
        if status_file.exists():
            try:
                existing = json.loads(status_file.read_text(encoding="utf-8"))
            except Exception:
                pass
        
        # Update fields
        data = {
            "status": status,
            "total_files": total_files if total_files is not None else existing.get("total_files"),
            "current_file": current_file,
            "error": error,
            "started_at": existing.get("started_at") or datetime.utcnow().isoformat(),
            "updated_at": datetime.utcnow().isoformat(),
        }
        
        if status == "completed":
            data["completed_at"] = datetime.utcnow().isoformat()
        
        try:
            status_file.write_text(json.dumps(data, indent=2), encoding="utf-8")
        except Exception as e:
            logger.warning(f"Failed to write status file: {e}")

    async def _execute(self) -> dict:
        """Execute the summarization process with per-file JSON storage."""
        logger.info(f"Generating file summaries for project {self.project_id}")
        
        # Ensure summaries directory exists
        self.summaries_dir.mkdir(parents=True, exist_ok=True)
        
        # 1. Fetch files from Database
        source_files = await self.repo.get_by_project(self.project_id)
        
        if not source_files:
            logger.warning(f"No source files found in DB for project {self.project_id}")
            return {"output_path": "", "file_count": 0}

        # Filter to only files we can summarize
        processable_files = []
        for db_file in source_files:
            try:
                file_enum = SourceFileType(db_file.file_type)
                if file_enum in self.strategies:
                    processable_files.append((db_file, file_enum))
            except ValueError:
                continue

        total_files = len(processable_files)
        logger.info(f"Found {total_files} processable files in database.")
        
        # Update status to in_progress
        self._update_status("in_progress", total_files=total_files)
        
        completed_count = 0
        
        try:
            # 2. Iterate through files
            for db_file, file_enum in processable_files:
                strategy = self.strategies[file_enum]
                summary_filename = self._get_summary_filename(file_enum.value, db_file.filename)
                summary_path = self.summaries_dir / summary_filename
                
                # SKIP if already summarized (crash recovery)
                if summary_path.exists():
                    logger.debug(f"Skipping {db_file.filename} - already summarized")
                    completed_count += 1
                    continue
                
                # Update status with current file
                self._update_status("in_progress", current_file=db_file.filename)
                
                # Construct physical path
                file_path = self.project_storage / file_enum.value / db_file.filename
                
                if not file_path.exists():
                    logger.error(f"File record exists in DB but not on disk: {file_path}")
                    continue
                
                try:
                    # Summarize file
                    summary = await self._summarize_file(file_path, strategy)
                    
                    if summary:
                        # Write summary JSON immediately (crash-safe)
                        summary_path.write_text(
                            json.dumps(summary, indent=2, ensure_ascii=False),
                            encoding="utf-8"
                        )
                        completed_count += 1
                        logger.debug(f"Saved summary: {summary_filename}")
                        
                except Exception as e:
                    logger.error(f"Failed to summarize {db_file.filename}: {e}")
                    # Continue to next file, don't fail entire run

            # 3. Merge all summaries into file_summaries.md
            result = self._merge_summaries()
            
            # Update status to completed
            self._update_status("completed", total_files=total_files)
            
            return result
            
        except Exception as e:
            logger.error(f"Summarization failed: {e}")
            self._update_status("failed", error=str(e))
            raise

    def _merge_summaries(self) -> dict:
        """Merge all per-file JSON summaries into file_summaries.md."""
        summaries = []
        
        # Read all JSON files (excluding _status.json)
        for json_file in sorted(self.summaries_dir.glob("*.json")):
            if json_file.name == "_status.json":
                continue
            
            try:
                summary = json.loads(json_file.read_text(encoding="utf-8"))
                summaries.append(summary)
            except Exception as e:
                logger.warning(f"Failed to read summary {json_file.name}: {e}")
        
        if summaries:
            md_content = generate_file_summaries_md(summaries)
            output_file = self.output_dir / "file_summaries.md"
            output_file.parent.mkdir(parents=True, exist_ok=True)
            output_file.write_text(md_content, encoding="utf-8")
            
            logger.info(f"File summaries merged and saved to {output_file}")
            
            return {
                "output_path": str(output_file),
                "file_count": len(summaries),
            }
        
        return {"output_path": "", "file_count": 0}

    async def _summarize_file(self, filepath: Path, strategy: ProcessingStrategy) -> dict:
        """Generic summarizer that follows the provided strategy."""
        logger.info(f"Summarizing {strategy.parser_type} file: {filepath.name}")
        
        # SPECIAL CASE: Assembly Line Count Logic
        if strategy.parser_type == "assembly":
            content = filepath.read_text(encoding="utf-8", errors="replace")
            line_count = len(content.splitlines())
            
            if line_count < 200:
                logger.info(f"Assembly file {filepath.name} is small ({line_count} lines). Using single pass.")
                prompt = strategy.prompt_template.format(chunk=content, previous_summary="None")
                summary = await self._call_llm_with_retry(prompt)
                return self._parse_structured_summary(filepath.name, strategy.parser_type, summary)

        # Instantiate the specific chunker
        chunker = strategy.chunker_cls()
        
        # Get chunks
        if hasattr(chunker, "get_whole") and not strategy.is_rolling:
            chunks = chunker.get_whole([str(filepath)])
        else:
            chunks = chunker.chunk([str(filepath)])

        if not chunks:
            return {}

        summary = ""

        if strategy.is_rolling:
            # Rolling Summarization Logic (Cobol, PL/I, etc.)
            total_chunks = len(chunks)
            for i, chunk in enumerate(chunks):
                chunk_content = chunk.get("content", "")
                chunk_name = chunk.get("name", "")
                
                if not chunk_content.strip():
                    continue
                
                logger.debug(f"Processing chunk {i+1}/{total_chunks} for {filepath.name}")
                
                prompt = strategy.prompt_template.format(
                    chunk=f"SECTION: {chunk_name}\n\n{chunk_content}",
                    previous_summary=summary or "None"
                )
                
                # Update summary with result of current chunk
                summary = await self._call_llm_with_retry(prompt)
        else:
            # Single Pass Logic (Copybooks, JCL, etc.)
            content = chunks[0].get("content", "")
            prompt = strategy.prompt_template.format(content=content)
            summary = await self._call_llm_with_retry(prompt)

        logger.info(f"Completed summarization for {filepath.name}")
        return self._parse_structured_summary(filepath.name, strategy.parser_type, summary)

    async def _call_llm_with_retry(self, prompt: str) -> str:
        """Call LLM with exponential backoff retry."""
        for attempt in range(self.MAX_RETRIES):
            try:
                response = await self.llm.ainvoke(prompt)
                return response.content
            except Exception as e:
                if attempt == self.MAX_RETRIES - 1:
                    logger.error(f"LLM call failed after {self.MAX_RETRIES} attempts: {e}")
                    raise SummarizationError(f"LLM call failed: {e}") from e
                wait_time = 2 ** attempt
                logger.warning(f"LLM call failed, retrying in {wait_time}s: {e}")
                await asyncio.sleep(wait_time)
        return ""

    def _parse_structured_summary(self, filename: str, file_type: str, raw_summary: str) -> dict:
        """Parse raw LLM output into structured dict."""
        parsed = {
            "filename": filename,
            "type": file_type,
            "purpose": "",
            "functionalities": [],
            "key_operations": [],
            "notes": [],
            "entity": "",
            "key_fields": [],
            "register_usage": [],
            "steps": [],
            "main_datasets": [], 
            "table_name": "",
            "host_structure": [],
            "table_structure": [],
            "workload_identity": [],
            "dependencies_triggers": [],
            "operational_rules": [],
            "configuration_areas": [],
            "key_parameters": [],
        }
        
        current_section = None
        
        for line in raw_summary.split("\n"):
            line = line.strip()
            if not line:
                continue
                
            lower_line = line.lower()
            
            if lower_line.startswith("purpose:"):
                parsed["purpose"] = line.split(":", 1)[1].strip()
                current_section = "purpose"
            
            elif lower_line.startswith("workload identity:"):
                current_section = "workload_identity"
            elif lower_line.startswith("dependencies & triggers:"):
                current_section = "dependencies_triggers"
            elif lower_line.startswith("operational rules:"):
                current_section = "operational_rules"

            elif lower_line.startswith("table name:"): 
                parsed["table_name"] = line.split(":", 1)[1].strip()
            elif lower_line.startswith("host variable structure:"): 
                parsed["host_structure"] = line.split(":", 1)[1].strip()

            elif lower_line.startswith("table structure:"):
                current_section = "table_structure"
            elif lower_line.startswith("entity:"):
                parsed["entity"] = line.split(":", 1)[1].strip()
                current_section = "entity"
            elif lower_line.startswith("functionalities:"):
                current_section = "functionalities"
            elif lower_line.startswith("key operations:"):
                current_section = "key_operations"
            elif lower_line.startswith("workflow steps:"):
                current_section = "steps"
            elif lower_line.startswith("main datasets:"):
                current_section = "main_datasets"
            elif lower_line.startswith("notes:"):
                current_section = "notes"
            elif lower_line.startswith("key fields:"):
                current_section = "key_fields"
            elif lower_line.startswith("register usage:"):
                current_section = "register_usage"
            elif lower_line.startswith("configuration areas:"):
                current_section = "configuration_areas"
            elif lower_line.startswith("key parameters:"):
                current_section = "key_parameters"
            elif line.startswith("- "):
                item = line[2:].strip()
                list_sections = [
                    "functionalities", "key_operations", "notes", "key_fields",
                    "register_usage", "steps", "main_datasets", "table_structure",
                    "workload_identity", "dependencies_triggers", "operational_rules",
                    "configuration_areas", "key_parameters"
                ]
                if current_section in list_sections:
                    parsed[current_section].append(item)
            elif current_section == "purpose" and not line.endswith(":"):
                # Append continuation lines to purpose
                parsed["purpose"] += " " + line
                
        return parsed
