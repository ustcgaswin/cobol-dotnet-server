"""File Summarization Service."""

import asyncio
import re
import uuid
from pathlib import Path
from typing import Any

from loguru import logger

from app.config.llm_config import get_llm
from app.config.settings import settings
from app.core.chunkers.cobol import CobolChunker
from app.core.chunkers.copybook import CopybookChunker
from app.core.exceptions import SummarizationError
from app.services.summarizer.generator import generate_file_summaries_md
from app.services.summarizer.prompts import COBOL_CHUNK_PROMPT, COPYBOOK_PROMPT


class SummarizerService:
    """Service for generating file summaries via LLM."""
    
    MAX_RETRIES = 3
    
    def __init__(self, project_id: uuid.UUID):
        """Initialize service."""
        self.project_id = project_id
        # Fix: use get_storage_path() and append project_id
        self.project_storage = settings.get_storage_path() / str(project_id)
        self.output_dir = settings.get_artifacts_path() / str(project_id)
        self.llm = get_llm()
    
    async def generate(self) -> dict:
        """Generate file_summaries.md for the project.
        
        Returns:
            Dictionary with output path and file count
        """
        logger.info(f"Generating file summaries for project {self.project_id}")
        
        summaries = []
        
        # Process COBOL files
        cobol_files = list.copy(list((self.project_storage / "cobol").glob("*.cbl"))) + \
                     list((self.project_storage / "cobol").glob("*.cob")) + \
                     list((self.project_storage / "cobol").glob("*.cobol"))
                     
        logger.info(f"Found {len(cobol_files)} COBOL files")
        
        for filepath in cobol_files:
            try:
                summary = await self._summarize_cobol_file(filepath)
                summaries.append(summary)
            except Exception as e:
                logger.error(f"Failed to summarize {filepath.name}: {e}")
                # We continue even if one file fails
        
        logger.info(f"Completed processing {len(cobol_files)} COBOL files")
                
        # Process Copybook files
        copy_files = list.copy(list((self.project_storage / "copybook").glob("*.cpy"))) + \
                    list((self.project_storage / "copybook").glob("*.copy"))
                    
        logger.info(f"Found {len(copy_files)} Copybook files")
        
        for filepath in copy_files:
            try:
                summary = await self._summarize_copybook_file(filepath)
                summaries.append(summary)
            except Exception as e:
                logger.error(f"Failed to summarize {filepath.name}: {e}")
                
        logger.info(f"Completed processing {len(copy_files)} Copybook files")
        
        # Generate markdown
        if summaries:
            md_content = generate_file_summaries_md(summaries)
            output_file = self.output_dir / "file_summaries.md"
            output_file.parent.mkdir(parents=True, exist_ok=True)
            output_file.write_text(md_content, encoding='utf-8')
            
            logger.info(f"File summaries saved to {output_file}")
            
            return {
                "output_path": str(output_file),
                "file_count": len(summaries),
            }
        
        return {
            "output_path": "",
            "file_count": 0,
        }

    async def _summarize_cobol_file(self, filepath: Path) -> dict:
        """Rolling summarization for COBOL files."""
        logger.info(f"Summarizing COBOL file: {filepath.name}")
        chunks = CobolChunker().chunk([str(filepath)])
        
        summary = ""
        total_chunks = len(chunks)
        
        for i, chunk in enumerate(chunks):
            chunk_content = chunk.get("content", "")
            chunk_type = chunk.get("type", "")
            chunk_name = chunk.get("name", "")
            
            if not chunk_content.strip():
                continue
                
            logger.debug(f"Processing chunk {i+1}/{total_chunks} ({chunk_type}: {chunk_name})")
            
            prompt = COBOL_CHUNK_PROMPT.format(
                chunk=f"SECTION: {chunk_name}\n\n{chunk_content}",
                previous_summary=summary or "None"
            )
            
            chunk_summary = await self._call_llm_with_retry(prompt)
            # Log progress for long running files
            logger.debug(f"Completed chunk {i+1}/{total_chunks} for {filepath.name}")
            
            # Here we might want to combine/refine, but for now we replace as per design (rolling)
            summary = chunk_summary

        logger.info(f"Completed summarization for {filepath.name}")
        return self._parse_structured_summary(filepath.name, "cobol", summary)

    async def _summarize_copybook_file(self, filepath: Path) -> dict:
        """Single-pass summarization for copybooks."""
        logger.debug(f"Summarizing Copybook file: {filepath.name}")
        chunks = CopybookChunker().get_whole([str(filepath)])
        
        if not chunks:
            return {}
            
        content = chunks[0].get("content", "")
        
        prompt = COPYBOOK_PROMPT.format(content=content)
        summary = await self._call_llm_with_retry(prompt)
        
        return self._parse_structured_summary(filepath.name, "copybook", summary)

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
            "entity": "", # for copybooks
            "key_fields": [], # for copybooks
        }
        
        current_section = None
        
        for line in raw_summary.split('\n'):
            line = line.strip()
            if not line:
                continue
                
            lower_line = line.lower()
            
            if lower_line.startswith("purpose:"):
                parsed["purpose"] = line.split(":", 1)[1].strip()
                current_section = "purpose"
            elif lower_line.startswith("entity:"):
                parsed["entity"] = line.split(":", 1)[1].strip()
                current_section = "entity"
            elif lower_line.startswith("functionalities:"):
                current_section = "functionalities"
            elif lower_line.startswith("key operations:"):
                current_section = "key_operations"
            elif lower_line.startswith("notes:"):
                current_section = "notes"
            elif lower_line.startswith("key fields:"):
                current_section = "key_fields"
            elif line.startswith("- "):
                item = line[2:].strip()
                if current_section == "functionalities":
                    parsed["functionalities"].append(item)
                elif current_section == "key_operations":
                    parsed["key_operations"].append(item)
                elif current_section == "notes":
                    parsed["notes"].append(item)
                elif current_section == "key_fields":
                    parsed["key_fields"].append(item)
            elif current_section == "purpose" and not line.endswith(":"):
                # Append continuation lines to purpose
                parsed["purpose"] += " " + line
                
        return parsed
