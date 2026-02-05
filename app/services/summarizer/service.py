
import asyncio
import uuid
from pathlib import Path
from dataclasses import dataclass
from typing import Type, Any, Optional

from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession

from app.config.llm import get_llm, DOCGEN
from app.config.settings import settings
from app.core.chunkers.assembly import AssemblyChunker
from app.core.chunkers.rexx import RexxChunker
from app.db.enums import SourceFileType
from app.db.models.source_file import SourceFile
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
    """Service for generating file summaries via LLM using DB-driven strategies."""
    
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
        self.llm = get_llm(DOCGEN)

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
                is_rolling=True, # Will be determined dynamically
                parser_type="assembly"
            ),

             # NEW Strategy for DCLGEN
            SourceFileType.DCLGEN: ProcessingStrategy(
                chunker_cls=CopybookChunker, # DCLGEN can be treated like copybooks
                prompt_template=DCLGEN_PROMPT,
                is_rolling=False,            # DCLGEN is always single-pass
                parser_type="dclgen"
            ),

            SourceFileType.CA7: ProcessingStrategy(
                chunker_cls=Ca7Chunker,
                prompt_template=CA7_PROMPT,
                is_rolling=True,  # CA-7 reports are often massive
                parser_type="ca7"
            ),
            SourceFileType.REXX: ProcessingStrategy(
                chunker_cls=RexxChunker,
                prompt_template=REXX_CHUNK_PROMPT,  # Your REXX chunker
                is_rolling=True,
                 # or False, depending on REXX file size patterns
                parser_type="rexx"
            ),

            # Parmlib strategy - chunked by parameter groups
            SourceFileType.PARMLIB: ProcessingStrategy(
                chunker_cls=ParmlibChunker,
                prompt_template=PARMLIB_CHUNK_PROMPT,
                is_rolling=True,  # Rolling summarization for large parmlib files
                parser_type="parmlib"
            ),
        }

    async def generate(self) -> dict:
        """Generate file_summaries.md for the project based on DB records."""
        logger.info(f"Generating file summaries for project {self.project_id}")
        
        # 1. Fetch files from Database
        source_files = await self.repo.get_by_project(self.project_id)
        
        if not source_files:
            logger.warning(f"No source files found in DB for project {self.project_id}")
            return {"output_path": "", "file_count": 0}

        logger.info(f"Found {len(source_files)} files in database.")
        
        summaries = []

        # 2. Iterate through DB records
        for db_file in source_files:
            # Map DB enum string/value to Enum object if necessary, or use directly
            # Assuming db_file.file_type is the string value of the Enum
            try:
                file_enum = SourceFileType(db_file.file_type)
            except ValueError:
                logger.debug(f"Skipping unknown file type: {db_file.file_type}")
                continue

            # Check if we have a strategy for this file type
            if file_enum not in self.strategies:
                logger.debug(f"No summarization strategy for {file_enum.value}, skipping.")
                continue

            # Construct physical path: storage / file_type / filename
            # Note: Ensure this matches your actual storage structure
            file_path = self.project_storage / file_enum.value / db_file.filename

            if not file_path.exists():
                logger.error(f"File record exists in DB but not on disk: {file_path}")
                continue

            try:
                # 3. Process using the generic handler
                summary = await self._summarize_file(file_path, self.strategies[file_enum])
                if summary:
                    summaries.append(summary)
            except Exception as e:
                logger.error(f"Failed to summarize {db_file.filename}: {e}")

        # 4. Generate Markdown
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
        
        return {"output_path": "", "file_count": 0}

    async def _summarize_file(self, filepath: Path, strategy: ProcessingStrategy) -> dict:
        """Generic summarizer that follows the provided strategy."""
        logger.info(f"Summarizing {strategy.parser_type} file: {filepath.name}")
        
        
        # Handle traditional chunker-based processing
         # 1. SPECIAL CASE: Assembly Line Count Logic
        if strategy.parser_type == "assembly":
            content = filepath.read_text(encoding='utf-8', errors='replace')
            line_count = len(content.splitlines())
            
            if line_count < 200:
                logger.info(f"Assembly file {filepath.name} is small ({line_count} lines). Using single pass.")
                prompt = strategy.prompt_template.format(chunk=content, previous_summary="None")
                summary = await self._call_llm_with_retry(prompt)
                return self._parse_structured_summary(filepath.name, strategy.parser_type, summary)

        # Instantiate the specific chunker
        chunker = strategy.chunker_cls()
        
        # Get chunks (API differs slightly based on chunker implementation, usually .chunk or .get_whole)
        # Standardizing on passing a list of file paths
        if hasattr(chunker, 'get_whole') and not strategy.is_rolling:
            chunks = chunker.get_whole([str(filepath)])
        else:
            chunks = chunker.chunk([str(filepath)])

        if not chunks:
            return {}

        summary = ""

        if strategy.is_rolling:
            # --- Rolling Summarization Logic (Cobol, PL/I) ---
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
            # --- Single Pass Logic (Copybooks) ---
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
            "entity": "", # for copybooks
            "key_fields": [], # for copybooks
            "register_usage": [], # for assembly 
            "steps": [],
            "main_datasets": [], 
            "table_name": "",
            "host_structure": [],
            "table_structure": [],
            "workload_identity": [], # for CA7
            "dependencies_triggers": [], # for CA7
            "operational_rules": [], # for CA7
            "configuration_areas": [],   # For parmlib
            "key_parameters": [],        # For parmlib
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
            
             # CA-7: Detect Identity section
            elif lower_line.startswith("workload identity:"):
                current_section = "workload_identity"
            # CA-7: Detect Dependencies/Triggers section
            elif lower_line.startswith("dependencies & triggers:"):
                current_section = "dependencies_triggers"
            # CA-7: Detect Rules section
            elif lower_line.startswith("operational rules:"):
                current_section = "operational_rules"

            elif lower_line.startswith("table name:"): 
                parsed["table_name"] = line.split(":", 1)[1].strip()
            elif lower_line.startswith("host variable structure:"): 
                parsed["host_structure"] = line.split(":", 1)[1].strip()

            elif lower_line.startswith("table structure:"): # <--- Add this
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
            # Parmlib specific sections
            elif lower_line.startswith("configuration areas:"):
                current_section = "configuration_areas"
            elif lower_line.startswith("key parameters:"):
                current_section = "key_parameters"
            elif line.startswith("- "):
                item = line[2:].strip()
                # Define all sections that expect a list of bullet points
                list_sections = ["functionalities", "key_operations", "notes", "key_fields", "register_usage", "steps", "main_datasets", "table_structure", "workload_identity", "dependencies_triggers", "operational_rules", "configuration_areas", "key_parameters"]
                if current_section in list_sections:
                    parsed[current_section].append(item)
            elif current_section == "purpose" and not line.endswith(":"):
                            # Append continuation lines to purpose
                parsed["purpose"] += " " + line
        return parsed
