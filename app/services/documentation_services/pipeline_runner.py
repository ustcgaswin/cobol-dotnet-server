import os
import faiss
import numpy as np
from pathlib import Path
from typing import Dict, List, Any

# Import all your parsers
from app.core.parsers.cobol_parser import CobolParser
from app.core.parsers.jcl_parser import JCLParser
from app.core.parsers.copybook_parser import CopybookParser
from app.core.parsers.pli_parser import PLIParser
from app.core.parsers.pli_copybook_parser import PLICopybookParser
from app.core.parsers.rexx_parser import REXXParser
from app.core.parsers.parmlib_parser import PARMLIBParser
from app.core.parsers.dclgen_parser import DclgenParser
from app.core.parsers.bind_parser import BindParser
from app.core.parsers.ca7_parser import CA7Parser
from app.core.parsers.assembly_parser import AssemblyParser
from app.core.parsers.flatfile_parser import CSVParser, FixedLengthParser

from app.config.llm_config import embeddings
from app.core.vector_store import FAISSVectorStore
from app.config.settings import settings
from langchain_core.documents import Document

# Import Orchestration components
from .dependency_mapper import DependencyMapper
from .llm_manager import LLMManager
from .report_generator import ReportGenerator
from .context_provider import ContextProvider

class PipelineRunner:
    def __init__(self, project_dir: str, output_dir: str):
        self.project_dir = Path(project_dir)
        self.output_dir = Path(output_dir)
        
        # 1. Initialize Graph and Logic Managers
        self.mapper = DependencyMapper()
        
        # 2. Initialize Parsers
        self.parsers = {
            ".cbl": CobolParser(),
            ".cob": CobolParser(),
            ".pli": PLIParser(),
            ".pli_copybook": PLICopybookParser(),
            ".jcl": JCLParser(),
            ".cpy": CopybookParser(),
            ".copy": CopybookParser(),
            ".rexx": REXXParser(),
            ".asm": AssemblyParser(),
            ".csv": CSVParser(),
            ".bind": BindParser(),
            ".dclgen": DclgenParser(),
            ".parmlib": PARMLIBParser(),
            ".ca7": CA7Parser()
        }

        # 3. FAISS & Embedding Setup
        self.vector_store = FAISSVectorStore(
            embeddings=embeddings,
            index_path=self.output_dir / "faiss_index"
        )
        self.index = faiss.IndexFlatL2(384) 
        self.summary_store = [] 

    def run_all(self):
        # STAGE 1: Deterministic Parsing
        print(f"--- Stage 1: Parsing Files in {self.project_dir} ---")
        parsed_jsons = self._trigger_parsers()
        
        # STAGE 2: Build Global Dependency Graph
        print("--- Stage 2: Building Dependency Map ---")
        graph = self.mapper.build_map(parsed_jsons)
        
        # STAGE 3: Setup LLM Context (Now that graph is built)
        ctx_provider = ContextProvider(graph)
        self.llm_worker = LLMManager(ctx_provider) # LLMManager now has graph access
        
        # STAGE 4: Deep Logic Analysis (Parallelized inside llm_worker)
        print("--- Stage 3: Deep Dive Logic Analysis ---")
        program_reports = {}
        
        for node, attributes in graph.nodes(data=True):
            # Only perform deep-dive on logic-heavy files
            if attributes.get('type') in ['cobol', 'pli', 'assembly']:
                print(f"Analyzing Logic: {node}")
                
                # This function chunks 10k lines, calls LLM, and returns a ProgramReport
                report = self.llm_worker.document_large_file(node)
                program_reports[node] = report
                
                # INDEXING: Store paragraph summaries in FAISS for future RAG
                self._index_summaries(report)
                
        # STAGE 5: Consolidated Report Generation
        print("--- Stage 4: Generating Master Manual ---")
        generator = ReportGenerator(
            output_dir=self.output_dir, 
            mapper_graph=graph, 
            llm_manager=self.llm_worker
        )
        
        master_file = generator.create_consolidated_report(program_reports)
        
        print(f"Documentation Pipeline Complete! Final Report: {master_file}")
        return master_file

    def _trigger_parsers(self) -> List[Dict]:
        """
        Crawls the project directory and dispatches files to correct parsers.
        """
        all_results = []
        
        for file_path in self.project_dir.rglob('*'):
            if file_path.is_dir(): continue
            
            ext = file_path.suffix.lower()
            parser = self.parsers.get(ext)
            
            # Special handling for files without extensions (common in PARMLIB/DCLGEN)
            if not parser:
                parser = self._guess_parser_by_content(file_path)

            if parser:
                try:
                    print(f"Parsing {file_path.name}...")
                    result = parser.parse_file(str(file_path))
                    # Ensure result has standard metadata for the Mapper
                    result['source_file'] = file_path.name
                    result['file_type'] = parser.FILE_TYPE
                    all_results.append(result)
                except Exception as e:
                    print(f"Failed to parse {file_path.name}: {e}")
        
        return all_results

    def _guess_parser_by_content(self, file_path: Path):
        """Fallback for files like PARMLIB members which often lack extensions."""
        content_sample = file_path.read_text(errors='ignore')[:1000].upper()
        if "EXEC SQL" in content_sample: return self.parsers['.dclgen']
        if "SORT FIELDS" in content_sample: return self.parsers['.parmlib']
        if "//" in content_sample and "JOB" in content_sample: return self.parsers['.jcl']
        return None

    def _index_summaries(self, program_report):
        """
        Adds paragraph-level business logic to your Azure-powered FAISS store.
        """
        if not hasattr(program_report, 'paragraph_summaries'):
            return

        documents = []
        for para in program_report.paragraph_summaries:
            content = (
                f"Program: {program_report.program_id} | "
                f"Section: {para.paragraph_name} | "
                f"Logic: {para.business_logic}"
            )
            
            # Create a LangChain Document object
            doc = Document(
                page_content=content,
                metadata={
                    "program_id": program_report.program_id,
                    "type": "logic_summary",
                    "source": program_report.program_id
                }
            )
            documents.append(doc)
        
        # 3. Add to store and save (Uses Azure OpenAI)
        if documents:
            self.vector_store.add_documents(documents)
            self.vector_store.save()
            print(f"Indexed {len(documents)} logic chunks for {program_report.program_id}")