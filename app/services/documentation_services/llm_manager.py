import concurrent.futures
import json
from typing import List, Dict, Any
from pydantic import BaseModel, Field
from langchain_core.messages import HumanMessage, SystemMessage
from app.config.llm_config import llm
from loguru import logger

# --- Data Schemas for Consistent Output ---

class ParagraphSummary(BaseModel):
    paragraph_name: str
    logic_type: str = Field(description="e.g., Data Validation, Calculation, I/O")
    business_logic: str = Field(description="2-sentence summary of what this does")

class ProgramReport(BaseModel):
    program_id: str
    business_purpose: str
    language: str
    external_dependencies: List[str]
    paragraph_summaries: List[ParagraphSummary]
    potential_risk_areas: List[str]

# --- Main Manager Class ---

class LLMManager:
    def __init__(self, context_provider):
        self.cp = context_provider
        self.llm = llm

    def generate_raw_text(self, prompt: str) -> str:
        """
        Used by ReportGenerator to synthesize system-level overviews.
        """
        try:
            response = self.llm.invoke([HumanMessage(content=prompt)])
            return response.content
        except Exception as e:
            return f"Error generating text: {str(e)}"

    def document_large_file(self, program_name: str) -> ProgramReport:
        """
        Orchestrates the 10k line chunking process.
        """
        logger.info(f"Starting Deep-Dive for {program_name}")
        prog_json = self.cp.graph.nodes[program_name]['data']
        
        # Determine language for the prompt
        lang = "PL/I" if "pli" in str(prog_json.get('file_type', '')).lower() else "COBOL"
        
        # Get paragraphs from the Procedure Division
        paragraphs = prog_json.get('procedure_division', {}).get('paragraphs', [])
        
        if not paragraphs:
            logger.warning(f"No paragraphs found in {program_name}, skipping deep-dive.")
            return self._create_empty_report(program_name, lang)

        summaries = []
        logger.info(f"Queueing {len(paragraphs)} paragraphs for parallel processing...")
        
        # Parallelize the paragraph summaries (The 'Map' phase)
        print(f"--- Parallel processing {len(paragraphs)} paragraphs for {program_name} ---")
        with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
            future_to_para = {
                executor.submit(self.summarize_chunk, program_name, p, lang): p 
                for p in paragraphs
            }
            for future in concurrent.futures.as_completed(future_to_para):
                try:
                    res = future.result()
                    if res:
                        summaries.append(res)
                        logger.debug(f"Done: {program_name} -> {res.paragraph_name}")
                except Exception as e:
                    print(f"Error in paragraph worker: {e}")
        
        logger.info(f"Synthesizing final Program Report for {program_name}...")
        return self.synthesize_program_doc(program_name, summaries, lang)

    def summarize_chunk(self, program_name: str, paragraph: Dict, lang: str) -> ParagraphSummary:
        """
        The worker function: Analyzes one paragraph with dynamic tree context.
        """
        para_name = paragraph.get('name', 'UNKNOWN')
        # We assume the paragraph dict contains the 'code' snippet
        code_snippet = paragraph.get('code', '(Code not found)')

        # 1. Get filtered variable definitions for THIS paragraph ONLY
        context = self.cp.get_context_for_paragraph(program_name, code_snippet)
        logger.trace(f"Chunk Context for {para_name}: {len(context)} variable definitions injected.")
        
        prompt = f"""
        Analyze this {lang} paragraph and provide a business-focused summary.
        
        PROGRAM: {program_name}
        PARAGRAPH: {para_name}
        
        DATA CONTEXT (Relevant variable definitions):
        {json.dumps(context, indent=2)}
        
        CODE:
        {code_snippet}
        
        Task: 
        1. Identify the Logic Type (e.g. Calculation, File Access, Validation).
        2. Provide a 2-sentence Business Summary of what this does.
        
        Return strictly valid JSON matching this schema:
        {{
            "paragraph_name": "{para_name}",
            "logic_type": "string",
            "business_logic": "string"
        }}
        """
        
        try:
            response = self.llm.invoke([HumanMessage(content=prompt)])
            data = self._clean_json_response(response.content)
            return ParagraphSummary(**data)
        except Exception as e:
            logger.error(f"LLM failure in {program_name} at {para_name}: {e}")
            return ParagraphSummary(
                paragraph_name=para_name,
                logic_type="Unknown",
                business_logic=f"Summary failed: {str(e)}"
            )

    def synthesize_program_doc(self, program_name: str, summaries: List[ParagraphSummary], lang: str) -> ProgramReport:
        """
        Combines all paragraph summaries into a single cohesive program report.
        """
        # Convert summaries to a readable string for the LLM
        logic_blocks = "\n".join([f"- {s.paragraph_name}: {s.business_logic}" for s in summaries])
        
        prompt = f"""
        You are a Technical Lead. Based on the following logic blocks from {program_name}, 
        generate a high-level technical specification.
        
        LANGUAGE: {lang}
        LOGIC BLOCKS:
        {logic_blocks}
        
        Task:
        1. Determine the overall Business Purpose of the file.
        2. List any external dependencies (called programs or tables).
        3. Identify potential risk areas (complex logic).
        
        Return strictly valid JSON matching this schema:
        {{
            "program_id": "{program_name}",
            "business_purpose": "string",
            "language": "{lang}",
            "external_dependencies": ["string"],
            "potential_risk_areas": ["string"]
        }}
        """
        
        try:
            response = self.llm.invoke([HumanMessage(content=prompt)])
            data = self._clean_json_response(response.content)
            # Add the detailed paragraph summaries back into the object
            data['paragraph_summaries'] = summaries
            return ProgramReport(**data)
        except Exception as e:
            return self._create_empty_report(program_name, lang, error=str(e))

    def _clean_json_response(self, content: str) -> Dict:
        """Removes markdown backticks and parses JSON."""
        content = content.strip()
        if "```json" in content:
            content = content.split("```json")[1].split("```")[0]
        elif "```" in content:
            content = content.split("```")[1].split("```")[0]
        return json.loads(content)

    def _create_empty_report(self, name, lang, error=None) -> ProgramReport:
        return ProgramReport(
            program_id=name,
            business_purpose=f"Analysis failed or no code found. {error if error else ''}",
            language=lang,
            external_dependencies=[],
            paragraph_summaries=[],
            potential_risk_areas=[]
        )