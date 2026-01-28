import json
from pathlib import Path
from docx import Document
from loguru import logger

from langchain_core.messages import SystemMessage, HumanMessage, ToolMessage, AIMessage
from app.config.settings import settings
from .renderers import RendererFactory
from .models import FileSummary

class DocAgentNodes:
    def __init__(self, llm, analyzer, tools):
        # FIX: Save the base LLM so it's accessible in generation nodes
        self.llm = llm 
        # Bind tools for the research phase
        self.llm_with_tools = llm.bind_tools(list(tools.values()))
        self.analyzer = analyzer
        self.tools = tools

    async def researcher_node(self, state: dict):
        """Phase 1: LLM autonomously searches for code and context."""
        filename = state["target_file"]
        file_type = state["file_type"]
        messages = state.get("messages", [])
        
        # Track research iterations
        research_count = state.get("research_iterations", 0)
        MAX_RESEARCH_ITERATIONS = 5
        
        # 1. INITIALIZATION (First Run)
        if not messages:
            mermaid = self.analyzer.generate_mermaid_diagram(max_nodes=10)
            path_context = f"\n\nCRITICAL: Use the path '{file_type.lower()}/{filename}' for all tool calls."
            
            from app.services.documentation_services.prompts import DOC_AGENT_RESEARCH_SYSTEM_PROMPT
            prompt = DOC_AGENT_RESEARCH_SYSTEM_PROMPT.format(target_file=filename) + path_context
            
            initial_messages = [
                SystemMessage(content=prompt),
                HumanMessage(content=f"System structure:\n{mermaid}\n\nStart your research into {filename}.")
            ]
            
            # Call LLM with the newly created messages
            response = await self.llm_with_tools.ainvoke(initial_messages)
            
            # Return the initial set + the AI response
            return {
                "mermaid_graph": mermaid,
                "research_iterations": 1,
                "messages": initial_messages + [response]
            }

        # 2. CONTINUATION (Looping from tools)
        research_count += 1
        
        # Safety break: If we are looping too much, force stop
        if research_count >= MAX_RESEARCH_ITERATIONS:
            return {
                "messages": [HumanMessage(content="Research turn limit reached. Proceeding to write docs.")],
                "research_iterations": research_count,
                "research_complete": True
            }
        
        # Ensure we are passing the existing history (Fixes 400 error)
        response = await self.llm_with_tools.ainvoke(messages)
        
        # operator.add handles the appending
        return {
            "messages": [response],
            "research_iterations": research_count
        }
    
    def _extract_code_snippets(self, messages):
        snippets = []
        for msg in messages:
            if isinstance(msg, ToolMessage):
                snippets.append(msg.content)
            # Also catch manual reads we might have injected
            elif isinstance(msg, HumanMessage) and "File content for" in msg.content:
                snippets.append(msg.content)
        return "\n\n".join(snippets) if snippets else "No snippets found."

    async def functional_gen_node(self, state: dict):
        """Generate Functional JSON."""
        from app.services.documentation_services.prompts import EXECUTIVE_SUMMARY_PROMPT, JSON_FORMAT_INSTRUCTION
        
        prompt = EXECUTIVE_SUMMARY_PROMPT.format(
            JSON_FORMAT_INSTRUCTION=JSON_FORMAT_INSTRUCTION,
            metrics=state.get("mermaid_graph", "N/A"),
            top_modules=state["target_file"]
        )
        
        # FIX: Wrap string in a list of messages to avoid "at least one message required"
        res = await self.llm.ainvoke([HumanMessage(content=prompt)])
        return {"functional_json": self._parse(res.content)}

    async def technical_gen_node(self, state: dict):
        """Generate Technical JSON based on File Type."""
        from app.services.documentation_services.prompts import (
            COBOL_CHUNK_PROMPT, CA7_PROMPT, JCL_PROMPT, BIND_PROMPT, 
            PLI_CHUNK_PROMPT, REXX_CHUNK_PROMPT, JSON_FORMAT_INSTRUCTION, 
            CUMULATIVE_MERGE_INSTRUCTION
        )
        
        prompt_map = {
            "COBOL": COBOL_CHUNK_PROMPT, "CA7": CA7_PROMPT, "JCL": JCL_PROMPT,
            "BIND": BIND_PROMPT, "PLI": PLI_CHUNK_PROMPT, "REXX": REXX_CHUNK_PROMPT
        }
        
        file_type = state["file_type"].upper()
        template = prompt_map.get(file_type, COBOL_CHUNK_PROMPT)
        
        code_snippets = self._extract_code_snippets(state.get("messages", []))
        
        prompt = template.format(
            JSON_FORMAT_INSTRUCTION=JSON_FORMAT_INSTRUCTION,
            CUMULATIVE_MERGE_INSTRUCTION=CUMULATIVE_MERGE_INSTRUCTION,
            chunk=code_snippets,
            content=code_snippets,
            previous_summary="{}"
        )
        
        # FIX: Wrap string in a list of messages
        res = await self.llm.ainvoke([HumanMessage(content=prompt)])
        return {"technical_json": self._parse(res.content)}

    async def render_node(self, state: dict):
        """Phase 3: Render to DOCX."""
        base = Path(settings.PROJECT_ARTIFACTS_PATH) / state["project_id"]
        filename = state["target_file"]
        
        for mode in ["functional", "technical"]:
            data = state.get(f"{mode}_json", {})
            if not data or "business_overview" not in data and "technical_analysis" not in data:
                # If LLM returned bad JSON, skip rendering
                continue
            
            try:
                folder = base / f"{mode}_requirements"
                folder.mkdir(parents=True, exist_ok=True)
                
                summary = FileSummary(**data)
                doc = Document()
                renderer = RendererFactory.get_renderer(state["file_type"])
                renderer.render_document(doc, summary)
                
                output_path = folder / f"{filename}_{mode.upper()}.docx"
                doc.save(str(output_path))
                logger.info(f"✅ Created {mode} doc for {filename}")
            except Exception as e:
                logger.error(f"Render Error for {filename}: {e}")
        
        return state

    def _parse(self, content: str):
        try:
            clean = content.replace("```json", "").replace("```", "").strip()
            start = clean.find('{')
            end = clean.rfind('}')
            if start != -1 and end != -1:
                clean = clean[start:end+1]
            return json.loads(clean)
        except Exception as e:
            logger.error(f"JSON parse error: {e}")
            return {"purpose": "Failed to parse LLM response"}