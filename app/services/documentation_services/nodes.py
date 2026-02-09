# import json
# from pathlib import Path
# from docx import Document
# from loguru import logger

# from langchain_core.messages import SystemMessage, HumanMessage, ToolMessage, AIMessage
# from app.config.settings import settings
# from .renderers import RendererFactory
# from ...api.schemas.doc_models import FileSummary

# from app.services.documentation_services.prompts import (
#             COBOL_CHUNK_PROMPT, CA7_PROMPT, JCL_PROMPT, JSON_FORMAT_INSTRUCTION, EXECUTIVE_SUMMARY_PROMPT,
#             CUMULATIVE_MERGE_INSTRUCTION, REXX_CHUNK_PROMPT, BIND_PROMPT,
#             DCLGEN_PROMPT, CSV_PROMPT,FIXED_LENGTH_PROMPT, DOC_AGENT_RESEARCH_SYSTEM_PROMPT, 
#             COPYBOOK_PROMPT, PLI_CHUNK_PROMPT, PLI_COPYBOOK_PROMPT, ASSEMBLY_CHUNK_PROMPT, PARMLIB_PROMPT
#         )

# class DocAgentNodes:
#     def __init__(self, llm, analyzer, tools):
#         # FIX: Save the base LLM so it's accessible in generation nodes
#         self.llm = llm 
#         # Bind tools for the research phase
#         self.llm_with_tools = llm.bind_tools(list(tools.values()))
#         self.analyzer = analyzer
#         self.tools = tools

#     async def researcher_node(self, state: dict):
#         """Phase 1: LLM autonomously searches for code and context."""
#         filename = state["target_file"]
#         file_type = state["file_type"]
#         messages = state.get("messages", [])
        
#         # Track research iterations
#         research_count = state.get("research_iterations", 0)
#         MAX_RESEARCH_ITERATIONS = 5
        
#         # 1. INITIALIZATION (First Run)
#         if not messages:
#             mermaid = self.analyzer.generate_mermaid_diagram(max_nodes=10)
#             path_context = f"\n\nCRITICAL: Use the path '{file_type.lower()}/{filename}' for all tool calls."
            
            
#             prompt = DOC_AGENT_RESEARCH_SYSTEM_PROMPT.format(target_file=filename) + path_context
            
#             initial_messages = [
#                 SystemMessage(content=prompt),
#                 HumanMessage(content=f"System structure:\n{mermaid}\n\nStart your research into {filename}.")
#             ]
            
#             # Call LLM with the newly created messages
#             response = await self.llm_with_tools.ainvoke(initial_messages)
            
#             # Return the initial set + the AI response
#             return {
#                 "mermaid_graph": mermaid,
#                 "research_iterations": 1,
#                 "messages": initial_messages + [response]
#             }

#         # 2. CONTINUATION (Looping from tools)
#         research_count += 1
        
#         # Safety break: If we are looping too much, force stop
#         if research_count >= MAX_RESEARCH_ITERATIONS:
#             return {
#                 "messages": [HumanMessage(content="Research turn limit reached. Proceeding to write docs.")],
#                 "research_iterations": research_count,
#                 "research_complete": True
#             }
        
#         # Ensure we are passing the existing history (Fixes 400 error)
#         response = await self.llm_with_tools.ainvoke(messages)
#         # logger.info(f"the research summary {response.content[:200]}")
        
#         # operator.add handles the appending
#         return {
#             "messages": [response],
#             "research_iterations": research_count
#         }
    
#     def _extract_code_snippets(self, messages):
#         snippets = []
#         for msg in messages:
#             if isinstance(msg, ToolMessage):
#                 snippets.append(msg.content)
#             # Also catch manual reads we might have injected
#             elif isinstance(msg, HumanMessage) and "File content for" in msg.content:
#                 snippets.append(msg.content)
#         return "\n\n".join(snippets) if snippets else "No snippets found."

#     async def functional_gen_node(self, state: dict):
#         """Generate Functional JSON and Map it to the Renderer structure."""
        
#         prompt = EXECUTIVE_SUMMARY_PROMPT.format(
#             JSON_FORMAT_INSTRUCTION=JSON_FORMAT_INSTRUCTION,
#             metrics=state.get("mermaid_graph", "N/A"),
#             top_modules=state["target_file"]
#         )
        
#         res = await self.llm.ainvoke([HumanMessage(content=prompt)])
#         raw_json = self._parse(res.content)

#         # 1. FORCE CORRECT METADATA (Stop Hallucinations)
#         raw_json["filename"] = state["target_file"]
#         raw_json["type"] = state["file_type"]

#         # 2. TRANSFORM FOR RENDERER (The Bridge)
#         # We put the flat fields into the 'business_overview' dictionary the renderer expects
#         raw_json["business_overview"] = {
#             "purpose": raw_json.get("business_purpose", "N/A"),
#             "scope": raw_json.get("business_scope", []),
#             "key_data_entities": raw_json.get("data_overview", {}).get("inputs", [])
#         }
        
#         # We put the flow steps into technical_analysis so that page isn't blank
#         raw_json["technical_analysis"] = {
#             "functional_capabilities": raw_json.get("process_flow_steps", []),
#             "key_operations": [f"{i['interface']} ({i['direction']})" for i in raw_json.get("external_interfaces", [])],
#             "technical_notes": raw_json.get("business_risks", [])
#         }

#         return {"functional_json": raw_json}

#     async def technical_gen_node(self, state: dict):
#         """Generate Technical JSON and Fix Hallucinated Metadata."""
        
#         # Select prompt based on type
#         prompt_map = {
#                       "COBOL": COBOL_CHUNK_PROMPT, "CA7": CA7_PROMPT,
#                       "JCL": JCL_PROMPT, "REXX": REXX_CHUNK_PROMPT,
#                       "PLI" : PLI_CHUNK_PROMPT, "PLI_COPYBOOK" : PLI_COPYBOOK_PROMPT,
#                       "DCLGEN": DCLGEN_PROMPT, "BIND" : BIND_PROMPT , "CSV": CSV_PROMPT,
#                       "FIXED_LENGTH" : FIXED_LENGTH_PROMPT , "COPYBOOK": COPYBOOK_PROMPT,
#                       "PARMLIB" : PARMLIB_PROMPT, "ASSEMBLY" : ASSEMBLY_CHUNK_PROMPT
#                     }
#         template = prompt_map.get(state["file_type"].upper(), COBOL_CHUNK_PROMPT)
        
#         evidence = self._extract_code_snippets(state.get("messages", []))
#         prompt = template.format(
#             JSON_FORMAT_INSTRUCTION=JSON_FORMAT_INSTRUCTION,
#             CUMULATIVE_MERGE_INSTRUCTION=CUMULATIVE_MERGE_INSTRUCTION,
#             chunk=evidence, content=evidence, previous_summary="{}"
#         )
        
#         res = await self.llm.ainvoke([HumanMessage(content=prompt)])
#         parsed_json = self._parse(res.content)

#         # FORCE CORRECT METADATA (Overwrite 'file_summaries.md + filename' hallucinations)
#         parsed_json["filename"] = state["target_file"]
#         parsed_json["type"] = state["file_type"]

#         return {"technical_json": parsed_json}

#     async def render_node(self, state: dict):
#         """Phase 3: Render to DOCX."""
#         base = Path(settings.PROJECT_ARTIFACTS_PATH) / state["project_id"]
        
#         # Process both types sequentially
#         for mode in ["functional", "technical"]:
#             data = state.get(f"{mode}_json")
#             if not data: continue
            
#             try:
#                 folder = base / f"{mode}_requirements"
#                 folder.mkdir(parents=True, exist_ok=True)
                
#                 # Use our super-model to validate the reshaped JSON
#                 summary_model = FileSummary(**data)
                
#                 from docx import Document
#                 doc = Document()
#                 renderer = RendererFactory.get_renderer(state["file_type"])
                
#                 # Render using your strategy pattern
#                 renderer.render_document(doc, summary_model)
                
#                 output_path = folder / f"{state['target_file']}_{mode.upper()}.docx"
#                 doc.save(str(output_path))
#                 logger.info(f"✅ Document successfully rendered: {output_path.name}")
#             except Exception as e:
#                 logger.error(f"Error rendering {mode} for {state['target_file']}: {e}")
                
#         return state

#     def _parse(self, content: str):
#         try:
#             clean = content.replace("```json", "").replace("```", "").strip()
#             start = clean.find('{')
#             end = clean.rfind('}')
#             if start != -1 and end != -1:
#                 clean = clean[start:end+1]
#             return json.loads(clean)
#         except Exception as e:
#             logger.error(f"JSON parse error: {e}")
#             return {"purpose": "Failed to parse LLM response"}

import json
from loguru import logger
from langchain_core.messages import SystemMessage, HumanMessage, ToolMessage

# Import Prompts
from app.services.documentation_services.prompts import (
    COBOL_CHUNK_PROMPT, CA7_PROMPT, JCL_PROMPT, JSON_FORMAT_INSTRUCTION, EXECUTIVE_SUMMARY_PROMPT,
    CUMULATIVE_MERGE_INSTRUCTION, REXX_CHUNK_PROMPT, BIND_PROMPT,
    DCLGEN_PROMPT, CSV_PROMPT, FIXED_LENGTH_PROMPT, DOC_AGENT_RESEARCH_SYSTEM_PROMPT, 
    COPYBOOK_PROMPT, PLI_CHUNK_PROMPT, PLI_COPYBOOK_PROMPT, ASSEMBLY_CHUNK_PROMPT, PARMLIB_PROMPT
)

class DocAgentNodes:
    def __init__(self, llm, analyzer, tools):
        self.llm = llm 
        self.llm_with_tools = llm.bind_tools(list(tools.values()))
        self.analyzer = analyzer
        self.tools = tools

    async def researcher_node(self, state: dict):
        """Phase 1: LLM autonomously searches for code and context."""
        filename = state["target_file"]
        file_type = state["file_type"]
        messages = state.get("messages", [])
        
        research_count = state.get("iterations", 0)
        MAX_RESEARCH_ITERATIONS = 5
        
        # 1. INITIALIZATION
        if not messages:
            mermaid = self.analyzer.generate_mermaid_diagram(max_nodes=10)
            path_context = f"\n\nCRITICAL: Use the path '{file_type.lower()}/{filename}' for all tool calls."
            
            prompt = DOC_AGENT_RESEARCH_SYSTEM_PROMPT.format(target_file=filename) + path_context
            
            initial_messages = [
                SystemMessage(content=prompt),
                HumanMessage(content=f"System structure:\n{mermaid}\n\nStart your research into {filename}.")
            ]
            
            response = await self.llm_with_tools.ainvoke(initial_messages)
            
            return {
                "mermaid_graph": mermaid,
                "iterations": 1,
                "messages": initial_messages + [response]
            }

        # 2. CONTINUATION
        research_count += 1
        
        if research_count >= MAX_RESEARCH_ITERATIONS:
            return {
                "messages": [HumanMessage(content="Research turn limit reached. Proceeding to write docs.")],
                "iterations": research_count,
                "research_complete": True
            }
        
        response = await self.llm_with_tools.ainvoke(messages)
        
        return {
            "messages": [response],
            "iterations": research_count
        }
    
    def _extract_code_snippets(self, messages):
        snippets = []
        for msg in messages:
            if isinstance(msg, ToolMessage):
                snippets.append(msg.content)
            elif isinstance(msg, HumanMessage) and "File content for" in msg.content:
                snippets.append(msg.content)
        return "\n\n".join(snippets) if snippets else "No snippets found."

    async def functional_gen_node(self, state: dict):
        """Generate Functional JSON (Business Overview)."""
        
        return {} # Pass-through, handled by technical_gen_node

    async def technical_gen_node(self, state: dict):
        """Generate FULL JSON (Business + Technical) for the file."""
        
        prompt_map = {
            "COBOL": COBOL_CHUNK_PROMPT, "CA7": CA7_PROMPT,
            "JCL": JCL_PROMPT, "REXX": REXX_CHUNK_PROMPT,
            "PLI" : PLI_CHUNK_PROMPT, "PLI_COPYBOOK" : PLI_COPYBOOK_PROMPT,
            "DCLGEN": DCLGEN_PROMPT, "BIND" : BIND_PROMPT , "CSV": CSV_PROMPT,
            "FIXED_LENGTH" : FIXED_LENGTH_PROMPT , "COPYBOOK": COPYBOOK_PROMPT,
            "PARMLIB" : PARMLIB_PROMPT, "ASSEMBLY" : ASSEMBLY_CHUNK_PROMPT
        }
        template = prompt_map.get(state["file_type"].upper(), COBOL_CHUNK_PROMPT)
        
        evidence = self._extract_code_snippets(state.get("messages", []))
        
        # Format the prompt
        prompt = template.format(
            JSON_FORMAT_INSTRUCTION=JSON_FORMAT_INSTRUCTION,
            CUMULATIVE_MERGE_INSTRUCTION=CUMULATIVE_MERGE_INSTRUCTION,
            chunk=evidence, 
            content=evidence, 
            previous_summary="{}" # We are doing single-shot agentic research now
        )
        
        res = await self.llm.ainvoke([HumanMessage(content=prompt)])
        parsed_json = self._parse(res.content)

        # FORCE METADATA
        parsed_json["filename"] = state["target_file"]
        parsed_json["type"] = state["file_type"]

        # Return both keys so the state is populated regardless of mode
        return {
            "technical_json": parsed_json,
            "functional_json": parsed_json # They come from the same extraction now
        }

    async def render_node(self, state: dict):
        """
        NO-OP NODE.
        We do NOT render per-file DOCX anymore.
        The JSONs are collected by the Service to build the Master PDF.
        """
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