# """
# LLM-based Assembly Parser for HLASM.
# Uses Anthropic/Azure LLM to extract business logic and technical intent.
# """

# import re
# import json
# import logging
# from pathlib import Path
# from typing import Optional

# from langchain_core.messages import SystemMessage, HumanMessage
# from app.core.parsers.base import BaseParser
# from app.config.llm_config import llm  # This is the 'llm' initialized in your config
# from app.core.exceptions import ParseError

# logger = logging.getLogger('assembly_parser')

# class AssemblyParser(BaseParser):
#     FILE_TYPE = "assembly"

#     SYSTEM_PROMPT = """
#     You are an expert in IBM z/OS Assembler Modules,
# COBOL interoperability, and legacy systems.

# The following input is a VALID and CLEAN  assembler module.
# It is NOT corrupted and does NOT contain encoding issues.

# Your task is NOT to parse syntax, but to ANALYZE and INFER
# the technical and business intent of the module.

# You may use:
# - Instruction semantics
# - Comments
# - Literals
# - Common mainframe conventions
# - Financial messaging knowledge

# You MUST produce a STRICT JSON object matching the schema below.

# IMPORTANT RULES:
# 1. Output MUST be valid JSON only.
# 2. If information is clearly present (e.g. module name, constants, offsets),
#    it MUST be extracted and MUST NOT be marked as "unknown".
# 3. If meaning is inferred, prefer a reasonable inference over "unknown".
# 4. Do NOT claim the input is corrupted or unparsable.
# 5. Confidence score must reflect certainty of inference.

# JSON OUTPUT SCHEMA (FIXED — DO NOT CHANGE KEYS):
#     {
#       "module_name": "",
#       "language": "",
#       "module_type": "assembler_module",
#       "entry_point": "",
#       "callable_from": ["cobol", "assembler"],
#       "technical_summary": "",
#       "business_summary": "",
#       "domain": "",
#       "protocol_or_format": "",
#       "data_elements": [],
#       "operations": [],
#       "constants": [],
#       "offsets": {},
#       "security_or_compliance": [],
#       "side_effects": ""
#     }

#     NOW ANALYZE THIS Assembly MODULE:


    
#     """

#     def parse_file(self, filepath: str) -> dict:
#         path = Path(filepath)
#         logger.info(f"Initiating LLM parse for Assembly: {path.name}")

#         # 1. Load file with EBCDIC awareness
#         content = self._load_file(path)

#         # 2. Call LLM
#         try:
#             response = llm.invoke([
#                 SystemMessage(content=self.SYSTEM_PROMPT),
#                 HumanMessage(content=f"NOW ANALYZE THE FOLLOWING ASSEMBLER MODULE:\n\n{content}")
#             ])

#             # 3. Clean and Parse JSON
#             raw_content = response.content.strip()
            
#             # Remove Markdown JSON blocks if LLM included them
#             if "```json" in raw_content:
#                 raw_content = raw_content.split("```json")[1].split("```")[0].strip()
#             elif "```" in raw_content:
#                  raw_content = raw_content.split("```")[1].split("```")[0].strip()

#             parsed_data = json.loads(raw_content)
            
#             # Add metadata for the tool
#             parsed_data["_source_file"] = path.name
#             parsed_data["_parsing_method"] = "llm_inference"
            
#             return parsed_data

#         except json.JSONDecodeError as e:
#             logger.error(f"LLM returned invalid JSON for {path.name}")
#             return {
#                 "error": "Failed to decode LLM JSON output",
#                 "raw_output": response.content[:500]
#             }
#         except Exception as e:
#             logger.exception(f"Unexpected error calling LLM for {path.name}")
#             raise ParseError(f"Assembly LLM Parse failed: {str(e)}")

#     # def _load_file(self, path: Path) -> str:
#     #     """Standard encoding detection logic."""
#     #     raw = path.read_bytes()
#     #     # Common Mainframe Codepages
#     #     for enc in ['cp1047', 'cp037', 'utf-8', 'ascii']:
#     #         try:
#     #             return raw.decode(enc)
#     #         except:
#     #             continue
#     #     return raw.decode('utf-8', errors='replace')

#     def _load_file(self, path: Path) -> str:
#         """
#         Improved encoding detection. 
#         Checks for keywords to verify if the decoding actually produced readable code.
#         """
#         raw = path.read_bytes()
#         keywords = [b'CSECT', b'USING', b'START', b'PRINT', b'ENTRY', b'DSECT']
        
#         # 1. First, check if the raw bytes contain EBCDIC keywords
#         # On a mainframe, 'CSECT' in cp1047 is b'\xc3\xe2\xc5\xc3\xe3'
#         is_ebcdic = any(k.decode('utf-8').encode('cp1047') in raw for k in keywords)

#         if is_ebcdic:
#             encodings = ['cp1047', 'cp037', 'utf-8']
#         else:
#             encodings = ['utf-8', 'ascii', 'cp1047']

#         for enc in encodings:
#             try:
#                 text = raw.decode(enc)
#                 # Verify heuristic: Does the decoded text actually look like HLASM?
#                 if any(kw in text.upper() for kw in ["CSECT", "USING", "START", "END"]):
#                     logger.info(f"Successfully decoded {path.name} using {enc}")
#                     return text
#             except:
#                 continue

#         # Last resort fallback
#         return raw.decode('utf-8', errors='replace')













































"""
LLM-based Assembly Parser for HLASM.
Uses Anthropic/Claude to extract business logic and technical intent.
Includes resilient EBCDIC/UTF-8 detection.
"""

import re
import json
import logging
from pathlib import Path
from typing import Optional, List

# Import ebcdic to register mainframe codecs in the Python registry
try:
    import ebcdic
    HAS_EBCDIC = True
except ImportError:
    HAS_EBCDIC = False

from langchain_core.messages import SystemMessage, HumanMessage
from app.core.parsers.base import BaseParser
from app.config.llm_config import llm  # Your LLM configuration
from app.core.exceptions import ParseError

logger = logging.getLogger('assembly_parser')

class AssemblyParser(BaseParser):
    FILE_TYPE = "assembly"

    # Your Expert HLASM Prompt
    SYSTEM_PROMPT = """
    You are an expert in IBM z/OS High Level Assembler (HLASM), COBOL interoperability,
    and legacy mainframe systems.

    Your task is to analyze the assembler code and produce a STRICT JSON object
    that summarizes the business and technical intent of the module.

    IMPORTANT RULES (MUST FOLLOW):
    1. Output MUST be valid JSON only. No explanations, no markdown, no comments.
    2. Do NOT invent information. If something is unclear, mark it as "unknown".
    3. Use only information present or strongly implied by the code and comments.
    4. Do NOT include assembly syntax in the output unless required by the schema.
    5. Keep values concise and normalized (lowercase snake_case where applicable).

    JSON OUTPUT SCHEMA (FIXED — DO NOT CHANGE KEYS):
    {
      "module_name": "",
      "language": "hlasm",
      "module_type": "assembler_module",
      "entry_point": "",
      "callable_from": ["cobol", "assembler"],
      "technical_summary": "",
      "business_summary": "",
      "domain": "",
      "protocol_or_format": "",
      "data_elements": [],
      "operations": [],
      "constants": [],
      "offsets": {},
      "security_or_compliance": [],
      "side_effects": ""
    }
    """

    def parse_file(self, filepath: str) -> dict:
        """Main entry point for parsing Assembly files via LLM."""
        path = Path(filepath)
        logger.info(f"Initiating LLM parse for Assembly: {path.name}")

        # 1. Resilient Encoding Detection
        content = self._load_file(path)

        # 2. Call the LLM (Claude via Azure Anthropic)
        try:
            response = llm.invoke([
                SystemMessage(content=self.SYSTEM_PROMPT),
                HumanMessage(content=f"NOW ANALYZE THE FOLLOWING ASSEMBLER MODULE:\n\n{content}")
            ])

            # 3. Clean up the response (Remove Markdown JSON wrappers if present)
            raw_content = response.content.strip()
            if "```json" in raw_content:
                raw_content = raw_content.split("```json")[1].split("```")[0].strip()
            elif "```" in raw_content:
                 raw_content = raw_content.split("```")[1].split("```")[0].strip()

            parsed_data = json.loads(raw_content)
            
            # Add metadata for internal tracking
            parsed_data["_source_file"] = path.name
            parsed_data["_parsing_method"] = "llm_inference"
            
            return parsed_data

        except json.JSONDecodeError as e:
            logger.error(f"LLM returned invalid JSON for {path.name}")
            return {
                "error": "LLM output was not valid JSON",
                "raw_output": response.content[:500]
            }
        except Exception as e:
            logger.exception(f"Unexpected error calling LLM for {path.name}")
            raise ParseError(f"Assembly LLM Parse failed: {str(e)}")

    def _load_file(self, path: Path) -> str:
        """
        Decodes file bytes into string. 
        Uses keyword-validation to ensure correct encoding (UTF-8 vs EBCDIC).
        """
        raw = path.read_bytes()
        
        # HLASM Keywords to validate a successful decode
        keywords = ["CSECT", "USING", "START", "PRINT", "ENTRY", "DSECT", "DC", "DS"]
        
        # Check UTF-8 first (most common for files already on a PC), then EBCDIC
        encodings = ['utf-8', 'cp1047', 'cp037', 'ascii']
        
        for enc in encodings:
            try:
                # Skip EBCDIC codecs if the package isn't installed
                if (enc.startswith('cp') or enc == 'ebcdic') and not HAS_EBCDIC:
                    continue
                    
                text = raw.decode(enc)
                
                # Validation: Does the decoded text actually contain Assembly?
                if any(kw in text.upper() for kw in keywords):
                    logger.info(f"Successfully decoded {path.name} using {enc}")
                    return text
            except (UnicodeDecodeError, LookupError):
                continue

        # Final Fallback
        logger.warning(f"No HLASM keywords found in {path.name} with standard codecs. Falling back to UTF-8.")
        return raw.decode('utf-8', errors='replace')