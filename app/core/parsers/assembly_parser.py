"""
HLASM Parser for IBM High Level Assembler for z/OS

Parses HLASM source files and outputs comprehensive JSON including:
- Assembly name (from TITLE or START)
- Sections (CSECT, DSECT, etc. with instructions)
- Symbols (labels, equates)
- Literals
- Dependencies (COPY files, macro calls, EXTRN symbols)
- USING/DROP scope tracking

Handles fixed & free format, EBCDIC, continuations, comments.

Usage:
    python hlasm-parser.py <hlasm_file> [-o output.json]
"""

import re
import json
import sys
import logging
import argparse
from pathlib import Path
from typing import List, Tuple, Dict, Any, Optional
from abc import ABC, abstractmethod
from langchain_core.messages import SystemMessage, HumanMessage
from app.config.llm_config import llm  # Import the initialized LLM from your config
from app.core.parsers.base import BaseParser

# Logging setup
logging.basicConfig(
    level=logging.WARNING,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger('hlasm_parser')

# =============================================================================
# Known HLASM instructions & directives (used to distinguish labels from ops)
# =============================================================================
KNOWN_OPS = {
    'A', 'AD', 'ADR', 'AE', 'AER', 'AH', 'AHI', 'AL', 'ALC', 'ALG', 'ALGR', 'ALR', 'ALY', 'AP', 'AR', 'AY',
    'BAKR', 'BAL', 'BALR', 'BAS', 'BASR', 'BC', 'BCR', 'BCT', 'BCTG', 'BCTR', 'BR', 'BRAS', 'BRC', 'BXH', 'BXLE',
    'C', 'CD', 'CDB', 'CDR', 'CE', 'CEB', 'CER', 'CLC', 'CLI', 'CLM', 'CLR', 'CP', 'CR', 'CS', 'CVB', 'CVD',
    'D', 'DD', 'DE', 'DL', 'DR', 'DSG', 'DXBR',
    'EAR', 'ED', 'EDMK', 'EX',
    'FIDBR', 'FIEBR', 'FIXBR',
    'ICM', 'IPM',
    'L', 'LA', 'LARL', 'LB', 'LCDBR', 'LCDFR', 'LCR', 'LD', 'LDEB', 'LE', 'LFPC', 'LG', 'LGR', 'LH', 'LLC',
    'LLGC', 'LLGF', 'LLGT', 'LLH', 'LM', 'LR', 'LT', 'LTR', 'LXD', 'LXE',
    'MDB', 'MDE', 'MLG', 'MP', 'MR', 'MS', 'MSG', 'MVC', 'MVN', 'MVO', 'MVZ',
    'N', 'NC', 'NI', 'NR',
    'O', 'OC', 'OI', 'OR',
    'PACK', 'PR',
    'S', 'SL', 'SLA', 'SLL', 'SLR', 'SP', 'SR', 'SRA', 'SRL', 'SRNM', 'ST', 'STC', 'STCM', 'STD', 'STE', 'STM',
    'STCKE', 'STCKC', 'STH', 'STY',
    'TM', 'TS', 'TR', 'TRT',
    'UNPK',
    'X', 'XC', 'XI', 'XR',
    'ZAP',
    # Common directives
    'CSECT', 'DSECT', 'RSECT', 'COM', 'LOCTR', 'DC', 'DS', 'EQU', 'ORG', 'USING', 'DROP', 'LTORG',
    'MACRO', 'MEND', 'MEXIT', 'MNOTE', 'COPY', 'START', 'END', 'TITLE', 'EJECT', 'SPACE', 'PRINT',
    'AMODE', 'RMODE', 'XATTR', 'ENTRY', 'EXTRN', 'WXTRN'
}

KNOWN_OPS = {op.upper() for op in KNOWN_OPS}

# =============================================================================
# Exit Codes
# =============================================================================

class ExitCode:
    SUCCESS = 0
    FILE_NOT_FOUND = 1
    EMPTY_FILE = 2
    ENCODING_ERROR = 3
    PARSE_ERROR = 4
    INVALID_INPUT = 5
    OUTPUT_ERROR = 6

# =============================================================================
# Parser Registry
# =============================================================================

class ParserRegistry:
    _parsers: Dict[str, type] = {}

    @classmethod
    def register(cls, name: str):
        def decorator(parser_class):
            cls._parsers[name] = parser_class
            return parser_class
        return decorator

    @classmethod
    def all_parsers(cls) -> Dict[str, type]:
        return cls._parsers.copy()

class BaseSectionParser(ABC):
    def __init__(self):
        self._unrecognized: List[Dict[str, Any]] = []

    @abstractmethod
    def parse(self, source: str, lines: List[Tuple[int, str, str]], is_free_format: bool) -> Dict[str, Any]:
        pass

    @property
    def unrecognized(self) -> List[Dict[str, Any]]:
        return self._unrecognized

# =============================================================================
# Main Sections & Instructions Parser
# =============================================================================

@ParserRegistry.register("sections")
class AssemblySectionsParser(BaseSectionParser):
    """Handles control sections, instructions, USING/DROP tracking"""

    def parse(self, source: str, lines: List[Tuple[int, str, str]], is_free_format: bool) -> Dict[str, Any]:
        result = {'sections': []}
        current_section = {'name': 'UNNAMED', 'type': 'CSECT', 'instructions': [], 'usings': []}
        result['sections'].append(current_section)

        active_usings: Dict[str, Dict[str, Any]] = {}  # register -> {'base': str, 'start_line': int}

        # Patterns
        section_pat = re.compile(r'^\s*([A-Z0-9_]{1,63})\s+(CSECT|DSECT|RSECT|COM|LOCTR)\b', re.I)
        # Strict: optional label (1-8 chars), then mandatory operation, then operands
        inst_pat = re.compile(
            r'^\s{0,8}([A-Z0-9_]{1,8})?\s{1,}([A-Z][A-Z0-9]{0,7})\s{0,}(.*?)(?:\s{2,}.*)?$',
            re.I
        )
        using_pat = re.compile(r'USING\s+(.+?)\s*,\s*([0-9R, ]+)', re.I)
        drop_pat   = re.compile(r'DROP\s+([0-9R, ]+)', re.I)

        for line_num, text, original in lines:
            # Clean line - remove trailing comment
            if '*' in text:
                text, cmt = re.split(r'\s*\*', text, maxsplit=1)
                comment = cmt.strip() if cmt.strip() else None
            else:
                comment = None
            text = text.rstrip()

            # Section start
            m = section_pat.match(text)
            if m:
                if active_usings:
                    current_section['usings'].append({
                        'active_at_end': list(active_usings.values()),
                        'end_line': line_num - 1
                    })
                active_usings.clear()

                name = (m.group(1) or 'UNNAMED').upper()
                typ  = m.group(2).upper()
                current_section = {'name': name, 'type': typ, 'instructions': [], 'usings': []}
                result['sections'].append(current_section)
                continue

            # Instruction / Directive
            m = inst_pat.match(text)
            if m:
                label = m.group(1).upper() if m.group(1) else None
                op    = m.group(2).upper()
                ops_str = m.group(3).strip()

                is_known = op in KNOWN_OPS

                operands = self._split_operands(ops_str)

                inst = {
                    'line': line_num,
                    'label': label,
                    'op': op,
                    'operands': operands,
                    'comment': comment,
                    'category': 'instruction' if is_known else 'macro_or_unknown'
                }

                # USING / DROP special handling
                if op == 'USING':
                    um = using_pat.search(text)
                    if um:
                        base = um.group(1).strip()
                        regs_str = um.group(2)
                        regs = [r.strip() for r in re.split(r'\s*,\s*', regs_str) if r.strip()]
                        for reg in regs:
                            active_usings[reg] = {'base': base, 'start': line_num}
                        current_section['usings'].append({
                            'type': 'USING',
                            'base_expr': base,
                            'registers': regs,
                            'line': line_num
                        })
                elif op == 'DROP':
                    dm = drop_pat.search(text)
                    if dm:
                        regs_str = dm.group(1)
                        if not regs_str.strip():
                            # DROP (all)
                            dropped = list(active_usings.keys())
                            active_usings.clear()
                        else:
                            dropped = [r.strip() for r in re.split(r'\s*,\s*', regs_str) if r.strip()]
                            for r in dropped:
                                active_usings.pop(r, None)
                        current_section['usings'].append({
                            'type': 'DROP',
                            'registers': dropped,
                            'line': line_num
                        })

                current_section['instructions'].append(inst)
            else:
                self._unrecognized.append({'line': line_num, 'text': text})

        # Final active usings
        if active_usings:
            current_section['usings'].append({
                'active_at_end': list(active_usings.values()),
                'end_line': lines[-1][0] if lines else 0
            })

        # Clean up empty default section
        if len(result['sections']) > 1 and not result['sections'][0]['instructions']:
            result['sections'].pop(0)

        return result

    def _split_operands(self, s: str) -> List[Dict[str, Any]]:
        """Split operands considering parentheses and quotes"""
        if not s:
            return []

        operands = []
        parts = []
        current = []
        paren = 0

        for c in s + ',':
            if c == ',' and paren == 0:
                part = ''.join(current).strip()
                if part:
                    parts.append(part)
                current = []
            else:
                current.append(c)
                if c == '(': paren += 1
                elif c == ')': paren = max(0, paren - 1)

        for part in parts:
            tokens = re.findall(
                r"[A-Z0-9_]+|=[A-Z]?'[^']*?'|=[A-Z]\([^\)]+\)'[^']*?'|[\(\)\+\-\*/=']|[0-9A-F]+",
                part, re.I
            )
            operands.append({'raw': part, 'tokens': tokens})

        return operands

# =============================================================================
# Other Parsers (Symbols, Dependencies) - kept minimal for brevity
# =============================================================================

@ParserRegistry.register("symbols")
class SymbolsParser(BaseSectionParser):
    def parse(self, source: str, lines: List[Tuple[int, str, str]], is_free: bool) -> Dict:
        result = {'symbols': {}, 'literals': []}
        equ_pat = re.compile(r'^\s*([A-Z0-9_]+)\s+EQU\s+(.*)', re.I)
        lit_pat = re.compile(r'=[A-Z0-9]\'[^\']*\'|=[A-Z]\([^\)]+\)\'[^\']*\'', re.I)

        for ln, txt, _ in lines:
            no_cmt = re.sub(r'\s*\*.*$', '', txt).strip()
            m = equ_pat.match(no_cmt)
            if m:
                result['symbols'][m.group(1).upper()] = {
                    'type': 'equate', 'value': m.group(2).strip(), 'line': ln
                }
            for lit in lit_pat.findall(no_cmt):
                result['literals'].append({'value': lit, 'line': ln})

        return result

@ParserRegistry.register("dependencies")
class DependenciesParser(BaseSectionParser):
    def parse(self, source: str, lines: List[Tuple[int, str, str]], is_free: bool) -> Dict:
        result = {'copy_files': [], 'macros_used': [], 'externals': []}
        copy_pat = re.compile(r'^\s*(?:[A-Z0-9_]+\s+)?COPY\s+([A-Z0-9_]+)', re.I)
        ext_pat = re.compile(r'^\s*(?:[A-Z0-9_]+\s+)?(EXTRN|WXTRN)\s+(.*)', re.I)

        for ln, txt, _ in lines:
            no_cmt = re.sub(r'\s*\*.*$', '', txt).strip()
            m = copy_pat.search(no_cmt)
            if m:
                result['copy_files'].append({'name': m.group(1).upper(), 'line': ln})
            m = ext_pat.search(no_cmt)
            if m:
                typ = m.group(1).upper()
                for s in re.split(r'\s*,\s*', m.group(2)):
                    s = s.strip().upper()
                    if s:
                        result['externals'].append({'name': s, 'type': typ, 'line': ln})

        return result

# =============================================================================
# Main Parser Class
# =============================================================================

class AssemblyParser(BaseParser):
    FILE_TYPE = "assembly"

    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    MAX_FILE_SIZE = 50 * 1024 * 1024

    # --- ADD THIS SYSTEM PROMPT ---
    LLM_TECHNICAL_PROMPT = """
    You are an expert IBM Mainframe Modernization Architect. 
    Your task is to analyze HLASM (Assembly) code and extract Technical Requirements for .NET conversion.
    
    Focus on:
    1. Business Logic Rules: Detailed conditions and calculations.
    2. Register Usage: Identify which registers hold pointers to external data structures.
    3. Memory Management: Identify GETMAIN/FREEMAIN or storage modification side effects.
    4. Integration Requirements: How this module interacts with COBOL (Linkage).

    Output MUST be a strict JSON object. No markdown.
    Schema:
    {
        "technical_requirements": {
            "logic_rules": ["string"],
            "storage_analysis": "string",
            "linkage_convention": "string",
        }
    }
    """

    def __init__(self, strict: bool = False):
        self.strict = strict
        self._source = ""
        self._lines: List[Tuple[int, str, str]] = []
        self._unrecognized = []

    # def parse_file(self, filepath: str) -> Dict:
    #     path = Path(filepath)
    #     if not path.is_file():
    #         raise FileNotFoundError(f"File not found: {filepath}")

    #     raw = path.read_bytes()

    #     content, enc = self._detect_and_decode(raw)
    #     self._source = content

    #     lines_raw = content.splitlines()
    #     is_free = self._detect_format(lines_raw)
    #     self._lines = self._preprocess(lines_raw, is_free)

    #     normalized = self._normalize_source(content, is_free)

    #     result = {
    #         'source_file': path.name,
    #         'format': 'free' if is_free else 'fixed',
    #         'detected_encoding': enc
    #     }

    #     for name, cls in ParserRegistry.all_parsers().items():
    #         parser = cls()
    #         data = parser.parse(normalized, self._lines, is_free)
    #         if data:
    #             result[name] = data

            
    #         if parser.unrecognized:
    #             self._unrecognized.extend(parser.unrecognized)

    #     name = self._extract_assembly_name(normalized)
    #     if name:
    #         result['assembly_name'] = name

    #     if self._unrecognized:
    #         result['unrecognized_lines'] = self._unrecognized

    #     return result

    def parse_file(self, filepath: str) -> Dict:
        path = Path(filepath)
        if not path.is_file():
            raise FileNotFoundError(f"File not found: {filepath}")

        # 1. Load and Decode
        raw = path.read_bytes()
        content, enc = self._detect_and_decode(raw)
        self._source = content

        # 2. Preprocess
        lines_raw = content.splitlines()
        is_free = self._detect_format(lines_raw)
        self._lines = self._preprocess(lines_raw, is_free)
        normalized = self._normalize_source(content, is_free)

        # 3. Initialize Result
        result = {
            'source_file': path.name,
            'format': 'free' if is_free else 'fixed',
            'detected_encoding': enc
        }

        # 4. Run Logic-Based Parsers (Loop)
        for parser_key, parser_cls in ParserRegistry.all_parsers().items():
            parser_instance = parser_cls()
            data = parser_instance.parse(normalized, self._lines, is_free)
            
            if data:
                result[parser_key] = data
            
            # COLLECT UNRECOGNIZED LINES (Must be inside the loop)
            if parser_instance.unrecognized:
                self._unrecognized.extend(parser_instance.unrecognized)

        # 5. NEW LLM CALL (After the loop finishes)
        logger.info(f"Requesting LLM technical analysis for {path.name}")
        try:
            llm_response = llm.invoke([
                SystemMessage(content=self.LLM_TECHNICAL_PROMPT),
                HumanMessage(content=f"Analyze this HLASM source:\n\n{normalized}")
            ])
            
            raw_json = llm_response.content.strip()
            if "```json" in raw_json:
                raw_json = raw_json.split("```json")[1].split("```")[0].strip()
            
            result['llm_analysis'] = json.loads(raw_json)
            
        except Exception as e:
            logger.error(f"LLM Technical Analysis failed: {e}")
            result['llm_analysis'] = {"error": "LLM analysis failed", "details": str(e)}

        # 6. Final Metadata Extraction
        asm_name = self._extract_assembly_name(normalized)
        if asm_name:
            result['assembly_name'] = asm_name

        if self._unrecognized:
            result['unrecognized_lines'] = self._unrecognized

        return result

    def _detect_and_decode(self, raw: bytes) -> Tuple[str, str]:
        for enc in ['utf-8', 'ascii'] + self.EBCDIC_CODEPAGES:
            try:
                txt = raw.decode(enc)
                if self._looks_like_hlasm(txt):
                    return txt, enc
            except:
                pass
        return raw.decode('utf-8', errors='replace'), 'utf-8-fallback'

    def _looks_like_hlasm(self, txt: str) -> bool:
        return bool(re.search(r'\b(CSECT|DSECT|MACRO|USING|LR|L|MVC|END)\b', txt.upper()))

    def _detect_format(self, lines: List[str]) -> bool:
        long_lines = sum(1 for l in lines if len(l) > 72)
        cont_flags = sum(1 for l in lines if len(l) >= 72 and l[71].strip())
        return long_lines > 5 and cont_flags < long_lines // 2

    def _preprocess(self, lines_raw: List[str], is_free: bool) -> List[Tuple[int, str, str]]:
        processed = []
        for i, line in enumerate(lines_raw, 1):
            if not line.strip() or line.strip().startswith('*'):
                continue
            text = line[:72].rstrip() if not is_free else line.rstrip()
            processed.append((i, text, line))
        return processed

    def _normalize_source(self, content: str, is_free: bool) -> str:
        lines = content.splitlines()
        result = []
        buf = []
        for line in lines:
            line = line.rstrip()
            if not line or line.startswith('*'):
                continue
            if not is_free and len(line) >= 72 and line[71].strip():
                buf.append(line[15:].lstrip())
            else:
                if buf:
                    result.append(' '.join(buf))
                    buf = []
                result.append(line[:72].rstrip() if not is_free else line)
        if buf:
            result.append(' '.join(buf))
        return '\n'.join(result)

    def _extract_assembly_name(self, source: str) -> Optional[str]:
        m = re.search(r'^\s*([A-Z0-9_]+)\s+START\b', source, re.I | re.M)
        if m:
            return m.group(1).upper()
        m = re.search(r'TITLE\s+[\'\"](.+?)[\'\"]', source, re.I)
        if m:
            return m.group(1).strip()
        return None

# =============================================================================
# CLI Entry Point
# =============================================================================

def main():
    parser = argparse.ArgumentParser(description='HLASM source parser')
    parser.add_argument('input', help='Input HLASM file')
    parser.add_argument('-o', '--output', help='Output JSON file')
    parser.add_argument('--indent', type=int, default=2)
    args = parser.parse_args()

    try:
        p = AssemblyParser()
        result = p.parse_file(args.input)
        json_str = json.dumps(result, indent=args.indent, ensure_ascii=False)

        if args.output:
            Path(args.output).write_text(json_str, encoding='utf-8')
            print(f"Written to {args.output}")
        else:
            print(json_str)

        sys.exit(ExitCode.SUCCESS)

    except Exception as e:
        logger.exception("Fatal error")
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(ExitCode.PARSE_ERROR)

if __name__ == '__main__':
    main()






# """
# LLM-based Assembly Parser for HLASM.
# Uses Anthropic/Claude to extract business logic and technical intent.
# Includes resilient EBCDIC/UTF-8 detection.
# """

# import re
# import json
# import logging
# from pathlib import Path
# from typing import Optional, List

# # Import ebcdic to register mainframe codecs in the Python registry
# try:
#     import ebcdic
#     HAS_EBCDIC = True
# except ImportError:
#     HAS_EBCDIC = False

# from langchain_core.messages import SystemMessage, HumanMessage
# from app.core.parsers.base import BaseParser
# from app.config.llm_config import llm  # Your LLM configuration
# from app.core.exceptions import ParseError

# logger = logging.getLogger('assembly_parser')

# class AssemblyParser(BaseParser):
#     FILE_TYPE = "assembly"

#     # Your Expert HLASM Prompt
#     SYSTEM_PROMPT = """
#     You are an expert in IBM z/OS High Level Assembler (HLASM), COBOL interoperability,
#     and legacy mainframe systems.

#     Your task is to analyze the assembler code and produce a STRICT JSON object
#     that summarizes the business and technical intent of the module.

#     IMPORTANT RULES (MUST FOLLOW):
#     1. Output MUST be valid JSON only. No explanations, no markdown, no comments.
#     2. Do NOT invent information. If something is unclear, mark it as "unknown".
#     3. Use only information present or strongly implied by the code and comments.
#     4. Do NOT include assembly syntax in the output unless required by the schema.
#     5. Keep values concise and normalized (lowercase snake_case where applicable).

#     JSON OUTPUT SCHEMA (FIXED — DO NOT CHANGE KEYS):
#     {
#       "module_name": "",
#       "language": "hlasm",
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
#     """

#     def parse_file(self, filepath: str) -> dict:
#         """Main entry point for parsing Assembly files via LLM."""
#         path = Path(filepath)
#         logger.info(f"Initiating LLM parse for Assembly: {path.name}")

#         # 1. Resilient Encoding Detection
#         content = self._load_file(path)

#         # 2. Call the LLM (Claude via Azure Anthropic)
#         try:
#             response = llm.invoke([
#                 SystemMessage(content=self.SYSTEM_PROMPT),
#                 HumanMessage(content=f"NOW ANALYZE THE FOLLOWING ASSEMBLER MODULE:\n\n{content}")
#             ])

#             # 3. Clean up the response (Remove Markdown JSON wrappers if present)
#             raw_content = response.content.strip()
#             if "```json" in raw_content:
#                 raw_content = raw_content.split("```json")[1].split("```")[0].strip()
#             elif "```" in raw_content:
#                  raw_content = raw_content.split("```")[1].split("```")[0].strip()

#             parsed_data = json.loads(raw_content)
            
#             # Add metadata for internal tracking
#             parsed_data["_source_file"] = path.name
#             parsed_data["_parsing_method"] = "llm_inference"
            
#             return parsed_data

#         except json.JSONDecodeError as e:
#             logger.error(f"LLM returned invalid JSON for {path.name}")
#             return {
#                 "error": "LLM output was not valid JSON",
#                 "raw_output": response.content[:500]
#             }
#         except Exception as e:
#             logger.exception(f"Unexpected error calling LLM for {path.name}")
#             raise ParseError(f"Assembly LLM Parse failed: {str(e)}")

#     def _load_file(self, path: Path) -> str:
#         """
#         Decodes file bytes into string. 
#         Uses keyword-validation to ensure correct encoding (UTF-8 vs EBCDIC).
#         """
#         raw = path.read_bytes()
        
#         # HLASM Keywords to validate a successful decode
#         keywords = ["CSECT", "USING", "START", "PRINT", "ENTRY", "DSECT", "DC", "DS"]
        
#         # Check UTF-8 first (most common for files already on a PC), then EBCDIC
#         encodings = ['utf-8', 'cp1047', 'cp037', 'ascii']
        
#         for enc in encodings:
#             try:
#                 # Skip EBCDIC codecs if the package isn't installed
#                 if (enc.startswith('cp') or enc == 'ebcdic') and not HAS_EBCDIC:
#                     continue
                    
#                 text = raw.decode(enc)
                
#                 # Validation: Does the decoded text actually contain Assembly?
#                 if any(kw in text.upper() for kw in keywords):
#                     logger.info(f"Successfully decoded {path.name} using {enc}")
#                     return text
#             except (UnicodeDecodeError, LookupError):
#                 continue

#         # Final Fallback
#         logger.warning(f"No HLASM keywords found in {path.name} with standard codecs. Falling back to UTF-8.")
#         return raw.decode('utf-8', errors='replace')
