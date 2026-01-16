
"""
HLASM Parser for IBM High Level Assembler for z/OS (Production Level)

Features:
- Assembly name (TITLE/START)
- Sections (CSECT/DSECT) with instructions
- Symbols, literals
- Dependencies: COPY, macros, EXTRN/WXTRN
- Enhanced: External subprogram calls (static + register-tracked indirect)
- File I/O: VSAM/Sequential (OPEN/CLOSE/GET/PUT/READ/WRITE)
- DB2 access: DSNHLI calls
- USING/DROP scope tracking

Handles fixed/free format, EBCDIC, continuations, comments.
"""

import re
import json
import sys
import logging
import argparse
from pathlib import Path
from typing import List, Tuple, Dict, Any, Optional
from abc import ABC, abstractmethod

from app.core.parsers.base import BaseParser

# Logging setup
logging.basicConfig(
    level=logging.WARNING,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger('hlasm_parser')

# =============================================================================
# Known HLASM instructions/directives (expanded list)
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
    # I/O macros (for file detection)
    'OPEN', 'CLOSE', 'GET', 'PUT', 'READ', 'WRITE', 'CHECK', 'POINT',
    # Directives
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
# Enhanced Sections Parser (with register tracking for calls)
# =============================================================================

@ParserRegistry.register("sections")
class AssemblySectionsParser(BaseSectionParser):
    """Handles control sections, instructions, USING/DROP, and register-tracked calls"""

    def parse(self, source: str, lines: List[Tuple[int, str, str]], is_free_format: bool) -> Dict[str, Any]:
        result = {'sections': []}
        current_section = {'name': 'UNNAMED', 'type': 'CSECT', 'instructions': [], 'usings': []}
        result['sections'].append(current_section)

        active_usings: Dict[str, Dict[str, Any]] = {}  # reg -> {'base': str, 'start_line': int}
        registers: Dict[str, Dict[str, Any]] = {}      # reg -> {'value': str (symbol or literal), 'last_set': int}

        section_pat = re.compile(r'^\s*([A-Z0-9_]{1,63})\s+(CSECT|DSECT|RSECT|COM|LOCTR)\b', re.I)
        inst_pat = re.compile(
            r'^\s{0,8}([A-Z0-9_]{1,8})?\s{1,}([A-Z][A-Z0-9]{0,7})\s{0,}(.*?)(?:\s{2,}.*)?$',
            re.I
        )
        using_pat = re.compile(r'USING\s+(.+?)\s*,\s*([0-9R, ]+)', re.I)
        drop_pat   = re.compile(r'DROP\s+([0-9R, ]+)', re.I)

        for line_num, text, original in lines:
            # Strip comment
            comment = None
            if '*' in text:
                text, cmt = re.split(r'\s*\*', text, maxsplit=1)
                comment = cmt.strip() if cmt.strip() else None
            text = text.rstrip().strip()

            # Section start
            m = section_pat.match(text)
            if m:
                if active_usings:
                    current_section['usings'].append({
                        'active_at_end': list(active_usings.values()),
                        'end_line': line_num - 1
                    })
                active_usings.clear()
                registers.clear()  # Reset register tracking per section

                name = (m.group(1) or 'UNNAMED').upper()
                typ  = m.group(2).upper()
                current_section = {'name': name, 'type': typ, 'instructions': [], 'usings': []}
                result['sections'].append(current_section)
                continue

            # Instruction
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

                # === USING / DROP ===
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

                # === Register Tracking for Calls (R15 is common for program address) ===
                if op == 'L' and len(operands) >= 2:
                    reg = operands[0]['raw'].upper()
                    value = operands[1]['raw'].upper()
                    if value.startswith('=V('):
                        symbol = re.sub(r'^=V\((.+?)\)$', r'\1', value).upper()
                        registers[reg] = {'type': 'V-constant', 'target': symbol, 'line': line_num}
                    elif re.match(r'^[A-Z0-9_]+$', value):  # Direct symbol load
                        registers[reg] = {'type': 'direct-symbol', 'target': value, 'line': line_num}

                # Detect BALR/BASR 14,Rx as call
                if op in ('BALR', 'BASR') and len(operands) >= 2:
                    ret_reg = operands[0]['raw'].upper()
                    target_reg = operands[1]['raw'].upper()
                    if ret_reg == '14' and target_reg in registers:
                        call_info = registers[target_reg]
                        current_section.setdefault('external_calls', []).append({
                            'target': call_info['target'],
                            'via_register': target_reg,
                            'load_type': call_info['type'],
                            'load_line': call_info['line'],
                            'call_line': line_num
                        })

                current_section['instructions'].append(inst)
            else:
                self._unrecognized.append({'line': line_num, 'text': text})

        # Final usings
        if active_usings:
            current_section['usings'].append({
                'active_at_end': list(active_usings.values()),
                'end_line': lines[-1][0] if lines else 0
            })

        if len(result['sections']) > 1 and not result['sections'][0]['instructions']:
            result['sections'].pop(0)

        return result

    # def _split_operands(self, s: str) -> List[Dict[str, Any]]:
    #     if not s:
    #         return []
    #     operands = []
    #     parts = []
    #     current = []
    #     paren = 0
    #     for c in s + ',':
    #         if c == ',' and paren == 0:
    #             part = ''.join(current).strip()
    #             if part:
    #                 parts.append(part)
    #             current = []
    #         else:
    #             current.append(c)
    #             if c == '(': paren += 1
    #             elif c == ')': paren = max(0, paren - 1)
    #     for part in parts:
    #         tokens = re.findall(
    #             r"[A-Z0-9_]+|=[A-Z]?'[^']*?'|=[A-Z]\([^\)]+\)'[^']*?'|[\(\)\+\-\*/=']|[0-9A-F]+",
    #             part, re.I
    #         )
    #         operands.append({'raw': part, 'tokens': tokens})
    #     return operands

    def _split_operands(self, s: str) -> List[Dict[str, Any]]:
        if not s:
            return []
        # Remove extra spaces around commas
        s = re.sub(r'\s*,\s*', ',', s)
        operands = []
        for part in s.split(','):
            part = part.strip()
            if not part:
                continue
            tokens = re.findall(
                r"[A-Z0-9_]+|=[A-Z]?'[^']*?'|=[A-Z]\([^\)]+\)'[^']*?'|[\(\)\+\-\*/=']|[0-9A-F]+",
                part, re.I
            )
            operands.append({'raw': part, 'tokens': tokens})
        return operands

# =============================================================================
# Symbols Parser (enhanced literal detection)
# =============================================================================

@ParserRegistry.register("symbols")
class SymbolsParser(BaseSectionParser):
    def parse(self, source: str, lines: List[Tuple[int, str, str]], is_free: bool) -> Dict:
        result = {'symbols': {}, 'literals': []}
        equ_pat = re.compile(r'^\s*([A-Z0-9_]+)\s+EQU\s+(.*)', re.I)
        lit_pat = re.compile(r'=[A-Z0-9]\'[^\']*?\'|=[A-Z]\([^\)]+\)\'[^\']*?\'|=[V]\([A-Z0-9_]+\)', re.I)

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

# =============================================================================
# Enhanced Dependencies Parser (now includes file I/O & DB2)
# =============================================================================


@ParserRegistry.register("dependencies")
class DependenciesParser(BaseSectionParser):
    def parse(self, source: str, lines: List[Tuple[int, str, str]], is_free: bool) -> Dict:
        result = {
            'copy_files': [],
            'macros_used': [],
            'externals': [],
            'files_accessed': [],
            'db2_access': [],
            'subprograms_called': []  # New simple field for =V() calls
        }

        copy_pat = re.compile(r'^\s*(?:[A-Z0-9_]+\s+)?COPY\s+([A-Z0-9_]+)', re.I)
        ext_pat = re.compile(r'^\s*(?:[A-Z0-9_]+\s+)?(EXTRN|WXTRN)\s+(.*)', re.I)
        # Broader I/O pattern - matches both (ddname) and ddname alone
        io_pat = re.compile(r'^\s*(OPEN|CLOSE|GET|PUT|READ|WRITE|CHECK|POINT)\b\s*(?:\(|,)?\s*([A-Z0-9_]+)?', re.I)
        db2_pat = re.compile(r'\bCALL\s+[\'"]DSNHLI[\'"]', re.I)
        # Simple =V() call detection (no register tracking needed)
        vcall_pat = re.compile(r'=\s*V\s*\(\s*([A-Z0-9_]{1,8})\s*\)', re.I)

        for ln, txt, _ in lines:
            no_cmt = re.sub(r'\s*\*.*$', '', txt).strip()

            # 1. COPY books
            m = copy_pat.search(no_cmt)
            if m:
                result['copy_files'].append({'name': m.group(1).upper(), 'line': ln})

            # 2. EXTRN/WXTRN (external symbols)
            m = ext_pat.search(no_cmt)
            if m:
                typ = m.group(1).upper()
                for s in re.split(r'\s*,\s*', m.group(2)):
                    s = s.strip().upper()
                    if s:
                        result['externals'].append({'name': s, 'type': typ, 'line': ln})

            # 3. File I/O (VSAM/Sequential)
            m = io_pat.search(no_cmt)
            if m:
                op_name = m.group(1).upper()
                ddname = m.group(2).upper() if m.group(2) else 'UNKNOWN'
                result['files_accessed'].append({
                    'operation': op_name,
                    'ddname': ddname,
                    'line': ln
                })

            # 4. DB2 access
            if db2_pat.search(no_cmt):
                result['db2_access'].append({
                    'type': 'CALL DSNHLI',
                    'line': ln
                })

            # Simple subprogram calls via =V()
            for m in vcall_pat.finditer(no_cmt):
                target = m.group(1).upper()
                result['subprograms_called'].append({
                    'target': target,
                    'type': 'static_V_call',
                    'line': ln
                })

        return result


# =============================================================================
# Main Parser Class (unchanged structure, but now with all enhancements)
# =============================================================================

class AssemblyParser(BaseParser):
    FILE_TYPE = "assembly"

    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    MAX_FILE_SIZE = 50 * 1024 * 1024

    def __init__(self, strict: bool = False):
        self.strict = strict
        self._source = ""
        self._lines: List[Tuple[int, str, str]] = []
        self._unrecognized = []

    def parse_file(self, filepath: str) -> Dict:
        path = Path(filepath)
        if not path.is_file():
            raise FileNotFoundError(f"File not found: {filepath}")

        raw = path.read_bytes()
        content, enc = self._detect_and_decode(raw)
        self._source = content

        lines_raw = content.splitlines()
        is_free = self._detect_format(lines_raw)
        self._lines = self._preprocess(lines_raw, is_free)

        normalized = self._normalize_source(content, is_free)

        result = {
            'source_file': path.name,
            'format': 'free' if is_free else 'fixed',
            'detected_encoding': enc
        }

        for name, cls in ParserRegistry.all_parsers().items():
            parser = cls()
            data = parser.parse(normalized, self._lines, is_free)
            if data:
                result[name] = data
            if parser.unrecognized:
                self._unrecognized.extend(parser.unrecognized)

        name = self._extract_assembly_name(normalized)
        if name:
            result['assembly_name'] = name

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
        # 1. TITLE is most reliable (usually at the top)
        m = re.search(r'TITLE\s+[\'\"](.+?)[\'\"]', source, re.I | re.M)
        if m:
            title = m.group(1).strip()
            # Optional: clean up common patterns like 'PROG001 - DESCRIPTION'
            if ' - ' in title:
                return title.split(' - ', 1)[0].strip()  # e.g., "PROG001"
            return title

        # 2. Fallback to START label
        m = re.search(r'^\s*([A-Z0-9_]{1,8})\s+START\b', source, re.I | re.M)
        if m:
            return m.group(1).upper()

        # 3. Last resort: first CSECT name (if no TITLE/START)
        m = re.search(r'^\s*([A-Z0-9_]{1,8})\s+CSECT\b', source, re.I | re.M)
        if m:
            return m.group(1).upper()

        return None
    
def main():
    parser = argparse.ArgumentParser(description='HLASM source parser (production)')
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