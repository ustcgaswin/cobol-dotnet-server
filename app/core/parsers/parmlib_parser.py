"""
PARMLIB Parser - Simplified for Dependency Extraction Only

This parser extracts only dependencies from PARMLIB control members:
- Program/utility references
- Dataset allocations
- JCL job references
- System parameters
- Symbolic substitutions

"""

import json
import logging
import re
from pathlib import Path
from typing import Dict, List, Any, Optional

from app.core.parsers.base import BaseParser
from app.core.exceptions.parser import PARMLIBParseError

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('parmlib_parser')


class PARMLIBParser(BaseParser):
    """
    Ultra-simplified PARMLIB Parser for Dependency Extraction.
    
    Extracts only dependencies:
    - Programs/utilities
    - Datasets
    - JCL references
    - System parameters
    - Symbolic substitutions
    """
    
    FILE_TYPE = "parmlib"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    
    # Regex patterns for dependency extraction
    DATASET_PATTERN = re.compile(
        r'\b([A-Z][A-Z0-9$#@]{0,7}(?:\.[A-Z0-9$#@]{1,8}){1,21})\b'
    )
    
    # Parameter patterns
    PARAM_PATTERN = re.compile(
        r'^([A-Z_][A-Z0-9_]*)\s*=\s*(.+)$',
        re.IGNORECASE
    )
    
    # Utility patterns
    SORT_PATTERN = re.compile(r'^\s*SORT\s+FIELDS', re.IGNORECASE)
    IDCAMS_PATTERN = re.compile(r'^\s*(?:DEFINE|DELETE|LISTCAT|REPRO|ALTER)', re.IGNORECASE)
    IEFBR14_PATTERN = re.compile(r'\bIEFBR14\b', re.IGNORECASE)
    IEBGENER_PATTERN = re.compile(r'\bIEBGENER\b', re.IGNORECASE)
    
    # Program reference patterns
    PROGRAM_PATTERN = re.compile(
        r'\b(?:PGM|PROGRAM)\s*=\s*([A-Z][A-Z0-9]{0,7})\b',
        re.IGNORECASE
    )
    
    # JCL reference patterns
    JOB_PATTERN = re.compile(
        r'\b(?:JOB|JOBNAME)\s*=\s*([A-Z][A-Z0-9]{0,7})\b',
        re.IGNORECASE
    )
    PROC_PATTERN = re.compile(
        r'\b(?:PROC|PROCEDURE)\s*=\s*([A-Z][A-Z0-9]{0,7})\b',
        re.IGNORECASE
    )
    
    # Symbol patterns
    SYMBOL_PATTERN = re.compile(
        r'&([A-Z_][A-Z0-9_]*)',
        re.IGNORECASE
    )
    SYMBOL_DEF_PATTERN = re.compile(
        r'SET\s+&([A-Z_][A-Z0-9_]*)\s*=\s*([^\s,]+)',
        re.IGNORECASE
    )
    
    # System parameter categories
    SYSTEM_PARAM_CATEGORIES = {
        'REGION': 'MEMORY',
        'TIME': 'EXECUTION',
        'COND': 'CONDITION',
        'CLASS': 'SCHEDULING',
        'MSGCLASS': 'OUTPUT',
        'MSGLEVEL': 'OUTPUT',
        'NOTIFY': 'NOTIFICATION',
        'SPACE': 'STORAGE',
        'UNIT': 'STORAGE',
        'VOL': 'STORAGE',
        'DISP': 'DISPOSITION',
        'DCB': 'DATASET',
        'LRECL': 'DATASET',
        'RECFM': 'DATASET',
        'BLKSIZE': 'DATASET',
    }
    
    # Common utilities
    UTILITIES = {
        'SORT', 'IDCAMS', 'IEFBR14', 'IEBGENER', 'IEBCOPY',
        'IEBUPDTE', 'IEHLIST', 'IEHPROGM', 'ICETOOL', 'DFSORT'
    }
    
    def __init__(self):
        """Initialize PARMLIB parser."""
        self._unrecognized: List[Dict[str, Any]] = []
        self._warnings: List[str] = []
        logger.info("Initialized ultra-simplified PARMLIBParser")
    
    def parse_file(self, filepath: str) -> dict:
        """Parse a PARMLIB file and extract dependencies.
        
        Args:
            filepath: Path to the PARMLIB file
            
        Returns:
            Dictionary containing dependencies
            
        Raises:
            FileNotFoundError: If file doesn't exist
            PARMLIBParseError: If parsing fails
        """
        path = Path(filepath)
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            return {
                "meta": {
                    "member_name": path.stem.upper(),
                    "source_file": path.name,
                    "file_type": self.FILE_TYPE
                },
                "dependencies": {
                    "programs": [],
                    "datasets": [],
                    "jcl_references": [],
                    "system_parameters": [],
                    "symbols": [],
                    "utilities": []
                },
                "_unrecognized": [],
                "_warnings": ["File not found"]
            }
        
        logger.info(f"Parsing PARMLIB file: {filepath}")
        
        try:
            # Read file with encoding detection
            raw_bytes = path.read_bytes()
            content, encoding = self._detect_and_decode(raw_bytes)
            logger.info(f"Detected encoding: {encoding}")
            
            # Parse content
            result = self.parse_string(content, path.name)
            
            logger.info(f"Successfully parsed {filepath}")
            return result
            
        except Exception as e:
            logger.error(f"Failed to parse {filepath}: {e}", exc_info=True)
            raise PARMLIBParseError(
                message=f"PARMLIB parsing failed: {e}",
                filename=path.name
            )
    
    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        """Detect file encoding and decode to string."""
        # Try UTF-8 first
        try:
            text = raw_bytes.decode('utf-8')
            return text, 'utf-8'
        except UnicodeDecodeError:
            pass
        
        # Try ASCII
        try:
            text = raw_bytes.decode('ascii')
            return text, 'ascii'
        except UnicodeDecodeError:
            pass
        
        # Try common EBCDIC codepages
        for codepage in self.EBCDIC_CODEPAGES:
            try:
                text = raw_bytes.decode(codepage)
                logger.info(f"Detected EBCDIC encoding: {codepage}")
                return text, codepage
            except (UnicodeDecodeError, LookupError):
                continue
        
        # Fallback: force UTF-8 with error replacement
        logger.warning("Could not detect encoding, falling back to UTF-8 with replacement")
        return raw_bytes.decode('utf-8', errors='replace'), 'utf-8-fallback'
    
    def parse_string(self, content: str, source_file: str = "unknown") -> Dict[str, Any]:
        """Parse PARMLIB content and extract dependencies.
        
        Args:
            content: PARMLIB content
            source_file: Name of the file for metadata
            
        Returns:
            Dictionary containing dependencies
        """
        self._unrecognized = []
        self._warnings = []
        
        lines = content.split('\n')
        logger.debug(f"Parsing PARMLIB content: {len(lines)} lines")
        
        # Extract dependencies
        programs = []
        datasets = []
        jcl_refs = []
        sys_params = []
        symbols = []
        utilities = []
        
        for line_num, line in enumerate(lines, 1):
            # Skip comments and empty lines
            if line.strip().startswith('*') or not line.strip():
                continue
            
            # Extract programs
            programs.extend(self._extract_programs(line, line_num))
            
            # Extract datasets
            datasets.extend(self._extract_datasets(line, line_num))
            
            # Extract JCL references
            jcl_refs.extend(self._extract_jcl_references(line, line_num))
            
            # Extract system parameters
            sys_params.extend(self._extract_system_parameters(line, line_num))
            
            # Extract symbols
            symbols.extend(self._extract_symbols(line, line_num))
            
            # Extract utilities
            utilities.extend(self._extract_utilities(line, line_num))
        
        # Deduplicate
        programs = self._deduplicate_by_name(programs)
        datasets = self._deduplicate_by_name(datasets)
        jcl_refs = self._deduplicate_by_key(jcl_refs, 'job_name')
        sys_params = self._deduplicate_by_name(sys_params)
        symbols = self._deduplicate_by_name(symbols)
        utilities = sorted(list(set(utilities)))
        
        # Construct output
        result = {
            "meta": {
                "member_name": Path(source_file).stem.upper(),
                "source_file": source_file,
                "file_type": self.FILE_TYPE
            },
            "dependencies": {
                "programs": programs,
                "datasets": datasets,
                "jcl_references": jcl_refs,
                "system_parameters": sys_params,
                "symbols": symbols,
                "utilities": utilities
            },
            "_unrecognized": self._unrecognized,
            "_warnings": self._warnings
        }
        
        return result
    
    def _extract_programs(self, line: str, line_num: int) -> List[Dict[str, Any]]:
        """Extract program references from a line."""
        programs = []
        
        # Look for PGM= or PROGRAM=
        for match in self.PROGRAM_PATTERN.finditer(line):
            prog_name = match.group(1).upper()
            programs.append({
                'name': prog_name,
                'purpose': 'EXECUTION',
                'line': line_num
            })
        
        # Look for utility names
        line_upper = line.upper()
        for util in self.UTILITIES:
            if util in line_upper:
                programs.append({
                    'name': util,
                    'purpose': 'UTILITY',
                    'line': line_num
                })
        
        return programs
    
    def _extract_datasets(self, line: str, line_num: int) -> List[Dict[str, Any]]:
        """Extract dataset references from a line."""
        datasets = []
        
        # Look for dataset name patterns
        for match in self.DATASET_PATTERN.finditer(line):
            ds_name = match.group(1).upper()
            
            # Must have at least one dot to be a dataset
            if '.' in ds_name and len(ds_name) >= 3:
                # Try to determine type and disposition from context
                ds_type = self._infer_dataset_type(line)
                disp = self._infer_disposition(line)
                
                datasets.append({
                    'name': ds_name,
                    'type': ds_type,
                    'disp': disp,
                    'line': line_num
                })
        
        return datasets
    
    def _extract_jcl_references(self, line: str, line_num: int) -> List[Dict[str, Any]]:
        """Extract JCL job/proc references from a line."""
        jcl_refs = []
        
        # Look for JOB= or JOBNAME=
        for match in self.JOB_PATTERN.finditer(line):
            job_name = match.group(1).upper()
            jcl_refs.append({
                'job_name': job_name,
                'step_name': None,
                'usage': 'JOB_REFERENCE',
                'line': line_num
            })
        
        # Look for PROC= or PROCEDURE=
        for match in self.PROC_PATTERN.finditer(line):
            proc_name = match.group(1).upper()
            jcl_refs.append({
                'job_name': proc_name,
                'step_name': None,
                'usage': 'PROC_REFERENCE',
                'line': line_num
            })
        
        return jcl_refs
    
    def _extract_system_parameters(self, line: str, line_num: int) -> List[Dict[str, Any]]:
        """Extract system parameter definitions from a line."""
        sys_params = []
        
        # Match parameter=value pattern
        match = self.PARAM_PATTERN.match(line.strip())
        if match:
            param_name = match.group(1).upper()
            param_value = match.group(2).strip()
            
            # Categorize the parameter
            category = self._categorize_parameter(param_name)
            
            sys_params.append({
                'name': param_name,
                'value': param_value,
                'category': category,
                'line': line_num
            })
        
        return sys_params
    
    def _extract_symbols(self, line: str, line_num: int) -> List[Dict[str, Any]]:
        """Extract symbolic parameter definitions and references."""
        symbols = []
        
        # Look for symbol definitions (SET &VAR=VALUE)
        for match in self.SYMBOL_DEF_PATTERN.finditer(line):
            symbol_name = match.group(1).upper()
            symbol_value = match.group(2)
            
            symbols.append({
                'name': symbol_name,
                'value': symbol_value,
                'scope': 'GLOBAL',
                'line': line_num
            })
        
        # Look for symbol references (&VAR)
        for match in self.SYMBOL_PATTERN.finditer(line):
            symbol_name = match.group(1).upper()
            
            # Only add if not already defined
            if not any(s['name'] == symbol_name for s in symbols):
                symbols.append({
                    'name': symbol_name,
                    'value': None,
                    'scope': 'REFERENCE',
                    'line': line_num
                })
        
        return symbols
    
    def _extract_utilities(self, line: str, line_num: int) -> List[str]:
        """Extract utility references from a line."""
        utilities = []
        
        line_upper = line.upper()
        
        # Check for SORT
        if self.SORT_PATTERN.search(line):
            utilities.append('SORT')
        
        # Check for IDCAMS
        if self.IDCAMS_PATTERN.search(line):
            utilities.append('IDCAMS')
        
        # Check for other utilities
        for util in self.UTILITIES:
            if util in line_upper:
                utilities.append(util)
        
        return utilities
    
    def _infer_dataset_type(self, line: str) -> str:
        """Infer dataset type from context."""
        line_upper = line.upper()
        
        if 'VSAM' in line_upper:
            return 'VSAM'
        elif 'GDG' in line_upper:
            return 'GDG'
        elif 'PDS' in line_upper:
            return 'PDS'
        elif 'SEQ' in line_upper or 'PS' in line_upper:
            return 'SEQUENTIAL'
        else:
            return 'UNKNOWN'
    
    def _infer_disposition(self, line: str) -> Optional[str]:
        """Infer dataset disposition from context."""
        line_upper = line.upper()
        
        if 'DISP=SHR' in line_upper or 'SHR' in line_upper:
            return 'SHR'
        elif 'DISP=OLD' in line_upper:
            return 'OLD'
        elif 'DISP=NEW' in line_upper:
            return 'NEW'
        elif 'DISP=MOD' in line_upper:
            return 'MOD'
        else:
            return None
    
    def _categorize_parameter(self, param_name: str) -> str:
        """Categorize a system parameter."""
        # Check if it's a known system parameter
        for key, category in self.SYSTEM_PARAM_CATEGORIES.items():
            if key in param_name.upper():
                return category
        
        # Default categorization
        if any(x in param_name.upper() for x in ['REGION', 'SIZE', 'SPACE']):
            return 'MEMORY'
        elif any(x in param_name.upper() for x in ['TIME', 'LIMIT', 'MAX']):
            return 'EXECUTION'
        elif any(x in param_name.upper() for x in ['CLASS', 'PRIORITY', 'QUEUE']):
            return 'SCHEDULING'
        elif any(x in param_name.upper() for x in ['DSN', 'DATASET', 'FILE']):
            return 'DATASET'
        else:
            return 'CONFIGURATION'
    
    def _deduplicate_by_name(self, items: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Deduplicate items by 'name' field."""
        seen = {}
        for item in items:
            key = item.get('name', '').upper()
            if key and key not in seen:
                seen[key] = item
        return list(seen.values())
    
    def _deduplicate_by_key(self, items: List[Dict[str, Any]], key: str) -> List[Dict[str, Any]]:
        """Deduplicate items by a specific key."""
        seen = {}
        for item in items:
            k = item.get(key, '').upper()
            if k and k not in seen:
                seen[k] = item
        return list(seen.values())


def parse_parmlib_file(file_path: str) -> Dict[str, Any]:
    """Convenience function to parse a PARMLIB file."""
    parser = PARMLIBParser()
    return parser.parse_file(file_path)


def parse_parmlib_string(content: str, file_name: str = "inline.txt") -> Dict[str, Any]:
    """Convenience function to parse PARMLIB content from string."""
    parser = PARMLIBParser()
    return parser.parse_string(content, file_name)