"""
REXX Parser - Simplified for Dependency Extraction Only

This parser extracts only external dependencies from REXX scripts:
- COBOL program calls
- JCL job submissions
- Dataset operations
- TSO/ISPF utilities
- Environment variables

"""

import json
import logging
import re
from pathlib import Path
from typing import Dict, List, Any

from app.core.parsers.base import BaseParser
from app.core.exceptions.parser import REXXParseError

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('rexx_parser')


class REXXParser(BaseParser):
    """
    Ultra-simplified REXX Parser for Dependency Extraction.
    
    Extracts only external dependencies:
    - COBOL programs
    - JCL jobs
    - Datasets
    - TSO/ISPF utilities
    - Environment variables
    """
    
    FILE_TYPE = "rexx"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    
    # TSO commands that indicate dataset operations
    TSO_COMMANDS = {
        'ALLOC', 'ALLOCATE', 'FREE', 'DELETE', 'LISTDS', 'LISTDSI', 'LISTCAT',
        'SUBMIT', 'TRANSMIT', 'RECEIVE', 'SEND', 'ATTRIB', 'RENAME'
    }
    
    # ISPF commands that might reference datasets
    ISPF_COMMANDS = {
        'ISPEXEC', 'ISREDIT', 'BROWSE', 'EDIT', 'VIEW', 'LMINIT',
        'LMOPEN', 'LMCLOSE', 'LMFREE', 'LMMOVE', 'LMCOPY'
    }
    
    def __init__(self):
        """Initialize REXX parser."""
        self._unrecognized: List[Dict[str, Any]] = []
        self._warnings: List[str] = []
        logger.info("Initialized ultra-simplified REXXParser")
    
    def parse_file(self, filepath: str) -> dict:
        """Parse a REXX file and extract dependencies.
        
        Args:
            filepath: Path to the REXX file
            
        Returns:
            Dictionary containing dependencies
            
        Raises:
            FileNotFoundError: If file doesn't exist
            REXXParseError: If parsing fails
        """
        path = Path(filepath)
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            return {
                "meta": {
                    "exec_name": path.stem.upper(),
                    "source_file": path.name,
                    "file_type": self.FILE_TYPE
                },
                "dependencies": {
                    "cobolPrograms": [],
                    "jclJobs": [],
                    "datasets": [],
                    "utilities": [],
                    "environmentVariables": []
                },
                "_unrecognized": [],
                "_warnings": ["File not found"]
            }
        
        logger.info(f"Parsing REXX file: {filepath}")
        
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
            raise REXXParseError(
                message=f"REXX parsing failed: {e}",
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
    
    def parse_string(self, content: str, file_name: str = "unknown.rexx") -> Dict[str, Any]:
        """Parse REXX content and extract dependencies.
        
        Args:
            content: REXX script content
            file_name: Name of the file for metadata
            
        Returns:
            Dictionary containing dependencies
        """
        self._unrecognized = []
        self._warnings = []
        
        lines = content.split('\n')
        logger.debug(f"Parsing REXX content: {len(lines)} lines")
        
        # Extract dependencies
        cobol_programs = self._extract_cobol_calls(lines)
        jcl_jobs = self._extract_jcl_submissions(lines)
        datasets = self._extract_datasets(lines)
        utilities = self._extract_utilities(lines)
        env_vars = self._extract_environment_variables(lines)
        
        # Construct output
        result = {
            "meta": {
                "exec_name": Path(file_name).stem.upper(),
                "source_file": file_name,
                "file_type": self.FILE_TYPE
            },
            "dependencies": {
                "cobolPrograms": sorted(list(set(cobol_programs))),
                "jclJobs": sorted(list(set(jcl_jobs))),
                "datasets": sorted(list(set(datasets))),
                "utilities": sorted(list(set(utilities))),
                "environmentVariables": sorted(list(set(env_vars)))
            },
            "_unrecognized": self._unrecognized,
            "_warnings": self._warnings
        }
        
        return result
    
    def _extract_cobol_calls(self, lines: List[str]) -> List[str]:
        """Extract COBOL program calls from REXX script.
        
        Patterns:
        - CALL 'PROGRAM'
        - CALL PROGRAM
        - ADDRESS LINK 'PROGRAM'
        - ADDRESS LINKMVS 'PROGRAM'
        """
        cobol_programs = []
        
        # Pattern: CALL followed by program name
        call_pattern = re.compile(
            r'\bCALL\s+["\']?([A-Z][A-Z0-9]{0,7})["\']?',
            re.IGNORECASE
        )
        
        # Pattern: ADDRESS LINK/LINKMVS
        link_pattern = re.compile(
            r'\bADDRESS\s+(?:LINK|LINKMVS)\s+["\']?([A-Z][A-Z0-9]{0,7})["\']?',
            re.IGNORECASE
        )
        
        for line in lines:
            # Skip comments
            if line.strip().startswith('/*'):
                continue
            
            # Find CALL statements
            for match in call_pattern.finditer(line):
                prog_name = match.group(1).upper()
                if len(prog_name) <= 8:  # Valid COBOL program name
                    cobol_programs.append(prog_name)
            
            # Find ADDRESS LINK statements
            for match in link_pattern.finditer(line):
                prog_name = match.group(1).upper()
                if len(prog_name) <= 8:
                    cobol_programs.append(prog_name)
        
        logger.debug(f"Found {len(set(cobol_programs))} COBOL programs")
        return cobol_programs
    
    def _extract_jcl_submissions(self, lines: List[str]) -> List[str]:
        """Extract JCL job submissions.
        
        Patterns:
        - SUBMIT 'JOB.NAME'
        - SUBMIT dataset(member)
        """
        jcl_jobs = []
        
        # Pattern: SUBMIT followed by dataset/job name
        submit_pattern = re.compile(
            r'\bSUBMIT\s+["\']?([A-Z0-9.()]+)["\']?',
            re.IGNORECASE
        )
        
        for line in lines:
            if line.strip().startswith('/*'):
                continue
            
            for match in submit_pattern.finditer(line):
                job_ref = match.group(1).upper()
                # Extract member name if present: DATASET(MEMBER)
                member_match = re.search(r'\(([A-Z0-9]+)\)', job_ref)
                if member_match:
                    jcl_jobs.append(member_match.group(1))
                else:
                    jcl_jobs.append(job_ref)
        
        logger.debug(f"Found {len(set(jcl_jobs))} JCL jobs")
        return jcl_jobs
    
    def _extract_datasets(self, lines: List[str]) -> List[str]:
        """Extract dataset references.
        
        Patterns:
        - ALLOC/ALLOCATE ... DA('DATASET.NAME')
        - LISTDSI('DATASET.NAME')
        - EXECIO ... DISKR/DISKW DDNAME
        - Any quoted string that looks like a dataset name
        """
        datasets = []
        
        # Pattern: Dataset names in quotes (HLQ.MLQ.LLQ format)
        dataset_pattern = re.compile(
            r"['\"]([A-Z][A-Z0-9.#$@]{2,43})['\"]",
            re.IGNORECASE
        )
        
        # Pattern: DA( or DSN( parameters
        da_pattern = re.compile(
            r'\b(?:DA|DSN)\s*\(\s*["\']?([A-Z][A-Z0-9.#$@()]+)["\']?\s*\)',
            re.IGNORECASE
        )
        
        for line in lines:
            if line.strip().startswith('/*'):
                continue
            
            # Look for dataset-related commands
            if any(cmd in line.upper() for cmd in ['ALLOC', 'LISTDSI', 'LISTDS', 'LISTCAT', 'DELETE']):
                # Extract dataset names
                for match in dataset_pattern.finditer(line):
                    ds_name = match.group(1).upper()
                    # Filter: must have at least one dot and look like a dataset
                    if '.' in ds_name and len(ds_name) >= 3:
                        # Exclude obvious non-datasets
                        if not ds_name.startswith('SPACE(') and not ds_name.startswith('REC.'):
                            datasets.append(ds_name)
                
                # Also check DA() and DSN() parameters
                for match in da_pattern.finditer(line):
                    ds_name = match.group(1).upper()
                    if '.' in ds_name:
                        datasets.append(ds_name.replace('(', '.').replace(')', ''))
        
        logger.debug(f"Found {len(set(datasets))} datasets")
        return datasets
    
    def _extract_utilities(self, lines: List[str]) -> List[str]:
        """Extract TSO/ISPF utility calls.
        
        Patterns:
        - ADDRESS TSO "COMMAND"
        - ISPEXEC commands
        - Direct TSO command calls
        """
        utilities = []
        
        for line in lines:
            if line.strip().startswith('/*'):
                continue
            
            line_upper = line.upper()
            
            # Check for TSO commands
            for tso_cmd in self.TSO_COMMANDS:
                if tso_cmd in line_upper:
                    utilities.append(tso_cmd)
            
            # Check for ISPF commands
            for ispf_cmd in self.ISPF_COMMANDS:
                if ispf_cmd in line_upper:
                    utilities.append(ispf_cmd)
        
        logger.debug(f"Found {len(set(utilities))} utilities")
        return utilities
    
    def _extract_environment_variables(self, lines: List[str]) -> List[str]:
        """Extract environment variable references.
        
        Patterns:
        - VALUE('VARNAME')
        - GETENV('VARNAME')
        - System variables like SYSUID, SYSENV, etc.
        """
        env_vars = []
        
        # Pattern: VALUE() or GETENV() functions
        value_pattern = re.compile(
            r'\b(?:VALUE|GETENV)\s*\(\s*["\']([A-Z_][A-Z0-9_]*)["\']',
            re.IGNORECASE
        )
        
        # Common system variables
        system_vars = {
            'SYSUID', 'SYSENV', 'SYSPREF', 'SYSNODE', 'SYSLRACF',
            'SYSSRV', 'SYSTERMID', 'SYSTSOE', 'SYSDFP', 'SYSJES'
        }
        
        for line in lines:
            if line.strip().startswith('/*'):
                continue
            
            # Extract from VALUE/GETENV
            for match in value_pattern.finditer(line):
                var_name = match.group(1).upper()
                env_vars.append(var_name)
            
            # Look for system variables
            line_upper = line.upper()
            for sys_var in system_vars:
                if sys_var in line_upper:
                    env_vars.append(sys_var)
        
        logger.debug(f"Found {len(set(env_vars))} environment variables")
        return env_vars


def parse_rexx_file(file_path: str) -> Dict[str, Any]:
    """Convenience function to parse a REXX file."""
    parser = REXXParser()
    return parser.parse_file(file_path)


def parse_rexx_string(content: str, file_name: str = "inline.rexx") -> Dict[str, Any]:
    """Convenience function to parse REXX content from string."""
    parser = REXXParser()
    return parser.parse_string(content, file_name)