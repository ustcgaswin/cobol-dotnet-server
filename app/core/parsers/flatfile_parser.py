"""
Enterprise-grade flat file parsers for CSV and Fixed-Length files.
Integrated with mainframe modernization parser framework.

Features:
- Comprehensive logging and error tracking
- EBCDIC encoding support
- LLM-powered file analysis and documentation
- RAG-enhanced understanding of flat file patterns
"""

import asyncio
import csv
import io
import json
import logging
import re
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, List, Optional

from tenacity import retry, stop_after_attempt, wait_exponential

from app.core.exceptions.parser import (
    CSVParseError,
    EmptyFileError,
    EncodingDetectionError,
    FixedLengthParseError,
    InvalidLayoutError,
    LLMAugmentationError,
    SchemaInferenceError,
)
from app.core.parsers.base import BaseParser

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('flatfile_parser')


# ============================================================================
# CSV Parser Implementation
# ============================================================================

class CSVParser(BaseParser):
    """Enterprise-grade CSV file parser with LLM-powered analysis."""
    
    FILE_TYPE = "csv"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    
    # LLM Augmentation Configuration
    AUGMENTATION_MAX_RETRIES = 3
    AUGMENTATION_TIMEOUT = 45  # seconds
    
    def __init__(
        self, 
        max_records: int = 100,
        return_all_records: bool = False,
        skip_invalid_rows: bool = True,
        llm_sample_size: int = 15
    ):
        """Initialize CSV parser.
        
        Args:
            max_records: Maximum number of records to return (default: 100)
            return_all_records: If True, return all records regardless of max_records
            skip_invalid_rows: If True, skip invalid rows and track them as warnings
            llm_sample_size: Number of sample records to send to LLM (default: 15)
        """
        self.max_records = max_records
        self.return_all_records = return_all_records
        self.skip_invalid_rows = skip_invalid_rows
        self.llm_sample_size = llm_sample_size
        self.max_sample_records = None if return_all_records else max_records
        self._unrecognized = []
        self._warnings = []
        
        logger.info(
            f"Initialized CSVParser: max_records={max_records}, "
            f"return_all={return_all_records}, llm_sample_size={llm_sample_size}"
        )
    
    def parse_file(self, filepath: str, **kwargs) -> dict:
        """Parse a CSV file from filesystem.
        
        Args:
            filepath: Path to the CSV file
            **kwargs: Additional parameters (max_records, return_all_records)
            
        Returns:
            Dictionary containing parsed data
            
        Raises:
            FileNotFoundError: If file doesn't exist
            EmptyFileError: If file is empty
            CSVParseError: If parsing fails
        """
        path = Path(filepath)
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            raise FileNotFoundError(f"File not found: {filepath}")
        
        logger.info(f"Parsing CSV file: {filepath} (size: {path.stat().st_size} bytes)")
        
        # Reset tracking lists
        self._unrecognized = []
        self._warnings = []
        
        # Handle dynamic parameters
        max_records = kwargs.get('max_records', self.max_records)
        return_all_records = kwargs.get('return_all_records', self.return_all_records)
        effective_max_records = None if return_all_records else max_records
        
        try:
            # Read and decode file
            raw_bytes = path.read_bytes()
            content, encoding = self._detect_and_decode(raw_bytes)
            logger.info(f"Detected encoding: {encoding}")
            
            # Parse content
            result = self._parse_content(
                content, 
                filename=path.name,
                max_sample_records=effective_max_records if effective_max_records else 999999999
            )
            
            # Add framework fields
            result['encoding'] = encoding
            result['file_type'] = self.FILE_TYPE
            result['source_file'] = path.name
            result['_warnings'] = self._warnings
            result['_unrecognized'] = self._unrecognized
            
            logger.info(
                f"Successfully parsed {filepath}: {result['totalRecords']} records, "
                f"{len(self._warnings)} warnings, {len(self._unrecognized)} unrecognized"
            )
            
            return result
            
        except EmptyFileError:
            logger.error(f"Empty file: {filepath}")
            raise
        except Exception as e:
            logger.error(f"Failed to parse {filepath}: {e}", exc_info=True)
            raise CSVParseError(
                message=f"CSV parsing failed: {e}",
                filename=path.name
            )
    
    async def augment(self, parsed_data: dict) -> dict:
        """Augment parsed CSV data with LLM-generated insights.
        
        Adds comprehensive analysis including:
        - File description and business purpose
        - Business domain classification
        - Per-column analysis and significance
        - Data quality assessment
        - Migration considerations
        
        Args:
            parsed_data: The parsed CSV JSON structure
            
        Returns:
            The augmented parsed data with LLM insights in 'llm_analysis' key
            
        Raises:
            LLMAugmentationError: If LLM analysis fails after retries
        """
        filename = parsed_data.get('source_file', 'unknown')
        logger.info(f"Starting LLM augmentation for: {filename}")
        
        try:
            agent = self._create_augmentation_agent()
            llm_insights = await self._augment_file(agent, parsed_data)
            
            # Validate response structure
            required_keys = ["file_description", "business_domain", "column_analysis"]
            missing_keys = [k for k in required_keys if k not in llm_insights]
            
            if missing_keys:
                logger.warning(f"LLM response missing keys: {missing_keys}")
                self._warnings.append(
                    f"LLM analysis incomplete: missing {', '.join(missing_keys)}"
                )
            
            parsed_data["llm_analysis"] = llm_insights
            logger.info(f"Successfully augmented {filename} with LLM insights")
            
            return parsed_data
            
        except json.JSONDecodeError as e:
            error_msg = f"LLM returned invalid JSON: {e}"
            logger.error(error_msg)
            
            # Continue with partial data
            parsed_data["llm_analysis"] = None
            parsed_data.setdefault("_augmentation_errors", []).append(error_msg)
            logger.warning(f"Continuing without LLM analysis for {filename}")
            
            return parsed_data
            
        except Exception as e:
            error_msg = f"LLM augmentation failed: {e}"
            logger.error(error_msg, exc_info=True)
            
            # Continue with partial data
            parsed_data["llm_analysis"] = None
            parsed_data.setdefault("_augmentation_errors", []).append(str(e))
            logger.warning(f"Continuing without LLM analysis for {filename}")
            
            return parsed_data
    
    def _create_augmentation_agent(self):
        """Create LangGraph ReAct agent with RAG tool access."""
        from langgraph.prebuilt import create_react_agent
        from app.config.llm import get_llm, DOCGEN
        from app.core.tools.rag_tools import search_flatfile_docs
        
        return create_react_agent(model=get_llm(DOCGEN), tools=[search_flatfile_docs])
    
    async def _augment_file(self, agent, parsed_data: dict) -> dict:
        """Generate comprehensive file analysis using LLM.
        
        Args:
            agent: LangGraph agent with RAG tool access
            parsed_data: Parsed file data
            
        Returns:
            Dict with comprehensive analysis
        """
        prompt = self._build_file_analysis_prompt(parsed_data)
        
        @retry(
            stop=stop_after_attempt(self.AUGMENTATION_MAX_RETRIES),
            wait=wait_exponential(multiplier=1, min=2, max=10),
        )
        async def invoke_with_retry():
            result = await asyncio.wait_for(
                agent.ainvoke({"messages": [("user", prompt)]}),
                timeout=self.AUGMENTATION_TIMEOUT,
            )
            response_text = result["messages"][-1].content.strip()
            
            # Extract JSON from markdown if present
            if "```json" in response_text:
                json_match = re.search(r'```json\s*(\{.*?\})\s*```', response_text, re.DOTALL)
                if json_match:
                    response_text = json_match.group(1)
            elif "```" in response_text:
                response_text = response_text.replace("```", "").strip()
            
            return json.loads(response_text)
        
        return await invoke_with_retry()
    
    def _build_file_analysis_prompt(self, parsed_data: dict) -> str:
        """Build comprehensive prompt for CSV analysis."""
        filename = parsed_data.get("fileName", "unknown")
        delimiter = parsed_data.get("delimiter", ",")
        total_records = parsed_data.get("totalRecords", 0)
        has_header = parsed_data.get("hasHeader", False)
        schema = parsed_data.get("schema", [])
        records = parsed_data.get("records", [])[:self.llm_sample_size]
        data_quality = parsed_data.get("dataQuality", {})
        
        column_info = []
        for col in schema:
            column_info.append({
                "name": col["columnName"],
                "type": col["dataType"],
                "nullable": col["nullable"],
                "dotnet_type": col["suggestedDotNetType"]
            })
        
        return f"""You are analyzing a CSV flat file from a mainframe modernization project.

FILE METADATA:
- Filename: {filename}
- Delimiter: '{delimiter}'
- Total Records: {total_records:,}
- Has Header Row: {has_header}
- Encoding: {parsed_data.get('encoding', 'unknown')}

COLUMN SCHEMA ({len(schema)} columns):
{json.dumps(column_info, indent=2)}

DATA QUALITY METRICS:
- Missing Values: {data_quality.get('missingValues', 0)}
- Invalid Rows: {data_quality.get('invalidRows', 0)}
- Inconsistent Columns: {data_quality.get('inconsistentColumnRows', 0)}

SAMPLE DATA (first {len(records)} of {total_records} records):
{json.dumps(records, indent=2)}

If you need information about CSV formats, data patterns, or mainframe file structures, use the search_flatfile_docs tool.

PROVIDE A COMPREHENSIVE ANALYSIS with the following JSON structure:
{{
  "file_description": "2-3 sentence description of what this file contains and its business purpose",
  
  "business_domain": "The business area (e.g., 'Customer Master Data', 'Financial Transactions', 'Product Inventory')",
  
  "column_analysis": [
    {{
      "column_name": "exact column name from schema",
      "purpose": "What this column represents",
      "data_pattern": "Observed pattern (e.g., 'Sequential numeric IDs', 'Email addresses', 'ISO dates')",
      "business_significance": "Why this column matters for business operations"
    }}
    // IMPORTANT: Include one entry for EVERY column in the schema above
  ],
  
  "data_quality_assessment": {{
    "overall_quality": "Excellent/Good/Fair/Poor",
    "issues_found": ["Specific quality issues observed in the data"],
    "recommendations": ["Specific recommendations for data cleansing or validation"]
  }},
  
  "migration_considerations": {{
    "complexity": "Low/Medium/High",
    "key_challenges": ["Specific challenges for migrating this file to .NET"],
    "suggested_dotnet_structure": "Recommended .NET class or database table structure"
  }},
  
  "notable_patterns": [
    "Any interesting patterns, relationships, or anomalies discovered in the data"
  ]
}}

CRITICAL: Return ONLY the JSON object, no markdown formatting, no additional text, no code blocks."""
        
    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        """Detect encoding with mainframe EBCDIC support.
        
        Args:
            raw_bytes: Raw file bytes
            
        Returns:
            Tuple of (decoded_content, encoding_name)
            
        Raises:
            EncodingDetectionError: If encoding cannot be determined
        """
        # Try UTF-8 first
        try:
            content = raw_bytes.decode('utf-8')
            logger.debug("Encoding detected: utf-8")
            return content, 'utf-8'
        except UnicodeDecodeError:
            pass
        
        # Try ASCII
        try:
            content = raw_bytes.decode('ascii')
            logger.debug("Encoding detected: ascii")
            return content, 'ascii'
        except UnicodeDecodeError:
            pass
        
        # Try EBCDIC
        for codepage in self.EBCDIC_CODEPAGES:
            try:
                content = raw_bytes.decode(codepage)
                if any(c in content for c in [',', ';', '\t', '|', '\n']):
                    logger.debug(f"Encoding detected: {codepage} (EBCDIC)")
                    return content, codepage
            except (UnicodeDecodeError, LookupError):
                continue
        
        # Fallback to chardet
        try:
            import chardet
            detected = chardet.detect(raw_bytes)
            encoding = detected.get('encoding', 'utf-8')
            confidence = detected.get('confidence', 0)
            
            logger.warning(f"Using chardet fallback: {encoding} (confidence: {confidence:.2%})")
            content = raw_bytes.decode(encoding, errors='replace')
            return content, encoding
        except Exception as e:
            logger.error(f"Encoding detection failed: {e}")
            raise EncodingDetectionError(
                message="Could not detect file encoding",
                filename=None
            )
    
    def _parse_content(
        self, 
        content: str, 
        filename: str = "unknown.csv",
        max_sample_records: int = 100
    ) -> Dict[str, Any]:
        """Parse CSV content and extract comprehensive metadata and records.
        
        Args:
            content: Raw CSV content as string
            filename: Name of the file for metadata
            max_sample_records: Maximum records to include in output
            
        Returns:
            Structured dictionary with analysis results
            
        Raises:
            EmptyFileError: If file is empty
            CSVParseError: If parsing fails
        """
        # [Implementation from previous flatfile_parser.py remains the same]
        # Normalize line endings
        content = content.replace('\r\n', '\n').replace('\r', '\n')
        
        # Strip BOM if present
        if content.startswith('\ufeff'):
            content = content[1:]
            logger.debug("Stripped BOM from file content")
        
        lines = [line for line in content.split('\n') if line.strip()]
        
        if not lines:
            raise EmptyFileError(
                message="CSV file contains no valid data",
                filename=filename
            )
        
        logger.debug(f"File contains {len(lines)} non-empty lines")
        
        delimiter = self._detect_delimiter(content)
        logger.info(f"Detected delimiter: {repr(delimiter)}")
        
        try:
            reader = csv.reader(io.StringIO(content), delimiter=delimiter)
            rows = []
            for row_num, row in enumerate(reader, 1):
                if row and any(cell.strip() for cell in row):
                    rows.append(row)
        except csv.Error as e:
            logger.error(f"CSV reader error: {e}")
            raise CSVParseError(
                message=f"CSV parsing error: {e}",
                filename=filename
            )
        
        if not rows:
            raise EmptyFileError(
                message="CSV file contains no valid data rows",
                filename=filename
            )
        
        logger.debug(f"Parsed {len(rows)} data rows")
        
        has_header = self._detect_header(rows)
        logger.info(f"Header detection: {has_header}")
        
        if has_header:
            headers = [h.strip() for h in rows[0]]
            data_rows = rows[1:]
        else:
            headers = [f"COLUMN_{i+1}" for i in range(len(rows[0]))]
            data_rows = rows
        
        if not data_rows:
            raise EmptyFileError(
                message="CSV file has no data rows after header",
                filename=filename
            )
        
        logger.debug(f"Schema: {len(headers)} columns, {len(data_rows)} data rows")
        
        schema = self._analyze_schema(headers, data_rows)
        parsed_records = self._parse_records(headers, data_rows)
        data_quality = self._analyze_data_quality(data_rows, headers)
        migration_insights = self._generate_migration_insights(schema, data_quality)
        
        return {
            "fileType": "CSV",
            "fileName": filename,
            "delimiter": delimiter,
            "hasHeader": has_header,
            "totalRecords": len(data_rows),
            "schema": schema,
            "records": parsed_records[:max_sample_records],
            "dataQuality": data_quality,
            "migrationInsights": migration_insights
        }
    
    def _detect_delimiter(self, content: str) -> str:
        lines = content.split('\n')[:5]
        sample = '\n'.join(lines)
        
        sniffer = csv.Sniffer()
        try:
            dialect = sniffer.sniff(sample, delimiters=',;\t|')
            return dialect.delimiter
        except:
            pass
        
        if lines:
            first_line = lines[0]
            delimiters = {',': 0, ';': 0, '\t': 0, '|': 0}
            for delim in delimiters:
                delimiters[delim] = first_line.count(delim)
            
            max_delim = max(delimiters, key=delimiters.get)
            if delimiters[max_delim] > 0:
                return max_delim
        
        return ','
    
    def _detect_header(self, rows: List[List[str]]) -> bool:
        if len(rows) < 2:
            return True
        
        first_row = rows[0]
        second_row = rows[1] if len(rows) > 1 else []
        
        if not second_row:
            return True
        
        first_numeric_count = sum(1 for val in first_row if val.strip() and self._is_numeric(val.strip()))
        second_numeric_count = sum(1 for val in second_row if val.strip() and self._is_numeric(val.strip()))
        
        if first_numeric_count < len(first_row) * 0.5 and second_numeric_count > len(second_row) * 0.5:
            return True
        
        header_pattern_count = sum(1 for val in first_row 
                                   if val.strip() and re.match(r'^[A-Z_][A-Z0-9_]*$', val.strip()))
        if header_pattern_count > len(first_row) * 0.6:
            return True
        
        return False
    
    def _analyze_schema(self, headers: List[str], data_rows: List[List[str]]) -> List[Dict[str, Any]]:
        schema = []
        
        for idx, header in enumerate(headers):
            column_values = [row[idx] if idx < len(row) else '' for row in data_rows]
            
            try:
                data_type, nullable = self._infer_data_type(column_values)
                dotnet_type = self._map_to_dotnet_type(data_type)
                
                schema.append({
                    "columnName": header.strip(),
                    "dataType": data_type,
                    "nullable": nullable,
                    "suggestedDotNetType": dotnet_type
                })
            except Exception as e:
                logger.warning(f"Failed to infer type for column '{header}': {e}")
                self._warnings.append(f"Column '{header}': Type inference failed, defaulting to STRING")
                schema.append({
                    "columnName": header.strip(),
                    "dataType": "STRING",
                    "nullable": True,
                    "suggestedDotNetType": "string"
                })
        
        return schema
    
    def _infer_data_type(self, values: List[str]) -> tuple:
        non_empty_values = [v.strip() for v in values if v.strip()]
        
        if not non_empty_values:
            return "STRING", True
        
        nullable = len(non_empty_values) < len(values)
        
        if all(v.upper() in ['TRUE', 'FALSE', 'T', 'F', 'Y', 'N', '0', '1'] for v in non_empty_values):
            return "BOOLEAN", nullable
        
        if all(self._is_integer(v) for v in non_empty_values):
            return "INTEGER", nullable
        
        if all(self._is_decimal(v) for v in non_empty_values):
            return "DECIMAL", nullable
        
        date_count = sum(1 for v in non_empty_values if self._is_date(v))
        if date_count > len(non_empty_values) * 0.8:
            return "DATE", nullable
        
        return "STRING", nullable
    
    def _is_numeric(self, value: str) -> bool:
        try:
            float(value.strip())
            return True
        except:
            return False
    
    def _is_integer(self, value: str) -> bool:
        try:
            val = value.strip()
            int(val)
            return '.' not in val
        except:
            return False
    
    def _is_decimal(self, value: str) -> bool:
        try:
            Decimal(value.strip())
            return True
        except:
            return False
    
    def _is_date(self, value: str) -> bool:
        date_patterns = [
            r'^\d{4}-\d{2}-\d{2}$',
            r'^\d{2}/\d{2}/\d{4}$',
            r'^\d{2}-\d{2}-\d{4}$',
            r'^\d{8}$'
        ]
        return any(re.match(pattern, value.strip()) for pattern in date_patterns)
    
    def _map_to_dotnet_type(self, data_type: str) -> str:
        type_mapping = {
            "STRING": "string",
            "INTEGER": "long",
            "DECIMAL": "decimal",
            "DATE": "DateTime",
            "BOOLEAN": "bool"
        }
        return type_mapping.get(data_type, "string")
    
    def _parse_records(self, headers: List[str], data_rows: List[List[str]]) -> List[Dict[str, Any]]:
        records = []
        expected_columns = len(headers)
        
        for row_num, row in enumerate(data_rows, 1):
            try:
                if len(row) != expected_columns:
                    error_msg = f"Column count mismatch: expected {expected_columns}, got {len(row)}"
                    self._unrecognized.append({
                        "line": row_num,
                        "content": str(row)[:100],
                        "error": error_msg
                    })
                    
                    if self.skip_invalid_rows:
                        logger.debug(f"Row {row_num}: {error_msg} - skipping")
                        continue
                
                record = {}
                for idx, header in enumerate(headers):
                    value = row[idx].strip() if idx < len(row) else ''
                    
                    if value == '':
                        record[header] = None
                    else:
                        try:
                            if self._is_integer(value):
                                record[header] = int(value)
                            elif self._is_decimal(value) and not self._is_integer(value):
                                record[header] = float(value)
                            else:
                                record[header] = value
                        except Exception as e:
                            logger.debug(f"Row {row_num}, Column '{header}': Type conversion failed - {e}")
                            record[header] = value
                
                records.append(record)
                
            except Exception as e:
                error_msg = f"Failed to parse row: {e}"
                logger.warning(f"Row {row_num}: {error_msg}")
                self._unrecognized.append({
                    "line": row_num,
                    "content": str(row)[:100],
                    "error": error_msg
                })
                
                if not self.skip_invalid_rows:
                    raise CSVParseError(
                        message=error_msg,
                        filename=None,
                        line_number=row_num,
                        content=str(row)[:100]
                    )
        
        return records
    
    def _analyze_data_quality(self, data_rows: List[List[str]], headers: List[str]) -> Dict[str, Any]:
        missing_values = 0
        invalid_rows = 0
        inconsistent_column_rows = 0
        expected_columns = len(headers)
        
        for row in data_rows:
            if len(row) != expected_columns:
                inconsistent_column_rows += 1
                if len(row) < expected_columns:
                    invalid_rows += 1
            
            missing_values += sum(1 for val in row if not val.strip())
        
        return {
            "missingValues": missing_values,
            "invalidRows": invalid_rows,
            "inconsistentColumnRows": inconsistent_column_rows
        }
    
    def _generate_migration_insights(self, schema: List[Dict], data_quality: Dict) -> Dict[str, Any]:
        fields_needing_cleansing = []
        recommendations = []
        
        for col in schema:
            if col["nullable"]:
                fields_needing_cleansing.append(col["columnName"])
        
        if data_quality["missingValues"] > 0:
            recommendations.append("Handle null/missing values")
        
        if data_quality["inconsistentColumnRows"] > 0:
            recommendations.append("Validate row structure consistency")
        
        recommendations.extend([
            "Trim whitespace from string fields",
            "Normalize date formats to ISO 8601"
        ])
        
        return {
            "fieldsNeedingCleansing": fields_needing_cleansing,
            "recommendedTransformations": recommendations
        }

# ============================================================================
# Fixed-Length Parser Implementation
# ============================================================================

class FixedLengthParser(BaseParser):
    """Enterprise-grade fixed-length flat file parser with LLM-powered analysis."""
    
    FILE_TYPE = "fixed_length"
    EBCDIC_CODEPAGES = ['cp1047', 'cp037', 'cp500', 'cp875']
    
    # LLM Augmentation Configuration
    AUGMENTATION_MAX_RETRIES = 3
    AUGMENTATION_TIMEOUT = 45  # seconds
    
    def __init__(
        self,
        max_records: int = 100,
        return_all_records: bool = False,
        layout: Optional[List[Dict[str, Any]]] = None,
        skip_invalid_rows: bool = True,
        llm_sample_size: int = 15
    ):
        """Initialize Fixed-Length parser.
        
        Args:
            max_records: Maximum number of records to return (default: 100)
            return_all_records: If True, return all records regardless of max_records
            layout: Optional field layout specification
            skip_invalid_rows: If True, skip invalid rows and track them as warnings
            llm_sample_size: Number of sample records to send to LLM (default: 15)
        """
        self.max_records = max_records
        self.return_all_records = return_all_records
        self.layout = layout
        self.skip_invalid_rows = skip_invalid_rows
        self.llm_sample_size = llm_sample_size
        self.max_sample_records = None if return_all_records else max_records
        self._unrecognized = []
        self._warnings = []
        
        logger.info(
            f"Initialized FixedLengthParser: max_records={max_records}, "
            f"return_all={return_all_records}, llm_sample_size={llm_sample_size}, "
            f"layout_provided={layout is not None}"
        )
    
    def parse_file(self, filepath: str, **kwargs) -> dict:
        """Parse a fixed-length file from filesystem.
        
        Args:
            filepath: Path to the fixed-length file
            **kwargs: Additional parameters (max_records, return_all_records, layout)
            
        Returns:
            Dictionary containing parsed data with structure:
            {
                "file_type": "fixed_length",
                "source_file": filename,
                "encoding": detected encoding,
                "fileType": "FIXED_LENGTH",
                "fileName": filename,
                "recordLength": int,
                "totalRecords": count,
                "schema": [...],
                "records": [...],
                "dataQuality": {...},
                "migrationInsights": {...},
                "_warnings": [...],
                "_unrecognized": [...]
            }
            
        Raises:
            FileNotFoundError: If file doesn't exist
            EmptyFileError: If file is empty
            FixedLengthParseError: If parsing fails
        """
        path = Path(filepath)
        if not path.exists():
            logger.error(f"File not found: {filepath}")
            raise FileNotFoundError(f"File not found: {filepath}")
        
        logger.info(f"Parsing fixed-length file: {filepath} (size: {path.stat().st_size} bytes)")
        
        # Reset tracking lists for each parse
        self._unrecognized = []
        self._warnings = []
        
        # Handle dynamic parameters
        max_records = kwargs.get('max_records', self.max_records)
        return_all_records = kwargs.get('return_all_records', self.return_all_records)
        layout = kwargs.get('layout', self.layout)
        effective_max_records = None if return_all_records else max_records
        
        try:
            # Read file with encoding detection
            raw_bytes = path.read_bytes()
            content, encoding = self._detect_and_decode(raw_bytes)
            logger.info(f"Detected encoding: {encoding}")
            
            # Parse using string parsing logic
            result = self._parse_content(
                content,
                filename=path.name,
                layout=layout,
                max_sample_records=effective_max_records if effective_max_records else 999999999
            )
            
            # Add framework-required fields
            result['encoding'] = encoding
            result['file_type'] = self.FILE_TYPE
            result['source_file'] = path.name
            
            # Add error tracking fields
            result['_warnings'] = self._warnings
            result['_unrecognized'] = self._unrecognized
            
            logger.info(
                f"Successfully parsed {filepath}: {result['totalRecords']} records, "
                f"record_length={result['recordLength']}, "
                f"{len(self._warnings)} warnings, {len(self._unrecognized)} unrecognized patterns"
            )
            
            return result
            
        except EmptyFileError:
            logger.error(f"Empty file: {filepath}")
            raise
        except Exception as e:
            logger.error(f"Failed to parse {filepath}: {e}", exc_info=True)
            raise FixedLengthParseError(
                message=f"Fixed-length parsing failed: {e}",
                filename=path.name
            )


    async def augment(self, parsed_data: dict) -> dict:
        """Augment parsed fixed-length data with LLM-generated insights.
        
        Adds comprehensive analysis including:
        - File description and business purpose
        - Business domain classification
        - Per-field analysis and significance
        - Layout quality assessment
        - Data quality assessment
        - Migration considerations
        
        Args:
            parsed_data: The parsed fixed-length JSON structure
            
        Returns:
            The augmented parsed data with LLM insights in 'llm_analysis' key
        """
        filename = parsed_data.get('source_file', 'unknown')
        logger.info(f"Starting LLM augmentation for: {filename}")
        
        try:
            agent = self._create_augmentation_agent()
            llm_insights = await self._augment_file(agent, parsed_data)
            
            # Validate response structure
            required_keys = ["file_description", "business_domain", "field_analysis"]
            missing_keys = [k for k in required_keys if k not in llm_insights]
            
            if missing_keys:
                logger.warning(f"LLM response missing keys: {missing_keys}")
                self._warnings.append(
                    f"LLM analysis incomplete: missing {', '.join(missing_keys)}"
                )
            
            parsed_data["llm_analysis"] = llm_insights
            logger.info(f"Successfully augmented {filename} with LLM insights")
            
            return parsed_data
            
        except json.JSONDecodeError as e:
            error_msg = f"LLM returned invalid JSON: {e}"
            logger.error(error_msg)
            
            # Continue with partial data (resilient mode)
            parsed_data["llm_analysis"] = None
            parsed_data.setdefault("_augmentation_errors", []).append(error_msg)
            logger.warning(f"Continuing without LLM analysis for {filename}")
            
            return parsed_data
            
        except Exception as e:
            error_msg = f"LLM augmentation failed: {e}"
            logger.error(error_msg, exc_info=True)
            
            # Continue with partial data (resilient mode)
            parsed_data["llm_analysis"] = None
            parsed_data.setdefault("_augmentation_errors", []).append(str(e))
            logger.warning(f"Continuing without LLM analysis for {filename}")
            
            return parsed_data
    
    def _create_augmentation_agent(self):
        """Create LangGraph ReAct agent with RAG tool access."""
        from langgraph.prebuilt import create_react_agent
        from app.config.llm import get_llm, DOCGEN
        from app.core.tools.rag_tools import search_flatfile_docs
        
        return create_react_agent(model=get_llm(DOCGEN), tools=[search_flatfile_docs])
    
    async def _augment_file(self, agent, parsed_data: dict) -> dict:
        """Generate comprehensive file analysis using LLM.
        
        Args:
            agent: LangGraph agent with RAG tool access
            parsed_data: Parsed file data
            
        Returns:
            Dict with comprehensive analysis
        """
        prompt = self._build_file_analysis_prompt(parsed_data)
        
        @retry(
            stop=stop_after_attempt(self.AUGMENTATION_MAX_RETRIES),
            wait=wait_exponential(multiplier=1, min=2, max=10),
        )
        async def invoke_with_retry():
            result = await asyncio.wait_for(
                agent.ainvoke({"messages": [("user", prompt)]}),
                timeout=self.AUGMENTATION_TIMEOUT,
            )
            response_text = result["messages"][-1].content.strip()
            
            # Extract JSON from markdown if present
            if "```json" in response_text:
                json_match = re.search(r'```json\s*(\{.*?\})\s*```', response_text, re.DOTALL)
                if json_match:
                    response_text = json_match.group(1)
            elif "```" in response_text:
                response_text = response_text.replace("```", "").strip()
            
            return json.loads(response_text)
        
        return await invoke_with_retry()
    
    def _build_file_analysis_prompt(self, parsed_data: dict) -> str:
        """Build comprehensive prompt for fixed-length file analysis."""
        filename = parsed_data.get("fileName", "unknown")
        record_length = parsed_data.get("recordLength", 0)
        total_records = parsed_data.get("totalRecords", 0)
        schema = parsed_data.get("schema", [])
        records = parsed_data.get("records", [])[:self.llm_sample_size]
        data_quality = parsed_data.get("dataQuality", {})
        
        # Extract field details for prompt
        field_info = []
        for field in schema:
            field_info.append({
                "name": field["fieldName"],
                "position": f"{field['startPosition']}-{field['startPosition'] + field['length'] - 1}",
                "length": field["length"],
                "type": field["dataType"],
                "padding": field.get("padding", "SPACE"),
                "dotnet_type": field["suggestedDotNetType"]
            })
        
        return f"""You are analyzing a fixed-length flat file from a mainframe modernization project.

FILE METADATA:
- Filename: {filename}
- Record Length: {record_length} bytes
- Total Records: {total_records:,}
- Encoding: {parsed_data.get('encoding', 'unknown')}
- Layout: {"User-provided" if self.layout else "Auto-inferred"}

FIELD LAYOUT ({len(schema)} fields):
{json.dumps(field_info, indent=2)}

DATA QUALITY METRICS:
- Invalid Records: {data_quality.get('invalidRecords', 0)}
- Inconsistent Length Records: {data_quality.get('inconsistentLengthRecords', 0)}
- Truncated Records: {data_quality.get('truncatedRecords', 0)}

SAMPLE DATA (first {len(records)} of {total_records} records):
{json.dumps(records, indent=2)}

If you need information about mainframe fixed-length formats, COBOL copybooks, or EBCDIC encoding, use the search_flatfile_docs tool.

PROVIDE A COMPREHENSIVE ANALYSIS with the following JSON structure:
{{
  "file_description": "2-3 sentence description of what this file contains and its business purpose",
  
  "business_domain": "The business area (e.g., 'Banking Transactions', 'Employee Payroll', 'Inventory Records')",
  
  "field_analysis": [
    {{
      "field_name": "exact field name from schema",
      "position": "start-end position",
      "purpose": "What this field represents",
      "data_pattern": "Observed pattern (e.g., 'Zero-padded account numbers', 'YYYYMMDD dates', 'Space-padded names')",
      "business_significance": "Why this field matters for business operations"
    }}
    // IMPORTANT: Include one entry for EVERY field in the schema above
  ],
  
  "layout_assessment": {{
    "layout_quality": "Well-structured/Partially structured/Unstructured",
    "confidence": "High/Medium/Low confidence in field boundaries",
    "issues": ["List of layout issues if auto-inferred or if data doesn't match layout"],
    "recommendations": ["Recommendations for layout improvement or copybook creation"]
  }},
  
  "data_quality_assessment": {{
    "overall_quality": "Excellent/Good/Fair/Poor",
    "issues_found": ["Specific quality issues observed in the data"],
    "recommendations": ["Specific recommendations for data cleansing or validation"]
  }},
  
  "migration_considerations": {{
    "complexity": "Low/Medium/High",
    "copybook_needed": true/false,
    "key_challenges": ["Specific challenges for migrating this file to .NET"],
    "suggested_dotnet_structure": "Recommended .NET class or database table structure with field mappings"
  }},
  
  "notable_patterns": [
    "Any interesting patterns, relationships, dependencies, or anomalies discovered in the data"
  ]
}}

CRITICAL: Return ONLY the JSON object, no markdown code blocks, no additional text, no explanations."""
    
    def _detect_and_decode(self, raw_bytes: bytes) -> tuple[str, str]:
        """Detect encoding with mainframe EBCDIC support.
        
        Args:
            raw_bytes: Raw file bytes
            
        Returns:
            Tuple of (decoded_content, encoding_name)
            
        Raises:
            EncodingDetectionError: If encoding cannot be determined
        """
        # Try UTF-8 first (most common)
        try:
            content = raw_bytes.decode('utf-8')
            logger.debug("Encoding detected: utf-8")
            return content, 'utf-8'
        except UnicodeDecodeError:
            pass
        
        # Try ASCII
        try:
            content = raw_bytes.decode('ascii')
            logger.debug("Encoding detected: ascii")
            return content, 'ascii'
        except UnicodeDecodeError:
            pass
        
        # Try EBCDIC codepages (mainframe files)
        for codepage in self.EBCDIC_CODEPAGES:
            try:
                content = raw_bytes.decode(codepage)
                # Heuristic: Check if content looks reasonable
                if content and not all(c in '\x00\xff' for c in content[:100]):
                    logger.debug(f"Encoding detected: {codepage} (EBCDIC)")
                    return content, codepage
            except (UnicodeDecodeError, LookupError):
                continue
        
        # Fallback to chardet
        try:
            import chardet
            detected = chardet.detect(raw_bytes)
            encoding = detected.get('encoding', 'utf-8')
            confidence = detected.get('confidence', 0)
            
            logger.warning(
                f"Using chardet fallback: {encoding} (confidence: {confidence:.2%})"
            )
            
            content = raw_bytes.decode(encoding, errors='replace')
            return content, encoding
        except Exception as e:
            logger.error(f"Encoding detection failed: {e}")
            raise EncodingDetectionError(
                message="Could not detect file encoding",
                filename=None
            )
    
    def _parse_content(
        self,
        content: str,
        filename: str = "unknown.dat",
        layout: Optional[List[Dict[str, Any]]] = None,
        max_sample_records: int = 100
    ) -> Dict[str, Any]:
        """Parse fixed-length file content.
        
        Args:
            content: Raw file content as string
            filename: Name of the file
            layout: Optional field layout specification
            max_sample_records: Maximum records to include in output
            
        Returns:
            Structured dictionary with analysis results
            
        Raises:
            EmptyFileError: If file is empty
            InvalidLayoutError: If layout is invalid
        """
        # Normalize line endings
        content = content.replace('\r\n', '\n').replace('\r', '\n')
        
        # Strip BOM if present
        if content.startswith('\ufeff'):
            content = content[1:]
            logger.debug("Stripped BOM from file content")
        
        lines = [line for line in content.split('\n') if line]
        
        if not lines:
            raise EmptyFileError(
                message="Fixed-length file is empty",
                filename=filename
            )
        
        logger.debug(f"File contains {len(lines)} non-empty lines")
        
        # Determine record length
        record_length = self._determine_record_length(lines)
        logger.info(f"Determined record length: {record_length}")
        
        # Analyze or use provided layout
        if layout:
            logger.info("Using provided layout specification")
            schema = self._validate_layout(layout, record_length)
        else:
            logger.info("Inferring layout from data")
            schema = self._infer_layout(lines, record_length)
        
        # Parse records
        parsed_records = self._parse_records(lines, schema)
        
        # Analyze data quality
        data_quality = self._analyze_data_quality(lines, record_length, schema)
        
        # Generate migration insights
        migration_insights = self._generate_migration_insights(schema, data_quality)
        
        return {
            "fileType": "FIXED_LENGTH",
            "fileName": filename,
            "recordLength": record_length,
            "totalRecords": len(lines),
            "schema": schema,
            "records": parsed_records[:max_sample_records],
            "dataQuality": data_quality,
            "migrationInsights": migration_insights
        }
    
    def _determine_record_length(self, lines: List[str]) -> int:
        """Determine the expected record length."""
        if not lines:
            return 0
        
        lengths = [len(line) for line in lines]
        most_common_length = max(set(lengths), key=lengths.count)
        
        # Log if there are inconsistent lengths
        unique_lengths = set(lengths)
        if len(unique_lengths) > 1:
            logger.warning(
                f"Inconsistent record lengths detected: {sorted(unique_lengths)}, "
                f"using most common: {most_common_length}"
            )
            self._warnings.append(
                f"File has inconsistent record lengths: {sorted(unique_lengths)}"
            )
        
        return most_common_length
    
    def _validate_layout(self, layout: List[Dict], record_length: int) -> List[Dict[str, Any]]:
        """Validate and standardize provided layout.
        
        Raises:
            InvalidLayoutError: If layout is invalid
        """
        schema = []
        total_length = 0
        
        for idx, field in enumerate(layout):
            try:
                start_pos = field.get("startPosition", 1)
                length = field.get("length", 1)
                data_type = field.get("dataType", "STRING")
                
                # Validate field definition
                if start_pos < 1:
                    raise InvalidLayoutError(
                        message=f"Invalid start position {start_pos} for field {idx}",
                        filename=None
                    )
                
                if length < 1:
                    raise InvalidLayoutError(
                        message=f"Invalid length {length} for field {idx}",
                        filename=None
                    )
                
                total_length = max(total_length, start_pos + length - 1)
                
                suggested_type = field.get("suggestedDotNetType")
                if not suggested_type:
                    if data_type == "NUMERIC":
                        suggested_type = "long"
                    elif data_type == "DECIMAL":
                        suggested_type = "decimal"
                    else:
                        suggested_type = "string"
                
                schema.append({
                    "fieldName": field.get("fieldName", f"FIELD_{start_pos}"),
                    "startPosition": start_pos,
                    "length": length,
                    "dataType": data_type,
                    "signed": field.get("signed"),
                    "nullable": field.get("nullable", True),
                    "padding": field.get("padding", "SPACE"),
                    "suggestedDotNetType": suggested_type
                })
            except InvalidLayoutError:
                raise
            except Exception as e:
                logger.error(f"Error validating layout field {idx}: {e}")
                raise InvalidLayoutError(
                    message=f"Layout validation failed for field {idx}: {e}",
                    filename=None
                )
        
        # Warn if layout doesn't match record length
        if total_length != record_length:
            logger.warning(
                f"Layout total length ({total_length}) differs from record length ({record_length})"
            )
            self._warnings.append(
                f"Layout length mismatch: layout={total_length}, actual={record_length}"
            )
        
        return schema
    
    def _infer_layout(self, lines: List[str], record_length: int) -> List[Dict[str, Any]]:
        """Infer field layout by analyzing content patterns."""
        sample_lines = lines[:min(1000, len(lines))]
        field_boundaries = self._detect_field_boundaries(sample_lines, record_length)
        
        logger.debug(f"Inferred {len(field_boundaries)} fields from data")
        
        schema = []
        for idx, (start, end) in enumerate(field_boundaries):
            length = end - start
            field_values = [line[start:end] if len(line) >= end else '' for line in sample_lines]
            
            data_type, signed, padding = self._infer_field_type(field_values)
            nullable = any(not val.strip() for val in field_values)
            
            logger.debug(
                f"Field {idx + 1}: pos={start + 1}, len={length}, "
                f"type={data_type}, nullable={nullable}"
            )
            
            schema.append({
                "fieldName": f"FIELD_{idx + 1}",
                "startPosition": start + 1,
                "length": length,
                "dataType": data_type,
                "signed": signed if data_type in ["NUMERIC", "DECIMAL"] else None,
                "nullable": nullable,
                "padding": padding,
                "suggestedDotNetType": self._map_to_dotnet_type(data_type, signed)
            })
        
        return schema
    
    def _detect_field_boundaries(self, lines: List[str], record_length: int) -> List[tuple]:
        """Detect field boundaries by analyzing whitespace patterns.
        
        Note: This is a simplified implementation. For production use,
        consider more sophisticated boundary detection algorithms.
        """
        # For now, treat entire line as single field
        # TODO: Implement sophisticated boundary detection based on:
        # - Whitespace column analysis
        # - Data type transitions
        # - Padding patterns
        
        logger.warning("Using simplified field boundary detection (single field)")
        self._warnings.append("Field boundaries inferred as single field - provide layout for better accuracy")
        
        return [(0, record_length)]
    
    def _infer_field_type(self, values: List[str]) -> tuple:
        """Infer field data type and characteristics."""
        non_empty = [v.strip() for v in values if v.strip()]
        
        if not non_empty:
            return "STRING", None, "SPACE"
        
        padding = self._detect_padding(values)
        
        numeric_count = sum(1 for v in non_empty if self._is_numeric_field(v.strip()))
        if numeric_count > len(non_empty) * 0.8:
            signed = any(v.strip().startswith('-') or v.strip().startswith('+') for v in non_empty)
            
            if any('.' in v.strip() for v in non_empty):
                return "DECIMAL", signed, padding
            else:
                return "NUMERIC", signed, padding
        
        return "STRING", None, padding
    
    def _is_numeric_field(self, value: str) -> bool:
        """Check if field value is numeric."""
        try:
            clean_val = value.replace(',', '').replace('+', '')
            float(clean_val)
            return True
        except:
            return False
    
    def _detect_padding(self, values: List[str]) -> str:
        """Detect padding type (SPACE or ZERO)."""
        zero_padded = sum(1 for v in values if v and v.lstrip() != v and len(v) > 0 and v[0] == '0')
        space_padded = sum(1 for v in values if v and v != v.strip())
        
        if zero_padded > space_padded:
            return "ZERO"
        return "SPACE"
    
    def _map_to_dotnet_type(self, data_type: str, signed: Optional[bool]) -> str:
        """Map field type to .NET type."""
        if data_type == "NUMERIC":
            return "long"
        elif data_type == "DECIMAL":
            return "decimal"
        else:
            return "string"
    
    def _parse_records(self, lines: List[str], schema: List[Dict]) -> List[Dict[str, Any]]:
        """Parse fixed-length records based on schema with error tracking."""
        records = []
        
        for line_num, line in enumerate(lines, 1):
            try:
                record = {}
                
                for field in schema:
                    start = field["startPosition"] - 1
                    end = start + field["length"]
                    
                    # Handle truncated lines
                    if len(line) < end:
                        if len(line) <= start:
                            value = ""
                        else:
                            value = line[start:]
                        
                        error_msg = f"Line truncated: expected length {end}, got {len(line)}"
                        self._unrecognized.append({
                            "line": line_num,
                            "content": line[:100],
                            "error": error_msg
                        })
                        
                        if not self.skip_invalid_rows:
                            raise FixedLengthParseError(
                                message=error_msg,
                                filename=None,
                                line_number=line_num,
                                content=line[:100]
                            )
                    else:
                        value = line[start:end]
                    
                    cleaned = value.strip()
                    
                    # Type conversion
                    if not cleaned:
                        record[field["fieldName"]] = None
                    elif field["dataType"] == "NUMERIC":
                        try:
                            record[field["fieldName"]] = int(cleaned)
                        except ValueError:
                            logger.debug(
                                f"Line {line_num}, Field '{field['fieldName']}': "
                                f"Could not convert to integer: '{cleaned}'"
                            )
                            record[field["fieldName"]] = cleaned
                    elif field["dataType"] == "DECIMAL":
                        try:
                            record[field["fieldName"]] = float(cleaned)
                        except ValueError:
                            logger.debug(
                                f"Line {line_num}, Field '{field['fieldName']}': "
                                f"Could not convert to float: '{cleaned}'"
                            )
                            record[field["fieldName"]] = cleaned
                    else:
                        record[field["fieldName"]] = cleaned
                
                records.append(record)
                
            except FixedLengthParseError:
                raise
            except Exception as e:
                error_msg = f"Failed to parse record: {e}"
                logger.warning(f"Line {line_num}: {error_msg}")
                self._unrecognized.append({
                    "line": line_num,
                    "content": line[:100],
                    "error": error_msg
                })
                
                if not self.skip_invalid_rows:
                    raise FixedLengthParseError(
                        message=error_msg,
                        filename=None,
                        line_number=line_num,
                        content=line[:100]
                    )
        
        return records
    
    def _analyze_data_quality(self, lines: List[str], record_length: int, 
                             schema: List[Dict]) -> Dict[str, Any]:
        """Analyze data quality issues."""
        invalid_records = 0
        inconsistent_length = 0
        truncated_records = 0
        
        for line in lines:
            line_length = len(line)
            
            if line_length != record_length:
                inconsistent_length += 1
                
                if line_length < record_length:
                    truncated_records += 1
                    invalid_records += 1
        
        return {
            "invalidRecords": invalid_records,
            "inconsistentLengthRecords": inconsistent_length,
            "truncatedRecords": truncated_records
        }
    
    def _generate_migration_insights(self, schema: List[Dict], 
                                    data_quality: Dict) -> Dict[str, Any]:
        """Generate migration insights."""
        fields_needing_validation = []
        
        for field in schema:
            if field["dataType"] in ["NUMERIC", "DECIMAL"]:
                fields_needing_validation.append(field["fieldName"])
        
        multi_layout = False
        
        return {
            "multiLayoutDetected": multi_layout,
            "fieldsNeedingValidation": fields_needing_validation
        }