"""
Prompt templates for file summarization and documentation generation.
Enforces strict JSON output for programmatic Markdown assembly.
"""

JSON_FORMAT_INSTRUCTION = """
CRITICAL OUTPUT INSTRUCTIONS:
1. You are a Mainframe Technical Analyst.
2. Analyze the provided code/context.
3. Return ONLY a valid, parseable JSON object.
4. Do NOT include Markdown formatting (no ```json ... ```). 
5. Do NOT include conversational text.
6. Ensure all arrays/lists are populated if data exists, otherwise empty list [].
"""

EXECUTIVE_SUMMARY_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

CONTEXT:
You are documenting a Mainframe System.
Below are the System Metrics and the descriptions of the most critical modules (God Classes).

SYSTEM METRICS:
{{metrics}}

CRITICAL MODULES:
{{top_modules}}

TASK:
Generate the "Business Overview" content for the Master Documentation.
Infer the business context based on the module names (e.g., 'PAY*' -> Payroll) and descriptions.

REQUIRED JSON STRUCTURE:
{{{{
    "business_purpose": "2-3 sentences explaining what this system achieves (e.g., 'Daily Merchant Settlement...')",
    "business_scope": [
        "Bullet point 1 (e.g., 'Processes daily batches')",
        "Bullet point 2"
    ],
    "process_flow_steps": [
        "Step 1 (e.g., 'Ingest ISO8583')",
        "Step 2 (e.g., 'Calculate Fees')",
        "Step 3 (e.g., 'Update DB2')"
    ],
    "schedule_frequency": {{
        "frequency": "e.g., Daily (Weekdays) or Real-time",
        "start_time": "Inferred start time or 'Event Driven'",
        "sla_window": "Inferred or 'Standard Batch Window'"
    }},
    "data_overview": {{
        "inputs": ["List of input data types inferred from file names"],
        "outputs": ["List of output artifacts/reports"]
    }},
    "business_risks": [
        "Risk 1 (e.g., 'Delays impact settlement')",
        "Risk 2"
    ],
    "external_interfaces": [
        {{{{ "interface": "Name", "direction": "Inbound/Outbound", "description": "Short desc" }}}}
    ],
    "ownership": {{
        "business_owner": "Inferred Department (e.g., Finance Ops)",
        "it_owner": "Inferred Team (e.g., Mainframe Batch Support)"
    }}
}}}}
"""

CUMULATIVE_MERGE_INSTRUCTION = """
This is a chunked analysis. 
1. Review 'previous_summary' (JSON).
2. Analyze 'chunk_code'.
3. MERGE the new findings into the JSON structure. 
   - Add new functionalities found.
   - Update file/table operations.
   - Do NOT overwrite existing valid data; extend lists.
"""

COBOL_CHUNK_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}
{{CUMULATIVE_MERGE_INSTRUCTION}}

PREVIOUS SUMMARY JSON:
{{previous_summary}}

CURRENT CODE CHUNK:
{{chunk}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "COBOL",
    "business_purpose": "1 sentence description of what this program does.",
    "functional_capabilities": [
        "Specific business rule 1",
        "Specific calculation 2"
    ],
    "key_operations": [
        "e.g., 'Reads CUSTOMER-FILE'",
        "e.g., 'Calls SUBPROG'"
    ],
    "data_interactions": [
        {{{{ "target": "Table/File Name", "operation": "READ/WRITE/UPDATE" }}}}
    ],
    "technical_notes": [
        "Complex logic details",
        "Error handling specifics"
    ]
}}}}
"""

PLI_CHUNK_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}
{{CUMULATIVE_MERGE_INSTRUCTION}}

PREVIOUS SUMMARY JSON:
{{previous_summary}}

CURRENT CODE CHUNK:
{{chunk}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "PL/I",
    "business_purpose": "1 sentence description.",
    "functional_capabilities": ["List of capabilities"],
    "key_operations": ["List of major ops"],
    "technical_notes": ["List of notes"]
}}}}
"""

ASSEMBLY_CHUNK_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}
{{CUMULATIVE_MERGE_INSTRUCTION}}

PREVIOUS SUMMARY JSON:
{{previous_summary}}

CURRENT CODE CHUNK:
{{chunk}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "ASSEMBLY",
    "business_purpose": "1 sentence description.",
    "register_usage": ["R12: Base", "R15: Return Code"],
    "macros_used": ["GETMAIN", "WTOR"],
    "functional_capabilities": ["List of logic performed"],
    "technical_notes": ["Memory management details", "Reentrancy notes"]
}}}}
"""

REXX_CHUNK_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}
{{CUMULATIVE_MERGE_INSTRUCTION}}

PREVIOUS SUMMARY JSON:
{{previous_summary}}

CURRENT CODE CHUNK:
{{chunk}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "REXX",
    "business_purpose": "1 sentence description (e.g., Automates file transfer).",
    "automation_tasks": [
        "Task 1 (e.g., Submits Job X)",
        "Task 2 (e.g., Checks dataset existence)"
    ],
    "external_utilities": [
        "TSO Command",
        "ISPF Service"
    ],
    "technical_notes": ["Error trapping", "Parse logic"]
}}}}
"""

JCL_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

Analyze this JCL/PROC.

CODE:
{{content}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "JCL",
    "job_header": {{{{
        "job_name": "Extracted from JOB card",
        "class": "CLASS value",
        "owner": "USER/NOTIFY value"
    }}}},
    "business_purpose": "1 sentence description of the workflow.",
    "steps": [
        {{{{ "step_name": "STEP01", "program": "PGMNAME", "description": "Brief explanation of this step" }}}}
    ],
    "io_datasets": [
        {{{{ "dataset": "A.B.C", "usage": "Input/Output based on DISP" }}}}
    ],
    "dependencies": [
        "List of PROCs or Programs called"
    ],
    "schedule_notes": "Any comments regarding timing or frequency found in JCL comments"
}}}}
"""

CA7_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

Analyze this CA-7 Job Definition.

CODE:
{{chunk}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "CA7",
    "identification": {{{{
        "job_name": "Name",
        "system": "System Name",
        "schid": "ID",
        "owner": "Owner ID",
        "class": "Class"
    }}}},
    "execution_characteristics": [
        {{{{ "flag": "e.g., Y", "meaning": "LOAD STEP EXECUTED" }}}}
    ],
    "conditional_dependencies": [
        {{{{ "job": "PRED_JOB", "type": "Job Dependency" }}}}
    ],
    "dataset_dependencies": [
        {{{{ "dataset": "DS.NAME", "type": "Input Trigger" }}}}
    ],
    "user_requirements": [
        "Manual requirement description"
    ]
}}}}
"""

COPYBOOK_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

Analyze this Copybook/DCLGEN.

CODE:
{{content}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "COPYBOOK",
    "business_purpose": "Description of the data entity (e.g., 'Customer Master Record').",
    "entity_name": "Root 01 level name or SQL Table name",
    "key_fields": [
        {{{{ "field": "Field Name", "description": "Inferred usage (ID, Amount, Date)" }}}}
    ],
    "technical_notes": ["Data types", "Redefines usage"]
}}}}
"""

# Alias for other single-pass types
DCLGEN_PROMPT = COPYBOOK_PROMPT
PLI_COPYBOOK_PROMPT = COPYBOOK_PROMPT

PARMLIB_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

Analyze this Parmlib Member.

CODE:
{{chunk}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "PARMLIB",
    "business_purpose": "Description of what this configures (e.g., 'System Initialization').",
    "configuration_areas": [
        "Area 1 (e.g., 'Memory Limits')",
        "Area 2 (e.g., 'Device addresses')"
    ],
    "key_parameters": [
        {{{{ "name": "PARAM", "value": "VALUE", "description": "What this controls" }}}}
    ]
}}}}
"""

CSV_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}
Analyze this Flat File sample.
CODE:
{{content}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "FLAT_FILE",
    "business_purpose": "Description of data content.",
    "columns": [
        {{{{ "name": "Col Name", "type": "DataType", "significance": "Business meaning" }}}}
    ],
    "volume_characteristics": "Observations about record width/types"
}}}}
"""

FIXED_LENGTH_PROMPT = CSV_PROMPT