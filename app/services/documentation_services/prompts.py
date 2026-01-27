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
    
    "business_overview": {{{{
        "title": "Business Process Name (Inferred from logic/comments)",
        "purpose": "2-3 sentences explaining the business goal of this specific program.",
        "scope": [
            "What business rules does this handle? (e.g. 'Calculates Interest')",
            "What data boundaries exist? (e.g. 'Processes only Active Accounts')"
        ],
        "key_data_entities": [
            "Business entities touched (e.g. 'Customer Record', 'General Ledger')"
        ]
    }}}},

    "technical_analysis": {{{{
        "functional_capabilities": [
            "Specific logic point 1",
            "Specific logic point 2"
        ],
        "key_operations": [
            "File I/O details (e.g. READ ACCT-FILE)",
            "Subroutine calls (e.g. CALL DATE-CONV)"
        ],
        "technical_notes": [
            "Error handling specifics",
            "Performance notes or sort logic"
        ]
    }}}}
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
    "type": "PLI",

    "business_overview": {{{{
        "title": "Process Name (Inferred)",
        "purpose": "2-3 sentences explaining the business goal of this PL/I program.",
        "scope": [
            "Business logic scope",
            "Processing boundaries"
        ],
        "key_data_entities": [
            "Entities processed (e.g. 'Invoices', 'Inventory')"
        ]
    }}}},

    "technical_analysis": {{{{
        "functional_capabilities": [
            "Specific procedure logic",
            "Exception handling (ON Units)"
        ],
        "key_operations": [
            "Stream I/O or Record I/O",
            "Database calls"
        ],
        "technical_notes": [
            "Memory management (ALLOCATE/FREE)",
            "Pointer manipulation notes"
        ]
    }}}}
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

    "business_overview": {{{{
        "title": "System Module Name",
        "purpose": "What low-level function does this perform? (e.g. 'Date conversion routine', 'I/O Driver').",
        "scope": [
            "Functions provided",
            "System interactions"
        ],
        "key_data_entities": [
            "Data areas or Control blocks accessed"
        ]
    }}}},

    "technical_analysis": {{{{
        "register_usage": [
            "R12: Base Register",
            "R15: Return Code"
        ],
        "functional_capabilities": [
            "Specific logic flow",
            "Bit manipulation or arithmetic"
        ],
        "key_operations": [
            "Macros used (GETMAIN, WTOR, LINK)",
            "System services accessed"
        ],
        "technical_notes": [
            "Reentrancy considerations",
            "Addressing modes (AMODE/RMODE)"
        ]
    }}}}
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

    "business_overview": {{{{
        "title": "Automation Process",
        "purpose": "What manual task does this script automate? (e.g. 'Daily Report Bursting').",
        "scope": [
            "Tasks performed",
            "Systems interacted with (TSO, ISPF, DB2)"
        ],
        "key_data_entities": [
            "Files moved/renamed",
            "Jobs submitted"
        ]
    }}}},

    "technical_analysis": {{{{
        "automation_tasks": [
            "Specific automation steps"
        ],
        "external_utilities": [
            "TSO Commands (ALLOC, FREE)",
            "ISPF Services (FTOPEN, FTCLOSE)"
        ],
        "technical_notes": [
            "Parsing logic",
            "Error trapping (SIGNAL ON ERROR)"
        ]
    }}}}
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

    "business_overview": {{{{
        "title": "Job Workflow",
        "purpose": "What business process does this Job orchestrate? (e.g. 'End of Month Settlement').",
        "scope": [
            "Process boundaries",
            "Frequency (if mentioned in comments)"
        ],
        "key_data_entities": [
            "Major inputs/outputs (e.g. 'Transaction Tape', 'Audit Report')"
        ]
    }}}},

    "technical_analysis": {{{{
        "job_header": {{{{
            "job_name": "Name from JOB card",
            "class": "CLASS value",
            "owner": "USER/NOTIFY value"
        }}}},
        "steps": [
            {{{{ "step_name": "STEP01", "program": "PGMNAME", "description": "Technical description of step" }}}}
        ],
        "io_datasets": [
            {{{{ "dataset": "A.B.C", "usage": "Input/Output based on DISP" }}}}
        ],
        "schedule_notes": "Restart logic, dependencies, or timing notes"
    }}}}
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

    "business_overview": {{{{
        "title": "Workload Schedule",
        "purpose": "What is the scheduling goal? (e.g. 'Triggers Nightly Batch').",
        "scope": [
            "Systems covered",
            "Timeframes managed"
        ],
        "key_data_entities": [
            "Triggering datasets",
            "Critical jobs managed"
        ]
    }}}},

    "technical_analysis": {{{{
        "identification": {{{{
            "job_name": "Name",
            "system": "System Name",
            "schid": "ID",
            "class": "Class"
        }}}},
        "dependencies_triggers": [
            {{{{ "type": "Job/Dataset", "target": "Name", "condition": "Requirement" }}}}
        ],
        "user_requirements": [
            "Manual sign-offs or checks"
        ],
        "notes": [
            "Operational rules",
            "Restart instructions"
        ]
    }}}}
}}}}
"""

COPYBOOK_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

Analyze this Data Definition (Copybook/DCLGEN/Include).

CODE:
{{content}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "COPYBOOK",

    "business_overview": {{{{
        "title": "Data Entity Definition",
        "purpose": "What business entity does this structure represent? (e.g. 'Customer Address Record').",
        "scope": [
            "Data domain (e.g. Billing, Inventory)",
            "Usage context"
        ],
        "key_data_entities": [
            "The primary entity defined"
        ]
    }}}},

    "technical_analysis": {{{{
        "table_name": "SQL Table Name (if DCLGEN) or Root Field Name",
        "key_fields": [
            {{{{ "field": "Field Name", "description": "Inferred usage (ID, Amount, Date)" }}}}
        ],
        "table_structure": [
             {{{{ "column_name": "Name", "type": "DataType", "nullable": "Yes/No" }}}}
        ],
        "technical_notes": [
            "Data types used (COMP-3, VARCHAR)",
            "Redefines or arrays present"
        ]
    }}}}
}}}}
"""

# Aliases for file types that use the same logic
DCLGEN_PROMPT = COPYBOOK_PROMPT
PLI_COPYBOOK_PROMPT = COPYBOOK_PROMPT

BIND_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

Analyze this DB2 BIND card.

CODE:
{{content}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "BIND",

    "business_overview": {{{{
        "title": "Database Plan Configuration",
        "purpose": "What data access strategy does this configure? (e.g. 'Binds Payroll Modules to DB2').",
        "scope": [
            "Modules covered",
            "Environments applied to"
        ],
        "key_data_entities": [
            "Plan Name",
            "Collections/Packages"
        ]
    }}}},

    "technical_analysis": {{{{
        "key_parameters": [
            "ISOLATION Level",
            "RELEASE option",
            "VALIDATE option"
        ],
        "configuration_areas": [
            "List of DBRMs or Packages included (PKLIST)",
            "Owner/Qualifier details"
        ],
        "technical_notes": [
            "Performance implications",
            "Concurrency settings"
        ]
    }}}}
}}}}
"""

PARMLIB_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

Analyze this System Parameter Member.

CODE:
{{chunk}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "PARMLIB",

    "business_overview": {{{{
        "title": "System Configuration",
        "purpose": "What aspect of the system does this control? (e.g. 'CICS Initialization', 'Storage Groups').",
        "scope": [
            "Subsystems affected",
            "Resources managed"
        ],
        "key_data_entities": [
            "Managed resources (APF Libraries, Page Datasets)"
        ]
    }}}},

    "technical_analysis": {{{{
        "configuration_areas": [
            "Area 1 (e.g. 'Memory Limits')",
            "Area 2 (e.g. 'Device addresses')"
        ],
        "key_parameters": [
            {{{{ "name": "PARAM", "value": "VALUE", "description": "What this controls" }}}}
        ],
        "technical_notes": [
            "IPL requirements",
            "Dependencies on other members"
        ]
    }}}}
}}}}
"""

CSV_PROMPT = f"""{{JSON_FORMAT_INSTRUCTION}}

Analyze this Flat File sample/layout.

CODE:
{{content}}

REQUIRED JSON STRUCTURE:
{{{{
    "filename": "From metadata",
    "type": "FLAT_FILE",

    "business_overview": {{{{
        "title": "Data Feed Definition",
        "purpose": "What business data does this file contain? (e.g. 'Daily Transactions').",
        "scope": [
            "Data origin",
            "Data destination/usage"
        ],
        "key_data_entities": [
            "Primary entity in record (e.g. Transaction)"
        ]
    }}}},

    "technical_analysis": {{{{
        "table_structure": [
            {{{{ "column_name": "Name", "type": "DataType", "nullable": "Inferred from data" }}}}
        ],
        "technical_notes": [
            "Delimiter details",
            "Volume characteristics",
            "Data quality observations"
        ]
    }}}}
}}}}
"""

FIXED_LENGTH_PROMPT = CSV_PROMPT