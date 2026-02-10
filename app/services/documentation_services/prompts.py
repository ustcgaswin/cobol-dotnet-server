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

CUMULATIVE_MERGE_INSTRUCTION = """
This is a chunked analysis. 
1. Review 'previous_summary' (JSON).
2. Analyze 'chunk_code'.
3. MERGE the new findings into the JSON structure. 
   - Add new functionalities found.
   - Update file/table operations.
   - Do NOT overwrite existing valid data; extend lists.
"""

# EXECUTIVE_SUMMARY_PROMPT = """{JSON_FORMAT_INSTRUCTION}

# CONTEXT:
# You are documenting a Mainframe System consisting of ~400 modules.
# Below are System Metrics and descriptions of the CORE modules that drive the system.

# SYSTEM METRICS:
# {metrics}

# CORE MODULE SUMMARIES:
# {top_modules}

# TASK:
# Synthesize a deep "Functional Specification". You must be verbose. Do not give 1-sentence answers.

# REQUIRED JSON STRUCTURE:
# {{
#     "business_purpose": "A detailed 2-3 paragraph explanation of the system's role in the enterprise.",
#     "business_scope": [
#         "Detail the primary business functions (e.g., Transaction Validation, Interest Calculation, Monthly Ledger Closing)."
#     ],
#     "process_flow_steps": [
#         "Step 1: [In-depth description of initial data entry/ingestion]",
#         "Step 2: [Description of core logic processing]",
#         "Step 3: [Description of final reporting/persistence]"
#     ],
#     "functional_flow_diagram": {{
#         "description": "Exhaustive description of the data lifecycle.",
#         "mermaid_code": "sequenceDiagram... (Show interaction between User, Batch, DB, and External Feeds)",
#         "steps_table": [
#             {{ "actor": "Name", "action": "Specific Action", "outcome": "Business Result" }}
#         ]
#     }},
#     "external_interfaces": [
#         {{ 
#            "interface": "Inferred System Name (e.g. GL System, HR Database)", 
#            "direction": "Inbound/Outbound", 
#            "description": "What data is exchanged and why?" 
#         }}
#     ],
#     "downstream_feeds_detail": [
#         "Specifically identify files or reports that appear to be sent to other departments based on the names (e.g. EXTRACT, REPORT, TAPE)."
#     ],
#     "glossary": [
#         {{ "term": "Acronym", "definition": "Business meaning" }}
#     ]
# }}
# """

EXECUTIVE_SUMMARY_PROMPT = """{JSON_FORMAT_INSTRUCTION}

CONTEXT:
You are documenting a Mainframe System.
Below are the System Metrics, descriptions of critical modules, and a harvest of raw business rules found in the code.

SYSTEM METRICS:
{metrics}

CRITICAL MODULES:
{top_modules}

TASK:
Generate the "Introduction" and "System Architecture" content.
Crucially, you must synthesize the "Raw Business Rules" into high-level Processing Boundaries (e.g., Data retention limits, fiscal year logic, purging criteria).

REQUIRED JSON STRUCTURE:
{{
    "business_purpose": "2-3 sentences explaining what this system achieves.",
    
    "business_scope": [
        "Bullet point 1",
        "Bullet point 2"
    ],

    "system_processing_boundaries": [
        "Constraint 1 (e.g., 'Data history is retained for 72 months for active accounts')",
        "Constraint 2 (e.g., 'Transactions are batched by Region ID')",
        "Constraint 3 (e.g., 'Loading process validates header dates against control file')"
    ],
    
    "system_landscape": "Technical summary sentence.",
    
    "glossary": [
        {{ "term": "Acronym", "definition": "Definition" }}
    ],

    "functional_flow_diagram": {{
        "description": "Flow description.",
        "mermaid_code": "sequenceDiagram...",
        "steps_table": [
            {{ "actor": "Actor", "action": "Action", "outcome": "Outcome" }}
        ]
    }},

    "schedule_frequency": {{ "frequency": "...", "start_time": "..." }},
    "data_overview": {{ "inputs": [], "outputs": [] }},
    "business_risks": [],
    "external_interfaces": [],
    "ownership": {{ "business_owner": "...", "it_owner": "..." }}
}}
"""

COBOL_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
{CUMULATIVE_MERGE_INSTRUCTION}

CURRENT CODE:
{chunk}

TASK:
Extract the BUSINESS RULES. If you see an IF statement, translate it to a functional requirement. 
(e.g., IF BALANCE < 0 translates to 'System prevents processing for overdrawn accounts').

REQUIRED JSON STRUCTURE:
{{
    "filename": "{filename}",
    "type": "COBOL",
    "business_overview": {{
        "title": "Business Process Name",
        "purpose": "DETAILED 3-4 sentence business explanation.",
        "functional_category": "Select: Transaction Processing, Reporting, Data Maintenance, or Utility",
        "business_rules": [
            "Rule 1: [Detailed rule extracted from IF/EVALUATE logic]",
            "Rule 2: [Detailed rule extracted from logic]"
        ],
        "key_data_entities": ["Specific entities from the DATA DIVISION"]
    }},
    "technical_analysis": {{
        "data_interactions": [
            {{ "target": "Table/File", "operation": "READ/WRITE/UPDATE" }}
        ],
        "key_operations": ["Logical steps taken"],
        "technical_notes": ["Error codes, specific IBM utilities used"]
    }}
}}
"""

PLI_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
{CUMULATIVE_MERGE_INSTRUCTION}

PREVIOUS SUMMARY JSON:
{previous_summary}

CURRENT CODE CHUNK:
{chunk}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "PLI",

    "business_overview": {{
        "title": "Process Name",
        "purpose": "Business goal description.",
        "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
        "scope": ["Business logic scope"],
        "key_data_entities": ["Entities processed"]
    }},

    "technical_analysis": {{
        "functional_capabilities": ["Specific procedure logic"],
        "key_operations": ["I/O and Calls"],
        "data_interactions": [
            {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
        ],
        "technical_notes": ["Memory/Pointer notes"]
    }}
}}
"""

ASSEMBLY_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
{CUMULATIVE_MERGE_INSTRUCTION}

PREVIOUS SUMMARY JSON:
{previous_summary}

CURRENT CODE CHUNK:
{chunk}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "ASSEMBLY",

    "business_overview": {{
        "title": "System Module Name",
        "purpose": "What low-level function does this perform? (e.g. 'Date conversion routine', 'I/O Driver').",
        "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
        "scope": [
            "Functions provided",
            "System interactions"
        ],
        "key_data_entities": [
            "Data areas or Control blocks accessed"
        ]
    }},

    "technical_analysis": {{
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
        "data_interactions": [
            {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
        ],
        "technical_notes": [
            "Reentrancy considerations",
            "Addressing modes (AMODE/RMODE)"
        ]
    }}
}}
"""

REXX_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
{CUMULATIVE_MERGE_INSTRUCTION}

PREVIOUS SUMMARY JSON:
{previous_summary}

CURRENT CODE CHUNK:
{chunk}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "REXX",

    "business_overview": {{
        "title": "Automation Process",
        "purpose": "What manual task does this script automate? (e.g. 'Daily Report Bursting').",
        "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
        "scope": [
            "Tasks performed",
            "Systems interacted with (TSO, ISPF, DB2)"
        ],
        "key_data_entities": [
            "Files moved/renamed",
            "Jobs submitted"
        ]
    }},

    "technical_analysis": {{
        "automation_tasks": [
            "Specific automation steps"
        ],
        "external_utilities": [
            "TSO Commands (ALLOC, FREE)",
            "ISPF Services (FTOPEN, FTCLOSE)"
        ],
        "data_interactions": [
            {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
        ],
        "technical_notes": [
            "Parsing logic",
            "Error trapping (SIGNAL ON ERROR)"
        ]
    }}
}}
"""

JCL_PROMPT = """{JSON_FORMAT_INSTRUCTION}

Analyze this JCL/PROC.

CODE:
{content}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "JCL",

    "business_overview": {{
        "title": "Job Workflow Name",
        "purpose": "What business process does this orchestrate?",
        "functional_category": "Must be one of: 'Core Batch', 'Reporting', 'Housekeeping', 'Ingestion', 'Distribution'",
        "scope": ["Process boundaries", "Frequency"],
        "key_data_entities": ["Major inputs/outputs"]
    }},

    "technical_analysis": {{
        "job_header": {{
            "job_name": "Name",
            "class": "CLASS",
            "owner": "USER/NOTIFY"
        }},
        "steps": [
            {{"step_name": "STEP01", "program": "PGMNAME", "description": "Technical description"}}
        ],
        "io_datasets": [
            {{"dataset": "A.B.C", "usage": "Input/Output"}}
        ],
        "schedule_notes": "Restart/timing notes"
    }}
}}
"""

CA7_PROMPT = """{JSON_FORMAT_INSTRUCTION}

Analyze this CA-7 Job Definition.

CODE:
{chunk}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "CA7",

    "business_overview": {{
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
    }},

    "technical_analysis": {{
        "identification": {{
            "job_name": "Name",
            "system": "System Name",
            "schid": "ID",
            "class": "Class"
        }},
        "dependencies_triggers": [
            {{"type": "Job/Dataset", "target": "Name", "condition": "Requirement"}}
        ],
        "user_requirements": [
            "Manual sign-offs or checks"
        ],
        "notes": [
            "Operational rules",
            "Restart instructions"
        ]
    }}
}}
"""

COPYBOOK_PROMPT = """{JSON_FORMAT_INSTRUCTION}

Analyze this Data Definition (Copybook/DCLGEN/Include).

CODE:
{content}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "COPYBOOK",

    "business_overview": {{
        "title": "Data Entity Definition",
        "purpose": "What business entity does this structure represent? (e.g. 'Customer Address Record').",
        "scope": [
            "Data domain (e.g. Billing, Inventory)",
            "Usage context"
        ],
        "key_data_entities": [
            "The primary entity defined"
        ]
    }},

    "technical_analysis": {{
        "table_name": "SQL Table Name (if DCLGEN) or Root Field Name",
        "key_fields": [
            {{"field": "Field Name", "description": "Inferred usage (ID, Amount, Date)"}}
        ],
        "table_structure": [
             {{"column_name": "Name", "type": "DataType", "nullable": "Yes/No"}}
        ],
        "technical_notes": [
            "Data types used (COMP-3, VARCHAR)",
            "Redefines or arrays present"
        ]
    }}
}}
"""

# Aliases for file types that use the same logic
DCLGEN_PROMPT = COPYBOOK_PROMPT
PLI_COPYBOOK_PROMPT = COPYBOOK_PROMPT

BIND_PROMPT = """{JSON_FORMAT_INSTRUCTION}

Analyze this DB2 BIND card.

CODE:
{content}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "BIND",

    "business_overview": {{
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
    }},

    "technical_analysis": {{
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
    }}
}}
"""

PARMLIB_PROMPT = """{JSON_FORMAT_INSTRUCTION}

Analyze this System Parameter Member.

CODE:
{chunk}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "PARMLIB",

    "business_overview": {{
        "title": "System Configuration",
        "purpose": "What aspect of the system does this control? (e.g. 'CICS Initialization', 'Storage Groups').",
        "scope": [
            "Subsystems affected",
            "Resources managed"
        ],
        "key_data_entities": [
            "Managed resources (APF Libraries, Page Datasets)"
        ]
    }},

    "technical_analysis": {{
        "configuration_areas": [
            "Area 1 (e.g. 'Memory Limits')",
            "Area 2 (e.g. 'Device addresses')"
        ],
        "key_parameters": [
            {{"name": "PARAM", "value": "VALUE", "description": "What this controls"}}
        ],
        "technical_notes": [
            "IPL requirements",
            "Dependencies on other members"
        ]
    }}
}}
"""

CSV_PROMPT = """{JSON_FORMAT_INSTRUCTION}

Analyze this Flat File sample/layout.

CODE:
{content}

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "FLAT_FILE",

    "business_overview": {{
        "title": "Data Feed Definition",
        "purpose": "What business data does this file contain? (e.g. 'Daily Transactions').",
        "scope": [
            "Data origin",
            "Data destination/usage"
        ],
        "key_data_entities": [
            "Primary entity in record (e.g. Transaction)"
        ]
    }},

    "technical_analysis": {{
        "table_structure": [
            {{"column_name": "Name", "type": "DataType", "nullable": "Inferred from data"}}
        ],
        "technical_notes": [
            "Delimiter details",
            "Volume characteristics",
            "Data quality observations"
        ]
    }}
}}
"""

FIXED_LENGTH_PROMPT = CSV_PROMPT

# Research Agent System Prompt
DOC_AGENT_RESEARCH_SYSTEM_PROMPT = """
You are a Lead Mainframe Research Architect. Your goal is to gather raw evidence for: {target_file}.

RESEARCH PROTOCOL:
1. ANALYZE: Use 'view_file' to read the code logic.
2. SEARCH: Use 'grep_search' to find external references if imports are unclear.
3. CONTEXT: The system structure has been provided in the user message (Mermaid Diagram).

STRICT EXIT CRITERIA:
- For COPYBOOKS/DCLGEN: Once you have the record structure/columns, STOP.
- For JCL/REXX: Once you have the steps/flow, STOP.
- For COBOL/PLI: Once you have the 'PROCEDURE DIVISION' or 'MAIN' logic, STOP.

If a tool returns 'No matches found', do not keep retrying. Move to the writing phase.
DO NOT loop more than 3-5 times.

When you have gathered sufficient information, respond with a summary of what you found WITHOUT calling any more tools.
"""