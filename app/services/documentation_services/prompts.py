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

# CUMULATIVE_MERGE_INSTRUCTION = """
# This is a chunked analysis. 
# 1. Review 'previous_summary' (JSON).
# 2. Analyze 'chunk_code'.
# 3. MERGE the new findings into the JSON structure. 
#    - Add new functionalities found.
#    - Update file/table operations.
#    - Do NOT overwrite existing valid data; extend lists.
# """

# EXECUTIVE_SUMMARY_PROMPT = """{JSON_FORMAT_INSTRUCTION}

# CONTEXT:
# You are documenting a Mainframe System.
# Below are the System Metrics, descriptions of critical modules, and a harvest of raw business rules found in the code.

# SYSTEM METRICS:
# {metrics}

# CRITICAL MODULES:
# {top_modules}

# TASK:
# Generate the "Introduction" and "System Architecture" content.
# Crucially, you must synthesize the "Raw Business Rules" into high-level Processing Boundaries (e.g., Data retention limits, fiscal year logic, purging criteria).

# REQUIRED JSON STRUCTURE:
# {{
#     "business_purpose": "2-3 sentences explaining what this system achieves.",
    
#     "business_scope": [
#         "Bullet point 1",
#         "Bullet point 2"
#     ],

#     "system_processing_boundaries": [
#         "Constraint 1 (e.g., 'Data history is retained for 72 months for active accounts')",
#         "Constraint 2 (e.g., 'Transactions are batched by Region ID')",
#         "Constraint 3 (e.g., 'Loading process validates header dates against control file')"
#     ],
    
#     "system_landscape": "Technical summary sentence.",
    
#     "glossary": [
#         {{ "term": "Acronym", "definition": "Definition" }}
#     ],

#     "functional_flow_diagram": {{
#         "description": "Flow description.",
#         "mermaid_code": "sequenceDiagram...",
#         "steps_table": [
#             {{ "actor": "Actor", "action": "Action", "outcome": "Outcome" }}
#         ]
#     }},

#     "schedule_frequency": {{ "frequency": "...", "start_time": "..." }},
#     "data_overview": {{ "inputs": [], "outputs": [] }},
#     "business_risks": [],
#     "external_interfaces": [],
#     "ownership": {{ "business_owner": "...", "it_owner": "..." }}
# }}
# """

# COBOL_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
# {CUMULATIVE_MERGE_INSTRUCTION}

# CURRENT CODE:
# {chunk}

# TASK:
# Extract the BUSINESS RULES. If you see an IF statement, translate it to a functional requirement. 
# (e.g., IF BALANCE < 0 translates to 'System prevents processing for overdrawn accounts').

# REQUIRED JSON STRUCTURE:
# {{
#     "filename": "{filename}",
#     "type": "COBOL",
#     "business_overview": {{
#         "title": "Business Process Name",
#         "purpose": "DETAILED 3-4 sentence business explanation.",
#         "functional_category": "Select: Transaction Processing, Reporting, Data Maintenance, or Utility",
#         "business_rules": [
#             "Rule 1: [Detailed rule extracted from IF/EVALUATE logic]",
#             "Rule 2: [Detailed rule extracted from logic]"
#         ],
#         "key_data_entities": ["Specific entities from the DATA DIVISION"]
#     }},
#     "technical_analysis": {{
#         "data_interactions": [
#             {{ "target": "Table/File", "operation": "READ/WRITE/UPDATE" }}
#         ],
#         "key_operations": ["Logical steps taken"],
#         "technical_notes": ["Error codes, specific IBM utilities used"]
#     }}
# }}
# """

CUMULATIVE_MERGE_INSTRUCTION = """
This is a chunked analysis. 
1. Review 'previous_summary' (JSON).
2. Analyze 'chunk_code'.
3. MERGE findings. 
   - Focus on BUSINESS LOGIC (Validations, Calculations, Decisions).
   - Ignore boilerplate (OPEN/CLOSE, Variable definitions).
"""

EXECUTIVE_SUMMARY_PROMPT = """{JSON_FORMAT_INSTRUCTION}

CONTEXT:
You are a Lead Business Analyst documenting a Legacy System for non-technical stakeholders.
Below are the System Metrics and descriptions of core modules.

SYSTEM METRICS:
{metrics}

CORE MODULE SUMMARIES:
{top_modules}

TASK:
Write a comprehensive Functional System Overview. 
DO NOT mention specific file names, JCL steps, or database column types.
Focus on the Business Value, User Process, and Data Lifecycle.

REQUIRED JSON STRUCTURE:
{{
    "business_purpose": "2 sentences. High-level goal (e.g., 'Manages end-of-day reconciliation...').",
    
    "executive_narrative": "3-4 detailed paragraphs describing the system's role in the organization. Explain what data comes in, what business rules are applied (in general terms), and what reports/outputs are generated. Write this like a magazine article about the software.",

    "technical_summary": "A detailed narrative (3-4 paragraphs) describing the technical architecture. Focus on how JCL orchestrates the COBOL/PLI modules, how data flows through files (VSAM/GDG) and tables (DB2), and the overall execution pattern (Batch vs. Online).",
    
    "key_business_processes": [
        {{
            "process_name": "Name (e.g., Transaction Ingestion)",
            "description": "Paragraph describing this flow from a business perspective."
        }},
        {{
            "process_name": "Name (e.g., Monthly Reporting)",
            "description": "Paragraph describing this flow."
        }}
    ],

    "functional_flow_diagram": {{
        "description": "Brief description of the flow.",
        "mermaid_code": "sequenceDiagram\\n    participant User/External\\n    participant System\\n    participant Database\\n    participant Reports\\n    User/External->>System: Sends Input Data\\n    System->>Database: Validates & Updates Records\\n    System->>Reports: Generates Audit Logs\\n    Reports-->>User/External: Delivers Final Output",
        "steps_table": [
            {{ "actor": "External Source", "action": "Submits Data", "outcome": "Processing Starts" }}
        ]
    }},

    "glossary": [
        {{ 
            "term": "Acronym", 
            "definition": "Business Definition",
            "source_clue": "Cite the comment or variable name that provided this definition (e.g. 'Found in comment in PROG01', 'Derived from DSN naming pattern')" 
        }}
    ]
}}
"""

STRUCTURE_EXTRACTION_PROMPT = """
You are a Mainframe System Architect. Your task is to analyze a raw dependency map and organize it into two structured, hierarchical JSON objects.

RAW RELATIONSHIP MAP:
{dependency_map}

TASK:
1. DATA FLOW ARCHITECTURE: Categorize components (Programs, Files, Tables) into high-level Business Domains (e.g., Payments, Customer, Accounting). Group these further into logical Functional Groups and Subgroups.
2. PROCESS FLOW SPECIFICATION: Sequence the batch execution into logical Phases (e.g., Intake, Validation, Core Processing, Reporting).

REQUIRED JSON FORMAT:
{{
    "data_flow_architecture": {{
        "domains": [
            {{
                "domain_name": "Domain Name",
                "groups": [
                    {{
                        "group_name": "Group Name",
                        "subgroups": [
                            {{
                                "subgroup_name": "Subgroup Name",
                                "components": ["List of component names"],
                                "description": "Brief functional purpose"
                            }}
                        ]
                    }}
                ]
            }}
        ]
    }},
    "process_flow_specification": {{
        "phases": [
            {{
                "phase_name": "Phase Name",
                "sequence": 1,
                "steps": [
                    {{
                        "step_id": "Step identifier",
                        "action": "Functional description of the action",
                        "technical_components": ["List of Jobs/Programs involved"],
                        "depends_on": ["Previous step IDs"]
                    }}
                ]
            }}
        ]
    }}
}}

CONSTRAINTS:
- Use naming conventions in the map to infer domains (e.g., 'PAY' -> Payments, 'CUST' -> Customer).
- Max 25 components per subgroup for readability.
- Return ONLY valid JSON.
"""

COBOL_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
{CUMULATIVE_MERGE_INSTRUCTION}

CURRENT CODE CHUNK:
{chunk}

TASK:
1.**Business Analyst View**. Read the code and explain the **Business Logic** in plain English.
Translate "IF AMT > 0" to "Ensures transaction amount is positive."
Translate "EXEC SQL UPDATE" to "Updates the customer ledger."
2. **Systems Programmer View:** Extract the low-level **Execution Flow** (PERFORM chains, Loops, SQL Cursors) for the Technical Analysis.

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "COBOL",
    
    "business_overview": {{
        "title": "Business Process Name",
        "purpose": "1 sentence goal.",
        "functional_category": "Transaction Processing / Reporting / Data Maintenance",
        
        "functional_description": "A detailed paragraph (3-5 sentences) explaining what this module does in business terms. Do NOT mention variable names or 'GO TO'. Explain the RULES.",
        
        "scope": [
            "Rule 1: (e.g. Accounts must be active)",
            "Rule 2: (e.g. Tax calculated at 5%)"
        ],
        "key_data_entities": [
            "Customer", "Account"
        ]
    }},

    "technical_analysis": {{
        "functional_capabilities": ["Technical logic points"],
        "key_operations": ["File I/O"],
        "external_calls": ["List of sub-programs called via CALL or LINK"],
        "logic_decision_points": [
            "Decision 1: (e.g. If balance < 100, trigger low-balance-warning)",
            "Decision 2: (e.g. If record-type is 'X', bypass validation)"
        ],
        "data_interactions": [
            {{ 
                "target": "Table or File Name", 
                "operation": "READ/WRITE/UPDATE",
                "access_method": "Sequential / Random / Dynamic",
                "is_gdg": true/false
            }}
        ],
        "execution_flow": [
            "1. Initialization: Open files and clear working storage.",
            "2. Data Ingestion: Read input record from SYSIN.",
            "3. Validation: Verify account status via decision point X.",
            "4. Finalization: Close files and return RC=0."
        ],
        
        "technical_notes": ["Error handling", "Performance notes"]
    }}
}}
"""

# PLI_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
# {CUMULATIVE_MERGE_INSTRUCTION}

# PREVIOUS SUMMARY JSON:
# {previous_summary}

# CURRENT CODE CHUNK:
# {chunk}

# REQUIRED JSON STRUCTURE:
# {{
#     "filename": "From metadata",
#     "type": "PLI",

#     "business_overview": {{
#         "title": "Process Name",
#         "purpose": "Business goal description.",
#         "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
#         "scope": ["Business logic scope"],
#         "key_data_entities": ["Entities processed"]
#     }},

#     "technical_analysis": {{
#         "functional_capabilities": ["Specific procedure logic"],
#         "key_operations": ["I/O and Calls"],
#         "data_interactions": [
#             {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
#         ],
#         "technical_notes": ["Memory/Pointer notes"]
#     }}
# }}
# """

# ASSEMBLY_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
# {CUMULATIVE_MERGE_INSTRUCTION}

# PREVIOUS SUMMARY JSON:
# {previous_summary}

# CURRENT CODE CHUNK:
# {chunk}

# REQUIRED JSON STRUCTURE:
# {{
#     "filename": "From metadata",
#     "type": "ASSEMBLY",

#     "business_overview": {{
#         "title": "System Module Name",
#         "purpose": "What low-level function does this perform? (e.g. 'Date conversion routine', 'I/O Driver').",
#         "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
#         "scope": [
#             "Functions provided",
#             "System interactions"
#         ],
#         "key_data_entities": [
#             "Data areas or Control blocks accessed"
#         ]
#     }},

#     "technical_analysis": {{
#         "register_usage": [
#             "R12: Base Register",
#             "R15: Return Code"
#         ],
#         "functional_capabilities": [
#             "Specific logic flow",
#             "Bit manipulation or arithmetic"
#         ],
#         "key_operations": [
#             "Macros used (GETMAIN, WTOR, LINK)",
#             "System services accessed"
#         ],
#         "data_interactions": [
#             {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
#         ],
#         "technical_notes": [
#             "Reentrancy considerations",
#             "Addressing modes (AMODE/RMODE)"
#         ]
#     }}
# }}
# """

# REXX_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
# {CUMULATIVE_MERGE_INSTRUCTION}

# PREVIOUS SUMMARY JSON:
# {previous_summary}

# CURRENT CODE CHUNK:
# {chunk}

# REQUIRED JSON STRUCTURE:
# {{
#     "filename": "From metadata",
#     "type": "REXX",

#     "business_overview": {{
#         "title": "Automation Process",
#         "purpose": "What manual task does this script automate? (e.g. 'Daily Report Bursting').",
#         "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
#         "scope": [
#             "Tasks performed",
#             "Systems interacted with (TSO, ISPF, DB2)"
#         ],
#         "key_data_entities": [
#             "Files moved/renamed",
#             "Jobs submitted"
#         ]
#     }},

#     "technical_analysis": {{
#         "automation_tasks": [
#             "Specific automation steps"
#         ],
#         "external_utilities": [
#             "TSO Commands (ALLOC, FREE)",
#             "ISPF Services (FTOPEN, FTCLOSE)"
#         ],
#         "data_interactions": [
#             {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
#         ],
#         "technical_notes": [
#             "Parsing logic",
#             "Error trapping (SIGNAL ON ERROR)"
#         ]
#     }}
# }}
# """

PLI_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
{CUMULATIVE_MERGE_INSTRUCTION}

PREVIOUS SUMMARY JSON:
{previous_summary}

CURRENT CODE CHUNK:
{chunk}

TASK:
Act as a Business Analyst. Explain the PL/I logic in plain English. Also extract control flow for the technical analysis.

REQUIRED JSON STRUCTURE:
{{
    "filename": "{filename}",
    "type": "PLI",

    "business_overview": {{
        "title": "Process Name",
        "purpose": "Business goal description.",
        "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
        
        "functional_description": "A detailed paragraph (3-5 sentences) explaining what this module does in business terms. Describe the core logic and decisions made.",
        
        "scope": ["Business logic scope"],
        "key_data_entities": ["Entities processed"]
    }},

    "technical_analysis": {{
        "functional_capabilities": ["Specific procedure logic"],
        "key_operations": ["I/O and Calls"],
        "external_calls": ["List of sub-programs called via CALL or LINK"],
        "logic_decision_points": [
            "Decision 1: (e.g. If balance < 100, trigger low-balance-warning)",
            "Decision 2: (e.g. If record-type is 'X', bypass validation)"
        ],
        "data_interactions": [
            {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
        ],
        "execution_flow": [
            "1. Initialization: Open files and clear working storage.",
            "2. Data Ingestion: Read input record from SYSIN.",
            "3. Validation: Verify account status via decision point X.",
            "4. Finalization: Close files and return RC=0."
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

TASK:
Identify the high-level business or system function this Assembly code serves. Also extract the register/macro logic for technical analysis.

REQUIRED JSON STRUCTURE:
{{
    "filename": "{filename}",
    "type": "ASSEMBLY",

    "business_overview": {{
        "title": "System Module Name",
        "purpose": "What low-level function does this perform? (e.g. 'Date conversion routine').",
        "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
        
        "functional_description": "A detailed paragraph explaining the role of this module in the system. Even though this is low-level, explain the result it produces for the business.",
        
        "scope": ["Functions provided"],
        "key_data_entities": ["Data areas or Control blocks accessed"]
    }},

    "technical_analysis": {{
        "register_usage": ["R12: Base Register", "R15: Return Code"],
        "functional_capabilities": ["Specific logic flow"],
        "key_operations": ["Macros used (GETMAIN, WTOR, LINK)"],
        "external_calls": ["List of sub-programs called via CALL or LINK"],
        "logic_decision_points": [
            "Decision 1: (e.g. If balance < 100, trigger low-balance-warning)",
            "Decision 2: (e.g. If record-type is 'X', bypass validation)"
        ],
        "data_interactions": [
            {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
        ],
        "execution_flow": [
            "1. Initialization: Open files and clear working storage.",
            "2. Data Ingestion: Read input record from SYSIN.",
            "3. Validation: Verify account status via decision point X.",
            "4. Finalization: Close files and return RC=0."
        ],
        "technical_notes": ["Addressing modes (AMODE/RMODE)"]
    }}
}}
"""

REXX_CHUNK_PROMPT = """{JSON_FORMAT_INSTRUCTION}
{CUMULATIVE_MERGE_INSTRUCTION}

PREVIOUS SUMMARY JSON:
{previous_summary}

CURRENT CODE CHUNK:
{chunk}

TASK:
Explain the automation logic and the business process it supports. Also extract the command flow for the technical analysis

REQUIRED JSON STRUCTURE:
{{
    "filename": "{filename}",
    "type": "REXX",

    "business_overview": {{
        "title": "Automation Process",
        "purpose": "What manual task does this script automate?",
        "functional_category": "Must be one of: 'Transaction Processing', 'Reporting', 'Data Maintenance', 'Interface/Transfer', or 'Utility'",
        
        "functional_description": "A detailed paragraph explaining the workflow this REXX script executes and its role in orchestration.",
        
        "scope": ["Tasks performed", "Systems interacted with"],
        "key_data_entities": ["Files moved/renamed", "Jobs submitted"]
    }},

    "technical_analysis": {{
        "automation_tasks": ["Specific automation steps"],
        "external_utilities": ["TSO Commands (ALLOC, FREE)"],
        "external_calls": ["List of sub-programs called via CALL or LINK"],
        "logic_decision_points": [
            "Decision 1: (e.g. If balance < 100, trigger low-balance-warning)",
            "Decision 2: (e.g. If record-type is 'X', bypass validation)"
        ],
        "data_interactions": [
            {{ "target": "Table or File Name", "operation": "READ/WRITE/UPDATE/DELETE" }}
        ],
        "execution_flow": [
            "1. Initialization: Open files and clear working storage.",
            "2. Data Ingestion: Read input record from SYSIN.",
            "3. Validation: Verify account status via decision point X.",
            "4. Finalization: Close files and return RC=0."
        ],
        "technical_notes": ["Error trapping (SIGNAL ON ERROR)"]
    }}
}}
"""

JCL_PROMPT = """{JSON_FORMAT_INSTRUCTION}

CONTEXT:
MASTER LIST OF JOBS IN THIS SYSTEM: {all_job_names}

TASK:
Analyze this JCL/PROC and identify the data lineage. 
1. PREDECESSORS: Which jobs produce the input files for this job? (Check comments and DISP=OLD/SHR).
2. SUCCESSORS: Which jobs consume the files created by this job? (Check DISP=NEW/MOD).

CODE:
{content}

TASK:
Perform a deep technical analysis to provide a developer with an operational roadmap of this job. 
Map every Step to its specific Programs, Input/Output files (DD Names), and Execution Logic.

REQUIRED JSON STRUCTURE:
{{
    "filename": "From metadata",
    "type": "JCL",

    "business_overview": {{
        "title": "Job Workflow Name",
        "purpose": "2-3 sentences on the business goal of this job.",
        "functional_category": "Core Batch / Reporting / Housekeeping / Ingestion / Distribution",
        "scope": ["Processing boundaries", "Data retention impact", "Run frequency"],
        "key_data_entities": ["Primary business objects modified (e.g., Master Customer File, GL Ledger)"]
    }},

    "technical_analysis": {{
        "job_header": {{
            "job_name": "Name from JOB card",
            "class": "Execution Class",
            "owner": "USER/NOTIFY ID",
            "region_limit": "Memory allocation (if specified)"
        }},
        
        "symbolic_parameters": [
            {{ "name": "VARNAME", "default_value": "VALUE", "description": "Purpose of this JCL variable" }}
        ],

        "steps": [
            {{"step_name": "STEP01", "program": "PGMNAME", "description": "Technical description"}}
        ],
        "io_datasets": [
            {{"dataset": "A.B.C", "usage": "Input/Output"}}
        ],
        "flow_context": {{
            "predecessors": ["List job names from the MASTER LIST that provide input to this job"],
            "successors": ["List job names from the MASTER LIST that will read outputs from this job"],
            "external_inputs": ["List descriptions of files that come from outside this system (e.g. 'Monthly Master from HR')"],
            "external_outputs": ["List descriptions of files sent to external systems"]
        }},
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
    "filename": "{filename}",
    "type": "COPYBOOK",

    "business_overview": {{
        "title": "Data Entity Definition",
        "purpose": "What business entity does this represent?",
        "data_domain": "e.g., Master Data / Transactional / Reference / Metadata",
        "key_data_entities": ["The primary entity defined"]
    }},

    "technical_analysis": {{
        "storage_type": "DB2 / VSAM-KSDS / VSAM-ESDS / FLAT",
        "record_length": "Calculated LRECL (if possible)",
        "key_fields": [
            {{
                "field": "Field Name", 
                "is_primary_key": true/false,
                "description": "Business meaning"
            }}
        ],
        "table_structure": [
             {{"column_name": "Name", "type": "DataType", "nullable": "Yes/No"}}
        ],
        "technical_notes": ["Usage of COMP-3, REDEFINES, or binary formats"]
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
4. GLOSSARY DISCOVERY: If you encounter an acronym (e.g., CATO, DIN, MST) that isn't defined, 
   search the project for the acronym followed by a hyphen or 'IS' 
   (e.g., 'grep_search "CATO -"' or 'grep_search "DIN IS"'). 
   This is critical for accurate business terminology.

STRICT EXIT CRITERIA:
- For COPYBOOKS/DCLGEN: Once you have the record structure/columns, STOP.
- For JCL/REXX: Once you have the steps/flow, STOP.
- For COBOL/PLI: Once you have the 'PROCEDURE DIVISION' or 'MAIN' logic, STOP.

If a tool returns 'No matches found', do not keep retrying. Move to the writing phase.
DO NOT loop more than 3-5 times.

When you have gathered sufficient information, respond with a summary of what you found WITHOUT calling any more tools.
"""