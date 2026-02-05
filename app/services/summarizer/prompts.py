"""Prompt templates for file summarization."""

COBOL_CHUNK_PROMPT = """Summarize this COBOL code.

Code:
{chunk}

Previous context:
{previous_summary}

Respond EXACTLY in this format:

Purpose: [1-2 sentences description of what this code does. Start directly with the verb, e.g., "Processes...", "Manages...". DO NOT start with "This is..." or "This code..."]

Functionalities:
- [specific business capability 1]
- [specific business capability 2]
...

Key Operations:
- [operation 1 (e.g. Reads FILE-A)]
- [operation 2 (e.g. Calls PROG-B)]

Notes:
- [special internal logic, algorithms, or technical details]
"""

COPYBOOK_PROMPT = """Summarize this COBOL copybook.

Copybook:
{content}

Respond EXACTLY in this format:

Purpose: [what entity/data this represents. Start directly with the noun or verb. DO NOT start with "This copybook..."]

Entity: [entity name]

Key Fields:
- [field1] - [description]
- [field2] - [description]
...
"""


PLI_CHUNK_PROMPT = """Summarize this PL/I code.

Code:
{chunk}

Previous context:
{previous_summary}

Respond EXACTLY in this format:

Purpose: [1-2 sentences description of what this code does. Start directly with the verb, e.g., "Processes...", "Calculates...". DO NOT start with "This is..." or "This code..."]

Functionalities:
- [specific business capability 1]
- [specific business capability 2]
...

Key Operations:
- [operation 1 (e.g. Reads FILE-A, Includes PLITAXRATES)]
- [operation 2 (e.g. Calls PROC-B)]

Notes:
- [special internal logic, algorithms, or technical details]
"""

PLI_COPYBOOK_PROMPT = """Summarize this PL/I include/copybook.

Copybook:
{content}

Respond EXACTLY in this format:

Purpose: [what entity/data this represents. Start directly with the noun or verb. DO NOT start with "This copybook..."]

Entity: [entity name]

Key Fields:
- [field1] - [description]
- [field2] - [description]
...
"""

ASSEMBLY_CHUNK_PROMPT = """Summarize this Mainframe Assembly (HLASM) code.

Code:
{chunk}

Previous context:
{previous_summary}

Respond EXACTLY in this format:

Purpose: [1-2 sentences description of what this module/block does. Start with a verb, e.g., "Masks...", "Calculates..."]

Register Usage:
- [Reg X] - [Usage (e.g. Base Register, Pointer to Input, Counter)]

Functionalities:
- [specific business capability 1]
- [specific business capability 2]

Key Logic/Macros:
- [Instruction/Macro 1] - [Purpose]

Notes:
- [Side effects, memory modifications, or complex branching logic]
"""

JCL_PROMPT = """Summarize this JCL code.

Code:
{content}

Respond EXACTLY in this format:

Purpose: [1-2 sentences description of what this job or procedure does. Start directly with the verb, e.g., "Executes...", "Performs...". DO NOT start with "This JCL..."]

Workflow Steps:
- [Step name or Program name] - [brief description of step purpose]
- [Step name or Program name] - [brief description of step purpose]
...

Main Datasets:
- [Dataset name] - [Input, Output, or Temp]
- [Dataset name] - [Input, Output, or Temp]

Notes:
- [Operational details, frequency, dependencies on other jobs, or special restart instructions]
"""

DCLGEN_PROMPT = """Summarize this DB2 DCLGEN (Table Declaration).

DCLGEN Content:
{content}

Respond EXACTLY in this format:

Purpose: [What business entity this table represents.]

Table Name: [The actual DB2 table name]

Host Variable Structure: [The name of the 01 level COBOL record]

Table Structure:
- [Column Name] | [DB2 Type] | [Nullable?]
- [Column Name] | [DB2 Type] | [Nullable?]
...

Notes:
- [Specific constraints or usage context]
"""

CA7_PROMPT = """Summarize this CA-7 Workload Job Definition.
 
Code:
{chunk}
 
Previous context:
{previous_summary}
 
Respond EXACTLY in this format:
 
Purpose: [1-2 sentences description of this job's role in the batch window. Start directly with the verb, e.g., "Orchestrates...", "Schedules...". DO NOT start with "This job..." or "This CA-7..."]
 
Workload Identity:
- System: [System Name]
- Schedule ID: [SCHID]
- Owner: [Owner ID]
- Class: [Execution Class]
 
Dependencies & Triggers:
- [Predecessor Job/DSN Name] - [Type: Predecessor, Dataset Trigger, or User Requirement]
- [Predecessor Job/DSN Name] - [Type: Predecessor, Dataset Trigger, or User Requirement]
...
 
Operational Rules:
- [Rule 1 (e.g., Restartable: Yes/No)]
- [Rule 2 (e.g., Triggers Successors: Yes/No)]
- [Rule 3 (e.g., Load Step: Yes/No)]
 
Notes:
- [Special scheduling logic, frequency, manual sign-off details, or timing constraints]
"""
 
REXX_CHUNK_PROMPT = """Summarize this REXX code.

Code:
{chunk}

Previous context:
{previous_summary}

Respond EXACTLY in this format:

Purpose: [1-2 sentences description of this job's role in the batch window. Start directly with the verb, e.g., "Orchestrates...", "Schedules...". DO NOT start with "This job..." or "This CA-7..."]

Workload Identity:
- System: [System Name]
- Schedule ID: [SCHID]
- Owner: [Owner ID]
- Class: [Execution Class]

Dependencies & Triggers:
- [Predecessor Job/DSN Name] - [Type: Predecessor, Dataset Trigger, or User Requirement]
- [Predecessor Job/DSN Name] - [Type: Predecessor, Dataset Trigger, or User Requirement]
...

Operational Rules:
- [Rule 1 (e.g., Restartable: Yes/No)]
- [Rule 2 (e.g., Triggers Successors: Yes/No)]
- [Rule 3 (e.g., Load Step: Yes/No)]

Notes:
- [Special scheduling logic, frequency, manual sign-off details, or timing constraints]
Purpose: [1-2 sentences description. Start with verb, e.g., "Automates...", "Processes..."]

Functionalities:
- [specific capability 1]
- [specific capability 2]

Key Operations:
- [operation 1 (e.g. Reads dataset X)]
- [operation 2 (e.g. Calls utility Y)]

Notes:
- [special logic, error handling, or technical details]
"""

PARMLIB_CHUNK_PROMPT = """Summarize this mainframe parmlib (parameter library) configuration.

Code:
{chunk}

Previous context:
{previous_summary}

Respond EXACTLY in this format:

Purpose: [1-2 sentences description of what system/subsystem this configures. Start with verb, e.g., "Configures...", "Defines...". DO NOT start with "This parmlib..."]

Configuration Areas:
- [area 1 (e.g., System initialization parameters)]
- [area 2 (e.g., Dataset allocation defaults)]
...

Key Parameters:
- [parameter_name] - [what it controls/specifies]
- [parameter_name] - [what it controls/specifies]
...

Notes:
- [System dependencies, IPL requirements, migration considerations, or critical settings]
"""
