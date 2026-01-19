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
