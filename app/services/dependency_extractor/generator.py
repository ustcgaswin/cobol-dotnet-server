"""Markdown generator for dependency graph."""

from datetime import datetime, timezone
from typing import Any


def generate_dependency_graph_md(
    project_id: str,
    cobol_deps: dict | None = None,
    copybook_deps: dict | None = None,
    jcl_deps: dict | None = None,
    assembly_deps: dict | None = None,
    ca7_deps: dict | None = None,
    missing_file_types: list[str] | None = None,
) -> str:
    """Generate dependency_graph.md content.
    
    Args:
        project_id: Project UUID string
        cobol_deps: Dependencies extracted from COBOL files (optional)
        copybook_deps: Dependencies extracted from copybook files (optional)
        jcl_deps: Dependencies extracted from JCL files (optional)
        missing_file_types: File types that weren't parsed (optional)
        
    Returns:
        Markdown content for dependency_graph.md
    """
    lines = []
    
    # Initialize defaults
    cobol_deps = cobol_deps or {}
    copybook_deps = copybook_deps or {}
    jcl_deps = jcl_deps or {}
    assembly_deps = assembly_deps or {}
    ca7_deps = ca7_deps or {}
    missing_file_types = missing_types = missing_file_types or []
    
    # Header
    lines.append("# Dependency Graph")
    lines.append("")
    lines.append(f"> Generated: {datetime.now(timezone.utc).isoformat()}")
    lines.append(f"> Project: {project_id}")
    lines.append("")
    

    program_calls = cobol_deps.get('program_calls', [])
    unresolved_calls = cobol_deps.get('unresolved_calls', [])
    copybooks = cobol_deps.get('copybooks', [])
    sql_tables = cobol_deps.get('sql_tables', [])
    file_definitions = cobol_deps.get('file_definitions', [])
    file_io = cobol_deps.get('file_io', [])
    copybook_to_copybook = copybook_deps.get('copybook_to_copybook', [])
    jcl_pgm = jcl_deps.get('jcl_program_calls', [])
    jcl_proc = jcl_deps.get('jcl_proc_calls', [])
    jcl_inc = jcl_deps.get('jcl_includes', [])
    jcl_files = jcl_deps.get('jcl_files', [])
    

    asm_calls = assembly_deps.get('program_calls', [])
    asm_copy = assembly_deps.get('copybooks', [])
    asm_db2 = assembly_deps.get('db2_usage', [])
    asm_io = assembly_deps.get('file_io', [])
    asm_ext = assembly_deps.get('externals', [])

    # CA-7 Sub-Dependencies
    ca7_flow = ca7_deps.get('ca7_job_flow', [])
    ca7_dsn = ca7_deps.get('ca7_dataset_triggers', [])
    ca7_usr = ca7_deps.get('ca7_user_requirements', [])



    lines.append("## Summary")
    lines.append("")
    lines.append("| Relationship Type | Count |")
    lines.append("|-------------------|-------|")
    lines.append(f"| Program → Program (CALL) | {len(program_calls)} |")
    lines.append(f"| Program → Copybook | {len(copybooks)} |")
    lines.append(f"| Program → Table (SQL) | {len(sql_tables)} |")
    lines.append(f"| Program → File (Definition) | {len(file_definitions)} |")
    lines.append(f"| Program → File (I/O) | {len(file_io)} |")
    lines.append(f"| Copybook → Copybook | {len(copybook_to_copybook)} |")
    lines.append(f"| Unresolved Dynamic CALLs | {len(unresolved_calls)} |")
    lines.append(f"| JCL → Program | {len(jcl_pgm)} |")
    lines.append(f"| JCL → Procedure | {len(jcl_proc)} |")
    lines.append(f"| JCL → Include Group | {len(jcl_inc)} |")
    lines.append(f"| JCL → Dataset (DSN) | {len(jcl_files)} |")
    lines.append(f"| Assembly → Program (CALL) | {len(asm_calls)} |")
    lines.append(f"| Assembly → Copybook | {len(asm_copy)} |")
    lines.append(f"| Assembly → File (I/O) | {len(asm_io)} |")
    lines.append(f"| Assembly → DB2 (DSNHLI) | {len(asm_db2)} |")
    lines.append(f"| CA-7 → Job Predecessor | {len(ca7_flow)} |")
    lines.append(f"| CA-7 → Dataset Trigger | {len(ca7_dsn)} |")
    lines.append("")
    
    # Program → Program (CALL)
    lines.append("---")
    lines.append("")
    lines.append("## Program → Program (CALL)")
    lines.append("")
    if program_calls:
        lines.append("| Source | Target | Type | Line |")
        lines.append("|--------|--------|------|------|")
        for call in sorted(program_calls, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            call_type = call.get('call_type', 'static')
            if call_type == 'dynamic' and call.get('resolved'):
                call_type = 'dynamic (resolved)'
            target = call.get('target') or '(unknown)'
            lines.append(f"| {call.get('source', '')} | {target} | {call_type} | {call.get('line', '')} |")
    else:
        lines.append("*No program-to-program calls found.*")
    lines.append("")
    
    # Program → Copybook
    lines.append("---")
    lines.append("")
    lines.append("## Program → Copybook")
    lines.append("")
    if copybooks:
        lines.append("| Program | Copybook | Line |")
        lines.append("|---------|----------|------|")
        for copy in sorted(copybooks, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {copy.get('source', '')} | {copy.get('copybook', '')} | {copy.get('line', '')} |")
    else:
        lines.append("*No copybook references found.*")
    lines.append("")
    
    # Program → Table (SQL)
    lines.append("---")
    lines.append("")
    lines.append("## Program → Table (SQL)")
    lines.append("")
    if sql_tables:
        lines.append("| Program | Table | Operations |")
        lines.append("|---------|-------|------------|")
        for table in sorted(sql_tables, key=lambda x: (x.get('source', ''), x.get('table', ''))):
            ops = ', '.join(table.get('operations', [])) or '-'
            lines.append(f"| {table.get('source', '')} | {table.get('table', '')} | {ops} |")
    else:
        lines.append("*No SQL table references found.*")
    lines.append("")
    
    # Program → File (Definition)
    lines.append("---")
    lines.append("")
    lines.append("## Program → File (Definition)")
    lines.append("")
    if file_definitions:
        lines.append("| Program | Logical Name | DD Name | Organization | Access |")
        lines.append("|---------|--------------|---------|--------------|--------|")
        for fd in sorted(file_definitions, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            org = fd.get('organization') or '-'
            access = fd.get('access_mode') or '-'
            lines.append(f"| {fd.get('source', '')} | {fd.get('logical_name', '')} | {fd.get('dd_name', '')} | {org} | {access} |")
    else:
        lines.append("*No file definitions found.*")
    lines.append("")
    
    # Program → File (I/O)
    lines.append("---")
    lines.append("")
    lines.append("## Program → File (I/O)")
    lines.append("")
    if file_io:
        lines.append("| Program | File | Operation | Mode | Line |")
        lines.append("|---------|------|-----------|------|------|")
        for io in sorted(file_io, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            mode = io.get('mode') or '-'
            lines.append(f"| {io.get('source', '')} | {io.get('file', '')} | {io.get('operation', '')} | {mode} | {io.get('line', '')} |")
    else:
        lines.append("*No file I/O operations found.*")
    lines.append("")
    
    # Copybook → Copybook
    lines.append("---")
    lines.append("")
    lines.append("## Copybook → Copybook")
    lines.append("")
    if copybook_to_copybook:
        lines.append("| Copybook | Includes | Line |")
        lines.append("|----------|----------|------|")
        for ref in sorted(copybook_to_copybook, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {ref.get('source', '')} | {ref.get('target', '')} | {ref.get('line', '')} |")
    else:
        lines.append("*No nested copybook references found.*")
    lines.append("")

    #JCL Dependencies
    lines.append("---")
    lines.append("")
    lines.append("## JCL Dependencies")
    lines.append("")
    
    lines.append("### JCL → Execution Calls")
    if jcl_pgm or jcl_proc:
        lines.append("| Source | Target | Type |")
        lines.append("|--------|--------|------|")
        for call in sorted(jcl_pgm + jcl_proc, key=lambda x: x.get('source', '')):
            ctype = "PROGRAM" if call in jcl_pgm else "PROCEDURE"
            lines.append(f"| {call.get('source')} | {call.get('target')} | {ctype} |")
    else:
        lines.append("*No EXEC calls found.*")
    lines.append("")

    lines.append("### JCL → Datasets (DSN)")
    if jcl_files:
        lines.append("| JCL/Job | Step | DD Name | DSN | Mode |")
        lines.append("|---------|------|---------|-----|------|")
        for f in sorted(jcl_files, key=lambda x: (x.get('source'), x.get('step'))):
            lines.append(f"| {f.get('source')} | {f.get('step')} | {f.get('dd_name')} | {f.get('dsn')} | {f.get('mode')} |")
    else:
        lines.append("*No dataset references found.*")
    lines.append("")

    lines.append("### JCL → Includes")
    if jcl_inc:
        lines.append("| JCL/Job | Include Member |")
        lines.append("|---------|----------------|")
        for inc in sorted(jcl_inc, key=lambda x: x.get('source')):
            lines.append(f"| {inc.get('source')} | {inc.get('target')} |")
    else:
        lines.append("*No INCLUDE statements found.*")
    lines.append("")


     # --- ASSEMBLY SECTION ---
    lines.append("---")
    lines.append("## Assembly Dependencies")
    lines.append("")

    if asm_copy:
        lines.append("### Assembly → Copybook References")
        lines.append("")
        lines.append("| Assembly Module | Copybook Name | Line |")
        lines.append("| :--- | :--- | :--- |")
        for copy in sorted(asm_copy, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {copy['source']} | {copy['copybook']} | {copy.get('line', '')} |")
        lines.append("")

    if asm_calls:
        lines.append("### Assembly → External Calls")
        lines.append("")
        lines.append("| Source (ASM) | Target | Type | Line |")
        lines.append("| :--- | :--- | :--- | :--- |")
        for call in sorted(asm_calls, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {call['source']} | {call['target']} | {call['call_type']} | {call['line']} |")
        lines.append("")

    if asm_io:
        lines.append("### Assembly → File I/O")
        lines.append("")
        lines.append("| Source (ASM) | DD Name | Operation | Line |")
        lines.append("| :--- | :--- | :--- | :--- |")
        for io in sorted(asm_io, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {io['source']} | {io['file']} | {io['operation']} | {io['line']} |")
        lines.append("")

    if asm_db2:
        lines.append("### Assembly → DB2 Usage")
        lines.append("")
        lines.append("| Source (ASM) | Access Type | Line |")
        lines.append("| :--- | :--- | :--- |")
        for db2 in sorted(asm_db2, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {db2['source']} | {db2['type']} | {db2['line']} |")
        lines.append("")
    

    # --- CA-7 WORKLOAD SECTION ---
    lines.append("---")
    lines.append("## CA-7 Dependencies")
    lines.append("")

    lines.append("### CA-7 → Job Dependency Flow")
    if ca7_flow:
        lines.append("| Predecessor (Trigger) | Successor (Target) | Type |")
        lines.append("|-----------------------|--------------------|------|")
        for flow in sorted(ca7_flow, key=lambda x: x.get('target', '')):
            lines.append(f"| {flow.get('source')} | {flow.get('target')} | {flow.get('type')} |")
    else:
        lines.append("*No CA-7 job-to-job dependencies found.*")
    lines.append("")

    lines.append("### CA-7 → Dataset (DSN) Triggers")
    lines.append("Jobs that automatically start when a specific file is created or updated.")
    if ca7_dsn:
        lines.append("| Triggering Dataset | Targeted Job | Type |")
        lines.append("|--------------------|--------------|------|")
        for dsn in sorted(ca7_dsn, key=lambda x: x.get('source', '')):
            lines.append(f"| {dsn.get('source')} | {dsn.get('target')} | {dsn.get('type')} |")
    else:
        lines.append("*No CA-7 dataset triggers found.*")
    lines.append("")

    lines.append("### CA-7 → User Requirements")
    lines.append("Manual intervention or external sign-offs required for job execution.")
    if ca7_usr:
        lines.append("| Requirement Name | Targeted Job | Type |")
        lines.append("|------------------|--------------|------|")
        for usr in sorted(ca7_usr, key=lambda x: x.get('source', '')):
            lines.append(f"| {usr.get('source')} | {usr.get('target')} | {usr.get('type')} |")
    else:
        lines.append("*No manual user requirements found.*")
    lines.append("")
 
    # Gaps section
    lines.append("---")
    lines.append("")
    lines.append("## Gaps")
    lines.append("")
    
    # Unresolved Dynamic CALLs
    lines.append("### Unresolved Dynamic CALLs")
    lines.append("")
    if unresolved_calls:
        lines.append("| Program | Variable | Line |")
        lines.append("|---------|----------|------|")
        for call in sorted(unresolved_calls, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {call.get('source', '')} | {call.get('variable', '')} | {call.get('line', '')} |")
    else:
        lines.append("*All dynamic CALLs resolved.*")
    lines.append("")
    
    # Missing File Types
    lines.append("### Missing File Types")
    lines.append("")
    if missing_file_types:
        lines.append("The following file types have not been parsed:")
        lines.append("")
        for ft in missing_file_types:
            lines.append(f"- **{ft}** (no {ft} dependencies available)")
    else:
        lines.append("*All expected file types have been parsed.*")
    lines.append("")
    
    return '\n'.join(lines)
