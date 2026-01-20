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
    pli_deps: dict | None = None,           # <--- Added
    pli_copybook_deps: dict | None = None,  # <--- Added
    rexx_deps: dict | None = None,
    parmlib_deps: dict | None = None, 
    missing_file_types: list[str] | None = None,
) -> str:
    """Generate dependency_graph.md content.
    
    Args:
        project_id: Project UUID string
        cobol_deps: Dependencies extracted from COBOL files
        copybook_deps: Dependencies extracted from COBOL copybooks
        jcl_deps: Dependencies extracted from JCL files
        assembly_deps: Dependencies extracted from Assembly files
        pli_deps: Dependencies extracted from PL/I files
        pli_copybook_deps: Dependencies extracted from PL/I copybooks
        parmlib_deps: Dependencies extracted from PARMLIB members
        missing_file_types: File types that weren't parsed
        
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
    pli_deps = pli_deps or {}
    pli_copybook_deps = pli_copybook_deps or {}
    rexx_deps = rexx_deps or {}
    parmlib_deps = parmlib_deps or {}
    missing_file_types = missing_types = missing_file_types or []
    
    # Header
    lines.append("# Dependency Graph")
    lines.append("")
    lines.append(f"> Generated: {datetime.now(timezone.utc).isoformat()}")
    lines.append(f"> Project: {project_id}")
    lines.append("")
    
    # --- Unpack COBOL Data ---
    cbl_calls = cobol_deps.get('program_calls', [])
    cbl_unresolved = cobol_deps.get('unresolved_calls', [])
    cbl_copy = cobol_deps.get('copybooks', [])
    cbl_sql = cobol_deps.get('sql_tables', [])
    cbl_fds = cobol_deps.get('file_definitions', [])
    cbl_io = cobol_deps.get('file_io', [])
    cbl_cb_refs = copybook_deps.get('copybook_to_copybook', [])

    # --- Unpack JCL Data ---
    jcl_pgm = jcl_deps.get('jcl_program_calls', [])
    jcl_proc = jcl_deps.get('jcl_proc_calls', [])
    jcl_inc = jcl_deps.get('jcl_includes', [])
    jcl_files = jcl_deps.get('jcl_files', [])
    
    # --- Unpack Assembly Data ---
    asm_calls = assembly_deps.get('program_calls', [])
    asm_copy = assembly_deps.get('copybooks', [])
    asm_db2 = assembly_deps.get('db2_usage', [])
    asm_io = assembly_deps.get('file_io', [])
    # asm_ext = assembly_deps.get('externals', []) # Optional to display

    # CA-7 Sub-Dependencies
    ca7_flow = ca7_deps.get('ca7_job_flow', [])
    ca7_dsn = ca7_deps.get('ca7_dataset_triggers', [])
    ca7_usr = ca7_deps.get('ca7_user_requirements', [])

    # --- Unpack PL/I Data ---
    pli_calls = pli_deps.get('program_calls', [])
    pli_unresolved = pli_deps.get('unresolved_calls', [])
    pli_copy = pli_deps.get('copybooks', [])
    pli_sql = pli_deps.get('sql_tables', [])
    pli_fds = pli_deps.get('file_definitions', [])
    pli_io = pli_deps.get('file_io', [])
    pli_cb_refs = pli_copybook_deps.get('copybook_to_copybook', [])

    # --- Aggregate for Gaps ---
    all_unresolved = cbl_unresolved + pli_unresolved
    
    # --- Unpack REXX Data ---
    rexx_cobol = rexx_deps.get('cobol_calls', [])
    rexx_jcl = rexx_deps.get('jcl_submissions', [])
    rexx_datasets = rexx_deps.get('dataset_operations', [])
    rexx_utils = rexx_deps.get('tso_utilities', [])
    rexx_env = rexx_deps.get('environment_vars', [])

     # --- Unpack PARMLIB Data ---
    parmlib_progs = parmlib_deps.get('program_references', [])
    parmlib_datasets = parmlib_deps.get('dataset_allocations', [])
    parmlib_jcl = parmlib_deps.get('jcl_references', [])
    parmlib_sysparms = parmlib_deps.get('system_parameters', [])
    parmlib_symbols = parmlib_deps.get('symbolic_substitutions', [])

    # --- SUMMARY SECTION ---
    lines.append("## Summary")
    lines.append("")
    lines.append("| Relationship Type | Count |")
    lines.append("|-------------------|-------|")
    # COBOL
    lines.append(f"| COBOL → Program (CALL) | {len(cbl_calls)} |")
    lines.append(f"| COBOL → Copybook | {len(cbl_copy)} |")
    lines.append(f"| COBOL → Table (SQL) | {len(cbl_sql)} |")
    lines.append(f"| COBOL → File (Definition) | {len(cbl_fds)} |")
    lines.append(f"| COBOL Copybook → Copybook | {len(cbl_cb_refs)} |")
    # PL/I
    lines.append(f"| PL/I → Program (CALL) | {len(pli_calls)} |")
    lines.append(f"| PL/I → Include | {len(pli_copy)} |")
    lines.append(f"| PL/I → Table (SQL) | {len(pli_sql)} |")
    lines.append(f"| PL/I → File (Definition) | {len(pli_fds)} |")
    lines.append(f"| PL/I Copybook → Copybook | {len(pli_cb_refs)} |")
    # JCL
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
    # rexx
    lines.append(f"| REXX → COBOL Programs | {len(rexx_cobol)} |")
    lines.append(f"| REXX → JCL Jobs | {len(rexx_jcl)} |")
    lines.append(f"| REXX → Datasets | {len(rexx_datasets)} |")
    lines.append(f"| REXX → TSO Utilities | {len(rexx_utils)} |")
    lines.append(f"| REXX → Environment Variables | {len(rexx_env)} |")
    lines.append("")
    # parmlib
    lines.append(f"| PARMLIB → Programs | {len(parmlib_progs)} |")
    lines.append(f"| PARMLIB → Datasets | {len(parmlib_datasets)} |")
    lines.append(f"| PARMLIB → JCL Jobs | {len(parmlib_jcl)} |")
    lines.append(f"| PARMLIB → System Parameters | {len(parmlib_sysparms)} |")
    lines.append(f"| PARMLIB → Symbolic Substitutions | {len(parmlib_symbols)} |")
    # =========================================================
    # COBOL SECTION
    # =========================================================
    lines.append("---")
    lines.append("## COBOL Dependencies")
    lines.append("")
    lines.append("## Program → Program (CALL)")
    if cbl_calls:
        lines.append("| Source | Target | Type | Line |")
        lines.append("|--------|--------|------|------|")
        for call in sorted(cbl_calls, key=lambda x: (x.get('source', ''), x.get('line', 0))):
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
    if cbl_copy:
        lines.append("| Program | Copybook | Line |")
        lines.append("|---------|----------|------|")
        for copy in sorted(cbl_copy, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {copy.get('source', '')} | {copy.get('copybook', '')} | {copy.get('line', '')} |")
    else:
        lines.append("*No copybook references found.*")
    lines.append("")
    
    # Program → Table (SQL)
    lines.append("---")
    lines.append("")
    lines.append("## Program → Table (SQL)")
    lines.append("")
    if cbl_sql:
        lines.append("| Program | Table | Operations |")
        lines.append("|---------|-------|------------|")
        for table in sorted(cbl_sql, key=lambda x: (x.get('source', ''), x.get('table', ''))):
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
    if cbl_fds:
        lines.append("| Program | Logical Name | DD Name | Organization | Access |")
        lines.append("|---------|--------------|---------|--------------|--------|")
        for fd in sorted(cbl_fds, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            org = fd.get('organization') or '-'
            access = fd.get('access_mode') or '-'
            lines.append(f"| {fd.get('source', '')} | {fd.get('logical_name', '')} | {fd.get('dd_name', '')} | {org} | {access} |")
    else:
        lines.append("*No file definitions found.*")
    lines.append("")
    
    lines.append("### Copybook → Copybook")
    if cbl_cb_refs:
        lines.append("| Copybook | Includes | Line |")
        lines.append("|----------|----------|------|")
        for ref in sorted(cbl_cb_refs, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {ref.get('source', '')} | {ref.get('target', '')} | {ref.get('line', '')} |")
    else:
        lines.append("*No nested copybook references found.*")
    lines.append("")

    # =========================================================
    # PL/I SECTION
    # =========================================================
    lines.append("---")
    lines.append("## PL/I Dependencies")
    lines.append("")

    lines.append("### PL/I Program → Calls")
    if pli_calls:
        lines.append("| Source | Target | Type | Line |")
        lines.append("|--------|--------|------|------|")
        for call in sorted(pli_calls, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            ctype = call.get('call_type', 'static')
            lines.append(f"| {call['source']} | {call['target']} | {ctype} | {call['line']} |")
    else:
        lines.append("*No PL/I calls found.*")
    lines.append("")

    lines.append("### PL/I Program → Includes")
    if pli_copy:
        lines.append("| Program | Include | Type | Line |")
        lines.append("|---------|---------|------|------|")
        for inc in sorted(pli_copy, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {inc['source']} | {inc['copybook']} | {inc.get('type', '-')} | {inc['line']} |")
    else:
        lines.append("*No PL/I includes found.*")
    lines.append("")

    lines.append("### PL/I Program → SQL Tables")
    if pli_sql:
        lines.append("| Program | Table | Access |")
        lines.append("|---------|-------|--------|")
        for tbl in sorted(pli_sql, key=lambda x: (x.get('source', ''), x.get('table', ''))):
            ops = ", ".join(tbl.get('operations', []))
            lines.append(f"| {tbl['source']} | {tbl['table']} | {ops} |")
    else:
        lines.append("*No PL/I SQL usage found.*")
    lines.append("")

    lines.append("### PL/I Program → File Descriptors")
    if pli_fds:
        lines.append("| Program | Internal Name | DD Name | Line |")
        lines.append("|---------|---------------|---------|------|")
        for fd in sorted(pli_fds, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {fd['source']} | {fd['logical_name']} | {fd['dd_name']} | {fd['line']} |")
    else:
        lines.append("*No PL/I file descriptors found.*")
    lines.append("")

    lines.append("### PL/I Copybook → Includes")
    if pli_cb_refs:
        lines.append("| Copybook | Includes | Line |")
        lines.append("|----------|----------|------|")
        for ref in sorted(pli_cb_refs, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {ref.get('source', '')} | {ref.get('target', '')} | {ref.get('line', '')} |")
    else:
        lines.append("*No nested PL/I includes found.*")
    lines.append("")

    # =========================================================
    # JCL SECTION
    # =========================================================
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


    # =========================================================
    # ASSEMBLY SECTION
    # =========================================================
    lines.append("---")
    lines.append("## Assembly Dependencies")
    lines.append("")

    lines.append("### Assembly → Copybook References")
    if asm_copy:
        lines.append("| Assembly Module | Copybook Name | Line |")
        lines.append("| :--- | :--- | :--- |")
        for copy in sorted(asm_copy, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {copy['source']} | {copy['copybook']} | {copy.get('line', '')} |")
    else:
        lines.append("*No Assembly copybooks found.*")
    lines.append("")

    lines.append("### Assembly → External Calls")
    if asm_calls:
        lines.append("| Source (ASM) | Target | Type | Line |")
        lines.append("| :--- | :--- | :--- | :--- |")
        for call in sorted(asm_calls, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {call['source']} | {call['target']} | {call['call_type']} | {call['line']} |")
    else:
        lines.append("*No Assembly calls found.*")
    lines.append("")

    lines.append("### Assembly → File I/O")
    if asm_io:
        lines.append("| Source (ASM) | DD Name | Operation | Line |")
        lines.append("| :--- | :--- | :--- | :--- |")
        for io in sorted(asm_io, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {io['source']} | {io['file']} | {io['operation']} | {io['line']} |")
    else:
        lines.append("*No Assembly I/O found.*")
    lines.append("")

    lines.append("### Assembly → DB2 Usage")
    if asm_db2:
        lines.append("| Source (ASM) | Access Type | Line |")
        lines.append("| :--- | :--- | :--- |")
        for db2 in sorted(asm_db2, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(f"| {db2['source']} | {db2['type']} | {db2['line']} |")
        lines.append("")
    else:
        lines.append("*No Assembly DB2 usage found.*")
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
    

      # =========================================================
    # REXX SECTION
    # =========================================================
    lines.append("---")
    lines.append("## REXX Dependencies")
    lines.append("")
    
    # 1. REXX → COBOL Programs
    lines.append("### REXX → COBOL Programs")
    if rexx_cobol:
        lines.append("| Source REXX Exec | COBOL Program | Type |")
        lines.append("|------------------|---------------|------|")
        for call in sorted(rexx_cobol, key=lambda x: x.get('source', '')):
            lines.append(
                f"| {call['source']} | {call['target']} | {call.get('type', 'CALL')} |"
            )
    else:
        lines.append("*No COBOL program calls found.*")
    lines.append("")
    
    # 2. REXX → JCL Job Submissions
    lines.append("### REXX → JCL Job Submissions")
    if rexx_jcl:
        lines.append("| Source REXX Exec | JCL Job | Operation |")
        lines.append("|------------------|---------|-----------|")
        for jcl in sorted(rexx_jcl, key=lambda x: x.get('source', '')):
            lines.append(
                f"| {jcl['source']} | {jcl.get('job', '')} | {jcl.get('operation', 'SUBMIT')} |"
            )
    else:
        lines.append("*No JCL job submissions found.*")
    lines.append("")
    
    # 3. REXX → Dataset Operations
    lines.append("### REXX → Dataset Operations")
    if rexx_datasets:
        lines.append("| Source REXX Exec | Dataset | Operation |")
        lines.append("|------------------|---------|-----------|")
        for ds in sorted(rexx_datasets, key=lambda x: x.get('source', '')):
            lines.append(
                f"| {ds['source']} | {ds.get('dataset', '')} | {ds.get('operation', 'REFERENCE')} |"
            )
    else:
        lines.append("*No dataset operations found.*")
    lines.append("")
    
    # 4. REXX → TSO Utilities
    lines.append("### REXX → TSO Utilities")
    if rexx_utils:
        lines.append("| Source REXX Exec | Utility |")
        lines.append("|------------------|---------|")
        for util in sorted(rexx_utils, key=lambda x: x.get('source', '')):
            lines.append(
                f"| {util['source']} | {util.get('utility', '')} |"
            )
    else:
        lines.append("*No TSO utilities found.*")
    lines.append("")
    
    # 5. REXX → Environment Variables
    lines.append("### REXX → Environment Variables")
    if rexx_env:
        lines.append("| Source REXX Exec | Variable |")
        lines.append("|------------------|----------|")
        for env in sorted(rexx_env, key=lambda x: x.get('source', '')):
            lines.append(
                f"| {env['source']} | {env.get('variable', '')} |"
            )
    else:
        lines.append("*No environment variables found.*")
    lines.append("")


    #==============================================================
    # 4. PARMLIB SECTION 
    # ==============================================================
    lines.append("---")
    lines.append("## PARMLIB Dependencies")
    lines.append("")
    lines.append("PARMLIB members contain system configuration and control parameters that affect job execution, resource allocation, and system behavior.")
    lines.append("")
    
    # 4.1 Program References
    lines.append("### PARMLIB → Program References")
    if parmlib_progs:
        lines.append("| PARMLIB Member | Program/Utility | Purpose | Line |")
        lines.append("|----------------|-----------------|---------|------|")
        for prog in sorted(parmlib_progs, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(
                f"| {prog['source']} | {prog.get('program', '')} | "
                f"{prog.get('purpose', '-')} | {prog.get('line', '')} |"
            )
    else:
        lines.append("*No program references found.*")
    lines.append("")
    
    # 4.2 Dataset Allocations
    lines.append("### PARMLIB → Dataset Allocations")
    if parmlib_datasets:
        lines.append("| PARMLIB Member | Dataset | Type | Disposition | Line |")
        lines.append("|----------------|---------|------|-------------|------|")
        for ds in sorted(parmlib_datasets, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(
                f"| {ds['source']} | {ds.get('dataset', '')} | "
                f"{ds.get('type', '-')} | {ds.get('disposition', '-')} | {ds.get('line', '')} |"
            )
    else:
        lines.append("*No dataset allocations found.*")
    lines.append("")
    
    # 4.3 JCL References
    lines.append("### PARMLIB → JCL References")
    lines.append("JCL jobs and procedures that reference these PARMLIB members.")
    lines.append("")
    if parmlib_jcl:
        lines.append("| PARMLIB Member | JCL Job | Step | Usage | Line |")
        lines.append("|----------------|---------|------|-------|------|")
        for jcl in sorted(parmlib_jcl, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            lines.append(
                f"| {jcl['source']} | {jcl.get('jcl_job', '')} | "
                f"{jcl.get('step', '-')} | {jcl.get('usage', '-')} | {jcl.get('line', '')} |"
            )
    else:
        lines.append("*No JCL references found.*")
    lines.append("")
    
    # 4.4 System Parameters
    lines.append("### PARMLIB → System Parameters")
    lines.append("System-level configuration settings defined in PARMLIB members.")
    lines.append("")
    if parmlib_sysparms:
        lines.append("| PARMLIB Member | Parameter | Value | Category | Line |")
        lines.append("|----------------|-----------|-------|----------|------|")
        for param in sorted(parmlib_sysparms, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            # Truncate long values for readability
            value = param.get('value', '-')
            if len(str(value)) > 50:
                value = str(value)[:47] + "..."
            lines.append(
                f"| {param['source']} | {param.get('parameter', '')} | "
                f"{value} | {param.get('category', '-')} | {param.get('line', '')} |"
            )
    else:
        lines.append("*No system parameters found.*")
    lines.append("")
    
    # 4.5 Symbolic Substitutions
    lines.append("### PARMLIB → Symbolic Substitutions")
    lines.append("Symbolic parameters that can be substituted in JCL.")
    lines.append("")
    if parmlib_symbols:
        lines.append("| PARMLIB Member | Symbol | Value | Scope | Line |")
        lines.append("|----------------|--------|-------|-------|------|")
        for symbol in sorted(parmlib_symbols, key=lambda x: (x.get('source', ''), x.get('line', 0))):
            value = symbol.get('value', '-')
            if len(str(value)) > 40:
                value = str(value)[:37] + "..."
            lines.append(
                f"| {symbol['source']} | {symbol.get('symbol', '')} | "
                f"{value} | {symbol.get('scope', '-')} | {symbol.get('line', '')} |"
            )
    else:
        lines.append("*No symbolic substitutions found.*")
    lines.append("")
    
 
    # =========================================================
    # GAPS SECTION
    # =========================================================
    lines.append("---")
    lines.append("## Gaps")
    lines.append("")
    
    # Unresolved Dynamic CALLs (Merged COBOL + PL/I)
    lines.append("### Unresolved Dynamic CALLs")
    lines.append("")
    if all_unresolved:
        lines.append("| Program | Variable | Line |")
        lines.append("|---------|----------|------|")
        for call in sorted(all_unresolved, key=lambda x: (x.get('source', ''), x.get('line', 0))):
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