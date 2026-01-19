"""Dependency extractors for different file types."""

import re
from typing import Any


def extract_cobol_dependencies(consolidated_data: list[dict]) -> dict:
    """Extract all dependencies from COBOL parsed data.
    
    Args:
        consolidated_data: List of parsed COBOL program dictionaries
        
    Returns:
        Dictionary with categorized dependencies:
        - program_calls: Static and dynamic CALLs
        - unresolved_calls: Dynamic CALLs that couldn't be resolved
        - copybooks: COPY statement references
        - sql_tables: Tables referenced in SQL queries
        - file_definitions: SELECT statements (file→DD mapping)
        - file_io: READ/WRITE/OPEN/CLOSE operations
    """
    program_calls = []
    unresolved_calls = []
    copybooks = []
    sql_tables = []
    file_definitions = []
    file_io = []
    
    for program in consolidated_data:
        program_name = program.get('program_name', 'UNKNOWN')
        dependencies = program.get('dependencies', {})
        
        # Extract CALL statements
        for call in dependencies.get('calls', []):
            call_type = call.get('call_type', 'static')
            target = call.get('program_name')
            
            call_entry = {
                'source': program_name,
                'target': target,
                'call_type': call_type,
                'line': call.get('line'),
            }
            
            if call_type == 'dynamic':
                call_entry['variable'] = call.get('variable')
                call_entry['resolved'] = call.get('resolved', False)
                if call.get('possible_values'):
                    call_entry['possible_values'] = call.get('possible_values')
                
                if not call.get('resolved', False):
                    unresolved_calls.append(call_entry)
                else:
                    program_calls.append(call_entry)
            else:
                program_calls.append(call_entry)
        
        # Extract COPY statements
        for copy in dependencies.get('copybooks', []):
            copybooks.append({
                'source': program_name,
                'copybook': copy.get('copybook_name', copy.get('copybook', '')),
                'line': copy.get('line'),
                'library': copy.get('library'),
                'replacing': copy.get('replacing'),
            })
        
        # Extract SQL tables
        tables_seen = set()
        for sql in dependencies.get('sql_queries', []):
            operation = sql.get('operation', 'UNKNOWN')
            # Get tables from either 'tables' list or single 'table'
            tables = sql.get('tables', [])
            if not tables and sql.get('table'):
                tables = [sql.get('table')]
            
            for table in tables:
                if table and table.upper() not in ('SQLCA',):  # Exclude non-tables
                    key = (program_name, table.upper())
                    if key not in tables_seen:
                        tables_seen.add(key)
                        # Find or create entry
                        existing = next(
                            (t for t in sql_tables 
                             if t['source'] == program_name and t['table'] == table.upper()),
                            None
                        )
                        if existing:
                            if operation not in existing['operations']:
                                existing['operations'].append(operation)
                        else:
                            sql_tables.append({
                                'source': program_name,
                                'table': table.upper(),
                                'operations': [operation] if operation != 'UNKNOWN' else [],
                            })
        
        # Extract file operations
        # Known non-file keywords that legacylens may incorrectly capture
        non_file_keywords = {
            'EXIT', 'END', 'PERFORM', 'CALL', 'MOVE', 'IF', 'ELSE',
            'END-IF', 'END-PERFORM', 'END-READ', 'END-WRITE', 'STOP',
            'GOBACK', 'CONTINUE', 'AT', 'NOT', 'INTO', 'FROM',
            'OPEN', 'OUTPUT', 'INPUT', 'I-O', 'EXTEND'
        }
        
        for file_op in dependencies.get('file_operations', []):
            operation = file_op.get('operation', '')
            
            if operation == 'SELECT':
                # File definition - extract from legacylens or custom format
                file_name = file_op.get('file_name', '')
                assign_to = file_op.get('assign_to', '')
                
                # Clean up assign_to - legacylens may include extra content
                if assign_to and '\n' in assign_to:
                    # Extract just the DDname (first word)
                    assign_to = assign_to.split()[0] if assign_to.split() else assign_to
                
                # Extract organization from either direct field or groups
                org = file_op.get('organization')
                access = file_op.get('access_mode')
                
                if file_name:  # Only add if we have a file name
                    file_definitions.append({
                        'source': program_name,
                        'logical_name': file_name,
                        'dd_name': assign_to.replace('\n', ' ').strip()[:20],  # Limit length
                        'organization': org,
                        'access_mode': access,
                        'line': file_op.get('line'),
                    })
            elif operation in ('OPEN', 'CLOSE', 'READ', 'WRITE', 'DELETE', 'REWRITE'):
                # File I/O operation
                files = file_op.get('files', [])
                if file_op.get('file_name'):
                    files = [file_op.get('file_name')]
                
                # Filter out non-file entries
                valid_files = [
                    f for f in files 
                    if f and f.upper() not in non_file_keywords
                    and not f.isdigit()
                    and len(f) > 1
                    and not f.upper().startswith('END-')
                    # Filter out paragraph names (pattern like 0000-NAME or NNNN-NAME)
                    and not (len(f) > 5 and f[4] == '-' and f[:4].isdigit())
                ]
                
                for file_name in valid_files:
                    file_io.append({
                        'source': program_name,
                        'file': file_name,
                        'operation': operation,
                        'mode': file_op.get('mode'),
                        'line': file_op.get('line'),
                    })
    
    return {
        'program_calls': program_calls,
        'unresolved_calls': unresolved_calls,
        'copybooks': copybooks,
        'sql_tables': sql_tables,
        'file_definitions': file_definitions,
        'file_io': file_io,
    }


def extract_copybook_dependencies(consolidated_data: list[dict]) -> dict:
    """Extract nested COPY references from copybook parsed data.
    
    Args:
        consolidated_data: List of parsed copybook dictionaries
        
    Returns:
        Dictionary with:
        - copybook_to_copybook: Nested COPY statements
    """
    copybook_to_copybook = []
    
    for copybook in consolidated_data:
        copybook_name = copybook.get('copybook_name', 'UNKNOWN')
        
        # Extract nested COPY references
        for copy_ref in copybook.get('copy_references', []):
            copybook_to_copybook.append({
                'source': copybook_name,
                'target': copy_ref.get('copybook_name', ''),
                'line': copy_ref.get('line'),
                'library': copy_ref.get('library'),
                'replacing': copy_ref.get('replacing'),
            })
    
    return {
        'copybook_to_copybook': copybook_to_copybook,
    }

def extract_jcl_dependencies(consolidated_data: list[dict]) -> dict:
    program_calls = []      # EXEC PGM
    proc_calls = []         # EXEC PROC
    include_groups = []     # INCLUDE
    file_references = []    # DD Statements
    
    for jcl in consolidated_data:
        source_name = jcl.get('job_name') or jcl.get('file_name', 'UNKNOWN')
        
        # Categorize calls based on Type
        for dep in jcl.get('dependencies', []):
            entry = {'source': source_name, 'target': dep.get('name')}
            
            if dep.get('type') == 'PROGRAM':
                program_calls.append(entry)
            elif dep.get('type') == 'PROCEDURE':
                proc_calls.append(entry)
            elif dep.get('type') == 'INCLUDE_GROUP':
                include_groups.append(entry)

        # Detailed File References (Similar to COBOL File I/O)
        for step in jcl.get('steps', []):
            step_name = step.get('step_name', 'STEP??')
            for dd in step.get('dd_statements', []):
                dsn = dd.get('dsn')
                if dsn:
                    # Determine "Mode" based on JCL Disposition
                    disp = dd.get('disp', 'SHR')
                    mode = "OUTPUT/CREATE" if disp in ['NEW', 'MOD'] else "INPUT/READ"
                    
                    file_references.append({
                        'source': source_name,
                        'step': step_name,
                        'dd_name': dd.get('dd_name'),
                        'dsn': dsn,
                        'mode': mode,
                        'disposition': disp
                    })
                    
    return {
        'jcl_program_calls': program_calls,
        'jcl_proc_calls': proc_calls,
        'jcl_includes': include_groups,
        'jcl_files': file_references,
    }


def extract_assembly_dependencies(consolidated_data: list[dict]) -> dict:
    """
    Extract dependencies from parsed Assembly (HLASM) data.
    
    Returns categorized lists including:
    - program_calls: Static calls via =V() constants
    - copy_files: COPY statement references
    - file_io: Files accessed via OPEN/CLOSE/GET/PUT/READ/WRITE
    - db2_usage: Flagging DSNHLI (DB2) access
    - externals: EXTRN/WXTRN symbols
    """
    program_calls = []
    copy_files = []
    file_io = []
    db2_usage = []
    externals = []

    for module in consolidated_data:
        # Module identification
        source_name = module.get('assembly_name') or module.get('source_file', 'UNKNOWN')
        deps = module.get('dependencies', {})
        
        # 1. Extract Copy Files
        for copy in deps.get('copy_files', []):
            copy_files.append({
                'source': source_name,
                'copybook': copy.get('name'),
                'line': copy.get('line')
            })

        # 2. Extract Static Subprogram Calls (=V calls)
        # Note: This captures only explicit =V(PROG) references
        for vcall in deps.get('subprograms_called', []):
            program_calls.append({
                'source': source_name,
                'target': vcall.get('target'),
                'call_type': 'static_v_con',
                'line': vcall.get('line')
            })

        # 3. Extract File I/O (Sequential/VSAM)
        for f in deps.get('files_accessed', []):
            file_io.append({
                'source': source_name,
                'file': f.get('ddname'),
                'operation': f.get('operation'),
                'line': f.get('line')
            })

        # 4. Extract DB2 Access
        for db2 in deps.get('db2_access', []):
            db2_usage.append({
                'source': source_name,
                'type': db2.get('type'),
                'line': db2.get('line')
            })

        # 5. Extract External Symbols (EXTRN/WXTRN)
        for ext in deps.get('externals', []):
            externals.append({
                'source': source_name,
                'name': ext.get('name'),
                'type': ext.get('type'),
                'line': ext.get('line')
            })

    return {
        'program_calls': program_calls,
        'copybooks': copy_files,
        'file_io': file_io,
        'db2_usage': db2_usage,
        'externals': externals
    }


def extract_pli_dependencies(consolidated_data: list[dict]) -> dict:
    """Extract dependencies from PL/I parsed data and normalize to the
    same shape as COBOL extractor so the generator can reuse templates.
    """
    program_calls = []
    unresolved_calls = []
    copybooks = []
    sql_tables = []
    file_definitions = []
    file_io = []

    for program in consolidated_data:
        # Program identification - prefer parser-provided program id
        program_name = (
            program.get('structure', {}).get('program_id')
            or program.get('meta', {}).get('source_file')
            or 'UNKNOWN'
        )

        deps = program.get('dependencies', {})

        # Calls
        for call in deps.get('calls', []):
            target = call.get('target') or call.get('program_name')
            ctype = call.get('type', '').upper()
            # treat FETCH/DYNAMIC types as dynamic
            call_type = 'dynamic' if 'DYNAMIC' in ctype or 'FETCH' in ctype else 'static'

            entry = {
                'source': program_name,
                'target': target,
                'call_type': call_type,
                'line': call.get('line'),
            }

            if call_type == 'dynamic':
                entry['variable'] = call.get('variable')
                entry['resolved'] = call.get('resolved', False)
                if not entry['resolved']:
                    unresolved_calls.append(entry)
                else:
                    program_calls.append(entry)
            else:
                program_calls.append(entry)

        # Includes / copybooks
        for inc in deps.get('includes', []):
            # include entries can be preprocessor/include or SQL includes
            copybooks.append({
                'source': program_name,
                'copybook': inc.get('name') or inc.get('copybook_name', ''),
                'line': inc.get('line'),
                'library': inc.get('library'),
                'replacing': inc.get('replacing'),
            })

        # SQL tables
        for tbl in deps.get('sql_tables', []):
            table_name = tbl.get('table')
            if table_name:
                existing = next((t for t in sql_tables if t['source'] == program_name and t['table'] == table_name), None)
                operation = tbl.get('access') or tbl.get('operation') or 'UNKNOWN'
                if existing:
                    if operation not in existing['operations']:
                        existing['operations'].append(operation)
                else:
                    sql_tables.append({
                        'source': program_name,
                        'table': table_name,
                        'operations': [operation] if operation != 'UNKNOWN' else []
                    })

        # File definitions (from io.file_descriptors)
        for fd in program.get('io', {}).get('file_descriptors', []):
            file_definitions.append({
                'source': program_name,
                'logical_name': fd.get('internal_name') or fd.get('name'),
                'dd_name': fd.get('ddname') or fd.get('internal_name'),
                'organization': None,
                'access_mode': None,
                'line': fd.get('line'),
            })

        # File I/O operations
        for op in program.get('io', {}).get('operations', []):
            file_io.append({
                'source': program_name,
                'file': op.get('file') or op.get('file_name'),
                'operation': op.get('op') or op.get('operation'),
                'mode': None,
                'line': op.get('line'),
            })

    return {
        'program_calls': program_calls,
        'unresolved_calls': unresolved_calls,
        'copybooks': copybooks,
        'sql_tables': sql_tables,
        'file_definitions': file_definitions,
        'file_io': file_io,
    }


def extract_pli_copybook_dependencies(consolidated_data: list[dict]) -> dict:
    """Extract nested include references from PL/I copybook parsed data.
    PL/I copybooks typically list includes under dependencies.includes.
    Normalize to same shape as COBOL copybook extractor.
    """
    copybook_to_copybook = []

    for cb in consolidated_data:
        cb_name = cb.get('meta', {}).get('source_file') or cb.get('copybook_name') or 'UNKNOWN'
        for inc in cb.get('dependencies', {}).get('includes', []):
            copybook_to_copybook.append({
                'source': cb_name,
                'target': inc.get('name') or inc.get('copybook_name', ''),
                'line': inc.get('line'),
                'library': inc.get('library'),
                'replacing': inc.get('replacing'),
            })

    return {'copybook_to_copybook': copybook_to_copybook}

def extract_ca7_dependencies(consolidated_data: list[dict]) -> dict:
    """
    Extracts dependencies from CA-7 JSON data.
    - Merges multiple entries for the same job name.
    - Filters out report headers (DATE, PAGE, LIST).
    - Categorizes into Job-to-Job, DSN-to-Job, and User-Requirements.
    """
    merged_jobs = {}
    
    # Noise names that often appear in report headers
    noise_names = {'DATE', 'TIME', 'PAGE', 'LIST', 'SYS'}

    for entry in consolidated_data:
        # consolidated_data is usually a list containing the parser result dict
        jobs_list = entry.get('jobs', []) if isinstance(entry, dict) else []
        
        for job in jobs_list:
            name = job.get('job_name', '').upper()
            
            # 1. Skip report noise and wildcards
            if not name or name in noise_names or '*' in name:
                continue

            # 2. Initialize or retrieve the merged record
            if name not in merged_jobs:
                merged_jobs[name] = {
                    'job_name': name,
                    'system': None,
                    'schid': None,
                    'job_dependencies': set(),
                    'dataset_dependencies': set(),
                    'user_requirements': set()
                }

            # 3. Merge Metadata (if present in this block)
            header = job.get('header', {})
            if header.get('system'):
                merged_jobs[name]['system'] = header.get('system')
            if header.get('schedule_id'):
                merged_jobs[name]['schid'] = header.get('schedule_id')

            # 4. Merge Requirements (Using sets to avoid duplicates)
            reqs = job.get('requirements', {})
            merged_jobs[name]['job_dependencies'].update(reqs.get('job_dependencies', []))
            merged_jobs[name]['dataset_dependencies'].update(reqs.get('dataset_dependencies', []))
            merged_jobs[name]['user_requirements'].update(reqs.get('user_requirements', []))

    # Prepare final output lists
    job_to_job = []
    dsn_to_job = []
    user_reqs = []

    for name, data in merged_jobs.items():
        # Job -> Job links (Predecessor -> Successor)
        for pred in data['job_dependencies']:
            job_to_job.append({
                'source': pred.upper(),
                'target': name,
                'type': 'CA7_PREDECESSOR'
            })
            
        # Dataset -> Job links (Triggering File -> Successor)
        for dsn in data['dataset_dependencies']:
            dsn_to_job.append({
                'source': dsn.upper(),
                'target': name,
                'type': 'CA7_DATASET_TRIGGER'
            })
            
        # User Requirements (Manual steps)
        for usr in data['user_requirements']:
            user_reqs.append({
                'source': usr.upper(),
                'target': name,
                'type': 'CA7_USER_REQUIREMENT'
            })

    return {
        'ca7_job_flow': job_to_job,
        'ca7_dataset_triggers': dsn_to_job,
        'ca7_user_requirements': user_reqs,
        'ca7_nodes': [
            {k: (list(v) if isinstance(v, set) else v) for k, v in j.items()} 
            for j in merged_jobs.values()
        ]
    }