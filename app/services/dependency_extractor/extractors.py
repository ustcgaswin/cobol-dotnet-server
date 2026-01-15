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