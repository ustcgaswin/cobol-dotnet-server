"""Graph Analysis Module using NetworkX.

Constructs a directed graph representing the entire system architecture,
including Control Flow (Calls/Execs), Data Flow (SQL/Files), and Structural dependencies.

This engine consumes the raw output from the various Parsers/Extractors and
synthesizes it into a mathematical graph for analysis and visualization.
"""

import networkx as nx
from typing import Dict, List, Any, Optional
from app.services.documentation_services.models import SystemMetrics

class GraphAnalyzer:
    def __init__(self, raw_dependency_data: Dict[str, Any]):
        """
        Args:
            raw_dependency_data: Dictionary where keys are file types (e.g., 'COBOL', 'JCL')
                                 and values are the output from the Extractor strategies.
        """
        self.graph = nx.DiGraph()
        
        # Normalize keys to upper case to match Enums/Strings consistently across different extractors
        self.data_map = {k.upper(): v for k, v in raw_dependency_data.items()}
        
        self._build_graph()

    def _build_graph(self):
        """Orchestrates the graph construction across all file types."""
        # Process Logic & Control Flow
        self._process_cobol()
        self._process_pli()
        self._process_assembly()
        self._process_rexx()
        
        # Process Orchestration & Config
        self._process_jcl()
        self._process_ca7()
        self._process_parmlib()
        
        # Process Structure (Copybooks/Includes)
        self._process_copybooks() 

    def _add_edge(self, source: str, target: str, rel_type: str, weight: int = 1):
        """Helper to add edges with consistent attributes."""
        if not source or not target:
            return
        
        # Standardize names to ensure 'PROGA' matches 'proga'
        src = source.upper().strip()
        tgt = target.upper().strip()
        
        # Don't link to self
        if src == tgt:
            return

        self.graph.add_edge(src, tgt, type=rel_type, weight=weight)

    def _process_cobol(self):
        """Map COBOL relationships."""
        data = self.data_map.get('COBOL', {})
        
        # 1. Program Calls (Control Flow)
        for call in data.get('program_calls', []):
            self._add_edge(call['source'], call['target'], 'CALL', weight=3)
            
        # 2. Copybooks (Structural)
        for copy in data.get('copybooks', []):
            self._add_edge(copy['source'], copy['copybook'], 'INCLUDE', weight=1)
            
        # 3. SQL Tables (Data Flow)
        for sql in data.get('sql_tables', []):
            self._add_edge(sql['source'], sql['table'], 'ACCESS_DB', weight=2)
            
        # 4. File I/O (Data Flow)
        for io in data.get('file_io', []):
            # Prefix files to avoid collision with programs (e.g. program named 'REPORT' writing to file 'REPORT')
            self._add_edge(io['source'], f"FILE:{io['file']}", 'ACCESS_FILE', weight=2)

    def _process_pli(self):
        """Map PL/I relationships based on PLIParser structure."""
        data = self.data_map.get('PLI', {})
        
        programs = data if isinstance(data, list) else [data]
        
        for prog in programs:
            src = prog.get('program_name', 'UNKNOWN_PLI')
            if src == 'UNKNOWN_PLI':
                src = prog.get('meta', {}).get('source_file', 'UNKNOWN_PLI')

            # 1. Calls
            calls = prog.get('program_calls', []) # Extractor format
            if not calls: calls = prog.get('dependencies', {}).get('calls', []) # Parser format
            
            for call in calls:
                target = call.get('target')
                self._add_edge(src, target, 'CALL', weight=3)

            # 2. Includes
            includes = prog.get('copybooks', []) # Extractor format
            if not includes: includes = prog.get('dependencies', {}).get('includes', []) # Parser format
            
            for inc in includes:
                target = inc.get('copybook') or inc.get('name')
                self._add_edge(src, target, 'INCLUDE', weight=1)

            # 3. SQL Tables
            tables = prog.get('sql_tables', []) # Extractor format
            if not tables: tables = prog.get('dependencies', {}).get('sql_tables', []) # Parser format
            
            for sql in tables:
                target = sql.get('table')
                self._add_edge(src, target, 'ACCESS_DB', weight=2)

            # 4. File I/O (Logical DD Names)
            # Check Extractor format first
            fds = prog.get('file_definitions', [])
            if not fds: fds = prog.get('io', {}).get('file_descriptors', [])
            
            for fd in fds:
                dd_name = fd.get('dd_name') or fd.get('ddname')
                if dd_name:
                    self._add_edge(src, f"DD:{dd_name}", 'ACCESS_FILE', weight=2)

    def _process_assembly(self):
        """Map HLASM relationships."""
        data = self.data_map.get('ASSEMBLY', {})
        
        # 1. External Calls (=V constants)
        for call in data.get('program_calls', []):
            self._add_edge(call['source'], call['target'], 'CALL', weight=3)
            
        # 2. Copy/Macros
        for copy in data.get('copybooks', []):
            self._add_edge(copy['source'], copy['copybook'], 'INCLUDE', weight=1)

    def _process_jcl(self):
        """Map JCL Execution flow."""
        data = self.data_map.get('JCL', {})
        
        # 1. Execute Programs
        for pgm in data.get('jcl_program_calls', []):
            self._add_edge(pgm['source'], pgm['target'], 'EXEC_PGM', weight=5)
            
        # 2. Execute Procedures
        for proc in data.get('jcl_proc_calls', []):
            self._add_edge(proc['source'], proc['target'], 'EXEC_PROC', weight=5)
            
        # 3. Includes
        for inc in data.get('jcl_includes', []):
            self._add_edge(inc['source'], inc['target'], 'INCLUDE', weight=1)

    def _process_rexx(self):
        """Map REXX orchestration logic."""
        data = self.data_map.get('REXX', {})
        
        # 1. External Calls (COBOL, JCL Submission)
        for call in data.get('cobol_calls', []):
            # Direct calls to COBOL
            self._add_edge(call['source'], call['target'], 'CALL', weight=3)
            
        for job in data.get('jcl_submissions', []):
            # Submitting a job
            self._add_edge(job['source'], job['job'], 'SUBMIT_JOB', weight=4)

        # 2. File Operations (from REXXParser.filesAccessed)
        for op in data.get('dataset_operations', []):
            # Using extractor's normalized output
            self._add_edge(op['source'], f"FILE:{op['dataset']}", 'ACCESS_FILE', weight=2)

    def _process_ca7(self):
        """Map Scheduler triggers."""
        data = self.data_map.get('CA7', {})
        
        # 1. Job Triggers (Job A -> triggers -> Job B)
        for flow in data.get('ca7_job_flow', []):
            self._add_edge(flow['source'], flow['target'], 'TRIGGER', weight=5)
            
        # 2. Dataset Triggers (File Creation -> triggers -> Job A)
        for dsn in data.get('ca7_dataset_triggers', []):
            # Normalize dataset name to match other file nodes
            dsn_name = f"FILE:{dsn['source']}" if not dsn['source'].startswith('FILE:') else dsn['source']
            self._add_edge(dsn_name, dsn['target'], 'TRIGGER', weight=5)

    def _process_copybooks(self):
        """Map nested structure definitions."""
        # Handles COBOL Copybooks and PL/I Copybooks
        for key in ['COPYBOOK', 'PLI_COPYBOOK']:
            data = self.data_map.get(key, {})
            for ref in data.get('copybook_to_copybook', []):
                self._add_edge(ref['source'], ref['target'], 'INCLUDE', weight=1)

    def _process_parmlib(self):
        """Map system configuration references."""
        data = self.data_map.get('PARMLIB', {})
        
        for ref in data.get('program_references', []):
            self._add_edge(ref['source'], ref['program'], 'CONFIGURES', weight=2)

    def get_metrics(self, total_files: int, type_counts: dict) -> SystemMetrics:
        """Calculates graph metrics to identify 'God Classes' and Orphans."""
        top_complex = []
        
        if self.graph.number_of_nodes() > 0:
            # We use 'degree' (in + out) to find central hubs.
            # Alternatively, use 'in_degree' to find most re-used components.
            sorted_nodes = sorted(self.graph.in_degree, key=lambda x: x[1], reverse=True)
            
            # Format: "PROGRAM_NAME (50 calls)"
            # Filter out generic nodes like 'SQLCA' if they appear
            filtered_nodes = [n for n in sorted_nodes if n[0] not in ('SQLCA', 'DFHEIBLK')]
            
            top_complex = [f"{n} ({d} references)" for n, d in filtered_nodes[:10]]

        return SystemMetrics(
            total_files=total_files,
            files_by_type=type_counts,
            top_complex_modules=top_complex
        )

    def generate_mermaid_diagram(self, max_nodes=50) -> str:
        """Generates a Mermaid.js diagram focusing on CONTROL FLOW.
        
        Filters out low-level noise (like Copybook includes) to create 
        a readable high-level architecture diagram.
        """
        if self.graph.number_of_nodes() == 0:
            return "*No dependencies detected.*"

        # 1. Filter edges to prioritize Control Flow
        # We value Execution and Data Flow higher than static Includes for the architecture view
        priority_types = {
            'EXEC_PGM', 'EXEC_PROC', 'TRIGGER', 'SUBMIT_JOB', 'CALL', 'ACCESS_DB'
        }
        
        control_edges = [
            (u, v) for u, v, d in self.graph.edges(data=True) 
            if d.get('type') in priority_types
        ]
        
        # If we have control flow, use it. Otherwise, fallback to everything.
        if control_edges:
            view_graph = self.graph.edge_subgraph(control_edges)
        else:
            view_graph = self.graph

        # 2. Select Top Nodes based on Degree (Centrality) within this view
        degrees = dict(view_graph.degree())
        if not degrees:
            return "*No significant control flow detected.*"
            
        important_nodes = sorted(degrees, key=degrees.get, reverse=True)[:max_nodes]
        subgraph = self.graph.subgraph(important_nodes)

        # 3. Generate Mermaid Syntax
        lines = ["```mermaid", "graph TD"]
        
        for u, v, data in subgraph.edges(data=True):
            u_clean = self._sanitize_node(u)
            v_clean = self._sanitize_node(v)
            edge_type = data.get('type')

            # Mermaid Styling
            if edge_type in ('EXEC_PGM', 'EXEC_PROC'):
                arrow = "==>" # Thick arrow for JCL Execution
            elif edge_type == 'TRIGGER':
                arrow = "-.->" # Dotted arrow for Scheduling
            elif edge_type == 'SUBMIT_JOB':
                arrow = "-- submits -->"
            elif edge_type == 'ACCESS_DB':
                arrow = "-- db -->"
            elif edge_type == 'ACCESS_FILE':
                arrow = "-- io -->"
            elif edge_type == 'INCLUDE':
                arrow = "---" # Solid line for static include
            else:
                arrow = "-->" # Standard call

            lines.append(f"    {u_clean}[{u}] {arrow} {v_clean}[{v}]")
            
        lines.append("```")
        return "\n".join(lines)

    def _sanitize_node(self, name: str) -> str:
        """Cleans node names for Mermaid syntax."""
        # Replace characters that break Mermaid (dots, colons, spaces, brackets)
        return name.replace('.', '_').replace('-', '_').replace(':', '_').replace(' ', '_').replace('(', '').replace(')', '')