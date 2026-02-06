"""Graph Analysis Module using NetworkX.

Constructs a directed graph representing the entire system architecture.
Updated to align with v2.0 Parsers and Extractors.
"""

import networkx as nx
from typing import Dict, List, Any
from app.api.schemas.doc_models import SystemMetrics
from loguru import logger

class GraphAnalyzer:
    def __init__(self, raw_dependency_data: Dict[str, Any]):
        """
        Args:
            raw_dependency_data: Dictionary from DependencyExtractorService.
                                 Keys: 'COBOL', 'JCL', 'REXX', etc.
        """
        self.graph = nx.DiGraph()
        
        # Normalize keys to upper case
        self.data_map = {k.upper(): v for k, v in raw_dependency_data.items()}
        
        self._build_graph()

    def _build_graph(self):
        """Orchestrates graph construction."""
        self._process_cobol()
        self._process_pli()
        self._process_assembly()
        self._process_rexx()
        self._process_jcl()
        self._process_ca7()
        self._process_parmlib()
        self._process_copybooks() 
        self._process_control_cards()

    def _add_edge(self, source: str, target: str, rel_type: str, weight: int = 1):
        if not source or not target: return
        src = source.upper().strip()
        tgt = target.upper().strip()
        # Don't link to self
        if src == tgt: return
        self.graph.add_edge(src, tgt, type=rel_type, weight=weight)

    # --- PROCESSORS ---

    def _process_cobol(self):
        """Map COBOL relationships."""
        data = self.data_map.get('COBOL', {})
        # 1. Calls
        for call in data.get('program_calls', []):
            self._add_edge(call['source'], call['target'], 'CALL', weight=3)
        # 2. Copybooks
        for copy in data.get('copybooks', []):
            self._add_edge(copy['source'], copy['copybook'], 'INCLUDE', weight=1)
        # 3. SQL
        for sql in data.get('sql_tables', []):
            self._add_edge(sql['source'], sql['table'], 'ACCESS_DB', weight=2)
        # 4. Files
        for io in data.get('file_io', []):
            self._add_edge(io['source'], f"FILE:{io['file']}", 'ACCESS_FILE', weight=2)

    def _process_pli(self):
        """Map PL/I relationships based on Extractor structure."""
        data = self.data_map.get('PLI', {})
        
        # The PLI Extractor (extract_pli_dependencies) returns a dictionary of lists, 
        # NOT a list of programs. We access the lists directly.

        # 1. Calls
        for call in data.get('program_calls', []):
            self._add_edge(call['source'], call['target'], 'CALL', weight=3)

        # 2. Includes
        for inc in data.get('copybooks', []):
            # Extractor normalizes keys to 'copybook'
            target = inc.get('copybook')
            self._add_edge(inc['source'], target, 'INCLUDE', weight=1)

        # 3. SQL Tables
        for sql in data.get('sql_tables', []):
            self._add_edge(sql['source'], sql['table'], 'ACCESS_DB', weight=2)

        # 4. File I/O
        for fd in data.get('file_definitions', []):
            # Links Program -> DDName (Logical File)
            dd_name = fd.get('dd_name') or fd.get('logical_name')
            if dd_name:
                self._add_edge(fd['source'], f"DD:{dd_name}", 'ACCESS_FILE', weight=2)
        
        for io in data.get('file_io', []):
             self._add_edge(io['source'], f"FILE:{io['file']}", 'ACCESS_FILE', weight=2)

    def _process_assembly(self):
        """Map HLASM relationships."""
        data = self.data_map.get('ASSEMBLY', {})
        for call in data.get('program_calls', []):
            self._add_edge(call['source'], call['target'], 'CALL', weight=3)
        for copy in data.get('copybooks', []):
            self._add_edge(copy['source'], copy['copybook'], 'INCLUDE', weight=1)
        for io in data.get('file_io', []):
            self._add_edge(io['source'], f"DD:{io['file']}", 'ACCESS_FILE', weight=2)

    def _process_jcl(self):
        """Map JCL Execution flow."""
        data = self.data_map.get('JCL', {})
        # 1. Exec PGM
        for pgm in data.get('jcl_program_calls', []):
            self._add_edge(pgm['source'], pgm['target'], 'EXEC_PGM', weight=5)
        # 2. Exec PROC
        for proc in data.get('jcl_proc_calls', []):
            self._add_edge(proc['source'], proc['target'], 'EXEC_PROC', weight=5)
        # 3. Datasets
        for ds in data.get('jcl_files', []):
            # Infer Read vs Write based on Disposition
            mode = ds.get('mode', '').upper()
            edge_type = 'WRITE_FILE' if 'NEW' in mode or 'OUTPUT' in mode else 'READ_FILE'
            self._add_edge(ds['source'], f"FILE:{ds['dsn']}", edge_type, weight=2)

    def _process_rexx(self):
        """Map REXX orchestration."""
        data = self.data_map.get('REXX', {})
        # 1. COBOL Calls
        for call in data.get('cobol_calls', []):
            self._add_edge(call['source'], call['target'], 'CALL', weight=3)
        # 2. JCL Submission
        for job in data.get('jcl_submissions', []):
            self._add_edge(job['source'], job['job'], 'SUBMIT_JOB', weight=4)
        # 3. File Operations
        for op in data.get('dataset_operations', []):
            self._add_edge(op['source'], f"FILE:{op['dataset']}", 'ACCESS_FILE', weight=2)

    def _process_ca7(self):
        """Map Scheduler triggers."""
        data = self.data_map.get('CA7', {})
        for flow in data.get('ca7_job_flow', []):
            self._add_edge(flow['source'], flow['target'], 'TRIGGER', weight=5)
        for dsn in data.get('ca7_dataset_triggers', []):
            dsn_name = f"FILE:{dsn['source']}" if not dsn['source'].startswith('FILE:') else dsn['source']
            self._add_edge(dsn_name, dsn['target'], 'TRIGGER', weight=5)

    def _process_copybooks(self):
        """Map nested structure definitions (COBOL & PLI)."""
        for key in ['COPYBOOK', 'PLI_COPYBOOK']:
            data = self.data_map.get(key, {})
            for ref in data.get('copybook_to_copybook', []):
                self._add_edge(ref['source'], ref['target'], 'INCLUDE', weight=1)

    def _process_parmlib(self):
        data = self.data_map.get('PARMLIB', {})
        for ref in data.get('program_references', []):
            edge_type = 'EXEC_UTIL' if ref.get('purpose') == 'UTILITY' else 'CONFIGURES'
            self._add_edge(ref['source'], ref['program'], edge_type, weight=2)

    def _process_control_cards(self):
        """Map Utilities found in .ctl files."""
        # Handle cases where .ctl files might be parsed as 'CONTROL_CARD' or 'FLAT_FILE'
        data = self.data_map.get('CONTROL_CARD', {})
        
        # If control cards contain references to Tables (DB2 Load) or Files (IDCAMS)
        for ref in data.get('referenced_objects', []):
            self._add_edge(ref['source'], ref['target'], 'CONFIGURES', weight=2)

    def get_metrics(self, total_files: int, type_counts: dict) -> SystemMetrics:
        """Calculates graph metrics to identify 'God Classes'."""
        top_complex = []
        if self.graph.number_of_nodes() > 0:
            # Sort by In-Degree (Popularity)
            sorted_nodes = sorted(self.graph.in_degree, key=lambda x: x[1], reverse=True)
            # Filter noise (Files/DDs)
            filtered = [n for n in sorted_nodes if not n[0].startswith('FILE:') and not n[0].startswith('DD:')]
            top_complex = [f"{n} ({d} refs)" for n, d in filtered[:10]]

        return SystemMetrics(
            total_files=total_files,
            files_by_type=type_counts,
            top_complex_modules=top_complex
        )

    def identify_critical_files(self, summaries: List[Any], top_n: int = 30) -> List[str]:
        """
        Scoring Algorithm to find the 'Critical Path'.
        Score = (Connections * 1.0) + (DB_Access * 2.5) + (LOC_Factor * 1.5)
        """
        scores = {}
        
        # Build LOC map
        loc_map = {}
        for s in summaries:
            # Handle Pydantic objects or dicts
            fname = s.filename if hasattr(s, 'filename') else s.get('filename', 'UNKNOWN')
            # Fallback LOC calculation
            content = getattr(s, 'original_content', '') or ''
            # Use 100 as default if content missing to avoid skewing low
            loc = len(content.split('\n')) if content else 100
            loc_map[fname] = loc

        for node in self.graph.nodes():
            # Skip data nodes for scoring
            if ":" in node: continue

            degree = self.graph.degree(node) or 0
            
            # Count DB/File edges (Outbound) - High Value
            data_edges = [
                v for u, v, d in self.graph.out_edges(node, data=True) 
                if d.get('type') in ['ACCESS_DB', 'ACCESS_FILE', 'WRITE_FILE']
            ]
            data_gravity = len(data_edges)

            # LOC Factor (Logarithmic capping)
            loc = loc_map.get(node, 0)
            loc_score = min(loc / 100, 50) 

            # Weighted Score
            final_score = (degree * 1.0) + (data_gravity * 2.5) + (loc_score * 1.5)
            scores[node] = final_score

        # Sort and return
        sorted_nodes = sorted(scores.items(), key=lambda x: x[1], reverse=True)
        return [node for node, score in sorted_nodes[:top_n]]

    def generate_mermaid_diagram(self, max_nodes=50) -> str:
        """Visualizes the Control Flow."""
        if self.graph.number_of_nodes() == 0:
            return "graph TD\nWait[No dependencies found]"

        # Prioritize Control Flow edges for diagram clarity
        priority_types = {'EXEC_PGM', 'EXEC_PROC', 'TRIGGER', 'SUBMIT_JOB', 'CALL', 'ACCESS_DB'}
        
        control_edges = [
            (u, v) for u, v, d in self.graph.edges(data=True) 
            if d.get('type') in priority_types
        ]
        
        view_graph = self.graph.edge_subgraph(control_edges) if control_edges else self.graph
        
        # Top nodes by degree
        degrees = dict(view_graph.degree())
        if not degrees: return "graph TD\nWait[No significant flow]"
        
        nodes = sorted(degrees, key=degrees.get, reverse=True)[:max_nodes]
        subgraph = self.graph.subgraph(nodes)

        lines = ["graph TD"]
        for u, v, data in subgraph.edges(data=True):
            u_c = self._sanitize_node(u)
            v_c = self._sanitize_node(v)
            etype = data.get('type')
            
            arrow = "-->"
            if etype in ('EXEC_PGM', 'TRIGGER'): arrow = "==>"
            elif etype == 'ACCESS_DB': arrow = "-.->"
            
            lines.append(f"    {u_c}[{u}] {arrow} {v_c}[{v}]")
            
        return "\n".join(lines)

    def _sanitize_node(self, name: str) -> str:
        """Cleans node names for Mermaid syntax."""
        return name.replace('.', '_').replace('-', '_').replace(':', '_').replace(' ', '')
    
    def render_mermaid_code_to_png(self, mermaid_code: str, output_path: str) -> bool:
        """
        Generic helper to convert any Mermaid string to a PNG file.
        Used for Context, Architecture, and Functional diagrams.
        """
        import base64
        import requests
        import urllib3

        urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

        if not mermaid_code:
            logger.error("No mermaid code provided for rendering")
            return False

        # Ensure the string is encoded properly for the URL
        graphbytes = mermaid_code.encode("utf8")
        base64_bytes = base64.urlsafe_b64encode(graphbytes)
        base64_string = base64_bytes.decode("ascii")
        
        url = "https://mermaid.ink/img/" + base64_string
        
        try:
            response = requests.get(url, verify=False, timeout=30)
            if response.status_code == 200:
                with open(output_path, 'wb') as f:
                    f.write(response.content)
                return True
            else:
                logger.error(f"Mermaid API error {response.status_code} for {output_path}")
                return False
        except Exception as e:
            logger.error(f"Failed to generate PNG at {output_path}: {e}")
            return False
    
    def generate_mermaid_png(self, output_path: str):
        """Standard Architecture Diagram (backward compatibility)."""
        return self.render_mermaid_code_to_png(self.generate_mermaid_diagram(), output_path)
        
    def generate_context_diagram(self) -> str:
        """
        Generates a high-level System Context diagram.
        Shows: External Inputs -> [Mainframe System] -> External Outputs
        """
        inputs = set()
        outputs = set()

        for node in self.graph.nodes():
            if not (node.startswith("FILE:") or node.startswith("DD:")):
                continue
                
            in_degree = self.graph.in_degree(node)
            out_degree = self.graph.out_degree(node)
            
            if in_degree == 0 and out_degree > 0:
                inputs.add(self._sanitize_node(node))
            elif out_degree == 0 and in_degree > 0:
                outputs.add(self._sanitize_node(node))

        inputs = list(inputs)[:5]
        outputs = list(outputs)[:5]
        
        lines = ["graph LR"]
        lines.append("    subgraph Mainframe_System")
        lines.append("        Core_Logic[Core Application Logic]")
        lines.append("    end")
        
        for i in inputs:
            lines.append(f"    {i}({i}) --> Core_Logic")
        
        for o in outputs:
            lines.append(f"    Core_Logic --> {o}({o})")
            
        return "\n".join(lines)