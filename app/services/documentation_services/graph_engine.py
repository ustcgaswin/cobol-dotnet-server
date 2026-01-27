"""Graph Analysis Module using NetworkX."""

import networkx as nx
from typing import Dict, List, Tuple, Any
from app.services.documentation_services.models import SystemMetrics

class GraphAnalyzer:
    def __init__(self, raw_dependency_data: Dict[str, Any]):
        self.graph = nx.DiGraph()
        self.raw_data = raw_dependency_data
        self._build_graph()

    def _build_graph(self):
        """Reconstructs a clean graph from raw upstream data."""
        # 1. COBOL Calls
        calls = self.raw_data.get('COBOL', {}).get('program_calls', [])
        for call in calls:
            self.graph.add_edge(call['source'], call['target'], type='CALL')

        # 2. JCL Executions
        execs = self.raw_data.get('JCL', {}).get('jcl_program_calls', [])
        for ex in execs:
            self.graph.add_edge(ex['source'], ex['target'], type='EXEC')
            
        # 3. CA7 Triggers
        triggers = self.raw_data.get('CA7', {}).get('ca7_job_flow', [])
        for trig in triggers:
            self.graph.add_edge(trig['source'], trig['target'], type='TRIGGER')

    def get_metrics(self, total_files: int, type_counts: dict) -> SystemMetrics:
        """Calculates graph centrality to find 'God Classes'."""
        if self.graph.number_of_nodes() > 0:
            # Sort by degree (number of connections)
            sorted_nodes = sorted(self.graph.degree, key=lambda x: x[1], reverse=True)
            top_complex = [f"{n} ({d} links)" for n, d in sorted_nodes[:5]]
        else:
            top_complex = []

        return SystemMetrics(
            total_files=total_files,
            files_by_type=type_counts,
            top_complex_modules=top_complex
        )

    def generate_mermaid_diagram(self, max_nodes=40) -> str:
        """Generates a Mermaid.js definition for the 'Overall' section."""
        if self.graph.number_of_nodes() == 0:
            return "*No dependencies detected.*"

        degrees = dict(self.graph.degree())
        important_nodes = sorted(degrees, key=degrees.get, reverse=True)[:max_nodes]
        subgraph = self.graph.subgraph(important_nodes)

        lines = ["```mermaid", "graph TD"]
        
        for u, v, data in subgraph.edges(data=True):
            u_clean = u.replace('.', '_').replace('-', '_').replace(' ', '_')
            v_clean = v.replace('.', '_').replace('-', '_').replace(' ', '_')
            
            arrow = "-->"
            if data.get('type') == 'EXEC': arrow = "==>"
            if data.get('type') == 'TRIGGER': arrow = "-.->"
            
            lines.append(f"    {u_clean}[{u}] {arrow} {v_clean}[{v}]")
            
        lines.append("```")
        return "\n".join(lines)