"""Graph Analysis Module using NetworkX.

Constructs a directed graph representing the entire system architecture.
Updated to align with v2.0 Parsers and Extractors.
"""

import matplotlib.pyplot as plt
import networkx as nx
import os
from typing import Dict, List, Any
from app.api.schemas.doc_models import SystemMetrics
from loguru import logger
from app.services.documentation_services.prompts import STRUCTURE_EXTRACTION_PROMPT
from app.config.settings import settings
from app.config.llm_config import get_llm
from langchain_core.messages import HumanMessage
import re
import json

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

    def generate_process_flow_diagram(self, max_nodes: int = 30) -> str:
        """
        VIEW 1: High-Level Orchestration (Timeline).
        Strictly filters for Job-to-Job and JCL-to-Program execution.
        """
        # 1. Strictly define 'Flow' relationships
        flow_rel_types = {'TRIGGER', 'SUBMIT_JOB', 'EXEC_PROC', 'EXEC_PGM'}
        
        # 2. Filter edges: ONLY include the execution flow
        flow_edges = [
            (u, v, d) for u, v, d in self.graph.edges(data=True) 
            if d.get('type') in flow_rel_types
        ]
        
        # 3. Filter nodes: Exclude Files, Tables, and Copybooks
        # This is the "Magic" that makes it look different!
        forbidden_prefixes = ('FILE:', 'DD:', 'TABLE:', 'CPY:')
        
        temp_graph = nx.DiGraph()
        for u, v, d in flow_edges:
            if not any(u.startswith(p) or v.startswith(p) for p in forbidden_prefixes):
                temp_graph.add_edge(u, v, **d)

        if temp_graph.number_of_nodes() == 0:
            return "graph LR\n    Start[\"No Execution Flow Detected\"]"

        # Limit nodes for API stability
        top_nodes = sorted(dict(temp_graph.degree()), key=dict(temp_graph.degree()).get, reverse=True)[:max_nodes]
        subgraph = temp_graph.subgraph(top_nodes)

        lines = ["graph LR"] # Left-to-Right is best for Process Flows
        lines.append("    classDef jobNode fill:#f9f,stroke:#333,stroke-width:2px;")
        
        for u, v, data in subgraph.edges(data=True):
            u_id, v_id = self._sanitize_node(u), self._sanitize_node(v)
            u_lbl, v_lbl = u[:20], v[:20]
            
            # Thicker arrows for Job-to-Job triggers
            arrow = "==>" if data.get('type') == 'TRIGGER' else "-->"
            lines.append(f"    {u_id}[\"{u_lbl}\"] {arrow} {v_id}[\"{v_lbl}\"]")
            
            if data.get('type') == 'TRIGGER':
                lines.append(f"    class {u_id} jobNode")
                lines.append(f"    class {v_id} jobNode")

        return "\n".join(lines)

    def generate_mermaid_diagram(self, max_nodes: int = 40) -> str:
        """
        VIEW 2: Structural Architecture (The Spiderweb).
        Focuses on Programs, Copybooks, and Database Tables.
        """
        # 1. Define 'Structural' relationships
        struct_rel_types = {'CALL', 'ACCESS_DB', 'INCLUDE', 'ACCESS_FILE', 'READ_FILE', 'WRITE_FILE'}
        
        # 2. Filter edges
        struct_edges = [
            (u, v, d) for u, v, d in self.graph.edges(data=True) 
            if d.get('type') in struct_rel_types
        ]
        
        temp_graph = nx.DiGraph()
        for u, v, d in struct_edges:
            temp_graph.add_edge(u, v, **d)

        if temp_graph.number_of_nodes() == 0:
            return "graph TD\n    Start[\"No Structural Dependencies Detected\"]"

        top_nodes = sorted(dict(temp_graph.degree()), key=dict(temp_graph.degree()).get, reverse=True)[:max_nodes]
        subgraph = temp_graph.subgraph(top_nodes)

        lines = ["graph TD"] # Top-Down is best for Hierarchy/Architecture
        lines.append("    classDef dataNode fill:#e1f5fe,stroke:#01579b;")
        
        for u, v, data in subgraph.edges(data=True):
            u_id, v_id = self._sanitize_node(u), self._sanitize_node(v)
            u_lbl, v_lbl = u[:20], v[:20]
            
            etype = data.get('type')
            arrow = "-.->" if "ACCESS" in etype else "-->"
            
            lines.append(f"    {u_id}[\"{u_lbl}\"] {arrow} {v_id}[\"{v_lbl}\"]")
            
            # Highlight Files and Tables
            if any(v.startswith(p) for p in ('FILE:', 'DD:', 'TABLE:')):
                lines.append(f"    class {v_id} dataNode")

        return "\n".join(lines)

    def _sanitize_node(self, name: str) -> str:
        """Extremely aggressive sanitization to ensure Mermaid compatibility."""
        # Remove everything except basic letters and numbers
        clean = re.sub(r'[^a-zA-Z0-9]', '', name)
        # Ensure it starts with a letter and has a unique prefix
        return f"node{clean}"
    
    def render_mermaid_code_to_png(self, mermaid_code: str, output_path: str) -> bool:
        import base64
        import requests
        import urllib3

        urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

        graphbytes = mermaid_code.encode("utf8")
        base64_string = base64.urlsafe_b64encode(graphbytes).decode("ascii")
        url = "https://mermaid.ink/img/" + base64_string
        
        # Total URL length shouldn't exceed ~4000 for most CDNs/APIs
        if len(url) > 4096:
            logger.error(f"URL too long ({len(url)}). Mermaid API will reject this.")
            return False

        try:
            response = requests.get(url, verify=False, timeout=30)
            if response.status_code == 200:
                with open(output_path, 'wb') as f:
                    f.write(response.content)
                return True
            else:
                # THIS LOG IS THE KEY:
                logger.error(f"Mermaid Syntax Error! Status: {response.status_code}")
                logger.error(f"Failed Mermaid Code:\n{mermaid_code}")
                return False
        except Exception as e:
            logger.error(f"Failed to generate PNG: {e}")
            return False
    
    def generate_mermaid_png(self, output_path: str):
        """Standard Architecture Diagram (backward compatibility)."""
        return self.render_mermaid_code_to_png(self.generate_mermaid_diagram(), output_path)
        
    def generate_context_diagram(self) -> str:
        """Fixed Context Diagram: Uses brackets and quotes to avoid 503/400 errors."""
        inputs = []
        outputs = []
        for node in self.graph.nodes():
            if not node.startswith("FILE:"): continue
            if self.graph.in_degree(node) == 0: inputs.append(node)
            if self.graph.out_degree(node) == 0: outputs.append(node)

        lines = ["graph LR"]
        lines.append("    subgraph Core[Internal System]")
        lines.append("        Logic[\"Business Logic\"]")
        lines.append("    end")
        
        for i in inputs[:7]:
            clean_id = self._sanitize_node(i)
            # Use [" "] for ALL labels to escape dots and hyphens
            label = i.replace('FILE:', '')
            lines.append(f"    {clean_id}[\"{label}\"] --> Logic")
        for o in outputs[:7]:
            clean_id = self._sanitize_node(o)
            label = o.replace('FILE:', '')
            lines.append(f"    Logic --> {clean_id}[\"{label}\"]")
        return "\n".join(lines)

    async def generate_hierarchical_structure_json(self, max_edges: int = 100) -> Dict[str, Any]:
        """
        New Function: Leverages LLM to transform raw graph data into 
        hierarchical Business Domain and Process Flow JSON.
        """
        try:
            # 1. Extract significant edges from the NetworkX graph for context
            edges_summary = []
            # We focus on execution and data access for high-level structure
            target_types = {'CALL', 'TRIGGER', 'EXEC_PGM', 'ACCESS_DB', 'SUBMIT_JOB'}
            
            count = 0
            for u, v, d in self.graph.edges(data=True):
                if d.get('type') in target_types:
                    edges_summary.append(f"{u} -> {v} ({d.get('type')})")
                    count += 1
                if count >= max_edges:
                    break

            if not edges_summary:
                logger.warning("Graph is empty or contains no relevant relationship types for JSON extraction.")
                return {}

            dependency_map_str = "\n".join(edges_summary)

            # 2. Prepare the Prompt
            prompt = STRUCTURE_EXTRACTION_PROMPT.format(
                dependency_map=dependency_map_str
            )

            # 3. Call LLM
            llm = get_llm()
            response = await llm.ainvoke([HumanMessage(content=prompt)])
            
            # 4. Parse and Validate JSON
            raw_content = response.content.strip()
            # Remove markdown code blocks if present
            clean_content = re.sub(r'```json|```', '', raw_content).strip()
            
            data = json.loads(clean_content)
            
            logger.info("Successfully generated hierarchical structure JSON via LLM.")
            return data

        except json.JSONDecodeError as e:
            logger.error(f"LLM returned invalid JSON for hierarchical structure: {e}")
            return {}
        except Exception as e:
            logger.error(f"Failed to generate hierarchical JSON: {e}")
            return {}
    
    # Inside GraphAnalyzer class
    def generate_visual_graph_from_json(self, data: Dict, output_path: str):
        """
        Renders a stylized JPG/PNG from hierarchical JSON using NetworkX/Matplotlib.
        Handles both 'phases' (Process Flow) and 'domains' (Architecture).
        """
        plt.switch_backend('Agg')  # Required for server environments without GUI
        G = nx.DiGraph()
        node_types = {}

        if "phases" in data:
            for phase in sorted(data["phases"], key=lambda x: x.get("sequence", 0)):
                p_name = phase["phase_name"]
                G.add_node(p_name)
                node_types[p_name] = "phase"
                for step in phase.get("steps", []):
                    s_id = step["step_id"]
                    G.add_node(s_id)
                    node_types[s_id] = "step"
                    G.add_edge(p_name, s_id)
                    for comp in step.get("technical_components", []):
                        G.add_node(comp)
                        node_types[comp] = "component"
                        G.add_edge(s_id, comp)
            title = "Process Flow Graph"

        elif "domains" in data:
            for domain in data["domains"]:
                d_name = domain["domain_name"]
                G.add_node(d_name)
                node_types[d_name] = "domain"
                for group in domain.get("groups", []):
                    g_name = group["group_name"]
                    G.add_node(g_name)
                    node_types[g_name] = "group"
                    G.add_edge(d_name, g_name)
                    for subgroup in group.get("subgroups", []):
                        sg_name = subgroup["subgroup_name"]
                        G.add_node(sg_name)
                        node_types[sg_name] = "subgroup"
                        G.add_edge(g_name, sg_name)
                        for comp in subgroup.get("components", []):
                            G.add_node(comp)
                            node_types[comp] = "component"
                            G.add_edge(sg_name, comp)
            title = "Architecture Hierarchy Graph"
        else:
            return False

        # Styling logic
        plt.figure(figsize=(16, 14))
        pos = nx.spring_layout(G, k=0.7, iterations=100, seed=42)
        node_colors = []
        node_sizes = []

        for node in G.nodes():
            ntype = node_types.get(node)
            if ntype in ["phase", "domain"]:
                node_colors.append("purple" if ntype=="phase" else "red")
                node_sizes.append(4000)
            elif ntype in ["step", "group"]:
                node_colors.append("orange" if ntype=="step" else "gold")
                node_sizes.append(3000)
            elif ntype == "subgroup":
                node_colors.append("green")
                node_sizes.append(2200)
            else:
                node_colors.append("skyblue")
                node_sizes.append(1200)

        nx.draw_networkx_nodes(G, pos, node_size=node_sizes, node_color=node_colors, edgecolors="black", linewidths=1.5)
        nx.draw_networkx_edges(G, pos, arrows=True, arrowstyle='-|>', width=1.5, alpha=0.6)
        nx.draw_networkx_labels(G, pos, font_size=8, font_weight="bold", font_color="white", 
                                bbox=dict(facecolor="black", alpha=0.6, edgecolor="none"))

        plt.title(title, fontsize=16)
        plt.axis("off")
        plt.savefig(output_path, format="JPG", dpi=300, bbox_inches="tight")
        plt.close()
        return True


    # def generate_batch_flow_diagram(self, summaries: List[Any]) -> str:
    #     """
    #     Optimized Batch Flow Generator.
    #     Limits nodes and edges to avoid 'URL Too Long' errors.
    #     Prioritizes central processing jobs over housekeeping.
    #     """
    #     import re
    #     import networkx as nx
        
    #     # 1. Setup temporary graph for analysis
    #     temp_nx = nx.DiGraph()
        
    #     def normalize_dsn(dsn):
    #         return re.sub(r'\(.*?\)', '', str(dsn)).strip().upper()

    #     jcl_summaries = [s for s in summaries if s.file_type == 'JCL']
        
    #     # Filter out 'Noise' jobs (Housekeeping/Utilities)
    #     noise_patterns = ['SORT', 'IDCAMS', 'BR14', 'GENER', 'DB2UTIL', 'CLEANUP', 'BACKUP']
    #     filtered_jcls = [
    #         s for s in jcl_summaries 
    #         if not any(noise in s.filename.upper() for noise in noise_patterns)
    #     ]

    #     # 2. Build Internal Graph to identify the 'Important' nodes
    #     dataset_producers = {}
    #     for s in filtered_jcls:
    #         io_list = s.technical_analysis.get('io_datasets', [])
    #         for ds in io_list:
    #             if not isinstance(ds, dict): continue
    #             usage = str(ds.get('usage', '')).upper()
    #             dsn = normalize_dsn(ds.get('dataset', ''))
    #             if any(k in usage for k in ['NEW', 'WRITE', 'OUTPUT', 'CATLG']) and dsn:
    #                 if 'TEMP' not in dsn and '&&' not in dsn:
    #                     dataset_producers[dsn] = s.filename

    #     # Add edges to temp_nx based on Dataset Handoffs and flow_context
    #     for s in filtered_jcls:
    #         # Add from flow_context (Predecessors/Successors)
    #         flow = s.technical_analysis.get('flow_context', {})
    #         preds = flow.get('predecessors', []) or []
    #         for p in preds:
    #             p_name = str(p).split(' ')[0].upper().replace('.JCL', '')
    #             temp_nx.add_edge(p_name, s.filename.upper().replace('.JCL', ''))

    #         # Add from Dataset Handoffs
    #         io_list = s.technical_analysis.get('io_datasets', [])
    #         for ds in io_list:
    #             if not isinstance(ds, dict): continue
    #             if any(k in str(ds.get('usage', '')).upper() for k in ['OLD', 'SHR', 'INPUT']):
    #                 dsn = normalize_dsn(ds.get('dataset', ''))
    #                 producer = dataset_producers.get(dsn)
    #                 if producer:
    #                     temp_nx.add_edge(producer.upper().replace('.JCL', ''), s.filename.upper().replace('.JCL', ''))

    #     if temp_nx.number_of_nodes() == 0:
    #         return ""

    #     # 3. CAP THE GRAPH (Complexity Management)
    #     # Keep only the top 25 nodes based on their connectivity (degree)
    #     top_nodes = sorted(temp_nx.degree, key=lambda x: x[1], reverse=True)[:25]
    #     nodes_to_keep = [n[0] for n in top_nodes]
    #     subgraph = temp_nx.subgraph(nodes_to_keep)

    #     # 4. Generate Mermaid with Shortened IDs to save URL space
    #     lines = ["graph TD"]
    #     lines.append("    classDef job fill:#e1f5fe,stroke:#01579b,stroke-width:2px;")
        
    #     # ID map to keep Mermaid IDs extremely short (e.g., j1, j2)
    #     id_map = {name: f"j{i}" for i, name in enumerate(subgraph.nodes())}
        
    #     for u, v in subgraph.edges():
    #         u_id, v_id = id_map[u], id_map[v]
    #         # [ID] is the label shown in the box
    #         lines.append(f"    {u_id}[\"{u}\"] --> {v_id}[\"{v}\"]")
    #         lines.append(f"    class {u_id},{v_id} job")

    #     # 5. Length Safety Valve
    #     mermaid_code = "\n".join(lines)
    #     if len(mermaid_code) > 3000: # Raw code length check
    #         # If still too long, return even more limited version
    #         return "graph TD\n    Warning[\"Batch Flow too complex to render\"]"
            
    #     return mermaid_code

    def generate_batch_flow_diagram(self, summaries: List[Any]) -> str:
        """
        Optimized Batch Flow Generator.
        1. Removes self-references.
        2. Uses flowchart TD for straighter arrows.
        3. Maintains URL length safety via node capping.
        """
        import re
        import networkx as nx
        
        # 1. Setup temporary graph for analysis
        temp_nx = nx.DiGraph()
        
        def normalize_dsn(dsn):
            return re.sub(r'\(.*?\)', '', str(dsn)).strip().upper()

        jcl_summaries = [s for s in summaries if s.file_type == 'JCL']
        
        # Filter out 'Noise' jobs (Housekeeping) to keep diagram clean
        noise_patterns = ['CLEANUP', 'BACKUP', 'BR14', 'IDCAMS']
        filtered_jcls = [
            s for s in jcl_summaries 
            if not any(noise in s.filename.upper() for noise in noise_patterns)
        ]

        # 2. Build Dataset Handoff Map
        dataset_producers = {}
        for s in filtered_jcls:
            io_list = s.technical_analysis.get('io_datasets', [])
            for ds in io_list:
                if not isinstance(ds, dict): continue
                usage = str(ds.get('usage', '')).upper()
                dsn = normalize_dsn(ds.get('dataset', ''))
                if any(k in usage for k in ['NEW', 'WRITE', 'OUTPUT', 'CATLG']) and dsn:
                    if 'TEMP' not in dsn and '&&' not in dsn:
                        dataset_producers[dsn] = s.filename

        # 3. Build Internal Graph
        for s in filtered_jcls:
            curr_job = s.filename.upper().replace('.JCL', '')
            
            # Layer A: flow_context
            flow = s.technical_analysis.get('flow_context', {})
            preds = flow.get('predecessors', []) or []
            for p in preds:
                p_name = str(p).split(' ')[0].upper().replace('.JCL', '')
                # FILTER: Remove Self-Reference
                if p_name != curr_job:
                    temp_nx.add_edge(p_name, curr_job)

            # Layer B: Dataset Handoffs
            io_list = s.technical_analysis.get('io_datasets', [])
            for ds in io_list:
                if not isinstance(ds, dict): continue
                if any(k in str(ds.get('usage', '')).upper() for k in ['OLD', 'SHR', 'INPUT']):
                    dsn = normalize_dsn(ds.get('dataset', ''))
                    producer = dataset_producers.get(dsn)
                    if producer:
                        p_name = producer.upper().replace('.JCL', '')
                        # FILTER: Remove Self-Reference
                        if p_name != curr_job:
                            temp_nx.add_edge(p_name, curr_job)

        if temp_nx.number_of_nodes() == 0:
            return ""

        # 4. CAP COMPLEXITY (URL Length Safety)
        top_nodes = sorted(temp_nx.degree, key=lambda x: x[1], reverse=True)[:25]
        nodes_to_keep = [n[0] for n in top_nodes]
        subgraph = temp_nx.subgraph(nodes_to_keep)

        # 5. Generate Mermaid Flowchart (TD + Optimized IDs)
        # Using 'flowchart TD' instead of 'graph TD' for straighter lines
        lines = ["flowchart TD"]
        
        # classDef ensures boxes are consistent size, making arrows straighter
        lines.append("    classDef job fill:#e1f5fe,stroke:#01579b,stroke-width:2px,color:#333;")
        
        # ID map to keep Mermaid IDs short for URL limits
        id_map = {name: f"j{i}" for i, name in enumerate(subgraph.nodes())}
        
        # Add Edges
        for u, v in subgraph.edges():
            u_id, v_id = id_map[u], id_map[v]
            # Use " " for labels to handle potential dots/special chars
            lines.append(f"    {u_id}[\"{u}\"] --> {v_id}[\"{v}\"]")
            lines.append(f"    class {u_id},{v_id} job")

        # 6. Final Code Assembly
        mermaid_code = "\n".join(lines)
        
        # Safety check: If still too massive, return a warning node
        if len(mermaid_code) > 4000:
            return "flowchart TD\n    Warn[\"Batch Graph too complex to render\"]"
            
        return mermaid_code