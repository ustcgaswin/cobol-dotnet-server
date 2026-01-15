import networkx as nx
from loguru import logger
from pathlib import Path
import os

class DependencyMapper:
    def __init__(self):
        self.graph = nx.DiGraph()

    def build_map(self, all_parsed_jsons: list):
        """
        Builds a directed graph representing the mainframe project structure.
        Uses a two-pass approach to ensure all nodes exist before drawing edges.
        """
        logger.info(f"Step 1: Building Graph Nodes from {len(all_parsed_jsons)} parsed entities...")
        
        # This map bridges 'FSMAIN' (called in JCL) to 'FSMAIN.cbl' (the actual file)
        member_to_node_id = {} 

        # --- PASS 1: Add Nodes and Create Member Lookup ---
        for data in all_parsed_jsons:
            # Priority list for node naming
            node_id = (
                data.get('source_file') or 
                data.get('program_name') or 
                data.get('module_name') or 
                data.get('table_name') or
                data.get('job_name') or
                (data.get('meta') and data.get('meta').get('source_file')) or
                (data.get('meta') and data.get('meta').get('file')) or
                (data.get('metadata') and data.get('metadata').get('source'))
            )

            if not node_id:
                logger.warning(f"Skipping entity: No valid identifier found in JSON keys. Keys: {list(data.keys())}")
                continue

            # Identify the File Type (normalized to lowercase)
            file_type = str(
                data.get('file_type') or 
                data.get('language') or 
                data.get('module_type') or
                "unknown"
            ).lower()

            # Normalize the Member Name (e.g., "FSMAIN.cbl" -> "FSMAIN")
            member_name = Path(node_id).stem.split('.')[0].upper()
            member_to_node_id[member_name] = node_id

            # Add node to graph
            self.graph.add_node(node_id, type=file_type, data=data, member=member_name)
            logger.debug(f"Node Created: [{file_type.upper()}] {node_id} (Member: {member_name})")

        # --- PASS 2: Connect Edges ---
        logger.info("Step 2: Drawing Edges (Connections) between components...")
        
        for node_id, attr in self.graph.nodes(data=True):
            data = attr['data']
            f_type = attr['type']
            member = attr.get('member')

            # 1. JCL -> Program/Proc Links
            if f_type == 'jcl':
                # Check both top-level 'dependencies' and 'steps'
                exec_list = data.get('dependencies', [])
                for exec_item in exec_list:
                    target_member = exec_item.get('name', '').upper()
                    self._create_smart_edge(node_id, target_member, member_to_node_id, 'executes')

            # 2. Logic Files (COBOL/PLI/ASM) -> Copybooks/Tables
            if f_type in ['cobol', 'pli', 'hlasm', 'assembler_module']:
                deps = data.get('dependencies', {})

                # Copybook Links
                for copy in deps.get('copybooks', []):
                    cp_member = copy.get('copybook_name', '').upper()
                    self._create_smart_edge(node_id, cp_member, member_to_node_id, 'uses_copybook')

                # SQL Table Links (DCLGEN)
                for sql in deps.get('sql_queries', []):
                    if sql.get('type') == 'INCLUDE':
                        # Extract table name from "EXEC SQL INCLUDE TABLE_NAME"
                        raw_stmt = sql.get('statement', '').upper()
                        table_member = raw_stmt.replace('EXEC SQL INCLUDE', '').replace('END-EXEC', '').strip()
                        if table_member:
                            self._create_smart_edge(node_id, table_member, member_to_node_id, 'references_table')

            # 3. CA-7 -> JCL Links
            if f_type == 'ca7':
                # CA7 files often contain multiple jobs
                jobs = data.get('jobs', []) if isinstance(data.get('jobs'), list) else []
                for job in jobs:
                    job_name = job.get('job_name', '').upper()
                    # Mainframe logic: Job Name usually corresponds to a JCL member
                    self._create_smart_edge(node_id, job_name, member_to_node_id, 'triggers')

        logger.success(f"Graph Built: {self.graph.number_of_nodes()} Nodes, {self.graph.number_of_edges()} Edges.")
        return self.graph

    def _create_smart_edge(self, source_id, target_member, lookup_map, relation):
        """
        Creates an edge. If the member name exists in our project, link to the full filename.
        If not, create a placeholder node marked as 'external'.
        """
        if not target_member:
            return

        if target_member in lookup_map:
            target_id = lookup_map[target_member]
            self.graph.add_edge(source_id, target_id, relation=relation)
            logger.debug(f"Edge Added: {source_id} --({relation})--> {target_id}")
        else:
            # This is a call to a system utility or a program we don't have source for
            self.graph.add_node(target_member, type='external', data={})
            self.graph.add_edge(source_id, target_member, relation=f"{relation}_external")
            logger.trace(f"External Edge: {source_id} points to missing member {target_member}")