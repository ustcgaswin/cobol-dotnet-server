import os
import networkx as nx
from typing import Dict, List, Any
from datetime import datetime

class ReportGenerator:
    def __init__(self, output_dir: str, mapper_graph: nx.DiGraph, llm_manager: Any):
        """
        Args:
            output_dir: Where to save the final MD.
            mapper_graph: The NetworkX graph built by DependencyMapper.
            llm_manager: The manager used to call LLM for synthesis.
        """
        self.output_dir = output_dir
        self.graph = mapper_graph
        self.llm = llm_manager
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

    def create_consolidated_report(self, program_reports: Dict[str, Any]):
        """
        Creates the 'Master_Project_Manual.md' for the entire project.
        """
        print("Generating Consolidated Master Report...")
        
        # Start building the Master Markdown
        report_lines = [
            f"# PROJECT ARCHITECTURE & LOGIC MANUAL",
            f"**Generated on:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
            f"\n---\n",
            "## TABLE OF CONTENTS",
            "1. [System-Level Overview](#1-system-level-overview)",
            "2. [System Topology & Dependency Map](#2-system-topology--dependency-map)",
            "3. [Data Lineage (Files & Databases)](#3-data-lineage-files--databases)",
            "4. [Program Specifications](#4-program-specifications)",
            "\n---\n"
        ]

        # 1. SECTION: SYSTEM OVERVIEW
        print("Synthesizing System Overview...")
        report_lines.append("## 1. SYSTEM-LEVEL OVERVIEW")
        report_lines.append(self._generate_system_summary())
        report_lines.append("\n---\n")

        # 2. SECTION: SYSTEM ARCHITECTURE (Deterministic)
        print("Generating Dependency Map...")
        report_lines.append("## 2. SYSTEM TOPOLOGY & DEPENDENCY MAP")
        report_lines.append("The following table represents the structural relationships between JCL Jobs, Programs, and Support Modules.")
        report_lines.append(self._generate_dependency_table())
        report_lines.append("\n---\n")

        # 3. SECTION: DATA LINEAGE
        print("Extracting Data Lineage...")
        report_lines.append("## 3. DATA LINEAGE (FILES & DATABASES)")
        report_lines.append(self._generate_data_lineage_section())
        report_lines.append("\n---\n")

        # 4. SECTION: FILE-BY-FILE SPECIFICATIONS
        print("Compiling individual Program Specifications...")
        report_lines.append("## 4. PROGRAM SPECIFICATIONS")
        report_lines.append(self._generate_file_specifications(program_reports))

        # Final Write
        file_path = os.path.join(self.output_dir, "Master_Project_Manual.md")
        with open(file_path, "w", encoding="utf-8") as f:
            f.write("\n".join(report_lines))
        
        print(f"Master Manual successfully generated at: {file_path}")
        return file_path

    def _generate_system_summary(self) -> str:
        """
        Uses LLM to synthesize an intro based on the project skeleton.
        """
        # Create a compact summary of the graph for the LLM
        nodes_summary = []
        for node, data in self.graph.nodes(data=True):
            node_type = data.get('type', 'unknown')
            nodes_summary.append(f"- {node} ({node_type})")

        prompt = f"""
        You are a Senior Systems Architect. Below is a list of components in a legacy mainframe system.
        Analyze the components and provide a 3-paragraph executive summary describing the system's 
        likely business domain and architectural flow.
        
        System Components:
        {chr(10).join(nodes_summary[:100])} 
        
        Return the summary in professional Markdown.
        """
        # Call your LLM manager's invoke method
        response = self.llm.generate_raw_text(prompt)
        return response

    def _generate_dependency_table(self) -> str:
        """
        Deterministic Markdown Table from NetworkX Graph.
        """
        table = "| Source Component | Relationship | Target Component | Type |\n"
        table += "| :--- | :--- | :--- | :--- |\n"
        
        edges = sorted(self.graph.edges(data=True), key=lambda x: x[0])
        for u, v, data in edges:
            v_type = self.graph.nodes[v].get('type', 'Internal')
            rel = data.get('relation', 'references').replace('_', ' ').capitalize()
            table += f"| {u} | {rel} | {v} | {v_type} |\n"
        
        return table

    def _generate_data_lineage_section(self) -> str:
        """
        Lists all Files and DB tables and which programs touch them.
        """
        lineage = "### Physical Data Dependencies\n"
        lineage += "| Data Entity | Type | Accessed By |\n"
        lineage += "| :--- | :--- | :--- |\n"

        # Group access by data entity
        data_nodes = [n for n, d in self.graph.nodes(data=True) if d.get('type') in ['dclgen', 'copybook', 'flat_file']]
        
        for entity in sorted(data_nodes):
            # Find programs that point to this entity
            parents = [u for u, v in self.graph.in_edges(entity)]
            e_type = self.graph.nodes[entity].get('type', 'file').upper()
            lineage += f"| {entity} | {e_type} | {', '.join(parents) if parents else 'Direct Access'} |\n"
            
        return lineage

    def _generate_file_specifications(self, program_reports: Dict[str, Any]) -> str:
        """
        Iterates through all Pydantic ProgramReports and creates the deep-dive section.
        """
        spec_content = []
        
        for prog_id, report in sorted(program_reports.items()):
            spec_content.append(f"### {prog_id}")
            spec_content.append(f"**Business Purpose:** {report.business_purpose}")
            
            # Sub-section: Metadata
            spec_content.append(f"\n- **Technical Type:** {report.language.upper()}")
            spec_content.append(f"- **External Calls:** {', '.join(report.external_dependencies) if report.external_dependencies else 'None'}")
            
            # Sub-section: Paragraph Logic (The 10k line breakdown)
            spec_content.append(f"\n#### Logic Breakdown")
            spec_content.append("| Section / Paragraph | Logic Type | Business Description |")
            spec_content.append("| :--- | :--- | :--- |")
            
            for para in report.paragraph_summaries:
                # We normalize logic type for the table
                l_type = para.logic_type if para.logic_type else "General"
                spec_content.append(f"| {para.paragraph_name} | {l_type} | {para.business_logic} |")
            
            # Sub-section: Risks/Warnings (Found by LLM)
            if hasattr(report, 'potential_risk_areas') and report.potential_risk_areas:
                spec_content.append(f"\n> **Developer Notes:** {', '.join(report.potential_risk_areas)}")
            
            spec_content.append("\n---\n") # Separator between programs
            
        return "\n".join(spec_content)