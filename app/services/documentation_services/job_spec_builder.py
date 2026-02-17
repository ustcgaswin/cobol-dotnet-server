# from typing import List, Any, Dict
# from pathlib import Path
# from reportlab.lib.pagesizes import A4
# from reportlab.lib.units import mm
# from reportlab.platypus import SimpleDocTemplate, Spacer, Paragraph
# from .renderers import BaseBuilder

# class SingleJCLReportBuilder(BaseBuilder):
#     """
#     A specialized builder for a Single JCL Runbook.
#     Combines JCL orchestration with the internal logic of the programs it calls.
#     """
    
#     def __init__(self, jcl_summary: Any, summaries: List[Any], metrics: Any, graph_analyzer: Any):
#         # We pass an empty dict for system_summary as this is a file-level report
#         super().__init__(summaries=summaries, metrics=metrics, graph_analyzer=graph_analyzer, system_summary={})
        
#         self.jcl = jcl_summary
#         self.all_summaries = summaries
#         self.title_text = f"Job Specification: {self.jcl.filename}"

#     def build(self):
#         """Main orchestrator for the Runbook layout."""
        
#         # 1. Functional Summary
#         self._section_functional_overview()
        
#         # 2. Technical Configuration
#         self._section_technical_config()
        
#         # 3. Execution Roadmap (The Core)
#         self._section_execution_steps()
        
#         # 4. Data Lineage & Flow
#         self._section_flow_dependencies()

#     def _section_functional_overview(self):
#         """Functional and Business context of the Job."""
#         self.h1(f"Job Specification: {self.jcl.filename}")
        
#         self.h2("1. Business Purpose")
#         purpose = self.jcl.business_overview.get('purpose', "Purpose not defined by analysis.")
#         self.para(purpose)

#         self.h2("2. Functional Classification")
#         category = self.jcl.business_overview.get('functional_category', 'General Batch Processing')
#         self.para(f"This job is classified under: <b>{category}</b>")

#         self.h2("3. Business Rules & Constraints")
#         rules = self.jcl.business_overview.get('scope', [])
#         if rules:
#             self.para("The following business boundaries apply to this job's execution:")
#             self.bullet_list(rules)
#         else:
#             self.para("No specific business constraints identified.")

#     def _section_technical_config(self):
#         """Extracts Job Card and Header details."""
#         self.h2("4. Technical Configuration")
        
#         header = self.jcl.technical_analysis.get('job_header', {})
#         rows = [
#             ["Job Name", header.get('job_name', self.jcl.filename)],
#             ["Execution Class", header.get('class', 'N/A')],
#             ["Owner / Notify", header.get('owner', 'N/A')],
#             ["Region Limit", header.get('region_limit', 'Standard')]
#         ]
#         self.table(["System Attribute", "Value"], rows, [60*mm, 110*mm])
        
#         # Parameters
#         params = self.jcl.technical_analysis.get('symbolic_parameters', [])
#         if params:
#             self.h3("4.1 Symbolic Parameters")
#             param_rows = [[p.get('name'), p.get('default_value'), p.get('description')] for p in params if isinstance(p, dict)]
#             if param_rows:
#                 self.table(["Variable", "Default", "Description"], param_rows, [40*mm, 40*mm, 90*mm])

#     def _section_execution_steps(self):
#         """Analyzes every JCL step and links to Program logic."""
#         self.h2("5. Execution Roadmap")
#         self.para("This section details the sequential processing steps, the programs executed, and their internal business logic.")

#         steps = self.jcl.technical_analysis.get('steps', [])
#         if not steps:
#             self.para("<i>No processing steps identified in the JCL source.</i>")
#             return

#         for idx, step in enumerate(steps, 1):
#             step_name = step.get('step_name', f"STEP{idx}")
#             program = step.get('program', 'UNKNOWN')
            
#             self.h3(f"Step {idx}: {step_name} (PGM: {program})")
#             self.para(f"<b>Step Description:</b> {step.get('description', 'N/A')}")
            
#             # --- DEEP LINK: Find the Program Summary ---
#             prog_summary = self._find_summary_by_name(program)
            
#             if prog_summary:
#                 self.h3(f"Internal Logic: {program}")
                
#                 # 1. Narrative Logic
#                 logic_desc = prog_summary.business_overview.get('functional_description')
#                 if logic_desc:
#                     self.para(logic_desc)

#                 # 2. Business Rules (Specific to program)
#                 prog_rules = prog_summary.business_overview.get('scope', [])
#                 if prog_rules:
#                     self.h4("Processing Rules")
#                     self.bullet_list(prog_rules)

#                 # 3. Calculations (BA Taxonomy)
#                 calcs = prog_summary.business_overview.get('core_calculations', [])
#                 if calcs:
#                     self.h4("Core Calculations")
#                     self.bullet_list(calcs)

#                 # 4. Integrity / Reconciliation (Balance & Control)
#                 recons = prog_summary.business_overview.get('reconciliation_logic', [])
#                 if recons:
#                     self.h4("Integrity & Reconciliation Controls")
#                     self.bullet_list(recons)
#             else:
#                 self.para(f"<i>Note: Internal logic for '{program}' is managed externally or via system utility.</i>")

#             # --- I/O Mappings for this step ---
#             io_mappings = step.get('io_mappings', [])
#             if io_mappings:
#                 self.h4("Data Interactions")
#                 io_rows = []
#                 for io in io_mappings:
#                     if not isinstance(io, dict): continue
#                     io_rows.append([
#                         io.get('dd_name', 'N/A'),
#                         io.get('dataset', 'N/A'),
#                         io.get('disposition', 'N/A'),
#                         io.get('purpose', 'N/A')
#                     ])
#                 if io_rows:
#                     self.table(["DD Name", "Dataset / Resource", "Disp", "Role"], io_rows, [25*mm, 85*mm, 15*mm, 45*mm])

#             self.elements.append(Spacer(1, 5*mm))

#     def _section_flow_dependencies(self):
#         """Workflow lineage (Predecessors/Successors)."""
#         self.h2("6. Dependencies & Data Lineage")
        
#         flow = self.jcl.technical_analysis.get('flow_context', {})
        
#         # 1. External Entrances
#         ext_in = flow.get('external_inputs', [])
#         if ext_in:
#             self.h3("6.1 External Data Entrances")
#             self.bullet_list(ext_in)

#         # 2. Predecessors
#         preds = flow.get('predecessors', [])
#         if preds:
#             self.h3("6.2 Upstream Job Dependencies")
#             self.para("This job requires data produced by the following upstream processes:")
#             self.bullet_list(preds)

#         # 3. Successors
#         succs = flow.get('successors', [])
#         if succs:
#             self.h3("6.3 Downstream Job Dependencies")
#             self.para("The following jobs rely on outputs generated by this process:")
#             self.bullet_list(succs)

#         # 4. Recovery
#         recovery = self.jcl.technical_analysis.get('restart_and_recovery', {})
#         if recovery and isinstance(recovery, dict):
#             self.h3("6.4 Restart & Recovery Procedures")
#             self.para(f"<b>Recommended Restart Point:</b> {recovery.get('restart_point', 'Step 01')}")
#             self.para(f"<b>Cleanup Actions:</b> {recovery.get('cleanup_requirements', 'Standard Scratch/Catalog')}")

#     def _find_summary_by_name(self, name: str):
#         """Helper to find a COBOL/PLI summary in the result set by program name."""
#         if not name or name.upper() in ['SORT', 'IDCAMS', 'IEFBR14', 'IEBGENER', 'IKJEFT01', 'ICETOOL']:
#             return None
            
#         clean_name = name.upper().strip()
#         for s in self.all_summaries:
#             # Check if summary filename matches (ignoring extensions)
#             s_base = s.filename.upper().split('.')[0]
#             if s_base == clean_name:
#                 return s
#         return None

#     def save(self, path: str):
#         """Builds a clean PDF without a Title Page or TOC."""
#         doc = SimpleDocTemplate(
#             path, pagesize=A4,
#             rightMargin=20*mm, leftMargin=20*mm,
#             topMargin=20*mm, bottomMargin=20*mm
#         )
#         try:
#             # We skip the Title Page logic and directly render the elements
#             doc.build(self.elements)
#         except Exception as e:
#             from loguru import logger
#             logger.error(f"Failed to build Single JCL PDF: {e}")

from typing import List, Any, Dict
from pathlib import Path
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import mm
from reportlab.platypus import SimpleDocTemplate, Spacer, Paragraph
from .renderers import BaseBuilder

class SingleJCLReportBuilder(BaseBuilder):
    """
    A specialized builder for a Single JCL Runbook.
    Combines JCL orchestration with the internal logic of the programs it calls,
    consolidated data requirements, and visual flow.
    """
    
    def __init__(self, jcl_summary: Any, summaries: List[Any], metrics: Any, graph_analyzer: Any, output_dir: Path):
        # We pass an empty dict for system_summary as this is a file-level report
        super().__init__(summaries=summaries, metrics=metrics, graph_analyzer=graph_analyzer, system_summary={})
        
        self.jcl = jcl_summary
        self.all_summaries = summaries
        self.output_dir = output_dir # Needed to store the step flow PNG
        self.title_text = f"Job Specification: {self.jcl.filename}"

    def build(self):
        """Main orchestrator for the Runbook layout."""
        
        # 1. Functional Summary (Existing)
        self._section_functional_overview()

        # 2. Step Flow Diagram (NEW)
        self._section_visual_step_flow()

        # 3. Input Data Requirements (NEW)
        self._section_input_data()

        # 4. Output Artifacts & Reports (NEW)
        self._section_output_data()
        
        # 5. Technical Configuration (Existing - Renumbered to 5)
        self._section_technical_config()
        
        # 6. Execution Roadmap (Existing - Renumbered to 6)
        self._section_execution_steps()

        # 7. Data Structure Definitions (NEW)
        self._section_data_structures()
        
        # 8. Data Lineage & Flow (Existing - Renumbered to 8)
        self._section_flow_dependencies()

    def _section_functional_overview(self):
        """Functional and Business context of the Job."""
        self.h1(f"Job Specification: {self.jcl.filename}")
        
        self.h2("1. Business Purpose")
        purpose = self.jcl.business_overview.get('purpose', "Purpose not defined by analysis.")
        self.para(purpose)

        self.h2("2. Functional Classification")
        category = self.jcl.business_overview.get('functional_category', 'General Batch Processing')
        self.para(f"This job is classified under: <b>{category}</b>")

        self.h2("3. Business Rules & Constraints")
        rules = self.jcl.business_overview.get('scope', [])
        if rules:
            self.para("The following business boundaries apply to this job's execution:")
            self.bullet_list(rules)
        else:
            self.para("No specific business constraints identified.")

    def _section_visual_step_flow(self):
        """Generates and renders a sequential flow of JCL steps."""
        self.h2("2. Job Execution Flow")
        steps = self.jcl.technical_analysis.get('steps', [])
        if not steps:
            return

        # Build Mermaid Sequence: Step 1 --> Step 2 --> Step 3
        mermaid_lines = ["flowchart LR"]
        mermaid_lines.append("    classDef stepNode fill:#e1f5fe,stroke:#01579b,stroke-width:2px,color:#333;")
        
        nodes = []
        for i, step in enumerate(steps):
            s_name = step.get('step_name', f"STEP{i+1}")
            pgm = step.get('program', 'UTILITY')
            node_id = f"s{i}"
            nodes.append(node_id)
            # Label with bold step name and program name
            mermaid_lines.append(f"    {node_id}[\"<b>{s_name}</b><br/>{pgm}\"]")
            mermaid_lines.append(f"    class {node_id} stepNode")

        # Connect nodes sequentially
        for i in range(len(nodes) - 1):
            mermaid_lines.append(f"    {nodes[i]} --> {nodes[i+1]}")

        mermaid_code = "\n".join(mermaid_lines)
        
        # Render PNG via GraphAnalyzer's existing utility
        img_path = self.output_dir / f"{self.jcl.filename}_step_flow.png"
        if self.graph_analyzer.render_mermaid_code_to_png(mermaid_code, str(img_path)):
            self.image(str(img_path), width=160*mm)
            self.para("<i>Figure 1: Sequential step execution and program orchestration.</i>")
        else:
            self.para("<i>[Visual flow diagram could not be generated]</i>")

    def _section_input_data(self):
        """Aggregates all DISP=SHR/OLD datasets across all steps."""
        self.h2("3. Input Data Requirements")
        self.para("The following permanent datasets are required as input for the processing steps in this job.")

        input_rows = []
        seen = set()
        
        for step in self.jcl.technical_analysis.get('steps', []):
            for io in step.get('io_mappings', []):
                dsn = io.get('dataset', 'UNKNOWN')
                disp = str(io.get('disposition', '')).upper()
                
                # Input if DISP is SHR or OLD
                is_input = any(x in disp for x in ['SHR', 'OLD'])
                # Filter out temp/work files
                is_work = any(x in dsn.upper() for x in ['SYSUT', 'SORTWK', 'TEMP', '&&'])
                
                if is_input and not is_work and dsn not in seen:
                    input_rows.append([dsn, io.get('purpose', 'Source Data'), step.get('step_name')])
                    seen.add(dsn)

        if input_rows:
            self.table(["Input Dataset / Resource", "Description", "Consuming Step"], input_rows, [85*mm, 50*mm, 35*mm])
        else:
            self.para("No permanent input datasets identified.")

    def _section_output_data(self):
        """Aggregates all DISP=NEW/MOD/CATLG datasets across all steps."""
        self.h2("4. Output Artifacts & Reports")
        self.para("The following files and reports are generated by the execution of this job.")

        output_rows = []
        seen = set()
        
        for step in self.jcl.technical_analysis.get('steps', []):
            for io in step.get('io_mappings', []):
                dsn = io.get('dataset', 'UNKNOWN')
                disp = str(io.get('disposition', '')).upper()
                
                # Output if DISP is NEW, MOD, or CATLG
                is_output = any(x in disp for x in ['NEW', 'MOD', 'CATLG'])
                is_work = any(x in dsn.upper() for x in ['SYSUT', 'SORTWK', 'TEMP', '&&'])

                if is_output and not is_work and dsn not in seen:
                    output_rows.append([dsn, io.get('purpose', 'Target / Report'), step.get('step_name')])
                    seen.add(dsn)

        if output_rows:
            self.table(["Output Dataset / Report", "Result Type", "Generating Step"], output_rows, [85*mm, 50*mm, 35*mm])
        else:
            self.para("No permanent output datasets or reports identified.")

    def _section_technical_config(self):
        """Extracts Job Card and Header details."""
        self.h2("5. Technical Configuration")
        
        header = self.jcl.technical_analysis.get('job_header', {})
        rows = [
            ["Job Name", header.get('job_name', self.jcl.filename)],
            ["Execution Class", header.get('class', 'N/A')],
            ["Owner / Notify", header.get('owner', 'N/A')],
            ["Region Limit", header.get('region_limit', 'Standard')]
        ]
        self.table(["System Attribute", "Value"], rows, [60*mm, 110*mm])
        
        # Parameters
        params = self.jcl.technical_analysis.get('symbolic_parameters', [])
        if params:
            self.h3("5.1 Symbolic Parameters")
            param_rows = [[p.get('name'), p.get('default_value'), p.get('description')] for p in params if isinstance(p, dict)]
            if param_rows:
                self.table(["Variable", "Default", "Description"], param_rows, [40*mm, 40*mm, 90*mm])

    def _section_execution_steps(self):
        """Analyzes every JCL step and links to Program logic."""
        self.h2("6. Execution Roadmap")
        self.para("Detailed breakdown of sequential processing steps and the internal program logic applied.")

        steps = self.jcl.technical_analysis.get('steps', [])
        if not steps:
            self.para("<i>No processing steps identified.</i>")
            return

        for idx, step in enumerate(steps, 1):
            step_name = step.get('step_name', f"STEP{idx}")
            program = step.get('program', 'UNKNOWN')
            
            self.h3(f"Step {idx}: {step_name} (PGM: {program})")
            self.para(f"<b>Step Action:</b> {step.get('description', 'N/A')}")
            
            prog_summary = self._find_summary_by_name(program)
            
            if prog_summary:
                self.h4(f"Internal Logic: {program}")
                logic_desc = prog_summary.business_overview.get('functional_description')
                if logic_desc: self.para(logic_desc)

                prog_rules = prog_summary.business_overview.get('scope', [])
                if prog_rules:
                    self.para("<b>Processing Rules:</b>")
                    self.bullet_list(prog_rules)

                calcs = prog_summary.business_overview.get('core_calculations', [])
                if calcs:
                    self.para("<b>Core Calculations:</b>")
                    self.bullet_list(calcs)

                recons = prog_summary.business_overview.get('reconciliation_logic', [])
                if recons:
                    self.para("<b>Integrity & Balance Controls:</b>")
                    self.bullet_list(recons)
            else:
                self.para(f"<i>Note: Logic for '{program}' is managed via system utility or external module.</i>")

            io_mappings = step.get('io_mappings', [])
            if io_mappings:
                self.h4("Local Step Data Interactions")
                io_rows = [[io.get('dd_name'), io.get('dataset'), io.get('disposition'), io.get('purpose')] for io in io_mappings if isinstance(io, dict)]
                if io_rows:
                    self.table(["DD Name", "Resource", "Disp", "Role"], io_rows, [25*mm, 85*mm, 15*mm, 45*mm])

            self.elements.append(Spacer(1, 5*mm))

    def _section_data_structures(self):
        """Extracts field-level details for layouts used in this job."""
        self.h2("7. Data Structure Definitions")
        self.para("The following record layouts define the data formats processed by the programs in this job.")

        layouts_to_render = []
        seen_layouts = set()

        for step in self.jcl.technical_analysis.get('steps', []):
            pgm = step.get('program')
            prog_summary = self._find_summary_by_name(pgm)
            
            if prog_summary:
                # Iterate through interactions to find mappings (Copybooks)
                interactions = prog_summary.technical_analysis.get('data_interactions', [])
                for io in interactions:
                    if not isinstance(io, dict): continue
                    layout_name = io.get('mapping')
                    if layout_name and layout_name not in seen_layouts:
                        layout_summary = self._find_summary_by_name(layout_name)
                        if layout_summary:
                            layouts_to_render.append(layout_summary)
                            seen_layouts.add(layout_name)

        if not layouts_to_render:
            self.para("<i>No specific file layouts or copybooks identified for this job's datasets.</i>")
            return

        for layout in layouts_to_render:
            self.h3(f"Entity Definition: {layout.filename}")
            self.para(f"<b>Purpose:</b> {layout.business_overview.get('purpose', 'N/A')}")
            
            fields = layout.technical_analysis.get('key_fields', []) or layout.technical_analysis.get('table_structure', [])
            if fields:
                rows = []
                for f in fields:
                    if isinstance(f, dict):
                        # Extract name and description/type
                        name = str(f.get('field') or f.get('column_name') or 'N/A')
                        desc = str(f.get('description') or f.get('type') or 'N/A')
                        rows.append([name, desc])
                if rows:
                    self.table(["Field Name", "Business Meaning / Format"], rows, [70*mm, 100*mm])
            self.elements.append(Spacer(1, 5*mm))

    def _section_flow_dependencies(self):
        """Workflow lineage (Predecessors/Successors)."""
        self.h2("8. Dependencies & Data Lineage")
        
        flow = self.jcl.technical_analysis.get('flow_context', {})
        
        self.h3("8.1 External Data Entrances")
        ext_in = flow.get('external_inputs', [])
        if ext_in: self.bullet_list(ext_in)
        else: self.para("No external upstream data sources identified.")

        self.h3("8.2 Upstream Job Dependencies")
        preds = flow.get('predecessors', [])
        if preds: self.bullet_list(preds)
        else: self.para("No internal predecessors identified.")

        self.h3("8.3 Downstream Job Dependencies")
        succs = flow.get('successors', [])
        if succs: self.bullet_list(succs)
        else: self.para("No internal successors identified.")

    def _find_summary_by_name(self, name: str):
        """Helper to find a COBOL/PLI/COPYBOOK summary by name."""
        if not name or name.upper() in ['SORT', 'IDCAMS', 'IEFBR14', 'IEBGENER']:
            return None
        clean_name = name.upper().strip().split('.')[0]
        for s in self.all_summaries:
            if s.filename.upper().split('.')[0] == clean_name:
                return s
        return None

    def save(self, path: str):
        """Builds a clean PDF without a Title Page or TOC."""
        doc = SimpleDocTemplate(
            path, pagesize=A4,
            rightMargin=20*mm, leftMargin=20*mm,
            topMargin=20*mm, bottomMargin=20*mm
        )
        try:
            doc.build(self.elements)
        except Exception as e:
            from loguru import logger
            logger.error(f"Failed to build Single JCL PDF: {e}")