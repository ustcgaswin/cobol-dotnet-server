# from typing import List, Any
# from pathlib import Path
# from reportlab.lib.pagesizes import A4
# from reportlab.lib.units import mm
# from reportlab.platypus import SimpleDocTemplate, Spacer, PageBreak, Paragraph
# from .renderers import BaseBuilder

# class SingleJCLReportBuilder(BaseBuilder):
#     """
#     Generates a combined Functional & Technical Runbook for a single JCL Job.
#     Links the Job steps directly to the COBOL/PLI business logic.
#     """
#     def __init__(self, target_jcl: str, *args, **kwargs):
#         super().__init__(*args, **kwargs)
#         # Normalize target name
#         self.target_jcl = target_jcl.upper().replace('.JCL', '').replace('.TXT', '').strip()
#         self.title_text = f"Job Specification: {self.target_jcl}"

#     def build(self):
#         # 1. Locate the specific JCL summary
#         jcl_summary = next((s for s in self.summaries 
#                             if s.file_type == 'JCL' and 
#                             s.filename.upper().replace('.JCL', '').replace('.TXT', '') == self.target_jcl), None)
        
#         if not jcl_summary:
#             self.h1("Error: Job Not Found")
#             self.para(f"The job <b>{self.target_jcl}</b> was not found in the analyzed project artifacts.")
#             return

#         # 2. Executive Summary for the Job
#         self._render_job_header(jcl_summary)
        
#         # 3. Technical Step-by-Step Breakdown
#         self._render_step_analysis(jcl_summary)
        
#         # 4. Data Dependencies (Predecessors/Successors)
#         self._render_lineage(jcl_summary)

#     def _render_job_header(self, s):
#         self.h1(f"Job Specification: {s.filename}")
        
#         self.h2("1. Business Purpose")
#         self.para(s.business_overview.get('purpose', "Purpose not defined."))

#         self.h2("2. Business Category")
#         self.para(f"Category: <b>{s.business_overview.get('functional_category', 'Batch Process')}</b>")
        
#         self.h2("3. Processing Constraints")
#         scope = s.business_overview.get('scope', [])
#         if scope:
#             self.bullet_list(scope)
#         else:
#             self.para("No specific business rules or constraints identified for this job.")

#     def _render_step_analysis(self, s):
#         self.h1("4. Technical Execution Roadmap")
        
#         # 4.1 Job Card Details
#         header = s.technical_analysis.get('job_header', {})
#         self.h2("4.1 Job Configuration")
#         rows = [
#             ["Job Name", header.get('job_name', s.filename)],
#             ["Execution Class", header.get('class', 'N/A')],
#             ["Owner", header.get('owner', 'N/A')],
#             ["Region", header.get('region_limit', 'N/A')]
#         ]
#         self.table(["Attribute", "Value"], rows, [60*mm, 110*mm])

#         # 4.2 Steps Breakdown
#         self.h2("4.2 Processing Steps & Logic")
#         steps = s.technical_analysis.get('steps', [])
        
#         if not steps:
#             self.para("No execution steps identified in JCL.")
#             return

#         for step in steps:
#             step_name = step.get('step_name', 'UNKNOWN')
#             program = step.get('program', 'UNKNOWN')
            
#             self.h3(f"Step: {step_name} (Executing: {program})")
#             self.para(f"<b>Step Action:</b> {step.get('description', 'N/A')}")
            
#             # --- LINK TO PROGRAM LOGIC ---
#             # Try to find the COBOL/PLI/REXX module summary
#             prog_summary = next((ps for ps in self.summaries if ps.filename.upper() == program.upper()), None)
            
#             if prog_summary:
#                 # 1. Functional Logic
#                 desc = prog_summary.business_overview.get('functional_description')
#                 if desc:
#                     self.para(f"<b>Program Logic:</b> {desc}")
                
#                 # 2. Business Rules/Scope
#                 rules = prog_summary.business_overview.get('scope', [])
#                 if rules:
#                     self.h4("Applicable Business Rules")
#                     self.bullet_list(rules)

#                 # 3. Calculations (From our Taxonamy update)
#                 calcs = prog_summary.business_overview.get('core_calculations', [])
#                 if calcs:
#                     self.h4("Calculations & Formulas")
#                     self.bullet_list(calcs)
                
#                 # 4. Reconciliation (From our Balance & Control update)
#                 recon = prog_summary.business_overview.get('reconciliation_logic', [])
#                 if recon:
#                     self.h4("Data Integrity & Reconciliation")
#                     self.bullet_list(recon)
#             else:
#                 self.para(f"<i>Note: Source logic for {program} is external or was not provided in this scope.</i>")

#             # --- IO MAPPINGS FOR THIS STEP ---
#             io_list = step.get('io_mappings', [])
#             if io_list:
#                 self.h4(f"I/O Dataset Mappings for {step_name}")
#                 io_rows = []
#                 for io in io_list:
#                     io_rows.append([
#                         io.get('dd_name', 'N/A'),
#                         io.get('dataset', 'N/A'),
#                         io.get('disposition', 'N/A'),
#                         io.get('purpose', 'N/A')
#                     ])
#                 self.table(["DD Name", "Physical Dataset / Resource", "Disp", "Role"], io_rows, [25*mm, 80*mm, 15*mm, 50*mm])
            
#             self.elements.append(Spacer(1, 5*mm))

#     def _render_lineage(self, s):
#         self.h1("5. Dependencies & Flow")
        
#         flow = s.technical_analysis.get('flow_context', {})
        
#         # We can draw the predecessors/successors here as tables or bullets
#         self.h2("5.1 External Data Entrances")
#         ext_in = flow.get('external_inputs', [])
#         if ext_in:
#             self.bullet_list(ext_in)
#         else:
#             self.para("No external upstream data sources identified.")

#         self.h2("5.2 Upstream Job Dependencies")
#         preds = flow.get('predecessors', [])
#         if preds:
#             self.bullet_list(preds)
#         else:
#             self.para("This job appears to be a starter or has no internal predecessors.")

#         self.h2("5.3 Downstream Job Dependencies")
#         succs = flow.get('successors', [])
#         if succs:
#             self.bullet_list(succs)
#         else:
#             self.para("This job appears to be a terminator or has no internal successors.")

#     def save(self, path: str):
#         """Overrides BaseBuilder.save to remove the Title Page and TOC."""
#         doc = SimpleDocTemplate(
#             path, pagesize=A4,
#             rightMargin=20*mm, leftMargin=20*mm,
#             topMargin=20*mm, bottomMargin=20*mm
#         )
#         # Build the PDF using only the self.elements list (No TOC/TitlePage logic)
#         try:
#             doc.build(self.elements)
#         except Exception as e:
#             # Handle empty element cases
#             print(f"Failed to save JCL spec: {e}")

from typing import List, Any, Dict
from pathlib import Path
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import mm
from reportlab.platypus import SimpleDocTemplate, Spacer, Paragraph
from .renderers import BaseBuilder

class SingleJCLReportBuilder(BaseBuilder):
    """
    A specialized builder for a Single JCL Runbook.
    Combines JCL orchestration with the internal logic of the programs it calls.
    """
    
    def __init__(self, jcl_summary: Any, summaries: List[Any], metrics: Any, graph_analyzer: Any):
        # We pass an empty dict for system_summary as this is a file-level report
        super().__init__(summaries=summaries, metrics=metrics, graph_analyzer=graph_analyzer, system_summary={})
        
        self.jcl = jcl_summary
        self.all_summaries = summaries
        self.title_text = f"Job Specification: {self.jcl.filename}"

    def build(self):
        """Main orchestrator for the Runbook layout."""
        
        # 1. Functional Summary
        self._section_functional_overview()
        
        # 2. Technical Configuration
        self._section_technical_config()
        
        # 3. Execution Roadmap (The Core)
        self._section_execution_steps()
        
        # 4. Data Lineage & Flow
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

    def _section_technical_config(self):
        """Extracts Job Card and Header details."""
        self.h2("4. Technical Configuration")
        
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
            self.h3("4.1 Symbolic Parameters")
            param_rows = [[p.get('name'), p.get('default_value'), p.get('description')] for p in params if isinstance(p, dict)]
            if param_rows:
                self.table(["Variable", "Default", "Description"], param_rows, [40*mm, 40*mm, 90*mm])

    def _section_execution_steps(self):
        """Analyzes every JCL step and links to Program logic."""
        self.h2("5. Execution Roadmap")
        self.para("This section details the sequential processing steps, the programs executed, and their internal business logic.")

        steps = self.jcl.technical_analysis.get('steps', [])
        if not steps:
            self.para("<i>No processing steps identified in the JCL source.</i>")
            return

        for idx, step in enumerate(steps, 1):
            step_name = step.get('step_name', f"STEP{idx}")
            program = step.get('program', 'UNKNOWN')
            
            self.h3(f"Step {idx}: {step_name} (PGM: {program})")
            self.para(f"<b>Step Description:</b> {step.get('description', 'N/A')}")
            
            # --- DEEP LINK: Find the Program Summary ---
            prog_summary = self._find_summary_by_name(program)
            
            if prog_summary:
                self.h3(f"Internal Logic: {program}")
                
                # 1. Narrative Logic
                logic_desc = prog_summary.business_overview.get('functional_description')
                if logic_desc:
                    self.para(logic_desc)

                # 2. Business Rules (Specific to program)
                prog_rules = prog_summary.business_overview.get('scope', [])
                if prog_rules:
                    self.h4("Processing Rules")
                    self.bullet_list(prog_rules)

                # 3. Calculations (BA Taxonomy)
                calcs = prog_summary.business_overview.get('core_calculations', [])
                if calcs:
                    self.h4("Core Calculations")
                    self.bullet_list(calcs)

                # 4. Integrity / Reconciliation (Balance & Control)
                recons = prog_summary.business_overview.get('reconciliation_logic', [])
                if recons:
                    self.h4("Integrity & Reconciliation Controls")
                    self.bullet_list(recons)
            else:
                self.para(f"<i>Note: Internal logic for '{program}' is managed externally or via system utility.</i>")

            # --- I/O Mappings for this step ---
            io_mappings = step.get('io_mappings', [])
            if io_mappings:
                self.h4("Data Interactions")
                io_rows = []
                for io in io_mappings:
                    if not isinstance(io, dict): continue
                    io_rows.append([
                        io.get('dd_name', 'N/A'),
                        io.get('dataset', 'N/A'),
                        io.get('disposition', 'N/A'),
                        io.get('purpose', 'N/A')
                    ])
                if io_rows:
                    self.table(["DD Name", "Dataset / Resource", "Disp", "Role"], io_rows, [25*mm, 85*mm, 15*mm, 45*mm])

            self.elements.append(Spacer(1, 5*mm))

    def _section_flow_dependencies(self):
        """Workflow lineage (Predecessors/Successors)."""
        self.h2("6. Dependencies & Data Lineage")
        
        flow = self.jcl.technical_analysis.get('flow_context', {})
        
        # 1. External Entrances
        ext_in = flow.get('external_inputs', [])
        if ext_in:
            self.h3("6.1 External Data Entrances")
            self.bullet_list(ext_in)

        # 2. Predecessors
        preds = flow.get('predecessors', [])
        if preds:
            self.h3("6.2 Upstream Job Dependencies")
            self.para("This job requires data produced by the following upstream processes:")
            self.bullet_list(preds)

        # 3. Successors
        succs = flow.get('successors', [])
        if succs:
            self.h3("6.3 Downstream Job Dependencies")
            self.para("The following jobs rely on outputs generated by this process:")
            self.bullet_list(succs)

        # 4. Recovery
        recovery = self.jcl.technical_analysis.get('restart_and_recovery', {})
        if recovery and isinstance(recovery, dict):
            self.h3("6.4 Restart & Recovery Procedures")
            self.para(f"<b>Recommended Restart Point:</b> {recovery.get('restart_point', 'Step 01')}")
            self.para(f"<b>Cleanup Actions:</b> {recovery.get('cleanup_requirements', 'Standard Scratch/Catalog')}")

    def _find_summary_by_name(self, name: str):
        """Helper to find a COBOL/PLI summary in the result set by program name."""
        if not name or name.upper() in ['SORT', 'IDCAMS', 'IEFBR14', 'IEBGENER', 'IKJEFT01', 'ICETOOL']:
            return None
            
        clean_name = name.upper().strip()
        for s in self.all_summaries:
            # Check if summary filename matches (ignoring extensions)
            s_base = s.filename.upper().split('.')[0]
            if s_base == clean_name:
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
            # We skip the Title Page logic and directly render the elements
            doc.build(self.elements)
        except Exception as e:
            from loguru import logger
            logger.error(f"Failed to build Single JCL PDF: {e}")