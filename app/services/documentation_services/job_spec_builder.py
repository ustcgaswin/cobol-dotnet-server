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
from reportlab.platypus import SimpleDocTemplate, Spacer, Paragraph, Image as RLImage
from .renderers import BaseBuilder
from reportlab.lib.utils import ImageReader
import re

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
        self.MAX_IMG_WIDTH = 165 * mm
        self.MAX_IMG_HEIGHT = 230 * mm 

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
        # self._section_data_structures()
        
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
    
    def _clean_for_mermaid(self, text: str) -> str:
        """Removes HTML tags and special characters that break Mermaid API."""
        if not text: return ""
        # Remove any existing HTML tags like <b> or <br/>
        clean = re.sub(r'<[^>]*>', '', text)
        # Remove characters that cause syntax errors in Mermaid labels
        clean = clean.replace('"', "'").replace('(', '[').replace(')', ']')
        return clean.strip()

    # def _section_visual_step_flow(self):
    #     """Generates a wrapped grid layout (5 steps per row) for the JCL flow."""
    #     self.h2("4. Job Execution Flow")
    #     steps = self.jcl.technical_analysis.get('steps', [])
    #     if not steps:
    #         self.para("No execution steps identified.")
    #         return

    #     STEPS_PER_ROW = 5
    #     mermaid_lines = ["flowchart TD"]
    #     mermaid_lines.append("    classDef stepNode fill:#e1f5fe,stroke:#01579b,stroke-width:1px,color:#333,font-size:12px;")
        
    #     rows = [steps[i:i + STEPS_PER_ROW] for i in range(0, len(steps), STEPS_PER_ROW)]
    #     last_node_id = None

    #     for row_idx, row_steps in enumerate(rows):
    #         # Using subgraphs to keep rows together
    #         mermaid_lines.append(f"    subgraph Row_{row_idx} [ ]")
    #         mermaid_lines.append("        direction LR")
            
    #         row_node_ids = []
    #         for step_idx, step in enumerate(row_steps):
    #             global_idx = (row_idx * STEPS_PER_ROW) + step_idx
    #             node_id = f"s{global_idx}"
    #             s_name = self._clean_for_mermaid(step.get('step_name', f"STP{global_idx}"))
    #             pgm = self._clean_for_mermaid(step.get('program', 'UTIL'))
    #             label = f"{s_name} : {pgm}"
                
    #             mermaid_lines.append(f"        {node_id}[\"{label}\"]")
    #             mermaid_lines.append(f"        class {node_id} stepNode")
    #             row_node_ids.append(node_id)

    #         for i in range(len(row_node_ids) - 1):
    #             mermaid_lines.append(f"        {row_node_ids[i]} --> {row_node_ids[i+1]}")
            
    #         mermaid_lines.append("    end") 

    #         if last_node_id and row_node_ids:
    #             # Vertical link between rows
    #             mermaid_lines.append(f"    {last_node_id} --> {row_node_ids[0]}")
            
    #         if row_node_ids:
    #             last_node_id = row_node_ids[-1]

    #     mermaid_code = "\n".join(mermaid_lines)
    #     img_path = self.output_dir / f"{self.jcl.filename}_flow.png"
        
    #     if self.graph_analyzer.render_mermaid_code_to_png(mermaid_code, str(img_path)):
    #         # --- FIX: SMART SCALING LOGIC ---
    #         self._add_scaled_image(str(img_path))
    #         self.para("<i>Figure 1: Sequential step execution roadmap.</i>")
    #     else:
    #         self.para("<i>[Visual flow could not be rendered]</i>")
    def _section_visual_step_flow(self):
        """
        Generates a Lineage Flow: 
        Predecessors --> Current Job (Highlighted) --> Successors
        """
        self.h2("2. Job Lineage & Connectivity")
        
        # 1. Extract context
        flow = self.jcl.technical_analysis.get('flow_context', {})
        preds = flow.get('predecessors', []) or []
        succs = flow.get('successors', []) or []
        current_name = self._clean_for_mermaid(self.jcl.filename)

        # If there's absolutely no connectivity, show a simple block
        if not preds and not succs:
            self.para("This job has no identified upstream or downstream job dependencies.")
            return

        # 2. Build Mermaid String
        mermaid_lines = ["flowchart LR"]
        
        # Styling: Normal Jobs (Blue) vs Current Job (Gold/Orange)
        mermaid_lines.append("    classDef normalJob fill:#e1f5fe,stroke:#01579b,stroke-width:1px,color:#333;")
        mermaid_lines.append("    classDef targetJob fill:#fff9c4,stroke:#fbc02d,stroke-width:3px,color:#333,font-weight:bold;")

        # Define the Main Target Node
        target_id = "target_node"
        mermaid_lines.append(f"    {target_id}[\"{current_name}\"]")
        mermaid_lines.append(f"    class {target_id} targetJob")

        # Handle Predecessors (Upstream)
        if preds:
            for idx, p in enumerate(preds[:10]): # Limit to 10 to keep URL short
                p_name = self._clean_for_mermaid(str(p))
                p_id = f"pred_{idx}"
                mermaid_lines.append(f"    {p_id}[\"{p_name}\"] --> {target_id}")
                mermaid_lines.append(f"    class {p_id} normalJob")
        else:
            # Optional: Show a "Batch Entry" start node if no preds
            mermaid_lines.append(f"    Start((Start)) --> {target_id}")

        # Handle Successors (Downstream)
        if succs:
            for idx, s in enumerate(succs[:10]):
                s_name = self._clean_for_mermaid(str(s))
                s_id = f"succ_{idx}"
                mermaid_lines.append(f"    {target_id} --> {s_id}[\"{s_name}\"]")
                mermaid_lines.append(f"    class {s_id} normalJob")
        else:
            # Optional: Show an "End" node if no successors
            mermaid_lines.append(f"    {target_id} --> End((End))")

        mermaid_code = "\n".join(mermaid_lines)
        
        # 3. Render and Add to PDF
        img_path = self.output_dir / f"{self.jcl.filename}_lineage_flow.png"
        if self.graph_analyzer.render_mermaid_code_to_png(mermaid_code, str(img_path)):
            self._add_scaled_image(str(img_path))
            self.para("<i>Figure 1: Upstream dependencies and downstream impact for this job.</i>")
        else:
            # Fallback if the API fails
            self.para("<i>[Lineage diagram unavailable - see Section 8 for text details]</i>")

    def _add_scaled_image(self, path: str):
        """Helper to add image to PDF while ensuring it fits on the page."""
        try:
            img_reader = ImageReader(path)
            iw, ih = img_reader.getSize()
            aspect = ih / float(iw)

            # Initial target based on width
            target_w = self.MAX_IMG_WIDTH
            target_h = target_w * aspect

            # If the resulting height is too big, scale down based on height
            if target_h > self.MAX_IMG_HEIGHT:
                target_h = self.MAX_IMG_HEIGHT
                target_w = target_h / aspect

            img_flowable = RLImage(path, width=target_w, height=target_h)
            img_flowable.hAlign = 'CENTER'
            self.elements.append(img_flowable)
            self.elements.append(Spacer(1, 5*mm))
        except Exception as e:
            from loguru import logger
            logger.error(f"Image scaling failed: {e}")

    def _section_input_data(self):
        """Aggregates all DISP=SHR/OLD datasets across all steps."""
        self.h2("5. Input Data Requirements")
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
        self.h2("6. Output Artifacts & Reports")
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
        self.h2("7. Technical Configuration")
        
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
            self.h3("7.1 Symbolic Parameters")
            param_rows = [[p.get('name'), p.get('default_value'), p.get('description')] for p in params if isinstance(p, dict)]
            if param_rows:
                self.table(["Variable", "Default", "Description"], param_rows, [40*mm, 40*mm, 90*mm])

    # def _section_execution_steps(self):
    #     """Analyzes every JCL step and links to Program logic."""
    #     self.h2("8. Execution Roadmap")
    #     self.para("Detailed breakdown of sequential processing steps and the internal program logic applied.")

    #     steps = self.jcl.technical_analysis.get('steps', [])
    #     if not steps:
    #         self.para("<i>No processing steps identified.</i>")
    #         return

    #     for idx, step in enumerate(steps, 1):
    #         step_name = step.get('step_name', f"STEP{idx}")
    #         program = step.get('program', 'UNKNOWN')
            
    #         self.h3(f"Step {idx}: {step_name} (PGM: {program})")
    #         self.para(f"<b>Step Action:</b> {step.get('description', 'N/A')}")
            
    #         prog_summary = self._find_summary_by_name(program)
            
    #         if prog_summary:
    #             self.h4(f"Internal Logic: {program}")
    #             logic_desc = prog_summary.business_overview.get('functional_description')
    #             if logic_desc: self.para(logic_desc)

    #             prog_rules = prog_summary.business_overview.get('scope', [])
    #             if prog_rules:
    #                 self.para("<b>Processing Rules:</b>")
    #                 self.bullet_list(prog_rules)

    #             calcs = prog_summary.business_overview.get('core_calculations', [])
    #             if calcs:
    #                 self.para("<b>Core Calculations:</b>")
    #                 self.bullet_list(calcs)

    #             recons = prog_summary.business_overview.get('reconciliation_logic', [])
    #             if recons:
    #                 self.para("<b>Integrity & Balance Controls:</b>")
    #                 self.bullet_list(recons)

    #             tech_notes = prog_summary.technical_analysis.get('technical_notes', [])
    #             if tech_notes:
    #                 self.h4("Technical Implementation Notes")
    #                 self.bullet_list(tech_notes)
    #         else:
    #             self.para(f"<i>Note: Logic for '{program}' is managed via system utility or external module.</i>")

    #         io_mappings = step.get('io_mappings', [])
    #         if io_mappings:
    #             self.h4("Local Step Data Interactions")
    #             io_rows = [[io.get('dd_name'), io.get('dataset'), io.get('disposition'), io.get('purpose')] for io in io_mappings if isinstance(io, dict)]
    #             if io_rows:
    #                 self.table(["DD Name", "Resource", "Disp", "Role"], io_rows, [25*mm, 85*mm, 15*mm, 45*mm])

    #         self.elements.append(Spacer(1, 5*mm))

    def _section_execution_steps(self):
        """Analyzes every JCL step and links to Program logic OR Control logic."""
        self.h2("8. Execution Roadmap")
        self.para("Detailed breakdown of sequential processing steps and the internal logic applied.")

        steps = self.jcl.technical_analysis.get('steps', [])
        if not steps:
            self.para("<i>No processing steps identified.</i>")
            return

        for idx, step in enumerate(steps, 1):
            step_name = step.get('step_name', f"STEP{idx}")
            program = step.get('program', 'UNKNOWN')
            
            self.h3(f"Step {idx}: {step_name} (PGM: {program})")
            self.para(f"<b>Step Action:</b> {step.get('description', 'N/A')}")
            
            logic_summary = self._find_summary_by_name(program)

            if not logic_summary:
                io_mappings = step.get('io_mappings', [])
                for io in io_mappings:
                    dsn = io.get('dataset', '')
                    # Look for a file in our project that matches this DSN or Member name
                    logic_summary = self._find_summary_by_name(dsn)
                    if logic_summary: 
                        break # Found the control file logic

            if logic_summary:
                # Determine title based on type
                stype = logic_summary.file_type.upper()
                label = f"Internal Logic: {logic_summary.filename}"
                if stype in ['CONTROL_CARD', 'PARMLIB', 'TEXT']:
                    label = f"Control/Configuration Logic: {logic_summary.filename}"
                
                self.h4(label)
                
                # Narrative Description
                desc = logic_summary.business_overview.get('functional_description') or \
                       logic_summary.business_overview.get('purpose')
                if desc: self.para(desc)

                # Rules/Processing points
                rules = logic_summary.business_overview.get('scope', [])
                if not rules and stype == 'PARMLIB':
                    # Fallback for parmlibs which store logic in configuration_areas
                    rules = logic_summary.technical_analysis.get('configuration_areas', [])
                
                if rules:
                    self.para("<b>Processing Rules / Configuration:</b>")
                    self.bullet_list(rules)

                # COBOL specific: Calculations & Reconciliation
                calcs = logic_summary.business_overview.get('core_calculations', [])
                if calcs:
                    self.para("<b>Core Calculations:</b>")
                    self.bullet_list(calcs)
                
                recons = logic_summary.business_overview.get('reconciliation_logic', [])
                if recons:
                    self.para("<b>Integrity Controls:</b>")
                    self.bullet_list(recons)

                # Technical Notes
                t_notes = logic_summary.technical_analysis.get('technical_notes', [])
                if t_notes:
                    self.h4("Technical Notes")
                    self.bullet_list(t_notes)
            else:
                self.para(f"<i>Note: Logic for '{program}' is managed via system utility or external module.</i>")

            # Local DD Table
            io_mappings = step.get('io_mappings', [])
            if io_mappings:
                self.h4("Step Data Interactions")
                io_rows = [[io.get('dd_name'), io.get('dataset'), io.get('disposition'), io.get('purpose')] for io in io_mappings if isinstance(io, dict)]
                if io_rows:
                    self.table(["DD Name", "Resource", "Disp", "Role"], io_rows, [25*mm, 85*mm, 15*mm, 45*mm])

            self.elements.append(Spacer(1, 5*mm))

    def _section_flow_dependencies(self):
        """Workflow lineage (Predecessors/Successors)."""
        self.h2("9. Dependencies & Data Lineage")
        
        flow = self.jcl.technical_analysis.get('flow_context', {})
        
        self.h3("9.1 External Data Entrances")
        ext_in = flow.get('external_inputs', [])
        if ext_in: self.bullet_list(ext_in)
        else: self.para("No external upstream data sources identified.")

        self.h3("9.2 Upstream Job Dependencies")
        preds = flow.get('predecessors', [])
        if preds: self.bullet_list(preds)
        else: self.para("No internal predecessors identified.")

        self.h3("9.3 Downstream Job Dependencies")
        succs = flow.get('successors', [])
        if succs: self.bullet_list(succs)
        else: self.para("No internal successors identified.")

    def _find_summary_by_name(self, name: str):
        """
        Aggressive helper to find ANY summary (COBOL, CTL, PARM) by name.
        Handles: Program IDs, Dataset Names, and Member names.
        """
        if not name: return None

        clean_search = name.upper().strip().split('.')[-1].split('(')[-1].replace(')', '').split('.')[0]

        if clean_search in ['SORT', 'IDCAMS', 'IEFBR14', 'IEBGENER', 'IKJEFT01', 'ICETOOL']:
            return None

        for s in self.all_summaries:
            s_filename = s.filename.upper()
            s_base = s_filename.split('.')[0].split('(')[-1].replace(')', '')
            
            if s_base == clean_search or s_base.startswith(clean_search) or clean_search.startswith(s_base):
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