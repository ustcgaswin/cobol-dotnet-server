# """
# PDF Builders for Master Specifications.
# Uses fpdf2 to generate professional Technical and Functional specs directly.
# Fully implements the v2.0 Table of Contents.
# """

# from typing import List, Dict, Any, Optional
# from fpdf import FPDF
# from app.api.schemas.doc_models import FileSummary, SystemMetrics

# from typing import List, Dict, Any, Optional
# from fpdf import FPDF
# from app.api.schemas.doc_models import FileSummary, SystemMetrics


# class PDFReport(FPDF):
#     def header(self):
#         self.set_draw_color(0, 51, 102) # Deep Blue
#         self.set_line_width(0.5)
#         self.line(10, 10, 200, 10)
#         self.ln(15)

#     def footer(self):
#         self.set_y(-15)
#         self.set_font('helvetica', 'I', 8)
#         self.set_text_color(128, 128, 128)
#         self.cell(0, 10, f'Page {self.page_no()}/{{nb}}', align='C')


# class BaseBuilder:
#     def __init__(self, summaries: List[FileSummary], metrics: SystemMetrics, graph_analyzer):
#         self.pdf = PDFReport()
#         self.pdf.set_auto_page_break(auto=True, margin=15)
#         self.pdf.add_page()
#         self.summaries = summaries
#         self.metrics = metrics
#         self.graph_analyzer = graph_analyzer
#         self.graph_image_path = None 
        
#         # Grouping for easy access
#         self.jcl_files = [s for s in summaries if s.file_type == 'JCL']
#         self.code_files = [s for s in summaries if s.file_type in ['COBOL', 'PLI', 'ASSEMBLY', 'REXX']]
#         self.data_files = [s for s in summaries if s.file_type in ['DCLGEN', 'SQL', 'COPYBOOK', 'PLI_COPYBOOK']]
#         self.configs = [s for s in summaries if s.file_type in ['PARMLIB', 'CONTROL_CARD']]

#     def save(self, path: str):
#         self.pdf.output(path)

#     # --- Formatting Helpers ---
#     def check_page_break(self, height=20):
#         if self.pdf.get_y() + height > 270:
#             self.pdf.add_page()

#     def h1(self, text):
#         self.pdf.add_page()
#         self.pdf.set_font('helvetica', 'B', 16)
#         self.pdf.set_text_color(0, 51, 102)
#         # Reset X to left margin to ensure clean start
#         self.pdf.set_x(self.pdf.l_margin)
#         self.pdf.cell(0, 10, text, new_x="LMARGIN", new_y="NEXT")
#         self.pdf.ln(5)
#         self.pdf.set_text_color(0, 0, 0)

#     def h2(self, text):
#         self.check_page_break(25)
#         self.pdf.set_font('helvetica', 'B', 14)
#         self.pdf.set_text_color(60, 60, 60)
#         self.pdf.set_x(self.pdf.l_margin)
#         self.pdf.cell(0, 10, text, new_x="LMARGIN", new_y="NEXT")
#         self.pdf.ln(2)
#         self.pdf.set_text_color(0, 0, 0)

#     def h3(self, text):
#         self.check_page_break(20)
#         self.pdf.set_font('helvetica', 'B', 12)
#         self.pdf.set_text_color(80, 80, 80)
#         self.pdf.set_x(self.pdf.l_margin)
#         self.pdf.cell(0, 8, text, new_x="LMARGIN", new_y="NEXT")
#         self.pdf.set_text_color(0, 0, 0)

#     def h4(self, text):
#         self.check_page_break(15)
#         self.pdf.set_font('helvetica', 'BI', 11)
#         self.pdf.set_x(self.pdf.l_margin)
#         self.pdf.cell(0, 8, text, new_x="LMARGIN", new_y="NEXT")

#     def para(self, text):
#         self.pdf.set_font('helvetica', '', 11)
#         self.pdf.set_x(self.pdf.l_margin)
#         # FIX: Use epw (Effective Page Width) instead of 0 to prevent "Not enough space" error
#         self.pdf.multi_cell(self.pdf.epw, 6, str(text))
#         self.pdf.ln(2)

#     def bullet(self, text):
#         self.pdf.set_font('helvetica', '', 11)
#         clean_text = str(text).replace('[', '').replace(']', '').replace("'", "")
#         self.pdf.set_x(self.pdf.l_margin)
#         # FIX: Use epw (Effective Page Width) instead of 0
#         self.pdf.multi_cell(self.pdf.epw, 6, f"  - {clean_text}")

#     def image(self, path, w=170):
#         try:
#             if self.pdf.get_y() + 100 > 270: self.pdf.add_page()
#             self.pdf.set_x(self.pdf.l_margin)
#             self.pdf.image(path, w=w)
#             self.pdf.ln(5)
#         except Exception:
#             self.para("[Image Content Not Available]")

#     def table(self, headers: List[str], rows: List[List[str]], col_widths: List[int]):
#         self.check_page_break(30)
#         self.pdf.set_font('helvetica', 'B', 10)
#         self.pdf.set_fill_color(220, 230, 240)
        
#         # Calculate max width if not provided or if mismatch
#         if not col_widths or len(col_widths) != len(headers):
#             # FIX: Use epw instead of manual calculation
#             width = self.pdf.epw / len(headers)
#             col_widths = [width] * len(headers)

#         # Header
#         self.pdf.set_x(self.pdf.l_margin)
#         for i, h in enumerate(headers):
#             self.pdf.cell(col_widths[i], 8, h, border=1, align='C', fill=True)
#         self.pdf.ln()
        
#         # Rows
#         self.pdf.set_font('helvetica', '', 9)
#         self.pdf.set_fill_color(255, 255, 255)
        
#         for row in rows:
#             # Safety check for row length vs header length
#             if len(row) != len(headers):
#                 continue # Skip malformed rows to prevent crash

#             if self.pdf.get_y() + 8 > 270:
#                 self.pdf.add_page()
#                 # Reprint header
#                 self.pdf.set_font('helvetica', 'B', 10)
#                 self.pdf.set_fill_color(220, 230, 240)
#                 self.pdf.set_x(self.pdf.l_margin)
#                 for i, h in enumerate(headers):
#                     self.pdf.cell(col_widths[i], 8, h, border=1, align='C', fill=True)
#                 self.pdf.ln()
#                 self.pdf.set_font('helvetica', '', 9)

#             self.pdf.set_x(self.pdf.l_margin)
#             for i, txt in enumerate(row):
#                 safe_txt = str(txt)[:60]
#                 self.pdf.cell(col_widths[i], 8, safe_txt, border=1)
#             self.pdf.ln()
#         self.pdf.ln(5)

# class TechnicalSpecBuilder(BaseBuilder):
    
#     def build(self):
#         # Title Page
#         self.pdf.add_page()
#         self.pdf.set_y(100)
#         self.pdf.set_font('helvetica', 'B', 24)
#         self.pdf.set_text_color(0, 51, 102)
#         self.pdf.cell(0, 20, 'Technical Specification', new_x="LMARGIN", new_y="NEXT", align='C')
#         self.pdf.set_font('helvetica', '', 16)
#         self.pdf.set_text_color(100, 100, 100)
#         self.pdf.cell(0, 10, 'System Reference Document', new_x="LMARGIN", new_y="NEXT", align='C')
#         self.pdf.set_text_color(0, 0, 0)
        
#         self._render_introduction()
#         self._render_architecture()
#         self._render_batch_execution()
#         self._render_app_logic()
#         self._render_data_spec()
#         self._render_operational_support()
#         self._render_appendices()
        
#         return self.pdf

#     def _render_introduction(self):
#         self.h1("1. Introduction")
#         self.h2("1.1 Purpose")
#         self.para("This document provides a detailed technical reference for the existing system, generated via automated static analysis.")
        
#         self.h2("1.2 Scope of Analysis")
#         rows = [[k, str(v)] for k, v in self.metrics.files_by_type.items()]
#         self.table(["Component Type", "Count"], rows, [100, 50])
        
#         self.h2("1.3 Technology Stack")
#         techs = ", ".join(list(self.metrics.files_by_type.keys()))
#         self.para(f"Identified technologies: {techs}")

#         self.h2("1.4 System Context Diagram")
#         # FIX: Directly show the image if available, else descriptive text
#         if self.graph_image_path:
#             self.image(self.graph_image_path)
#             self.para("Figure 1: System Context and Boundaries")
#         else:
#             self.para("Context diagram could not be generated from the available source code dependencies.")

#         self.h2("1.5 Acronyms & Definitions")
#         glossary = []
#         if self.summaries:
#             for s in self.summaries[:5]:
#                 g_list = s.business_overview.get('glossary', [])
#                 if g_list:
#                     glossary.extend(g_list)
#                     break 
        
#         if glossary:
#             rows = [[g.get('term', ''), g.get('definition', '')] for g in glossary]
#             self.table(["Acronym", "Definition"], rows, [50, 130])
#         else:
#             self.para("No glossary terms identified.")

#     def _render_architecture(self):
#         self.h1("2. System Architecture")
        
#         self.h2("2.1 System Landscape")
#         landscape = "System landscape details unavailable."
#         if self.summaries:
#              landscape = self.summaries[0].business_overview.get('system_landscape') or landscape
#         self.para(landscape)
        
#         self.h2("2.2 Integration Architecture")
        
#         self.h3("Upstream Systems (Inbound)")
#         inputs = []
#         for jcl in self.jcl_files:
#             for ds in jcl.technical_analysis.get('io_datasets', []):
#                 if isinstance(ds, dict) and 'OLD' in str(ds.get('usage', '')).upper():
#                     inputs.append([str(ds.get('dataset', 'Unknown')), jcl.filename])
        
#         if inputs: 
#             unique_inputs = [list(x) for x in set(tuple(x) for x in inputs)]
#             self.table(["Dataset", "Ingested By"], unique_inputs[:10], [100, 80])
#         else:
#             self.para("No upstream feeds identified.")
        
#         self.h3("Downstream Systems (Outbound)")
#         outputs = []
#         for jcl in self.jcl_files:
#             for ds in jcl.technical_analysis.get('io_datasets', []):
#                 if isinstance(ds, dict) and 'NEW' in str(ds.get('usage', '')).upper():
#                     outputs.append([str(ds.get('dataset', 'Unknown')), jcl.filename])
        
#         if outputs: 
#             unique_outputs = [list(x) for x in set(tuple(x) for x in outputs)]
#             self.table(["Dataset", "Generated By"], unique_outputs[:10], [100, 80])
#         else:
#             self.para("No downstream feeds identified.")

#         self.h2("2.3 High-Level Process Flow")
#         if self.graph_image_path:
#             self.image(self.graph_image_path)
#         else:
#             self.para("Process flow diagram generation failed.")

#         self.h2("2.4 Data Flow Architecture")
#         self.para("The system architecture centers around Batch Processing updating DB2/VSAM stores.")

#     def _render_batch_execution(self):
#         self.h1("3. Batch Execution Specification")
        
#         self.h2("3.1 Job Definitions")
#         for jcl in self.jcl_files:
#             self.h3(f"Job: {jcl.filename}")
#             header = jcl.technical_analysis.get('job_header', {})
#             self.para(f"Purpose: {jcl.business_overview.get('purpose', 'N/A')}")
#             self.para(f"Class: {header.get('class', 'N/A')} | Owner: {header.get('owner', 'N/A')}")
            
#             steps = jcl.technical_analysis.get('steps', [])
#             if steps:
#                 data = [[str(s.get('step_name')), str(s.get('program')), str(s.get('description'))[:60]] for s in steps]
#                 self.table(["Step", "Program", "Description"], data, [30, 40, 110])

#         self.h2("3.2 JCL Procedures (PROCs)")
#         procs = [s for s in self.summaries if s.file_type == 'PROC']
#         if procs:
#             for p in procs:
#                 self.h3(f"Procedure: {p.filename}")
#                 self.para(p.business_overview.get('purpose', ''))
#         else:
#             self.para("No JCL Procedures (PROCs) identified in scope.")

#         self.h2("3.3 Job Dependencies (Inferred)")
#         # FIX: Replaced "See Diagram" with actual data list
#         self.para("The following job chains were inferred from dataset dependencies (Job A creates -> Job B reads):")
        
#         chains = []
#         # Simple heuristic for dependency chain visualization in text
#         # This requires analyzing the graph analyzer's edge data if available
#         # Fallback to JCL 'COND' params
#         for jcl in self.jcl_files:
#             tech = jcl.technical_analysis
#             notes = str(tech.get('schedule_notes', ''))
#             if 'COND' in notes or 'DEPEND' in notes:
#                 chains.append([jcl.filename, notes[:80]])
        
#         if chains:
#             self.table(["Job", "Dependency Logic"], chains, [60, 120])
#         else:
#             self.para("No explicit job chains detected via static analysis parameters.")

#         self.h2("3.4 Utility & Control Specifications")
#         if self.configs:
#             for cfg in self.configs:
#                 self.h3(f"Control Member: {cfg.filename}")
#                 self.para(cfg.business_overview.get('purpose', ''))
                
#                 params = cfg.technical_analysis.get('key_parameters', [])
#                 if params:
#                     self.para("Key Logic:")
#                     for param in params:
#                         val = param.get('value') if isinstance(param, dict) else param
#                         self.bullet(str(val))
#         else:
#             self.para("No specific utility control cards found.")

#     def _render_app_logic(self):
#         self.h1("4. Application Logic Specification")
#         self.para("Detailed analysis of core application modules.")
        
#         sorted_code = sorted(self.code_files, key=lambda x: (x.file_type, x.filename))
        
#         for idx, prog in enumerate(sorted_code, 1):
#             self.h3(f"4.1.{idx} {prog.filename} ({prog.file_type})")
            
#             self.h4("Functional Logic")
#             self.para(prog.business_overview.get('purpose', 'N/A'))
#             for rule in prog.business_overview.get('scope', []):
#                 self.bullet(rule)
            
#             self.h4("Call Graph (Inferred)")
#             # FIX: Explicitly list called programs instead of "See Graph"
#             ops = prog.technical_analysis.get('key_operations', [])
#             calls = [op for op in ops if 'CALL' in str(op).upper() or 'LINK' in str(op).upper()]
#             if calls:
#                 for c in calls: self.bullet(c)
#             else:
#                 self.para("No outbound calls detected.")
            
#             self.h4("Key Operations & I/O")
#             if ops:
#                 for op in ops: self.bullet(op)

#             self.h4("Error Handling & Return Codes")
#             notes = prog.technical_analysis.get('technical_notes', [])
#             if notes:
#                 for note in notes: self.bullet(note)
#             else:
#                 self.para("Standard error handling assumed.")
            
#             self.pdf.ln(5)

#     def _render_data_spec(self):
#         self.h1("5. Data Specification")
        
#         self.h2("5.1 Database Schema (DB2)")
#         dclgens = [f for f in self.data_files if f.file_type in ['DCLGEN', 'SQL']]
#         if dclgens:
#             for dcl in dclgens:
#                 self.h3(f"Table: {dcl.technical_analysis.get('table_name', dcl.filename)}")
#                 cols = dcl.technical_analysis.get('table_structure', [])
#                 if cols:
#                     rows = []
#                     for c in cols:
#                         if isinstance(c, dict):
#                             rows.append([str(c.get('column_name')), str(c.get('type')), str(c.get('nullable'))])
#                         else:
#                             rows.append([str(c), "-", "-"])
#                     self.table(["Column", "Type", "Null"], rows, [80, 50, 30])
#         else:
#             self.para("No DCLGEN/SQL definitions found.")

#         self.h2("5.2 File Layouts (Copybooks)")
#         copybooks = [f for f in self.data_files if f.file_type in ['COPYBOOK', 'PLI_COPYBOOK']]
#         if copybooks:
#             for copy in copybooks:
#                 self.h3(f"Layout: {copy.filename}")
#                 fields = copy.technical_analysis.get('table_structure') or copy.technical_analysis.get('key_fields')
#                 if fields:
#                     rows = []
#                     for f in fields:
#                         if isinstance(f, dict):
#                             name = str(f.get('column_name') or f.get('field'))
#                             typ = str(f.get('type') or f.get('description'))
#                             rows.append([name, typ])
#                         else:
#                             rows.append([str(f), "-"])
#                     self.table(["Field Name", "Description"], rows, [90, 90])
#         else:
#             self.para("No Copybooks found.")

#         self.h2("5.3 CRUD Matrix")
#         rows = []
#         for prog in self.code_files:
#             ops = prog.technical_analysis.get('data_interactions', [])
#             for op in ops:
#                 if isinstance(op, dict):
#                     rows.append([prog.filename, str(op.get('target', 'Unknown')), str(op.get('operation', 'Access'))])
        
#         if rows:
#             self.table(["Program", "Table/File", "Operation"], rows[:25], [60, 60, 60])
#         else:
#             self.para("No CRUD operations explicitly detected.")

#     def _render_operational_support(self):
#         self.h1("6. Operational Support & Reliability")
        
#         self.h2("6.1 Restart & Recovery Strategy")
#         restart_jobs = [j.filename for j in self.jcl_files if "RESTART" in str(j.technical_analysis).upper()]
#         if restart_jobs:
#             self.para(f"The following jobs have explicit RESTART logic: {', '.join(restart_jobs)}")
#         else:
#             self.para("No explicit RESTART parameters found in JCL.")

#         self.h2("6.2 Error Handling Framework")
#         self.para("Standard return code (RC) checking is implemented in JCL via COND parameters.")

#         self.h2("6.3 Performance Characteristics")
#         self.para("Analysis of high-volume modules based on I/O frequency.")

#     def _render_appendices(self):
#         self.h1("7. Appendices")
#         self.h2("7.1 File List & Checksums")
#         rows = [[s.filename, s.file_type] for s in self.summaries]
#         self.table(["Filename", "Type"], rows, [100, 60])
        
#         self.h2("7.2 Missing Dependencies Report")
#         self.para("List of called modules not found in source analysis:")
#         self.para("No missing dependencies detected.")


# # =============================================================================
# # 2. FUNCTIONAL SPECIFICATION BUILDER (Full Implementation)
# # =============================================================================

# class FunctionalSpecBuilder(BaseBuilder):

#     def build(self):
#         # Title Page
#         self.pdf.add_page()
#         self.pdf.set_y(100)
#         self.pdf.set_font('helvetica', 'B', 24)
#         self.pdf.set_text_color(0, 51, 102)
#         self.pdf.cell(0, 20, 'Functional Specification', new_x="LMARGIN", new_y="NEXT", align='C')
#         self.pdf.set_font('helvetica', '', 16)
#         self.pdf.set_text_color(100, 100, 100)
#         self.pdf.cell(0, 10, 'System Overview', new_x="LMARGIN", new_y="NEXT", align='C')
#         self.pdf.set_text_color(0, 0, 0)
        
#         self._render_doc_control()
#         self._render_introduction()
#         self._render_functional_flows()
#         self._render_detailed_logic()
#         self._render_interfaces()
#         self._render_operational_functions()
#         self._render_appendices()
        
#         return self.pdf

#     def _render_doc_control(self):
#         self.h1("1. Document Control")
#         self.h2("1.1 Version Control")
#         self.table(["Version", "Date", "Description"], [["1.0", "Auto-Gen", "Initial Draft"]], [30, 40, 110])

#     def _render_introduction(self):
#         self.h1("2. Introduction")
        
#         self.h2("2.1 Business Overview")
#         if self.summaries:
#             self.para(self.summaries[0].business_overview.get('business_purpose', 'Business overview unavailable.'))
#         else:
#             self.para("No summary data available.")

#         self.h2("2.2 System Purpose")
#         self.para("The system facilitates core transaction processing and reporting.")

#         self.h2("2.3 Scope of Current Functionality")
#         if self.summaries:
#             types = list(set([s.file_type for s in self.summaries]))
#             self.para("Included modules cover: " + ", ".join(types))

#         self.h2("2.4 Glossary")
#         glossary = []
#         if self.summaries:
#              for s in self.summaries[:5]:
#                 g = s.business_overview.get('glossary', [])
#                 if g: 
#                     glossary.extend(g)
#                     break
        
#         if glossary:
#             rows = [[g.get('term', ''), g.get('definition', '')] for g in glossary]
#             self.table(["Term", "Definition"], rows, [50, 130])
#         else:
#             self.para("No specific glossary items found.")

#     def _render_functional_flows(self):
#         self.h1("3. Functional Flows")
        
#         self.h2("3.1 High-Level Process Diagram")
#         flow_data = {}
#         if self.summaries:
#             flow_data = self.summaries[0].business_overview.get('functional_flow', {})
        
#         if flow_data.get('description'):
#             self.para(flow_data.get('description'))
        
#         self.h3("Process Interaction Table")
#         steps = flow_data.get('steps_table', [])
        
#         if steps:
#             rows = [[str(s.get('actor')), str(s.get('action')), str(s.get('outcome'))] for s in steps]
#             self.table(["Actor", "Action", "Outcome"], rows, [50, 70, 60])
#         else:
#             rows = []
#             for jcl in self.jcl_files[:5]:
#                 rows.append(["Batch Scheduler", f"Runs {jcl.filename}", "Updates Data"])
#             if rows:
#                 self.table(["Actor", "Action", "Outcome"], rows, [50, 70, 60])
#             else:
#                 self.para("No process flow detected.")

#         self.h2("3.2 Core Functional Groups")
        
#         tx_progs = []
#         rpt_progs = []
#         maint_progs = []

#         for p in self.code_files:
#             raw_text = str(p.technical_analysis) + str(p.business_overview)
#             upper_text = raw_text.upper()
            
#             if any(x in upper_text for x in ['INSERT', 'UPDATE', 'WRITE', 'TRANSACTION', 'CALCULATE']):
#                 tx_progs.append(p.filename)
#             elif any(x in upper_text for x in ['REPORT', 'PRINT', 'DISPLAY', 'EXTRACT', 'READ']):
#                 rpt_progs.append(p.filename)
#             elif any(x in upper_text for x in ['BACKUP', 'ARCHIVE', 'DELETE', 'CLEANUP', 'PURGE']):
#                 maint_progs.append(p.filename)

#         self.h3("3.2.1 Transaction Processing Group")
#         if tx_progs:
#             for p in tx_progs: self.bullet(p)
#         else:
#             self.para("No explicit transaction modules identified.")

#         self.h3("3.2.2 Reporting & Analysis Group")
#         if rpt_progs:
#             for p in rpt_progs: self.bullet(p)
#         else:
#             self.para("No reporting modules identified.")
            
#         self.h3("3.2.3 Data Maintenance Group")
#         if maint_progs:
#             for p in maint_progs: self.bullet(p)
#         else:
#             self.para("No maintenance modules identified.")

#         self.h2("3.3 Reporting & Extraction Process")
#         # FIX: Replaced "See Interface" with actual logic
#         reports = []
#         for jcl in self.jcl_files:
#             for ds in jcl.technical_analysis.get('io_datasets', []):
#                 if isinstance(ds, dict) and ('SYSOUT' in str(ds) or '.RPT' in ds.get('dataset', '')):
#                     reports.append([jcl.filename, str(ds.get('dataset', ''))])
        
#         if reports:
#             self.para("The following jobs perform extraction and reporting:")
#             self.table(["Job", "Output"], reports, [80, 100])
#         else:
#             self.para("No specific reporting processes identified.")

#     def _render_detailed_logic(self):
#         self.h1("4. Detailed Functional Logic")
        
#         self.h2("4.1 Business Rules & Validations")
#         for prog in self.code_files:
#             scope = prog.business_overview.get('scope', [])
#             if scope:
#                 self.h3(f"Module: {prog.filename}")
#                 for rule in scope: self.bullet(rule)

#         self.h2("4.2 Data Management Functions")
        
#         inserts = []
#         updates = []
#         deletes = []
        
#         for prog in self.code_files:
#             ops = str(prog.technical_analysis.get('key_operations', [])).upper()
#             sql = str(prog.technical_analysis.get('data_interactions', [])).upper()
            
#             if 'INSERT' in ops or 'WRITE' in ops or 'INSERT' in sql:
#                 inserts.append(prog.filename)
#             if 'UPDATE' in ops or 'REWRITE' in ops or 'UPDATE' in sql:
#                 updates.append(prog.filename)
#             if 'DELETE' in ops or 'DELETE' in sql:
#                 deletes.append(prog.filename)

#         self.h3("4.2.1 Create/Insert Logic")
#         if inserts:
#             self.para("Programs performing insertions:")
#             for p in inserts: self.bullet(p)
#         else:
#             self.para("No insert logic detected.")

#         self.h3("4.2.2 Update/Maintain Logic")
#         if updates:
#             self.para("Programs performing updates:")
#             for p in updates: self.bullet(p)
#         else:
#             self.para("No update logic detected.")

#         self.h3("4.2.3 Logical Deletion")
#         if deletes:
#             self.para("Programs performing deletions:")
#             for p in deletes: self.bullet(p)
#         else:
#             self.para("No delete logic detected.")

#         self.h2("4.3 Transformations")
#         self.para("Data format conversions identified in COBOL logic.")

#     def _render_interfaces(self):
#         self.h1("5. Interface Specification")
        
#         self.h2("5.1 User Interfaces (Screens)")
#         cics_progs = [p.filename for p in self.code_files if 'CICS' in str(p.technical_analysis).upper()]
#         if cics_progs:
#             self.para(f"Online modules: {', '.join(cics_progs)}")
#         else:
#             self.para("Batch Process Only (No Screens detected).")

#         self.h2("5.2 External Business Dependencies")
#         inputs = []
#         for jcl in self.jcl_files:
#             for ds in jcl.technical_analysis.get('io_datasets', []):
#                 if isinstance(ds, dict) and 'OLD' in str(ds.get('usage', '')).upper():
#                     inputs.append([str(ds.get('dataset')), jcl.filename])
        
#         if inputs: 
#             unique_inputs = [list(x) for x in set(tuple(x) for x in inputs)]
#             self.table(["External File", "Consumed By"], unique_inputs[:15], [100, 60])
#         else:
#             self.para("No external dependencies identified.")

#         self.h2("5.3 Reporting Outputs")
#         outputs = []
#         for jcl in self.jcl_files:
#             for ds in jcl.technical_analysis.get('io_datasets', []):
#                 if isinstance(ds, dict) and ('SYSOUT' in str(ds) or '.RPT' in ds.get('dataset', '')):
#                     outputs.append([str(ds.get('dataset')), jcl.filename])
#         if outputs: 
#             self.table(["Report Name", "Job"], outputs, [100, 60])
#         else:
#             self.para("No reports identified.")

#         self.h2("5.4 Downstream Feeds")
#         feeds = []
#         for jcl in self.jcl_files:
#             for ds in jcl.technical_analysis.get('io_datasets', []):
#                 if isinstance(ds, dict): 
#                     usage = str(ds.get('usage', '')).upper()
#                     name = str(ds.get('dataset', ''))
#                     if 'NEW' in usage and not 'TEMP' in name:
#                         feeds.append([name, jcl.filename])
        
#         if feeds: 
#             self.table(["Output File", "Job"], feeds[:10], [100, 60])
#         else:
#             self.para("No downstream feeds identified.")

#     def _render_operational_functions(self):
#         self.h1("6. Operational Functions")
        
#         self.h2("6.1 Processing Frequencies")
#         freq_rows = []
#         for jcl in self.jcl_files:
#             name = jcl.filename.upper()
#             if 'DLY' in name: freq_rows.append([jcl.filename, "Daily"])
#             elif 'MTH' in name: freq_rows.append([jcl.filename, "Monthly"])
        
#         if freq_rows: 
#             self.table(["Job", "Frequency"], freq_rows, [80, 80])
#         else: 
#             self.para("On-demand / Ad-hoc execution.")

#         self.h2("6.2 Data Volume Capacities")
#         self.para("Inferred from JCL SPACE parameters (CYL vs TRK).")

#         self.h2("6.3 Backup & Recovery Procedures")
#         self.para("Backup jobs identified:")
#         backup_jobs = [j.filename for j in self.jcl_files if 'BACKUP' in str(j.business_overview).upper() or 'IEBGENER' in str(j.technical_analysis)]
#         if backup_jobs:
#             for b in backup_jobs: self.bullet(b)
#         else:
#             self.para("No specific backup jobs found.")

#         self.h2("6.4 Archiving Logic")
#         self.para("Archival steps identified in JCL (GDG Generation).")

#         self.h2("6.5 Error Handling Mechanisms")
#         self.para("Standard batch error reporting via SYSOUT.")

#     def _render_appendices(self):
#         self.h1("7. Appendices")
        
#         self.h2("7.1 Data Dictionary (Business View)")
#         for copy in self.data_files:
#             self.h3(copy.filename)
#             ent = copy.business_overview.get('key_data_entities', [])
#             if ent:
#                 for e in ent: self.bullet(e)
#             else:
#                 self.para("No entities defined.")

#         self.h2("7.2 Report Catalog")
#         # FIX: Full render instead of cross-ref
#         reports = []
#         for jcl in self.jcl_files:
#             for ds in jcl.technical_analysis.get('io_datasets', []):
#                 if isinstance(ds, dict):
#                     name = str(ds.get('dataset', ''))
#                     if 'SYSOUT' in name or '.RPT' in name:
#                         reports.append([jcl.filename, name])
        
#         if reports:
#             self.table(["Job", "Report Name"], reports, [80, 100])
#         else:
#             self.para("No reports identified.")

"""
ReportLab Builders for Master Specifications.
Enterprise-grade PDF generation with automatic layout, wrapping, and ToC.
Matches the v2.0 Templates exactly.
"""

from typing import List, Dict, Any, Union
from reportlab.lib import colors
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import mm, inch
from reportlab.lib.utils import ImageReader
from PIL import Image as PILImage # Use an alias to avoid conflict with ReportLab Image
import html
from pathlib import Path
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, 
    PageBreak, Image, ListFlowable, ListItem
)
from reportlab.platypus.tableofcontents import TableOfContents
from reportlab.lib.enums import TA_JUSTIFY, TA_LEFT, TA_CENTER

from app.api.schemas.doc_models import FileSummary, SystemMetrics
from loguru import logger

class DocTemplateWithTOC(SimpleDocTemplate):
    def __init__(self, filename, **kw):
        super().__init__(filename, **kw)
        self.toc = TableOfContents()
        self.toc.levelStyles = [
            ParagraphStyle(fontName='Helvetica-Bold', fontSize=12, name='TOCHeading1', leftIndent=20, firstLineIndent=-20, spaceBefore=5, leading=16),
            ParagraphStyle(fontName='Helvetica', fontSize=10, name='TOCHeading2', leftIndent=40, firstLineIndent=-20, spaceBefore=2, leading=12),
        ]
        self.story = []

    def afterFlowable(self, flowable):
        """Registers TOC entries automatically based on Heading Styles."""
        if flowable.__class__.__name__ == 'Paragraph':
            style_name = flowable.style.name
            text = flowable.getPlainText()
            if style_name == 'Heading1':
                self.notify('TOCEntry', (0, text, self.page))
            elif style_name == 'Heading2':
                self.notify('TOCEntry', (1, text, self.page))

class BaseBuilder:
    def __init__(self, summaries: List[FileSummary], metrics: SystemMetrics, graph_analyzer, system_summary: Dict = None, images: Dict[str, str] = None):
        self.summaries = summaries
        self.metrics = metrics
        self.system_summary = system_summary or {}
        self.graph_analyzer = graph_analyzer
        self.AVAILABLE_WIDTH = 170 * mm
        # self.title_text = "Mainframe Documentation" # Default
        
        # --- Multi-Image Handling ---
        self.images = images or {}
        self.context_image_path = self.images.get('context')
        self.architecture_image_path = self.images.get('architecture')
        self.process_flow_image_path = self.images.get('process_flow')
        self.functional_image_path = self.images.get('functional')
        self.batch_flow_image_path = self.images.get('batch_flow')
        
        # Data Slicing
        self.jcl_files = [s for s in summaries if s.file_type == 'JCL']
        self.code_files = [s for s in summaries if s.file_type in ['COBOL', 'PLI', 'ASSEMBLY', 'REXX']]
        self.data_files = [s for s in summaries if s.file_type in ['DCLGEN', 'SQL', 'COPYBOOK', 'PLI_COPYBOOK']]
        self.configs = [s for s in summaries if s.file_type in ['PARMLIB', 'CONTROL_CARD']]

        # Styles Setup
        styles = getSampleStyleSheet()
        self.styleN = styles['Normal']
        self.styleN.fontSize = 10
        self.styleN.leading = 12
        
        self.styleH1 = ParagraphStyle(
            'Heading1', parent=styles['Heading1'], 
            fontSize=16, leading=20, spaceBefore=18, spaceAfter=12, 
            textColor=colors.HexColor("#003366")
        )
        self.styleH2 = ParagraphStyle(
            'Heading2', parent=styles['Heading2'], 
            fontSize=13, leading=16, spaceBefore=12, spaceAfter=6, 
            textColor=colors.HexColor("#404040")
        )
        self.styleH3 = ParagraphStyle(
            'Heading3', parent=styles['Heading3'], 
            fontSize=11, leading=14, spaceBefore=10, spaceAfter=4, 
            textColor=colors.HexColor("#606060")
        )

        self.styleH4 = ParagraphStyle(
            'Heading4', parent=styles['Normal'], 
            fontSize=10, leading=12, spaceBefore=6, spaceAfter=2, 
            fontName='Helvetica-BoldOblique'
        )

        self.elements = []
    
    def _clean_text(self, text: Any) -> str:
        """
        Escapes special characters to prevent ReportLab Paragraph XML errors.
        Example: 'a < b' becomes 'a &lt; b'
        """
        if text is None:
            return ""
        return html.escape(str(text))

    def bullet(self, text):
        """Renders a single bullet point (Fixes the AttributeError)."""
        if not text: return
        self.bullet_list([text])

    def bullet_list(self, items: List[str]):
        """Renders a list of items as bullets."""
        if not items: return
        list_items = []
        for item in items:
            # Clean the text before creating Paragraph
            raw_str = str(item).strip("[]'")
            clean_str = self._clean_text(raw_str)
            list_items.append(ListItem(Paragraph(clean_str, self.styleN)))
        self.elements.append(
            ListFlowable(list_items, bulletType='bullet', start='•', leftIndent=15, bulletFontSize=12)
        )
        self.elements.append(Spacer(1, 3*mm))

    def para(self, text):
        if not text: return
        escaped = self._clean_text(text)
        formatted = escaped.replace('&lt;b&gt;', '<b>').replace('&lt;/b&gt;', '</b>')\
                           .replace('&lt;i&gt;', '<i>').replace('&lt;/i&gt;', '</i>')\
                           .replace('\n', '<br/>')
        self.elements.append(Paragraph(formatted, self.styleN))
        self.elements.append(Spacer(1, 2*mm))

    def h1(self, text):
        if self.elements:  # Only add page break if this isn't the first item
            self.elements.append(PageBreak())
        self.elements.append(Paragraph(self._clean_text(text), self.styleH1))

    def h2(self, text):
        self.elements.append(Paragraph(self._clean_text(text), self.styleH2))

    def h3(self, text):
        self.elements.append(Paragraph(self._clean_text(text), self.styleH3))
    
    def h4(self, text):
        self.elements.append(Paragraph(self._clean_text(text), self.styleH4))

    def table(self, headers: List[str], rows: List[List[str]], col_widths=None):
        if not rows:
            self.elements.append(Paragraph("<i>No data available.</i>", self.styleN))
            return
        
        # --- LOGIC TO ENFORCE CONSISTENT TOTAL WIDTH ---
        if not col_widths:
            # If no widths provided, split equally across the available 170mm
            col_widths = [self.AVAILABLE_WIDTH / len(headers)] * len(headers)
        else:
            # If widths are provided, scale them proportionally to ensure total is exactly 170mm
            current_total = sum(col_widths)
            scaling_factor = self.AVAILABLE_WIDTH / current_total
            col_widths = [w * scaling_factor for w in col_widths]
        
        data = [[Paragraph(f"<b>{self._clean_text(h)}</b>", self.styleN) for h in headers]]
        for row in rows:
            data.append([Paragraph(self._clean_text(cell), self.styleN) for cell in row])
        
        # If col_widths not provided, use standard distribution
        if not col_widths:
            col_widths = [170*mm / len(headers)] * len(headers)

        t = Table(data, colWidths=col_widths, repeatRows=1, hAlign='LEFT')
        t.setStyle(TableStyle([
            ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor("#E6E6E6")),
            ('TEXTCOLOR', (0, 0), (-1, 0), colors.black),
            ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
            ('VALIGN', (0, 0), (-1, -1), 'TOP'),
            ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
        ]))
        self.elements.append(t)
        self.elements.append(Spacer(1, 6*mm))

    def image(self, path, width=160*mm):
        if not path or not Path(path).exists():
            return
        try:
            img = Image(path)
            aspect = img.drawHeight / img.drawWidth
            img.drawWidth = width
            img.drawHeight = width * aspect
            self.elements.append(img)
            self.elements.append(Spacer(1, 5*mm))
        except Exception as e:
            logger.error(f"Image error: {e}")

    def save(self, path: str):
        doc = DocTemplateWithTOC(
            path, pagesize=A4,
            rightMargin=20*mm, leftMargin=20*mm,
            topMargin=20*mm, bottomMargin=20*mm
        )
        story = []
        # Title Page
        story.append(Spacer(1, 60*mm))
        story.append(Paragraph(self.title_text, ParagraphStyle('Title', parent=self.styleH1, fontSize=24, alignment=TA_CENTER)))
        story.append(Spacer(1, 10*mm))
        story.append(Paragraph("Project Reference Document", ParagraphStyle('SubTitle', parent=self.styleN, fontSize=16, alignment=TA_CENTER)))
        story.append(PageBreak())
        
        # TOC
        story.append(Paragraph("Table of Contents", self.styleH1))
        story.append(doc.toc)
        story.append(PageBreak())
        
        # Content
        story.extend(self.elements)
        doc.multiBuild(story)

class TechnicalSpecBuilder(BaseBuilder):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.title_text = "Technical Specification"

    def build(self):
        self._intro()
        self._arch()
        self._batch()
        self._logic()
        self._data()
        self._ops()
        self._appx()

    def _intro(self):
        self.h1("1. Introduction")
        self.h2("1.1 Purpose")
        logger.info(f"the purpose : {self.system_summary.get('technical_summary')}")
        self.para(self.system_summary.get('technical_summary', "Technical reference generated via static analysis of the codebase."))
        self.h2("1.2 Scope of Analysis")
        rows = [[k, str(v)] for k, v in self.metrics.files_by_type.items()]
        self.table(["Component Type", "Count"], rows, [100*mm, 50*mm])
        self.h2("1.3 Technology Stack")
        techs = ", ".join(list(self.metrics.files_by_type.keys()))
        self.para(f"Identified technologies: {techs}")
        
        # --- Technical Spec 1.4: Context Diagram ---
        self.h2("1.4 System Context Diagram")
        if self.context_image_path:
            self.image(self.context_image_path)
            self.para("<i>Figure 1: High-level system boundaries and external interfaces.</i>")
        else:
            self.para("Context diagram could not be generated from the available source dependencies.")
        
        # self.h2("1.5 Acronyms & Definitions")
        # glossary = []
        # if self.summaries:
        #     for s in self.summaries[:5]:
        #         g = s.business_overview.get('glossary', [])
        #         if g: glossary.extend(g); break
        # if glossary:
        #     rows = [[g.get('term',''), g.get('definition','')] for g in glossary]
        #     self.table(["Acronym", "Definition"], rows, [40*mm, 130*mm])
        # else:
        #     self.para("No glossary terms identified.")

    def _arch(self):
        self.h1("2. System Architecture")
        self.h2("2.1 System Landscape")
        
        self.para("The following technical landscape has been derived from static analysis of the source artifacts:")
        features = {}
        os_identity = "Unknown Environment"

        has_config = any(c.file_type in ['PARMLIB', 'CONTROL_CARD'] for c in self.configs)
        has_jcl = len(self.jcl_files) > 0
        all_config_text = str([c.technical_analysis for c in self.configs]).upper()
        is_ibm_utility = any(u in all_config_text for u in ['DFSORT', 'IDCAMS', 'IEBGENER', 'IKJEFT', 'DSNUPROC', 'DB2 LOAD'])

        if has_config and is_ibm_utility:
            os_identity = "IBM z/OS (Confirmed via Utility Control Cards)"
        elif has_config:
            os_identity = "IBM z/OS (Inferred from System Config)"
        elif has_jcl:
            os_identity = "IBM Mainframe Compatible (z/OS, VSE, or Re-hosted)"
        elif self.code_files:
            os_identity = "Cross-Platform (COBOL/PLI Runtime)"

        features["Operating System"] = os_identity

        langs = [k for k in self.metrics.files_by_type.keys() if k in ['COBOL', 'PLI', 'ASSEMBLY', 'REXX', 'JCL']]
        features["Programming Languages"] = ", ".join(langs)

        data_layer = []
        if any(f.file_type in ['DCLGEN', 'SQL'] for f in self.data_files) or \
           any('EXEC SQL' in str(f.technical_analysis).upper() for f in self.code_files):
            data_layer.append("IBM DB2 (Relational)")

        vsam_count = 0
        for f in self.code_files:
            if 'VSAM' in str(f.technical_analysis).upper() or \
               'INDEXED' in str(f.technical_analysis).upper():
                vsam_count += 1
        if vsam_count > 0:
            data_layer.append(f"VSAM (KSDS/ESDS, {vsam_count} modules)")
        
        features["Data Persistence"] = ", ".join(data_layer) if data_layer else "Flat Files / Sequential"

        exec_envs = []

        cics_count = sum(1 for f in self.code_files if 'CICS' in str(f.technical_analysis).upper())
        if cics_count > 0:
            exec_envs.append(f"CICS Transaction Server ({cics_count} modules)")
        
        ims_count = sum(1 for f in self.code_files if 'DL/I' in str(f.technical_analysis).upper() or 'IMS' in str(f.technical_analysis).upper())
        if ims_count > 0:
            exec_envs.append(f"IMS Transaction Manager ({ims_count} modules)")

        if self.jcl_files:
            exec_envs.append(f"Batch Processing (JES, {len(self.jcl_files)} jobs)")

        rexx_count = sum(1 for f in self.code_files if f.file_type == 'REXX')
        if rexx_count > 0:
            exec_envs.append(f"TSO/E Interactive ({rexx_count} scripts)")

        if exec_envs:
            features["Execution Environment"] = ", ".join(exec_envs)
        else:
            features["Execution Environment"] = "Standard z/OS Environment"

        middleware = []
        all_code_text = str([f.technical_analysis for f in self.code_files]).upper()
        
        if 'MQ' in all_code_text or 'MQSERIES' in all_code_text: middleware.append("IBM MQ")
        if 'FTP' in all_code_text or 'NDM' in all_code_text: middleware.append("File Transfer (FTP/Connect:Direct)")
        if 'XML' in all_code_text: middleware.append("XML Parsing")
        if 'JSON' in all_code_text: middleware.append("JSON Parsing")
        
        features["Integration & Middleware"] = ", ".join(middleware) if middleware else "File-Based Integration"

        rows = [[k, v] for k, v in features.items()]
        self.table(["Infrastructure Layer", "Detected Components"], rows, [60*mm, 110*mm])

        self.h2("2.2 Integration Architecture")
        self.h3("Upstream Systems")
        inputs = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and 'OLD' in str(ds.get('usage', '')).upper():
                    inputs.append([str(ds.get('dataset', 'Unknown')), jcl.filename])
        if inputs:
            unique = [list(x) for x in set(tuple(x) for x in inputs)]
            self.table(["Dataset", "Ingested By"], unique[:15], [100*mm, 70*mm])
        else:
            self.para("No upstream feeds identified.")

        self.h3("Downstream Systems")
        outputs = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and 'NEW' in str(ds.get('usage', '')).upper():
                    outputs.append([str(ds.get('dataset', 'Unknown')), jcl.filename])
        if outputs:
            unique = [list(x) for x in set(tuple(x) for x in outputs)]
            self.table(["Dataset", "Generated By"], unique[:15], [100*mm, 70*mm])
        else:
            self.para("No downstream feeds identified.")

        # --- Technical Spec 2.3: Process Flow ---
        self.h2("2.3 High-Level Process Flow")
        if self.process_flow_image_path and Path(self.process_flow_image_path).exists():
            self._render_stretched_image(self.process_flow_image_path, "Figure 2: Sequential execution flow of JCL Jobs and batch procedures.", is_architecture=False)
        else:
            self.para("Process flow diagram generation failed or no job chains detected.")
        
        self.h2("2.4 Data Flow Architecture")
        self.para("The following diagram illustrates the data-level dependencies, showing how programs interact with DB2 tables, VSAM files, and GDGs.")
    
        if self.architecture_image_path and Path(self.architecture_image_path).exists():
            # Use existing Architecture Diagram for Data Flow
            self._render_stretched_image(self.architecture_image_path, "Figure 3: System data architecture and component dependency graph.", is_architecture = True)
        else:
            self.para("Data flow architecture diagram unavailable.")
        
    # Private helper to handle the elongation/squeezing for this specific builder
    def _render_stretched_image(self, path, caption, is_architecture=False):
        try:
            from reportlab.platypus import Image as RLImage
            from reportlab.lib.utils import ImageReader
            from PIL import Image as PILImage
            
            if not Path(path).exists():
                return

            final_path = path
            
            # 1. Open image with Pillow to check dimensions
            with PILImage.open(path) as img:
                iw, ih = img.size
                
                if is_architecture and iw > ih:
                    # --- ARCHITECTURE LOGIC: Force Elongation ---
                    # We physically add vertical pixels to prevent the "squashed" look
                    new_width = iw
                    new_height = int(iw * 0.8) # Force a 4:5 vertical-ish ratio
                    
                    img = img.resize((new_width, new_height), PILImage.Resampling.LANCZOS)
                    stretched_path = path.replace(".png", "_stretched.png")
                    img.save(stretched_path)
                    final_path = stretched_path

            # 2. Calculate ReportLab scaling
            img_reader = ImageReader(final_path)
            riw, rih = img_reader.getSize()
            aspect = rih / float(riw)

            # 3. WIDTH LOGIC
            if is_architecture:
                # Architecture fills the page width (170mm)
                target_width = self.AVAILABLE_WIDTH
            else:
                # Process Flow uses original width
                # Convert pixels to mm (standard 96 DPI: 1px ≈ 0.2645mm)
                original_width_mm = riw * 0.2645 * mm
                
                # SAFETY: If original width is wider than the page, cap it at AVAILABLE_WIDTH
                # Otherwise, use the natural size so it doesn't stretch.
                target_width = min(original_width_mm, self.AVAILABLE_WIDTH)
            
            # 4. HEIGHT LOGIC
            target_height = target_width * aspect
            
            # 5. PAGE HEIGHT SAFETY
            # Ensure the image doesn't bleed off the bottom of the A4 page
            max_h = 230 * mm
            if target_height > max_h:
                target_height = max_h
                target_width = target_height / aspect

            # 6. RENDER
            img_flowable = RLImage(final_path, width=target_width, height=target_height)
            img_flowable.hAlign = 'CENTER'
            self.elements.append(img_flowable)
            self.elements.append(Spacer(1, 5*mm))
            self.para(f"<i>{caption}</i>")

        except Exception as e:
            logger.error(f"Image processing failed for {path}: {e}")
            self.para("<i>[Error rendering diagram content]</i>")

    def _batch(self):
        self.h1("3. Batch Execution Specification")
        
        if not self.jcl_files:
            self.para("No JCL/Batch Job artifacts identified in current scope.")
            return
        
        self.h2("3.1 Job Definitions")

        for jcl in self.jcl_files:
            # 3.1 Job Identification
            self.h3(f"Job: {jcl.filename}")
            
            # Overview & Purpose
            self.para(f"<b>Business Purpose:</b> {jcl.business_overview.get('purpose', 'N/A')}")
            
            header = jcl.technical_analysis.get('job_header', {})
            self.para(f"<b>Technical Context:</b> Class {header.get('class', 'N/A')} | Owner: {header.get('owner', 'N/A')} | Region: {header.get('region_limit', 'Default')}")

            # 3.1.1 Symbolic Parameters (JCL Variables)
            sym_params = jcl.technical_analysis.get('symbolic_parameters', [])
            if sym_params:
                self.h4("Symbolic Parameters")
                rows = [[p.get('name'), p.get('default_value'), p.get('description')] for p in sym_params if isinstance(p, dict)]
                self.table(["Parameter", "Default Value", "Functional Description"], rows, [40*mm, 50*mm, 80*mm])

            # 3.1.2 Step-wise Execution & I/O Mapping
            steps = jcl.technical_analysis.get('steps', [])
            if steps:
                self.h3("Step Execution Logic & I/O Mapping")
                for step in steps:
                    s_name = step.get('step_name', 'STEP')
                    prog = step.get('program', 'UNKNOWN')
                    self.para(f"<b>{s_name}</b> (Program: <b>{prog}</b>)")
                    self.para(f"<b><i>Action:</i></b> {step.get('description', 'No description.')}")
                    
                    if step.get('condition_logic'):
                        self.para(f"<b><i>Condition:</i></b>. {step.get('condition_logic')}")

                    # THE GAP FILLER: Step-Level I/O Table
                    io_list = step.get('io_mappings', [])
                    if io_list:
                        io_rows = []
                        for io in io_list:
                            dsn = io.get('dataset', 'Temporary / Inline')
                            # Shorten long DSNs for table fit
                            safe_dsn = dsn[:45] + "..." if len(dsn) > 45 else dsn
                            io_rows.append([
                                io.get('dd_name', 'SYSIN'),
                                safe_dsn,
                                io.get('disposition', 'SHR'),
                                io.get('purpose', 'Work/Data')
                            ])
                        self.table(["DD Name", "Physical Dataset / Resource", "Disp", "Role"], io_rows, [30*mm, 80*mm, 20*mm, 40*mm])

                    # Control Card Logic (e.g., SORT/IDCAMS instructions)
                    if step.get('control_card_summary'):
                        self.para(f"<b>Utility Logic:</b> {step.get('control_card_summary')}")
                    
                    self.elements.append(Spacer(1, 2*mm))

            # 3.1.3 Restart & Recovery Instructions
            recovery = jcl.technical_analysis.get('restart_and_recovery', {})
            if recovery:
                self.h3("Restart & Recovery")
                self.para(f"<b>Restart Point:</b> {recovery.get('restart_point', 'Standard')}")
                if recovery.get('cleanup_requirements'):
                    self.para(f"<b>Cleanup Needed:</b> {recovery.get('cleanup_requirements')}")
                checkpoints = recovery.get('critical_checkpoints', [])
                if checkpoints:
                    self.para(f"<b>Checkpoints:</b> {', '.join(checkpoints)}")

            self.elements.append(Spacer(1, 5*mm))

        # 3.2 JCL Procedures (PROCs)
        self.h2("3.2 JCL Procedures (PROCs)")
        procs = [s for s in self.summaries if s.file_type == 'PROC']
        if procs:
            rows = [[p.filename, p.business_overview.get('purpose', 'Logic snippet')] for p in procs]
            self.table(["Procedure Name", "Encapsulated Logic Description"], rows, [60*mm, 110*mm])
        else:
            self.para("No shared procedures (PROCs) identified.")
        
        # --- NEW: 3.3 JOB DEPENDENCIES (The Execution Chain) ---
        self.h2("3.3 Job Execution Dependencies")
        self.para("The following execution dependencies have been identified via CA7/Control-M definitions and JCL triggers.")
        
        chains = []
        # Extract from the Graph Analyzer
        for u, v, d in self.graph_analyzer.graph.edges(data=True):
            if d.get('type') in ['TRIGGER', 'SUBMIT_JOB']:
                chains.append([u, v, d.get('type', 'Sequential')])

        if chains:
            self.table(["Predecessor Job", "Successor Job", "Relationship Type"], chains[:30], [60*mm, 60*mm, 50*mm])
        else:
            # Fallback to JCL schedule notes if graph is sparse
            notes_rows = []
            for jcl in self.jcl_files:
                notes = jcl.technical_analysis.get('schedule_notes')
                if notes and notes != "N/A":
                    notes_rows.append([jcl.filename, notes])
            
            if notes_rows:
                self.table(["Job Name", "Scheduling / Dependency Notes"], notes_rows, [60*mm, 110*mm])
            else:
                self.para("No cross-job automated dependencies detected in the provided source.")

        # --- NEW: 3.4 UTILITY & CONTROL SPECIFICATIONS ---
        self.h2("3.4 Utility & Control Specifications")
        self.para("This section details the parameters for system utilities (SORT, IDCAMS, DB2 Utilities) found in control cards and parameter libraries.")

        if self.configs:
            for cfg in self.configs:
                self.h3(f"Control Member: {cfg.filename}")
                self.para(f"<b>Associated Utility/Program:</b> {cfg.business_overview.get('title', 'System Utility')}")
                self.para(f"<b>Description:</b> {cfg.business_overview.get('purpose', 'Configuration parameters.')}")

                # Logic Rules (e.g., SORT FIELDS..., DELETE...)
                rules = cfg.technical_analysis.get('configuration_areas', [])
                if rules:
                    self.h4("Configuration Logic")
                    self.bullet_list(rules)

                # Key Parameters (extracted name/value pairs)
                params = cfg.technical_analysis.get('key_parameters', [])
                if params and isinstance(params, list) and isinstance(params[0], dict):
                    p_rows = [[p.get('name', 'N/A'), p.get('value', 'N/A'), p.get('description', 'N/A')] for p in params]
                    self.table(["Parameter", "Value", "Technical Impact"], p_rows, [40*mm, 40*mm, 90*mm])
                
                self.elements.append(Spacer(1, 4*mm))
        else:
            # Check if JCL steps have inline control card summaries
            inline_found = False
            for jcl in self.jcl_files:
                for step in jcl.technical_analysis.get('steps', []):
                    if step.get('control_card_summary'):
                        if not inline_found:
                            self.para("Inline Utility Logic identified within Job Steps:")
                            inline_found = True
                        self.bullet(f"<b>{jcl.filename}</b> <b>({step.get('step_name')}):</b> {step.get('control_card_summary')}")
            
            if not inline_found:
                self.para("No external or inline utility control specifications were identified.")

    def _logic(self):
        self.h1("4. Application Logic Specification")
        self.para("This section details the internal structural components, control flow, and dependencies of application modules.")
        
        sorted_code = sorted(self.code_files, key=lambda x: (x.file_type, x.filename))
        
        for idx, prog in enumerate(sorted_code, 1):
            self.h3(f"4.1.{idx} {prog.filename} ({prog.file_type})")
            
            # 1. Functional Overview
            self.para(f"<b>Purpose:</b> {prog.business_overview.get('purpose', 'N/A')}")
            desc = prog.business_overview.get('functional_description')
            if desc:
                self.para(desc)

            # 2. External Call Hierarchy (Filling the Gap)
            calls = prog.technical_analysis.get('external_calls', [])
            if calls:
                self.h4("External Program Dependencies")
                self.para(f"This module interacts with the following sub-programs: {', '.join(calls)}")

            # 3. Decision Logic & Business Rules (Filling the Gap)
            decisions = prog.technical_analysis.get('logic_decision_points', [])
            if decisions:
                self.h4("Critical Decision Logic")
                self.bullet_list(decisions)

            # 4. Execution Control Flow (FIXED: No double bullets)
            flow = prog.technical_analysis.get('execution_flow', [])
            if flow:
                self.h4("Execution Control Flow")
                # We do NOT use bullet_list here because the LLM provides numbers.
                # We use para() to maintain the numbered format clearly.
                for step in flow:
                    self.para(str(step).strip())

            # 5. Data Interactions (Table)
            interactions = prog.technical_analysis.get('data_interactions', [])
            if interactions:
                self.h4("Data File & Table Access")
                seen_deps = set()
                rows = []
                for i in interactions:
                    if isinstance(i, dict):
                        tgt, op = i.get('target', 'Unknown'), i.get('operation', 'Access')
                    else:
                        tgt, op = str(i), "Access"
                    
                    if f"{tgt}-{op}" not in seen_deps:
                        rows.append([tgt, op])
                        seen_deps.add(f"{tgt}-{op}")
                
                if rows:
                    self.table(["Object / File", "Operation"], rows, [100*mm, 60*mm])

            # 6. Exception Handling
            notes = prog.technical_analysis.get('technical_notes', [])
            if notes:
                self.h4("Technical Operational Notes")
                self.bullet_list(notes)
            
            self.elements.append(Spacer(1, 5*mm))

    def _data(self):
        self.h1("5. Data Specification")
        
        # 5.1 Database Schema (DB2)
        self.h2("5.1 Database Schema (DB2)")
        dclgens = [f for f in self.data_files if f.file_type in ['DCLGEN', 'SQL']]
        if dclgens:
            for dcl in dclgens:
                self.h3(f"Table: {dcl.technical_analysis.get('table_name', dcl.filename)}")
                self.para(f"<b>Domain:</b> {dcl.business_overview.get('data_domain', 'Relational Store')}")
                cols = dcl.technical_analysis.get('table_structure', [])
                if cols:
                    rows = [[str(c.get('column_name')), str(c.get('type')), str(c.get('nullable'))] for c in cols if isinstance(c, dict)]
                    self.table(["Column", "Type", "Null"], rows, [60*mm, 60*mm, 40*mm])
        else:
            self.para("No DB2 relational tables identified.")

        # 5.2 Physical File Stores (VSAM & QSAM) - FILLING THE GAP
        self.h2("5.2 Physical File Specifications")
        file_specs = []
        for copy in self.data_files:
            if copy.file_type in ['COPYBOOK', 'PLI_COPYBOOK']:
                tech = copy.technical_analysis
                if tech.get('storage_type') != 'DB2':
                    file_specs.append([
                        copy.filename,
                        tech.get('storage_type', 'FLAT'),
                        str(tech.get('record_length', 'Variable')),
                        copy.business_overview.get('data_domain', 'Transactional')
                    ])
        
        if file_specs:
            self.table(["File/Layout", "Type", "LRECL", "Domain"], file_specs, [50*mm, 40*mm, 30*mm, 50*mm])
        else:
            self.para("No physical file layouts identified.")

        # 5.3 File Layouts & Keys
        self.h2("5.3 Data Layouts & Key Identifiers")
        for copy in self.data_files:
            fields = copy.technical_analysis.get('key_fields', [])
            if fields:
                self.h3(f"Structure: {copy.filename}")
                rows = []
                for f in fields:
                    if isinstance(f, dict):
                        key_tag = "(Key)" if f.get('is_primary_key') else ""
                        rows.append([f"{f.get('field')} {key_tag}", f.get('description', 'N/A')])
                if rows:
                    self.table(["Field Name", "Business Description"], rows, [70*mm, 100*mm])

        # 5.4 Data Versioning (GDG) - FILLING THE GAP
        self.h2("5.4 Data Versioning & Retention")
        gdg_files = []
        seen_gdg = set()
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict):
                    dsn = ds.get('dataset', '')
                    if '(+' in dsn or '(0)' in dsn:
                        base_dsn = dsn.split('(')[0]
                        if base_dsn not in seen_gdg:
                            gdg_files.append([base_dsn, "Generation Data Group", jcl.filename])
                            seen_gdg.add(base_dsn)
        
        if gdg_files:
            self.para("The following datasets utilize GDG versioning for historical retention:")
            self.table(["Base Dataset Name", "Management Type", "Orchestrated By"], gdg_files, [80*mm, 45*mm, 45*mm])
        else:
            self.para("No versioned (GDG) datasets identified.")

        # 5.5 CRUD Matrix
        self.h2("5.5 Data Access & CRUD Matrix")
        rows = []
        for prog in self.code_files:
            ops = prog.technical_analysis.get('data_interactions', [])
            for op in ops:
                if isinstance(op, dict):
                    rows.append([
                        prog.filename, 
                        str(op.get('target', 'Unknown')), 
                        str(op.get('operation', 'Access')),
                        str(op.get('access_method', 'Sequential'))
                    ])
        if rows:
            # Added Access Method column to the CRUD matrix
            self.table(["Program", "Table/File", "Operation", "Access Method"], rows[:30], [40*mm, 60*mm, 35*mm, 35*mm])

    def _ops(self):
        self.h1("6. Operational Support & Reliability")
        self.h2("6.1 Restart & Recovery")
        restart_rows = []
        for jcl in self.jcl_files:
            tech_text = str(jcl.technical_analysis).upper()
            
            strategy = "Standard Step Restart"
            if "RESTART=" in tech_text:
                strategy = "Explicit Point Restart"
            elif "GDG" in tech_text or "(+" in tech_text:
                strategy = "Data-Level Rollback (GDG)"

            if len(jcl.technical_analysis.get('steps', [])) > 1 or strategy != "Standard Step Restart":
                restart_rows.append([jcl.filename, strategy])

        if restart_rows:
            self.table(
                ["Job Name", "Recovery Strategy / Mechanism"], 
                restart_rows[:30],
                [80*mm, 80*mm]
            )
        else:
            self.para("No specialized restart parameters detected. Standard JES2/JES3 step-level recovery applies.")

        self.h2("6.2 Error Handling Framework")
        self.para("System-wide error handling is managed through JCL Condition Codes (COND) and COBOL Return Codes (RC).")

        error_logic = []
        for jcl in self.jcl_files:
            notes = str(jcl.technical_analysis.get('schedule_notes', '')).upper()
            if 'COND' in notes:
                error_logic.append([jcl.filename, "Conditional Step Execution", "Batch Control"])

        for prog in self.code_files:
            tech = prog.technical_analysis
            notes = str(tech.get('technical_notes', [])).upper()
            if any(k in notes for k in ['ABEND', 'SQLCODE', 'INVALID KEY', 'ERROR-LOG']):
                error_logic.append([prog.filename, "Internal Exception Logic", "Programmatic"])

        if error_logic:
            unique_error = [list(x) for x in set(tuple(x) for x in error_logic)]
            self.table(
                ["Component", "Error Handling Type", "Level"], 
                unique_error[:20], 
                [60*mm, 60*mm, 40*mm]
            )
        else:
            self.para("Standard return code validation (RC=00) is used across all batch modules.")

        self.h2("6.3 Performance Characteristics")
        self.para("The following modules are identified as high-utilization components based on database interaction frequency and code complexity.")

        perf_rows = []
        for mod_str in self.metrics.top_complex_modules:
            name = mod_str.split(' ')[0]
            
            category = "Core Processing"
            for s in self.summaries:
                if s.filename == name:
                    category = s.business_overview.get('functional_category', 'Business Logic')
                    break
            
            perf_rows.append([name, category, "High (Critical Path)"])

        if perf_rows:
            self.table(
                ["Module Name", "Functional Area", "Resource Profile"], 
                perf_rows, 
                [60*mm, 60*mm, 40*mm]
            )
        else:
            self.para("System performance profile appears uniform across analyzed modules.")

    def _appx(self):
        self.h1("7. Appendices")
        self.h2("7.1 File List & Checksums")
        rows = [[s.filename, s.file_type] for s in self.summaries]
        self.table(["Filename", "Type"], rows, [100*mm, 50*mm])
        self.h2("7.2 Missing Dependencies")
        self.para("No missing dependencies detected in current scope.")

class FunctionalSpecBuilder(BaseBuilder):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.title_text = "Functional Specification"

    def build(self):
        self._render_doc_control()         # 1
        self._render_introduction()        # 2
        self._render_functional_flows()    # 3
        self._render_detailed_logic()      # 4
        self._render_interfaces()          # 5
        self._render_ops()                 # 6
        self._render_appendices()          # 7
        
        # return self.pdf

    def _render_doc_control(self):
        self.h1("1. Document Control")
        self.h2("1.1 Version Control")
        self.table(["Version", "Date", "Description"], [["1.0", "Auto-Generated", "Initial Draft"]], [30*mm, 40*mm, 100*mm])

    def _render_introduction(self):
        self.h1("2. Introduction")
        
        # 2.1 Business Overview
        self.h2("2.1 Business Overview")
        # Use the NEW narrative field
        narrative = self.system_summary.get('executive_narrative')
        if narrative:
            # Handle multi-paragraph splits
            for para in narrative.split('\n'):
                if para.strip(): self.para(para.strip())
        else:
            self.para(self.system_summary.get('business_purpose', 'Overview unavailable.'))

        # 2.2 System Purpose (Simplified)
        self.h2("2.2 System Purpose")
        self.para(self.system_summary.get('business_purpose', "The system facilitates core data processing."))

        # 2.3 Scope (Business Focussed)
        self.h2("2.3 Scope of Current Functionality")
        
        # Use the "Key Business Processes" from the System Summary
        processes = self.system_summary.get('key_business_processes', [])
        if processes:
            self.para("The system encompasses the following core business processes:")
            for proc in processes:
                # Render as Text, not Table
                if isinstance(proc, dict):
                    self.h3(proc.get('process_name', 'Process'))
                    self.para(proc.get('description', ''))
        else:
            # Fallback to file counts if LLM failed
            self.para(f"The system is comprised of {len(self.code_files)} processing modules handling batch transactions.")

        # 2.4 Glossary
        self.h2("2.4 Glossary")
        # (Same glossary logic as before - this is fine as a table)
        glossary = self.system_summary.get('harvested_acronyms', []) or self.system_summary.get('glossary', [])
        if glossary:
            rows = [[g.get('term',''), g.get('definition','')] for g in glossary if isinstance(g, dict)]
            self.table(["Term", "Definition"], rows[:30], [40*mm, 130*mm])

    def _render_functional_flows(self):
        self.h1("3. High-Level Functional Flows")
        
        # 3.1 Diagram
        self.h2("3.1 High-Level Process Diagram")
        if self.functional_image_path:
            self.image(self.functional_image_path)
            self.para("<i>Figure: End-to-End Business Data Lifecycle.</i>")

        # 3.2 Groups (Text based)
        self.h2("3.2 Core Functional Groups")
        self.para("The system logic is segmented into the following functional areas:")

        groups = {}
        for p in self.code_files:
            cat = p.business_overview.get('functional_category', 'Uncategorized')
            if cat not in groups: groups[cat] = []
            groups[cat].append(p)

        valid_categories = sorted([k for k in groups.keys() if k != 'Uncategorized'])
            
        if valid_categories:
            for idx, cat in enumerate(valid_categories, 1):
                progs = groups[cat]
                self.h3(f"3.2.{idx} {cat}")
                self.para(f"This group contains {len(progs)} modules responsible for {cat.lower()}.")

                for p in progs:
                    self.bullet(f"{p.filename}: {p.business_overview.get('purpose', 'N/A')}")
        else:
            self.para("No specific functional groups identified.")

        # self.h2("3.3 Batch Execution Flow (Data Lineage)")
        # self.para("The following narratives describe how data moves between batch jobs via shared datasets.")
        # import re
        # def normalize_dsn(dsn):
        #     return re.sub(r'\(.*?\)', '', str(dsn)).strip().upper()

        # producer_map = {}
        # for jcl in self.jcl_files:
        #     steps = jcl.technical_analysis.get('steps', [])
        #     progs = [s.get('program', '') for s in steps if s.get('program')]
        #     prog_str = ", ".join([p for p in progs if p not in ['IEFBR14', 'IEBGENER']][:2]) # Top 2 programs
        #     if not prog_str: prog_str = "System Utility"

        #     for ds in jcl.technical_analysis.get('io_datasets', []):
        #         if not isinstance(ds, dict): continue
        #         usage = str(ds.get('usage', '')).upper()
        #         name = normalize_dsn(str(ds.get('dataset', '')))

        #         if 'TEMP' in name or 'SYSOUT' in name: continue
        #         if 'NEW' in usage or 'WRITE' in usage or 'OUTPUT' in usage:
        #             if name not in producer_map:
        #                 producer_map[name] = {"job": jcl.filename, "progs": prog_str}
        # chains_found = False

        # sorted_jcls = sorted(self.jcl_files, key=lambda x: x.filename)

        # for jcl in sorted_jcls:
        #     job_dependencies = []
            
        #     for ds in jcl.technical_analysis.get('io_datasets', []):
        #         if not isinstance(ds, dict): continue
        #         usage = str(ds.get('usage', '')).upper()
        #         name = normalize_dsn(str(ds.get('dataset', '')))

        #         if 'OLD' in usage or 'READ' in usage or 'INPUT' in usage:
        #             producer = producer_map.get(name)
        #             if producer and producer['job'] != jcl.filename:
        #                 job_dependencies.append({
        #                     "pred_job": producer['job'],
        #                     "pred_logic": producer['progs'],
        #                     "file": name
        #                 })

        #     if job_dependencies:
        #         chains_found = True
        #         self.h4(f"Flow: Inputs for {jcl.filename}")
        #         self.para(f"Job <b>{jcl.filename}</b> relies on data produced by the following upstream processes:")
                
        #         for dep in job_dependencies:
        #             text = (
        #                 f"From <b>{dep['pred_job']}</b> (executing {dep['pred_logic']}): "
        #                 f"Receives dataset {dep['file']}."
        #             )
        #             self.bullet(text)
        #         self.elements.append(Spacer(1, 4*mm))

        # if not chains_found:
        #     self.para("No direct file-based job chains detected. Jobs may operate as independent silos.")

        self.h2("3.2 Batch Execution Flow (Data Lineage)")
        self.para("The following diagram illustrates the sequential flow of data through the batch system based on dataset handoffs.")

        if self.batch_flow_image_path:
             self.image(self.batch_flow_image_path)
             self.para("<i>Figure: Automatic JCL Dependency Graph (Producer-Consumer Analysis).</i>")
        else:
             self.para("No visual batch flow could be generated (no shared datasets found).")

        # self.para("The following narratives describe the end-to-end processing sequence, including external data entrances and internal job-to-job handoffs.")

        # found_any = False

        # for jcl in sorted(self.jcl_files, key=lambda x: x.filename):
        #     flow = jcl.technical_analysis.get('flow_context', {})
            
        #     # 1. Document External Entrances (The "Monthly Master" fix)
        #     ext_in = flow.get('external_inputs', [])
        #     if ext_in:
        #         found_any = True
        #         self.h4(f"External Input: {jcl.filename}")
        #         self.para(f"Job <b>{jcl.filename}</b> initiates processing using the following external sources:")
        #         for item in ext_in:
        #             self.bullet(f"<b>Input Source:</b> {item}")
        #         self.elements.append(Spacer(1, 3*mm))

        #     # 2. Document Internal Handoffs
        #     preds = flow.get('predecessors', [])
        #     if preds:
        #         found_any = True
        #         self.h4(f"Process Chain: {jcl.filename}")
        #         self.para(f"Job <b>{jcl.filename}</b> relies on successful completion of upstream processes:")
                
        #         for pred in preds:
        #             # Look for the purpose of the predecessor job in our summaries list
        #             pred_purpose = "Upstream Data Provider"
        #             for s in self.jcl_files:
        #                 if s.filename == pred:
        #                     pred_purpose = s.business_overview.get('purpose', pred_purpose)
        #                     break
                    
        #             self.bullet(f"<b>Predecessor: {pred}</b> - {pred_purpose}")
        #         self.elements.append(Spacer(1, 3*mm))

        # if not found_any:
        #     self.para("No explicit job-to-job execution chains or external data entrances were identified.")

    def _render_detailed_logic(self):
        self.h1("4. Detailed Functional Logic")
        self.para("This section details the specific business rules enforced by the system components, ordered by their execution sequence in the batch schedule.")

        code_map = {f.filename: f for f in self.code_files}
        
        ordered_files = []
        seen_files = set()

        sorted_jcls = sorted(self.jcl_files, key=lambda x: x.filename)
        
        for jcl in sorted_jcls:
            steps = jcl.technical_analysis.get('steps', [])
            for step in steps:
                pgm_name = step.get('program', 'UNKNOWN')

                if pgm_name in code_map and pgm_name not in seen_files:
                    code_map[pgm_name].execution_context = f"Executed by {jcl.filename} (Step: {step.get('step_name')})"
                    
                    ordered_files.append(code_map[pgm_name])
                    seen_files.add(pgm_name)

        orphans = [f for f in self.code_files if f.filename not in seen_files]
        orphans.sort(key=lambda x: x.filename)
        
        if orphans:
            for f in orphans:
                f.execution_context = "Called dynamically as Subroutine / Utility"
            ordered_files.extend(orphans)

        for prog in ordered_files:
            description = prog.business_overview.get('functional_description')
            rules = prog.business_overview.get('scope', [])

            if not rules:
                rules = prog.technical_analysis.get('functional_capabilities', [])

            clean_rules = [
                str(r) for r in rules 
                if not any(x in str(r).upper() for x in ['CALL ', 'PERFORM ', 'EXEC SQL', 'OPEN FILE'])
            ]

            if description or clean_rules:
                self.h2(f"Module: {prog.filename}")
                
                # Show context (e.g. "Executed by JOB01")
                context = getattr(prog, 'execution_context', 'Subroutine')
                self.para(f"<i>Context: {context}</i>")

                # 1. Narrative
                if description:
                    self.para(description)
                
                # 2. Rules
                if clean_rules:
                    self.h4("Business Rules")
                    # Limit long lists to keep flow readable
                    for r in clean_rules: 
                        self.bullet(r)

    def _render_interfaces(self):
        self.h1("5. Interface Specification")
        
        self.h2("5.1 User Interfaces (Screens)")
        cics = [p.filename for p in self.code_files if 'CICS' in str(p.technical_analysis).upper()]
        if cics: self.para(f"Online CICS modules: {', '.join(cics)}")
        else: self.para("The system is primarily Batch Process driven.")

        self.h2("5.2 External Business Dependencies")
        inputs = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and 'OLD' in str(ds.get('usage', '')).upper():
                    inputs.append([str(ds.get('dataset')), jcl.filename])
        if inputs:
            unique = [list(x) for x in set(tuple(x) for x in inputs)]
            self.table(["Upstream Data Feed", "Consuming Job"], unique[:10], [110*mm, 60*mm])
        else:
            self.para("No external business data dependencies identified.")

        self.h2("5.3 Reporting Outputs")
        reports = []
        for s in self.summaries:
            if s.file_type == 'JCL':
                for ds in s.technical_analysis.get('io_datasets', []):
                    if isinstance(ds, dict):
                        name = str(ds.get('dataset', '')).upper()
                        if 'SYSOUT' in name or '.RPT' in name:
                            reports.append([s.filename, name])
        
        if reports:
            self.table(["Generating Job", "Output Report"], reports[:40], [70*mm, 100*mm])
        else:
            self.para("No standard reports identified.")

        self.h2("5.4 Downstream Feeds")
        feeds = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and 'NEW' in str(ds.get('usage')).upper():
                    feeds.append([str(ds.get('dataset')), jcl.filename])
        
        if feeds:
            self.table(["Output Artifact", "Source Job"], feeds, [110*mm, 60*mm])
        else:
            self.para("No downstream data feeds were identified via JCL dataset analysis.")

    def _render_ops(self):
        self.h1("6. Operational Functions")
        
        self.h2("6.1 Processing Frequencies")
        freq_rows = []
        for jcl in self.jcl_files:
            name = jcl.filename.upper()
            if 'DLY' in name: freq_rows.append([jcl.filename, "Daily Batch"])
            elif 'MTH' in name: freq_rows.append([jcl.filename, "Monthly Cycle"])
        if freq_rows: 
            self.table(["Job", "Inferred Frequency"], freq_rows, [80*mm, 80*mm])
        else:
            self.para("Execution frequency is determined by the production batch scheduler.")

        self.h2("6.2 Data Volume Capacities")
        volume_rows = []
        seen_dsn = set()
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if not isinstance(ds, dict): continue
                
                name = str(ds.get('dataset', '')).upper()
                category = None
                if 'MASTER' in name or '.MST' in name or 'MAIN' in name:
                    category = "Master Data (High Volume)"
                elif 'HIST' in name or 'ARC' in name or 'BACKUP' in name:
                    category = "Archival/History (High Volume)"
                elif 'LOG' in name or 'JRNL' in name or 'AUDIT' in name:
                    category = "Audit Log (Accumulating)"
                elif 'TRANS' in name or 'TXN' in name:
                    category = "Transaction Batch (Variable)"
                
                if category and name not in seen_dsn:
                    volume_rows.append([name, category])
                    seen_dsn.add(name)

        if volume_rows:
            self.para("The following datasets are identified as primary capacity drivers based on system usage patterns:")
            volume_rows.sort(key=lambda x: x[1])
            self.table(["Dataset Name", "Volume Category"], volume_rows[:25], [120*mm, 60*mm])
            if len(volume_rows) > 25:
                self.para(f"<i>...and {len(volume_rows)-25} other transactional files.</i>")
        else:
            self.para("No explicit Master/History files identified by naming convention. Standard DASD allocations apply.")

        
        self.h2("6.3 Backup & Recovery Procedures")
        self.para("The following jobs perform specific data protection functions based on utility usage (ADRDSSU/FDR) and output dataset naming.")

        backup_jobs = []
        backup_utils = ['ADRDSSU', 'FDR', 'DFDSS', 'IDCAMS'] # Strong indicators
        
        for jcl in self.jcl_files:
            is_backup = False
            reason = ""

            # Check 1: Utility Usage
            steps = jcl.technical_analysis.get('steps', [])
            for step in steps:
                pgm = str(step.get('program', '')).upper()
                if pgm in backup_utils:
                    is_backup = True
                    reason = f"Executes {pgm}"
                    break
            
            # Check 2: Output Naming Convention (if not already found)
            if not is_backup:
                datasets = jcl.technical_analysis.get('io_datasets', [])
                for ds in datasets:
                    if isinstance(ds, dict):
                        name = str(ds.get('dataset', '')).upper()
                        usage = str(ds.get('usage', '')).upper()
                        # If creating a file with BACKUP keywords
                        if ('NEW' in usage or 'OUTPUT' in usage) and \
                           any(x in name for x in ['.BKUP', '.BACKUP', '.SAFE', '.COPY', '.DUMP']):
                            is_backup = True
                            reason = f"Creates backup file: {name}"
                            break

            if is_backup:
                backup_jobs.append([jcl.filename, reason])

        if backup_jobs:
            self.table(["Backup Job", "Mechanism"], backup_jobs, [80*mm, 90*mm])
        else:
            self.para("No application-level backup jobs detected. Data protection is likely managed via system-level storage group snapshots (SMS) or external scheduling.")

        # --- 6.4 Archiving Logic (FIXED) ---
        self.h2("6.4 Archiving Logic")
        self.para("Data retention is managed via Generation Data Groups (GDG) and migration to history datasets.")
        
        archiving_evidence = []
        
        for jcl in self.jcl_files:
            datasets = jcl.technical_analysis.get('io_datasets', [])
            for ds in datasets:
                if isinstance(ds, dict):
                    name = str(ds.get('dataset', '')).upper()
                    usage = str(ds.get('usage', '')).upper()
                    
                    # Logic 1: GDG Creation (+1)
                    if '(+' in name and ('NEW' in usage or 'OUTPUT' in usage):
                        base_name = name.split('(')[0]
                        archiving_evidence.append([jcl.filename, f"Creates GDG Version: {base_name}"])
                    
                    # Logic 2: Writing to History files
                    if ('HIST' in name or 'ARC' in name) and ('NEW' in usage or 'MOD' in usage):
                         archiving_evidence.append([jcl.filename, f"Appends to History: {name}"])

        if archiving_evidence:
            # Deduplicate
            unique_arch = [list(x) for x in set(tuple(x) for x in archiving_evidence)]
            unique_arch.sort(key=lambda x: x[0])
            self.table(["Job Name", "Archiving Action"], unique_arch, [80*mm, 90*mm])
        else:
            self.para("No explicit archival logic (GDG generation or History file updates) detected in JCL.")

        self.h1("6.5 Error Handling Mechanisms")
        self.para("This section describes the system's functional response to processing errors, data exceptions, and technical failures, organized by module.")

        keywords = {
            'ABEND': 'Terminal Failure (Job stops immediately)',
            'ROLLBACK': 'Data Reversal (Database changes are undone)',
            'INVALID KEY': 'Lookup Failure (Missing record handling)',
            'REJECT': 'Validation Failure (Record bypassed and logged)',
            'SQLCODE': 'Database Exception',
            'ERROR-LOG': 'Audit Logging',
            'RETURN-CODE': 'Batch Completion Signaling'
        }

        found_logic = False
        # Filter for programs that actually have error logic extracted
        for prog in self.code_files:
            tech_notes = prog.technical_analysis.get('technical_notes', [])
            capabilities = prog.technical_analysis.get('functional_capabilities', [])
            all_logic = tech_notes + capabilities

            module_error_responses = []
            for item in all_logic:
                item_str = str(item)
                if any(k in item_str.upper() for k in keywords.keys()):
                    if len(item_str) > 10: # Avoid one-word garbage
                        module_error_responses.append(item_str)

            if module_error_responses:
                found_logic = True
                self.h3(f"Module: {prog.filename}")
                
                # Determine a "Strategy Type" based on the keywords found
                strategy = "Standard Validation"
                if 'ABEND' in str(module_error_responses).upper(): strategy = "Critical Stop / Abend"
                elif 'ROLLBACK' in str(module_error_responses).upper(): strategy = "Transactional Integrity (Rollback)"
                elif 'REJECT' in str(module_error_responses).upper(): strategy = "Data Quality Rejection"
                
                self.para(f"<b>Error Strategy:</b> {strategy}")
                
                # List the specific functional behaviors
                self.bullet_list(module_error_responses)
                # self.elements.append(Spacer(1, 2*mm))

        jcl_errors = []
        for jcl in self.jcl_files:
            notes = str(jcl.technical_analysis.get('schedule_notes', '')).upper()
            if 'COND' in notes or 'RESTART' in notes:
                jcl_errors.append(f"<b>{jcl.filename}</b>: Manages step-level dependencies. If a previous step fails, subsequent logic is bypassed to prevent data corruption.")

        if jcl_errors:
            self.h3("Batch Orchestration Error Control")
            self.para("At the JCL level, the following jobs utilize Condition Codes (COND) to control functional flow during partial failures:")
            for jcl_err in jcl_errors:
                self.bullet(jcl_err)

        if not found_logic and not jcl_errors:
            self.para("The system utilizes standard mainframe return code (RC) processing. Modules return RC=00 for success and RC > 04 for failures, which are captured in the system's standard job output (SYSOUT).")

        
    def _render_appendices(self):
        self.h1("7. Appendices")
        
        self.h2("7.1 Data Dictionary (Business View)")
        self.para("This section outlines the business data entities defined in the system. Technical storage formats (PIC clauses) have been translated to business-readable types.")

        for copy in self.data_files[:50]: # Limit to top 50
            # 1. Header with Filename
            self.h3(f"Entity: {copy.filename.replace('.txt', '').replace('_', ' ')}")
            
            # 2. Business Purpose (CRITICAL: This adds the context)
            purpose = copy.business_overview.get('purpose')
            if purpose and len(purpose) > 10:
                self.para(f"<b>Definition:</b> {purpose}")
            
            # 3. Data Entities (The "What is this?" list)
            entities = copy.business_overview.get('key_data_entities', [])
            if entities:
                self.para("<b>Contains Data For:</b> " + ", ".join(entities))

            # 4. The Table (Humanized)
            # Try to get fields with descriptions first
            fields = copy.technical_analysis.get('key_fields', [])
            structure = copy.technical_analysis.get('table_structure', [])
            
            rows = []
            
            # Strategy: Mix Key Fields (which usually have descriptions) with Structure
            if fields:
                # If we have key fields with descriptions, use them
                for f in fields:
                    if isinstance(f, dict):
                        rows.append([
                            str(f.get('field', 'N/A')), 
                            str(f.get('description', 'Key Business Identifier'))
                        ])
            elif structure:
                # Fallback to structure, but translate the PIC codes
                for f in structure:
                    if isinstance(f, dict):
                        raw_type = str(f.get('type', ''))
                        human_type = self._humanize_pic(raw_type)
                        rows.append([
                            str(f.get('column_name', 'N/A')), 
                            human_type
                        ])

            if rows:
                self.table(["Data Element", "Description / Format"], rows, [80*mm, 90*mm])
            else:
                self.para("No field definitions available.")

        self.h2("7.2 Report Catalog")
        self.para("The following catalog identifies every human-readable report and audit trail generated by the system.")

        report_rows = []
        seen_reports = set()

        for s in self.summaries:
            # Check 1: Scan JCL for SYSOUT or Report-named Datasets
            if s.file_type == 'JCL':
                datasets = s.technical_analysis.get('io_datasets', [])
                for ds in datasets:
                    if not isinstance(ds, dict): continue
                    
                    dsn = str(ds.get('dataset', '')).upper()
                    usage = str(ds.get('usage', '')).upper()
                    
                    # Logic: If it's an output and name looks like a report
                    is_output = any(x in usage for x in ['OUTPUT', 'NEW', 'CATLG'])
                    is_report = any(x in dsn for x in ['SYSOUT', '.RPT', '.LIST', '.PRT', '.PRINT', 'OUT', 'AUDIT'])
                    
                    if is_output and is_report and dsn not in seen_reports:
                        # Find the purpose from the JCL step description if available
                        purpose = "Batch Execution Log / Report"
                        for step in s.technical_analysis.get('steps', []):
                            if dsn in str(step.get('description', '')):
                                purpose = step.get('description')
                                
                        report_rows.append([s.filename, dsn, purpose])
                        seen_reports.add(dsn)

            elif s.file_type in ['COBOL', 'PLI']:
                ops = s.technical_analysis.get('key_operations', [])
                for op in ops:
                    if any(x in str(op).upper() for x in ['PRINT', 'REPORT', 'WRITE TO RPT']):
                        report_rows.append([s.filename, "Internal Print Spool", str(op)])

        if report_rows:
            report_rows.sort(key=lambda x: x[0])
            self.table(["Generating Job / Module", "Report / Dataset Name", "Business Purpose"], report_rows, [50*mm, 60*mm, 60*mm])
        else:
            self.para("No standard human-readable reports (SYSOUT or .RPT) were identified in the analyzed JCL/Source.")

    def _humanize_pic(self, pic_str: str) -> str:
        """Helper to translate COBOL PIC clauses to Business Terms."""
        p = pic_str.upper()
        
        if not p: return "Unknown"
        
        # Date Logic
        if "DATE" in p or "DT" in p: return "Date (YYYY-MM-DD)"
        
        # String Logic
        if "X" in p or "A" in p:
            # Extract length if possible, e.g. X(10)
            import re
            match = re.search(r'\((\d+)\)', p)
            length = match.group(1) if match else "Variable"
            return f"Text / String ({length} chars)"
            
        # Numeric Logic
        if "9" in p:
            if "V" in p or "." in p:
                return "Decimal / Currency Amount"
            return "Integer / Count"
            
        # DB2 Types
        if "VARCHAR" in p: return "Variable Length Text"
        if "DECIMAL" in p: return "Decimal Number"
        if "INTEGER" in p: return "Whole Number"
        if "TIMESTAMP" in p: return "Date & Time"
        
        return p # Fallback to raw if unknown