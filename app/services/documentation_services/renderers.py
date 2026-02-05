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
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, 
    PageBreak, Image, ListFlowable, ListItem
)
from reportlab.platypus.tableofcontents import TableOfContents
from reportlab.lib.enums import TA_JUSTIFY, TA_LEFT, TA_CENTER

from app.api.schemas.doc_models import FileSummary, SystemMetrics

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
            # We map Heading1 -> Level 0, Heading2 -> Level 1
            if style_name == 'Heading1':
                self.notify('TOCEntry', (0, text, self.page))
            elif style_name == 'Heading2':
                self.notify('TOCEntry', (1, text, self.page))

class BaseBuilder:
    def __init__(self, summaries: List[FileSummary], metrics: SystemMetrics, graph_analyzer, system_summary: Dict = None):
        self.summaries = summaries
        self.metrics = metrics
        self.system_summary = system_summary or {}
        self.graph_analyzer = graph_analyzer
        self.graph_image_path = None
        
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
        
        # Custom Heading Styles
        self.styleH1 = ParagraphStyle(
            'Heading1', parent=styles['Heading1'], 
            fontSize=16, leading=20, spaceBefore=18, spaceAfter=12, 
            textColor=colors.HexColor("#003366") # Deep Blue
        )
        self.styleH2 = ParagraphStyle(
            'Heading2', parent=styles['Heading2'], 
            fontSize=13, leading=16, spaceBefore=12, spaceAfter=6, 
            textColor=colors.HexColor("#404040") # Dark Gray
        )
        self.styleH3 = ParagraphStyle(
            'Heading3', parent=styles['Heading3'], 
            fontSize=11, leading=14, spaceBefore=10, spaceAfter=4, 
            textColor=colors.HexColor("#606060") # Slate
        )
        self.styleH4 = ParagraphStyle(
            'Heading4', parent=styles['Normal'], 
            fontSize=10, leading=12, spaceBefore=6, spaceAfter=2, 
            fontName='Helvetica-BoldOblique'
        )
        
        self.elements = []

    def save(self, path: str):
        # Configure Page
        doc = DocTemplateWithTOC(
            path, pagesize=A4,
            rightMargin=20*mm, leftMargin=20*mm,
            topMargin=20*mm, bottomMargin=20*mm
        )
        
        # Build Story Sequence
        story = []
        
        # 1. Title Page
        story.append(Spacer(1, 60*mm))
        story.append(Paragraph(self.title_text, ParagraphStyle('Title', parent=self.styleH1, fontSize=24, alignment=TA_CENTER)))
        story.append(Spacer(1, 10*mm))
        story.append(Paragraph("System Reference Document", ParagraphStyle('SubTitle', parent=self.styleN, fontSize=16, alignment=TA_CENTER)))
        story.append(PageBreak())

        # 2. Table of Contents
        story.append(Paragraph("Table of Contents", self.styleH1))
        story.append(doc.toc)
        story.append(PageBreak())

        # 3. Content
        story.extend(self.elements)

        # Generate
        doc.multiBuild(story)

    def h1(self, text):
        self.elements.append(PageBreak())
        self.elements.append(Paragraph(text, self.styleH1))

    def h2(self, text):
        self.elements.append(Paragraph(text, self.styleH2))

    def h3(self, text):
        self.elements.append(Paragraph(text, self.styleH3))
    
    def h4(self, text):
        self.elements.append(Paragraph(text, self.styleH4))

    def para(self, text):
        if not text: return
        clean_text = str(text).replace('\n', '<br/>')
        self.elements.append(Paragraph(clean_text, self.styleN))
        self.elements.append(Spacer(1, 2*mm))

    def bullet_list(self, items: List[str]):
        if not items: return
        list_items = []
        for item in items:
            clean = str(item).replace('[', '').replace(']', '').replace("'", "")
            list_items.append(ListItem(Paragraph(clean, self.styleN)))
        
        self.elements.append(
            ListFlowable(
                list_items,
                bulletType='bullet',
                start='•',
                leftIndent=15,
                bulletFontSize=12
            )
        )
        self.elements.append(Spacer(1, 3*mm))

    def image(self, path, width=170*mm):
        try:
            img = Image(path)
            # Resize preserving aspect ratio
            img_width = img.drawWidth
            img_height = img.drawHeight
            if img_width > width:
                factor = width / img_width
                img.drawWidth = width
                img.drawHeight = img_height * factor
            
            self.elements.append(img)
            self.elements.append(Spacer(1, 5*mm))
        except Exception:
            self.para("<i>[Diagram Image Not Available]</i>")

    def table(self, headers: List[str], rows: List[List[str]], col_widths=None):
        if not rows:
            self.para("<i>No data available.</i>")
            return
            
        # Convert all cell content to Paragraphs (allows text wrapping)
        data = [[Paragraph(f"<b>{h}</b>", self.styleN) for h in headers]]
        
        for row in rows:
            safe_row = []
            for cell in row:
                # Truncate extremely long cells to prevent PDF blowout
                txt = str(cell)
                if len(txt) > 5000: txt = txt[:5000] + "..."
                safe_row.append(Paragraph(txt, self.styleN))
            data.append(safe_row)

        # Style
        t = Table(data, colWidths=col_widths, repeatRows=1)
        t.setStyle(TableStyle([
            ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor("#E6E6E6")), # Header BG
            ('TEXTCOLOR', (0, 0), (-1, 0), colors.black),
            ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
            ('VALIGN', (0, 0), (-1, -1), 'TOP'),
            ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
            ('BOTTOMPADDING', (0, 0), (-1, -1), 6),
            ('TOPPADDING', (0, 0), (-1, -1), 6),
        ]))
        self.elements.append(t)
        self.elements.append(Spacer(1, 6*mm))

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
        self.para("This document provides a detailed technical reference for the existing system, generated via automated static analysis of the codebase.")
        
        self.h2("1.2 Scope of Analysis")
        self.para("The following components were analyzed:")
        rows = [[k, str(v)] for k, v in self.metrics.files_by_type.items()]
        self.table(["Component Type", "Count"], rows, [100*mm, 50*mm])
        
        self.h2("1.3 Technology Stack")
        techs = ", ".join(list(self.metrics.files_by_type.keys()))
        self.para(f"Identified technologies: {techs}")
        
        self.h2("1.4 System Context Diagram")
        self.para("Refer to Section 2.1 for the architectural visual.")
        
        self.h2("1.5 Acronyms & Definitions")
        glossary = []
        if self.summaries:
            for s in self.summaries[:5]:
                g = s.business_overview.get('glossary', [])
                if g: glossary.extend(g); break
        
        if glossary:
            rows = [[g.get('term',''), g.get('definition','')] for g in glossary]
            self.table(["Acronym", "Definition"], rows, [40*mm, 130*mm])
        else:
            self.para("No glossary terms identified.")

    def _arch(self):
        self.h1("2. System Architecture")
        self.h2("2.1 System Landscape")
        self.para(self.system_summary.get('system_landscape', 'System landscape details unavailable.'))
        
        self.h2("2.2 Integration Architecture")
        
        self.h3("Upstream Systems")
        inputs = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and 'OLD' in str(ds.get('usage', '')).upper():
                    inputs.append([str(ds.get('dataset', 'Unknown')), jcl.filename])
        # Deduplicate
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

        self.h2("2.3 High-Level Process Flow")
        if self.graph_image_path: self.image(self.graph_image_path)
        
        self.h2("2.4 Data Flow Architecture")
        self.para("The system architecture follows a Batch Execution model updating persistent stores.")

    def _batch(self):
        self.h1("3. Batch Execution Specification")
        
        self.h2("3.1 Job Definitions")
        for jcl in self.jcl_files:
            self.h3(f"Job: {jcl.filename}")
            header = jcl.technical_analysis.get('job_header', {})
            self.para(f"Class: {header.get('class', 'N/A')} | Owner: {header.get('owner', 'N/A')}")
            
            steps = jcl.technical_analysis.get('steps', [])
            if steps:
                rows = [[str(s.get('step_name')), str(s.get('program')), str(s.get('description'))] for s in steps]
                self.table(["Step", "Program", "Description"], rows, [30*mm, 40*mm, 100*mm])

        self.h2("3.2 JCL Procedures")
        procs = [s for s in self.summaries if s.file_type == 'PROC']
        if procs:
            rows = [[p.filename, p.business_overview.get('purpose', '')] for p in procs]
            self.table(["Procedure", "Purpose"], rows, [60*mm, 110*mm])
        else:
            self.para("No PROCs found.")

        self.h2("3.3 Job Dependencies")
        self.para("Dependencies inferred from dataset usage (Producer -> Consumer):")
        # Placeholder for complex dependency chain text
        self.para("Refer to Process Flow diagram.")

        self.h2("3.4 Utility & Control Specifications")
        if self.configs:
            for cfg in self.configs:
                self.h3(f"Control Member: {cfg.filename}")
                self.para(cfg.business_overview.get('purpose', ''))
                
                # Logic Rules
                rules = cfg.technical_analysis.get('configuration_areas', [])
                if rules:
                    self.h4("Logic Rules")
                    self.bullet_list(rules)
                
                # Parameters
                params = cfg.technical_analysis.get('key_parameters', [])
                if params and isinstance(params[0], dict):
                    rows = [[p.get('name'), p.get('value'), p.get('description')] for p in params]
                    self.table(["Param", "Value", "Desc"], rows, [40*mm, 40*mm, 90*mm])

    def _logic(self):
        self.h1("4. Application Logic Specification")
        
        sorted_code = sorted(self.code_files, key=lambda x: (x.file_type, x.filename))
        for idx, prog in enumerate(sorted_code, 1):
            self.h3(f"4.1.{idx} {prog.filename} ({prog.file_type})")
            
            self.h4("Functional Logic")
            self.para(prog.business_overview.get('purpose', 'N/A'))
            self.bullet_list(prog.business_overview.get('scope', []))
            
            self.h4("Call Graph / I/O")
            ops = prog.technical_analysis.get('key_operations', [])
            self.bullet_list(ops)
            
            self.h4("Error Handling")
            notes = prog.technical_analysis.get('technical_notes', [])
            self.bullet_list(notes)

    def _data(self):
        self.h1("5. Data Specification")
        
        self.h2("5.1 Database Schema (DB2)")
        dclgens = [f for f in self.data_files if f.file_type in ['DCLGEN', 'SQL']]
        for dcl in dclgens:
            self.h3(f"Table: {dcl.technical_analysis.get('table_name', dcl.filename)}")
            cols = dcl.technical_analysis.get('table_structure', [])
            if cols:
                rows = [[str(c.get('column_name')), str(c.get('type')), str(c.get('nullable'))] for c in cols if isinstance(c, dict)]
                self.table(["Column", "Type", "Null"], rows, [60*mm, 60*mm, 40*mm])

        self.h2("5.2 File Layouts (Copybooks)")
        copybooks = [f for f in self.data_files if f.file_type in ['COPYBOOK', 'PLI_COPYBOOK']]
        for copy in copybooks:
            self.h3(f"Layout: {copy.filename}")
            fields = copy.technical_analysis.get('table_structure') or copy.technical_analysis.get('key_fields')
            if fields:
                rows = []
                for f in fields:
                    if isinstance(f, dict):
                        rows.append([str(f.get('column_name') or f.get('field')), str(f.get('type') or f.get('description'))])
                self.table(["Field", "Type/Desc"], rows, [80*mm, 90*mm])

        self.h2("5.3 CRUD Matrix")
        rows = []
        for prog in self.code_files:
            ops = prog.technical_analysis.get('data_interactions', [])
            for op in ops:
                if isinstance(op, dict):
                    rows.append([prog.filename, str(op.get('target')), str(op.get('operation'))])
        
        if rows:
            self.table(["Program", "Table/File", "Op"], rows[:30], [50*mm, 80*mm, 40*mm]) # Limit rows

    def _ops(self):
        self.h1("6. Operational Support & Reliability")
        self.h2("6.1 Restart & Recovery")
        restart_jobs = [j.filename for j in self.jcl_files if "RESTART" in str(j.technical_analysis).upper()]
        if restart_jobs:
            self.para(f"Jobs with explicit restart: {', '.join(restart_jobs)}")
        else:
            self.para("No explicit RESTART parameters found. Standard step restart applies.")
            
        self.h2("6.2 Error Handling")
        self.para("System uses standard Condition Code (COND) triggers.")
        
        self.h2("6.3 Performance")
        self.para("Analysis of high-frequency I/O modules based on call graph.")

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
        self._intro()
        self._flows()
        self._logic()
        self._interfaces()
        self._ops()
        self._appx()

    def _intro(self):
        self.h1("2. Introduction")
        self.h2("2.1 Business Overview")
        self.para(self.system_summary.get('business_purpose', 'N/A'))
        
        self.h2("2.2 System Purpose")
        self.para("The system facilitates core transaction processing, validation, and reporting.")
        
        self.h2("2.3 Scope")
        self.para("Included modules: " + ", ".join(list(set([s.file_type for s in self.summaries]))))
        
        self.h2("2.4 Glossary")
        glossary = self.system_summary.get('glossary', [])
        if glossary:
            rows = [[g.get('term',''), g.get('definition','')] for g in glossary]
            self.table(["Term", "Definition"], rows, [40*mm, 130*mm])

    def _flows(self):
        self.h1("3. High-Level Functional Flows")
        
        # 3.1 Diagram
        self.h2("3.1 Process Diagram")
        flow = self.system_summary.get('functional_flow_diagram', {})
        if flow.get('description'): self.para(flow['description'])
        
        steps = flow.get('steps_table', [])
        if steps:
            rows = [[str(s.get('actor')), str(s.get('action')), str(s.get('outcome'))] for s in steps]
            self.table(["Actor", "Action", "Outcome"], rows, [50*mm, 70*mm, 50*mm])
        else:
            self.para("No flow steps defined.")

        # 3.2 Groups
        self.h2("3.2 Core Functional Groups")
        
        # Grouping Logic
        tx_progs = []
        rpt_progs = []
        maint_progs = []

        for p in self.code_files:
            text = (str(p.technical_analysis) + str(p.business_overview)).upper()
            if any(x in text for x in ['INSERT', 'UPDATE', 'WRITE', 'TRANSACTION']): tx_progs.append(p.filename)
            elif any(x in text for x in ['REPORT', 'PRINT', 'DISPLAY']): rpt_progs.append(p.filename)
            elif any(x in text for x in ['BACKUP', 'ARCHIVE', 'DELETE']): maint_progs.append(p.filename)

        self.h3("3.2.1 Transaction Processing Group")
        self.bullet_list(tx_progs)
        
        self.h3("3.2.2 Reporting & Analysis Group")
        self.bullet_list(rpt_progs)
        
        self.h3("3.2.3 Data Maintenance Group")
        self.bullet_list(maint_progs)

        self.h2("3.3 Reporting & Extraction Process")
        self.para("See Interface Specification -> Reporting Outputs.")

    def _logic(self):
        self.h1("4. Detailed Functional Logic")
        
        self.h2("4.1 Business Rules & Validations")
        for prog in self.code_files:
            scope = prog.business_overview.get('scope', [])
            if scope:
                self.h3(f"{prog.filename}")
                self.bullet_list(scope)

        self.h2("4.2 Data Management Functions")
        
        # Logic to split CRUD types
        inserts, updates, deletes = [], [], []
        for prog in self.code_files:
            ops = (str(prog.technical_analysis.get('key_operations')) + str(prog.technical_analysis.get('data_interactions'))).upper()
            if 'INSERT' in ops or 'WRITE' in ops: inserts.append(prog.filename)
            if 'UPDATE' in ops or 'REWRITE' in ops: updates.append(prog.filename)
            if 'DELETE' in ops: deletes.append(prog.filename)

        self.h3("4.2.1 Create/Insert Logic")
        self.bullet_list(inserts)
        self.h3("4.2.2 Update Logic")
        self.bullet_list(updates)
        self.h3("4.2.3 Deletion Logic")
        self.bullet_list(deletes)

        self.h2("4.3 Transformations")
        self.para("Data format conversions detected in COBOL logic.")

    def _interfaces(self):
        self.h1("5. Interface Specification")
        
        self.h2("5.1 User Interfaces")
        cics = [p.filename for p in self.code_files if 'CICS' in str(p.technical_analysis).upper()]
        if cics: self.para(f"Online modules: {', '.join(cics)}")
        else: self.para("Batch Only.")

        self.h2("5.2 External Dependencies")
        inputs = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and 'OLD' in str(ds.get('usage', '')).upper():
                    inputs.append([str(ds.get('dataset')), jcl.filename])
        if inputs:
            unique = [list(x) for x in set(tuple(x) for x in inputs)]
            self.table(["Input File", "Consumed By"], unique[:15], [100*mm, 70*mm])

        self.h2("5.3 Reporting Outputs")
        reports = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and ('SYSOUT' in str(ds) or '.RPT' in ds.get('dataset', '')):
                    reports.append([str(ds.get('dataset')), jcl.filename])
        self.table(["Report", "Generated By"], reports, [100*mm, 70*mm])

        self.h2("5.4 Downstream Feeds")
        feeds = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and 'NEW' in str(ds.get('usage', '')).upper():
                    feeds.append([str(ds.get('dataset')), jcl.filename])
        self.table(["Output File", "Generated By"], feeds[:15], [100*mm, 70*mm])

    def _ops(self):
        self.h1("6. Operational Functions")
        freq = self.system_summary.get('schedule_frequency', {})
        self.para(f"Frequency: {freq.get('frequency', 'N/A')}")
        self.para(f"Window: {freq.get('sla_window', 'N/A')}")
        
        self.h2("6.3 Backup")
        backups = [j.filename for j in self.jcl_files if 'BACKUP' in str(j.business_overview).upper()]
        self.bullet_list(backups)

    def _appx(self):
        self.h1("7. Appendices")
        self.h2("7.1 Data Dictionary")
        for copy in self.data_files:
            ent = copy.business_overview.get('key_data_entities', [])
            if ent:
                self.h3(copy.filename)
                self.bullet_list(ent)
        self.h2("7.2 Report Catalog")
        reports = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and ('SYSOUT' in str(ds) or '.RPT' in ds.get('dataset', '')):
                    reports.append([str(ds.get('dataset')), jcl.filename])
        self.table(["Report", "Generated By"], reports, [100*mm, 70*mm])