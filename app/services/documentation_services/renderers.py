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

# class BaseBuilder:
#     def __init__(self, summaries: List[FileSummary], metrics: SystemMetrics, graph_analyzer, system_summary: Dict = None, images: Dict[str, str] = None):
#         self.summaries = summaries
#         self.metrics = metrics
#         self.system_summary = system_summary or {}
#         self.graph_analyzer = graph_analyzer
        
#         # --- Multi-Image Handling ---
#         self.images = images or {}
#         self.context_image_path = self.images.get('context')
#         self.architecture_image_path = self.images.get('architecture')
#         self.functional_image_path = self.images.get('functional')
        
#         # Data Slicing
#         self.jcl_files = [s for s in summaries if s.file_type == 'JCL']
#         self.code_files = [s for s in summaries if s.file_type in ['COBOL', 'PLI', 'ASSEMBLY', 'REXX']]
#         self.data_files = [s for s in summaries if s.file_type in ['DCLGEN', 'SQL', 'COPYBOOK', 'PLI_COPYBOOK']]
#         self.configs = [s for s in summaries if s.file_type in ['PARMLIB', 'CONTROL_CARD']]

#         # Styles Setup
#         styles = getSampleStyleSheet()
#         self.styleN = styles['Normal']
#         self.styleN.fontSize = 10
#         self.styleN.leading = 12
        
#         self.styleH1 = ParagraphStyle(
#             'Heading1', parent=styles['Heading1'], 
#             fontSize=16, leading=20, spaceBefore=18, spaceAfter=12, 
#             textColor=colors.HexColor("#003366")
#         )
#         self.styleH2 = ParagraphStyle(
#             'Heading2', parent=styles['Heading2'], 
#             fontSize=13, leading=16, spaceBefore=12, spaceAfter=6, 
#             textColor=colors.HexColor("#404040")
#         )
#         self.styleH3 = ParagraphStyle(
#             'Heading3', parent=styles['Heading3'], 
#             fontSize=11, leading=14, spaceBefore=10, spaceAfter=4, 
#             textColor=colors.HexColor("#606060")
#         )
#         self.styleH4 = ParagraphStyle(
#             'Heading4', parent=styles['Normal'], 
#             fontSize=10, leading=12, spaceBefore=6, spaceAfter=2, 
#             fontName='Helvetica-BoldOblique'
#         )
        
#         self.elements = []

#     def save(self, path: str):
#         doc = DocTemplateWithTOC(
#             path, pagesize=A4,
#             rightMargin=20*mm, leftMargin=20*mm,
#             topMargin=20*mm, bottomMargin=20*mm
#         )
#         story = []
#         story.append(Spacer(1, 60*mm))
#         story.append(Paragraph(self.title_text, ParagraphStyle('Title', parent=self.styleH1, fontSize=24, alignment=TA_CENTER)))
#         story.append(Spacer(1, 10*mm))
#         story.append(Paragraph("System Reference Document", ParagraphStyle('SubTitle', parent=self.styleN, fontSize=16, alignment=TA_CENTER)))
#         story.append(PageBreak())
#         story.append(Paragraph("Table of Contents", self.styleH1))
#         story.append(doc.toc)
#         story.append(PageBreak())
#         story.extend(self.elements)
#         doc.multiBuild(story)

#     def h1(self, text):
#         self.elements.append(PageBreak())
#         self.elements.append(Paragraph(text, self.styleH1))

#     def h2(self, text):
#         self.elements.append(Paragraph(text, self.styleH2))

#     def h3(self, text):
#         self.elements.append(Paragraph(text, self.styleH3))
    
#     def h4(self, text):
#         self.elements.append(Paragraph(text, self.styleH4))

#     def para(self, text):
#         if not text: return
#         clean_text = str(text).replace('\n', '<br/>')
#         self.elements.append(Paragraph(clean_text, self.styleN))
#         self.elements.append(Spacer(1, 2*mm))

#     def bullet_list(self, items: List[str]):
#         if not items: return
#         list_items = [ListItem(Paragraph(str(item).strip("[]'"), self.styleN)) for item in items]
#         self.elements.append(ListFlowable(list_items, bulletType='bullet', start='•', leftIndent=15, bulletFontSize=12))
#         self.elements.append(Spacer(1, 3*mm))

#     def image(self, path, width=160*mm):
#         if not path or not Path(path).exists():
#             self.para("<i>[Diagram Image Not Available]</i>")
#             return
#         try:
#             img = Image(path)
#             aspect = img.drawHeight / img.drawWidth
#             img.drawWidth = width
#             img.drawHeight = width * aspect
#             self.elements.append(img)
#             self.elements.append(Spacer(1, 5*mm))
#         except Exception as e:
#             logger.error(f"Failed to embed image: {e}")
#             self.para("<i>[Error rendering diagram file]</i>")

#     def table(self, headers: List[str], rows: List[List[str]], col_widths=None):
#         if not rows:
#             self.para("<i>No data available.</i>")
#             return
#         data = [[Paragraph(f"<b>{h}</b>", self.styleN) for h in headers]]
#         for row in rows:
#             data.append([Paragraph(str(cell), self.styleN) for cell in row])
#         t = Table(data, colWidths=col_widths, repeatRows=1)
#         t.setStyle(TableStyle([
#             ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor("#E6E6E6")),
#             ('TEXTCOLOR', (0, 0), (-1, 0), colors.black),
#             ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
#             ('VALIGN', (0, 0), (-1, -1), 'TOP'),
#             ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
#             ('BOTTOMPADDING', (0, 0), (-1, -1), 6),
#             ('TOPPADDING', (0, 0), (-1, -1), 6),
#         ]))
#         self.elements.append(t)
#         self.elements.append(Spacer(1, 6*mm))

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
        self.functional_image_path = self.images.get('functional')
        
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

    def bullet(self, text):
        """Renders a single bullet point (Fixes the AttributeError)."""
        if not text: return
        self.bullet_list([text])

    def bullet_list(self, items: List[str]):
        """Renders a list of items as bullets."""
        if not items: return
        list_items = [ListItem(Paragraph(str(item).strip("[]'"), self.styleN)) for item in items]
        self.elements.append(
            ListFlowable(list_items, bulletType='bullet', start='•', leftIndent=15, bulletFontSize=12)
        )
        self.elements.append(Spacer(1, 3*mm))

    def para(self, text):
        if not text: return
        # Wrap text in Paragraph for ReportLab
        self.elements.append(Paragraph(str(text).replace('\n', '<br/>'), self.styleN))
        self.elements.append(Spacer(1, 2*mm))

    def h1(self, text):
        if self.elements:  # Only add page break if this isn't the first item
            self.elements.append(PageBreak())
        self.elements.append(Paragraph(text, self.styleH1))

    def h2(self, text):
        self.elements.append(Paragraph(text, self.styleH2))

    def h3(self, text):
        self.elements.append(Paragraph(text, self.styleH3))
    
    def h4(self, text):
        self.elements.append(Paragraph(text, self.styleH4))

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
        
        data = [[Paragraph(f"<b>{h}</b>", self.styleN) for h in headers]]
        for row in rows:
            data.append([Paragraph(str(cell), self.styleN) for cell in row])
        
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
        self.para(self.system_summary.get('business_purpose', "Technical reference generated via static analysis of the codebase."))
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
        if self.architecture_image_path and Path(self.architecture_image_path).exists():
            try:
                # 1. Get the actual pixel dimensions of the image
                img_reader = ImageReader(self.architecture_image_path)
                iw, ih = img_reader.getSize()
                aspect = ih / float(iw)

                # 2. Set the target width (matching your AVAILABLE_WIDTH of 170mm)
                target_width = self.AVAILABLE_WIDTH
                target_height = target_width * aspect

                # 3. CRITICAL: Height Guard
                # A4 page is ~297mm. With margins, usable height is ~250mm.
                # If the diagram is very tall, we must reduce width to maintain aspect ratio
                # otherwise ReportLab will force it into the frame, causing the "squeeze".
                max_allowed_height = 190*mm 
                if target_height > max_allowed_height:
                    target_height = max_allowed_height
                    target_width = target_height / aspect

                # 4. Create the Image object manually and add to elements
                from reportlab.platypus import Image as RLImage
                img = RLImage(self.architecture_image_path, width=target_width, height=target_height)
                img.hAlign = 'CENTER'
                
                self.elements.append(img)
                self.elements.append(Spacer(1, 5*mm))
                self.para("<i>Figure 2: Component interaction and control flow graph.</i>")
                
            except Exception as e:
                # Fallback if image processing fails
                self.para(f"Process flow diagram generation failed: {e}")
        else:
            self.para("Process flow diagram generation failed.")
        
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
        chains = []
        for u, v, d in self.graph_analyzer.graph.edges(data=True):
            if d.get('type') in ['TRIGGER', 'EXEC_PGM', 'EXEC_PROC']:
                chains.append([u, v, d.get('type')])
        if chains:
            self.table(["Source Job/File", "Target Component", "Relationship"], chains[:20], [60*mm, 60*mm, 40*mm])
        else:
            self.para("No automated job chains identified.")

        self.h2("3.4 Utility & Control Specifications")
        if self.configs:
            for cfg in self.configs:
                self.h3(f"Control Member: {cfg.filename}")
                self.para(cfg.business_overview.get('purpose', ''))
                rules = cfg.technical_analysis.get('configuration_areas', [])
                if rules:
                    self.h4("Logic Rules")
                    self.bullet_list(rules)
                params = cfg.technical_analysis.get('key_parameters', [])
                if params and isinstance(params[0], dict):
                    rows = [[p.get('name'), p.get('value'), p.get('description')] for p in params]
                    self.table(["Param", "Value", "Desc"], rows, [40*mm, 40*mm, 90*mm])
        else:
            self.para("No utilities identified")

    def _logic(self):
        self.h1("4. Application Logic Specification")
        sorted_code = sorted(self.code_files, key=lambda x: x.filename)
        for idx, prog in enumerate(sorted_code, 1):
            self.h3(f"4.1.{idx} {prog.filename} ({prog.file_type})")
            self.h4("Functional Logic")
            self.para(prog.business_overview.get('purpose', 'N/A'))
            self.bullet_list(prog.business_overview.get('scope', []))
            self.h4("Call Graph / I/O")
            self.bullet_list(prog.technical_analysis.get('key_operations', []))
            self.h4("Error Handling")
            self.bullet_list(prog.technical_analysis.get('technical_notes', []))

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
                rows = [[str(f.get('column_name') or f.get('field')), str(f.get('type') or f.get('description'))] for f in fields if isinstance(f, dict)]
                self.table(["Field", "Type/Desc"], rows, [80*mm, 90*mm])

        self.h2("5.3 CRUD Matrix")
        rows = []
        for prog in self.code_files:
            ops = prog.technical_analysis.get('data_interactions', [])
            for op in ops:
                if isinstance(op, dict):
                    rows.append([prog.filename, str(op.get('target')), str(op.get('operation'))])
        if rows:
            self.table(["Program", "Table/File", "Op"], rows[:30], [50*mm, 80*mm, 40*mm])

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
        self.table(["Version", "Date", "Description"], [["1.0", "Auto-Generated", "Initial System Documentation"]], [30*mm, 40*mm, 100*mm])

    def _render_introduction(self):
        self.h1("2. Introduction")
        
        self.h2("2.1 Business Overview")
        self.para(self.system_summary.get('business_purpose', 'High-level business overview of the system processing.'))

        self.h2("2.2 System Purpose")
        counts = self.metrics.files_by_type
        primary_lang = "COBOL" if counts.get('COBOL', 0) > counts.get('PLI', 0) else "PL/I"
        
        sys_purpose = (
            f"The primary purpose of this COBOL-based system is to facilitate high-volume "
            f"batch processing and data management. It orchestrates {counts.get('JCL', 0)} job workflows "
            f"to process transactions against {counts.get('DCLGEN', 0) + counts.get('SQL', 0)} relational tables."
        )
        
        self.para(sys_purpose)
        top_mods = self.metrics.top_complex_modules
        if top_mods:
            self.para("Key functional drivers identified by architectural centrality:")
            clean_mods = [m for m in top_mods if not any(x in m for x in ['ABEND', 'ERROR'])]
            for mod in clean_mods[:20]:
                self.bullet(mod)

                self.h2("2.3 Scope of Current Functionality")

        self.h3("Technical Domains")
        ftypes = self.metrics.files_by_type
        scope_items = []
        if ftypes.get('COBOL'): scope_items.append(" Business Logic and Calculations (COBOL)")
        if ftypes.get('JCL'): scope_items.append("Batch Orchestration (JCL)")
        if ftypes.get('DCLGEN') or ftypes.get('SQL'): scope_items.append("Relational Database Persistence (DB2)")
        self.para("The analyzed system scope encompasses: " + ", ".join(scope_items) + ".")

        self.h3("Key Business & Processing Boundaries")
        boundaries = self.system_summary.get('system_processing_boundaries', [])
        
        if boundaries:
            self.para("The following high-level constraints and data handling rules define the system's operational boundaries:")
            self.bullet_list(boundaries)
        else:
            self.para("Operational boundaries extracted from module scopes:")
            all_scopes = []
            for s in self.code_files:
                file_scope = s.business_overview.get('scope', [])
                if file_scope: all_scopes.extend(file_scope)
            unique_scopes = sorted(list(set(all_scopes)))

            constraints = [s for s in unique_scopes if any(x in s.upper() for x in ['ONLY', 'MUST', 'RETAIN', 'LIMIT', 'MAX', 'MIN', 'MONTHS', 'YEARS'])]
            
            if constraints:
                self.bullet_list(constraints[:10])
            else:
                self.para("No specific processing boundaries (retention, limits) explicitly defined in source comments.")

        self.h2("2.4 Glossary")
        glossary_data = self.system_summary.get('harvested_acronyms', [])

        if not glossary_data:
            glossary_data = self.system_summary.get('glossary', [])

        if not glossary_data:
            glossary_data = [
                {"term": "GDG", "definition": "Generation Data Group (Versioning)"},
                {"term": "JCL", "definition": "Job Control Language"},
                {"term": "VSAM", "definition": "Virtual Storage Access Method"},
                {"term": "DB2", "definition": "IBM Relational Database"}
            ]

        rows = []
        for g in glossary_data:
            if isinstance(g, dict):
                term = g.get('term', 'N/A')
                defi = g.get('definition', 'N/A')
            else:
                continue
            
            if term != "N/A" and len(term) < 20:
                rows.append([term, defi])
        
        rows.sort(key=lambda x: x[0])
        
        if rows:
            self.table(["Term / Acronym", "Definition"], rows[:40], [40*mm, 130*mm])
        else:
            self.para("No technical acronyms identified.")

    def _render_functional_flows(self):
        self.h1("3. High-Level Functional Flows")
        
        self.h2("3.1 High-Level Process Diagram")
        if self.functional_image_path:
            self.image(self.functional_image_path)
            self.para("<i>Figure: End-to-End Business Sequence.</i>")
        else:
            self.para("Logical flow: Inbound Feeds -> Batch Validation -> Core Processing -> Data Update -> Reporting.")
        
        self.h2("3.2 Batch Execution Flow (Data Lineage)")
        self.para("The following table illustrates the sequential flow of data through the batch system, mapping how jobs chain together via shared datasets.")      
        import re
        def normalize_dsn(dsn):
            return re.sub(r'\(.*?\)', '', str(dsn)).strip().upper()
        producer_map = {}
        
        for jcl in self.jcl_files:
            # Get program list for context
            steps = jcl.technical_analysis.get('steps', [])
            progs = [s.get('program', '') for s in steps if s.get('program')]
            prog_str = ", ".join([p for p in progs if p not in ['IEFBR14', 'IEBGENER', 'IDCAMS', 'SORT']][:3])
            if not prog_str: prog_str = "System Util"

            for ds in jcl.technical_analysis.get('io_datasets', []):
                if not isinstance(ds, dict): continue
                
                usage = str(ds.get('usage', '')).upper()
                raw_name = str(ds.get('dataset', ''))
                norm_name = normalize_dsn(raw_name)

                # Ignore temp files
                if 'TEMP' in norm_name or '&&' in norm_name or 'SYSOUT' in norm_name: continue

                # If Creating/Writing
                if 'NEW' in usage or 'WRITE' in usage or 'OUTPUT' in usage or 'CATLG' in usage:
                    if norm_name not in producer_map:
                        producer_map[norm_name] = {"job": jcl.filename, "progs": prog_str}

        # 2. Map Consumers (Who reads that data?)
        flow_rows = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if not isinstance(ds, dict): continue
                
                usage = str(ds.get('usage', '')).upper()
                raw_name = str(ds.get('dataset', ''))
                norm_name = normalize_dsn(raw_name)

                # If Reading
                if 'OLD' in usage or 'READ' in usage or 'INPUT' in usage or 'SHR' in usage:
                    producer = producer_map.get(norm_name)
                    
                    # If we found the creator, and it's a different job
                    if producer and producer['job'] != jcl.filename:
                        flow_rows.append([
                            producer['job'],      # Predecessor
                            producer['progs'],    # Logic applied
                            raw_name,             # The specific file used
                            jcl.filename          # Successor
                        ])

        if flow_rows:
            # Deduplicate
            unique_flows = [list(x) for x in set(tuple(x) for x in flow_rows)]
            unique_flows.sort(key=lambda x: x[0])
            
            self.table(
                ["Predecessor", "Logic", "Shared Data", "Successor"], 
                unique_flows[:40], 
                [40*mm, 50*mm, 50*mm, 40*mm]
            )
            if len(unique_flows) > 40:
                self.para(f"<i>...and {len(unique_flows)-40} additional dependency chains.</i>")
        else:
            self.para("No file-based job dependencies detected.")

        self.h2("3.3 Core Functional Groups")
        tx_progs = []
        rpt_progs = []
        maint_progs = []

        for p in self.code_files:
            raw_text = (str(p.technical_analysis) + str(p.business_overview)).upper()
            
            if any(x in raw_text for x in ['BACKUP', 'ARCHIVE', 'DELETE', 'CLEANUP', 'PURGE', 'UTILITY', 'REORG', 'COPY', 'MAINTENANCE']):
                maint_progs.append(p.filename)
            elif any(x in raw_text for x in ['REPORT', 'PRINT', 'DISPLAY', 'EXTRACT', 'SYSOUT']):
                rpt_progs.append(p.filename)
            elif any(x in raw_text for x in ['INSERT', 'UPDATE', 'WRITE', 'TRANSACTION', 'CALCULATE', 'PROCESS']):
                tx_progs.append(p.filename)

        if tx_progs:
            self.h3("3.3.1 Transaction Processing Group")
            self.bullet_list(tx_progs[:15])

        if rpt_progs:
            self.h3("3.3.2 Reporting & Analysis Group")
            self.bullet_list(rpt_progs[:15])

        if maint_progs:
            self.h3("3.3.3 Data Maintenance Group")
            self.bullet_list(maint_progs[:10])

        if not tx_progs and not rpt_progs and not maint_progs:
            self.para("No distinct functional groups identified in the source code analysis.")

        self.h2("3.4 Reporting & Extraction Process")
        reports = []
        for jcl in self.jcl_files:
            for ds in jcl.technical_analysis.get('io_datasets', []):
                if isinstance(ds, dict) and ('SYSOUT' in str(ds) or '.RPT' in ds.get('dataset', '')):
                    reports.append([jcl.filename, str(ds.get('dataset', ''))])
        
        if reports:
            self.para("The following jobs perform extraction and reporting:")
            self.table(["Job", "Output"], reports[:15], [80*mm, 100*mm])
        else:
            self.para("No specific reporting processes identified.")

    def _render_detailed_logic(self):
        self.h1("4. Detailed Functional Logic")
        
        self.h2("4.1 Business Rules & Validations")
        rule_count = 0
        sorted_files = sorted(self.code_files, key=lambda x: (x.file_type, x.filename))

        for prog in sorted_files:
            rules = prog.business_overview.get('scope', [])
            if not rules:
                rules = prog.technical_analysis.get('functional_capabilities', [])
            if not rules:
                purpose = prog.business_overview.get('purpose', '')
                if purpose and len(purpose) > 20:
                    rules = [s.strip() for s in purpose.split('.') if len(s.strip()) > 10]
            clean_rules = []
            for r in rules:
                r_str = str(r)
                if any(x in r_str.upper() for x in ['CALL ', 'PERFORM ', 'EXEC SQL', 'OPEN FILE', 'CLOSE FILE']):
                    continue
                clean_rules.append(r_str)
            if clean_rules:
                if rule_count < 30: 
                    self.h3(f"Module: {prog.filename}")
                    
                    if not prog.business_overview.get('scope') and not prog.technical_analysis.get('functional_capabilities'):
                        self.para(f"<i>Summary: {clean_rules[0]}</i>")
                        if len(clean_rules) > 1:
                            self.bullet_list(clean_rules[1:])
                    else:
                        self.bullet_list(clean_rules)
                    
                    rule_count += 1

        if rule_count == 0:
            self.para("No distinct business rules could be isolated from the codebase. Refer to Technical Specification for logic details.")

        self.h2("4.2 Data Management Functions")
        inserts, updates, deletes = [], [], []
        for prog in self.code_files:
            ops = (str(prog.technical_analysis.get('key_operations')) + str(prog.technical_analysis.get('data_interactions'))).upper()
            if 'INSERT' in ops: inserts.append(prog.filename)
            if 'UPDATE' in ops or 'REWRITE' in ops: updates.append(prog.filename)
            if 'DELETE' in ops: deletes.append(prog.filename)

        self.h3("4.2.1 Create/Insert Logic")
        self.para("Programs adding new system records:")
        self.bullet_list(inserts)

        self.h3("4.2.2 Update/Maintain Logic")
        self.para("Programs maintaining existing records:")
        self.bullet_list(updates)

        self.h3("4.2.3 Logical Deletion")
        self.para("Programs handling record removal or deactivation:")
        self.bullet_list(deletes)

        self.h2("4.3 Transformations")
        transformations = []
        keywords = ['CONVERT', 'TRANSFORM', 'FORMAT', 'CALCULATE', 'MAP', 'NORMALIZE', 'REFORMAT', 'COMPUTE']
        
        for p in self.code_files:
            sources = p.business_overview.get('scope', []) + \
                      p.technical_analysis.get('functional_capabilities', [])
            
            for item in sources:
                item_str = str(item)
                if any(k in item_str.upper() for k in keywords) and len(item_str) > 10:
                    transformations.append(f"{p.filename}: {item_str}")

        transformations = sorted(list(set(transformations)))

        if transformations:
            self.para("The following data transformations and calculations were identified in the codebase:")
            self.bullet_list(transformations)
        else:
            self.para("Standard data movement (MOVE) and arithmetic (COMPUTE) handles most transformations. No complex reformatting logic was explicitly highlighted in the analysis.")

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
        backups = []
        for s in self.summaries:
            text = (str(s.technical_analysis) + str(s.business_overview)).upper()
            if any(x in text for x in ['BACKUP', 'ADRDSSU', 'REPRO', 'COPY']):
                backups.append(s.filename)
        
        if backups:
            self.para("The following modules handle backup and recovery:")
            for b in list(set(backups))[:20]: self.bullet(b)
        else:
            self.para("Backup procedures managed via system-level storage groups.")


        self.h2("6.4 Archiving Logic")
        gdg_jobs = [j.filename for j in self.jcl_files if '(+' in str(j.technical_analysis) or '(-' in str(j.technical_analysis)]
        if gdg_jobs:
            self.para("The system uses Generation Data Groups (GDG) for automated data retention:")
            self.bullet_list(gdg_jobs)

        self.h2("6.5 Error Handling Mechanisms") 
        error_routines = []
        keywords = ['ABEND', 'EXCEPTION', 'RETURN-CODE', 'STATUS CODE', 'INVALID KEY', 'ON ERROR', 'ROLLBACK']
        
        for prog in self.code_files:
            notes = prog.technical_analysis.get('technical_notes', [])
            caps = prog.technical_analysis.get('functional_capabilities', [])
            
            for note in notes + caps:
                note_str = str(note)
                if any(k in note_str.upper() for k in keywords):
                    if len(note_str) > 10:
                        error_routines.append(f"{prog.filename}: {note_str}")

        for jcl in self.jcl_files:
            notes = str(jcl.technical_analysis.get('schedule_notes', ''))
            if 'COND' in notes.upper() or 'RESTART' in notes.upper():
                error_routines.append(f"{jcl.filename}: Implements batch condition codes (COND) for step control.")

        if error_routines:
            self.para("Specific error handling strategies identified in source code and JCL:")
            unique_routines = sorted(list(set(error_routines)))
            self.bullet_list(unique_routines)
        else:
            self.para("Standard return code processing (RC checking) is assumed. No explicit custom error routines (ABEND/ROLLBACK) detected in module summaries.")

        
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
            self.table(["Generating Job / Module", "Report / Dataset Name", "Business Purpose"], report_rows[:60], [50*mm, 60*mm, 60*mm])
            if len(report_rows) > 60:
                self.para(f"<i>Note: Total of {len(report_rows)} reports identified. First 60 items shown.</i>")
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