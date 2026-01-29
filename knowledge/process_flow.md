# FIN-SETTLE-PRO - Global Merchant Settlement System Technical Documentation

**Version:** Production Suite (Artifacts dated 2026-01-09)  
**Author:** Mainframe-Suite-Gen (Generated Suite)  
**System Context:** Finance Batch Processing Pipeline for Merchant Settlements  
**Core Design Principles:**  
- High-volume transaction processing aligned to ISO 8583 standards.  
- Monetary precision: 13 digits total (e.g., S9(11)V99 COMP-3 in COBOL, FIXED DECIMAL(13,2) in PL/I).  
- Multi-language integration: COBOL (core logic), PL/I (date/math utilities), Assembler (PII masking), REXX (pre/post utilities).  
- Defensive programming: Exhaustive validation, retry mechanisms for DB2 deadlocks (up to 3 attempts), verbose logging.  
- Data persistence: VSAM for merchant masters (KSDS with dynamic access), DB2 for audits and contracts.  
- Scheduling: CA-7 automated daily runs (Mon-Fri, 02:00 submit).  
- Compliance: PAN masking, RACF authorization, audit trails per transaction.  

This documentation provides a comprehensive overview of the entire application suite, derived from all provided source artifacts. It covers architecture, components, flows, configurations, and operational details to enable full understanding and maintenance without referencing original sources.

## 1. System Overview and Architecture

FIN-SETTLE-PRO is a mainframe-based batch settlement system designed for processing merchant transactions in a financial environment. It handles incoming ISO 8583-formatted transaction files, performs data validation and scrubbing, calculates fees and taxes based on merchant categories (MCC), updates balances in VSAM datasets, logs audits to DB2 tables, generates reports, and performs post-processing reconciliation and backups.

### Key Features:
- **Transaction Processing:** Reads sequential input files, masks sensitive PAN data, validates numerics and structures, applies business rules for fees/taxes, and commits updates atomically to VSAM and DB2.
- **Fee and Tax Engine:** Tiered and category-specific calculations with seasonal adjustments (e.g., December surcharges).
- **Error Resilience:** Non-fatal rejects for invalid records, fatal abends for critical issues (e.g., file open failures), retry loops for transient DB2 errors like -911 (deadlock).
- **Integration Points:** Inter-language calls for specialized functions; DB2 static SQL via bound plans; VSAM with buffered access.
- **Reporting and Monitoring:** CSV settlement reports, REXX-based stats, CA-7 scheduling with dependencies.
- **Scalability Notes:** Handles batches up to 50,000+ records (with warnings for large sets); VSAM configured for 64K buffers and 10 strings.
- **Environment:** Production (PROD) datasets; DB2 subsystem DB2P; JCL class A, MSGCLASS X.

### High-Level Components:
- **Core Batch Job:** SETLJOB (main pipeline).
- **Dependent Jobs:** SETLRECO (reconciliation), SETLBKUP (backup).
- **Procedures:** PROCSETL (main exec), PROCRPT (report), PROCFX (FX mode variant).
- **Databases/Files:** DB2 tables (MERCHANT_CONTRACTS, TRANSACTION_AUDIT), VSAM KSDS (MERCH.MASTER), sequential inputs/outputs.
- **Utilities:** REXX scripts for checks/stats, IDCAMS for backups, DB2 binds for SQL access.

## 2. Detailed Processing Flow

### 2.1 Main Settlement Pipeline (SETLJOB)
```jcl
//SETLJOB  JOB (FINANCE),'SETTLE MASTER',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
```
- **STEP01: Pre-Flight Integrity Check (REXX - FINCHECK)**  
  - Validates dataset existence, record count, and header integrity.

- **STEP02: Execute Main Settlement Engine (PROCSETL - FSMAIN COBOL)**  
  - Full transaction validation, PAN masking, DB2 contract lookup, fee/tax calculation, VSAM/DB2 updates.

- **STEP03: Generate Settlement Report (FSRPT COBOL)**  
  - Produces CSV: MERCH-ID, MERCH-NAME, BALANCE-YTD.

- **STEP04: Post-Run Performance Stats (REXX - JOBSTATS)**  
  - Simulated SMF-like output.

### 2.2 Dependent Flows (CA-7)
- SETLRECO (FSDB2)
- SETLBKUP (IDCAMS REPRO)

## 3. Programs and Modules

| Program | Language | Description |
|--------|----------|-------------|
| FSMAIN | COBOL | Main orchestrator for validation, fee/tax calculation, VSAM/DB2 updates |
| FSFEE | COBOL | MCC-based fee engine with surcharges and adjustments |
| FSDB2 | COBOL | Reconciliation program comparing DB2 vs VSAM totals |
| FSRPT | COBOL | CSV report generator |
| VALMATH | PL/I | Tiered tax calculator |
| VALDATE | PL/I | Lilian → ISO date converter |
| IDMASK | Assembler | PAN masking routine |
| FINCHECK | REXX | Pre-flight validator |
| JOBSTATS | REXX | Stats printer |

## 4. Data Structures (Copybooks/DCLGEN)
- ISO8583 transaction structure
- MERCHREC VSAM record
- DCLMERCH (MERCHANT_CONTRACTS)
- DCLTRANS (TRANSACTION_AUDIT)
- ERRBLOCK for standardized error handling
- WORKAREA scratchpad
- SETLREC batch totals

## 5. Files, Databases, Configurations
- Input: `PROD.ISO8583.INPUT`
- Output: `PROD.SETTLE.REPT(+1)`
- VSAM: `PROD.MERCH.MASTER.KSDS`
- DB2 subsystem: DB2P (tables: MERCHANT_CONTRACTS, TRANSACTION_AUDIT)
- PARMLIB: BUSRULES, SYSCONF

## 6. Error Handling and Recovery
- ERRBLOCK used system-wide
- SQL retry logic for -911 (max 3)
- Non-fatal/fatal path separation
- VSAM status code handling

## 7. Scheduling (CA-7)
- SETLJOB runs Mon–Fri @ 02:00–02:15
- Reco/Backup triggered on success

## 8. Inter-Language Interfaces
- COBOL ↔ PL/I
- COBOL ↔ Assembler
- COBOL ↔ DB2 (static SQL)
- REXX via IKJEFT01

## 9. Precision, Rounding, Business Logic
- COMP-3 monetary fields
- Tiered tax thresholds (50k/100k)
- MCC + network + seasonal fee logic

## 10. Reporting (FSRPT)
- CSV 150-byte fixed-length output
- Header + Details + Trailer

## 11. Security & Compliance
- PAN masking
- RACF auth
- Audit trails
- DB2 isolation: CS

## 12. Restartability & Maintenance
- Restartable CA-7 job
- Daily VSAM backup (GDG)
- Verbose logging for troubleshooting
