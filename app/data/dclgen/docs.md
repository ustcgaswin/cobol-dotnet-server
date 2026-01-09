# DCLGEN (Declarations Generator) in COBOL-DB2 Programming

## Overview

DCLGEN stands for **Declarations Generator**. It is a utility in IBM DB2 (primarily for z/OS mainframes) and compatible environments (for example, Micro Focus/OpenText Enterprise Developer with Host Compatibility Option for DB2) that automatically generates:

- An **SQL DECLARE TABLE** statement for a specified DB2 table or view
- A corresponding **COBOL host variable structure** matching the table’s columns
- Optionally, **indicator variables** for handling NULL values in nullable columns

The generated output is stored in a copybook (a member in a PDS or PDSE library). This copybook can be included in COBOL-DB2 programs to ensure accurate mapping between DB2 columns and COBOL variables, significantly reducing manual coding errors.

DCLGEN can be invoked via DB2 Interactive (DB2I) panels, batch JCL, or tools such as `mfhcodcl` in Micro Focus environments.

---

## Purpose and Benefits

- **Automation**  
  Eliminates manual declaration of table structures and host variables.

- **Accuracy**  
  Ensures DB2 column definitions and COBOL PIC clauses remain compatible in type and length.

- **NULL Handling**  
  Generates indicator variables (PIC S9(4) COMP) for nullable columns.

- **Consistency**  
  Enables reuse of copybooks across multiple programs; regenerate when the table definition changes.

- **Error Reduction**  
  Prevents common SQL runtime errors such as `-305` (NULL value without indicator) and data truncation issues.

Without DCLGEN, developers must manually code these declarations, which is error-prone and difficult to maintain.

---

## Generating DCLGEN

### IBM DB2 for z/OS

- **Online (DB2I)**  
  Accessed via ISPF → DB2I → option 2 (DCLGEN).  
  Developers specify:
  - Table or view name
  - Output dataset/member
  - Target language (COBOL)
  - Indicator variable option (YES/NO)

- **Batch (JCL)**  
  Executed using the DB2 DSN command processor with parameters such as:
  - `TABLE`
  - `LIBRARY`
  - `LANGUAGE(IBMCOB)`

### Micro Focus / OpenText (HCO for DB2)

- Generated via GUI tools or the command-line utility `mfhcodcl`
- Supports advanced customization, including:
  - Variable name prefixes or suffixes
  - Indicator naming conventions
  - Numbered field names instead of column-based names

---

## Key Features and Customizations

### Host Variable Naming

- Underscores (`_`) in DB2 column names are converted to hyphens (`-`) in COBOL variables.
- Optional prefixes (for example, table name or a custom prefix like `HV-`).
- In Micro Focus environments:
  - Custom indicator suffixes (for example, `-IND`)
  - Numbered fields (for example, `FIELD-001`)

### Data Type Mapping (Common Examples)

| DB2 Data Type | COBOL PIC Clause | Notes |
|--------------|------------------|------|
| CHAR(n) | PIC X(n) | Fixed length |
| VARCHAR(n) | Group with<br>49 LEN PIC S9(4) COMP<br>49 TEXT PIC X(n) | Length + data |
| SMALLINT | PIC S9(4) COMP | |
| INTEGER | PIC S9(9) COMP | |
| DECIMAL(p,s) | PIC S9(p-s)V9(s) COMP-3 | Packed decimal |
| DATE | PIC X(10) | |
| TIMESTAMP | PIC X(26) | |

### Indicator Variables

- Generated only for nullable columns
- Typically defined as `PIC S9(4) COMP`
- A value of `-1` indicates a NULL value

---

## Structure of Generated Output

A typical DCLGEN copybook contains:

1. Comment header with generation details and options
2. `EXEC SQL DECLARE <table> TABLE ... END-EXEC`
3. Host variable structure (01-level group)
4. Indicator structure (if enabled)

---

### Example 1: IBM z/OS Style (EMP Table)

```cobol
EXEC SQL DECLARE EMP TABLE
( EMP_ID    CHAR(5)      NOT NULL,
  EMP_NAME  VARCHAR(30)  NOT NULL,
  SALARY    DECIMAL(9,2),
  DEPT      CHAR(4)
) END-EXEC.

01  DCL-EMP.
    10 EMP-ID           PIC X(5).
    10 EMP-NAME.
       49 EMP-NAME-LEN  PIC S9(4) COMP.
       49 EMP-NAME-TEXT PIC X(30).
    10 SALARY           PIC S9(7)V9(2) COMP-3.
    10 DEPT             PIC X(4).

01  DCL-EMP-IND.
    10 IND-SALARY       PIC S9(4) COMP.
    10 IND-DEPT         PIC S9(4) COMP.

```


### Example 2: Micro Focus Style with Table Prefix (DEMO.ITEMS Table)

```cobol
* MFHCODCL OPTIONS:
* DATABASE : DB2DEMO
* SCHEMA   : DEMO
* PREFIX   : TABLE NAME

EXEC SQL DECLARE DEMO.ITEMS TABLE
( ORD_NO       INTEGER      NOT NULL,
  ITEM_NO      SMALLINT     NOT NULL,
  PROD_ID      CHAR(4),
  QTY_ORDERED  SMALLINT     NOT NULL WITH DEFAULT,
  ...
  ITEM_NOTES   VARCHAR(560)
) END-EXEC.

01 DCLITEMS.
   10 ITEMS-ORD-NO              PIC S9(9) COMP.
   10 ITEMS-ITEM-NO             PIC S9(4) COMP.
   10 ITEMS-PROD-ID             PIC X(4).
   ...
   10 ITEMS-ITEM-NOTES.
      49 ITEMS-ITEM-NOTES-LEN   PIC S9(4) COMP.
      49 ITEMS-ITEM-NOTES-TEXT  PIC X(560).

01 DCLITEMS-NULL.
   10 ITEMS-PROD-ID-NULL        PIC S9(4) COMP.
   ...
   10 ITEMS-ITEM-NOTES-NULL     PIC S9(4) COMP.

```



## Using DCLGEN in COBOL-DB2 Programs

The generated copybook is included in the **WORKING-STORAGE SECTION** using `EXEC SQL INCLUDE`.  
This inclusion is processed by the **DB2 precompiler**, not by the COBOL compiler’s `COPY` statement.

```cobol
WORKING-STORAGE SECTION.

EXEC SQL INCLUDE SQLCA END-EXEC.   *> SQL communication area

EXEC SQL INCLUDE DCLEMP END-EXEC.  *> DCLGEN member

EXEC SQL
    SELECT EMP_NAME, SALARY
    INTO :DCL-EMP.EMP-NAME,
         :DCL-EMP.SALARY :IND-SALARY
    FROM EMP
    WHERE EMP_ID = :WS-INPUT-ID
END-EXEC.

IF IND-SALARY = -1
    *> Handle NULL
END-IF.

```

## Best Practices

- Store DCLGEN copybooks in a shared copybook library
- Regenerate DCLGEN after any table alteration
- Always use indicator variables for nullable columns
- Prefix variables in multi-table programs to avoid ambiguity
- Use `EXEC SQL INCLUDE` instead of COBOL `COPY` so the DB2 precompiler processes the code correctly
