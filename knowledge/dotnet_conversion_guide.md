# Conversion Guide: COBOL to C#

## Data Type Mappings

| COBOL | C# | Notes |
|-------|-----|-------|
| `PIC 9(n)` | `int` or `long` | Use `long` if n > 9 |
| `PIC 9(n)V9(m)` | `decimal` | Always use decimal for money |
| `PIC X(n)` | `string` | Pad/trim as needed |
| `PIC S9(n) COMP-3` | `decimal` | Packed decimal → decimal |
| `PIC S9(n) COMP` | `int` or `long` | Binary |
| `88-level` | `enum` | Group related conditions |
| `OCCURS n TIMES` | `List<T>` or `T[]` | Prefer List for flexibility |
| `REDEFINES` | See pattern below | Requires careful analysis |

---

## Control Flow Patterns

### PERFORM → Method Call

**COBOL:**
```cobol
PERFORM 1000-VALIDATE-INPUT.
```

**C#:**
```csharp
ValidateInput();
```

### PERFORM THRU → Wrapper Method

**COBOL:**
```cobol
PERFORM 1000-START THRU 1999-END.
```

**C# Pattern:** Create a wrapper method that calls each paragraph in sequence:
```csharp
private void PerformStartThruEnd()
{
    Para1000Start();
    Para1100Process();
    // ... all paragraphs in range
    Para1999End();
}
```

### GO TO DEPENDING ON → Switch

**COBOL:**
```cobol
GO TO PARA-A PARA-B PARA-C DEPENDING ON WS-INDEX.
```

**C#:**
```csharp
switch (wsIndex)
{
    case 1: ParaA(); break;
    case 2: ParaB(); break;
    case 3: ParaC(); break;
}
```

---

## Anti-Patterns (DON'T DO)

### ❌ Don't use float/double for money
**Wrong:**
```csharp
float amount = 123.45f; // Precision loss!
```
**Correct:**
```csharp
decimal amount = 123.45m;
```

### ❌ Don't use Math.Round for COBOL truncation
COBOL truncates, doesn't round.

**Wrong:**
```csharp
decimal result = Math.Round(value, 2);
```
**Correct:**
```csharp
decimal result = Math.Truncate(value * 100) / 100;
```

### ❌ Don't use static fields for WORKING-STORAGE
Use instance fields to maintain state within a service.

**Wrong:**
```csharp
public static decimal WsTotal; // Shared across instances!
```
**Correct:**
```csharp
private decimal _wsTotal; // Instance field
```

---

## File I/O Patterns

### Sequential Read

**COBOL:**
```cobol
READ INPUT-FILE INTO WS-RECORD
    AT END SET END-OF-FILE TO TRUE.
```

**C#:**
```csharp
using var reader = new StreamReader(inputPath);
string? line;
while ((line = reader.ReadLine()) != null)
{
    var record = ParseRecord(line);
    // process
}
```

### Fixed-Length Records

**C#:**
```csharp
byte[] buffer = new byte[80];
int bytesRead = stream.Read(buffer, 0, 80);
string record = Encoding.ASCII.GetString(buffer);
```

---

## SQL Patterns

### EXEC SQL SELECT → EF Core

**COBOL:**
```cobol
EXEC SQL
    SELECT EMP_NAME, EMP_SALARY
    INTO :WS-NAME, :WS-SALARY
    FROM EMPLOYEE
    WHERE EMP_ID = :WS-EMP-ID
END-EXEC.
```

**C#:**
```csharp
var employee = await _dbContext.Employees
    .Where(e => e.EmpId == wsEmpId)
    .Select(e => new { e.EmpName, e.EmpSalary })
    .FirstOrDefaultAsync();

if (employee != null)
{
    wsName = employee.EmpName;
    wsSalary = employee.EmpSalary;
}
```

---

## REDEFINES Pattern

When a field is REDEFINEd, create a class that can parse/interpret the same bytes differently:

**COBOL:**
```cobol
01 WS-DATE.
   05 WS-DATE-NUM    PIC 9(8).
01 WS-DATE-PARTS REDEFINES WS-DATE.
   05 WS-YEAR        PIC 9(4).
   05 WS-MONTH       PIC 9(2).
   05 WS-DAY         PIC 9(2).
```

**C#:**
```csharp
public class DateRecord
{
    public string RawDate { get; set; } // 8 chars
    
    public int Year => int.Parse(RawDate[..4]);
    public int Month => int.Parse(RawDate[4..6]);
    public int Day => int.Parse(RawDate[6..8]);
}
```

---

## Parameterized Copybooks (REPLACING)

Copybooks may use placeholder tags that get replaced at compile time:

**COBOL Copybook (CUSTCOPY.cpy):**
```cobol
01 :TAG:-CUSTOMER-REC.
   05 :TAG:-CUST-ID    PIC 9(10).
   05 :TAG:-CUST-NAME  PIC X(50).
```

**Usage:**
```cobol
COPY CUSTCOPY REPLACING ==:TAG:== BY ==INP==.
COPY CUSTCOPY REPLACING ==:TAG:== BY ==OUT==.
```

**C# Pattern:** Generate ONE class without the tag prefix:
```csharp
public class CustomerRecord
{
    public long CustId { get; set; }
    public string CustName { get; set; }
}
```

- Do NOT create separate classes for each REPLACING usage (e.g., no `InpCustomerRecord`, `OutCustomerRecord`)
- Strip the placeholder tag (`:TAG:`, `:PREFIX:`, etc.) from field names
- If truly needed for type safety, create empty derived classes inheriting from one base

---

## Source File Format Notes

COBOL source files use fixed-format columns:
- **Columns 1-6**: Sequence numbers (ignore these)
- **Column 7**: Indicator (`*` = comment, `-` = continuation)
- **Columns 8-72**: Actual source code
- **Columns 73-80**: Identification/sequence area (ignore these)

**Important:** Numbers appearing at the end of lines (columns 73-80) are sequence numbers, not part of the code. Ignore them during conversion.

---

## Flagged Constructs (Require Human Review)

| Construct | Reason |
|-----------|--------|
| `ALTER` verb | Self-modifying code, cannot convert |
| Complex `GO TO` networks | Unstructured flow |
| `EXEC CICS` | Out of scope |
| Dynamic SQL (`EXECUTE IMMEDIATE`) | Runtime query building |
 