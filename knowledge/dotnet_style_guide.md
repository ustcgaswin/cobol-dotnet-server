# C# Code Style Guide

## Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Class | PascalCase | `PayrollService` |
| Interface | IPascalCase | `IPayrollService` |
| Method | PascalCase | `CalculateGrossPay()` |
| Property | PascalCase | `EmployeeId` |
| Private field | _camelCase | `_employeeCount` |
| Parameter | camelCase | `employeeId` |
| Local variable | camelCase | `totalAmount` |
| Constant | PascalCase | `MaxRetryCount` |

## File Structure

Each service should follow this structure:
```csharp
// 1. File header comment
// 2. Using statements
// 3. Namespace
// 4. Class/Interface

using System;
using Microsoft.Extensions.Logging;

namespace ConvertedBatch.Core.Services;

/// <summary>
/// [Brief description of service purpose]
/// Converted from: [Original COBOL program name]
/// </summary>
public class PayrollService : IPayrollService
{
    // Private fields (from WORKING-STORAGE)
    private readonly ILogger<PayrollService> _logger;
    private decimal _wsTotal;
    
    // Constructor
    public PayrollService(ILogger<PayrollService> logger)
    {
        _logger = logger;
    }
    
    // Public methods (main entry points)
    public void Execute() { }
    
    // Private methods (from PROCEDURE DIVISION paragraphs)
    private void ValidateInput() { }
}
```

## Documentation

### XML Comments Required For:
- All public classes and interfaces
- All public methods
- Include `<summary>`, `<param>`, `<returns>` as applicable

### TODO Format
When uncertain about conversion, use:
```csharp
// TODO: [COBOL-REVIEW] Original logic unclear - verify with business
// Original COBOL: PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
```

### Comments for Traceability
Link back to original COBOL:
```csharp
/// <summary>
/// Validates employee input record.
/// </summary>
/// <remarks>
/// Converted from: PAYROLL.cbl, paragraph 1000-VALIDATE-INPUT (lines 234-289)
/// </remarks>
private void ValidateInput()
{
    // ...
}
```

## Project Structure

```
src/
├── Core/                    # Business logic, no external dependencies
│   ├── Entities/            # POCOs from copybooks
│   ├── Services/            # Service implementations
│   ├── Interfaces/          # Service interfaces  
│   └── Enums/              # Enums from 88-levels
├── Infrastructure/          # External dependencies
│   ├── Data/               # DbContext, repositories
│   └── Storage/            # File I/O implementations
└── Worker/Jobs/            # Console apps per JCL step
```

## Interface Pattern

Every service should have an interface:
```csharp
public interface IPayrollService
{
    void Execute();
    decimal CalculateGrossPay(int hours, decimal rate);
}
```

## Dependency Injection

Services receive dependencies via constructor:
```csharp
public class PayrollService : IPayrollService
{
    private readonly ILogger<PayrollService> _logger;
    private readonly ITaxCalculationService _taxService;
    private readonly IEmployeeRepository _employeeRepo;
    
    public PayrollService(
        ILogger<PayrollService> logger,
        ITaxCalculationService taxService,
        IEmployeeRepository employeeRepo)
    {
        _logger = logger;
        _taxService = taxService;
        _employeeRepo = employeeRepo;
    }
}
```

## Error Handling

Use typed exceptions where appropriate:
```csharp
public class RecordNotFoundException : Exception
{
    public RecordNotFoundException(string recordType, string key)
        : base($"{recordType} not found: {key}") { }
}
```

Log errors with context:
```csharp
_logger.LogError(ex, "Failed to process employee {EmployeeId}", employeeId);
```
