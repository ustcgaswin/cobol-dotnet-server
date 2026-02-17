# COBOL to Java Conversion Guide

## Data Types
- `PIC 9` -> `int` or `long`
- `PIC 9(v)` -> `BigDecimal` (Recommended for financial)
- `PIC X` -> `String`

## Logic
- `PERFORM` -> Method Call
- `IF` -> `if` statement
- `EVALUATE` -> `switch` statement (Java 17 switch expressions recommended)

## Database
- `EXEC SQL` -> Spring Data JPA Repository methods
- `CURSOR` -> `Stream<Entity>` or `List<Entity>` via Repository

## File I/O
- `READ` -> `Files.readAllLines` or `Scanner`
- `WRITE` -> `Files.write` (with `StandardOpenOption.APPEND`)
