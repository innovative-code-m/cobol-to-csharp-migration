# Rule Definitions
COBOL to C# Migration Framework

Last Updated: 2026-01-30
Version: MVP02

---

## Overview

This document defines the transformation rules for converting COBOL constructs to C# equivalents. Each rule is designed to preserve business logic while adapting to modern C# patterns and practices.

---

## Rule Categories

### R-001 Series: Data Movement Operations

#### R-001-01: MOVE Statement
**COBOL Pattern:**
```cobol
MOVE source-field TO target-field
```

**C# Transformation:**
```csharp
targetField = sourceField;
```

**Implementation Status:** ✅ Implemented
**Test Coverage:** Complete

---

### R-002 Series: String Operations

#### R-002-01: UNSTRING Statement
**COBOL Pattern:**
```cobol
UNSTRING input-string 
    DELIMITED BY delimiter
    INTO field1 field2 ...
```

**C# Transformation:**
```csharp
string[] parts = inputString.Trim().Split(new[] { delimiter }, StringSplitOptions.RemoveEmptyEntries);
field1 = parts.Length > 0 ? parts[0] : string.Empty;
field2 = parts.Length > 1 ? parts[1] : string.Empty;
// ... additional fields as needed
```

**Implementation Status:** ✅ Implemented
**Test Coverage:** Basic scenarios covered
**Known Limitations:** 
- Multiple delimiter patterns not fully supported
- Complex INTO clause variations pending

#### R-002-02: INSPECT Statement
**COBOL Pattern:**
```cobol
INSPECT field-name CONVERTING source-chars TO target-chars
```

**C# Transformation:**
```csharp
fieldName = fieldName.ToUpperInvariant(); // for lowercase to uppercase conversion
// Additional character conversion logic as needed
```

**Implementation Status:** ✅ Implemented
**Test Coverage:** Basic case conversion
**Known Limitations:**
- Only supports lowercase to uppercase conversion currently
- Complex character replacement patterns pending

---

### R-003 Series: I/O Operations

#### R-003-01: DISPLAY Statement
**COBOL Pattern:**
```cobol
DISPLAY field-name
```

**C# Transformation:**
```csharp
Console.WriteLine(fieldName);
```

**Implementation Status:** ✅ Implemented
**Test Coverage:** Complete

---

## Rule Implementation Guidelines

### Transformation Principles

1. **Business Logic Preservation**: All transformations must preserve the original business logic intent
2. **Type Safety**: Leverage C# type system for improved reliability
3. **Performance Consideration**: Optimize for modern runtime characteristics
4. **Maintainability**: Generate readable, maintainable C# code

### Error Handling Strategy

- Use exceptions for unrecoverable errors
- Implement validation for data type conversions
- Preserve COBOL error handling semantics where applicable

### Testing Requirements

Each rule must include:
- Unit tests for typical scenarios
- Edge case validation
- Error condition handling
- Performance benchmarks (where applicable)

---

## Rule Versioning

Rules follow semantic versioning:
- Major version: Breaking changes to transformation logic
- Minor version: New rule additions or enhancements
- Patch version: Bug fixes and clarifications

---

## Extension Points

### Custom Rule Integration

The framework supports custom rule definitions for:
- Organization-specific COBOL patterns
- Legacy system peculiarities
- Domain-specific transformations

### Rule Validation

All rules undergo:
- Automated testing
- Manual review
- Production validation (where possible)

---

## Future Rule Categories

Planned rule categories for future releases:

- **R-004 Series**: File Operations (READ, WRITE, OPEN, CLOSE)
- **R-005 Series**: Control Flow (IF, PERFORM, GO TO)
- **R-006 Series**: Arithmetic Operations
- **R-007 Series**: Data Definition (PIC clauses, OCCURS)
- **R-008 Series**: Program Structure (DIVISION, SECTION)

---

Status: Active Development
Stability: MVP Level
Next Review: After MVP02 stabilization