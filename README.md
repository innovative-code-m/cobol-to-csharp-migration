# COBOL to C# Migration Framework

A **practical repository for systematic COBOL to C# (.NET) migration** focused on 
**transformation rules**, **audit materials**, and **verification processes** 
that can be continuously developed and refined on GitHub.

This repository follows the principle of **not rushing into building conversion tools**. 
Instead, it prioritizes **establishing transformation rules and audit loops first**, 
then gradually progressing toward automation and semi-automation.

The framework particularly focuses on **preventing migration accidents** in 
COBOL-specific challenging areas:

- REDEFINES
- OCCURS / OCCURS DEPENDING ON  
- COPYBOOK
- Aggregation, digits, signs, and edited fields

---

## Objectives

- Develop **business-ready deliverables for sales and proposals** 
  in COBOL → C# replacement projects
- Minimize migration risks (specification misunderstandings, data inconsistencies, migration gaps) 
  through **audit and verification mechanisms**
- Build a foundation of rule sets, samples, and verification procedures 
  for gradual expansion into **semi-automated conversion tools**

---

## Deliverables

### 1. Transformation Rule Set
- Numbered rules (R-001...) for converting COBOL syntax and conventions to C#
- Each rule includes:
  - Scope of application
  - Transformation approach
  - Acceptance criteria
  - Before/After examples
  - Notes and unresolved issues

### 2. Audit Samples
- Test materials for verifying rule comprehensiveness and validity
- Managed sets of COBOL + Copybook + input data + expected results

### 3. Verification Framework
- Validation of correctness after file → DBMS migration
- Approaches, procedures, and tool strategies for record counting, 
  aggregate comparison, and difference detection

### 4. Migration Support Tools (Future Extension)
- Rule-based conversion assistance
- Static analysis
- Check and test harness capabilities

---

## Version Management Policy (Tags)

This repository **fixes implementation and verification milestones with Git tags**.

- Naming Convention: `YYYY-MM-DD_NN_Implement` (e.g., `2026-02-11_01_Implement`)

- Examples:
  - `2026-02-03_01_Implement`
    - Snapshot of OrderValidation implementation and related rules/audit deliverables

The purpose is to make it reproducible on a tag-by-tag basis:
- "What was finalized at this point"
- "How far verification had progressed"

---

## Repository Structure

```
cobol-to-csharp-migration/
├── README.md
├── PROJECT_STATUS.md                # Current project status
├── PROJECT_STATUS_JP.md             # Detailed project status (Japanese)
├── .gitignore
├── .gitattributes
├── CobolToCsharpMigration.sln
│
├── .ai/                             # AI development support
│   ├── context/
│   │   └── glossary.md
│   └── tasks/
│       ├── backlog.md
│       └── pr-checklist.md
│
├── .claude/                         # Claude AI settings
│   ├── settings.json
│   └── settings.local.json
│
├── .vscode/                         # VS Code / Cursor settings
│   ├── extensions.json
│   ├── settings.json
│   ├── tasks.json
│   └── README-ai-usage.md
│
├── apps/                            # Applications
│   └── backend/                     # C# backend
│       ├── CobolToCsharpMigration.csproj
│       ├── src/
│       │   ├── CobolMvpRuntime/     # MVP COBOL→C# runtime
│       │   │   ├── CobolMvpRuntime.csproj
│       │   │   ├── Mvp01Program.cs
│       │   │   └── Mvp02Program.cs
│       │   └── OrderService.cs
│       └── tests/
│           ├── CobolMvpRuntimeMvp01Tests.cs
│           ├── CobolMvpRuntimeMvp02Tests.cs
│           └── OrderServiceTests.cs
│
├── docs/                            # Documentation
│   ├── rules/                       # Transformation rules
│   │   └── CobolToCsharpRules.md
│   ├── audit/                       # Audit materials
│   │   ├── CoverageMatrix.md
│   │   ├── TodoInventory.md
│   │   ├── TodoInventory.csv
│   │   ├── MissingList.md
│   │   └── ProposedRules.md
│   ├── samples/                     # Sample programs
│   │   ├── mvp01/
│   │   │   ├── cobol/MVP01.cbl
│   │   │   ├── input/INFILE.DAT
│   │   │   └── expected/OUTFILE_expected.DAT
│   │   └── mvp02/
│   │       ├── cobol/MVP02.cbl
│   │       ├── input/INFILE.DAT
│   │       ├── expected/OUTFILE_expected_todo.DAT
│   │       └── expected/OUTFILE_ideal.DAT
│   └── prompts/                     # AI execution logs
│       └── exec/
│           ├── 2026-02-03_01_Implement.OrderValidation.md
│           ├── 2026-02-08_01_Implement_CobolMvpRuntime.md
│           └── 2026-02-11_01_Implement_TodoHandling.md
│
├── log/                             # Development logs
│   └── working-log/                 # Daily logs (Japanese)
│       ├── 2026-02-13.md
│       ├── 2026-02-14.md
│       └── 2026-02-15.md
│
└── tools/                           # Migration tools
    └── verifier/
        └── extract-todos.ps1        # TODO extraction script
```


---

## Key Features

### 🎯 **Systematic Approach**
- Rule-based transformation methodology
- Comprehensive audit and verification framework
- Incremental implementation with MVP milestones

### 🔍 **Transparency & Traceability**
- All transformation rules documented with examples
- TODO tracking for incomplete implementations
- Daily development logs for process transparency

### 🛠️ **Practical Implementation**
- Working C# runtime for COBOL constructs
- Unit tests with real COBOL sample validation
- PowerShell tools for automated verification
- For script execution, `pwsh` (PowerShell 7+) is recommended.

### 📊 **Coverage Visibility**
- Implementation coverage matrix
- Missing functionality identification
- Progress tracking through Git tags

---

## Usage Scenarios

- **Pre-investigation and proposals** for COBOL → C# replacement projects
- **Structure analysis and risk assessment** of existing COBOL assets
- **Verification and construction** of AI-assisted rule-based migration workflows
- **Technical portfolio** for migration projects

---

## Development Philosophy

- Prioritize **reproducibility and explainability** over conversion accuracy
- Document **"why this transformation"** as rules
- Use AI for **implementation, audit, and organization role distribution**
- Maintain **transparency in development process** through comprehensive logging

---

## Current Status

**Phase**: MVP14 — File I/O scope (sequential READ / WRITE integration)  
**Implemented (through MVP14)**: MOVE, DISPLAY, INSPECT, UNSTRING, EVALUATE (THRU / OTHER / ALSO), PERFORM (UNTIL / VARYING), sequential file READ (AT END) / WRITE, and a migration-trace guarantee layer — each landed with transformation rules, samples, and unit tests, tagged per milestone.  
**Next**: Rule system formalization and coverage expansion.

For detailed status information, see:
- [PROJECT_STATUS.md](PROJECT_STATUS.md) - English overview
- [PROJECT_STATUS_JP.md](PROJECT_STATUS_JP.md) - Detailed Japanese status
- [log/working-log/](log/working-log/) - Daily development logs
