# PROJECT STATUS
cobol-to-csharp-migration

Last Updated: 2026-03-13
Phase: MVP14 – File I/O scope (sequential READ / WRITE integration)

---

## 🎯 Mission

To build a **verifiable, rule-driven COBOL→C# migration framework**
designed for real-world legacy modernization projects.

This repository is not a simple code converter.
It is a structured migration methodology prototype.

---

## 🔍 Current Focus

- Sequential file I/O (READ AT END / WRITE) and READ→WRITE integration
- Migration-trace guarantee layer
- Improving TODO-based missing construct detection
- Expanding unit test coverage

---

## 🧱 Architecture Overview

Runtime:
- `apps/backend/src/CobolMvpRuntime/`

Tests:
- `apps/backend/tests/CobolMvpRuntimeTests/`

Verification:
- `tools/verifier/extract-todos.ps1`

Documentation:
- Rule definitions
- Coverage matrix

---

## 📊 Coverage Snapshot

Implemented through MVP14 (see `docs/audit/CoverageMatrix.md` for the authoritative matrix):

| Area | Status |
|------|--------|
| MOVE / DISPLAY | Implemented |
| INSPECT / UNSTRING | Implemented |
| EVALUATE (THRU / OTHER / ALSO) | Implemented |
| PERFORM (UNTIL / VARYING) | Implemented |
| Sequential file READ (AT END) / WRITE | Implemented |
| Migration-trace guarantee layer | Foundation |

---

## 🚀 Strategic Direction

The long-term goal is to establish:

- Structured transformation rules
- Coverage visibility
- Migration verification workflow
- AI-assisted engineering process

---

## 📂 Detailed Logs

Daily development logs are maintained in:

`log/working-log/` (Japanese)

---

Status: In Progress
Stability: Controlled Experimental
