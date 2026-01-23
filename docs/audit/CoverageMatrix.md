# Coverage Matrix: COBOL to C# Migration Audit (v1)

| 要素区分 | 出現パターン / 要素名 | 出現箇所 | 対応RuleID | 判定 |
|:---|:---|:---|:---|:---|
| **Data** | `PAT-DATA-REDEFINES`: 型の再定義 | CUSTREC.cpy | R-003, R-020 | **TODO** |
| **Data** | `PAT-DATA-ENUM-88`: 条件名 | CUSTREC.cpy | R-024 | **OK** |
| **Data** | `PAT-DATA-PACKED`: COMP-3 (内部10進) | ORDREC.cpy | R-021 | **OK** |
| **Data** | `PAT-DATA-ODO`: OCCURS DEPENDING ON | ORDREC.cpy | R-026 | **TODO** |
| **Data** | `PAT-DATA-EDITED`: 編集用PIC (Z,ZZ9等) | BILLING.cbl | 未定義 | **未定義** |
| **Proc** | `PAT-PROC-FILE-LOOP`: 基本的なI/Oループ | CUSTSUM.cbl | R-036, R-050 | **OK (スタブ)** |
| **Proc** | `PAT-PROC-EVAL-TRUE`: EVALUATE TRUE | CUSTSUM.cbl | R-032 | **OK** |
| **Proc** | `PAT-PROC-ROUND-ERR`: 丸めとサイズエラー | BILLING.cbl | R-033 | **TODO** |
| **Proc** | `PAT-PROC-UNSTRING`: 文字列分割 | BILLING.cbl | R-040 | **OK (限定)** |
| **Proc** | `PAT-PROC-EXIT`: STOP RUN/エラー終了 | BILLING.cbl | 未定義 | **未定義** |

---
**Audit Report Summary:**
- **Total Elements Analyzed:** 10
- **Coverage (OK/Stub):** 50%
- **Pending (TODO):** 30%
- **Undefined:** 20%
- **System Date:** 2026-01-24