# Proposed New Rules: COBOL2CSHARP-RULES-v1 Extension

今回の監査で抽出された不足要素を補完するための、具体的な追加ルール案です。

## R-027: 編集用PIC (Numeric Edited) の書式化
* **分類**: データ定義 / 出力整形
* **内容**: `PIC Z` や `PIC -` 等の編集文字を含む項目への代入時、C#側でCOBOL互換のフォーマッタを適用する。
* **変換例**:
    * COBOL: `05 WS-TOTAL PIC Z,ZZZ,ZZ9.99.`
    * C# (案): `WsTotal = CobolFormatter.Format(value, "Z,ZZZ,ZZ9.99");`
* **検出方法**: `DATA DIVISION` 内の書式記号（Z, *, +, -, ., ,）をスキャンして検知。

## R-052: File Statusコードのマッピング
* **分類**: I/O / 例外処理
* **内容**: COBOLの二桁ステータス（00, 10, 23等）をC#の `Enum` で定義し、I/O層のラッパーで変換する。
* **変換例**:
    * COBOL: `IF FS-ORD = "10"` (EOF判定)
    * C# (案): `if (fsOrd == CobolFileStatus.AtEnd)`
* **検出方法**: `SELECT` 文の `STATUS` 句で指定された変数の参照箇所を特定。

## R-062: 異常終了ハンドリング (STOP RUN)
* **分類**: 運用 / 制御
* **内容**: `STOP RUN` や強制終了を、C#の `Environment.Exit()` ではなく、独自の `CobolProcessException` のスローとして変換し、上位のバッチ管理クラスでトラップする。
* **理由**: リソースの解放（Close処理）を確実に行うため。

---
**Audit Date**: 2026-01-24