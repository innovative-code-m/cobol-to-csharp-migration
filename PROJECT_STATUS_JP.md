# PROJECT STATUS（詳細版）
cobol-to-csharp-migration

最終更新日: 2026-02-13
フェーズ: MVP02 安定化フェーズ

---

## 1. 🎯 プロジェクトの目的

本プロジェクトは、
COBOLシステムをC#へ移行するための
「構造化された変換基盤」を構築することを目的とする。

単なるコード変換ではなく、以下を実現する：

- 業務ロジックの構造抽出
- ルールベースの変換定義
- 構文カバレッジの可視化
- 変換漏れの検出機構
- 検証可能な移行プロセス

---

## 2. 📌 現在の位置づけ

### フェーズ：MVP02（Runtime安定化）

対象範囲：
- 基本COBOL文の変換
- TODO抽出による未対応構文の可視化
- UnitTestによる検証

MVP01：基本構造生成
MVP02：構文対応拡張＋検証強化（現在）

---

## 3. 🔥 現在の重点課題

優先度順：

1. UNSTRING実装の安定化
2. INSPECT実装着手
3. extract-todos.ps1の誤検出排除確認
4. テストケース拡充
5. ルール番号体系（R-001〜）の整理

---

## 4. 🧱 技術構成

### 実装
- apps/backend/src/CobolMvpRuntime/

### テスト
- apps/backend/tests/CobolMvpRuntimeTests/

### 検証ツール
- tools/verifier/extract-todos.ps1

### ドキュメント
- RuleDefinitions
- CoverageMatrix

---

## 5. 📊 構文対応状況（概念レベル）

| COBOL構文 | 状態 |
|------------|--------|
| MOVE       | 実装済 |
| DISPLAY    | 実装済 |
| UNSTRING   | 部分実装 |
| INSPECT    | 実装済 |

---

## 6. 🐛 現在認識している課題

- UNSTRINGの複数区切りパターン未網羅
- INSPECT未対応
- 構文カバレッジ定量評価未整備
- エッジケーステスト不足

---

## 7. 🧭 直近の目標

短期：

- UNSTRINGテスト通過
- INSPECT最低限実装
- TODO誤検出ゼロ確認

中期：

- ルール体系確立
- CoverageMatrix正式化
- 安定版タグ付け（v1.0-pre）

---

## 8. 🤖 AI活用モデル

| 役割 | ツール |
|------|--------|
| 設計整理 | ChatGPT |
| コード生成 | Cursor |
| 検証 | PowerShell |
| 別視点レビュー | Gemini / Claude |

※AIは補助。最終判断は人間主導。

---

## 9. 🚩 本プロジェクトの本質

目的は「変換すること」ではない。

本質は：

- レガシー業務ロジックの構造抽出
- 再現可能な変換ルール定義
- 検証可能な移行プロセス構築

---

## 10. 📂 参照ログ

日次ログ：

- log/working-memo/2026-02-13.md
- log/working-memo/2026-02-14.md
- log/working-memo/2026-02-15.md
- log/working-memo/2026-02-16.md

---

## 11. 📈 成熟度評価（自己評価）

安定性：実験的だが統制下
方向性：明確
方法論：構築中
公開適性：問題なし

---

状態：進行中
次レビュー予定：UNSTRING安定化後
