# Exec: 2026-02-11_01_Implement_Mvp02_TodoHandling

> 保存先: docs/prompts/exec/2026-02-11_01_Implement_Mvp02_TodoHandling.md  
> 種別: 実行ログ（契約ログ）  
> 目的: MVP02（変換不能を `//TODO` で“壊さず残す”）の実装・テスト追加

---

## 対象ファイル

### 追加（サンプル・期待値）
- docs/samples/mvp02/cobol/MVP02.cbl（※作成済み）
- docs/samples/mvp02/input/INFILE.DAT（※作成済み）
- docs/samples/mvp02/expected/OUTFILE_expected_todo.DAT（※作成済み）
- docs/samples/mvp02/expected/OUTFILE_ideal.DAT（※作成済み）

### 実装（C#）
- apps/backend/src/ （MVP02 実行コードを追加）
  - 方式A（推奨）: `CobolMvpRuntime.Mvp02` の新規プロジェクト追加
  - 方式B: 既存 `CobolMvpRuntime` に MVP02 実行クラス追加
- apps/backend/tests/ （MVP02 テスト追加）
- apps/backend/CobolToCsharpMigration.sln（必要に応じて参照追加）

---

## 変更目的

- MVP02 サンプル（UNSTRING / INSPECT を含む）に対して、変換器／ランタイムが未対応箇所を **`//TODO` として残し**、それ以外は動作することを検証する。
- `UNSTRING` / `INSPECT` は今回は **未実装（= 代替実装もしない）** とし、C# 側に `//TODO(MVP02): ...` を必ず出す。
- 生成される OUTFILE は **暫定期待値**（姓・名が空欄）に一致させ、`dotnet test` を通す。

---

## レコード定義

### 入力（INFILE.DAT）
1行固定長（合計 32 文字 + 改行）

- ID: 5文字（0..4）
- SEP1: 1文字（5） 例: `,`
- NAME: 20文字（6..25）例: `TARO YAMADA         `（右パディング）
- SEP2: 1文字（26）例: `,`
- AMOUNT: 5文字（27..31）例: `00123`

### 出力（OUTFILE.DAT）
CSV 形式（改行区切り）

- `ID,Last,First,Amount`

---

## 受入基準（すべて必須）

- [ ] `dotnet test` が成功する
- [ ] MVP02 実行により OUTFILE.DAT が生成される
- [ ] 生成 OUTFILE.DAT が `docs/samples/mvp02/expected/OUTFILE_expected_todo.DAT` と **完全一致**する
- [ ] `UNSTRING` / `INSPECT` 相当は **未実装**であり、C# ソースに以下形式の TODO が残っている
  - `//TODO(MVP02): UNSTRING ...`
  - `//TODO(MVP02): INSPECT ...`

（推奨・任意）
- [ ] テストで `//TODO(MVP02):` を文字列検索して検証する

---

## 禁止事項（今回固有の追加があれば記入）

- `UNSTRING` / `INSPECT` を **勝手に完全実装しない**（今回は“壊さず残す”の検証）
- 期待値（暫定）に合わせるため、Last/First を埋めるような代替実装（Split/ToUpper 等）も行わない  
  → もし将来「壊さず進める暫定実装」をやる場合は、別の exec を切って行う

---

## 使用テンプレート

- docs/prompts/dev/01_Implement.prompt.md

---

## 実行用プロンプト本文（AIに投入する“全文”）

> ここから下を **そのまま** Copilot Chat / Codex に貼り付けて実行してください。  
> （下部に `01_Implement.prompt.md` の全文を “丸ごと” 挿入しています）

### 実行固有条件（最優先）

- テストデータは作成済み（docs/samples/mvp02 配下）
- MVP02 の入出力仕様は「レコード定義」の通り
- `UNSTRING` / `INSPECT` 相当は未実装のまま `//TODO(MVP02): ...` を残す
- OUTFILE は暫定期待値（姓・名空欄）に一致させる
- 可能なら新規プロジェクト `CobolMvpRuntime.Mvp02` として分離（難しければ既存に追加でも可）
- 変更後、必ず `dotnet test` が通る状態にする
- 変更差分（diff）を出力すること

---

### 01_Implement.prompt.md（全文貼り付け）

# Dev Template: 01_Implement（実装担当向け・共通テンプレ）

> 保存先：`docs/prompts/dev/01_Implement.prompt.md`
>
> このファイルは **共通テンプレ** です。個別タスクは `docs/prompts/exec/*.md` に記録し、
> 「実行用プロンプト本文」で **このテンプレ全文** を貼り付けて実行します。

---

## 役割定義（固定）
あなたは **実装担当（Codex 相当）** です。責務は、**確定仕様を正確に、最小差分で**コードに反映することです。  
設計変更・仕様提案・リファクタリング・過剰な抽象化は行ってはいけません。

---

## 共通ルール（常に適用）
- 仕様に無い動作を追加しない
- 既存の public API（public クラス/メソッド/プロパティ）は変更しない（新規作成は可）
- 既存ファイル/既存クラスを削除しない
- 新規依存ライブラリを追加しない（NuGet追加禁止）
- 例外の握り潰し禁止（必要な場合は明示的にログ/メッセージを残す）
- テスト基盤が既にある場合は、**今回の変更を検証するテストを追加/更新**する
- テスト基盤が無い場合は、**無理に追加しない**（理由を明記する）

---

## 出力要件（厳守）
次の順序・形式で出力すること（ファイル全文貼り付けは禁止）。

1. **変更ファイル一覧**（追加/更新を明記）
2. **変更点の要約**（箇条書き）
3. **差分（patch 形式）**（`diff` で開始する unified diff）

例：

```diff
diff --git a/src/Foo.cs b/src/Foo.cs
index 1111111..2222222 100644
--- a/src/Foo.cs
+++ b/src/Foo.cs
@@ -10,6 +10,12 @@
```

---

## 不明点の扱い
- リポジトリ構成（.sln、testsの有無など）が不明な場合は、手元情報から推測できる範囲で最小変更で進める。
- それでも結論が変わる点は質問すること。

---

## 実行プロンプト（テンプレ本文）

以下のプロンプトに従って実装してください。

【最優先条件（本実行固有・テンプレより優先）】  
以下は今回の実行における確定条件であり、テンプレ内の曖昧表現やプレースホルダは使用しないこと。

（ここに exec ファイルで定義した “対象ファイル/変更目的/受入基準/禁止事項（固有）” が入る想定）

【差分出力要件】
- 必ず差分（patch形式）で返すこと
- 変更が不要と判断した場合も、その理由と確認結果を出すこと（ただしファイル全文は出さない）


---

## 実行結果メモ（任意）

- 実行環境:
- 実行コマンド:
- 結果:
- 補足:
