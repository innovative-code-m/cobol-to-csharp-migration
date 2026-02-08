# Exec: 2026-02-08_01_Implement – CobolMvpRuntime（固定長40→60, CRLF区切り）

> 保存先：`docs/prompts/exec/2026-02-08_01_Implement_CobolMvpRuntime.md`
>
> このファイルは **実行ログ（契約ログ）** です。AI に投入した内容と一致するように記録します。
>
> 参考：以前の方式（OrderValidation の実行ログ）と同じ構造にします。fileciteturn1file0

---

## 対象ファイル

- 実装対象（新規作成を許可）：
  - `src/CobolMvpRuntime/Program.cs`
  - （必要なら）`src/CobolMvpRuntime/CobolMvpRuntime.csproj`（.NET Framework 4.8 Console）
  - （必要なら）`.sln` への追加（既存の運用に合わせて最小限）
- テスト対象（既存がある場合のみ）：
  - `tests/` 配下にMVPの自動テストを追加（既存FWに合わせる）
- サンプルデータ（任意だが推奨）：
  - `samples/mvp01/INFILE.DAT`
  - `samples/mvp01/OUTFILE_expected.DAT`

---

## 変更目的

COBOLのMVPサンプル（MOVE/IF/PERFORM/WRITE相当）を C#（.NET Framework 4.8）で再現する。  
固定長順次ファイルを入力し、固定長順次ファイルを出力する。

- 入力：1行 = **40バイト本体 + CRLF**（テキスト行）
- 出力：1行 = **60バイト本体 + CRLF**（テキスト行）
- 文字コード：**ASCII固定（日本語なし）**
- 行末CRLFは行読み取り時に除去された状態とし、**本体バイト長が40であることを検証**する
- 出力も **本体バイト長が60であることを検証**してから出力（WriteLine等でCRLFを付与）

---

## レコード定義

### 入力レコード（本体40バイト）
- CustomerId: 5（ASCII数字）
- Name: 20（ASCII、右側空白パディング）
- Qty: 3（ASCII数字）
- UnitPrice: 5（ASCII数字）
- Filler: 7（未使用）

### 出力レコード（本体60バイト）
- CustomerId: 5
- Name: 20
- Qty: 3
- UnitPrice: 5
- Total: 7（Qty*UnitPrice、整数、ゼロ埋め）
- BigFlag: 1（Qty>=100なら 'Y'、それ以外 'N'）
- Filler: 17（空白）

---

## 受入基準（すべて必須）

- [ ] .NET Framework 4.8 でビルドできる（コンパイルエラーなし）
- [ ] `INFILE.DAT` を読み込み `OUTFILE.DAT` を生成できる（引数なしならカレントディレクトリの同名ファイルを使用）
- [ ] 入力各行について、ASCIIバイト長が **40** でない場合は例外で停止する
- [ ] 出力各行について、ASCIIバイト長が **60** であることを検証してから出力する
- [ ] Total=Qty*UnitPrice（整数）で、7桁ゼロ埋め文字列になっている
- [ ] BigFlag は Qty>=100 で 'Y'、それ以外 'N'
- [ ] （可能なら）`samples/mvp01/INFILE.DAT` を入力したとき、`samples/mvp01/OUTFILE_expected.DAT` と完全一致する
- [ ] 仕様にない挙動を追加しない（例：自動トリム、エンコーディング推測など）

---

## 禁止事項（今回固有の追加があれば記入）

- ASCII固定を崩さない（Shift-JIS/UTF-8にしない）
- NuGet等の新規依存追加禁止
- 過剰な抽象化・大規模リファクタ禁止（MVPとして最小構成）
- 例外を握り潰さない（入力不正は例外で停止でよい）

※ ここに書かない場合でも、下記テンプレ（01_Implement）の共通ルールは常に適用される。

---

## 使用テンプレート

- `docs/prompts/dev/01_Implement.prompt.md`

---

## 実行用プロンプト本文（AIに投入する“全文”）

> ✅ ここから下を **そのまま Copilot Chat（Codex想定）** に貼り付けて実行します。
> 実行後、AIが返した差分（patch）も別ファイルで保存するとより良いです（任意）。

---

以下のプロンプトに従って実装してください。

【最優先条件（本実行固有・テンプレより優先）】  
以下は今回の実行における確定条件であり、テンプレ内のプレースホルダ（`src/...` 等）は使用しないこと。

【対象ファイル】
- 実装対象：
  - `src/CobolMvpRuntime/Program.cs`
  - （必要なら）`src/CobolMvpRuntime/CobolMvpRuntime.csproj`（.NET Framework 4.8 Console）
  - （必要なら）`.sln` への追加（既存の運用に合わせて最小限）
- テスト対象（既存がある場合のみ）：
  - `tests/` 配下にMVPの自動テストを追加（既存FWに合わせる）
- サンプルデータ（任意だが推奨）：
  - `samples/mvp01/INFILE.DAT`
  - `samples/mvp01/OUTFILE_expected.DAT`

【変更目的】
固定長順次ファイル（40バイト本体+CRLF）を行単位で読み込み、  
レコード定義に従って変換し、固定長60バイト本体+CRLFとして出力する。  
文字コードはASCII固定とし、行末CRLFは除去されている前提で本体バイト長を検証する。

【受入基準】
- .NET Framework 4.8 でビルドできる
- 引数なしなら `INFILE.DAT`→`OUTFILE.DAT` をカレントディレクトリで処理
- 入力行の本体ASCIIバイト長が40でない場合は例外で停止
- 出力行の本体ASCIIバイト長が60であることを検証してから出力
- Total=Qty*UnitPrice（整数、7桁ゼロ埋め）
- BigFlag は Qty>=100 で 'Y'、それ以外 'N'

【禁止事項（今回固有）】
- ASCII固定を崩さない
- 新規依存ライブラリ追加禁止
- 過剰な抽象化・大規模リファクタ禁止
- 例外を握り潰さない

---

（ここに `docs/prompts/dev/01_Implement.prompt.md` の全文を貼り付ける）

---

## 実行結果メモ（任意）

- 実行したAI：Copilot Chat（Codex想定）
- 実行日時：2026-02-08
- 結果：成功 / 要修正
- 修正が必要だった点：
  -

---

以上
