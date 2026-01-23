# COBOL→C# 変換ルール集 v1（MVP / GitHub公開版）
**Document ID**: `COBOL2CSHARP-RULES-v1`  
**Version**: 1.0.0  
**Last Updated**: 2026-01-23 (JST)  
**License**: CC BY 4.0（推奨）  

> 目的：COBOL資産をC#へ移植する際の「変換基準」を固定し、半自動変換・レビュー・検証（データ移行/仕様化/変換ツール）に接続できる “納品物” として成立させる。  
> 方針：**全部変換より「危険箇所の検出・可視化」**。未対応は黙殺せず **TODO化＋候補提示＋レポート化**。

---

## 目次
- [1. スコープ（対象定義）](#1-スコープ対象定義)
  - [1.1 想定するCOBOL方言](#11-想定するcobol方言)
  - [1.2 v1対応サブセット](#12-v1対応サブセット)
  - [1.3 v1非対応（検出してTODO化）](#13-v1非対応検出してtodo化)
- [2. 変換ルール表（一覧）](#2-変換ルール表一覧)
- [3. 共通原則（揺らがせないルール）](#3-共通原則揺らがせないルール)
- [4. データ定義（DATA DIVISION）ルール](#4-データ定義data-divisionルール)
- [5. 手続き（PROCEDURE DIVISION）ルール](#5-手続きprocedure-divisionルール)
- [6. 文字操作（STRING/UNSTRING/INSPECT）ルール](#6-文字操作stringunstringinspectルール)
- [7. I/O（ファイル/DB）ルール（v1は設計＋スタブ）](#7-ioファイルdbルールv1は設計スタブ)
- [8. 変換不能時の運用（TODO/レポート）](#8-変換不能時の運用todoreport)
- [9. 命名規約（追跡性を最優先）](#9-命名規約追跡性を最優先)
- [10. 変換サンプル（v1見本）](#10-変換サンプルv1見本)
- [11. v1の完成条件（MVP受入基準）](#11-v1の完成条件mvp受入基準)
- [付録A. ルールID一覧（R-001〜）](#付録a-ルールid一覧r-001)

---

## 1. スコープ（対象定義）

### 1.1 想定するCOBOL方言
**R-001 方言基準**  
- 基本：COBOL85相当（IBM Enterprise COBOL / Micro Focus / GnuCOBOL の共通部分を優先）  
- 方言差・拡張は **「検出して隔離」**（変換不能/要確認としてレポート対象へ）

### 1.2 v1対応サブセット
**R-002 v1対象（対応）**  
- **データ定義**：`PIC X/9`, `S9`, `V`, `COMP/COMP-3(基本)`, `OCCURS(固定長)`, `88(条件名)`  
- **手続き**：`MOVE`, `IF/ELSE`, `EVALUATE`, `COMPUTE`, `ADD/SUBTRACT/MULTIPLY/DIVIDE`, `PERFORM(段落/ループ)`, `CALL(スタブ)`  
- **文字**：`STRING`, `UNSTRING`（限定）、`INSPECT`（限定）

### 1.3 v1非対応（検出してTODO化）
**R-003 v1非対象（未対応）**  
- `REDEFINES`（型の重ね合わせ・メモリ共有が難所）  
- 可変長 `OCCURS DEPENDING ON`  
- ポインタ/アドレス系：`SET ADDRESS OF` 等  
- `SCREEN SECTION`  
- `SORT/MERGE`  
- `ON SIZE ERROR` 等の方言・例外細部（段階導入）  
- ファイルI/O完全互換（v1は設計＋スタブ）

---

## 2. 変換ルール表（一覧）

> 使い方：変換ツール（⑤）やレビュー時に、この表の **Rule ID** で議論・指摘・例外処理を統一します。

| Rule ID | 分類 | COBOL要素 | C#変換の要点 | v1対応 | 出力/運用 |
|---|---|---|---|---:|---|
| R-001 | スコープ | 方言差 | 検出して隔離、レポート化 | ✅ | Unsupported一覧 |
| R-002 | スコープ | 対応サブセット | 対象構文を明示してMVP化 | ✅ | 変換率算出 |
| R-003 | スコープ | 非対応構文 | TODO＋候補＋リスクを残す | ✅ | 危険箇所レポート |
| R-010 | 原則 | 読みやすさ | “動く”より“追える” | ✅ | コメント/命名 |
| R-011 | 原則 | 層分離 | ロジックとI/Oを分離 | ✅ | Domain/IO/Batch |
| R-012 | 原則 | データ主役 | 型/桁/丸めを優先管理 | ✅ | 型規約/検証 |
| R-020 | データ | レベル構造 | Recordクラス/プロパティへ | ✅ | 元名索引コメント |
| R-021 | データ | PIC→型 | 金額/固定小数は decimal | ✅ | float/double禁止 |
| R-022 | データ | 固定長文字 | パディング/トリム規約 | ✅ | 共通関数化 |
| R-023 | データ | NULL扱い | 原則NOT NULL（""/0） | ✅ | Nullableは例外 |
| R-024 | データ | 88レベル | boolプロパティ（派生） | ✅ | 値は本体に保持 |
| R-025 | データ | OCCURS固定 | 配列（固定長）へ | ✅ | 1始まり注意 |
| R-026 | データ | OCCURS可変 | TODO化（依存変数も） | ✅ | 危険箇所 |
| R-030 | 手続き | MOVE | 代入＋整形（桁/固定長） | ✅ | Fit関数 |
| R-031 | 手続き | IF/ELSE | if/else。比較規約統一 | ✅ | 文字比較注意 |
| R-032 | 手続き | EVALUATE | switch または if連鎖 | ✅ | TRUEはif推奨 |
| R-033 | 手続き | COMPUTE | decimal計算＋丸め明示 | ✅ | SIZE ERRORはTODO |
| R-034 | 手続き | 四則演算 | decimal優先、桁意識 | ✅ | 溢れ検出候補 |
| R-035 | 手続き | PERFORM段落 | メソッド化（ProcA()） | ✅ | 名前に番号保持 |
| R-036 | 手続き | PERFORM UNTIL | while/ do-whileへ | ✅ | 条件反転注意 |
| R-037 | 手続き | PERFORM VARYING | forへ（境界明示） | ✅ | 1始まり補正 |
| R-038 | 手続き | CALL | Registry/DI経由に集約 | ✅ | スタブ生成 |
| R-040 | 文字 | STRING/UNSTRING | 単純区切りのみ対応 | ⚠️ 限定 | 複雑はTODO |
| R-041 | 文字 | INSPECT | 検出＋候補提示が主 | ⚠️ 限定 | 置換/カウント |
| R-050 | I/O | ファイル操作 | I/O層へ隔離、v1はスタブ | ⚠️ 設計 | 意図コメント |
| R-051 | DB | DB移行 | 検証設計へ接続（③） | ✅ | 集計一致 |
| R-060 | 運用 | TODO書式 | TODO(COBOL)形式を統一 | ✅ | レポート連動 |
| R-061 | 運用 | レポート | 変換率・未対応一覧出力 | ✅ | 種類別カウント |
| R-070 | 命名 | 追跡性 | COBOL名を捨てず索引化 | ✅ | `// COBOL:` |
| R-080 | 受入 | 完成条件 | MVP受入基準を満たす | ✅ | チェックリスト |

---

## 3. 共通原則（揺らがせないルール）

**R-010 意図追跡優先**  
- 変換後コードは「COBOLの意図」が追える構造を優先する  
- 変換不能は黙殺しない（**TODO化＋候補提示**）

**R-011 ロジックとI/O分離**  
- `PROCEDURE DIVISION` を1クラスに詰めない  
- 推奨構造：`Domain`（計算/判定）＋`IO`（ファイル/DB）＋`Batch`（段落呼び出し制御）

**R-012 データ定義が主役**  
- 移行事故の主因は **型/桁/符号/丸め/ゼロ埋め**  
- v1ではデータ規約を最優先に固定する

---

## 4. データ定義（DATA DIVISION）ルール

### 4.1 レベル構造 → C#型
**R-020 レベル構造の写像**  
- 01/05/10… の階層は `class` + `property` へ  
- 必要に応じてネスト/別クラス化（巨大01は分割を検討）  
- バイナリ互換が要る場合は `RecordBuffer(byte[])` を別途設計（v1は方針のみ）

### 4.2 PIC → C#型マッピング
**R-021 PIC型対応（v1基準）**  
- 金額/数量/固定小数は **`decimal`**（`float/double` 禁止）  
- 整数は桁数に応じて `int/long/decimal`

| COBOL PIC | 意味 | C#推奨型 | 注意 |
|---|---|---|---|
| `PIC X(n)` | 文字 | `string` | 固定長規約（R-022） |
| `PIC 9(n)` | 数字（符号なし） | `long` or `decimal` | 桁大はdecimal |
| `PIC S9(n)` | 数字（符号あり） | `long` or `decimal` | 符号注意 |
| `PIC 9(n)V9(m)` | 小数 | `decimal` | 丸め仕様明記 |
| `COMP` | バイナリ | `int/long` | I/O互換は別対応 |
| `COMP-3` | パック10進 | `decimal` | 重要：移行検証対象 |

### 4.3 固定長文字規約（MOVE/比較/出力）
**R-022 固定長文字の規約**  
- `PIC X(n)` に代入する場合：  
  1) `null → ""`  
  2) 長い場合：右を切る（`Substring(0,n)`）  
  3) 短い場合：右スペース埋め（`PadRight(n)`）  
- 比較（=, >, <）時は原則 **Trimして比較**（業務仕様で例外があれば明示）

> 実装は共通関数化（例：`CobolString.Fit(value, n)`）

### 4.4 空白・ゼロ・NULL
**R-023 NULLの抑制**  
- COBOLは基本 “NOT NULL” 発想  
- C#側は原則：文字 `""`、数値 `0`  
- “未設定”を表現したい場合のみ `Nullable<T>` を採用し、仕様に明記

### 4.5 88レベル（条件名）
**R-024 条件名の表現**  
- 値そのものは `Status` 等のフィールド/プロパティに保持  
- 88条件は `bool` プロパティとして表現（読みやすさ優先）

```cobol
01  WS-STATUS        PIC X.
    88  ST-OK        VALUE "0".
    88  ST-NG        VALUE "9".
```

```csharp
public string Status { get; set; } = "0"; // COBOL: WS-STATUS
public bool IsOk => Status == "0";        // COBOL: ST-OK
public bool IsNg => Status == "9";        // COBOL: ST-NG
```

### 4.6 OCCURS（配列）
**R-025 OCCURS固定長**  
- `OCCURS n TIMES` → `T[]`（固定長）または `List<T>`（固定要素数として運用）  
- COBOL添字が1始まりの場合、C#側で補正（ラッパ関数推奨）

**R-026 OCCURS可変長**  
- `OCCURS DEPENDING ON` は v1未対応  
- TODO化し、**依存変数名・上限値・想定最大長**をレポートへ

---

## 5. 手続き（PROCEDURE DIVISION）ルール

### 5.1 MOVE
**R-030 MOVEは「代入＋整形」**  
- `MOVE A TO B` → `B = A;` では不十分なケースがある  
- 文字/数値/桁/符号/固定長整形を適用（R-021, R-022）

### 5.2 IF / ELSE
**R-031 IF/ELSE**  
- `IF ... ELSE ... END-IF` → `if (...) { ... } else { ... }`  
- 比較の前提（Trim/Pad/数値化）を規約化して統一する

### 5.3 EVALUATE
**R-032 EVALUATE**  
- `EVALUATE` → `switch` または `if-else`  
- `EVALUATE TRUE` パターンは `if-else` 推奨（意図が読みやすい）

### 5.4 COMPUTE / 四則演算
**R-033 COMPUTE**  
- まず `decimal` で計算（R-021）  
- `ROUNDED` 等の丸めは仕様がある場合のみ明示  
- `ON SIZE ERROR` は v1では **TODO化＋候補提示＋危険箇所レポート**

**R-034 ADD/SUBTRACT/MULTIPLY/DIVIDE**  
- COBOLの桁固定の影響に注意（桁・丸め・溢れ）  
- 溢れ検出が必要ならチェック関数を導入（後続工程）

### 5.5 PERFORM
**R-035 PERFORM 段落呼び出し**  
- `PERFORM PROC-A` → `ProcA();`

**R-036 PERFORM UNTIL（ループ）**  
- `PERFORM UNTIL cond ... END-PERFORM` → `while (!cond) { ... }` 等  
- 条件反転ミスが事故要因。変換時に **元条件をコメント**で残す

**R-037 PERFORM VARYING（for）**  
- `PERFORM VARYING i FROM 1 BY 1 UNTIL i > n` → `for (i=1; i<=n; i++)`  
- 1始まり添字の補正を規約化（R-025）

### 5.6 CALL
**R-038 CALLは “集約ポイント” を作る**  
- `CALL "PGM001" USING ...` は直接呼び出しにしない  
- v1は `ICobolProgramRegistry.Call("PGM001", args...)` に集約し、スタブ生成する  
- 実装の差し替え（DI/テスト）を可能にする

---

## 6. 文字操作（STRING/UNSTRING/INSPECT）ルール

### 6.1 STRING / UNSTRING
**R-040 限定対応**  
- v1は区切り文字が単純なもののみ（例：`,` `/` など）  
- `DELIMITED BY SIZE` 等の複雑ケースは TODO化

### 6.2 INSPECT
**R-041 限定対応（検出＋候補提示）**  
- `INSPECT ... TALLYING` 等は難所  
- v1では “検出して候補提示” を優先  
  - 例：`count = target.Count(ch => ch == 'A');`

---

## 7. I/O（ファイル/DB）ルール（v1は設計＋スタブ）

### 7.1 原則
**R-050 I/O隔離**  
- COBOLの `FD/SELECT/READ/WRITE` は I/O層へ隔離  
- v1は **意図コメント＋スタブ** まで（互換実装は後続）

### 7.2 DB移行（③に接続）
**R-051 DB移行は検証が主役**  
- レコード件数一致  
- キー重複/欠落  
- 桁/符号/丸め差（特に COMP-3）  
- サマリ一致（合計/件数/最大/最小）

---

## 8. 変換不能時の運用（TODO/Report）

### 8.1 TODO書式（統一）
**R-060 TODOの統一フォーマット**  
変換不能/要確認/方言/曖昧さは **同一書式**にする：

```csharp
// TODO(COBOL): REDEFINES detected: WS-A REDEFINES WS-B
// Candidate: Use explicit union mapping or separate views over same buffer.
// Risk: Data overlay / alignment / endian.
```

### 8.2 レポート出力（⑤の出力仕様）
**R-061 レポートに必ず出す項目**  
- 変換率（行数/文数）  
- 未対応構文一覧（種類別カウント＋出現箇所）  
- 危険箇所（REDEFINES, COMP-3, OCCURS DEPENDING ON, SIZE ERROR…）  
- 仕様不明（外部CALL、COPY未解決、ファイル定義不明）

---

## 9. 命名規約（追跡性を最優先）

**R-070 COBOL名を捨てない**  
- 変数：`WS-ORDER-ID` → `WsOrderId`（元名はコメントで索引化）  
- 段落：`1000-MAIN` → `Main_1000()`  
- 追跡性のため、`// COBOL: WS-ORDER-ID` を残す（検索性が上がる）

---

## 10. 変換サンプル（v1見本）

### 10.1 IF + MOVE + COMPUTE
```cobol
IF WS-QTY > 0
   COMPUTE WS-AMOUNT = WS-QTY * WS-PRICE
ELSE
   MOVE 0 TO WS-AMOUNT
END-IF
```

```csharp
if (wsQty > 0)
{
    wsAmount = wsQty * wsPrice; // decimal推奨
}
else
{
    wsAmount = 0m;
}
```

### 10.2 PERFORM VARYING
```cobol
PERFORM VARYING I FROM 1 BY 1 UNTIL I > CNT
   ADD ARR(I) TO SUM
END-PERFORM
```

```csharp
for (int i = 1; i <= cnt; i++)
{
    sum += arr[i]; // 1始まりに注意（配列補正 or ラッパ）
}
```

---

## 11. v1の完成条件（MVP受入基準）
**R-080 MVP受入**  
- [ ] PIC→C#マッピングが固定（decimal優先、固定長文字規約あり）  
- [ ] MOVE/IF/EVALUATE/PERFORM/COMPUTE の変換規約が明文化  
- [ ] 未対応を TODO 化し、レポート化する前提が明示  
- [ ] 危険箇所（REDEFINES, COMP-3, OCCURS DEPENDING ON…）を検出対象として定義  
- [ ] 命名規約（追跡性）と索引コメントの方針がある  

---

## 付録A. ルールID一覧（R-001〜）

### スコープ
- R-001 方言基準（検出して隔離）
- R-002 v1対象（対応サブセット）
- R-003 v1非対象（未対応＝TODO化）

### 共通原則
- R-010 意図追跡優先（TODO化＋候補提示）
- R-011 ロジック/I-O分離
- R-012 データ定義最優先

### データ定義
- R-020 レベル構造→Record
- R-021 PIC→C#型（decimal優先）
- R-022 固定長文字規約（Fit/Pad/Trim）
- R-023 NULL抑制（""/0）
- R-024 88レベル→bool派生
- R-025 OCCURS固定長→配列
- R-026 OCCURS可変長→TODO

### 手続き
- R-030 MOVE（整形含む）
- R-031 IF/ELSE（比較規約統一）
- R-032 EVALUATE（switch/if）
- R-033 COMPUTE（decimal＋丸め明示）
- R-034 四則演算（桁/溢れ注意）
- R-035 PERFORM段落→メソッド
- R-036 PERFORM UNTIL→while
- R-037 PERFORM VARYING→for
- R-038 CALL→Registry/DI＋スタブ

### 文字
- R-040 STRING/UNSTRING（限定）
- R-041 INSPECT（限定・候補提示）

### I/O
- R-050 I/O隔離（v1は設計＋スタブ）
- R-051 DB移行検証（③へ接続）

### 運用
- R-060 TODO書式統一
- R-061 レポート必須項目

### 受入
- R-080 MVP受入基準

---

## 同梱推奨（GitHub公開で強くなるセット）
- `rules/COBOL2CSHARP-RULES-v1.md`（この文書）
- `rules/unsupported-list.md`（未対応構文一覧＋方針）
- `templates/todo-format.md`（TODO書式の例）
- `templates/report-format.md`（変換率・未対応一覧レポート雛形）
- `samples/`（COBOL→C#の短例10本）
