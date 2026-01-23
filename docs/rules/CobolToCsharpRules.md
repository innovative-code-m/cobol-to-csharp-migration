# COBOL→C# 変換ルール集 v1.1（MVP / GitHub公開版）
**Document ID**: `COBOL2CSHARP-RULES`  
**Version**: 1.1.0  
**Last Updated**: 2026-01-24 (JST)  
**License**: CC BY 4.0（推奨）

> 目的：COBOL資産をC#へ移植する際の「変換基準」を固定し、半自動変換・レビュー・検証（データ移行/仕様化/変換ツール）に接続できる “納品物” として成立させる。  
> 方針：**全部変換より「危険箇所の検出・可視化」**。未対応は黙殺せず **TODO化＋候補提示＋レポート化**。

---

## Changelog（v1.1）
### v1.1.0（2026-01-24）
- **追加（Accepted提案の昇格）**
  - **R-027 編集用PIC（Numeric Edited）の書式化** を追加（CoverageMatrix 未定義要素 `PAT-DATA-EDITED` を解消）
  - **R-062 プログラム終了制御（STOP RUN / 異常終了）** を追加（CoverageMatrix 未定義要素 `PAT-PROC-EXIT` を解消）
- **更新**
  - **R-060 TODO書式**：REDEFINES 検出時の「再定義元/先」を必須項目として追加
  - **R-050 I/O隔離（スタブ）**：File Status の扱い（スタブ方針＋付録Enum）を明記（新規Rule追加はせず、既存RuleのNotesへ統合）
- **整備**
  - ルール本文を **Scope / Conversion / Acceptance Criteria / Before/After example / Notes** の統一フォーマットへ揃えた
  - ルール索引テーブル更新
  - MissingList 各項目に「解決Rule」参照（未解決は v1.1 での扱いを明記）

---

## 目次
- [1. スコープ（対象定義）](#1-スコープ対象定義)
- [2. 変換ルール表（一覧 / Index）](#2-変換ルール表一覧--index)
- [3. 共通原則（揺らがせないルール）](#3-共通原則揺らがせないルール)
- [4. データ定義（DATA DIVISION）ルール](#4-データ定義data-divisionルール)
- [5. 手続き（PROCEDURE DIVISION）ルール](#5-手続きprocedure-divisionルール)
- [6. 文字操作（STRING/UNSTRING/INSPECT）ルール](#6-文字操作stringunstringinspectルール)
- [7. I/O（ファイル/DB）ルール（v1は設計＋スタブ）](#7-ioファイルdbルールv1は設計スタブ)
- [8. 変換不能時の運用（TODO/レポート）](#8-変換不能時の運用todoreport)
- [9. 命名規約（追跡性を最優先）](#9-命名規約追跡性を最優先)
- [10. 変換サンプル（見本）](#10-変換サンプル見本)
- [11. v1.1の完成条件（MVP受入基準）](#11-v11の完成条件mvp受入基準)
- [12. MissingList（v1.1反映結果）](#12-missinglistv11反映結果)
- [付録A. ルールID一覧（R-001〜）](#付録a-ルールid一覧r-001)
- [付録B. File Status（スタブ用Enum例）](#付録b-file-statusスタブ用enum例)
- [付録C. Coverage Matrix（監査サマリ）](#付録c-coverage-matrix監査サマリ)

---

## 1. スコープ（対象定義）

### R-001 方言基準
- **Scope**: COBOl85相当（IBM Enterprise COBOL / Micro Focus / GnuCOBOL の共通部分を優先）
- **Conversion**: 方言差・拡張は **検出して隔離**し、変換出力では「未対応/要確認」としてレポートへ送る
- **Acceptance Criteria**
  - 方言拡張が検出された場合、変換コードに黙って落とさず **TODO＋レポート**に残る
- **Before/After example**
  - **Before (COBOL)**: `SPECIAL-NAMES. DECIMAL-POINT IS COMMA.`
  - **After (C#)**:
    ```csharp
    // TODO(COBOL): Dialect feature detected: SPECIAL-NAMES DECIMAL-POINT IS COMMA
    // Candidate: Normalize numeric parsing/formatting with invariant culture and explicit decimal separator.
    ```
- **Notes**: v1.1では互換実装よりも **検出の確実性**を優先する

### R-002 v1対象（対応サブセット）
- **Scope**: v1.1で「変換対象」として保証する構文サブセット
- **Conversion**: 対象外は R-003 + R-060/061 の運用へ必ず送る
- **Acceptance Criteria**
  - 対象サブセットが文書で明確
  - 対象外構文が出た場合に TODO/レポートへ流れる
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    MOVE A TO B
    IF X > 0
       COMPUTE Y = X * P
    END-IF
    ```
  - **After (C#)**:
    ```csharp
    b = a;              // MOVE (R-030)
    if (x > 0) y = x*p; // IF/COMPUTE (R-031/R-033)
    ```
- **Notes**: 対応範囲を固定することで、変換ツール（⑤）の品質評価が可能になる

### R-003 v1非対象（未対応＝検出してTODO化）
- **Scope**: v1.1では互換実装しない難所（例：REDEFINES / ODO / ポインタ / SORT等）
- **Conversion**: **必ず検出し、TODOコメント＋候補＋リスク**を残し、レポートに計上
- **Acceptance Criteria**
  - 未対応構文が検出されたら、(a) TODOコメント (b) レポート計上 が両方行われる
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    05 WS-A PIC X(10).
    05 WS-B REDEFINES WS-A PIC 9(10).
    ```
  - **After (C#)**:
    ```csharp
    // TODO(COBOL): REDEFINES detected: WS-B REDEFINES WS-A
    // Candidate: Use explicit union mapping or separate views over same buffer.
    // Risk: Data overlay / alignment.
    ```
- **Notes**: v1.1は「危険箇所の見える化」を成果として納品可能にする

---

## 2. 変換ルール表（一覧 / Index）

| Rule ID | 分類 | COBOL要素 | C#変換の要点 | v1.1対応 | 出力/運用 |
|---|---|---|---|---:|---|
| R-001 | スコープ | 方言差 | 検出して隔離、レポート化 | ✅ | Unsupported一覧 |
| R-002 | スコープ | 対応サブセット | 対象構文を明示してMVP化 | ✅ | 変換率算出 |
| R-003 | スコープ | 非対応構文 | TODO＋候補＋リスクを残す | ✅ | 危険箇所レポート |
| R-010 | 原則 | 意図追跡 | “動く”より“追える” | ✅ | コメント/命名 |
| R-011 | 原則 | 層分離 | ロジックとI/Oを分離 | ✅ | Domain/IO/Batch |
| R-012 | 原則 | データ主役 | 型/桁/丸めを優先管理 | ✅ | 型規約/検証 |
| R-020 | データ | レベル構造 | Recordクラス/プロパティへ | ✅ | 元名索引コメント |
| R-021 | データ | PIC→型 | 金額/固定小数は decimal | ✅ | float/double禁止 |
| R-022 | データ | 固定長文字 | パディング/トリム規約 | ✅ | 共通関数化 |
| R-023 | データ | NULL扱い | 原則NOT NULL（""/0） | ✅ | Nullableは例外 |
| R-024 | データ | 88レベル | boolプロパティ（派生） | ✅ | 値は本体に保持 |
| R-025 | データ | OCCURS固定 | 配列（固定長）へ | ✅ | 1始まり注意 |
| R-026 | データ | OCCURS可変 | TODO化（依存変数も） | ✅ | 危険箇所 |
| **R-027** | **データ** | **編集用PIC** | **互換書式化ユーティリティ** | ✅ | 帳票/表示互換 |
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
| **R-062** | **運用** | **終了制御** | **例外/終了コードへ集約** | ✅ | ジョブ管理連携 |
| R-070 | 命名 | 追跡性 | COBOL名を捨てず索引化 | ✅ | `// COBOL:` |
| R-080 | 受入 | 完成条件 | MVP受入基準を満たす | ✅ | チェックリスト |

---

## 3. 共通原則（揺らがせないルール）

### R-010 意図追跡優先
- **Scope**: 変換後コードは「COBOLの意図」が追えることを最優先
- **Conversion**: 変換不能は黙殺せず、R-060 の TODO に落とし、R-061 のレポートに必ず出す
- **Acceptance Criteria**
  - 変換不能箇所が 0 件にならなくてもよいが、**検出漏れ**がないこと
- **Before/After example**
  - **Before (COBOL)**: `REDEFINES` / `ODO` を含む箇所
  - **After (C#)**:
    ```csharp
    // TODO(COBOL): <reason> ... (R-060 format)
    ```
- **Notes**: v1.1は「変換率」より「危険箇所の見える化」が価値

### R-011 ロジックとI/O分離
- **Scope**: `PROCEDURE DIVISION` を1クラスに詰め込まず責務分離
- **Conversion**: 推奨：`Domain`（計算/判定）＋`IO`（ファイル/DB）＋`Batch`（段落呼び出し制御）
- **Acceptance Criteria**
  - I/O処理がドメイン計算と混ざらない（呼出し点が限定される）
- **Before/After example**
  - **Before (COBOL)**: `READ ... AT END ...` と計算ロジックが同一段落
  - **After (C#)**:
    ```csharp
    io.ReadNext();   // IO層
    domain.Calc();   // Domain層
    ```
- **Notes**: I/O互換は v1.1 ではスタブ（R-050）

### R-012 データ定義が主役
- **Scope**: 事故要因（型/桁/符号/丸め/ゼロ埋め）を最優先で固定
- **Conversion**: PIC→型（R-021）、固定長文字（R-022）、丸め（R-033）を共通化
- **Acceptance Criteria**
  - 金額/固定小数の型が `decimal` に統一されている
- **Before/After example**
  - **Before (COBOL)**: `PIC 9(7)V99 COMP-3`
  - **After (C#)**:
    ```csharp
    public decimal Amount { get; set; } // Packed decimal -> decimal
    ```
- **Notes**: 互換I/Oが必要なら別設計（バッファ/エンディアン等）

---

## 4. データ定義（DATA DIVISION）ルール

### R-020 レベル構造の写像
- **Scope**: 01/05/10… の階層構造
- **Conversion**: `class` + `property` へ写像。巨大01は分割を検討
- **Acceptance Criteria**
  - 元COBOL名が追跡できる（R-070）
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    01 CUST-REC.
       05 CUST-ID    PIC 9(10).
       05 CUST-NAME  PIC X(20).
    ```
  - **After (C#)**:
    ```csharp
    public sealed class CustRec
    {
        public long CustId { get; set; }      // COBOL: CUST-ID
        public string CustName { get; set; } = ""; // COBOL: CUST-NAME
    }
    ```
- **Notes**: バイナリ互換が要る場合は別途 `RecordBuffer(byte[])` を設計（v1.1は方針のみ）

### R-021 PIC型対応（v1.1基準）
- **Scope**: `PIC X/9`, `S9`, `V`, `COMP`, `COMP-3`（基本）
- **Conversion**: 金額/数量/固定小数は **`decimal`**。`float/double` は禁止
- **Acceptance Criteria**
  - `COMP-3` 相当のフィールドが `decimal` になっている
  - 小数点位置（V）の意味が設計に残る（丸め/桁）
- **Before/After example**
  - **Before (COBOL)**: `05 PRICE PIC 9(5)V99 COMP-3.`
  - **After (C#)**:
    ```csharp
    public decimal Price { get; set; } // COBOL: PRICE (V99)
    ```
- **Notes**: I/O互換（Packedのバイト列）まで要求される場合は別設計に切り出す

### R-022 固定長文字の規約
- **Scope**: `PIC X(n)` の代入/比較/出力
- **Conversion**: 代入時に `Fit(value,n)`（切り捨て/PadRight/Null→""）を適用
- **Acceptance Criteria**
  - `PIC X(n)` 相当の代入が、素の `=` ではなく Fitを経由できる（少なくとも方針として固定）
- **Before/After example**
  - **Before (COBOL)**: `MOVE IN-NAME TO WS-NAME`
  - **After (C#)**:
    ```csharp
    wsName = CobolString.Fit(inName, 20); // PIC X(20)
    ```
- **Notes**: 比較は原則 Trim（業務仕様でPad比較が必要なら例外として明記）

### R-023 NULLの抑制
- **Scope**: COBOLの “NOT NULL” 前提
- **Conversion**: 文字は `""`、数値は `0` を初期値にする。Nullableは例外
- **Acceptance Criteria**
  - ドメインデータが Nullable だらけになっていない
- **Before/After example**
  - **Before (COBOL)**: 未設定時も SPACE / ZERO で保持
  - **After (C#)**:
    ```csharp
    public string Name { get; set; } = "";
    public decimal Amount { get; set; } = 0m;
    ```
- **Notes**: 「未設定」を意味として使う場合のみ Nullable を採用し仕様に残す

### R-024 88レベル（条件名）
- **Scope**: 88条件名（値の別名）
- **Conversion**: 値は本体フィールドに保持、条件は `bool` 派生プロパティ化
- **Acceptance Criteria**
  - 条件名の意図が C# 側で読み取れる（`IsOk` など）
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    01 WS-STATUS PIC X.
       88 ST-OK VALUE "0".
    ```
  - **After (C#)**:
    ```csharp
    public string Status { get; set; } = "0";
    public bool IsOk => Status == "0";
    ```
- **Notes**: 条件名に複数VALUEがある場合は `HashSet<string>` 等で表現してもよい

### R-025 OCCURS固定長
- **Scope**: `OCCURS n TIMES`（固定長）
- **Conversion**: `T[]`（固定長）を基本。COBOL添字が1始まりなら補正方針を統一
- **Acceptance Criteria**
  - 添字境界（1..n）がコードで明確（for条件等）
- **Before/After example**
  - **Before (COBOL)**: `05 ARR OCCURS 10 TIMES PIC 9(2).`
  - **After (C#)**:
    ```csharp
    public int[] Arr { get; } = new int[10]; // index 0..9（COBOLは1..10に注意）
    ```
- **Notes**: 1始まりを隠蔽するラッパ（GetAtCobolIndex等）を用意すると事故が減る

### R-026 OCCURS可変長（ODO）
- **Scope**: `OCCURS DEPENDING ON`
- **Conversion**: v1.1は未対応。**依存変数/上限/最大長**をTODO＋レポートへ
- **Acceptance Criteria**
  - ODOを検出できる
  - 依存変数名と上限がTODOに残る
- **Before/After example**
  - **Before (COBOL)**: `OCCURS 1 TO 100 DEPENDING ON CNT`
  - **After (C#)**:
    ```csharp
    // TODO(COBOL): ODO detected: OCCURS 1 TO 100 DEPENDING ON CNT (max=100)
    // Candidate: Represent as List<T> with explicit Count=CNT.
    ```
- **Notes**: 互換I/Oを伴う場合は難度が上がるため段階導入推奨

### R-027 編集用PIC（Numeric Edited）【v1.1追加】
- **Scope**: `PIC Z, ZZ9, ZZZ,ZZ9`, `PIC -9`, `PIC 9,999`, `PIC $$$9` 等の **出力書式**（帳票/表示に直結）
- **Conversion**: C#標準の数値書式だけで完全互換が難しいため、**CobolNumericEdited.Format(...)** のような共通ユーティリティに集約する
- **Acceptance Criteria**
  - 編集用PICが検出されたら「書式化ユーティリティ経由」へ誘導できる（少なくともTODO＋候補提示）
  - 代表パターン（カンマ区切り/ゼロ抑止/符号/通貨）の振る舞いがテストで固定できる
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    05 OUT-AMT PIC Z,ZZZ,ZZ9.
    MOVE AMT TO OUT-AMT.
    ```
  - **After (C#)**:
    ```csharp
    // Candidate: Numeric edited picture formatting
    outAmt = CobolNumericEdited.Format(amt, "Z,ZZZ,ZZ9");
    ```
- **Notes**
  - v1.1の狙いは “互換の設計点” を固定すること（全面互換の実装は段階導入でよい）
  - まずは **検出＋ユーティリティ入口** を作り、監査用サンプルで差分を可視化する

---

## 5. 手続き（PROCEDURE DIVISION）ルール

### R-030 MOVEは「代入＋整形」
- **Scope**: `MOVE A TO B`
- **Conversion**: 型/固定長/桁の規約（R-021/R-022）を適用して代入する
- **Acceptance Criteria**
  - 固定長文字や数値桁の変換が “無条件の代入” になっていない（方針が固定されている）
- **Before/After example**
  - **Before (COBOL)**: `MOVE IN-NAME TO WS-NAME`
  - **After (C#)**:
    ```csharp
    wsName = CobolString.Fit(inName, 20);
    ```
- **Notes**: MOVEの暗黙変換（数値⇔文字等）がある場合は候補提示＋テスト優先

### R-031 IF/ELSE
- **Scope**: `IF/ELSE/END-IF`
- **Conversion**: `if/else`。比較前提（Trim/Pad/数値化）を規約化
- **Acceptance Criteria**
  - 文字比較で Trim有無がブレない
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    IF WS-NAME = "A" ...
    ```
  - **After (C#)**:
    ```csharp
    if (wsName.TrimEnd() == "A") { /* ... */ }
    ```
- **Notes**: 固定長文字は TrimEnd（右スペース）前提が多い

### R-032 EVALUATE
- **Scope**: `EVALUATE`（特に `EVALUATE TRUE`）
- **Conversion**: `switch` または `if-else`。`EVALUATE TRUE` は `if-else` 推奨
- **Acceptance Criteria**
  - 分岐条件が読みやすい構造に落ちている
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    EVALUATE TRUE
      WHEN A > 0 ...
      WHEN OTHER ...
    END-EVALUATE
    ```
  - **After (C#)**:
    ```csharp
    if (a > 0) { /* ... */ }
    else { /* ... */ }
    ```
- **Notes**: WHEN条件の優先順位が保たれていること

### R-033 COMPUTE（丸め / SIZE ERRORはTODO）
- **Scope**: `COMPUTE`（`ROUNDED` / `ON SIZE ERROR` を含む可能性）
- **Conversion**: `decimal` 計算を基本。丸めは明示。`ON SIZE ERROR` は v1.1 では TODO化
- **Acceptance Criteria**
  - 金額計算が `decimal` である
  - SIZE ERROR の有無が TODO に残る
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    COMPUTE AMT = QTY * PRICE ON SIZE ERROR MOVE 0 TO AMT
    ```
  - **After (C#)**:
    ```csharp
    // TODO(COBOL): ON SIZE ERROR detected in COMPUTE. Decide overflow/rounding policy.
    amt = qty * price;
    ```
- **Notes**: 溢れや丸めは “仕様” なので、監査サンプルで実態確認してから固定する

### R-034 四則演算（ADD/SUBTRACT/MULTIPLY/DIVIDE）
- **Scope**: 四則演算系命令
- **Conversion**: `decimal` 優先。桁/丸め/溢れが絡む場合はチェック関数導入を検討
- **Acceptance Criteria**
  - 計算経路が `double` に落ちていない
- **Before/After example**
  - **Before (COBOL)**: `ADD A TO B`
  - **After (C#)**:
    ```csharp
    b += a;
    ```
- **Notes**: 受入で “数値差分” を必ずチェック（R-051）

### R-035 PERFORM 段落呼び出し
- **Scope**: `PERFORM PROC-A`
- **Conversion**: 段落をメソッドへ。元段落名/番号をメソッド名に残す
- **Acceptance Criteria**
  - 呼出しグラフが追える（段落→メソッド）
- **Before/After example**
  - **Before (COBOL)**: `PERFORM 1000-MAIN`
  - **After (C#)**:
    ```csharp
    Main_1000();
    ```
- **Notes**: “落とし穴” は FALL THROUGH（次段落への自然落下）—必要なら構造化して明示

### R-036 PERFORM UNTIL
- **Scope**: `PERFORM UNTIL cond`
- **Conversion**: `while` / `do-while`。条件反転ミスを避けるため元条件をコメントで残す
- **Acceptance Criteria**
  - ループ条件が意図どおり（終了条件/継続条件の誤反転なし）
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    PERFORM UNTIL EOF
      READ ...
    END-PERFORM
    ```
  - **After (C#)**:
    ```csharp
    // COBOL: PERFORM UNTIL EOF
    while (!eof)
    {
        // ...
    }
    ```
- **Notes**: I/O絡みは R-050 スタブ方針に寄せる

### R-037 PERFORM VARYING
- **Scope**: `PERFORM VARYING i FROM ... BY ... UNTIL ...`
- **Conversion**: `for` へ。境界条件を明示し、1始まり補正方針を固定
- **Acceptance Criteria**
  - ループ境界がレビューで判断できる形になっている
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > CNT
    ```
  - **After (C#)**:
    ```csharp
    for (int i = 1; i <= cnt; i++)
    {
        // ...
    }
    ```
- **Notes**: 配列添字の補正（R-025）とセットで運用

### R-038 CALL（集約ポイント）
- **Scope**: `CALL "PGM001" USING ...`
- **Conversion**: 直接呼び出しにせず、Registry/DIに集約しスタブ生成
- **Acceptance Criteria**
  - 呼び出し先が未実装でもビルド可能（スタブで受ける）
- **Before/After example**
  - **Before (COBOL)**: `CALL "PGM001" USING A B`
  - **After (C#)**:
    ```csharp
    registry.Call("PGM001", a, b); // stubbed
    ```
- **Notes**: 呼出しインタフェース（引数型）を後続で固める

---

## 6. 文字操作（STRING/UNSTRING/INSPECT）ルール

### R-040 STRING/UNSTRING（限定対応）
- **Scope**: 区切りが単純なケースのみ
- **Conversion**: `Split` 等へ。複雑な `DELIMITED BY SIZE` は TODO化
- **Acceptance Criteria**
  - “限定対応” の範囲が文書化されている
- **Before/After example**
  - **Before (COBOL)**: `UNSTRING S DELIMITED BY "," INTO A B`
  - **After (C#)**:
    ```csharp
    var parts = s.Split(',');
    a = parts.ElementAtOrDefault(0) ?? "";
    b = parts.ElementAtOrDefault(1) ?? "";
    ```
- **Notes**: 例外処理や空要素の扱いはテストで固定する

### R-041 INSPECT（限定・候補提示）
- **Scope**: `INSPECT`（置換/カウント等）
- **Conversion**: v1.1は “検出＋候補提示” を優先
- **Acceptance Criteria**
  - INSPECT検出時に TODO/候補が残る
- **Before/After example**
  - **Before (COBOL)**: `INSPECT S TALLYING CNT FOR ALL "A"`
  - **After (C#)**:
    ```csharp
    // Candidate:
    cnt = s.Count(ch => ch == 'A');
    ```
- **Notes**: 完全互換は難所が多いので段階導入

---

## 7. I/O（ファイル/DB）ルール（v1は設計＋スタブ）

### R-050 I/O隔離（v1.1はスタブ）
- **Scope**: `SELECT/FD/READ/WRITE/FILE STATUS` 等
- **Conversion**: I/O層へ隔離し、v1.1は **意図コメント＋スタブ** まで
- **Acceptance Criteria**
  - ドメインロジックが I/O 実装に直接依存しない
  - ファイル状態（EOF/Status）分岐が “I/O層からの結果” として表現される
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    READ INFILE
      AT END SET EOF TO TRUE
    END-READ
    ```
  - **After (C#)**:
    ```csharp
    var r = io.ReadInfile();
    if (r.IsEof) eof = true;
    ```
- **Notes**
  - File Status の頻出コード（00,10,23等）は v1.1では **I/Oスタブ用Enum** を付録に置き、ロジック側での分岐を読みやすくする（付録B参照）
  - 互換I/Oは後続で段階導入（記録形式・エンコーディング等）

### R-051 DB移行は検証が主役（③へ接続）
- **Scope**: ファイル→DBMS移植後の整合性検証
- **Conversion**: 件数/キー/サマリ一致を必須とし、差分をレポート化
- **Acceptance Criteria**
  - 件数一致
  - キー重複/欠落なし
  - サマリ一致（合計/最大/最小など、業務で意味のある指標）
- **Before/After example**
  - **Before (COBOL)**: ファイル集計（合計、件数）
  - **After (C# / SQL)**:
    ```csharp
    // Example: compare totals between source parse and DB query results
    ```
- **Notes**: 特に COMP-3 と編集用PIC（R-027）は差分要因になりやすい

---

## 8. 変換不能時の運用（TODO/Report）

### R-060 TODO書式（統一フォーマット：v1.1強化）
- **Scope**: 変換不能/要確認/方言/曖昧さ
- **Conversion**: 下記の統一フォーマットを必須とする（REDEFINESは v1.1で必須項目追加）
- **Acceptance Criteria**
  - TODOが機械抽出可能（プレフィックス/キーが揃っている）
  - REDEFINESの場合、**再定義元/先** が必ず入る
- **Before/After example**
  - **Before (COBOL)**: `05 B REDEFINES A ...`
  - **After (C#)**:
    ```csharp
    // TODO(COBOL): REDEFINES detected
    // Source: A
    // Target: B
    // Candidate: union mapping / separate views
    // Risk: overlay/alignment
    ```
- **Notes**: TODOは “未完了の証拠” ではなく “リスクを握るための成果物”

### R-061 レポート（変換率・未対応一覧）
- **Scope**: 変換結果の監査・受入
- **Conversion**: 変換率、未対応構文種類別カウント、危険箇所一覧を出力
- **Acceptance Criteria**
  - 変換率（行数/文数）が算出できる
  - TODO/未定義が種類別に集計される
- **Before/After example**
  - **Before**: 変換後に根拠が残らない
  - **After**: `UnsupportedSummary.json` / `UnsupportedSummary.md` 等に集計（形式は自由）
- **Notes**: CoverageMatrix（付録C）と突合できる形が望ましい

### R-062 プログラム終了制御（STOP RUN / 異常終了）【v1.1追加】
- **Scope**: `STOP RUN`、異常終了、終了ステータス管理（バッチ/ジョブ制御）
- **Conversion**: C#側は **例外 + 終了コード** に集約する（例：`CobolProcessException`）
- **Acceptance Criteria**
  - STOP RUN相当が検出されたら、(a) 例外化 (b) 終了コード/メッセージが保持される
  - 終了時のクリーンアップ（Close/Dispose）の設計点が残る
- **Before/After example**
  - **Before (COBOL)**:
    ```cobol
    DISPLAY "ABEND"
    STOP RUN.
    ```
  - **After (C#)**:
    ```csharp
    throw new CobolProcessException("ABEND", exitCode: 12);
    ```
- **Notes**
  - v1.1では “運用上の意図” を固定するのが主目的（例外型/終了コードの体系は後続で拡張可能）
  - I/Oクローズなどの後処理は、例外の上位で確実に行う設計に寄せる

---

## 9. 命名規約（追跡性を最優先）

### R-070 COBOL名を捨てない
- **Scope**: 変数/段落/ファイル名/コピー句名など
- **Conversion**: C#命名へ正規化しつつ、元名をコメント（`// COBOL:`）で残す
- **Acceptance Criteria**
  - COBOL名からC#側を検索できる（追跡性）
- **Before/After example**
  - **Before (COBOL)**: `WS-ORDER-ID`
  - **After (C#)**:
    ```csharp
    public string WsOrderId { get; set; } = ""; // COBOL: WS-ORDER-ID
    ```
- **Notes**: “綺麗さ” より “追えること” を優先する

---

## 10. 変換サンプル（見本）

（サンプル自体も規約フォーマットに揃える場合は、`samples/` へ分離するのが推奨）

---

## 11. v1.1の完成条件（MVP受入基準）

### R-080 MVP受入
- **Scope**: v1.1として納品成立させるための受入条件
- **Conversion**: ルールと監査（Coverage/Missing）を “回る形” に固定する
- **Acceptance Criteria**
  - [ ] PIC→型が固定（R-021、decimal優先）
  - [ ] 固定長文字の規約が固定（R-022）
  - [ ] MOVE/IF/EVALUATE/PERFORM/COMPUTEの変換規約が明文化（R-030〜R-037, R-033）
  - [ ] 未対応は TODO 化し、レポート化される（R-003, R-060, R-061）
  - [ ] 危険箇所（REDEFINES/ODO/SIZE ERROR等）が検出対象として定義済み（R-003, R-026, R-033）
  - [ ] 編集用PICが “設計点として” 固定（R-027）
  - [ ] 終了制御が “設計点として” 固定（R-062）
- **Before/After example**
  - **Before**: 変換後の品質が測れない
  - **After**: CoverageMatrixとMissingListで監査→ルール更新が回る
- **Notes**: v1.1は “全面互換” ではなく “監査ループが回る” を完成と定義する

---

## 12. MissingList（v1.1反映結果）

| 優先度 | Missing（原文） | v1.1での扱い | 解決Rule参照 |
|---:|---|---|---|
| High | 編集用PICの書式化 (R-027) | **ルール追加で解決** | **R-027** |
| High | REDEFINES 検出・報告 | TODO書式へ「再定義元/先」を必須追加 | **R-060**（＋R-003） |
| Med | プログラム終了制御 (R-062) | **ルール追加で解決** | **R-062** |
| Med | File Status 定数定義 (R-052) | **新規Ruleは追加せず**、I/O隔離（スタブ）に統合＋付録Enum例を追加 | **R-050**（付録B） |
| Low | SIGN句の影響調査 | v1.1ではOut of Scope（必要箇所のみ個別対応） | **R-003** |

---

## 付録A. ルールID一覧（R-001〜）

- スコープ：R-001, R-002, R-003  
- 原則：R-010, R-011, R-012  
- データ：R-020, R-021, R-022, R-023, R-024, R-025, R-026, **R-027**  
- 手続き：R-030, R-031, R-032, R-033, R-034, R-035, R-036, R-037, R-038  
- 文字：R-040, R-041  
- I/O：R-050, R-051  
- 運用：R-060, R-061, **R-062**  
- 命名：R-070  
- 受入：R-080  

---

## 付録B. File Status（スタブ用Enum例）

> 注：これは **v1.1のI/Oスタブ運用** のための “読みやすさ目的” の例。完全互換（全コード網羅）は後続で拡張。

```csharp
public enum CobolFileStatus
{
    Ok_00,          // "00" Success
    EndOfFile_10,   // "10" End of file (common)
    RecordNotFound_23, // "23" Key not found (common)
    Unknown
}
```

---

## 付録C. Coverage Matrix（監査サマリ）

- **Total Elements Analyzed**: 10  
- **Coverage (OK/Stub)**: 50%  
- **Pending (TODO)**: 30%  
- **Undefined**: 20% → v1.1で **R-027 / R-062** を追加し、未定義要素を解消（監査の更新で再集計推奨）

---

## 参照した入力（監査根拠）
- CobolToCsharpRules.md（v1） fileciteturn1file0  
- CoverageMatrix.md fileciteturn1file1  
- MissingList.md fileciteturn1file2  
- ProposedRules.md fileciteturn1file3
