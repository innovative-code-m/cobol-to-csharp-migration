# COBOL → C# Migration Kit

COBOL資産を C#（.NET）へ移行する際に必要となる
**「変換ルール」「監査（テスト素材）」「正当性検証」** を中心に、
GitHub 上で **継続的に育てていくための実務向けリポジトリ** です。

本リポジトリは、**いきなり変換ツールを作らない** ことを前提とし、
まず **変換ルールと監査ループを固定** してから、自動化・半自動化へ進む方針を採用しています。

特に以下のような COBOL 特有の難所で
「変換事故を起こさないこと」を最優先とします。

- REDEFINES
- OCCURS / OCCURS DEPENDING ON
- COPYBOOK
- 集計・桁・符号・編集項目

---

## 目的

- COBOL → C# リプレース案件において
  **営業・提案時に提示できる実務直結の成果物** を整備する
- 変換に伴うリスク（仕様誤解・データ不整合・移行漏れ）を
  **監査と検証の仕組み** によって最小化する
- ルール集を中核に、サンプル・検証手順を蓄積し、
  将来的な **半自動変換ツール** へ段階的に拡張する

---

## 成果物（Deliverables）

### 1. 変換ルール集（Rulebook）
- COBOL 構文・慣習を C# へ変換するためのルールを番号付き（R-001…）で管理
- 各ルールは以下を含む
  - 適用範囲
  - 変換方針
  - 受入条件
  - Before / After 例
  - 注意点・未解決事項

### 2. 監査用サンプル（Audit Samples）
- ルールの網羅性・妥当性を確認するためのテスト素材
- COBOL + Copybook + 入力データ例 + 期待結果例 をセットで管理

### 3. 検証（Verification）
- ファイル → DBMS 移行後の正当性確認
- 件数突合、集計比較、差分検出などの観点・手順・ツール方針

### 4. 変換支援ツール（Tools）※将来拡張
- ルールに基づく変換補助
- 静的解析
- チェック・テストハーネス

---

## バージョン管理方針（Tags）

本リポジトリでは、**実装・検証の節目を Git タグで固定**します。

- 命名ルール: `YYYY-MM-DD_連番2桁_Implement`（例: `2026-02-11_01_Implement`）

- 例：
  - `2026-02-03_01_Implement`
    - OrderValidation 実装および関連ルール・監査成果物のスナップショット

タグ単位で
「この時点で何が確定していたか」
「どこまで検証できていたか」
を再現可能にすることを目的としています。

---

## リポジトリ構成

```
cobol-to-csharp-migration/
├── README.md
├── .gitignore
├── .gitattributes
│
├── .ai/                    # AI開発支援用コンテキスト
│   ├── context/
│   │   └── glossary.md
│   └── tasks/
│       ├── backlog.md
│       └── pr-checklist.md
│
├── .claude/                 # Claude AI設定
│   └── settings.json
│
├── .vscode/                 # VS Code / Cursor 設定
│   ├── extensions.json
│   ├── settings.json
│   ├── tasks.json
│   └── README-ai-usage.md
│
├── apps/                    # アプリケーション
│   ├── backend/             # C# バックエンド
│   │   ├── CobolToCsharpMigration.sln
│   │   ├── CobolToCsharpMigration.csproj
│   │   ├── src/
│   │   │   ├── CobolMvpRuntime/    # MVP01 COBOL→C#変換実装
│   │   │   │   ├── CobolMvpRuntime.csproj
│   │   │   │   └── Program.cs
│   │   │   └── OrderService.cs
│   │   └── tests/
│   │       ├── CobolMvpRuntimeTests.cs
│   │       ├── OrderServiceTests.cs
│   │       └── CobolToCsharpMigration.Tests.csproj
│   └── frontend/            # React フロントエンド
│       ├── package.json
│       ├── package-lock.json
│       ├── src/
│       │   ├── App.js
│       │   ├── App.css
│       │   ├── App.test.js
│       │   ├── index.js
│       │   ├── index.css
│       │   ├── logo.svg
│       │   ├── reportWebVitals.js
│       │   └── setupTests.js
│       └── public/
│           ├── index.html
│           ├── favicon.ico
│           ├── logo192.png
│           ├── logo512.png
│           ├── manifest.json
│           └── robots.txt
│
├── docs/                   # 成果物・ドキュメント
│   ├── rules/              # 変換ルール集
│   │   └── CobolToCsharpRules.md
│   ├── audit/              # 監査成果物
│   │   ├── CoverageMatrix.md
│   │   ├── MissingList.md
│   │   └── ProposedRules.md
│   ├── prompts/            # AI 実行用プロンプト
│   │   ├── dev/
│   │   │   ├── 01_Implement.prompt.md
│   │   │   ├── 02_Refactor.prompt.md
│   │   │   └── 03_Test.prompt.md
│   │   ├── audit/
│   │   │   ├── 11_SpecAudit.prompt.md
│   │   │   └── 12_CoverageMatrix.prompt.md
│   │   ├── editor/
│   │   │   └── 21_Integrate.prompt.md
│   │   ├── exec/
│   │   │   ├── README.md
│   │   │   ├── 2026-02-03_01_Implement.OrderValidation.md
│   │   │   ├── 2026-02-08_01_Implement_CobolMvpRuntime.md
│   │   │   └── 2026-02-11_01_Implement_TodoHandling.md
│   │   ├── refactor/
│   │   │   └── 21_LargeRefactor.prompt.md
│   │   └── README.md
│   ├── decisions/          # アーキテクチャ決定記録（ADR）
│   │   ├── ADR-0001_Tooling.md
│   │   ├── ADR-0002_AI-Roles.md
│   │   └── ADR-0003_Workflow.md
│   ├── diagrams/           # ダイアグラム
│   │   ├── CobolMvpRuntime_ClassDiagram.md
│   │   └── TestProgram_Structure.md
│   ├── spec/               # プロジェクト仕様書
│   │   ├── 00_ProjectCharter.md
│   │   ├── 10_Architecture.md
│   │   ├── 20_CodingRules.md
│   │   └── 30_TestStrategy.md
│   ├── samples/            # 監査用サンプル
│   │   └── mvp01/
│   │       ├── cobol/
│   │       │   └── MVP01.cbl
│   │       ├── expected/
│   │       │   └── OUTFILE_expected.DAT
│   │       └── input/
│   │           └── INFILE.DAT
│   └── verification/       # 検証用素材
│       └── testdata/
│
├── samples/                # サンプルデータ
│   └── mvp01/
│       ├── INFILE.DAT
│       ├── OUTFILE.DAT
│       ├── OUTFILE_actual.DAT
│       └── OUTFILE_expected.DAT
│
└── tools/                  # 変換・検証ツール（将来拡張）
    ├── converter/
    ├── verifier/
    ├── lint.ps1
    └── run-tests.ps1
```


---

## 想定利用シーン

- COBOL → C# リプレース案件の事前調査・提案
- 既存 COBOL 資産の構造把握・リスク洗い出し
- AI を活用したルールベース移行フローの検証・構築
- 移行プロジェクトの **技術ポートフォリオ**

---

## 方針メモ

- 変換精度より **再現性・説明可能性** を優先
- 「なぜその変換になるか」をルールとして残す
- AI は **実装・監査・整理の役割分担** で使用する
