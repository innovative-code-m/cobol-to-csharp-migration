# COBOL → C# Migration Kit

COBOL資産をC#（.NET）へ移行する際に必要となる「変換ルール」「監査（テスト素材）」「正当性検証」の成果物を、GitHubで継続的に育てるためのリポジトリです。

本リポジトリは、**いきなり変換ツールを作らず**、まず **変換ルールと監査ループを固定**してから自動化に進む方針を採用します。  
（REDEFINES / OCCURS DEPENDING ON / COPYBOOK などの難所で“事故らない”ことを最優先）

---

## 目的

- COBOL→C#リプレース案件に向けて、営業・提案時に提示できる **実務直結の成果物**を整備する
- 変換に伴うリスク（仕様誤解、データ不整合、移行漏れ）を **監査と検証の仕組み**で最小化する
- ルール集を中心に、サンプルと検証手順を揃え、将来的な半自動変換へ段階的に拡張する

---

## 成果物（Deliverables）

- **変換ルール集（Rulebook）**  
  COBOL構文・慣習をC#へ落とすためのルールを、番号付き（R-001…）で管理

- **監査用サンプル（Audit Samples）**  
  ルールの網羅性を確認するための COBOL + Copybook + 入力データ例 + 期待結果例 を蓄積

- **検証（Verification）**  
  ファイル→DBMS移行後の正当性チェック、差分比較、集計突合などの観点・手順・ツール方針

- **変換支援ツール（Tools）**（将来拡張）  
  ルールに基づく変換補助、静的解析、チェック、テストハーネス等

---

## リポジトリ構成

```
cobol-to-csharp-migration/
├── .gitignore
├── package.json              # Node.js（React）依存関係
├── package-lock.json
├── README.md
│
├── .vscode/
│   ├── README-ai-usage.md
│   └── README-ai-usage.md.txt
│
├── docs/                     # 成果物・ドキュメント
│   ├── audit/                # 監査用成果物
│   │   ├── CoverageMatrix.md
│   │   ├── MissingList.md
│   │   └── ProposedRules.md
│   ├── prompts/              # AI用プロンプト
│   │   ├── audit/
│   │   │   └── 11_SpecAudit.prompt.md
│   │   └── dev/
│   │       └── 01_Implement.prompt.md
│   ├── rules/                # 変換ルール集
│   │   └── CobolToCsharpRules.md
│   ├── samples/              # 監査用サンプル（COBOL / C#）
│   │   ├── cobol/
│   │   └── csharp/
│   └── verification/         # 検証用素材
│       └── testdata/
│
├── public/                   # React 静的アセット
│   ├── favicon.ico
│   ├── index.html
│   ├── logo192.png
│   ├── logo512.png
│   ├── manifest.json
│   └── robots.txt
│
├── src/                      # React フロントエンド
│   ├── App.css
│   ├── App.js
│   ├── App.test.js
│   ├── index.css
│   ├── index.js
│   ├── logo.svg
│   ├── reportWebVitals.js
│   └── setupTests.js
│
└── tools/                    # 変換・検証ツール（将来拡張）
    ├── converter/
    └── verifier/
```

