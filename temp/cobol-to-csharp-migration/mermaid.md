```mermaid
flowchart TB
  subgraph L[チャット運用]
    L1[AI① ChatGPT\n統合・編集] --> L2[途中成果物\nmd / Missing / diff画像]
    L2 --> L3[チャットへアップロード]
    L3 --> L4[AI② Gemini\n監査・Missing生成]
    L4 --> L5[結果をダウンロード/コピペ]
    L5 --> L6[AI③ Claude/Codex\n改修・実装]
    L6 --> L7[更新版を再アップロード]
    L7   --> L1
  end

  subgraph R[IDE運用（repo内で完結）]
    R0[(Git repo)] --> R1[docs/ が唯一の正\nspec / prompts / decisions]
    R1 --> R2[AI役割分担で作業\n（拡張/Agent）]
    R2 --> R3[差分（patch）適用]
    R3 --> R4[ローカル実行\nbuild/test]
    R4 --> R5[git diff / commit / tag]
    R5 --> R1
  end

  L2 -.課題: 運搬コスト.- R1
  ``` 

```mermaid
flowchart TB
  T[AI役割分担（モデル名ではなく“役割”で固定）]

  T --> A[🧭 ChatGPT\n統合・仕様/ルールの最終決定\n成果物（docs/）の編集長]
  T --> B[🔎 Gemini\n監査・抜け漏れ検出\nCoverage / Missing / 優先度付け]
  T --> C[🛡️ Claude\n探索・大改修\n複数ファイル横断リファクタ]
  T --> D[⚙️ Codex系\n実装・テスト生成\n差分（patch）で出力]

  A --> E[(Single Source of Truth)\nrepo/docs/]
  B --> E
  C --> E
  D --> E
```
```mermaid
flowchart TB
  %% =========================
  %% AI分業ポスター
  %% =========================

  subgraph RULE[AI運用ルール（固定）]
    R1[1タスク=1AI=1役割]
    R2[設計・仕様の決定権：人間＋ChatGPT]
    R3[正はGit：会話ログは正本にしない]
  end

  subgraph AI[使用AIと役割]
    A[ChatGPT\n編集長/司令塔]
    B[Codex\n実装担当]
    C[Gemini\n監査担当]
    D[Claude\n探索・改修担当]
  end

  subgraph FILE[ファイル別AI割当]
    F1[docs/spec/*.md\n→ ChatGPT]
    F2[docs/decisions/*.md\n→ ChatGPT]
    F3[src/**\n→ Codex]
    F4[tests/**\n→ Codex → Gemini]
    F5[複数ファイル横断\n→ Claude]
  end

  RULE --> AI --> FILE
  FILE --> W["標準フロー：ChatGPTで確定 → Codex実装 → ローカル検証<br/>Gemini監査 → Claude整理 → ChatGPTで更新"]
  ```

  
  
  
  
  
  
```mermaid
%%{init: {"flowchart": {"rankSpacing": 20, "nodeSpacing": 18}} }%%
flowchart TB
  %% =========================
  %% AI分業ポスター（X貼り付け前提：文言短め・折返し抑制）
  %% =========================

  subgraph RULE["AI運用ルール（固定）"]
   direction TB
    R1["1タスク=1AI=1役割<br/>同一チャットで混在しない"]
    R2["決定権：人間＋ChatGPT<br/>設計・仕様はAIに委ねない"]
    R3["正本はGit管理<br/>会話ログは参考扱い"]
  end

  subgraph AI["使用AIと役割"]
   direction TB
    A["ChatGPT<br/>編集長 / 司令塔"]
    B["Codex<br/>実 装担当"]
    C["Gemini<br/>監査担当"]
    D["Claude<br/>探索・改修担当"]
  end

  subgraph FILE["ファイル別AI割当"]
    F1["docs/spec/*.md<br/>ChatGPT：仕様の正本"]
    F2["docs/decisions/*.md<br/>ChatGPT：ADR（判断記録）"]
    F3["src/**<br/>Codex：差分で実装"]
    F4["tests/**<br/>Codex→Gemini：実装後に監査"]
    F5["複数ファイル横断<br/>Claude：構造整理のみ"]
  end

  subgraph FLOW["標準フロー"]
    W["標準フロー：<br/>ChatGPTで確定 → Codex実装 → ローカル検証<br/>Gemini監査 → Claude整理 → ChatGPTで更新"]
  end

  RULE --> AI --> FILE --> FLOW


```
## 2026/01/29 

  
```mermaid
flowchart LR
  %% ===== ポスター：たるい原因の正体（横長・左→右） =====
  %% 超安全版：C2の各項目を「1項目＝2行」に分割してはみ出し防止

  subgraph L["🟦 現状（VSCode＋複数AI分業）"]
    direction TB
    L1["🟦 VSCode運用<br/>ChatGPT＋Gemini＋Claude"]
    L2["🟦 目的：COBOL→C#移行<br/>業務分析→仕様化<br/>ルール化→検証→ツール化"]
    L3["🟨 ハンドオフが多い<br/>コピペ範囲が増える<br/>正本更新が重い"]
    L1 --> L2 --> L3
  end

  subgraph C["🟥 重さの原因（正体）"]
    direction TB
    C1["🟥 AI人数ではない"]
    C2["🟥 手続き摩擦（運用の摩擦）<br/>・正本（docs/ADR/spec）<br/>　更新コストが高い<br/>・差分が収束しない<br/>　→レビューが肥大化<br/>・責任分界が曖昧<br/>　→判断が揺れがち"]
    C3["🟪 結果：<br/>『たるい』と感じる"]
    C1 --> C2 --> C3
  end

  subgraph R["🟩 処方箋（速さを取り込む）"]
    direction TB
    R1["🟦 司令塔で固定<br/>受入基準／正本<br/>変更範囲を明示"]
    R2["🟩 実装は差分生成に限定<br/>変更ファイルを明示"]
    R3["🟨 Geminiで監査固定<br/>Coverage／Missing<br/>仕様逸脱／テスト不足"]
    R4["🟪 ループ：<br/>Plan→Diff→Test→Audit→Update"]
    R1 --> R2 --> R3 --> R4
  end

  L --> C --> R

  %% 強調（見た目）
  classDef blue fill:#e8f2ff,stroke:#1f6feb,stroke-width:2px,color:#0b2e6b;
  classDef red fill:#ffe8e8,stro:q
  ke:#cf222e,stroke-width:2px,color:#6b0b0b;
  classDef green fill:#eaffea,stroke:#2da44e,stroke-width:2px,color:#0b6b2e;
  classDef yellow fill:#fff8dc,stroke:#bf8700,stroke-width:1.5px,color:#5a4300;
  classDef purple fill:#f2e8ff,stroke:#8250df,stroke-width:1.5px,color:#3b1d7a;

  class L1,L2 blue;
  class L3 yellow;

  class C1,C2 red;
  class C3 purple;

  class R1 blue;
  class R2 green;
  class R3 yellow;
  class R4 purple;

```
## 2026/02/03

```mermaid
flowchart TB

%% 1) フォルダ概要（上部の箱）
O["docs/prompts/<br/>
dev/      : 実装・テスト・軽量整理<br/>
audit/    : 仕様監査・網羅性分析<br/>
editor/   : 統合・文書化<br/>
refactor/ : 大規模構造改修"]:::overview

%% 2) 一覧（4ブロック）
subgraph DEV["3.1 dev（実装フェーズ）"]
direction TB
D1["01_Implement.prompt.md<br/><b>役割</b>：差分実装<br/><b>使用タイミング</b>：機能追加・修正時"]:::dev
D2["02_Refactor.prompt.md<br/><b>役割</b>：軽量整理<br/><b>使用タイミング</b>：読みにくくなった時"]:::dev
D3["03_Test.prompt.md<br/><b>役割</b>：テスト実装<br/><b>使用タイミング</b>：実装直後・不足検出後"]:::dev
end

subgraph AUD["3.2 audit（監査フェーズ）"]
direction TB
A1["11_SpecAudit.prompt.md<br/><b>役割</b>：仕様監査<br/><b>使用タイミング</b>：実装・テスト後"]:::audit
A2["12_CoverageMatrix.prompt.md<br/><b>役割</b>：網羅性の可視化<br/><b>使用タイミング</b>：監査直後"]:::audit
end

subgraph EDT["3.3 editor（統合フェーズ）"]
direction TB
E1["21_Integrate.prompt.md<br/><b>役割</b>：統合・編集<br/><b>使用タイミング</b>：作業完了時"]:::editor
end

subgraph RFA["3.4 refactor（構造改修フェーズ）"]
direction TB
R1["21_LargeRefactor.prompt.md<br/><b>役割</b>：構造大改修<br/><b>使用タイミング</b>：構造破綻時のみ"]:::refactor
end

%% 3) 推奨フロー（矢印）
O --> DEV
DEV --> AUD --> EDT
DEV -. "構造破綻時のみ" .-> RFA
RFA -. "収束後に統合へ戻す" .-> EDT

%% 4) スタイル
classDef overview fill:#f7f7f7,stroke:#666,stroke-width:1px,color:#111;
classDef dev fill:#e8f0ff,stroke:#3b6cff,stroke-width:1px,color:#111;
classDef audit fill:#e9fbef,stroke:#1f9d55,stroke-width:1px,color:#111;
classDef editor fill:#fff4e6,stroke:#f08c00,stroke-width:1px,color:#111;
classDef refactor fill:#ffe8e8,stroke:#e03131,stroke-width:1px,color:#111;

style DEV fill:#ffffff,stroke:#3b6cff,stroke-width:1px,rx:8,ry:8
style AUD fill:#ffffff,stroke:#1f9d55,stroke-width:1px,rx:8,ry:8
style EDT fill:#ffffff,stroke:#f08c00,stroke-width:1px,rx:8,ry:8
style RFA fill:#ffffff,stroke:#e03131,stroke-width:1px,rx:8,ry:8

```
## 2026/02/04
```mermaid 
flowchart TB
  %% =========================
  %% X向け：横長になりすぎない（TB + 2分岐）
  %% =========================

  %% --- 上段（全体フロー）
  subgraph TOP[" "]
    direction LR
    A["🧭 指示<br/>dev/01_Implement.prompt.md"]
    B["⚙️ 実行<br/>exec/2026-02-03_01_Implement_*.md"]
    C["🤖 実行AI<br/>Claude 4.5<br/>※Codex相当"]
    D["✅ 結果<br/>dotnet test PASS"]
    A --> B --> C --> D
  end

  %% 分岐のためのジョイント（見えない中継点）
  J(( ))
  C --> J

  %% --- 下段 左（実装系）
  subgraph L[" "]
    direction LR
    L1["🧩 実装<br/>src/OrderService.cs"]
    L2["📌 ルール<br/>金額<=0はfalse"]
    L3["🧪 追加/調整<br/>tests/OrderServiceTests.cs"]
    L4["🔁 再テスト<br/>PASS"]
    L1 --> L2 --> L3 --> L4
  end

  %% --- 下段 右（分業設計系）
  subgraph R[" "]
    direction LR
    R1["🎯 課題<br/>役割分離"]
    R2["🧠 Codex=実装"]
    R3["🔎 Gemini=監査"]
    R4["🧱 Claude=整理"]
    R1 --> R2 --> R3 --> R4
  end

  %% 分岐線
  J --> L1
  J --> R1

  %% =========================
  %% Styles（色付き）
  %% =========================
  classDef spec fill:#E3F2FD,stroke:#1E88E5,stroke-width:2px,color:#0D47A1;
  classDef exec fill:#E8F5E9,stroke:#43A047,stroke-width:2px,color:#1B5E20;
  classDef ai   fill:#FFF3E0,stroke:#FB8C00,stroke-width:2px,color:#E65100;
  classDef pass fill:#E8F5E9,stroke:#2E7D32,stroke-width:3px,color:#1B5E20;
  classDef code fill:#F3E5F5,stroke:#8E24AA,stroke-width:2px,color:#4A148C;
  classDef test fill:#E0F7FA,stroke:#00ACC1,stroke-width:2px,color:#006064;
  classDef todo fill:#FFEBEE,stroke:#E53935,stroke-width:2px,color:#B71C1C;
  classDef joint fill:#FFFFFF,stroke:#FFFFFF,color:#FFFFFF;

  class A spec;
  class B exec;
  class C ai;
  class D pass;

  class L1 code;
  class L2 todo;
  class L3 test;
  class L4 pass;

  class R1 todo;
  class R2 ai;
  class R3 ai;
  class R4 ai;

  class J joint;

  %% 枠（subgraph）を薄く
  style TOP fill:#FFFFFF,stroke:#BDBDBD,stroke-width:1px
  style L   fill:#FFFFFF,stroke:#BDBDBD,stroke-width:1px
  style R   fill:#FFFFFF,stroke:#BDBDBD,stroke-width:1px

  linkStyle default stroke:#607D8B,stroke-width:1.5px


```

# 2026/02/14
```mermaid
flowchart TB
  %% ===== Layout =====
  classDef card fill:#ffffff,stroke:#cbd5e1,stroke-width:1px,rx:10,ry:10,color:#0f172a;
  classDef accent fill:#f8fafc,stroke:#94a3b8,stroke-width:1px,rx:10,ry:10,color:#0f172a;
  classDef strong fill:#ecfeff,stroke:#06b6d4,stroke-width:1.5px,rx:12,ry:12,color:#083344;
  classDef warn fill:#fff7ed,stroke:#fb923c,stroke-width:1.2px,rx:10,ry:10,color:#7c2d12;

  linkStyle default stroke:#64748b,stroke-width:1.2px;

  %% ===== Header =====
  H["COBOL→C# 変換MVP（ルール→実装→検証）"]:::strong

  %% ===== Main lanes =====
  subgraph L["🛠 作る（変換の型を作る）"]
    direction TB
    A["① COBOL構文を観察"]:::card
    B["② 変換ルール集（R-001〜）"]:::accent
    C["③ サンプルCOBOL作成"]:::card
    D["④ C#変換（MVP）"]:::card
  end

  subgraph R["🧪 証明する（正しさを担保する）"]
    direction TB
    E["⑤ xUnitテスト"]:::card
    F["⑥ 実行して一致確認"]:::card
    G["⑦ 検証ログ／差分の可視化"]:::card
  end

  %% ===== Notes =====
  N["⚠ 罠：TODO棚卸しは誤検出に注意<br/>（テスト内文字列まで拾うことがある）"]:::warn

  %% ===== Flow =====
  H --> A --> B --> C --> D
  D --> E --> F --> G

  B -. "前提が固まる" .-> E
  N -. "検出パターンを見直す" .-> G

```
