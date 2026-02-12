<#
概要:
  ソース内の //TODO(TAG): message 形式コメントを抽出し、
  優先度スコア付きで一覧化して CSV / Markdown を生成するスクリプト。

Usage:
  # 既定値（RepoRoot=カレント、OutDir=docs/audit）で実行
  powershell -ExecutionPolicy Bypass -File .\tools\verifier\extract-todos.ps1

  # 出力先を変更して実行
  powershell -ExecutionPolicy Bypass -File .\tools\verifier\extract-todos.ps1 -OutDir "docs/audit"

  # リポジトリルートを明示して実行
  powershell -ExecutionPolicy Bypass -File .\tools\verifier\extract-todos.ps1 -RepoRoot "d:\dev\cobol-to-csharp-migration"
#>

param(
  [string]$RepoRoot = (Resolve-Path ".").Path,
  [string]$OutDir   = "docs/audit",
  [string]$Pattern  = '^\s*//TODO\((?<tag>[^)]+)\)\s*:\s*(?<msg>.*)$'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$root = Resolve-Path $RepoRoot
$outDirFull = Join-Path $root $OutDir
New-Item -ItemType Directory -Force -Path $outDirFull | Out-Null

# 対象：バックエンドの C# を中心に（必要なら後で拡張）
$targets = @(
  "apps/backend/src",
  "apps/backend/tests",
  "tools"
) | ForEach-Object { Join-Path $root $_ } | Where-Object { Test-Path $_ }

$items =
  $targets |
  ForEach-Object {
    Get-ChildItem -Path $_ -Recurse -File -Include *.cs,*.ps1,*.md -ErrorAction SilentlyContinue
  } |
  ForEach-Object {
    $file = $_.FullName
    Select-String -Path $file -Pattern $Pattern -AllMatches |
      ForEach-Object {
        # Select-String は 1行に複数マッチもあり得るが、TODOは通常1行1件想定
        $m = $_.Matches[0]
        [pscustomobject]@{
          Tag      = $m.Groups["tag"].Value.Trim()
          Message  = $m.Groups["msg"].Value.Trim()
          File     = (Resolve-Path $file).Path.Replace($root.Path + [System.IO.Path]::DirectorySeparatorChar, "")
          Line     = $_.LineNumber
          RawLine  = $_.Line.Trim()
        }
      }
  } |
  Sort-Object Tag, File, Line

# 優先度付け（ルール：最低限の自動スコア）
# - 実行時クラッシュ/ビルド破壊/データ不整合っぽい語を重く
# - "MVP"が若い/早いほど（MVP02など）優先度を上げる例
function Get-PriorityScore([string]$tag, [string]$msg) {
  $score = 0

  if ($msg -match "crash|例外|exception|落ちる|停止|fatal") { $score += 50 }
  if ($msg -match "データ|不整合|整合|loss|欠落|破壊|誤") { $score += 40 }
  if ($msg -match "未実装|unsupported|未対応") { $score += 20 }
  if ($msg -match "性能|performance|遅い") { $score += 10 }

  # Tag に MVPxx が含まれる場合、数字が小さいほど加点（例：MVP02 > MVP10）
  if ($tag -match "MVP(?<n>\d+)") {
    $n = [int]$Matches["n"]
    $score += [Math]::Max(0, 30 - $n)  # MVP02なら28点、MVP20なら10点
  }

  return $score
}

$items =
  $items |
  Select-Object *,
    @{Name="Score"; Expression={ Get-PriorityScore $_.Tag $_.Message }},
    @{Name="Priority"; Expression={
      $s = Get-PriorityScore $_.Tag $_.Message
      if ($s -ge 70) { "P0" }
      elseif ($s -ge 45) { "P1" }
      elseif ($s -ge 25) { "P2" }
      else { "P3" }
    }}

# 出力（CSV）
$csvPath = Join-Path $outDirFull "TodoInventory.csv"
$items | Export-Csv -Path $csvPath -NoTypeInformation -Encoding UTF8

# 出力（Markdown）
$mdPath = Join-Path $outDirFull "TodoInventory.md"
$now = Get-Date -Format "yyyy-MM-dd HH:mm:ss"

$grouped =
  $items |
  Group-Object Priority |
  Sort-Object Name

$md = New-Object System.Collections.Generic.List[string]
$md.Add("# TODO Inventory")
$md.Add("")
$md.Add("- Generated: $now")
$md.Add("- Pattern: ``$Pattern``")
$md.Add("")
$md.Add("## Summary")
$md.Add("")
$md.Add("| Priority | Count |")
$md.Add("|---:|---:|")
foreach ($g in $grouped) {
  $md.Add("| $($g.Name) | $($g.Count) |")
}
$md.Add("")

foreach ($g in $grouped) {
  $md.Add("## $($g.Name)")
  $md.Add("")
  $md.Add("| Score | Tag | Message | File:Line |")
  $md.Add("|---:|---|---|---|")
  foreach ($x in ($g.Group | Sort-Object -Property `
      @{Expression="Score"; Descending=$true}, `
      @{Expression="Tag"; Descending=$false}, `
      @{Expression="File"; Descending=$false}, `
      @{Expression="Line"; Descending=$false})) {
    $loc = "$($x.File):$($x.Line)"
    $msg = $x.Message.Replace("|","\\|")
    $md.Add("| $($x.Score) | $($x.Tag) | $msg | $loc |")
  }
  $md.Add("")
}

$mdText = $md -join "`n"
[System.IO.File]::WriteAllText($mdPath, $mdText, [System.Text.UTF8Encoding]::new($true))

Write-Host "OK: $csvPath"
Write-Host "OK: $mdPath"
