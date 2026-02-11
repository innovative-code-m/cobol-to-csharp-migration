# テストプログラム構造ダイアグラム

## 概要

MVP01テストプログラムの構造を説明するダイアグラムです。COBOLテストプログラム、C#実装プログラム、C#テストコードの関係性と構造を示します。

## テストプログラム全体構造

```mermaid
graph TB
    subgraph "COBOLテストプログラム"
        COBOL[MVP01.cbl<br/>COBOLプログラム]
        COBOL_IN[INFILE.DAT<br/>40バイト固定長]
        COBOL_OUT[OUTFILE.DAT<br/>60バイト固定長]
        COBOL --> COBOL_IN
        COBOL --> COBOL_OUT
    end

    subgraph "C#実装プログラム"
        CS[Program.cs<br/>C#実装]
        CS_IN[INFILE.DAT<br/>40バイト固定長]
        CS_OUT[OUTFILE.DAT<br/>60バイト固定長]
        CS --> CS_IN
        CS --> CS_OUT
    end

    subgraph "C#テストコード"
        TEST[CobolMvpRuntimeTests.cs<br/>xUnitテスト]
        TEST_DATA[テストデータ<br/>メモリ内]
        TEST --> TEST_DATA
    end

    COBOL -.変換.-> CS
    CS -.テスト.-> TEST
    COBOL_OUT -.期待値比較.-> CS_OUT

    style COBOL fill:#e1f5ff
    style CS fill:#fff4e1
    style TEST fill:#e8f5e9
```

## COBOLテストプログラム構造

```mermaid
classDiagram
    class MVP01 {
        <<COBOL Program>>
        +MAIN-SECTION()
        +PROCESS-ONE-RECORD()
    }

    class INREC {
        <<File Record 40 bytes>>
        +IN-CUST-ID PIC 9(5)
        +IN-NAME PIC X(20)
        +IN-QTY PIC 9(3)
        +IN-UNIT-PRICE PIC 9(5)
        +IN-FILLER PIC X(7)
    }

    class OUTREC {
        <<File Record 60 bytes>>
        +OUT-CUST-ID PIC 9(5)
        +OUT-NAME PIC X(20)
        +OUT-QTY PIC 9(3)
        +OUT-UNIT-PRICE PIC 9(5)
        +OUT-TOTAL PIC 9(7)
        +OUT-BIG-FLAG PIC X(1)
        +OUT-FILLER PIC X(17)
    }

    class WORKING_STORAGE {
        +WS-EOF PIC X
        +WS-TOTAL PIC 9(7)
    }

    class INFILE {
        <<Sequential File>>
        +RECORD CONTAINS 40 CHARACTERS
    }

    class OUTFILE {
        <<Sequential File>>
        +RECORD CONTAINS 60 CHARACTERS
    }

    MVP01 --> INREC : reads
    MVP01 --> OUTREC : writes
    MVP01 --> WORKING_STORAGE : uses
    MVP01 --> INFILE : OPEN INPUT
    MVP01 --> OUTFILE : OPEN OUTPUT
    INFILE --> INREC : contains
    OUTFILE --> OUTREC : contains
```

## C#テストコード構造

```mermaid
classDiagram
    class CobolMvpRuntimeTests {
        <<xUnit Test Class>>
        +TransformRecord_KnownInputs_ReturnsExpected(string, string)
        +TransformRecord_QtyExactly100_SetsBigFlagY()
        +TransformRecord_QtyBelow100_SetsBigFlagN()
        +ProcessFile_SampleData_MatchesExpected()
        +ProcessFile_InvalidRecordLength_Throws()
    }

    class Program {
        <<internal>>
        +Main(string[] args) void
        #ProcessFile(string, string) void
        #TransformRecord(string) string
    }

    class Assert {
        <<Xunit>>
        +Equal(expected, actual)
        +Throws~TException~(action)
    }

    class File {
        <<System.IO>>
        +WriteAllLines(path, lines, encoding)
        +ReadAllLines(path, encoding) string[]
        +Exists(path) bool
        +Delete(path)
    }

    CobolMvpRuntimeTests --> Program : tests
    CobolMvpRuntimeTests --> Assert : uses
    CobolMvpRuntimeTests --> File : uses
```

## テスト実行フロー

```mermaid
sequenceDiagram
    participant Test as CobolMvpRuntimeTests
    participant Program as Program
    participant FileIO as File I/O
    participant Assert as Assert

    Note over Test: TransformRecord テスト
    Test->>Program: TransformRecord(input)
    Program->>Program: レコード変換処理
    Program-->>Test: result
    Test->>Assert: Equal(expected, result)

    Note over Test: ProcessFile テスト
    Test->>FileIO: WriteAllLines(テストデータ)
    Test->>Program: ProcessFile(inFile, outFile)
    Program->>FileIO: ReadLine()
    FileIO-->>Program: レコード1
    Program->>Program: TransformRecord()
    Program->>FileIO: WriteLine(出力レコード1)
    loop 全レコード処理
        Program->>FileIO: ReadLine()
        FileIO-->>Program: レコードN
        Program->>Program: TransformRecord()
        Program->>FileIO: WriteLine(出力レコードN)
    end
    Program-->>Test: 完了
    Test->>FileIO: ReadAllLines(outFile)
    FileIO-->>Test: 出力レコード配列
    Test->>Assert: Equal(期待値, 実際値)
    Test->>FileIO: Delete(一時ファイル)
```

## テストケース分類

```mermaid
mindmap
  root((テストプログラム))
    単体テスト
      TransformRecord
        正常系
          既知の入力値
          境界値テスト
            Qty=100
            Qty<100
        異常系
          バイト長検証
    統合テスト
      ProcessFile
        正常系
          サンプルデータ一致
          ファイルI/O
        異常系
          不正レコード長
          例外処理
```

## データフロー

```mermaid
flowchart LR
    subgraph "入力データ"
        IN1["00001YAMADA TARO         00500050       "]
        IN2["00002SUZUKI HANAKO       12000010       "]
        IN3["00003SATO JIRO           10000100       "]
    end

    subgraph "変換処理"
        T1[TransformRecord]
        T2[TransformRecord]
        T3[TransformRecord]
    end

    subgraph "出力データ"
        OUT1["00001YAMADA TARO         005000500000250N                   "]
        OUT2["00002SUZUKI HANAKO       120000100001200Y                   "]
        OUT3["00003SATO JIRO           100001000010000Y                   "]
    end

    IN1 --> T1 --> OUT1
    IN2 --> T2 --> OUT2
    IN3 --> T3 --> OUT3

    style IN1 fill:#e3f2fd
    style IN2 fill:#e3f2fd
    style IN3 fill:#e3f2fd
    style OUT1 fill:#e8f5e9
    style OUT2 fill:#e8f5e9
    style OUT3 fill:#e8f5e9
```

## テストカバレッジ

```mermaid
graph TD
    A[テストプログラム] --> B[TransformRecord]
    A --> C[ProcessFile]

    B --> B1[正常系: 既知の入力値]
    B --> B2[境界値: Qty=100]
    B --> B3[境界値: Qty<100]
    B --> B4[バイト長検証]

    C --> C1[正常系: サンプルデータ]
    C --> C2[異常系: 不正レコード長]
    C --> C3[ファイルI/O]
    C --> C4[例外処理]

    style B fill:#fff4e1
    style C fill:#fff4e1
    style B1 fill:#e8f5e9
    style B2 fill:#e8f5e9
    style B3 fill:#e8f5e9
    style B4 fill:#ffebee
    style C1 fill:#e8f5e9
    style C2 fill:#ffebee
    style C3 fill:#e8f5e9
    style C4 fill:#ffebee
```

## レコード構造比較

```mermaid
graph LR
    subgraph "入力レコード 40バイト"
        IN[INREC]
        IN --> IN1[CUST-ID: 5]
        IN --> IN2[NAME: 20]
        IN --> IN3[QTY: 3]
        IN --> IN4[UNIT-PRICE: 5]
        IN --> IN5[FILLER: 7]
    end

    subgraph "変換処理"
        TRANS[TransformRecord]
        TRANS --> CALC1[TOTAL = Qty × UnitPrice]
        TRANS --> CALC2[BIG-FLAG = Qty >= 100 ? 'Y' : 'N']
    end

    subgraph "出力レコード 60バイト"
        OUT[OUTREC]
        OUT --> OUT1[CUST-ID: 5]
        OUT --> OUT2[NAME: 20]
        OUT --> OUT3[QTY: 3]
        OUT --> OUT4[UNIT-PRICE: 5]
        OUT --> OUT5[TOTAL: 7]
        OUT --> OUT6[BIG-FLAG: 1]
        OUT --> OUT7[FILLER: 19]
    end

    IN --> TRANS
    TRANS --> OUT

    style IN fill:#e3f2fd
    style TRANS fill:#fff4e1
    style OUT fill:#e8f5e9
```

## 関連ファイル

- COBOLテストプログラム: `docs/samples/mvp01/cobol/MVP01.cbl`
- C#実装プログラム: `src/CobolMvpRuntime/Program.cs`
- C#テストコード: `tests/CobolMvpRuntimeTests.cs`
- テストデータ: `samples/mvp01/INFILE.DAT`, `samples/mvp01/OUTFILE_expected.DAT`
- クラス図: `docs/diagrams/CobolMvpRuntime_ClassDiagram.md`
