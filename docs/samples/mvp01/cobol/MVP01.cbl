       IDENTIFICATION DIVISION.
       PROGRAM-ID. MVP01.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE
               ASSIGN TO "INFILE.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT OUTFILE
               ASSIGN TO "OUTFILE.DAT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 40 CHARACTERS
           RECORDING MODE IS F.
       01  INREC.
           05 IN-CUST-ID        PIC 9(5).
           05 IN-NAME           PIC X(20).
           05 IN-QTY            PIC 9(3).
           05 IN-UNIT-PRICE     PIC 9(5).
           05 IN-FILLER         PIC X(7).

       FD  OUTFILE
           RECORD CONTAINS 60 CHARACTERS
           RECORDING MODE IS F.
       01  OUTREC.
           05 OUT-CUST-ID       PIC 9(5).
           05 OUT-NAME          PIC X(20).
           05 OUT-QTY           PIC 9(3).
           05 OUT-UNIT-PRICE    PIC 9(5).
           05 OUT-TOTAL         PIC 9(7).
           05 OUT-BIG-FLAG      PIC X(1).
           05 OUT-FILLER        PIC X(17).

       WORKING-STORAGE SECTION.
       01  WS-EOF               PIC X VALUE "N".
           88 EOF                         VALUE "Y".
           88 NOT-EOF                     VALUE "N".

       01  WS-TOTAL             PIC 9(7) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           OPEN INPUT INFILE
                OUTPUT OUTFILE

           PERFORM UNTIL EOF
               READ INFILE
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM PROCESS-ONE-RECORD
               END-READ
           END-PERFORM

           CLOSE INFILE OUTFILE
           STOP RUN
           .

       PROCESS-ONE-RECORD.
           *> MOVEで出力項目へ転送
           MOVE IN-CUST-ID     TO OUT-CUST-ID
           MOVE IN-NAME        TO OUT-NAME
           MOVE IN-QTY         TO OUT-QTY
           MOVE IN-UNIT-PRICE  TO OUT-UNIT-PRICE

           *> 合計計算（整数）
           COMPUTE WS-TOTAL = IN-QTY * IN-UNIT-PRICE
           MOVE WS-TOTAL TO OUT-TOTAL

           *> IFでフラグ設定
           IF IN-QTY >= 100
               MOVE "Y" TO OUT-BIG-FLAG
           ELSE
               MOVE "N" TO OUT-BIG-FLAG
           END-IF

           *> 固定長出力
           WRITE OUTREC
           .
