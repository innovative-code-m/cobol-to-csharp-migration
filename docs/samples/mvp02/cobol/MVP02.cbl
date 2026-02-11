       IDENTIFICATION DIVISION.
       PROGRAM-ID. MVP02.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE  ASSIGN TO "INFILE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "OUTFILE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE.
       01  IN-REC.
           05 IN-ID        PIC X(5).
           05 IN-SEP1      PIC X(1).
           05 IN-NAME      PIC X(20).
           05 IN-SEP2      PIC X(1).
           05 IN-AMOUNT    PIC 9(5).

       FD  OUTFILE.
       01  OUT-REC         PIC X(60).

       WORKING-STORAGE SECTION.
       01  WS-EOF          PIC X VALUE "N".
           88  EOF         VALUE "Y".
           88  NOT-EOF     VALUE "N".

       01  WS-FIRST        PIC X(10) VALUE SPACES.
       01  WS-LAST         PIC X(10) VALUE SPACES.

       01  WS-OUT          PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INFILE
                OUTPUT OUTFILE

           PERFORM UNTIL EOF
               READ INFILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       PERFORM PROCESS-ONE
               END-READ
           END-PERFORM

           CLOSE INFILE
                 OUTFILE

           STOP RUN.

       PROCESS-ONE.
           MOVE SPACES TO WS-FIRST
           MOVE SPACES TO WS-LAST

           * 変換器が未対応になりがちな構文（ここが //TODO の狙い）
           UNSTRING IN-NAME
               DELIMITED BY SPACE
               INTO WS-FIRST WS-LAST
           END-UNSTRING

           * これも未対応になりがち（小文字→大文字の例）
           INSPECT WS-LAST
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
                        TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           END-INSPECT

           MOVE SPACES TO WS-OUT
           STRING
               IN-ID        DELIMITED BY SIZE
               ","          DELIMITED BY SIZE
               WS-LAST      DELIMITED BY SPACE
               ","          DELIMITED BY SIZE
               WS-FIRST     DELIMITED BY SPACE
               ","          DELIMITED BY SIZE
               IN-AMOUNT    DELIMITED BY SIZE
               INTO WS-OUT
           END-STRING

           MOVE WS-OUT TO OUT-REC
           WRITE OUT-REC
           .
