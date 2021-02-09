      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBL0106
       AUTHOR.        Otto B. Boolean.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05  ACCT-NO-O      PIC X(8).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  LAST-NAME-O    PIC X(20).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
           05  FILLER         PIC X(02) VALUE SPACES.
      *
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
           05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
           05  LAST-NAME          PIC X(20).
           05  FIRST-NAME         PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR    PIC X(25).
               10  CITY-COUNTY    PIC X(20).
               10  USA-STATE      PIC X(15).
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
      *
       WORKING-STORAGE SECTION.
       01  Filler.
           05 LASTREC          PIC X VALUE SPACE.
           05 DISP-SUB1        PIC 9999.
           05 SUB1             PIC 99.

         01 OVERLIMIT.
           03 FILLER OCCURS 5  TIMES.
               05  OL-ACCT-NO            PIC X(8).
               05  OL-ACCT-LIMIT         PIC S9(7)V99 COMP-3.
               05  OL-ACCT-BALANCE       PIC S9(7)V99 COMP-3.
               05  OL-LASTNAME           PIC X(20).
               05  OL-FIRSTNAME          PIC X(15).


      *
       01  CLIENTS-PER-STATE.
           05 FILLER              PIC X(19) VALUE
              'Virginia Clients = '.
           05 VIRGINIA-CLIENTS    PIC 9(3) VALUE ZERO.
           05 FILLER              PIC X(59) VALUE SPACES.

       01  OVERLIMIT-STATUS.
           05 OLS-STATUS          PIC X(30) VALUE
              'No Accounts Overlimit '.
           05 OLS-ACCTNUM         PIC XXXX VALUE SPACES.
           05 FILLER              PIC X(45) VALUE SPACES.

      *
       01  HEADER-1.
           05  FILLER         PIC X(20) VALUE 'Financial Report for'.
           05  FILLER         PIC X(60) VALUE SPACES.
      *
       01  HEADER-2.
           05  FILLER         PIC X(05) VALUE 'Year '.
           05  HDR-YR         PIC 9(04).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE 'Month '.
           05  HDR-MO         PIC X(02).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(04) VALUE 'Day '.
           05  HDR-DAY        PIC X(02).
           05  FILLER         PIC X(56) VALUE SPACES.
      *
       01  HEADER-3.
           05  FILLER         PIC X(08) VALUE 'Account '.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE 'Last Name '.
           05  FILLER         PIC X(15) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE 'Limit '.
           05  FILLER         PIC X(06) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE 'Balance '.
           05  FILLER         PIC X(40) VALUE SPACES.
      *
       01  HEADER-4.
           05  FILLER         PIC X(08) VALUE '--------'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '----------'.
           05  FILLER         PIC X(15) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '----------'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(13) VALUE '-------------'.
           05  FILLER         PIC X(40) VALUE SPACES.
      *
       01 WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
               10  WS-CURRENT-SECOND       PIC 9(02).
               10  WS-CURRENT-MILLISECONDS PIC 9(02).
      *
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
      *
       WRITE-HEADERS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR  TO HDR-YR.
           MOVE WS-CURRENT-MONTH TO HDR-MO.
           MOVE WS-CURRENT-DAY   TO HDR-DAY.
           WRITE PRINT-REC FROM HEADER-1.
           WRITE PRINT-REC FROM HEADER-2.
           MOVE SPACES TO PRINT-REC.
           WRITE PRINT-REC AFTER ADVANCING 1 LINES.
           WRITE PRINT-REC FROM HEADER-3.
           WRITE PRINT-REC FROM HEADER-4.
           MOVE SPACES TO PRINT-REC.
           MOVE 1 TO SUB1.
      *
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM IS-STATE-VIRGINIA
            PERFORM IS-OVERLIMIT
            PERFORM WRITE-RECORD
            PERFORM READ-RECORD
            END-PERFORM
           .
      *
       CLOSE-STOP.
           WRITE PRINT-REC FROM CLIENTS-PER-STATE.
           PERFORM WRITE-OVERLIMIT.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           GOBACK.
      *
       READ-RECORD.
           READ ACCT-REC
           AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       IS-OVERLIMIT.
           IF ACCT-LIMIT < ACCT-BALANCE THEN
               MOVE ACCT-LIMIT TO OL-ACCT-LIMIT(SUB1)
               MOVE ACCT-BALANCE TO OL-ACCT-BALANCE(SUB1)
               MOVE LAST-NAME TO OL-LASTNAME(SUB1)
               MOVE FIRST-NAME TO OL-FIRSTNAME(SUB1)
            END-IF.
            ADD 1 TO SUB1.

       IS-STATE-VIRGINIA.
           IF USA-STATE = 'Virginia' THEN
              ADD 1 TO VIRGINIA-CLIENTS
           END-IF.
      *
       WRITE-OVERLIMIT.
           IF SUB1 = 1 THEN
               MOVE OVERLIMIT-STATUS TO PRINT-REC
               WRITE PRINT-REC
           ELSE
               MOVE 'ACCOUNTS OVERLIMIT' TO OLS-STATUS
               MOVE SUB1 TO  DISP-SUB1
               MOVE DISP-SUB1 TO OLS-ACCTNUM
               MOVE OVERLIMIT-STATUS TO PRINT-REC
               WRITE PRINT-REC
           END-IF.
      *
       WRITE-RECORD.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           WRITE PRINT-REC.
      *
