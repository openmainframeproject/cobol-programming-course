      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBL0009
       AUTHOR.        Otto B. Mathwiz.
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
       01  FLAGS.
           05 LASTREC          PIC X VALUE SPACE.
      *
       01  TLIMIT-TBALANCE.
           05 TLIMITED            PIC S9(9)V99 COMP-3 VALUE ZERO.
           05 TBALANCE            PIC S9(9)V99 COMP-3 VALUE ZERO.
      *    TLIMITED -- variable for total of all client's
      *    bank account limits.
      *    TBALANCE -- variable for total of all client's
      *    bank account balances.
      *    The PIC Clause S9 allows representation of positive and
      *    negative balances. Using PIC Clause $$$,$$$,$$9.99 
      *    only a positive total balance could be displayed . 
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
       01  TRAILER-1.
           05  FILLER         PIC X(31) VALUE SPACES.
           05  FILLER         PIC X(14) VALUE '--------------'.
           05  FILLER         PIC X(01) VALUE SPACES.
           05  FILLER         PIC X(14) VALUE '--------------'.
           05  FILLER         PIC X(40) VALUE SPACES.
      *
       01  TRAILER-2.
           05  FILLER         PIC X(22) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE 'Totals ='.
           05  FILLER         PIC X(01) VALUE SPACES.
           05  TLIMIT-O       PIC $$$,$$$,$$9.99.
           05  FILLER         PIC X(01) VALUE SPACES.
           05  TBALANCE-O     PIC $$$,$$$,$$9.99.
           05  FILLER         PIC X(40) VALUE SPACES.
      *    Just like HEADER, TRAILER formats the report for
      *    total client account limit and balance
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
      *
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM LIMIT-BALANCE-TOTAL
            PERFORM WRITE-RECORD
            PERFORM READ-RECORD
            END-PERFORM
           .
      * 
       WRITE-TLIMIT-TBALANCE.
            MOVE TLIMIT   TO TLIMIT-O.
            MOVE TBALANCE TO TBALANCE-O.
            WRITE PRINT-REC FROM TRAILER-1.
            WRITE PRINT-REC FROM TRAILER-2.
      *
       CLOSE-STOP.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           GOBACK.
      *
       READ-RECORD.
           READ ACCT-REC
           AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
      *     The LIMIT-BALANCE-TOTAL paragraph performs an arithmetic
      *     statement for each client through the loop,
      *     in order to calculate the final limit and balance report.
      *
       LIMIT-BALANCE-TOTAL.
           COMPUTE TLIMIT   = TLIMIT   + ACCT-LIMIT   END-COMPUTE
           COMPUTE TBALANCE = TBALANCE + ACCT-BALANCE END-COMPUTE
           .
      *    The COMPUTE verb assigns the value of the arithmetic 
      *    expression to the TLIMIT and TBALANCE data items.
      *    Since the expression only includes an addition operation,
      *    the statements can also be written as:
      *    ADD ACCT-LIMIT TO TLIMIT.
      *    ADD ACCT-BALANCE TO TBALANCE.
      *    Or, alternatively specifying the target variable:
      *    ADD ACCT-LIMIT TO TLIMIT GIVING TLIMIT. 
      *    ADD ACCT-BALANCE TO TBALANCE GIVING TLIMIT.
      *    A END-COMPUTE or END-ADD stetement is optional.
      *
       WRITE-RECORD.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           WRITE PRINT-REC.
      *
