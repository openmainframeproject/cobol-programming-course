      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBL0004
       AUTHOR.        Otto B. Formatted
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
      *FD -- describes the layout of PRINT-LINE file,
      *including level numbers, variable names, data types and lengths
      *
       01  PRINT-REC.
           05  ACCT-NO-O      PIC X(8).
           05  FILLER         PIC X(02) VALUE SPACES.
      *    FILLER -- COBOL reserved word used as data name to remove 
      *    the need of variable names only for inserting spaces
      *
           05  LAST-NAME-O    PIC X(20).
           05  FILLER         PIC X(02) VALUE SPACES.
      *    SPACES -- used for structured spacing data outputs rather
      *    than using a higher PIC Clause length as in CBL0001.cobol,
      *    which makes a good design practice and a legible output
      *
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
      *    The repeated $ characters revert to spaces and then one $
      *    in front of the printed amount.
      *
           05  FILLER         PIC X(02) VALUE SPACES.
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
           05  FILLER         PIC X(02) VALUE SPACES.
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
       01 FLAGS.
           05 LASTREC         PIC X VALUE SPACE.
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
      *HEADER -- structures for report or column headers,   
      *that need to be setup in WORKING-STORAGE so they can be used
      *in the PROCEDURE DIVISION 
      *
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
               10  WS-CURRENT-SECOND       PIC 9(02).
               10  WS-CURRENT-MILLISECONDS PIC 9(02).
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
       OPEN-FILES-END.
      *OPEN-FILES-END -- consists of an empty paragraph suffixed by
      *-END that ends the past one and serves as a visual delimiter
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
      *    PERFORM -- in this case transfers control to another
      *    paragraph of the code, executes it and returns control to
      *    the following line.
      *
            PERFORM UNTIL LASTREC = 'Y'
      *     here PERFORM allows a loops to be entered   
      *
            PERFORM WRITE-RECORD
            PERFORM READ-RECORD
            END-PERFORM
            .
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
       WRITE-RECORD.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           WRITE PRINT-REC.
      *
