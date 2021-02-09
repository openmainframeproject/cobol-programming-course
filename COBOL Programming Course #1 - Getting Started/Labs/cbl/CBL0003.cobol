      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBL0003
       AUTHOR.        Otto B. Fun.
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
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
           05  LAST-NAME-O    PIC X(20).
           05  FIRST-NAME-O   PIC X(15).
           05  COMMENTS-O     PIC X(50).
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
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.
       01 COUNTER             PIC 9(2) VALUE 0.
      *COUNTER is used for the PERFORM VARYING statement
      *------------------
       PROCEDURE DIVISION.
      *------------------
       1000-OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
       1000-OPEN-FILES-END.
      *The prefix "1000" is increased thoughout the code and
      *is used as a programming technique to better locate
      *referenced paragraphs in a sentence.
      *The paragraph suffixed with "-END" is an empty one
      *that serves as a visual delimiter for ending a paragraph
      *similiar to a "}" in other programming languages
      *
       2000-READ-FIRST-RECORD.
           PERFORM 4000-READ-RECORD.
           PERFORM 5000-WRITE-RECORD.
           GO TO 2100-READ-TEN-RECORDS.
      *GO TO passes control to another section of the code, but
      *unlike the PERFORM keyword, it will not return control
      *to the next line in the code
       2000-READ-FIRST-RECORD-END.
      *
           DISPLAY ' THIS IS THE FIRST RECORD '.
      *notice that because of GO TO, this command will
      *never be executed
      *
       2100-READ-TEN-RECORDS.
           PERFORM 10 TIMES
            PERFORM 4000-READ-RECORD
            PERFORM 5000-WRITE-RECORD
           END-PERFORM.
       2100-READ-TEN-RECORDS-END.
      *TIMES repeats a perform statement, it's number can be
      *either set or given by a variable
      *
       2200-READ-ANOTHER-RECORD.
           PERFORM 4000-READ-RECORD THRU 5000-WRITE-RECORD.
       2200-READ-ANOTHER-RECORD-END.
      *THRU or THROUGH list the start and end of which
      *paragraphs will be executed in a sequential order
      *
       2300-READ-NEXT-RECORDS.
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER
           EQUAL 34
            PERFORM 4000-READ-RECORD THRU 5000-WRITE-RECORD
           END-PERFORM.
       2300-READ-NEXT-RECORDS-END.
      *Loop printing the remaining 34 records by varying the
      *variable COUNTER after each iteration.
      *The sentence PERFORM VARYING is continued on the next
      *line by starting on the B Area (column 12)
      *
       2400-CALLING-SUBPROGRAM.
           CALL 'HELLO'.
       2400-CALLING-SUBPROGRAM-END.
      *The program HELLO from the first Lab is called as a
      *subprogram from CBL0003. The JCL that compiles CBL0003
      *also compiles HELLO and links them together.
      *
       3000-CLOSE-STOP.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           GOBACK.
      *Without GOBACK here, the next paragraphs would
      *execute once more
       3000-CLOSE-STOP-END.
      *
       4000-READ-RECORD.
           READ ACCT-REC
           AT END MOVE 'Y' TO LASTREC
           END-READ.
       4000-READ-RECORD-END.
      *
       5000-WRITE-RECORD.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           MOVE FIRST-NAME   TO  FIRST-NAME-O.
           MOVE COMMENTS     TO  COMMENTS-O.
           WRITE PRINT-REC.
       5000-WRITE-RECORD-END.
      *
