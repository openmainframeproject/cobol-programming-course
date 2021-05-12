       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           ADDAMT.
      *******************************************************
      *    This program accepts input and displays output    *
      *******************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  KEYED-INPUT.
           05  CUST-NO-IN                  PIC X(15).
           05  AMT1-IN                     PIC 9(5).
           05  AMT2-IN                     PIC 9(5).
           05  AMT3-IN                     PIC 9(5).
       01  DISPLAYED-OUTPUT.
           05  CUST-NO-OUT                 PIC X(15).
           05  TOTAL-OUT                   PIC 9(6).
       01  MORE-DATA                       PIC X(3) VALUE 'YES'.
       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM UNTIL MORE-DATA = 'NO '
               DISPLAY 'ENTER NAME       (15 CHARACTERS)'
               ACCEPT CUST-NO-IN
               DISPLAY 'Enter amount of first purchase (5 digits)'
               ACCEPT AMT1-IN
               DISPLAY 'Enter amount of second purchase (5 digits)'
               ACCEPT AMT2-IN
               DISPLAY 'Enter amount of third purchase (5 digits)'
               ACCEPT AMT3-IN
               MOVE CUST-NO-IN TO CUST-NO-OUT
               ADD AMT1-IN  AMT2-IN  AMT3-IN
                   GIVING TOTAL-OUT
               DISPLAY CUST-NO-OUT 'Total Amount = ' TOTAL-OUT
               DISPLAY 'MORE INPUT DATA (YES/NO)?'
               ACCEPT MORE-DATA
               INSPECT MORE-DATA CONVERTING 'noyes' to 'NOYES'
           END-PERFORM
           GOBACK.
