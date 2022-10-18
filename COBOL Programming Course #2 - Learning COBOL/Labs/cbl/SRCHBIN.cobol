      *-----------------------
      * Copyright Contributors to the COBOL Programming Course
      * SPDX-License-Identifier: CC-BY-4.0
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    SRCHBIN.
       AUTHOR.        Otto B. Boolean.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS        PIC X(170).
      *
       WORKING-STORAGE SECTION.
       01  FLAGS.
           05  LASTREC            PIC X VALUE SPACE.
               88  END-OF-FILE          VALUE 'Y'.
           05  TABLE-VAR          PIC S9(4) COMP.
           05  TABLE-MAX          PIC S9(4) COMP VALUE 45.
      *
       01  ACCT-TABLE.
           05  ACCT-TABLE-ITEM OCCURS 45 TIMES ASCENDING KEY IS ACCT-NO
               INDEXED BY TABLE-IDX.
               10  ACCT-NO            PIC X(8).
               10  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
               10  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
               10  LAST-NAME          PIC X(20).
               10  FIRST-NAME         PIC X(15).
               10  CLIENT-ADDR.
                   15  STREET-ADDR    PIC X(25).
                   15  CITY-COUNTY    PIC X(20).
                   15  USA-STATE      PIC X(15).
               10  RESERVED           PIC X(7).
               10  COMMENTS           PIC X(50).
      *
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT ACCT-REC.
      *
       LOAD-TABLES.
           PERFORM READ-RECORD.
           PERFORM VARYING TABLE-VAR FROM 1 BY 1
             UNTIL TABLE-VAR = TABLE-MAX OR END-OF-FILE
               MOVE ACCT-FIELDS TO ACCT-TABLE-ITEM (TABLE-VAR)
               PERFORM READ-RECORD
           END-PERFORM.
      *
       SEARCH-RECORD.
           SET TABLE-IDX TO 1.
           SEARCH ALL ACCT-TABLE-ITEM
               AT END DISPLAY "Not Found"
               WHEN ACCT-NO (TABLE-IDX) = 18011809
                   DISPLAY "User with Acct No 18011809 is found!".
      *
       CLOSE-STOP.
           CLOSE ACCT-REC.
           GOBACK.
      *
       READ-RECORD.
           READ ACCT-REC
               AT END SET END-OF-FILE TO TRUE
           END-READ.
      *
