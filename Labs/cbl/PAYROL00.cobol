      *         COBOL reference format (Figure 1., page 32)
      *Columns:
      *  1         2         3         4         5         6         7
      *89012345678901234567890123456789012345678901234567890123456789012
      *<A-><--------------------------B-------------------------------->
      *Area                          Area
      *<---Sequence Number Area                 Identification Area---->
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID. PAYROL00.
      *-------------
       DATA DIVISION.
      *-------------
       WORKING-STORAGE SECTION.
      ****** Variables for the report
      * level number
      * |  variable name
      * |  |          picture clause
      * |  |          |
      * V  V          V
       77  WHO        PIC X(15).
       77  WHERE      PIC X(20).
       77  WHY        PIC X(30).
       77  RATE       PIC 9(3).
       77  HOURS      PIC 9(3).
       77  GROSS-PAY  PIC 9(5).

      * PIC X(15) -- fiftheen alphanumeric characters
      * PIC 9(3)  -- three-digit value
      *------------------
       PROCEDURE DIVISION.
      *------------------
      ****** COBOL MOVE statements - Literal Text to Variables
           MOVE  "Captain COBOL" TO WHO.
           MOVE "San Jose, California" TO WHERE.
           MOVE "Learn to be a COBOL expert" TO WHY.
           MOVE 19 TO HOURS.
           MOVE 23 TO RATE.
      * The string "Captain COBOL" only contains 13 characters,
      * the remaining positions of variable WHO are filled with spaces
      * The value 19 only needs 2 digits,
      * the leftmost position of variable HOURS is filled with zero
      ****** Calculation using COMPUTE reserved word verb
           COMPUTE GROSS-PAY = HOURS * RATE.
      * The result of the multiplication only needs 3 digits,
      * the remaining leftmost positions are filled with zeroes
      ****** DISPLAY statements
           DISPLAY "Name: " WHO.
           DISPLAY "Location: " WHERE
           DISPLAY "Reason: " WHY
           DISPLAY "Hours Worked: " HOURS.
           DISPLAY "Hourly Rate: " RATE.
           DISPLAY "Gross Pay: " GROSS-PAY.
           DISPLAY WHY " from " WHO.
           GOBACK.
