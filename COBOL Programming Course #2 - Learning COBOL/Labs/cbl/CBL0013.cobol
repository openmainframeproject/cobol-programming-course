       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBL0013.
       AUTHOR.        Athar Ramzan.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMERATOR     PIC 9(04) VALUE 1000.
       01 DENOMINATOR   PIC 9(04) VALUE 0.
       01 RESULT        PIC 9(04).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Starting Division".
           DIVIDE NUMERATOR BY DENOMINATOR GIVING RESULT.
           DISPLAY "Result is: " RESULT.
           STOP RUN.
