       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLC2.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNTER               PIC 9(09) VALUE 0.
       01  WRK1                  PIC 9(09) VALUE 1.
       01  WRK2                  PIC 9(09) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Starting controlled loop...".
           PERFORM VARYING WRK1 FROM 1 BY 1 UNTIL WRK1 > 1000000
              ADD WRK1 TO WRK2 GIVING COUNTER
           END-PERFORM
           DISPLAY "Loop finished successfully".
           STOP RUN.
