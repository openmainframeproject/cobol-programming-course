       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL0015.
       AUTHOR. Athar Ramzan.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ALWAYS-TRUE           PIC X VALUE 'Y'.
       01  COUNTER               PIC 9(09) VALUE 0.
       01  WRK1                  PIC 9(09) VALUE 1.
       01  WRK2                  PIC 9(09) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Starting tight CPU loop (no I/O in loop) ...".
         *> Loop forever, no DISPLAY inside loop (no I/O), just burn CPU
           PERFORM UNTIL ALWAYS-TRUE = 'N'
              ADD WRK1 TO WRK2 GIVING COUNTER
              ADD 1    TO WRK1
              IF WRK1 > 999999999
                 MOVE 1 TO WRK1
              END-IF
           END-PERFORM
           .
           DISPLAY "This line will never be reached".
           STOP RUN.
