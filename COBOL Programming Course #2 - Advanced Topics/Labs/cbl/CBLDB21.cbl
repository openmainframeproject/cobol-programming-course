       IDENTIFICATION DIVISION.                                         00010000
      *-----------------------                                          00020000
       PROGRAM-ID.    CBLDB21                                           00021000
       AUTHOR.        Otto B. Relational                                00022000
                                                                        00040000
       ENVIRONMENT DIVISION.                                            01290000
      *--------------------                                             01300000
       CONFIGURATION SECTION.                                           01310000
       INPUT-OUTPUT SECTION.                                            01330000
       FILE-CONTROL.                                                    01340000
           SELECT REPOUT                                                01370000
                  ASSIGN TO UT-S-REPORT.                                01380000
                                                                        01390000
       DATA DIVISION.                                                   01400000
      *-------------                                                    01410000
       FILE SECTION.                                                    01420000
       FD  REPOUT                                                       01490000
               RECORD CONTAINS 120 CHARACTERS                           01500000
               LABEL RECORDS ARE OMITTED                                01510000
               DATA RECORD IS REPREC.                                   01520000
                                                                        01521000
       01  REPREC.                                                      01530000
           05  ACCT-NO-O      PIC X(8).                                 01541000
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.                        01542000
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.                        01543000
           05  ACCT-LASTN-O   PIC X(20).                                01544000
           05  ACCT-FIRSTN-O  PIC X(15).                                01545000
           05  ACCT-COMMENT-O PIC X(50).                                01546000
                                                                        01547000
       WORKING-STORAGE SECTION.                                         01550000
      *****************************************************             02531000
      * SQL INCLUDE FOR SQLCA                             *             02532000
      *****************************************************             02533000
                EXEC SQL INCLUDE SQLCA  END-EXEC.                       02534000
      *****************************************************             02535000
      * SQL DECLARATION FOR VIEW ACCOUNTS                 *             02536000
      *****************************************************             02537000
                EXEC SQL DECLARE Z#####T TABLE                          02538000
                        (ACCTNO     CHAR(8)  NOT NULL,                  02539000
                         LIMIT      DECIMAL(9,2)     ,                  02539100
                         BALANCE    DECIMAL(9,2)     ,                  02539200
                         SURNAME    CHAR(20) NOT NULL,                  02539300
                         FIRSTN     CHAR(15) NOT NULL,                  02539400
                         ADDRESS1   CHAR(25) NOT NULL,                  02539500
                         ADDRESS2   CHAR(20) NOT NULL,                  02539600
                         ADDRESS3   CHAR(15) NOT NULL,                  02539700
                         RESERVED   CHAR(7)  NOT NULL,                  02539800
                         COMMENTS   CHAR(50) NOT NULL)                  02539900
                         END-EXEC.                                      02540100
      *****************************************************             02540200
      * SQL CURSORS                                       *             02540300
      *****************************************************             02540400
                EXEC SQL DECLARE CUR1  CURSOR FOR                       02540700
                         SELECT * FROM Z#####T                          02540900
                     END-EXEC.                                          02541000
      *****************************************************             02542000
      * STRUCTURE FOR CUSTOMER RECORD                     *             02543000
      *****************************************************             02544000
       01 CUSTOMER-RECORD.                                              02545000
          02 ACCT-NO            PIC X(8).                               02546000
          02 ACCT-LIMIT         PIC S9(7)V99 COMP-3.                    02547000
          02 ACCT-BALANCE       PIC S9(7)V99 COMP-3.                    02548000
          02 ACCT-LASTN         PIC X(20).                              02549000
          02 ACCT-FIRSTN        PIC X(15).                              02550000
          02 ACCT-ADDR1         PIC X(25).                              02560000
          02 ACCT-ADDR2         PIC X(20).                              02570000
          02 ACCT-ADDR3         PIC X(15).                              02580000
          02 ACCT-RSRVD         PIC X(7).                               02590000
          02 ACCT-COMMENT       PIC X(50).                              02600000
                                                                        03280000
       PROCEDURE DIVISION.                                              03290000
      *------------------                                               03300000
                                                                        03310000
      *****************************************************             03390000
      * MAIN PROGRAM ROUTINE                              *             03400000
      *****************************************************             03410000
       PROG-START.                                                      03420000
                OPEN OUTPUT REPOUT.                                     03450000
                PERFORM LIST-ALL.                                       03520000
       PROG-END.                                                        03540000
                CLOSE REPOUT.                                           03570000
                GOBACK.                                                 03580006
      *****************************************************             04270000
      * LIST ALL CLIENTS                                  *             04280000
      *****************************************************             04290000
       LIST-ALL.                                                        04300000
                EXEC SQL OPEN CUR1 END-EXEC.                            04320000
                EXEC SQL FETCH CUR1 INTO :CUSTOMER-RECORD END-EXEC.     04350000
                PERFORM PRINT-AND-GET1                                  04470000
                     UNTIL SQLCODE IS NOT EQUAL TO ZERO.                04480000
                EXEC SQL CLOSE CUR1   END-EXEC.                         04510000
       PRINT-AND-GET1.                                                  04530000
                PERFORM PRINT-A-LINE.                                   04540000
                EXEC SQL FETCH CUR1 INTO :CUSTOMER-RECORD END-EXEC.     04560000
       PRINT-A-LINE.                                                    05200000
                MOVE  ACCT-NO      TO  ACCT-NO-O.                       05220000
                MOVE  ACCT-LIMIT   TO  ACCT-LIMIT-O.                    05230000
                MOVE  ACCT-BALANCE TO  ACCT-BALANCE-O.                  05240000
                MOVE  ACCT-LASTN   TO  ACCT-LASTN-O.                    05250000
                MOVE  ACCT-FIRSTN  TO  ACCT-FIRSTN-O.                   05260000
                MOVE  ACCT-COMMENT TO  ACCT-COMMENT-O.                  05270000
                WRITE REPREC AFTER ADVANCING 2 LINES.                   05310000
