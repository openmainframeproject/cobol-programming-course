       IDENTIFICATION DIVISION.                                         00022700
      *-----------------------                                          00022800
       PROGRAM-ID.    CBLDB22                                           00022900
                                                                        00026000
       ENVIRONMENT DIVISION.                                            00027000
      *--------------------                                             00028000
       CONFIGURATION SECTION.                                           00029000
       INPUT-OUTPUT SECTION.                                            00030000
       FILE-CONTROL.                                                    00040000
           SELECT REPOUT ASSIGN TO UT-S-REPORT.                         00060000
           SELECT RECIN  ASSIGN TO DA-S-RECIN.                          00062000
                                                                        00070000
       DATA DIVISION.                                                   00080000
      *-------------                                                    00090000
       FILE SECTION.                                                    00100000
      *-------------                                                    00101000
       FD  REPOUT                                                       00110000
           RECORD CONTAINS 120 CHARACTERS                               00120000
           LABEL RECORDS ARE OMITTED                                    00130000
           RECORDING MODE F                                             00131000
           DATA RECORD IS REPREC.                                       00140000
      *                                                                 00140100
       01  REPREC.                                                      00141000
           05  ACCT-NO-O      PIC X(8).                                 00141100
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.                        00141200
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.                        00141300
           05  ACCT-LASTN-O   PIC X(20).                                00141400
           05  ACCT-FIRSTN-O  PIC X(15).                                00141500
           05  ACCT-COMMENT-O PIC X(50).                                00141600
      *-------------                                                    00141700
       FD  RECIN                                                        00142000
           RECORD CONTAINS 80 CHARACTERS                                00143000
           BLOCK CONTAINS 0 RECORDS                                     00144000
           RECORDING MODE F                                             00144100
           LABEL RECORDS ARE OMITTED.                                   00145000
      *                                                                 00145100
       01  INREC                      PIC X(80).                        00146000
                                                                        00150000
       WORKING-STORAGE SECTION.                                         00240000
      *****************************************************             00241000
      * STRUCTURE FOR INPUT                               *             00242000
      *****************************************************             00243000
       01  IOAREA.                                                      00244000
               02  LNAME              PIC X(25).                        00245000
               02  FILLER             PIC X(55).                        00246000
       77  INPUT-SWITCH        PIC X          VALUE  'Y'.               00247000
               88  NOMORE-INPUT               VALUE  'N'.               00248000
      *****************************************************             00250000
      * SQL INCLUDE FOR SQLCA                             *             00260000
      *****************************************************             00270000
                EXEC SQL INCLUDE SQLCA  END-EXEC.                       00280000
      *****************************************************             00290000
      * SQL DECLARATION FOR VIEW ACCOUNTS                 *             00300000
      *****************************************************             00310000
                EXEC SQL DECLARE Z#####T TABLE                          00320000
                        (ACCTNO     CHAR(8)  NOT NULL,                  00330000
                         LIMIT      DECIMAL(9,2)     ,                  00340000
                         BALANCE    DECIMAL(9,2)     ,                  00350000
                         SURNAME    CHAR(20) NOT NULL,                  00360000
                         FIRSTN     CHAR(15) NOT NULL,                  00370000
                         ADDRESS1   CHAR(25) NOT NULL,                  00380000
                         ADDRESS2   CHAR(20) NOT NULL,                  00390000
                         ADDRESS3   CHAR(15) NOT NULL,                  00400000
                         RESERVED   CHAR(7)  NOT NULL,                  00410000
                         COMMENTS   CHAR(50) NOT NULL)                  00420000
                         END-EXEC.                                      00430000
      *****************************************************             00440000
      * SQL CURSORS                                       *             00450000
      *****************************************************             00460000
                EXEC SQL DECLARE CUR1  CURSOR FOR                       00470000
                         SELECT * FROM Z#####T                          00480000
                     END-EXEC.                                          00490000
      *                                                                 00490100
                EXEC SQL DECLARE CUR2  CURSOR FOR                       00491000
                         SELECT *                                       00492000
                         FROM   Z#####T                                 00493000
                         WHERE  SURNAME = :LNAME                        00494000
                      END-EXEC.                                         00495000
      *****************************************************             00500000
      * STRUCTURE FOR CUSTOMER RECORD                     *             00510000
      *****************************************************             00520000
       01 CUSTOMER-RECORD.                                              00530000
          02 ACCT-NO            PIC X(8).                               00540000
          02 ACCT-LIMIT         PIC S9(7)V99 COMP-3.                    00550000
          02 ACCT-BALANCE       PIC S9(7)V99 COMP-3.                    00560000
          02 ACCT-LASTN         PIC X(20).                              00570000
          02 ACCT-FIRSTN        PIC X(15).                              00580000
          02 ACCT-ADDR1         PIC X(25).                              00590000
          02 ACCT-ADDR2         PIC X(20).                              00600000
          02 ACCT-ADDR3         PIC X(15).                              00610000
          02 ACCT-RSRVD         PIC X(7).                               00620000
          02 ACCT-COMMENT       PIC X(50).                              00630000
                                                                        00640000
       PROCEDURE DIVISION.                                              00650000
      *------------------                                               00660000
       PROG-START.                                                      00670000
                OPEN INPUT  RECIN.                                      00671000
                OPEN OUTPUT REPOUT.                                     00672000
                READ RECIN  RECORD INTO IOAREA                          00680000
                   AT END MOVE 'N' TO INPUT-SWITCH.                     00690000
                PERFORM PROCESS-INPUT                                   00700000
                   UNTIL NOMORE-INPUT.                                  00710000
      *                                                                 00711000
       PROG-END.                                                        00720000
                CLOSE RECIN                                             00730000
                      REPOUT.                                           00740000
                GOBACK.                                                 00750002
      *                                                                 00751000
       PROCESS-INPUT.                                                   00760000
                IF LNAME = '*'                                          00770000
                   PERFORM GET-ALL                                      00780000
                ELSE                                                    00790000
                   PERFORM GET-SPECIFIC.                                00800000
                READ RECIN  RECORD INTO IOAREA                          00810000
                   AT END MOVE 'N' TO INPUT-SWITCH.                     00820000
      *                                                                 00821000
       GET-ALL.                                                         00830000
                EXEC SQL OPEN CUR1  END-EXEC.                           00840000
                EXEC SQL FETCH CUR1  INTO :CUSTOMER-RECORD END-EXEC.    00850000
                   PERFORM PRINT-ALL                                    00860000
                     UNTIL SQLCODE IS NOT EQUAL TO ZERO.                00870000
                EXEC SQL CLOSE CUR1  END-EXEC.                          00880000
      *                                                                 00880100
       PRINT-ALL.                                                       00881000
                PERFORM PRINT-A-LINE.                                   00882000
                EXEC SQL FETCH CUR1  INTO :CUSTOMER-RECORD END-EXEC.    00883000
      *                                                                 00883100
       GET-SPECIFIC.                                                    00884000
                EXEC SQL OPEN  CUR2  END-EXEC.                          00885000
                EXEC SQL FETCH CUR2  INTO :CUSTOMER-RECORD END-EXEC.    00886000
                   PERFORM PRINT-SPECIFIC                               00887000
                     UNTIL SQLCODE IS NOT EQUAL TO ZERO.                00888000
                EXEC SQL CLOSE CUR2  END-EXEC.                          00889000
      *                                                                 00889100
       PRINT-SPECIFIC.                                                  00889200
                PERFORM PRINT-A-LINE.                                   00889300
                EXEC SQL FETCH CUR2  INTO :CUSTOMER-RECORD END-EXEC.    00889400
      *                                                                 00889500
       PRINT-A-LINE.                                                    00890000
                MOVE  ACCT-NO      TO  ACCT-NO-O.                       00900000
                MOVE  ACCT-LIMIT   TO  ACCT-LIMIT-O.                    00910000
                MOVE  ACCT-BALANCE TO  ACCT-BALANCE-O.                  00920000
                MOVE  ACCT-LASTN   TO  ACCT-LASTN-O.                    00930000
                MOVE  ACCT-FIRSTN  TO  ACCT-FIRSTN-O.                   00940000
                MOVE  ACCT-COMMENT TO  ACCT-COMMENT-O.                  00950000
                WRITE REPREC AFTER ADVANCING 2 LINES.                   00960000
