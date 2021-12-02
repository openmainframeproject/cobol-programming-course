//CREATE1   JOB 1                                                       JOB03105
//SQLEXEC   EXEC DB2JCL                                                 00000202
//SQL.SYSIN DD *,SYMBOLS=CNVTSYS                                        00000302
--** SQL FOLLOWS ****************************************               00000402
--DROP TABLESPACE DB2.&SYSUID.S;
--COMMIT;                                                               00050000
  CREATE TABLESPACE &SYSUID.S  IN DB2                                   00000500
    USING STOGROUP DB2DISK PRIQTY 20 SECQTY 20 ERASE NO                 00000600
    LOCKSIZE PAGE LOCKMAX SYSTEM                                        00000700
    BUFFERPOOL BP0 CLOSE NO COMPRESS YES;                               00000800
  COMMIT;                                                               00000900
  CREATE TABLE &SYSUID.T                                                00001100
                (ACCTNO    CHAR(8)        NOT NULL,                     00002000
                 LIMIT     DECIMAL(9,2)           ,                     00003000
                 BALANCE   DECIMAL(9,2)           ,                     00004000
                 SURNAME   CHAR(20)       NOT NULL,                     00005000
                 FIRSTN    CHAR(15)       NOT NULL,                     00006000
                 ADDRESS1  CHAR(25)               ,                     00007000
                 ADDRESS2  CHAR(20)               ,                     00008000
                 ADDRESS3  CHAR(15)               ,                     00009000
                 RESERVED  CHAR(7)                ,                     00010000
                 COMMENTS  CHAR(50)               ,                     00020000
                 PRIMARY KEY(ACCTNO))                                   00030000
         IN DB2.&SYSUID.S;                                              00040000
  COMMIT;                                                               00050000
--*******************************************************               00060000
  CREATE UNIQUE INDEX &SYSUID.I                                         00070000
                   ON &SYSUID.T (ACCTNO  ASC)                           00080000
                   USING STOGROUP DB2DISK  PRIQTY 12 ERASE NO           00080100
                   BUFFERPOOL BP0 CLOSE NO;                             00080200
--*******************************************************               00080300
  COMMIT;                                                               00080400
