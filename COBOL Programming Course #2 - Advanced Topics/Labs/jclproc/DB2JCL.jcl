//DB2JCL   PROC                                                         00010001
//SQL      EXEC PGM=IKJEFT01                                            00030001
//STEPLIB  DD  DSN=DSNC10.SDSNLOAD,DISP=SHR                             00031002
//SYSTSPRT DD  SYSOUT=*                                                 00040000
//SYSTSIN  DD  *                                                        00050000
  DSN SYSTEM(DBCG)                                                      00060004
  RUN  PROGRAM(DSNTEP2) PLAN(DSNTEP12) +                                00070000
       LIB('DSNC10.DBCG.RUNLIB.LOAD') PARMS('/ALIGN(MID)')              00090006
  END                                                                   00100000
//SYSPRINT DD  SYSOUT=*                                                 00110000
//SYSUDUMP DD  DUMMY                                                    00120000
//         PEND                                                         00150001
