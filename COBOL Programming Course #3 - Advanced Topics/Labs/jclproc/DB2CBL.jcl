//DB2CBL   PROC MBR='DB2CBL'                                            00010000
//********************************************************************  00200000
//*  COMPILE - COBOL PLUS EXPANDED EXEC SQL CODE                     *  00210000
//********************************************************************  00220000
//         IF RC = 0 THEN                                               00230000
//COBOL    EXEC PGM=IGYCRCTL,REGION=0M,PARM='SQL'                       00240002
//STEPLIB  DD  DSN=IGY630.SIGYCOMP,DISP=SHR                             00250001
//         DD  DSN=DSNC10.DBCG.SDSNEXIT,DISP=SHR                        00251000
//         DD  DSN=DSNC10.SDSNLOAD,DISP=SHR                             00252000
//         DD  DSN=CEE.SCEERUN,DISP=SHR                                 00260000
//         DD  DSN=CEE.SCEERUN2,DISP=SHR                                00270000
//SYSIN    DD  DISP=SHR,DSN=&SYSUID..CBL(&MBR)                          00271003
//DBRMLIB  DD  DISP=SHR,DSN=&SYSUID..DBRMLIB(&MBR)                      00272000
//SYSPRINT DD  SYSOUT=*                                                 00290000
//SYSLIN   DD  DSN=&&LOADSET,UNIT=SYSALLDA,                             00300000
//             DISP=(MOD,PASS),SPACE=(CYL,(1,1))                        00310000
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00320000
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00330000
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00340000
//SYSUT4   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00350000
//SYSUT5   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00360000
//SYSUT6   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00370000
//SYSUT7   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00380000
//SYSUT8   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00390000
//SYSUT9   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00400000
//SYSUT10  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00410000
//SYSUT11  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00420000
//SYSUT12  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00430000
//SYSUT13  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00440000
//SYSUT14  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00450000
//SYSUT15  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00460000
//SYSMDECK DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00470000
//         ENDIF                                                        00480000
//********************************************************************  00490000
//*  CREATE EXECUTABLE MODULE                                        *  00500000
//********************************************************************  00510000
//         IF RC <= 4 THEN                                              00520004
//LKED     EXEC PGM=IEWBLINK,COND=(8,LT,COBOL),REGION=0M                00530000
//SYSLIB   DD  DSN=CEE.SCEELKED,DISP=SHR                                00540000
//         DD  DSN=DSNC10.SDSNLOAD,DISP=SHR                             00550000
//SYSPRINT DD  SYSOUT=*                                                 00560000
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                          00570000
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)                          00580000
//SYSLMOD  DD  DSN=&SYSUID..LOAD(&MBR),DISP=SHR                         00590000
//         ENDIF                                                        00600000
//********************************************************************  00610000
//*  BIND DB2 PLAN                                                   *  00620000
//********************************************************************  00630000
//         IF RC <= 4 THEN                                              00640000
//BIND     EXEC PGM=IKJEFT01                                            00650000
//STEPLIB  DD DSN=DSNC10.SDSNLOAD,DISP=SHR                              00660000
//DBRMLIB  DD DSN=&SYSUID..DBRMLIB,DISP=SHR                             00670000
//SYSUDUMP DD DUMMY                                                     00680000
//SYSTSPRT DD SYSOUT=*                                                  00690000
//SYSPRINT DD SYSOUT=*                                                  00700000
//SYSTSIN  DD DUMMY                                                     00710007
//         ENDIF                                                        00760000
