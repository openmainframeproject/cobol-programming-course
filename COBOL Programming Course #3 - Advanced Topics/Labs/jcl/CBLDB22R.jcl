//CBLDB22R JOB 1,NOTIFY=&SYSUID,REGION=0M
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course 
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//*        RUN                                   
//***************************************************/
//RUN     EXEC PGM=IKJEFT01
//STEPLIB  DD DSN=DSNC10.SDSNLOAD,DISP=SHR
//RECIN    DD *
LINCOLN
//SYSTSIN  DD *,SYMBOLS=CNVTSYS
 DSN SYSTEM(DBCG)
 RUN PROGRAM(CBLDB22) PLAN(&SYSUID) LIB('&SYSUID..LOAD')
 END
//SYSIN    DD DUMMY
//SYSUDUMP DD DUMMY
//CEEDUMP  DD DUMMY
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
/*
