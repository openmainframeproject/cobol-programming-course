//CBLDB23C JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course 
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COMPILE  EXEC DB2CBL,MBR=CBLDB23,PARM=('SQL,CODEPAGE(1047)')
//BIND.SYSTSIN  DD *,SYMBOLS=CNVTSYS
 DSN SYSTEM(DBCG)
 BIND PLAN(&SYSUID) PKLIST(&SYSUID..CBLDB23) MEMBER(CBLDB23) -
      ACT(REP) ISO(CS) ENCODING(EBCDIC)
/*
