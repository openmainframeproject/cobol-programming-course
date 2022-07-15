//ADDAMT   JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(ADDAMT),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(ADDAMT),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//STEP2 EXEC PGM=ADDAMT
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//SYSIN     DD *
CUSTOMER
00025
00050
00015
NO
//***************************************************/
// ELSE
// ENDIF
