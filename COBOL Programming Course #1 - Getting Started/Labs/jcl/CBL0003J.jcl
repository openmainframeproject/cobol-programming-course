//CBL0003J JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(HELLO),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(HELLO),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(CBL0003),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(CBL0003),DISP=SHR
//LKED.SYSLIB  DD DSN=&SYSUID..LOAD(HELLO),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=CBL0003
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ACCTREC   DD DSN=&SYSUID..DATA,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
// ELSE
// ENDIF
