//CBL0067J JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(CBL0067),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(CBL0067),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=CBL0067
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD SYSOUT=*
//SYSUDUMP  DD SYSOUT=*
//***************************************************/
// ELSE
// ENDIF
