//CBL0013J JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(CBL0013),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(CBL0013),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=CBL0013
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD SYSOUT=*
//SYSUDUMP  DD SYSOUT=*
//***************************************************/
// ELSE
// ENDIF
