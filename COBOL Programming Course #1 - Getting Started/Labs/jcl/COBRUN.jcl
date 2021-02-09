//COBOL   JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(COBOL),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(COBEXEC),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//STEP2     EXEC PGM=COBEXEC
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//PRTDONE   DD DISP=(NEW,CATLG),DSN=&SYSUID..COBRUN.OUTPUT,
//            UNIT=SYSALLDA,SPACE=(TRK,1)
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
