//DB2CBL   PROC MBR='DB2CBL' 
//********************************************************************
//* Copyright Contributors to the COBOL Programming Course           * 
//* SPDX-License-Identifier: CC-BY-4.0                               * 
//******************************************************************** 
//*  COMPILE - COBOL PLUS EXPANDED EXEC SQL CODE                     * 
//******************************************************************** 
//         IF RC = 0 THEN                                              
//COBOL    EXEC PGM=IGYCRCTL,REGION=0M,PARM='SQL'                      
//STEPLIB  DD  DSN=IGY630.SIGYCOMP,DISP=SHR                            
//         DD  DSN=DSNC10.DBCG.SDSNEXIT,DISP=SHR                       
//         DD  DSN=DSNC10.SDSNLOAD,DISP=SHR                            
//         DD  DSN=CEE.SCEERUN,DISP=SHR                                
//         DD  DSN=CEE.SCEERUN2,DISP=SHR                               
//SYSIN    DD  DISP=SHR,DSN=&SYSUID..CBL(&MBR)                         
//DBRMLIB  DD  DISP=SHR,DSN=&SYSUID..DBRMLIB(&MBR)                     
//SYSPRINT DD  SYSOUT=*                                                
//SYSLIN   DD  DSN=&&LOADSET,UNIT=SYSALLDA,                            
//             DISP=(MOD,PASS),SPACE=(CYL,(1,1))                       
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT4   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT5   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT6   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT7   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT8   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT9   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT10  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT11  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT12  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT13  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT14  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSUT15  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSMDECK DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//         ENDIF                                                       
//******************************************************************** 
//*  CREATE EXECUTABLE MODULE                                        * 
//******************************************************************** 
//         IF RC <= 4 THEN                                             
//LKED     EXEC PGM=IEWBLINK,COND=(8,LT,COBOL),REGION=0M               
//SYSLIB   DD  DSN=CEE.SCEELKED,DISP=SHR                               
//         DD  DSN=DSNC10.SDSNLOAD,DISP=SHR                            
//SYSPRINT DD  SYSOUT=*                                                
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1))                         
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)                         
//SYSLMOD  DD  DSN=&SYSUID..LOAD(&MBR),DISP=SHR                        
//         ENDIF                                                       
//******************************************************************** 
//*  BIND DB2 PLAN                                                   * 
//******************************************************************** 
//         IF RC <= 4 THEN                                             
//BIND     EXEC PGM=IKJEFT01                                            
//STEPLIB  DD DSN=DSNC10.SDSNLOAD,DISP=SHR                              
//DBRMLIB  DD DSN=&SYSUID..DBRMLIB,DISP=SHR                             
//SYSUDUMP DD DUMMY                                                     
//SYSTSPRT DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSTSIN  DD DUMMY                                                     
//         ENDIF                                                        
