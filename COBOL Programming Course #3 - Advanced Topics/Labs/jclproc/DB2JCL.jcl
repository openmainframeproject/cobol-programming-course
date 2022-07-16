//DB2JCL   PROC                   
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course 
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/               
//SQL      EXEC PGM=IKJEFT01                                            
//STEPLIB  DD  DSN=DSNC10.SDSNLOAD,DISP=SHR                             
//SYSTSPRT DD  SYSOUT=*                                                 
//SYSTSIN  DD  *                                                        
  DSN SYSTEM(DBCG)                                                      
  RUN  PROGRAM(DSNTEP2) PLAN(DSNTEP12) +                                
       LIB('DSNC10.DBCG.RUNLIB.LOAD') PARMS('/ALIGN(MID)')              
  END                                                                   
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUDUMP DD  DUMMY                                                    
//         PEND                                                         
