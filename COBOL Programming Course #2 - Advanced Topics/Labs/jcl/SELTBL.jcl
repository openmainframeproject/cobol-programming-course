//SELTBL  JOB 1                                                         
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course 
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//SQLEXEC EXEC DB2JCL                                                   
//SYSIN   DD *,SYMBOLS=CNVTSYS                                          
--******* SQL FOLLOWS                                                   
  SELECT * FROM &SYSUID.T;                                              
