//CREATE1   JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course 
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//SQLEXEC   EXEC DB2JCL                                                 
//SQL.SYSIN DD *,SYMBOLS=CNVTSYS                                        
--** SQL FOLLOWS ****************************************               
--DROP TABLESPACE DB2.&SYSUID.S;
--COMMIT;                                                               
  CREATE TABLESPACE &SYSUID.S  IN DB2                                   
    USING STOGROUP DB2DISK PRIQTY 20 SECQTY 20 ERASE NO                 
    LOCKSIZE PAGE LOCKMAX SYSTEM                                        
    BUFFERPOOL BP0 CLOSE NO COMPRESS YES;                               
  COMMIT;                                                               
  CREATE TABLE &SYSUID.T                                                
                (ACCTNO    CHAR(8)        NOT NULL,                     
                 LIMIT     DECIMAL(9,2)           ,                     
                 BALANCE   DECIMAL(9,2)           ,                     
                 SURNAME   CHAR(20)       NOT NULL,                     
                 FIRSTN    CHAR(15)       NOT NULL,                     
                 ADDRESS1  CHAR(25)               ,                     
                 ADDRESS2  CHAR(20)               ,                     
                 ADDRESS3  CHAR(15)               ,                     
                 RESERVED  CHAR(7)                ,                     
                 COMMENTS  CHAR(50)               ,                     
                 PRIMARY KEY(ACCTNO))                                   
         IN DB2.&SYSUID.S;                                              
  COMMIT;                                                               
--*******************************************************               
  CREATE UNIQUE INDEX &SYSUID.I                                         
                   ON &SYSUID.T (ACCTNO  ASC)                           
                   USING STOGROUP DB2DISK  PRIQTY 12 ERASE NO           
                   BUFFERPOOL BP0 CLOSE NO;                             
--*******************************************************               
  COMMIT;
/*
