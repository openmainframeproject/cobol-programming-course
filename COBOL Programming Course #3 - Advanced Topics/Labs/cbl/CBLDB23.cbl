      *-----------------------
      * Copyright Contributors to the COBOL Programming Course
      * SPDX-License-Identifier: CC-BY-4.0
      *----------------------- 
       IDENTIFICATION DIVISION.                                         
      *-----------------------                                          
       PROGRAM-ID.    CBLDB23                                           
       AUTHOR.        Otto B. Relational                                
                                                                        
       ENVIRONMENT DIVISION.                                            
      *--------------------                                             
       CONFIGURATION SECTION.                                           
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT REPOUT                                                
                  ASSIGN TO UT-S-REPORT.                                
           SELECT CARDIN                                                
                  ASSIGN TO DA-S-CARDIN.                                
                                                                        
       DATA DIVISION.                                                   
      *-------------                                                    
       FILE SECTION.                                                    
       FD  REPOUT                                                       
               RECORD CONTAINS 120 CHARACTERS                           
               LABEL RECORDS ARE OMITTED                                
               RECORDING MODE F                                         
               DATA RECORD IS REPREC.                                   
       01  REPREC.                                                      
           05  ACCT-NO-O      PIC X(8).                                 
           05  ACCT-LASTN-O   PIC X(20).                                
           05  ACCT-FIRSTN-O  PIC X(15).                                
           05  ACCT-ADDR3-O   PIC X(15).                                
       FD      CARDIN                                                   
               RECORD CONTAINS 80 CHARACTERS                            
               BLOCK CONTAINS 0 RECORDS                                 
               RECORDING MODE F                                         
               LABEL RECORDS ARE OMITTED.                               
       01  CARDREC                    PIC X(80).                        
                                                                        
       WORKING-STORAGE SECTION.                                         
      *****************************************************             
      * STRUCTURE FOR INPUT                               *             
      *****************************************************             
       01  IOAREA.                                                      
               02  STATE              PIC X(25).                        
               02  FILLER             PIC X(55).                        
       77  INPUT-SWITCH        PIC X          VALUE  'Y'.               
               88  NOMORE-INPUT               VALUE  'N'.               
      *****************************************************             
      * SQL INCLUDE FOR SQLCA                             *             
      *****************************************************             
                EXEC SQL INCLUDE SQLCA  END-EXEC.                       
      *****************************************************             
      * SQL DECLARATION FOR VIEW ACCOUNTS                 *             
      *****************************************************             
                EXEC SQL DECLARE Z#####T TABLE                          
                        (ACCTNO     CHAR(8)  NOT NULL,                  
                         LIMIT      DECIMAL(9,2)     ,                  
                         BALANCE    DECIMAL(9,2)     ,                  
                         SURNAME    CHAR(20) NOT NULL,                  
                         FIRSTN     CHAR(15) NOT NULL,                  
                         ADDRESS1   CHAR(25) NOT NULL,                  
                         ADDRESS2   CHAR(20) NOT NULL,                  
                         ADDRESS3   CHAR(15) NOT NULL,                  
                         RESERVED   CHAR(7)  NOT NULL,                  
                         COMMENTS   CHAR(50) NOT NULL)                  
                         END-EXEC.                                      
      *****************************************************             
      * SQL CURSORS                                       *             
      *****************************************************             
                EXEC SQL DECLARE CUR1  CURSOR FOR                       
                         SELECT * FROM Z#####T                          
                     END-EXEC.                                          
                EXEC SQL DECLARE CUR2  CURSOR FOR                       
                         SELECT *                                       
                         FROM   Z#####T                                 
                         WHERE  ADDRESS3 = :STATE                       
                      END-EXEC.                                         
      *****************************************************             
      * STRUCTURE FOR CUSTOMER RECORD                     *             
      *****************************************************             
       01 CUSTOMER-RECORD.                                              
          02 ACCT-NO            PIC X(8).                               
          02 ACCT-LIMIT         PIC S9(7)V99 COMP-3.                    
          02 ACCT-BALANCE       PIC S9(7)V99 COMP-3.                    
          02 ACCT-LASTN         PIC X(20).                              
          02 ACCT-FIRSTN        PIC X(15).                              
          02 ACCT-ADDR1         PIC X(25).                              
          02 ACCT-ADDR2         PIC X(20).                              
          02 ACCT-ADDR3         PIC X(15).                              
          02 ACCT-RSRVD         PIC X(7).                               
          02 ACCT-COMMENT       PIC X(50).                              
                                                                        
       PROCEDURE DIVISION.                                              
      *------------------                                               
       PROG-START.                                                      
                OPEN INPUT  CARDIN.                                     
                OPEN OUTPUT REPOUT.                                     
                READ CARDIN RECORD INTO IOAREA                          
                   AT END MOVE 'N' TO INPUT-SWITCH.                     
                PERFORM PROCESS-INPUT                                   
                   UNTIL NOMORE-INPUT.                                  
       PROG-END.                                                        
                CLOSE CARDIN                                            
                      REPOUT.                                           
                GOBACK.                                                 
       PROCESS-INPUT.                                                   
                IF STATE = '*'                                          
                   PERFORM GET-ALL                                      
                ELSE                                                    
                   PERFORM GET-SPECIFIC.                                
                READ CARDIN RECORD INTO IOAREA                          
                   AT END MOVE 'N' TO INPUT-SWITCH.                     
       GET-ALL.                                                         
                EXEC SQL OPEN CUR1  END-EXEC.                           
                EXEC SQL FETCH CUR1  INTO :CUSTOMER-RECORD END-EXEC.    
                   PERFORM PRINT-ALL                                    
                     UNTIL SQLCODE IS NOT EQUAL TO ZERO.                
                EXEC SQL CLOSE CUR1  END-EXEC.                          
       PRINT-ALL.                                                       
                PERFORM PRINT-A-LINE.                                   
                EXEC SQL FETCH CUR1  INTO :CUSTOMER-RECORD END-EXEC.    
       GET-SPECIFIC.                                                    
                EXEC SQL OPEN  CUR2  END-EXEC.                          
                EXEC SQL FETCH CUR2  INTO :CUSTOMER-RECORD END-EXEC.    
                   PERFORM PRINT-SPECIFIC                               
                     UNTIL SQLCODE IS NOT EQUAL TO ZERO.                
                EXEC SQL CLOSE CUR2  END-EXEC.                          
       PRINT-SPECIFIC.                                                  
                PERFORM PRINT-A-LINE.                                   
                EXEC SQL FETCH CUR2  INTO :CUSTOMER-RECORD END-EXEC.    
       PRINT-A-LINE.                                                    
                MOVE  ACCT-NO      TO  ACCT-NO-O.                       
                MOVE  ACCT-LASTN   TO  ACCT-LASTN-O.                    
                MOVE  ACCT-FIRSTN  TO  ACCT-FIRSTN-O.                   
                MOVE  ACCT-ADDR3   TO  ACCT-ADDR3-O.                    
                WRITE REPREC AFTER ADVANCING 2 LINES.                   
