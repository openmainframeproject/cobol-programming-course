      *-----------------------
      * Copyright Contributors to the COBOL Programming Course
      * SPDX-License-Identifier: CC-BY-4.0
      *----------------------- 
       IDENTIFICATION DIVISION.                                         
      *-----------------------                                          
       PROGRAM-ID.    CBLDB21                                           
       AUTHOR.        Otto B. Relational                                
                                                                        
       ENVIRONMENT DIVISION.                                            
      *--------------------                                             
       CONFIGURATION SECTION.                                           
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT REPOUT                                                
                  ASSIGN TO UT-S-REPORT.                                
                                                                        
       DATA DIVISION.                                                   
      *-------------                                                    
       FILE SECTION.                                                    
       FD  REPOUT                                                       
               RECORD CONTAINS 120 CHARACTERS                           
               LABEL RECORDS ARE OMITTED                                
               DATA RECORD IS REPREC.                                   
                                                                        
       01  REPREC.                                                      
           05  ACCT-NO-O      PIC X(8).                                 
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.                        
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.                        
           05  ACCT-LASTN-O   PIC X(20).                                
           05  ACCT-FIRSTN-O  PIC X(15).                                
           05  ACCT-COMMENT-O PIC X(50).                                
                                                                        
       WORKING-STORAGE SECTION.                                         
      *****************************************************             
      * SQL INCLUDE FOR SQLCA                             *             
      *****************************************************             
                EXEC SQL INCLUDE SQLCA  END-EXEC.                       
      *****************************************************
      * DECLARATIONS FOR SQL ERROR HANDLING               *
      *****************************************************
       01 ERROR-MESSAGE.
           02 ERROR-LEN      PIC S9(4)  COMP VALUE +1320.
           02 ERROR-TEXT     PIC X(132) OCCURS 10 TIMES
                                        INDEXED BY ERROR-INDEX.
       77 ERROR-TEXT-LEN     PIC S9(9)  COMP VALUE +132.
       77 ERROR-TEXT-HBOUND  PIC S9(9)  COMP VALUE +10.
      * USER DEFINED ERROR MESSAGE
       01 UD-ERROR-MESSAGE   PIC X(80)  VALUE SPACES.
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
                                                                        
      *****************************************************             
      * MAIN PROGRAM ROUTINE                              *             
      *****************************************************             
       PROG-START.                                                      
                OPEN OUTPUT REPOUT.                                     
                PERFORM LIST-ALL.                                       
       PROG-END.                                                        
                CLOSE REPOUT.                                           
                GOBACK.                                                 
      *****************************************************             
      * LIST ALL CLIENTS                                  *             
      *****************************************************             
       LIST-ALL.                                                        
                EXEC SQL OPEN CUR1 END-EXEC.                            
                IF SQLCODE NOT = 0 THEN
                   MOVE 'OPEN CUR1' TO UD-ERROR-MESSAGE
                   PERFORM SQL-ERROR-HANDLING
                END-IF
                EXEC SQL FETCH CUR1 INTO :CUSTOMER-RECORD END-EXEC.     
                PERFORM PRINT-AND-GET1                                  
                     UNTIL SQLCODE IS NOT EQUAL TO ZERO.                
                IF SQLCODE NOT = 100 THEN
                   MOVE 'FETCH CUR1' TO UD-ERROR-MESSAGE
                   PERFORM SQL-ERROR-HANDLING
                END-IF
                EXEC SQL CLOSE CUR1   END-EXEC.                         
                IF SQLCODE NOT = 0 THEN
                   MOVE 'CLOSE CUR1' TO UD-ERROR-MESSAGE
                   PERFORM SQL-ERROR-HANDLING
                END-IF
                .
       PRINT-AND-GET1.                                                  
                PERFORM PRINT-A-LINE.                                   
                EXEC SQL FETCH CUR1 INTO :CUSTOMER-RECORD END-EXEC.     
       PRINT-A-LINE.                                                    
                MOVE  ACCT-NO      TO  ACCT-NO-O.                       
                MOVE  ACCT-LIMIT   TO  ACCT-LIMIT-O.                    
                MOVE  ACCT-BALANCE TO  ACCT-BALANCE-O.                  
                MOVE  ACCT-LASTN   TO  ACCT-LASTN-O.                    
                MOVE  ACCT-FIRSTN  TO  ACCT-FIRSTN-O.                   
                MOVE  ACCT-COMMENT TO  ACCT-COMMENT-O.                  
                WRITE REPREC AFTER ADVANCING 2 LINES.                   
      
       SQL-ERROR-HANDLING.
           DISPLAY 'ERROR AT ' FUNCTION TRIM(UD-ERROR-MESSAGE, TRAILING)
           CALL 'DSNTIAR' USING SQLCA ERROR-MESSAGE ERROR-TEXT-LEN.
           PERFORM VARYING ERROR-INDEX FROM 1 BY 1
                     UNTIL ERROR-INDEX > ERROR-TEXT-HBOUND
                        OR ERROR-TEXT(ERROR-INDEX) = SPACES
              DISPLAY FUNCTION TRIM(ERROR-TEXT(ERROR-INDEX), TRAILING)
           END-PERFORM
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE 1000 TO RETURN-CODE
              STOP RUN
           END-IF
           .
