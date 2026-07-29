CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PGET.                                                 
      *              PROGRAM CONVERTED BY                                       
      *              CCCA FOR OS/390 & MVS & VM 5648-B05                        
      *              CONVERSION DATE 07/24/08 07:27:53.                         
      *AUTHOR.        JIM KLAPWYK.                                              
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.                                                            
      *DATE-COMPILED.                                                           
      *----------------------------------------------------------------*        
      *                                                                         
      *  SYSTEM    : GROUP ELIGIBILITY                                          
      *                                                                         
      *  LANGUAGE  : COBOL II                                                   
      *                                                                         
      *  THIS MODULE WILL TAKE A MESSAGE AND SEARCH ITS INDEX TO                
      *  RETURN THE TYPE LENGTH AND VALUE ASSOCIATED WITH THE                   
      *  REQUESTED PACKET.                                                      
      *                                                                         
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  SEP 1998   J KLAPWYK  CREATED                                     
      *  002  SEP 2002   TRUDY      PERFORMANCE CHANGES                         
      *----------------------------------------------------------------*        
      /                                                                         
       ENVIRONMENT DIVISION.                                                    
                                                                                
       CONFIGURATION SECTION.                                                   
                                                                                
       SOURCE-COMPUTER. IBM-370.                                                
       OBJECT-COMPUTER. IBM-370.                                                
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
      /                                                                         
       WORKING-STORAGE SECTION.                                                 
       01  FILLER                             PIC X(40) VALUE                   
               '**   MLX2PGET WORKING STORAGE BEGINS  **'.                      
       01  FIXED.                                                               
           05  WS-HEADER-START                PIC X(4) VALUE '9991'.            
           05  WS-BODY-START                  PIC X(4) VALUE '9992'.            
           05  WS-TRAILER-START               PIC X(4) VALUE '9993'.            
                                                                                
       01  VARIABLES.                                                           
           05  WS-HOLD-PACKET                 PIC X(4).                         
               88  P-SPECIAL-VALUE          VALUE '9991' THRU   '9999'.         
           05  WS-HOLD-OFFSET                 PIC S9(8) COMP.                   
           05  WS-HOLD-LENGTH-X.                                                
               10  WS-HOLD-LENGTH             PIC 9(9).                         
           05  WS-HOLD-TYPE                   PIC 9(2).                         
               88  GROUP-START              VALUE 20.                           
               88  PREDEF-SEGMENT           VALUE 21 22.                        
           05  WS-FOUND-SW                    PIC X(1).                         
               88  WS-FOUND                 VALUE 'Y'.                          
               88  WS-NOT-FOUND             VALUE 'N'.                          
           05  WS-VALID-REQUEST-SW            PIC X(1).                         
               88  WS-VALID-REQUEST         VALUE 'Y'.                          
               88  WS-INVALID-REQUEST       VALUE 'N'.                          
                                                                                
      *----------------------------------------------------------------*        
      *  SINGLE INDEX ENTRY                                                     
      *----------------------------------------------------------------*        
       01  INDEX-ENTRY.                                                         
           COPY MLX2NDXE.                                                       
                                                                                
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  INPUT PARAMETERS                                                       
      *----------------------------------------------------------------*        
       01  INPUT-PARMS.                                                         
           COPY MLX2INPT.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  RETURNS                                                                
      *----------------------------------------------------------------*        
       01  PARSER-RETURNS.                                                      
           COPY MLX2RTRN.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  COMMON FIELDS                                                          
      *----------------------------------------------------------------*        
       01  COMM-FIELDS.                                                         
           COPY MLX2COMM.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  INDEX DEFINITION                                                       
      *----------------------------------------------------------------*        
       01  INDEX-DEFN.                                                          
           COPY MLX2INDX.                                                       
                                                                                
       01  AREA1                               PIC X(1).                        
       01  AREA3                               PIC X(1).                        
       01  PGET-RETURN                         PIC 9(2).                        
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING INPUT-PARMS                                     
                                PARSER-RETURNS                                  
                                COMM-FIELDS                                     
                                INDEX-DEFN                                      
                                AREA1                                           
                                AREA3                                           
                                PGET-RETURN.                                    
      *----------------------------------------------------------------*        
                                                                                
      *                                                                         
       0000-MAINLINE.                                                           
           PERFORM 1000-INIT THRU                                               
                   1000-INIT-EXIT.                                              
                                                                                
           IF PGET-RETURN = 0                                                   
              PERFORM 2000-SEARCH-INDEX THRU                                    
                      2000-SEARCH-INDEX-EXIT                                    
           END-IF.                                                              
                                                                                
           PERFORM 3000-SETUP-RETURN THRU                                       
                   3000-SETUP-RETURN-EXIT.                                      
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      * INITIALIZATION ROUTINE.                                                 
      ****************************************************************          
       1000-INIT.                                                               
           SET WS-VALID-REQUEST TO TRUE.                                        
           MOVE 0               TO PGET-RETURN.                                 
                                                                                
           EVALUATE TRUE                                                        
              WHEN INPT-SECTION-HEADER                                          
                 MOVE WS-HEADER-START        TO NDXE-SECTION-ID                 
              WHEN INPT-SECTION-BODY                                            
                 MOVE WS-BODY-START          TO NDXE-SECTION-ID                 
              WHEN INPT-SECTION-TRAILER                                         
                 MOVE WS-TRAILER-START       TO NDXE-SECTION-ID                 
              WHEN OTHER                                                        
                 MOVE 50                     TO PGET-RETURN                     
                 GO TO 1000-INIT-EXIT                                           
           END-EVALUATE.                                                        
                                                                                
           MOVE INPT-SECTION-OCCUR           TO NDXE-SECTION-OCCUR.             
           MOVE INPT-GRP-LVL-1-ID            TO NDXE-LVL-1-ID.                  
           MOVE INPT-GRP-LVL-1-OCCUR         TO NDXE-LVL-1-OCCUR.               
           MOVE INPT-GRP-LVL-2-ID            TO NDXE-LVL-2-ID.                  
           MOVE INPT-GRP-LVL-2-OCCUR         TO NDXE-LVL-2-OCCUR.               
           MOVE INPT-GRP-LVL-3-ID            TO NDXE-LVL-3-ID.                  
           MOVE INPT-GRP-LVL-3-OCCUR         TO NDXE-LVL-3-OCCUR.               
           MOVE SPACE                        TO NDXE-LVL-3-X.                   
           MOVE INPT-PACKET-ID               TO NDXE-PACKET-ID.                 
           SET NDXE-AREA1                    TO TRUE.                           
                                                                                
           IF NDXE-PACKET-ID = NDXE-LVL-1-ID                                    
           OR NDXE-PACKET-ID = NDXE-LVL-2-ID                                    
           OR NDXE-PACKET-ID = NDXE-LVL-3-ID                                    
              MOVE 54                 TO PGET-RETURN                            
              SET WS-INVALID-REQUEST  TO TRUE                                   
           END-IF.                                                              
                                                                                
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SEARCH INDEX                                                            
      ****************************************************************          
       2000-SEARCH-INDEX.                                                       
           MOVE INPT-PACKET-ID        TO WS-HOLD-PACKET.                        
           IF P-SPECIAL-VALUE                                                   
              MOVE 54                 TO PGET-RETURN                            
              SET WS-INVALID-REQUEST  TO TRUE                                   
           ELSE                                                                 
              SEARCH ALL INDX-PACKETS                                           
                 AT END                                                         
                    MOVE 08           TO PGET-RETURN                            
                    SET WS-NOT-FOUND  TO TRUE                                   
                 WHEN INDX-KEY(INDX-XXX) = NDXE-KEY                             
                    SET WS-FOUND      TO TRUE                                   
              END-SEARCH                                                        
           END-IF.                                                              
                                                                                
       2000-SEARCH-INDEX-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SETUP RETURN INFORMATION                                                
      ****************************************************************          
       3000-SETUP-RETURN.                                                       
           IF WS-NOT-FOUND                                                      
           OR WS-INVALID-REQUEST                                                
              MOVE 0       TO RTRN-PACKET-TYPE                                  
              MOVE SPACE   TO RTRN-SEGMENT-VERSION                              
              MOVE +0      TO RTRN-AREA3-DATA-LENGTH                            
              GO TO 3000-SETUP-RETURN-EXIT                                      
           END-IF.                                                              
                                                                                
           MOVE INDX-PACKET-OFFSET(INDX-XXX) TO WS-HOLD-OFFSET.                 
                                                                                
           ADD 4 TO WS-HOLD-OFFSET.                                             
                                                                                
           MOVE AREA1(WS-HOLD-OFFSET:2)      TO RTRN-PACKET-TYPE                
                                                WS-HOLD-TYPE.                   
                                                                                
           IF GROUP-START                                                       
              MOVE 54                 TO PGET-RETURN                            
              SET WS-INVALID-REQUEST  TO TRUE                                   
              MOVE 0       TO RTRN-PACKET-TYPE                                  
              MOVE SPACE   TO RTRN-SEGMENT-VERSION                              
              MOVE +0      TO RTRN-AREA3-DATA-LENGTH                            
              GO TO 3000-SETUP-RETURN-EXIT                                      
           END-IF.                                                              
                                                                                
           ADD 2 TO WS-HOLD-OFFSET.                                             
                                                                                
           IF PREDEF-SEGMENT                                                    
              MOVE AREA1(WS-HOLD-OFFSET:6)     TO RTRN-SEGMENT-VERSION          
              ADD 6 TO WS-HOLD-OFFSET                                           
                                                                                
              MOVE AREA1(WS-HOLD-OFFSET:9)     TO WS-HOLD-LENGTH                
              ADD 9 TO WS-HOLD-OFFSET                                           
           ELSE                                                                 
              MOVE SPACE        TO RTRN-SEGMENT-VERSION                         
              MOVE 0            TO WS-HOLD-LENGTH                               
              MOVE AREA1(WS-HOLD-OFFSET:3)                                      
                                TO WS-HOLD-LENGTH-X(7:3)                        
              ADD 3 TO WS-HOLD-OFFSET                                           
           END-IF.                                                              
                                                                                
           MOVE WS-HOLD-LENGTH                TO RTRN-AREA3-DATA-LENGTH.        
                                                                                
           IF (RTRN-PACKET-TYPE         NOT = INPT-PACKET-TYPE                  
               AND INPT-PACKET-TYPE     NOT = SPACE)                            
           OR (RTRN-AREA3-DATA-LENGTH   NOT = INPT-PACKET-LENGTH                
               AND INPT-PACKET-LENGTH   NOT = 0)                                
           OR (RTRN-SEGMENT-VERSION     NOT = INPT-SEGMENT-VERSION              
               AND INPT-SEGMENT-VERSION NOT = SPACE)                            
                                                                                
              IF RTRN-AREA3-DATA-LENGTH > LENGTH OF RTRN-WORK-AREA              
                 MOVE 45 TO PGET-RETURN                                         
                 GO TO 3000-SETUP-RETURN-EXIT                                   
              ELSE                                                              
                 MOVE AREA1(WS-HOLD-OFFSET:WS-HOLD-LENGTH)                      
                                TO RTRN-WORK-AREA(1:WS-HOLD-LENGTH)             
              END-IF                                                            
           ELSE                                                                 
              IF WS-HOLD-LENGTH > INPT-MAX-LENGTH-AREA3                         
                 MOVE 40 TO PGET-RETURN                                         
                 GO TO 3000-SETUP-RETURN-EXIT                                   
                                                                                
              ELSE                                                              
                 MOVE AREA1(WS-HOLD-OFFSET:WS-HOLD-LENGTH)                      
                                   TO AREA3(1:WS-HOLD-LENGTH)                   
              END-IF                                                            
           END-IF.                                                              
                                                                                
       3000-SETUP-RETURN-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
