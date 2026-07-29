CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PCLR.                                                 
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
      *  THIS MODULE WILL TAKE A MESSAGE AND DELETE A PACKET                    
      *  RETURNING THE FULL NEW MESSAGE                                         
      *                                                                         
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  SEP 1998   J KLAPWYK  CREATED                                     
      *  002  JUL 2008   IBM GR     UPGRADED IN ECU PROJECT                     
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
               '**   MLX2PCLR WORKING STORAGE BEGINS  **'.                      
       01  FIXED.                                                               
           05  WS-V2-FIXED-HEADER-LENGTH      PIC S9(4) COMP                    
                                            VALUE +174.                         
           05  WS-HEADER-START                PIC X(4) VALUE '9991'.            
           05  WS-BODY-START                  PIC X(4) VALUE '9992'.            
           05  WS-TRAILER-START               PIC X(4) VALUE '9993'.            
                                                                                
       01  VARIABLES.                                                           
           05  WS-INDEX-VALUE                 PIC S9(4) COMP.                   
           05  WS-HOLD-PACKET                 PIC X(4).                         
               88  P-SPECIAL-VALUE          VALUE '9995' '9996' '9997'          
                                                  '9998' '9999'.                
           05  WS-MOVE-OFFSET                 PIC S9(4) COMP.                   
           05  WS-MOVE-TO-OFFSET              PIC S9(4) COMP.                   
           05  WS-MOVE-LENGTH                 PIC S9(4) COMP.                   
           05  WS-LEVEL-DEL-IND               PIC X(1).                         
               88  WS-MESSAGE-DEL           VALUE 'M'.                          
               88  WS-SECTION-DEL           VALUE 'S'.                          
               88  WS-LVL-3-DEL             VALUE '3'.                          
               88  WS-LVL-2-DEL             VALUE '2'.                          
               88  WS-LVL-1-DEL             VALUE '1'.                          
               88  WS-PACKET-DEL            VALUE 'P'.                          
           05  WS-FOUND-SW                    PIC X(1).                         
               88  WS-FOUND                 VALUE 'Y'.                          
               88  WS-NOT-FOUND             VALUE 'N'.                          
           05  WS-FINISH-IND                  PIC X(1).                         
               88  WS-DONE                  VALUE 'Y'.                          
               88  WS-NOT-DONE              VALUE 'N'.                          
                                                                                
       01  WS-CALLED-MODULES.                                                   
           05  MLX2PBMG                       PIC X(8) VALUE 'MLX2PBMG'.        
           05  PBMG-RETURN                    PIC 9(2).                         
                                                                                
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
       01  AREA2                               PIC X(1).                        
       01  AREA3                               PIC X(1).                        
       01  PCLR-RETURN                         PIC 9(2).                        
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING INPUT-PARMS                                     
                                PARSER-RETURNS                                  
                                COMM-FIELDS                                     
                                INDEX-DEFN                                      
                                AREA1                                           
                                AREA2                                           
                                AREA3                                           
                                PCLR-RETURN.                                    
      *----------------------------------------------------------------*        
       0000-MAINLINE.                                                           
           PERFORM 1000-INIT THRU                                               
                   1000-INIT-EXIT.                                              
                                                                                
           IF PCLR-RETURN NOT = 0                                               
              GO TO 0000-MAINLINE-EXIT                                          
           END-IF.                                                              
                                                                                
           PERFORM 2000-CLEAR THRU                                              
                   2000-CLEAR-EXIT.                                             
                                                                                
           IF PCLR-RETURN NOT = 0                                               
           AND PCLR-RETURN NOT = 08                                             
              GO TO 0000-MAINLINE-EXIT                                          
           END-IF.                                                              
                                                                                
           PERFORM 3000-REBUILD THRU                                            
                   3000-REBUILD-EXIT.                                           
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      * INITIALIZATION ROUTINE.                                                 
      ****************************************************************          
       1000-INIT.                                                               
           MOVE 0                     TO PCLR-RETURN                            
                                         RTRN-PACKET-TYPE.                      
                                                                                
           MOVE SPACE                 TO RTRN-SEGMENT-VERSION.                  
                                                                                
           MOVE INPT-PACKET-ID        TO WS-HOLD-PACKET.                        
           IF P-SPECIAL-VALUE                                                   
              MOVE 54                 TO PCLR-RETURN                            
              GO TO 1000-INIT-EXIT                                              
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
              WHEN INPT-SECTION-HEADER                                          
                 MOVE WS-HEADER-START        TO NDXE-SECTION-ID                 
              WHEN INPT-SECTION-BODY                                            
                 MOVE WS-BODY-START          TO NDXE-SECTION-ID                 
              WHEN INPT-SECTION-TRAILER                                         
                 MOVE WS-TRAILER-START       TO NDXE-SECTION-ID                 
              WHEN OTHER                                                        
                 IF INPT-ACTION-CLEAR                                           
                 AND INPT-SECTION = SPACE                                       
                    MOVE SPACE TO NDXE-SECTION-ID                               
                 ELSE                                                           
                    MOVE 50                     TO PCLR-RETURN                  
                    GO TO 1000-INIT-EXIT                                        
                 END-IF                                                         
           END-EVALUATE.                                                        
                                                                                
           MOVE INPT-SECTION-OCCUR           TO NDXE-SECTION-OCCUR.             
           MOVE INPT-GRP-LVL-1-ID            TO NDXE-LVL-1-ID.                  
           MOVE INPT-GRP-LVL-1-OCCUR         TO NDXE-LVL-1-OCCUR.               
           MOVE INPT-GRP-LVL-2-ID            TO NDXE-LVL-2-ID.                  
           MOVE INPT-GRP-LVL-2-OCCUR         TO NDXE-LVL-2-OCCUR.               
           MOVE INPT-GRP-LVL-3-ID            TO NDXE-LVL-3-ID.                  
           MOVE INPT-GRP-LVL-3-OCCUR         TO NDXE-LVL-3-OCCUR.               
           MOVE INPT-PACKET-ID               TO NDXE-PACKET-ID.                 
           SET NDXE-AREA1                    TO TRUE.                           
                                                                                
           EVALUATE TRUE                                                        
              WHEN NDXE-SECTION-ID = SPACE                                      
                 SET WS-MESSAGE-DEL TO TRUE                                     
              WHEN NDXE-PACKET-ID = SPACE                                       
                 SET WS-SECTION-DEL TO TRUE                                     
                 MOVE LOW-VALUES    TO NDXE-LVL-1-ID                            
                                       NDXE-LVL-2-ID                            
                                       NDXE-LVL-3-ID                            
                                       NDXE-LVL-3-X                             
                 MOVE +0            TO NDXE-LVL-1-OCCUR                         
                                       NDXE-LVL-2-OCCUR                         
                                       NDXE-LVL-3-OCCUR                         
                 EVALUATE TRUE                                                  
                   WHEN INPT-SECTION-HEADER                                     
                      MOVE WS-HEADER-START TO NDXE-PACKET-ID                    
                   WHEN INPT-SECTION-BODY                                       
                      MOVE WS-BODY-START   TO NDXE-PACKET-ID                    
                   WHEN INPT-SECTION-TRAILER                                    
                      MOVE WS-TRAILER-START TO NDXE-PACKET-ID                   
                 END-EVALUATE                                                   
              WHEN NDXE-PACKET-ID = NDXE-LVL-3-ID                               
                 SET WS-LVL-3-DEL   TO TRUE                                     
                 MOVE LOW-VALUES    TO NDXE-LVL-3-X                             
              WHEN NDXE-PACKET-ID = NDXE-LVL-2-ID                               
                 SET WS-LVL-2-DEL   TO TRUE                                     
                 MOVE LOW-VALUES     TO NDXE-LVL-3-ID                           
                                        NDXE-LVL-3-X                            
                 MOVE +0             TO NDXE-LVL-3-OCCUR                        
              WHEN NDXE-PACKET-ID = NDXE-LVL-1-ID                               
                 SET WS-LVL-1-DEL   TO TRUE                                     
                 MOVE LOW-VALUES     TO NDXE-LVL-2-ID                           
                                        NDXE-LVL-3-ID                           
                                        NDXE-LVL-3-X                            
                 MOVE +0             TO NDXE-LVL-2-OCCUR                        
                                        NDXE-LVL-3-OCCUR                        
              WHEN OTHER                                                        
                 MOVE SPACE         TO NDXE-LVL-3-X                             
                 SET WS-PACKET-DEL  TO TRUE                                     
           END-EVALUATE.                                                        
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * CLEAR INDEX ENTRY (OR ENTRIES).                                         
      ****************************************************************          
       2000-CLEAR.                                                              
           IF WS-MESSAGE-DEL                                                    
              IF INPT-MAX-LENGTH-AREA3 < (WS-V2-FIXED-HEADER-LENGTH + 4)        
                 MOVE 40           TO PCLR-RETURN                               
                 GO TO 2000-CLEAR-EXIT                                          
              END-IF                                                            
              MOVE SPACE TO AREA3(1:INPT-MAX-LENGTH-AREA3)                      
              MOVE AREA1(1:WS-V2-FIXED-HEADER-LENGTH)                           
                          TO AREA3(1:WS-V2-FIXED-HEADER-LENGTH)                 
              MOVE '9998' TO AREA3(WS-V2-FIXED-HEADER-LENGTH + 1:4)             
              COMPUTE RTRN-AREA3-DATA-LENGTH =                                  
                               WS-V2-FIXED-HEADER-LENGTH + 4                    
              GO TO 2000-CLEAR-EXIT                                             
           END-IF.                                                              
                                                                                
           SEARCH ALL INDX-PACKETS                                              
              AT END                                                            
                 MOVE 08           TO PCLR-RETURN                               
                 GO TO 2000-CLEAR-EXIT                                          
              WHEN INDX-KEY(INDX-XXX) = NDXE-KEY                                
                 SET WS-NOT-DONE TO TRUE                                        
           END-SEARCH.                                                          
                                                                                
           SET WS-INDEX-VALUE TO INDX-XXX.                                      
           PERFORM 2200-DELETE-ENTRIES THRU                                     
                   2200-DELETE-ENTRIES-EXIT                                     
             UNTIL WS-DONE.                                                     
                                                                                
       2000-CLEAR-EXIT.                                                         
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      * DELETE ENTRIES THAT MATCH REQUESTED DELETE                              
      ****************************************************************          
       2200-DELETE-ENTRIES.                                                     
           EVALUATE TRUE                                                        
              WHEN WS-PACKET-DEL                                                
                 PERFORM 2210-PACKET-DELETE THRU                                
                         2210-PACKET-DELETE-EXIT                                
              WHEN WS-LVL-1-DEL                                                 
                 PERFORM 2220-LVL-1-DELETE THRU                                 
                         2220-LVL-1-DELETE-EXIT                                 
              WHEN WS-LVL-2-DEL                                                 
                 PERFORM 2230-LVL-2-DELETE THRU                                 
                         2230-LVL-2-DELETE-EXIT                                 
              WHEN WS-LVL-3-DEL                                                 
                 PERFORM 2240-LVL-3-DELETE THRU                                 
                         2240-LVL-3-DELETE-EXIT                                 
              WHEN WS-SECTION-DEL                                               
                 PERFORM 2250-SECTION-DELETE THRU                               
                         2250-SECTION-DELETE-EXIT                               
           END-EVALUATE.                                                        
                                                                                
           IF WS-NOT-DONE                                                       
              PERFORM 4000-DELETE THRU                                          
                      4000-DELETE-EXIT                                          
                                                                                
              IF PCLR-RETURN NOT = 0                                            
                 SET WS-DONE TO TRUE                                            
              END-IF                                                            
           END-IF.                                                              
                                                                                
       2200-DELETE-ENTRIES-EXIT.                                                
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SETUP FOR PACKET DELETE                                                 
      ****************************************************************          
       2210-PACKET-DELETE.                                                      
           IF INDX-KEY(INDX-XXX) = NDXE-KEY                                     
              SET WS-NOT-DONE TO TRUE                                           
           ELSE                                                                 
              SET WS-DONE     TO TRUE                                           
           END-IF.                                                              
       2210-PACKET-DELETE-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SETUP FOR LEVEL 1 DELETE                                                
      ****************************************************************          
       2220-LVL-1-DELETE.                                                       
           IF INDX-SECTION-ID(INDX-XXX)       = NDXE-SECTION-ID                 
           AND INDX-SECTION-OCCUR(INDX-XXX)   = NDXE-SECTION-OCCUR              
           AND INDX-GRP-LVL-1-ID(INDX-XXX)    = NDXE-LVL-1-ID                   
           AND INDX-GRP-LVL-1-OCCUR(INDX-XXX) = NDXE-LVL-1-OCCUR                
              SET WS-NOT-DONE TO TRUE                                           
           ELSE                                                                 
              SET WS-DONE     TO TRUE                                           
           END-IF.                                                              
       2220-LVL-1-DELETE-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SETUP FOR LEVEL 2 DELETE                                                
      ****************************************************************          
       2230-LVL-2-DELETE.                                                       
           IF INDX-SECTION-ID(INDX-XXX)       = NDXE-SECTION-ID                 
           AND INDX-SECTION-OCCUR(INDX-XXX)   = NDXE-SECTION-OCCUR              
           AND INDX-GRP-LVL-1-ID(INDX-XXX)    = NDXE-LVL-1-ID                   
           AND INDX-GRP-LVL-1-OCCUR(INDX-XXX) = NDXE-LVL-1-OCCUR                
           AND INDX-GRP-LVL-2-ID(INDX-XXX)    = NDXE-LVL-2-ID                   
           AND INDX-GRP-LVL-2-OCCUR(INDX-XXX) = NDXE-LVL-2-OCCUR                
              SET WS-NOT-DONE TO TRUE                                           
           ELSE                                                                 
              SET WS-DONE     TO TRUE                                           
           END-IF.                                                              
       2230-LVL-2-DELETE-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SETUP FOR LEVEL 3 DELETE                                                
      ****************************************************************          
       2240-LVL-3-DELETE.                                                       
           IF INDX-SECTION-ID(INDX-XXX)       = NDXE-SECTION-ID                 
           AND INDX-SECTION-OCCUR(INDX-XXX)   = NDXE-SECTION-OCCUR              
           AND INDX-GRP-LVL-1-ID(INDX-XXX)    = NDXE-LVL-1-ID                   
           AND INDX-GRP-LVL-1-OCCUR(INDX-XXX) = NDXE-LVL-1-OCCUR                
           AND INDX-GRP-LVL-2-ID(INDX-XXX)    = NDXE-LVL-2-ID                   
           AND INDX-GRP-LVL-2-OCCUR(INDX-XXX) = NDXE-LVL-2-OCCUR                
           AND INDX-GRP-LVL-3-ID(INDX-XXX)    = NDXE-LVL-3-ID                   
           AND INDX-GRP-LVL-3-OCCUR(INDX-XXX) = NDXE-LVL-3-OCCUR                
              SET WS-NOT-DONE TO TRUE                                           
           ELSE                                                                 
              SET WS-DONE     TO TRUE                                           
           END-IF.                                                              
       2240-LVL-3-DELETE-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SETUP FOR SECTION DELETE                                                
      ****************************************************************          
       2250-SECTION-DELETE.                                                     
           IF INDX-SECTION-ID(INDX-XXX)       = NDXE-SECTION-ID                 
           AND INDX-SECTION-OCCUR(INDX-XXX)   = NDXE-SECTION-OCCUR              
              SET WS-NOT-DONE TO TRUE                                           
           ELSE                                                                 
              SET WS-DONE     TO TRUE                                           
           END-IF.                                                              
       2250-SECTION-DELETE-EXIT.                                                
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * REBUILD MESSAGE USING UPDATED INDEX                                     
      ****************************************************************          
       3000-REBUILD.                                                            
           IF WS-MESSAGE-DEL                                                    
              SET COMM-REBUILD-INDEX TO TRUE                                    
              GO TO 3000-REBUILD-EXIT                                           
           END-IF.                                                              
                                                                                
      *                                                                         
      *   BATCH OR CICS CALL                                                    
      *                                                                         
      *   CALL MLX2PBMG USING INPUT-PARMS                                       
      *                       PARSER-RETURNS                                    
      *                       COMM-FIELDS                                       
      *                       INDEX-DEFN                                        
      *                       AREA1                                             
      *                       AREA2                                             
      *                       AREA3                                             
      *                       PBMG-RETURN.                                      
      *                                                                         
           COPY MLX2CBMG.                                                       
                                                                                
           IF PBMG-RETURN NOT = 0                                               
              MOVE PBMG-RETURN TO PCLR-RETURN                                   
           END-IF.                                                              
                                                                                
       3000-REBUILD-EXIT.                                                       
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * DELETE INDEX ENTRY FROM INDEX                                           
      ****************************************************************          
       4000-DELETE.                                                             
           COMPUTE WS-MOVE-OFFSET = (LENGTH OF INDX-PACKETS *                   
                                    WS-INDEX-VALUE ) + 1.                       
                                                                                
           COMPUTE WS-MOVE-LENGTH = (LENGTH OF INDX-PACKETS-X -                 
                                     WS-MOVE-OFFSET) + 1.                       
                                                                                
           COMPUTE WS-MOVE-TO-OFFSET = WS-MOVE-OFFSET -                         
                                       LENGTH OF INDX-PACKETS.                  
                                                                                
           MOVE INDX-PACKETS-X(WS-MOVE-OFFSET:WS-MOVE-LENGTH)                   
                    TO INDX-PACKETS-X(WS-MOVE-TO-OFFSET:WS-MOVE-LENGTH).        
                                                                                
           SUBTRACT 1 FROM INDX-PACKET-COUNT.                                   
           IF INDX-PACKET-COUNT < 0                                             
              MOVE 41 TO PCLR-RETURN                                            
              GO TO 4000-DELETE-EXIT                                            
           END-IF.                                                              
       4000-DELETE-EXIT.                                                        
           EXIT.                                                                
      /                                                                         
