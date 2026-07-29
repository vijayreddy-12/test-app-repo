CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2VER1.                                                 
      *AUTHOR.        JIM KLAPWYK.                                              
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.                                                            
      *DATE-COMPILED.                                                           
      *----------------------------------------------------------------*        
      *                                                                         
      *  SYSTEM    : BUSINESS CONTINUITY                                        
      *                                                                         
      *  LANGUAGE  : COBOL II                                                   
      *                                                                         
      *  THIS MODULE WILL CALL THE VERSION 1 PARSER AND SET UP THE              
      *  RETURNS FOR THE CALLING MODULE.                                        
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  SEP 1998   J KLAPWYK  CREATED                                     
      *  002  AUG 2008   IBM        ENTERRPISE COMPILER UPGRADE                 
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
               '**   MLX2VER1 WORKING STORAGE BEGINS  **'.                      
                                                                                
       01  WS-FIXED.                                                            
           05  WS-V1-HEADER-LENGTH          PIC S9(4) COMP VALUE 30.            
           05  WS-VERSION-LENGTH            PIC S9(4) COMP VALUE 6.             
       01  WS-VARIABLES.                                                        
           05  XXX                          PIC S9(4) COMP.                     
           05  WS-VERSION-NUMBER-X.                                             
               10  WS-VERSION-NUMBER        PIC 9(6).                           
                   88  WS-VERSION-000100  VALUE 000100.                         
           05  WS-HOLD-STRUCTURE            PIC X(16000).                       
           05  WS-LENGTH                    PIC S9(4) COMP.                     
           05  WS-COUNT                     PIC S9(4) COMP.                     
           05  WS-DONE-IND                  PIC X(1).                           
               88  WS-DONE                VALUE 'Y'.                            
               88  WS-NOT-DONE            VALUE 'N'.                            
           05  WS-FILE-OPEN-IND             PIC X(1) VALUE 'N'.                 
               88  WS-FILES-OPEN          VALUE 'Y'.                            
               88  WS-FILES-NOT-OPEN      VALUE 'N'.                            
           05  WS-CLEAR-MESSAGE-IND         PIC X(1).                           
               88  WS-CLEAR-MESSAGE       VALUE 'Y'.                            
               88  WS-NOT-CLEAR-MESSAGE   VALUE 'N'.                            
           05  WS-CALL                      PIC X(1).                           
               88  WS-GET                 VALUE 'G'.                            
               88  WS-PUT                 VALUE 'P'.                            
               88  WS-CLEAR               VALUE 'D'.                            
               88  WS-TAGGED-TO-COPY      VALUE 'T'.                            
               88  WS-COPY-TO-TAGGED      VALUE 'C'.                            
               88  WS-COPY-TO-COPY        VALUE 'B'.                            
           05  WS-CALLED-MODULES.                                               
               10  MLXGPARB                 PIC X(8) VALUE 'MLXGPARB'.          
               10  MLXGPDRV                 PIC X(8) VALUE 'MLXGPDRV'.          
                                                                                
      *----------------------------------------------------------------*        
      *  PARSER HIGH LEVEL CONTROL VALUES                                       
      *----------------------------------------------------------------*        
       01  MLXXPCTL-RECORD.                                                     
           COPY MLXXPCTL.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  PARSER RETURNS                                                         
      *----------------------------------------------------------------*        
       01  MLETEROR-RECORD.                                                     
           COPY MLETEROR.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  STRUCTURE 1                                                            
      *----------------------------------------------------------------*        
       01  WS-STRUCTURE-1                     PIC X(16000).                     
                                                                                
      *----------------------------------------------------------------*        
      *  STRUCTURE 2                                                            
      *----------------------------------------------------------------*        
       01  WS-STRUCTURE-2.                                                      
           05  WS-STRUCT-2-TAG                PIC X(4).                         
           05  WS-STRUCT-2-TYPE               PIC X(2).                         
           05  WS-STRUCT-2-LENGTH             PIC 9(3).                         
           05  WS-STRUCT-2-VALUE              PIC X(15991).                     
                                                                                
      *----------------------------------------------------------------*        
      *  TARGET STRUCTURE                                                       
      *----------------------------------------------------------------*        
       01  WS-TARGET-STRUCTURE                PIC X(16000).                     
                                                                                
                                                                                
       01  FILLER                             PIC X(40) VALUE                   
               '***  MLX2VER1 WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  PARSER PARMS                                                           
      *----------------------------------------------------------------*        
       01  WS-CALL-TYPE                       PIC X(1).                         
                                                                                
       01  PARSER-INPUTS.                                                       
           COPY MLX2PRSI.                                                       
                                                                                
       01  AREA1                               PIC X(1).                        
                                                                                
       01  AREA2                               PIC X(1).                        
                                                                                
       01  AREA3                               PIC X(1).                        
                                                                                
       01  PARSER-OUTPUTS.                                                      
           COPY MLX2PRSO.                                                       
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING WS-CALL-TYPE                                    
                                PARSER-INPUTS                                   
                                AREA1                                           
                                AREA2                                           
                                AREA3                                           
                                PARSER-OUTPUTS.                                 
      *----------------------------------------------------------------*        
      ****************************************************************          
      *    MAINLINE                                                             
      ****************************************************************          
       0000-MAINLINE.                                                           
           PERFORM 1000-INIT THRU                                               
                   1000-INIT-EXIT.                                              
                                                                                
           IF PRSO-CRITICAL-ERROR                                               
              GO TO 0000-MAINLINE-EXIT                                          
           END-IF.                                                              
                                                                                
           IF NOT WS-CLEAR-MESSAGE                                              
              PERFORM 2000-PARSER-CALL THRU                                     
                      2000-PARSER-CALL-EXIT                                     
           END-IF.                                                              
                                                                                
           PERFORM 3000-SETUP-RETURNS THRU                                      
                   3000-SETUP-RETURNS-EXIT.                                     
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      *    INITIALIZE RETURNS.                                                  
      ****************************************************************          
       1000-INIT.                                                               
           SET WS-VERSION-000100    TO TRUE.                                    
           SET WS-NOT-CLEAR-MESSAGE TO TRUE.                                    
           MOVE WS-VERSION-NUMBER-X TO PRSO-MESSAGE-VERSION.                    
           MOVE WS-CALL-TYPE        TO WS-CALL.                                 
                                                                                
           IF PRSI-MAX-LENGTH-AREA1 > 16000                                     
           OR PRSI-MAX-LENGTH-AREA2 > 16000                                     
           OR PRSI-MAX-LENGTH-AREA3 > 16000                                     
              SET PRSO-CRITICAL-ERROR TO TRUE                                   
              MOVE 1 TO PRSO-ERROR-COUNT                                        
              MOVE 44 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)                      
              GO TO 1000-INIT-EXIT                                              
           END-IF.                                                              
                                                                                
      *                                                                         
      *    SET UP BATCH OR CICS ENVIRONMENT SWITCHES                            
      *                                                                         
           COPY MLX2ENVI.                                                       
                                                                                
           IF PCTL-ENV-BATCH                                                    
           AND WS-FILES-NOT-OPEN                                                
              SET PCTL-REQ-FILE-OPENS           TO TRUE                         
              PERFORM 2000-PARSER-CALL THRU                                     
                      2000-PARSER-CALL-EXIT                                     
              IF NOT PCTL-RET-OK                                                
                 SET PRSO-CRITICAL-ERROR TO TRUE                                
                 MOVE 1 TO PRSO-ERROR-COUNT                                     
                 MOVE 70 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)                   
                 GO TO 1000-INIT-EXIT                                           
              END-IF                                                            
              SET WS-FILES-OPEN TO TRUE                                         
           ELSE                                                                 
              SET WS-FILES-OPEN TO TRUE                                         
           END-IF.                                                              
                                                                                
           IF PRSI-SECTION-BODY                                                 
              NEXT SENTENCE                                                     
           ELSE                                                                 
              IF WS-CLEAR                                                       
              AND PRSI-SECTION-ALL                                              
                 SET WS-CLEAR-MESSAGE TO TRUE                                   
              ELSE                                                              
                 SET PRSO-CRITICAL-ERROR TO TRUE                                
                 MOVE 1 TO PRSO-ERROR-COUNT                                     
                 MOVE 54 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)                   
                 GO TO 1000-INIT-EXIT                                           
              END-IF                                                            
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
              WHEN WS-GET                                                       
                 PERFORM 1100-GET THRU                                          
                         1100-GET-EXIT                                          
              WHEN WS-PUT                                                       
                 PERFORM 1200-PUT THRU                                          
                         1200-PUT-EXIT                                          
              WHEN WS-CLEAR                                                     
                 PERFORM 1300-CLEAR THRU                                        
                         1300-CLEAR-EXIT                                        
              WHEN WS-TAGGED-TO-COPY                                            
                 PERFORM 1400-TAG-TO-COPY THRU                                  
                         1400-TAG-TO-COPY-EXIT                                  
              WHEN WS-COPY-TO-TAGGED                                            
                 PERFORM 1500-COPY-TO-TAG THRU                                  
                         1500-COPY-TO-TAG-EXIT                                  
              WHEN OTHER                                                        
                 SET PRSO-CRITICAL-ERROR TO TRUE                                
                 MOVE 1 TO PRSO-ERROR-COUNT                                     
                 MOVE 54 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)                   
                 GO TO 1000-INIT-EXIT                                           
           END-EVALUATE.                                                        
                                                                                
           MOVE SPACE TO EROR-RETURN-MESSAGE-AREA.                              
           MOVE ZERO  TO EROR-MESSAGE-COUNTER.                                  
                                                                                
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP PARAMETERS FOR GET CALL                                       
      ****************************************************************          
       1100-GET.                                                                
           SET PCTL-REQ-RETRIEVE             TO TRUE.                           
           SET PCTL-SRC-STR-TYPE-TAGGED      TO TRUE.                           
           SET PCTL-SRC-2-STR-TYPE-NONE      TO TRUE.                           
           SET PCTL-TGT-STR-TYPE-COPYBOOK    TO TRUE.                           
           SET PCTL-DEBUG-OFF                TO TRUE.                           
           SET PCTL-RUN-NORMAL               TO TRUE.                           
           MOVE SPACE                        TO PCTL-SRC-STRUCT-NAME            
                                                PCTL-SRC-2-STRUCT-NAME.         
           MOVE 'RETRIEVE'                   TO PCTL-TGT-STRUCT-NAME.           
           MOVE AREA1(1:WS-VERSION-LENGTH)   TO WS-VERSION-NUMBER.              
           MOVE AREA1(WS-V1-HEADER-LENGTH + 1:                                  
                     PRSI-MAX-LENGTH-AREA1 - WS-V1-HEADER-LENGTH)               
                            TO WS-STRUCTURE-1.                                  
                                                                                
           MOVE PRSI-PACKET-ID     TO WS-STRUCT-2-TAG.                          
           MOVE PRSI-PACKET-TYPE   TO WS-STRUCT-2-TYPE.                         
           MOVE PRSI-PACKET-LENGTH TO WS-STRUCT-2-LENGTH.                       
                                                                                
           MOVE SPACE TO WS-TARGET-STRUCTURE.                                   
       1100-GET-EXIT.                                                           
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP PARAMETERS FOR PUT CALL                                       
      ****************************************************************          
       1200-PUT.                                                                
           SET PCTL-REQ-UPDATE               TO TRUE.                           
           SET PCTL-SRC-STR-TYPE-TAGGED      TO TRUE.                           
           SET PCTL-SRC-2-STR-TYPE-NONE      TO TRUE.                           
           SET PCTL-TGT-STR-TYPE-TAGGED      TO TRUE.                           
           SET PCTL-RUN-SKIP-DEFAULT-PASS    TO TRUE.                           
           SET PCTL-DEBUG-OFF                TO TRUE.                           
           MOVE SPACE                        TO PCTL-SRC-STRUCT-NAME            
                                                PCTL-SRC-2-STRUCT-NAME          
                                                PCTL-TGT-STRUCT-NAME            
           MOVE AREA1(1:WS-VERSION-LENGTH)   TO WS-VERSION-NUMBER.              
           MOVE AREA1(WS-V1-HEADER-LENGTH + 1:                                  
                      PRSI-MAX-LENGTH-AREA1 - WS-V1-HEADER-LENGTH)              
                           TO WS-STRUCTURE-1.                                   
                                                                                
           MOVE PRSI-PACKET-ID     TO WS-STRUCT-2-TAG.                          
           MOVE PRSI-PACKET-TYPE   TO WS-STRUCT-2-TYPE.                         
           MOVE PRSI-PACKET-LENGTH TO WS-STRUCT-2-LENGTH.                       
           MOVE AREA2 (1:PRSI-MAX-LENGTH-AREA2)                                 
                           TO WS-STRUCT-2-VALUE.                                
                                                                                
           MOVE SPACE TO WS-TARGET-STRUCTURE.                                   
       1200-PUT-EXIT.                                                           
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP PARAMETERS FOR CLEAR CALL                                     
      ****************************************************************          
       1300-CLEAR.                                                              
           MOVE AREA1(1:WS-VERSION-LENGTH)   TO WS-VERSION-NUMBER.              
                                                                                
           IF WS-CLEAR-MESSAGE                                                  
              MOVE '9998' TO WS-TARGET-STRUCTURE                                
              GO TO 1300-CLEAR-EXIT                                             
           END-IF.                                                              
                                                                                
           SET PCTL-REQ-DELETE               TO TRUE.                           
           SET PCTL-SRC-STR-TYPE-TAGGED      TO TRUE.                           
           SET PCTL-SRC-2-STR-TYPE-NONE      TO TRUE.                           
           SET PCTL-TGT-STR-TYPE-TAGGED      TO TRUE.                           
           SET PCTL-DEBUG-OFF                TO TRUE.                           
           SET PCTL-RUN-SKIP-DEFAULT-PASS    TO TRUE.                           
           MOVE SPACE                        TO PCTL-SRC-STRUCT-NAME            
                                                PCTL-SRC-2-STRUCT-NAME          
                                                PCTL-TGT-STRUCT-NAME            
           MOVE AREA1(WS-V1-HEADER-LENGTH + 1:                                  
                    PRSI-MAX-LENGTH-AREA1 - WS-V1-HEADER-LENGTH)                
                            TO WS-STRUCTURE-1.                                  
                                                                                
           MOVE PRSI-PACKET-ID     TO WS-STRUCT-2-TAG.                          
                                                                                
           MOVE SPACE TO WS-TARGET-STRUCTURE.                                   
       1300-CLEAR-EXIT.                                                         
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP PARAMETERS FOR TAG TO COPY CALL                               
      ****************************************************************          
       1400-TAG-TO-COPY.                                                        
           SET PCTL-REQ-PARSE-DATA           TO TRUE.                           
           SET PCTL-SRC-STR-TYPE-TAGGED      TO TRUE.                           
           SET PCTL-SRC-2-STR-TYPE-NONE      TO TRUE.                           
           SET PCTL-TGT-STR-TYPE-COPYBOOK    TO TRUE.                           
           SET PCTL-DEBUG-OFF                TO TRUE.                           
                                                                                
           EVALUATE TRUE                                                        
              WHEN PRSI-NORMAL                                                  
                 SET PCTL-RUN-NORMAL            TO TRUE                         
              WHEN PRSI-SKIP-DEFAULT                                            
                 SET PCTL-RUN-SKIP-DEFAULT-PASS TO TRUE                         
              WHEN PRSI-SKIP-VALUE                                              
                 SET PCTL-RUN-SKIP-VALUES-PASS  TO TRUE                         
              WHEN OTHER                                                        
                 SET PRSO-CRITICAL-ERROR TO TRUE                                
                 MOVE 1 TO PRSO-ERROR-COUNT                                     
                 MOVE 67 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)                   
                 GO TO 1400-TAG-TO-COPY-EXIT                                    
           END-EVALUATE.                                                        
                                                                                
           MOVE SPACE                        TO PCTL-SRC-STRUCT-NAME            
                                                PCTL-SRC-2-STRUCT-NAME.         
           MOVE PRSI-STRUCTURE-NAME1         TO PCTL-TGT-STRUCT-NAME.           
           MOVE AREA1(WS-V1-HEADER-LENGTH + 1:                                  
                   PRSI-MAX-LENGTH-AREA1 - WS-V1-HEADER-LENGTH)                 
                          TO WS-STRUCTURE-1.                                    
           MOVE SPACE TO WS-TARGET-STRUCTURE.                                   
           MOVE AREA1(1:WS-VERSION-LENGTH)   TO WS-VERSION-NUMBER.              
       1400-TAG-TO-COPY-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP PARAMETERS FOR COPY TO TAG CALL                               
      ****************************************************************          
       1500-COPY-TO-TAG.                                                        
           SET PCTL-REQ-PARSE-DATA           TO TRUE.                           
           SET PCTL-SRC-STR-TYPE-COPYBOOK    TO TRUE.                           
           SET PCTL-TGT-STR-TYPE-TAGGED      TO TRUE.                           
           SET PCTL-DEBUG-OFF                TO TRUE.                           
           MOVE PRSI-STRUCTURE-NAME1         TO PCTL-SRC-STRUCT-NAME.           
           MOVE SPACE                        TO PCTL-TGT-STRUCT-NAME.           
                                                                                
           MOVE AREA3(1:PRSI-MAX-LENGTH-AREA3)                                  
                          TO WS-STRUCTURE-1.                                    
                                                                                
           IF AREA1(WS-V1-HEADER-LENGTH + 1:4) NUMERIC                          
           AND AREA1(WS-V1-HEADER-LENGTH + 1:4) NOT = '9998'                    
              MOVE AREA1(WS-V1-HEADER-LENGTH + 1:                               
                           PRSI-MAX-LENGTH-AREA1 - WS-V1-HEADER-LENGTH)         
                         TO WS-STRUCTURE-2                                      
              SET PCTL-SRC-2-STR-TYPE-TAGGED TO TRUE                            
           ELSE                                                                 
              MOVE SPACE TO WS-STRUCTURE-2                                      
              SET PCTL-SRC-2-STR-TYPE-NONE TO TRUE                              
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
              WHEN PRSI-NORMAL                                                  
                 SET PCTL-RUN-NORMAL            TO TRUE                         
              WHEN PRSI-SKIP-DEFAULT                                            
                 SET PCTL-RUN-SKIP-DEFAULT-PASS TO TRUE                         
              WHEN PRSI-SKIP-VALUE                                              
                 SET PCTL-RUN-SKIP-VALUES-PASS  TO TRUE                         
              WHEN OTHER                                                        
                 SET PRSO-CRITICAL-ERROR TO TRUE                                
                 MOVE 1 TO PRSO-ERROR-COUNT                                     
                 MOVE 67 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)                   
                 GO TO 1500-COPY-TO-TAG-EXIT                                    
           END-EVALUATE.                                                        
                                                                                
           MOVE AREA1(1:WS-VERSION-LENGTH)   TO WS-VERSION-NUMBER.              
                                                                                
           MOVE SPACE TO WS-TARGET-STRUCTURE.                                   
       1500-COPY-TO-TAG-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CALL VERSION 1 PARSER                                                
      ****************************************************************          
       2000-PARSER-CALL.                                                        
           MOVE '00' TO PCTL-RETURN-CODE.                                       
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *  FOR BATCH:  CALL MLXGPARB                                              
      *  FOR CICS:   CALL MLXGPDRV                                              
      *                            USING MLXXPCTL-RECORD                        
      *                                  WS-STRUCTURE-1                         
      *                                  WS-STRUCTURE-2                         
      *                                  WS-TARGET-STRUCTURE                    
      *                                  EROR-RETURN-MESSAGE-AREA.              
      *                                                                         
      *                                                                         
           COPY MLX2CPV1.                                                       
                                                                                
       2000-PARSER-CALL-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SETUP RETURN VALUES FOR CALLING MODULE                               
      ****************************************************************          
       3000-SETUP-RETURNS.                                                      
           MOVE WS-VERSION-NUMBER-X TO PRSO-MESSAGE-VERSION.                    
                                                                                
           EVALUATE TRUE                                                        
              WHEN PCTL-RET-OK                                                  
                 SET PRSO-NO-ERROR           TO TRUE                            
              WHEN NON-CRITICAL-ERROR                                           
                 SET PRSO-NON-CRITICAL-ERROR TO TRUE                            
              WHEN OTHER                                                        
                 SET PRSO-CRITICAL-ERROR     TO TRUE                            
           END-EVALUATE.                                                        
                                                                                
           MOVE EROR-MESSAGE-COUNTER      TO PRSO-ERROR-COUNT.                  
                                                                                
           PERFORM VARYING XXX FROM 1 BY 1                                      
           UNTIL XXX > EROR-MESSAGE-COUNTER                                     
              MOVE EROR-GROUP-ID-LVL1(XXX)     TO PRSO-LVL1-ID(XXX)             
              MOVE EROR-GROUP-SEQ-NO-LVL1(XXX) TO PRSO-LVL1-OCCUR(XXX)          
              MOVE EROR-GROUP-ID-LVL2(XXX)     TO PRSO-LVL2-ID(XXX)             
              MOVE EROR-GROUP-SEQ-NO-LVL2(XXX) TO PRSO-LVL2-OCCUR(XXX)          
              MOVE EROR-GROUP-ID-LVL3(XXX)     TO PRSO-LVL3-ID(XXX)             
              MOVE EROR-GROUP-SEQ-NO-LVL3(XXX) TO PRSO-LVL3-OCCUR(XXX)          
              MOVE 09                          TO PRSO-ERROR-CODE(XXX)          
              MOVE EROR-MESSAGE-DATA(XXX)      TO PRSO-V1-MESSAGE(XXX)          
           END-PERFORM.                                                         
                                                                                
           IF PRSO-NO-ERROR                                                     
              EVALUATE TRUE                                                     
                 WHEN WS-GET                                                    
                    MOVE PRSI-PACKET-LENGTH TO PRSO-DATA-LENGTH                 
                    IF WS-TARGET-STRUCTURE(1:PRSI-PACKET-LENGTH)                
                                  = LOW-VALUES                                  
                       IF PRSO-ERROR-COUNT < 10                                 
                          ADD 1 TO PRSO-ERROR-COUNT                             
                       END-IF                                                   
                       SET PRSO-NOT-FOUND      TO TRUE                          
                       MOVE PRSI-PACKET-ID                                      
                               TO PRSO-ERROR-PACKET(PRSO-ERROR-COUNT)           
                       MOVE 08 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)             
                       MOVE 0  TO PRSO-DATA-LENGTH                              
                    ELSE                                                        
                    MOVE WS-TARGET-STRUCTURE(1:PRSO-DATA-LENGTH)                
                            TO AREA3(1:PRSO-DATA-LENGTH)                        
                    END-IF                                                      
                 WHEN WS-TAGGED-TO-COPY                                         
                 WHEN WS-COPY-TO-COPY                                           
                    MOVE PRSI-MAX-LENGTH-AREA3 TO PRSO-DATA-LENGTH              
                    MOVE WS-TARGET-STRUCTURE(1:PRSO-DATA-LENGTH)                
                            TO AREA3(1:PRSO-DATA-LENGTH)                        
                 WHEN WS-PUT                                                    
                 WHEN WS-CLEAR                                                  
                    PERFORM 3200-CALC-LENGTH THRU                               
                            3200-CALC-LENGTH-EXIT                               
                    MOVE WS-LENGTH             TO PRSO-DATA-LENGTH              
                    IF WS-LENGTH > PRSI-MAX-LENGTH-AREA3                        
                       IF PRSO-ERROR-COUNT < 10                                 
                          ADD 1 TO PRSO-ERROR-COUNT                             
                       END-IF                                                   
                       SET PRSO-CRITICAL-ERROR TO TRUE                          
                       MOVE 40 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)             
                       GO TO 3000-SETUP-RETURNS-EXIT                            
                    END-IF                                                      
                    IF WS-CLEAR-MESSAGE                                         
                       MOVE SPACE TO AREA3(1:PRSI-MAX-LENGTH-AREA3)             
                    END-IF                                                      
                    MOVE AREA1(1:WS-V1-HEADER-LENGTH)                           
                              TO AREA3(1:WS-V1-HEADER-LENGTH)                   
                    MOVE WS-TARGET-STRUCTURE(1:PRSO-DATA-LENGTH -               
                                                   WS-V1-HEADER-LENGTH)         
                              TO AREA3(WS-V1-HEADER-LENGTH + 1:                 
                                PRSO-DATA-LENGTH - WS-V1-HEADER-LENGTH)         
                 WHEN WS-COPY-TO-TAGGED                                         
                    PERFORM 3200-CALC-LENGTH THRU                               
                            3200-CALC-LENGTH-EXIT                               
                    MOVE WS-LENGTH             TO PRSO-DATA-LENGTH              
                    IF WS-LENGTH > PRSI-MAX-LENGTH-AREA1                        
                       IF PRSO-ERROR-COUNT < 10                                 
                          ADD 1 TO PRSO-ERROR-COUNT                             
                       END-IF                                                   
                       SET PRSO-CRITICAL-ERROR TO TRUE                          
                       MOVE 40 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)             
                       GO TO 3000-SETUP-RETURNS-EXIT                            
                    END-IF                                                      
                    MOVE WS-TARGET-STRUCTURE(1:PRSO-DATA-LENGTH -               
                                                   WS-V1-HEADER-LENGTH)         
                              TO AREA1(WS-V1-HEADER-LENGTH + 1:                 
                                PRSO-DATA-LENGTH - WS-V1-HEADER-LENGTH)         
              END-EVALUATE                                                      
           END-IF.                                                              
       3000-SETUP-RETURNS-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CALCULATE THE LENGTH OF THE RETURNED MESSAGE                         
      ****************************************************************          
       3200-CALC-LENGTH.                                                        
           MOVE WS-TARGET-STRUCTURE TO WS-HOLD-STRUCTURE.                       
                                                                                
                                                                                
           MOVE LOW-VALUES TO WS-STRUCTURE-1                                    
                              WS-STRUCTURE-2.                                   
                                                                                
           IF WS-TARGET-STRUCTURE(1:4) = '9998'                                 
              MOVE 4          TO WS-LENGTH                                      
              SET WS-DONE     TO TRUE                                           
           ELSE                                                                 
              MOVE 0          TO WS-LENGTH                                      
              SET WS-NOT-DONE TO TRUE                                           
           END-IF.                                                              
                                                                                
           PERFORM UNTIL WS-DONE                                                
              UNSTRING WS-HOLD-STRUCTURE                                        
                DELIMITED BY '9998'                                             
                INTO WS-STRUCTURE-1 COUNT IN WS-COUNT                           
                     WS-STRUCTURE-2                                             
              END-UNSTRING                                                      
                                                                                
              IF WS-STRUCTURE-2(1:1) = LOW-VALUES                               
              OR WS-COUNT >= LENGTH OF WS-HOLD-STRUCTURE                        
                 SET WS-DONE TO TRUE                                            
              ELSE                                                              
                 MOVE WS-TARGET-STRUCTURE(WS-LENGTH + WS-COUNT + 5:             
                         LENGTH OF WS-HOLD-STRUCTURE - (WS-COUNT + 4))          
                                 TO WS-HOLD-STRUCTURE                           
                 MOVE LOW-VALUES TO WS-STRUCTURE-1                              
                                    WS-STRUCTURE-2                              
              END-IF                                                            
                                                                                
              COMPUTE WS-LENGTH = WS-LENGTH + WS-COUNT + 4                      
           END-PERFORM.                                                         
                                                                                
           COMPUTE WS-LENGTH = WS-LENGTH + WS-V1-HEADER-LENGTH.                 
                                                                                
       3200-CALC-LENGTH-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
