       CBL TRUNC(OPT),LIST,DATA(31)                                             
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PTAG.                                                 
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
      *  THIS MODULE WILL CALL THE PARSER WITH INDIVIDUAL PACKET CALLS          
      *  AS WELL AS CALLING THE TRANSLATOR, TO ENSURE THAT DATA IS IN           
      *  THE REQUIRED FORMAT.                                                   
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
               '**   MLX2PTAG WORKING STORAGE BEGINS  **'.                      
                                                                                
       01  WS-VARIABLES.                                                        
           05  PTAG-CODE                    PIC 9(2).                           
               88  PTAG-CRITICAL          VALUE 06 THRU 07                      
                                                09 THRU 99.                     
               88  PTAG-NOT-FOUND         VALUE 08.                             
           05  WS-AREA-IND                  PIC X(1).                           
               88  WS-AREA2               VALUE '2'.                            
               88  WS-RTRN-WORK-AREA      VALUE 'R'.                            
               88  WS-TRNS-WORK-AREA      VALUE 'T'.                            
           05  WS-TRUNCATE-IND              PIC X(1).                           
               88  WS-TRUNCATE            VALUE 'Y'.                            
               88  WS-NO-TRUNCATE         VALUE 'N'.                            
           05  WS-CALLED-MODULES.                                               
               10  MLX2PDRV                 PIC X(8) VALUE 'MLX2PDRV'.          
               10  MLX2PTRN                 PIC X(8) VALUE 'MLX2PTRN'.          
                                                                                
      *----------------------------------------------------------------*        
      *  PARSER INPUT PARAMETERS                                                
      *----------------------------------------------------------------*        
       01  INPUT-PARMS.                                                         
           COPY MLX2INPT.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  PARSER RETURNS                                                         
      *----------------------------------------------------------------*        
       01  PARSER-RETURNS.                                                      
           COPY MLX2RTRN.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  TRANSLATION PARAMETERS.                                                
      *----------------------------------------------------------------*        
       01  TRANSLATE-PARMS.                                                     
           COPY MLX2TRNS.                                                       
                                                                                
       01  FILLER                             PIC X(40) VALUE                   
               '***  MLX2PTAG WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  PARSER PARMS                                                           
      *----------------------------------------------------------------*        
       01  PARSER-INPUTS.                                                       
           COPY MLX2PARS.                                                       
                                                                                
       01  AREA1                               PIC X(1).                        
                                                                                
       01  AREA2                               PIC X(1).                        
                                                                                
       01  AREA3                               PIC X(1).                        
                                                                                
       01  PARSER-OUTPUTS.                                                      
           COPY MLX2OUTP.                                                       
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING PARSER-INPUTS                                   
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
                                                                                
           PERFORM 2000-MAIN THRU                                               
                   2000-MAIN-EXIT.                                              
                                                                                
           PERFORM 3000-FINAL THRU                                              
                   3000-FINAL-EXIT.                                             
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      *    INITIALIZE                                                           
      ****************************************************************          
       1000-INIT.                                                               
           MOVE SPACE                 TO WS-AREA-IND.                           
                                                                                
           MOVE +0                    TO OUTP-TABLE-ENTRIES                     
                                         PTAG-CODE                              
                                         RTRN-AREA3-DATA-LENGTH.                
                                                                                
           INITIALIZE                    OUTP-TABLE.                            
           SET OUTP-NO-ERROR          TO TRUE.                                  
           SET WS-NO-TRUNCATE         TO TRUE.                                  
                                                                                
           MOVE PARS-ACTION           TO INPT-ACTION.                           
           MOVE PARS-SECTION          TO INPT-SECTION.                          
           MOVE PARS-SECTION-OCCUR    TO INPT-SECTION-OCCUR.                    
           MOVE PARS-GRP-LVL-1-ID     TO INPT-GRP-LVL-1-ID.                     
           MOVE PARS-GRP-LVL-1-OCCUR  TO INPT-GRP-LVL-1-OCCUR.                  
           MOVE PARS-GRP-LVL-2-ID     TO INPT-GRP-LVL-2-ID.                     
           MOVE PARS-GRP-LVL-2-OCCUR  TO INPT-GRP-LVL-2-OCCUR.                  
           MOVE PARS-GRP-LVL-3-ID     TO INPT-GRP-LVL-3-ID.                     
           MOVE PARS-GRP-LVL-3-OCCUR  TO INPT-GRP-LVL-3-OCCUR.                  
           MOVE PARS-PACKET-ID        TO INPT-PACKET-ID.                        
           MOVE PARS-PACKET-TYPE      TO INPT-PACKET-TYPE.                      
           MOVE PARS-PACKET-LENGTH    TO INPT-PACKET-LENGTH.                    
           MOVE PARS-SEGMENT-VERSION  TO INPT-SEGMENT-VERSION.                  
           MOVE PARS-INDEX-HANDLE-X   TO INPT-INDEX-HANDLE-X.                   
           MOVE PARS-MAX-LENGTH-AREA1 TO INPT-MAX-LENGTH-AREA1.                 
           MOVE PARS-MAX-LENGTH-AREA2 TO INPT-MAX-LENGTH-AREA2.                 
           MOVE PARS-MAX-LENGTH-AREA3 TO INPT-MAX-LENGTH-AREA3.                 
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    MAINLINE                                                             
      ****************************************************************          
       2000-MAIN.                                                               
           IF INPT-ACTION-PUT                                                   
              PERFORM 4000-TRANSLATE THRU                                       
                      4000-TRANSLATE-EXIT                                       
              IF TRNS-NO-CHG                                                    
                 NEXT SENTENCE                                                  
              ELSE                                                              
                 MOVE TRNS-OUT-TYPE            TO INPT-PACKET-TYPE              
                 MOVE TRNS-OUT-LENGTH          TO INPT-PACKET-LENGTH            
                 MOVE TRNS-OUT-SEGMENT-VERSION TO INPT-SEGMENT-VERSION          
              END-IF                                                            
           END-IF.                                                              
                                                                                
           IF PTAG-CODE NOT = 0                                                 
              GO TO 2000-MAIN-EXIT                                              
           END-IF.                                                              
                                                                                
           IF INPT-ACTION-GET                                                   
           OR TRNS-NO-CHG                                                       
              PERFORM 5000-CALL-PARSER THRU                                     
                      5000-CALL-PARSER-EXIT                                     
           ELSE                                                                 
              PERFORM 5100-CALL-PARSER-2 THRU                                   
                      5100-CALL-PARSER-2-EXIT                                   
           END-IF.                                                              
                                                                                
           IF RTRN-CODE NOT = 0                                                 
              MOVE RTRN-CODE TO PTAG-CODE                                       
              GO TO 2000-MAIN-EXIT                                              
           END-IF.                                                              
                                                                                
           IF INPT-ACTION-GET                                                   
              IF RTRN-PACKET-TYPE         = INPT-PACKET-TYPE                    
              AND RTRN-AREA3-DATA-LENGTH  = INPT-PACKET-LENGTH                  
              AND RTRN-SEGMENT-VERSION    = INPT-SEGMENT-VERSION                
                 MOVE AREA3(1:RTRN-AREA3-DATA-LENGTH) TO                        
                      RTRN-WORK-AREA                                            
              END-IF                                                            
                                                                                
              PERFORM 4000-TRANSLATE THRU                                       
                      4000-TRANSLATE-EXIT                                       
           END-IF.                                                              
                                                                                
       2000-MAIN-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
                                                                                
                                                                                
      ****************************************************************          
      *    SET UP FINAL RETURNS.                                                
      ****************************************************************          
       3000-FINAL.                                                              
           IF PTAG-CODE NOT = 0                                                 
              PERFORM 3020-ERROR-TABLE THRU                                     
                      3020-ERROR-TABLE-EXIT                                     
           END-IF.                                                              
                                                                                
           IF WS-TRUNCATE                                                       
           OR PTAG-NOT-FOUND                                                    
              NEXT SENTENCE                                                     
           ELSE                                                                 
              IF PTAG-CODE NOT = 0                                              
                 GO TO 3000-FINAL-EXIT                                          
              END-IF                                                            
           END-IF.                                                              
                                                                                
           MOVE RTRN-INDEX-HANDLE-X         TO OUTP-HANDLE-X.                   
                                                                                
           IF INPT-ACTION-GET                                                   
              EVALUATE TRUE                                                     
              WHEN TRNS-CHG                                                     
              WHEN TRNS-OUT-TYPE = (21 OR 22)                                   
                 MOVE TRNS-OUT-TYPE            TO OUTP-PACKET-TYPE              
                 MOVE TRNS-OUT-LENGTH          TO OUTP-AREA3-DATA-LENGTH        
                 MOVE TRNS-OUT-SEGMENT-VERSION TO OUTP-SEGMENT-VERSION          
              WHEN OTHER                                                        
                 MOVE INPT-PACKET-TYPE         TO OUTP-PACKET-TYPE              
                 MOVE INPT-PACKET-LENGTH       TO OUTP-AREA3-DATA-LENGTH        
                 MOVE INPT-SEGMENT-VERSION     TO OUTP-SEGMENT-VERSION          
              END-EVALUATE                                                      
                                                                                
              IF PTAG-NOT-FOUND                                                 
                 MOVE 0 TO OUTP-AREA3-DATA-LENGTH                               
              END-IF                                                            
              EVALUATE TRUE                                                     
                 WHEN WS-TRNS-WORK-AREA                                         
                    IF OUTP-AREA3-DATA-LENGTH > INPT-MAX-LENGTH-AREA3           
                       MOVE 45 TO PTAG-CODE                                     
                       PERFORM 3020-ERROR-TABLE THRU                            
                               3020-ERROR-TABLE-EXIT                            
                       GO TO 3000-FINAL-EXIT                                    
                    END-IF                                                      
                    MOVE TRNS-WORK-AREA(1:OUTP-AREA3-DATA-LENGTH)               
                                      TO AREA3(1:OUTP-AREA3-DATA-LENGTH)        
                 WHEN WS-RTRN-WORK-AREA                                         
                    IF OUTP-AREA3-DATA-LENGTH > INPT-MAX-LENGTH-AREA3           
                       MOVE 45 TO PTAG-CODE                                     
                       PERFORM 3020-ERROR-TABLE THRU                            
                               3020-ERROR-TABLE-EXIT                            
                       GO TO 3000-FINAL-EXIT                                    
                    END-IF                                                      
                    MOVE RTRN-WORK-AREA(1:OUTP-AREA3-DATA-LENGTH)               
                                      TO AREA3(1:OUTP-AREA3-DATA-LENGTH)        
              END-EVALUATE                                                      
           ELSE                                                                 
              MOVE 0                           TO OUTP-PACKET-TYPE              
              MOVE RTRN-AREA3-DATA-LENGTH      TO OUTP-AREA3-DATA-LENGTH        
              MOVE RTRN-SEGMENT-VERSION        TO OUTP-SEGMENT-VERSION          
           END-IF.                                                              
                                                                                
                                                                                
           IF PTAG-CODE = 0                                                     
           AND WS-TRUNCATE                                                      
              MOVE 04          TO PTAG-CODE                                     
              PERFORM 3020-ERROR-TABLE THRU                                     
                      3020-ERROR-TABLE-EXIT                                     
           END-IF.                                                              
                                                                                
           MOVE RTRN-TRAILER-OCCUR             TO OUTP-TRAILER-OCCUR.           
                                                                                
       3000-FINAL-EXIT.                                                         
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *    ADD ERROR TO ERROR TABLE                                             
      ****************************************************************          
       3020-ERROR-TABLE.                                                        
           IF OUTP-TABLE-ENTRIES < 10                                           
              ADD +1               TO OUTP-TABLE-ENTRIES                        
           END-IF.                                                              
                                                                                
           MOVE INPT-GRP-LVL-1-ID                                               
                           TO OUTP-LVL1-ID(OUTP-TABLE-ENTRIES).                 
                                                                                
           MOVE INPT-GRP-LVL-1-OCCUR                                            
                           TO OUTP-LVL1-OCCUR(OUTP-TABLE-ENTRIES).              
                                                                                
           MOVE INPT-GRP-LVL-2-ID                                               
                           TO OUTP-LVL2-ID(OUTP-TABLE-ENTRIES).                 
                                                                                
           MOVE INPT-GRP-LVL-2-OCCUR                                            
                           TO OUTP-LVL2-OCCUR(OUTP-TABLE-ENTRIES).              
                                                                                
           MOVE INPT-GRP-LVL-3-ID                                               
                           TO OUTP-LVL3-ID(OUTP-TABLE-ENTRIES).                 
                                                                                
           MOVE INPT-GRP-LVL-3-OCCUR                                            
                           TO OUTP-LVL3-OCCUR(OUTP-TABLE-ENTRIES).              
                                                                                
           MOVE INPT-PACKET-ID                                                  
                           TO OUTP-ERROR-PACKET(OUTP-TABLE-ENTRIES).            
                                                                                
           MOVE PTAG-CODE       TO OUTP-CODE(OUTP-TABLE-ENTRIES).               
                                                                                
           EVALUATE TRUE                                                        
              WHEN PTAG-CRITICAL                                                
                 SET OUTP-CRITICAL-ERROR TO TRUE                                
              WHEN PTAG-NOT-FOUND                                               
                 IF NOT OUTP-CRITICAL-ERROR                                     
                    SET OUTP-NOT-FOUND          TO TRUE                         
                 END-IF                                                         
              WHEN OTHER                                                        
                 IF NOT OUTP-CRITICAL-ERROR                                     
                    SET OUTP-NON-CRITICAL-ERROR TO TRUE                         
                 END-IF                                                         
           END-EVALUATE.                                                        
                                                                                
           MOVE 0 TO PTAG-CODE.                                                 
       3020-ERROR-TABLE-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *    TRANSLATE ROUTINE                                                    
      ****************************************************************          
       4000-TRANSLATE.                                                          
           MOVE INPT-ACTION       TO TRNS-ACTION.                               
           MOVE PARS-CHAR-OPTIONS TO TRNS-OPTIONS.                              
                                                                                
           IF INPT-ACTION-PUT                                                   
              MOVE INPT-PACKET-TYPE     TO TRNS-IN-TYPE                         
              MOVE INPT-PACKET-LENGTH   TO TRNS-IN-LENGTH                       
              MOVE INPT-SEGMENT-VERSION TO TRNS-IN-SEGMENT-VERSION              
              SET WS-AREA2              TO TRUE                                 
              PERFORM 4200-PUT-TRANS-CALL THRU                                  
                      4200-PUT-TRANS-CALL-EXIT                                  
              MOVE TRNS-OUT-LENGTH        TO INPT-PACKET-LENGTH                 
           ELSE                                                                 
              MOVE RTRN-PACKET-TYPE       TO TRNS-IN-TYPE                       
              MOVE RTRN-AREA3-DATA-LENGTH TO TRNS-IN-LENGTH                     
              MOVE RTRN-SEGMENT-VERSION   TO TRNS-IN-SEGMENT-VERSION            
              MOVE INPT-PACKET-TYPE       TO TRNS-OUT-TYPE                      
              MOVE INPT-PACKET-LENGTH     TO TRNS-OUT-LENGTH                    
              MOVE INPT-SEGMENT-VERSION   TO TRNS-OUT-SEGMENT-VERSION           
              SET WS-RTRN-WORK-AREA       TO TRUE                               
              PERFORM 4400-GET-TRANS-CALL THRU                                  
                      4400-GET-TRANS-CALL-EXIT                                  
           END-IF.                                                              
                                                                                
           IF TRNS-RETURN = 04                                                  
              SET WS-TRUNCATE TO TRUE                                           
           ELSE                                                                 
              IF TRNS-RETURN NOT = 0                                            
                 MOVE TRNS-RETURN TO PTAG-CODE                                  
                 GO TO 4000-TRANSLATE-EXIT                                      
              END-IF                                                            
           EXIT.                                                                
                                                                                
           IF TRNS-NO-CHG                                                       
              NEXT SENTENCE                                                     
           ELSE                                                                 
              SET WS-TRNS-WORK-AREA TO TRUE                                     
           END-IF.                                                              
       4000-TRANSLATE-EXIT.                                                     
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CALL TRANSLATE MODULE USING AREA2                                    
      ****************************************************************          
       4200-PUT-TRANS-CALL.                                                     
      *                                                                         
      *   BATCH OR CICS CALL                                                    
      *                                                                         
      *    CALL MLX2PTRN USING TRANSLATE-PARMS                                  
      *                        AREA2                                            
      *                        PTRN-RETURN.                                     
      *                                                                         
           COPY MLX2TRN2.                                                       
                                                                                
       4200-PUT-TRANS-CALL-EXIT.                                                
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CALL TRANSLATE MODULE USING RTRN-AREA                                
      ****************************************************************          
       4400-GET-TRANS-CALL.                                                     
      *                                                                         
      *   BATCH OR CICS CALL                                                    
      *                                                                         
      *    CALL MLX2PTRN USING TRANSLATE-PARMS                                  
      *                        RTRN-WORK-AREA                                   
      *                        PTRN-RETURN.                                     
      *                                                                         
           COPY MLX2TRN1.                                                       
                                                                                
       4400-GET-TRANS-CALL-EXIT.                                                
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    PARSER CALL WITH NORMAL WORKING AREAS                                
      ****************************************************************          
       5000-CALL-PARSER.                                                        
      *    CALL MLX2PDRV USING INPUT-PARMS                                      
      *                        PARSER-RETURNS                                   
      *                        AREA1                                            
      *                        AREA2                                            
      *                        AREA3.                                           
                COPY MLX2CDRV.                                                  
                                                                                
       5000-CALL-PARSER-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    PARSER CALL USING TRANSLATE AREA                                     
      ****************************************************************          
       5100-CALL-PARSER-2.                                                      
           MOVE LENGTH OF TRNS-WORK-AREA TO INPT-MAX-LENGTH-AREA2.              
           MOVE TRNS-OUT-TYPE            TO INPT-PACKET-TYPE.                   
           MOVE TRNS-OUT-LENGTH          TO INPT-PACKET-LENGTH.                 
           MOVE TRNS-OUT-SEGMENT-VERSION TO INPT-SEGMENT-VERSION.               
                                                                                
      *    CALL MLX2PDRV USING INPUT-PARMS                                      
      *                        PARSER-RETURNS                                   
      *                        AREA1                                            
      *                        TRNS-WORK-AREA                                   
      *                        AREA3.                                           
                COPY MLX2DRV2.                                                  
                                                                                
       5100-CALL-PARSER-2-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
