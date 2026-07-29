CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PUT.                                                  
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
      *  THIS MODULE WILL CALL THE PARSER TO DO AN INDIVIDUAL PUT TO            
      *  A MESSAGE FOR VERSION 1 OR VERSION 2.                                  
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  SEP 1998   J KLAPWYK  CREATED                                     
      *  002  AUG 2008   IBM        ENTERPRISE COMPILER UPGRADE                 
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
               '**   MLX2PUT  WORKING STORAGE BEGINS  **'.                      
                                                                                
       01  WS-FIXED.                                                            
           05  WS-VERSION-LENGTH            PIC 9(1) VALUE 6.                   
       01  WS-VARIABLES.                                                        
           05  XXX                          PIC S9(4) COMP.                     
           05  WS-CALL-TYPE                 PIC X(1).                           
               88  WS-CALL-PUT            VALUE 'P'.                            
           05  WS-VERSION-X.                                                    
               10  WS-VERSION               PIC 9(6).                           
                   88  VERSION-1          VALUE 000100 THRU 000199.             
                   88  VERSION-2          VALUE 000200 THRU 000299.             
           05  WS-CALLED-MODULES.                                               
               10  MLX2VER1                 PIC X(8) VALUE 'MLX2VER1'.          
               10  MLX2PTAG                 PIC X(8) VALUE 'MLX2PTAG'.          
                                                                                
      *----------------------------------------------------------------*        
      *  PARSER INPUT PARAMETERS                                                
      *----------------------------------------------------------------*        
       01  INPUT-PARMS.                                                         
           COPY MLX2PARS.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  PARSER RETURNS                                                         
      *----------------------------------------------------------------*        
       01  PARSER-RETURNS.                                                      
           COPY MLX2OUTP.                                                       
                                                                                
       01  FILLER                             PIC X(40) VALUE                   
               '***  MLX2PUT  WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  PARSER PARMS                                                           
      *----------------------------------------------------------------*        
       01  PARSER-INPUTS.                                                       
           COPY MLX2PRSI.                                                       
                                                                                
       01  AREA1                               PIC X(1).                        
                                                                                
       01  AREA2                               PIC X(1).                        
                                                                                
       01  AREA3                               PIC X(1).                        
                                                                                
       01  PARSER-OUTPUTS.                                                      
           COPY MLX2PRSO.                                                       
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
                                                                                
           PERFORM 2000-DETERMINE-VERSION THRU                                  
                   2000-DETERMINE-VERSION-EXIT.                                 
                                                                                
           IF PRSO-CRITICAL-ERROR                                               
              GO TO 0000-MAINLINE-EXIT                                          
           END-IF.                                                              
                                                                                
           PERFORM 3000-PARSER-CALL THRU                                        
                   3000-PARSER-CALL-EXIT.                                       
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      *    INITIALIZE RETURNS.                                                  
      ****************************************************************          
       1000-INIT.                                                               
           INITIALIZE PARSER-OUTPUTS.                                           
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    DETERMINE MESSAGE VERSION                                            
      ****************************************************************          
       2000-DETERMINE-VERSION.                                                  
           MOVE AREA1(1:WS-VERSION-LENGTH)    TO WS-VERSION-X.                  
                                                                                
           IF WS-VERSION-X NOT NUMERIC                                          
              SET PRSO-CRITICAL-ERROR TO TRUE                                   
              MOVE 1 TO PRSO-ERROR-COUNT                                        
              MOVE 65 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)                      
              GO TO 2000-DETERMINE-VERSION-EXIT                                 
           END-IF.                                                              
                                                                                
       2000-DETERMINE-VERSION-EXIT.                                             
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CALL APPROPRIATE PARSER MODULE                                       
      ****************************************************************          
       3000-PARSER-CALL.                                                        
           EVALUATE TRUE                                                        
              WHEN VERSION-1                                                    
                 PERFORM 3200-VERSION-1-CALL THRU                               
                         3200-VERSION-1-CALL-EXIT                               
              WHEN VERSION-2                                                    
                 PERFORM 3400-VERSION-2-CALL THRU                               
                         3400-VERSION-2-CALL-EXIT                               
              WHEN OTHER                                                        
                 SET PRSO-CRITICAL-ERROR TO TRUE                                
                 MOVE 1 TO PRSO-ERROR-COUNT                                     
                 MOVE 66 TO PRSO-ERROR-CODE(PRSO-ERROR-COUNT)                   
                 GO TO 3000-PARSER-CALL-EXIT                                    
           END-EVALUATE.                                                        
       3000-PARSER-CALL-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SETUP PARMS AND CALL VERSION 1 PARSER                                
      ****************************************************************          
       3200-VERSION-1-CALL.                                                     
           SET WS-CALL-PUT  TO TRUE.                                            
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *          CALL MLX2VER1 USING WS-CALL-TYPE                               
      *                              PARSER-INPUTS                              
      *                              AREA1                                      
      *                              AREA2                                      
      *                              AREA3                                      
      *                              PARSER-OUTPUTS.                            
           COPY MLX2CVR1.                                                       
                                                                                
       3200-VERSION-1-CALL-EXIT.                                                
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SETUP PARMS AND CALL VERSION 2 PARSER                                
      ****************************************************************          
       3400-VERSION-2-CALL.                                                     
           SET  PARS-ACTION-PUT         TO TRUE.                                
           MOVE PRSI-STRUCTURE-NAME1    TO PARS-STRUCTURE-NAME.                 
           MOVE PRSI-SECTION            TO PARS-SECTION.                        
           IF PRSI-SECTION-HEADER                                               
           OR PRSI-SECTION-BODY                                                 
              MOVE +1                   TO PARS-SECTION-OCCUR                   
           ELSE                                                                 
              MOVE PRSI-SECTION-OCCUR   TO PARS-SECTION-OCCUR                   
           END-IF.                                                              
           MOVE PRSI-GRP-LVL-1-ID       TO PARS-GRP-LVL-1-ID.                   
           MOVE PRSI-GRP-LVL-1-OCCUR    TO PARS-GRP-LVL-1-OCCUR.                
           MOVE PRSI-GRP-LVL-2-ID       TO PARS-GRP-LVL-2-ID.                   
           MOVE PRSI-GRP-LVL-2-OCCUR    TO PARS-GRP-LVL-2-OCCUR.                
           MOVE PRSI-GRP-LVL-3-ID       TO PARS-GRP-LVL-3-ID.                   
           MOVE PRSI-GRP-LVL-3-OCCUR    TO PARS-GRP-LVL-3-OCCUR.                
           MOVE PRSI-PACKET-ID          TO PARS-PACKET-ID.                      
           MOVE PRSI-PACKET-TYPE        TO PARS-PACKET-TYPE.                    
           MOVE PRSI-PACKET-LENGTH      TO PARS-PACKET-LENGTH.                  
           MOVE PRSI-SEGMENT-VERSION    TO PARS-SEGMENT-VERSION.                
           MOVE PRSI-CHAR-OPTIONS       TO PARS-CHAR-OPTIONS.                   
           MOVE PRSI-RUN-OPTION         TO PARS-RUN-OPTION.                     
           MOVE PRSI-INDEX-HANDLE       TO PARS-INDEX-HANDLE-X.                 
           MOVE PRSI-MAX-LENGTH-AREA1   TO PARS-MAX-LENGTH-AREA1.               
           MOVE PRSI-MAX-LENGTH-AREA2   TO PARS-MAX-LENGTH-AREA2.               
           MOVE PRSI-MAX-LENGTH-AREA3   TO PARS-MAX-LENGTH-AREA3.               
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *          CALL MLX2PTAG USING INPUT-PARMS                                
      *                              AREA1                                      
      *                              AREA2                                      
      *                              AREA3                                      
      *                              PARSER-RETURNS.                            
           COPY MLX2CTAG.                                                       
                                                                                
           EVALUATE TRUE                                                        
              WHEN OUTP-NO-ERROR                                                
                 SET PRSO-NO-ERROR           TO TRUE                            
              WHEN OUTP-NON-CRITICAL-ERROR                                      
                 SET PRSO-NON-CRITICAL-ERROR TO TRUE                            
              WHEN OUTP-NOT-FOUND                                               
                 SET PRSO-NOT-FOUND          TO TRUE                            
              WHEN OTHER                                                        
                 SET PRSO-CRITICAL-ERROR     TO TRUE                            
           END-EVALUATE.                                                        
                                                                                
           MOVE OUTP-TABLE-ENTRIES      TO PRSO-ERROR-COUNT.                    
                                                                                
           PERFORM VARYING XXX FROM 1 BY 1                                      
           UNTIL XXX > OUTP-TABLE-ENTRIES                                       
              MOVE OUTP-LVL1-ID(XXX)      TO PRSO-LVL1-ID(XXX)                  
              MOVE OUTP-LVL1-OCCUR(XXX)   TO PRSO-LVL1-OCCUR(XXX)               
              MOVE OUTP-LVL2-ID(XXX)      TO PRSO-LVL2-ID(XXX)                  
              MOVE OUTP-LVL2-OCCUR(XXX)   TO PRSO-LVL2-OCCUR(XXX)               
              MOVE OUTP-LVL3-ID(XXX)      TO PRSO-LVL3-ID(XXX)                  
              MOVE OUTP-LVL3-OCCUR(XXX)   TO PRSO-LVL3-OCCUR(XXX)               
              MOVE OUTP-ERROR-PACKET(XXX) TO PRSO-ERROR-PACKET(XXX)             
              MOVE OUTP-CODE(XXX)         TO PRSO-ERROR-CODE(XXX)               
           END-PERFORM.                                                         
                                                                                
           MOVE OUTP-AREA3-DATA-LENGTH    TO PRSO-DATA-LENGTH.                  
                                                                                
           MOVE WS-VERSION-X              TO PRSO-MESSAGE-VERSION.              
                                                                                
           IF OUTP-HANDLE-X = LOW-VALUES                                        
              MOVE 0                      TO PRSO-INDEX-HANDLE                  
           ELSE                                                                 
              MOVE OUTP-HANDLE            TO PRSO-INDEX-HANDLE                  
           END-IF.                                                              
       3400-VERSION-2-CALL-EXIT.                                                
           EXIT.                                                                
      /                                                                         
