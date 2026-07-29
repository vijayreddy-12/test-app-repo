CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PBMG.                                                 
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
      *  THIS MODULE WILL TAKE AN INDEX AND A DATA AREA AND REBUILD             
      *  A FULL MESSAGE.                                                        
      *                                                                         
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  SEP 1998   J KLAPWYK  CREATED                                     
      *  002  SEP 2002   TRUDY      PERFORMANCE CHANGES                         
      *  003  JUL 2008   IBM GR     UPGRADED IN ECU PROJECT                     
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
               '**   MLX2PBMG WORKING STORAGE BEGINS  **'.                      
       01  WS-FIXED.                                                            
           05  WS-V2-FIXED-HEADER-LENGTH      PIC S9(4) COMP                    
                                            VALUE +174.                         
       01  WS-VARIABLE.                                                         
           05  WS-INDEX-VALUE                 PIC S9(4) COMP.                   
           05  WS-INPUT-OFFSET                PIC S9(9) COMP.                   
           05  WS-OUTPUT-OFFSET               PIC S9(9) COMP.                   
           05  WS-HOLD-SEGMENT-VER            PIC X(6).                         
           05  WS-HOLD-LENGTH            COMP PIC S9(9).                        
TCB   *    05  WS-HOLD-LENGTH                 PIC 9(9).                         
TCB   *    15  WS-TEMP-LENGTH                 PIC 9(9).                         
           05  WS-TEMP-LENGTH        COMP     PIC S9(9).                        
           05  WS-HOLD-LENGTH-9               PIC 9(9).                         
           05  WS-LOOP-ERROR-IND              PIC X(1).                         
               88  LOOP-ERROR               VALUE 'Y'.                          
               88  NO-LOOP-ERROR            VALUE 'N'.                          
           COPY MLX2WSVR.                                                       
                                                                                
       01  FILLER                             PIC X(40) VALUE                   
               '***  MLX2PBMG WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  INPUT PARAMTERS                                                        
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
       01  PBMG-RETURN                         PIC 9(2).                        
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING INPUT-PARMS                                     
                                PARSER-RETURNS                                  
                                COMM-FIELDS                                     
                                INDEX-DEFN                                      
                                AREA1                                           
                                AREA2                                           
                                AREA3                                           
                                PBMG-RETURN.                                    
      *----------------------------------------------------------------*        
                                                                                
      *                                                                         
       0000-MAINLINE.                                                           
      *    DISPLAY 'MLX2PBMG BEGINS'.                                           
           PERFORM 1000-INIT THRU                                               
                   1000-INIT-EXIT.                                              
                                                                                
           PERFORM 2000-BUILD-MSG THRU                                          
                   2000-BUILD-MSG-EXIT                                          
                    VARYING WS-INDEX-VALUE                                      
                    FROM    +1                                                  
                    BY      +1                                                  
                    UNTIL   WS-INDEX-VALUE > INDX-PACKET-COUNT                  
                    OR      LOOP-ERROR.                                         
                                                                                
                                                                                
           COMPUTE RTRN-AREA3-DATA-LENGTH = WS-OUTPUT-OFFSET - 1.               
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      * INITIALIZATION ROUTINE.                                                 
      ****************************************************************          
       1000-INIT.                                                               
           MOVE 0     TO PBMG-RETURN.                                           
           SET NO-LOOP-ERROR TO TRUE.                                           
                                                                                
           MOVE WS-V2-FIXED-HEADER-LENGTH TO WS-TEMP-LENGTH.                    
           IF WS-TEMP-LENGTH > INPT-MAX-LENGTH-AREA3                            
              MOVE 40          TO PBMG-RETURN                                   
              SET LOOP-ERROR TO TRUE                                            
              GO TO 1000-INIT-EXIT                                              
           END-IF.                                                              
                                                                                
           MOVE AREA1(1:WS-V2-FIXED-HEADER-LENGTH)                              
             TO AREA3(1:WS-V2-FIXED-HEADER-LENGTH).                             
                                                                                
           COMPUTE WS-OUTPUT-OFFSET = WS-V2-FIXED-HEADER-LENGTH + 1.            
                                                                                
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * BUILD MESSAGE                                                           
      ****************************************************************          
       2000-BUILD-MSG.                                                          
           SET INDX-XXX TO WS-INDEX-VALUE.                                      
                                                                                
           MOVE INDX-PACKET-OFFSET(INDX-XXX) TO WS-INPUT-OFFSET.                
                                                                                
      ****************************************************************          
      * DETERMINE TYPE                                                          
      ****************************************************************          
           IF INDX-AREA1(INDX-XXX)                                              
              MOVE AREA1(WS-INPUT-OFFSET:6)   TO WS-HOLD-PACKET-TYPE            
              ADD 6 TO WS-INPUT-OFFSET                                          
           ELSE                                                                 
              IF INDX-WORK(INDX-XXX)                                            
                 MOVE COMM-WORK-AREA(WS-INPUT-OFFSET:6)                         
                                              TO WS-HOLD-PACKET-TYPE            
                 ADD 6 TO WS-INPUT-OFFSET                                       
              ELSE                                                              
                 MOVE INPT-PACKET-ID          TO WS-HOLD-PACKET                 
                 MOVE INPT-PACKET-TYPE        TO WS-HOLD-TYPE                   
              END-IF                                                            
           END-IF.                                                              
                                                                                
           IF P-SPECIAL-VALUE                                                   
              MOVE WS-HOLD-PACKET TO WS-PACKET-TYPE                             
              MOVE 0              TO WS-HOLD-TYPE                               
           ELSE                                                                 
              IF T-SPECIAL-VALUE                                                
                 MOVE WS-HOLD-TYPE TO WS-PACKET-TYPE                            
              ELSE                                                              
                 SET FIELD-LEVEL   TO TRUE                                      
              END-IF                                                            
           END-IF.                                                              
                                                                                
      ****************************************************************          
      * DETERMINE LENGTH                                                        
      ****************************************************************          
           EVALUATE TRUE                                                        
              WHEN FIELD-LEVEL                                                  
              WHEN GROUP-START                                                  
                   IF INDX-AREA1(INDX-XXX)                                      
                      MOVE AREA1(WS-INPUT-OFFSET:3) TO WS-HOLD-LENGTH           
                      ADD 3 TO WS-INPUT-OFFSET                                  
                   ELSE                                                         
                      IF INDX-WORK(INDX-XXX)                                    
                         MOVE COMM-WORK-AREA(WS-INPUT-OFFSET:3)                 
                                                    TO WS-HOLD-LENGTH           
                         ADD 3 TO WS-INPUT-OFFSET                               
                      ELSE                                                      
                         MOVE INPT-PACKET-LENGTH    TO WS-HOLD-LENGTH           
                      END-IF                                                    
                   END-IF                                                       
              WHEN PREDEF-SEGMENT                                               
                   IF INDX-AREA1(INDX-XXX)                                      
                      MOVE AREA1(WS-INPUT-OFFSET:6)                             
                        TO WS-HOLD-SEGMENT-VER                                  
                      ADD 6 TO WS-INPUT-OFFSET                                  
                      MOVE AREA1(WS-INPUT-OFFSET:9)                             
                        TO WS-HOLD-LENGTH                                       
                      ADD 9 TO WS-INPUT-OFFSET                                  
                   ELSE                                                         
                      IF INDX-WORK(INDX-XXX)                                    
                         MOVE COMM-WORK-AREA(WS-INPUT-OFFSET:6)                 
                           TO WS-HOLD-SEGMENT-VER                               
                         ADD 6 TO WS-INPUT-OFFSET                               
                         MOVE COMM-WORK-AREA(WS-INPUT-OFFSET:6)                 
                           TO WS-HOLD-LENGTH                                    
                         ADD 9 TO WS-INPUT-OFFSET                               
                      ELSE                                                      
                         MOVE INPT-SEGMENT-VERSION                              
                           TO WS-HOLD-SEGMENT-VER                               
                         MOVE INPT-PACKET-LENGTH                                
                           TO WS-HOLD-LENGTH                                    
                      END-IF                                                    
                   END-IF                                                       
           END-EVALUATE.                                                        
                                                                                
           COMPUTE WS-TEMP-LENGTH = WS-OUTPUT-OFFSET + 6.                       
           IF WS-TEMP-LENGTH > INPT-MAX-LENGTH-AREA3                            
              MOVE 40          TO PBMG-RETURN                                   
              SET LOOP-ERROR TO TRUE                                            
              GO TO 2000-BUILD-MSG-EXIT                                         
           END-IF.                                                              
                                                                                
           MOVE WS-HOLD-PACKET TO AREA3(WS-OUTPUT-OFFSET:4).                    
           ADD 4 TO WS-OUTPUT-OFFSET.                                           
                                                                                
           IF NOT P-SPECIAL-VALUE                                               
               PERFORM 2300-CREATE-TARGET THRU                                  
                       2300-CREATE-TARGET-EXIT                                  
           END-IF.                                                              
                                                                                
       2000-BUILD-MSG-EXIT.                                                     
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * MOVE INPUT PACKET TO TARGET                                             
      ****************************************************************          
       2300-CREATE-TARGET.                                                      
                                                                                
           MOVE WS-HOLD-TYPE TO AREA3(WS-OUTPUT-OFFSET:2).                      
           ADD 2 TO WS-OUTPUT-OFFSET.                                           
                                                                                
           IF PREDEF-SEGMENT                                                    
              COMPUTE WS-TEMP-LENGTH = WS-OUTPUT-OFFSET + 15                    
              IF WS-TEMP-LENGTH > INPT-MAX-LENGTH-AREA3                         
                 MOVE 40          TO PBMG-RETURN                                
                 SET LOOP-ERROR TO TRUE                                         
                 GO TO 2300-CREATE-TARGET-EXIT                                  
              END-IF                                                            
                                                                                
              MOVE WS-HOLD-SEGMENT-VER TO AREA3(WS-OUTPUT-OFFSET:6)             
              ADD 6 TO WS-OUTPUT-OFFSET                                         
                                                                                
              MOVE WS-HOLD-LENGTH      TO AREA3(WS-OUTPUT-OFFSET:9)             
              ADD 9 TO WS-OUTPUT-OFFSET                                         
           ELSE                                                                 
              COMPUTE WS-TEMP-LENGTH = WS-OUTPUT-OFFSET + 3                     
              IF WS-TEMP-LENGTH > INPT-MAX-LENGTH-AREA3                         
                 MOVE 40          TO PBMG-RETURN                                
                 SET LOOP-ERROR TO TRUE                                         
                 GO TO 2300-CREATE-TARGET-EXIT                                  
              END-IF                                                            
                                                                                
              MOVE WS-HOLD-LENGTH TO WS-HOLD-LENGTH-9                           
              MOVE WS-HOLD-LENGTH-9(7:3) TO AREA3(WS-OUTPUT-OFFSET:3)           
              ADD 3 TO WS-OUTPUT-OFFSET                                         
           END-IF.                                                              
                                                                                
           COMPUTE WS-TEMP-LENGTH = WS-OUTPUT-OFFSET + WS-HOLD-LENGTH.          
           IF WS-TEMP-LENGTH > INPT-MAX-LENGTH-AREA3                            
              MOVE 40          TO PBMG-RETURN                                   
              SET LOOP-ERROR TO TRUE                                            
              GO TO 2300-CREATE-TARGET-EXIT                                     
           END-IF.                                                              
                                                                                
           IF INDX-AREA1(INDX-XXX)                                              
              MOVE AREA1(WS-INPUT-OFFSET:WS-HOLD-LENGTH)                        
                TO AREA3(WS-OUTPUT-OFFSET:WS-HOLD-LENGTH)                       
           ELSE                                                                 
              IF INDX-WORK(INDX-XXX)                                            
                 MOVE COMM-WORK-AREA(WS-INPUT-OFFSET:WS-HOLD-LENGTH)            
                   TO AREA3(WS-OUTPUT-OFFSET:WS-HOLD-LENGTH)                    
              ELSE                                                              
                 MOVE AREA2(WS-INPUT-OFFSET:WS-HOLD-LENGTH)                     
                   TO AREA3(WS-OUTPUT-OFFSET:WS-HOLD-LENGTH)                    
              END-IF                                                            
           END-IF.                                                              
                                                                                
           ADD WS-HOLD-LENGTH TO WS-INPUT-OFFSET                                
                                 WS-OUTPUT-OFFSET.                              
       2300-CREATE-TARGET-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
