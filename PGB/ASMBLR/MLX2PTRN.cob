CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PTRN.                                                 
      *              PROGRAM CONVERTED BY                                       
      *              CCCA FOR OS/390 & MVS & VM 5648-B05                        
      *              CONVERSION DATE 07/24/08 07:28:31.                         
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
      *  THIS MODULE WILL TRANSLATE DATA BETWEEN TYPES, AS WELL AS              
      *  ADJUST LENGTHS AND VALUES.                                             
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  SEP 1998   J KLAPWYK  CREATED                                     
      *  002  APR 1999   J KLAPWYK  UPDATED TO ALLOW PREDEF. SEG. WITH          
      *                                 DIFFERENT LENGTHS                       
      *  003  FEB 2009   M COOPER   CHANGED DATA VALIDATION LOGIC TO            
      *                                 CHECK FOR ZERO LENGTH                   
      *                                 COMPARISON TO CORRECT AN                
      *                                 INVALID WARNING CAUSED BY THE           
      *                                 SWITCH TO THE LE COMPILERS              
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
               '**   MLX2PTRN WORKING STORAGE BEGINS  **'.                      
                                                                                
       01  WS-VARIABLES.                                                        
           05  XXX                            PIC S9(4) COMP.                   
           05  WS-WHOLE-OFFSET                PIC S9(4) COMP.                   
           05  WS-WHOLE-LENGTH                PIC S9(4) COMP.                   
           05  WS-DECIMAL-OFFSET              PIC S9(4) COMP.                   
           05  WS-DECIMAL-LENGTH              PIC S9(4) COMP.                   
           05  WS-DECIMALS                    PIC S9(4) COMP.                   
           05  WS-OFFSET                      PIC S9(4) COMP.                   
           05  WS-LENGTH                      PIC S9(4) COMP.                   
           05  WS-HOLD-OFFSET                 PIC S9(4) COMP.                   
           05  WS-NUMERIC                     PIC 9(18).                        
           05  WS-WHOLE-X.                                                      
               10  WS-WHOLE                   PIC 9(18).                        
           05  WS-FRACTION-X.                                                   
               10  WS-FRACTION                PIC 9(18).                        
           05  WS-COMP-X.                                                       
               10  WS-COMP                    PIC S9(18) COMP.                  
           05  WS-COMP-3-X.                                                     
               10  WS-COMP-3                  PIC S9(17) COMP-3.                
           05  WS-CYYMMDD-X.                                                    
               10  WS-CYYMMDD                 PIC S9(7) COMP-3.                 
           05  WS-SIGN                        PIC X(1).                         
               88  WS-POSITIVE              VALUE '+'.                          
               88  WS-NEGATIVE              VALUE '-'.                          
           05  WS-TYPES.                                                        
               10  WS-IN-TYPE                 PIC 9(2).                         
                   88  WS-IN-PACKED         VALUES 41 THRU 42                   
                                                   51 THRU 58.                  
                   88  WS-IN-COMP           VALUES 41 THRU 42.                  
                   88  WS-IN-COMP-3         VALUES 51 THRU 58.                  
                   88  WS-IN-DATE           VALUES 12 THRU 17 32.               
                   88  WS-IN-YYYYMMDD       VALUE  12.                          
                   88  WS-IN-CYYMMDD-P      VALUE  32.                          
                   88  WS-IN-NUMERIC        VALUES 01 THRU 08                   
                                                   41 THRU 42                   
                                                   51 THRU 58.                  
                   88  WS-IN-ALPHA          VALUES 09 THRU 10.                  
                   88  WS-IN-SEGMENT        VALUES 21 THRU 22.                  
               10  WS-OUT-TYPE                PIC 9(2).                         
                   88  WS-OUT-PACKED        VALUES 41 THRU 42                   
                                                   51 THRU 58.                  
                   88  WS-OUT-COMP          VALUES 41 THRU 42.                  
                   88  WS-OUT-COMP-3        VALUES 51 THRU 58.                  
                   88  WS-OUT-DATE          VALUES 12 THRU 17 32.               
                   88  WS-OUT-YYYYMMDD      VALUE  12.                          
                   88  WS-OUT-CYYMMDD-P     VALUE  32.                          
                   88  WS-OUT-NUMERIC       VALUES 01 THRU 08                   
                                                   41 THRU 42                   
                                                   51 THRU 58.                  
                   88  WS-OUT-ALPHA         VALUES 09 THRU 10.                  
                   88  WS-OUT-ALPHANUM      VALUE  10.                          
                   88  WS-OUT-SEGMENT       VALUES 21 THRU 22.                  
           05  WS-FINISHED-IND                PIC X(1).                         
               88  WS-FINISHED              VALUE 'Y'.                          
               88  WS-NOT-FINISHED          VALUE 'N'.                          
           05  WS-CALLED-MODULES.                                               
               10  GC2DATE                    PIC X(8) VALUE 'GC2DATE'.         
               10  CGC2DATE                   PIC X(8) VALUE 'CGC2DATE'.        
      *-----------------------------------------------------------------        
      * ACCENTS TO NO-ACCENT CONVERSION.                                        
      *-----------------------------------------------------------------        
           COPY MLXXSETA.                                                       
      /                                                                         
      *-----------------------------------------------------------------        
      * LOWERCASE TO UPPERCASE WITH ACCENT TO NO-ACCENT CONVERSION.             
      *-----------------------------------------------------------------        
           COPY MLXXSETB.                                                       
      /                                                                         
      *-----------------------------------------------------------------        
      * GC2DATE PARAMETERS.                                                     
      *-----------------------------------------------------------------        
       01  GAC-DATE-PARAMETERS.                                                 
           COPY GARDATEP.                                                       
      /                                                                         
       01  FILLER                             PIC X(40) VALUE                   
               '***  MLX2PTRN WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  TRANSLATE-PARMS.                                                     
           COPY MLX2TRNS.                                                       
                                                                                
       01  INPUT-VALUE                          PIC X(1).                       
                                                                                
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING TRANSLATE-PARMS                                 
                                INPUT-VALUE.                                    
      *----------------------------------------------------------------*        
      ****************************************************************          
      *    ADD ITEM TO END OF INDEX                                             
      ****************************************************************          
       0000-MAINLINE.                                                           
           MOVE TRNS-IN-TYPE    TO WS-IN-TYPE.                                  
                                                                                
           PERFORM 3000-CHAR-SUBSTITUTION THRU                                  
                   3000-CHAR-SUBSTITUTION-EXIT.                                 
                                                                                
           PERFORM 1000-INIT THRU                                               
                   1000-INIT-EXIT.                                              
                                                                                
           IF WS-FINISHED                                                       
              GO TO 0000-MAINLINE-EXIT                                          
           END-IF.                                                              
                                                                                
           PERFORM 2000-CAST THRU                                               
                   2000-CAST-EXIT.                                              
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      *    INITIALIZE VALUES                                                    
      ****************************************************************          
       1000-INIT.                                                               
           MOVE +0             TO TRNS-RETURN.                                  
           SET TRNS-CHG        TO TRUE.                                         
           SET WS-NOT-FINISHED TO TRUE.                                         
           SET WS-POSITIVE     TO TRUE.                                         
                                                                                
           IF NOT TRNS-ACTION-PUT                                               
           AND TRNS-IN-TYPE  = TRNS-OUT-TYPE                                    
           AND TRNS-IN-LENGTH = TRNS-OUT-LENGTH                                 
           AND TRNS-IN-SEGMENT-VERSION = TRNS-OUT-SEGMENT-VERSION               
              SET WS-FINISHED   TO TRUE                                         
              SET TRNS-NO-CHG   TO TRUE                                         
              GO TO 1000-INIT-EXIT                                              
           END-IF.                                                              
                                                                                
           IF NOT TRNS-ACTION-PUT                                               
           AND WS-IN-SEGMENT                                                    
           AND TRNS-IN-TYPE = TRNS-OUT-TYPE                                     
           AND TRNS-IN-SEGMENT-VERSION = TRNS-OUT-SEGMENT-VERSION               
              EVALUATE TRUE                                                     
                 WHEN TRNS-IN-LENGTH < TRNS-OUT-LENGTH                          
                    MOVE 05             TO TRNS-RETURN                          
                    MOVE TRNS-IN-LENGTH TO TRNS-OUT-LENGTH                      
                    SET WS-FINISHED     TO TRUE                                 
                    SET TRNS-NO-CHG     TO TRUE                                 
                    GO TO 1000-INIT-EXIT                                        
                 WHEN TRNS-IN-LENGTH > TRNS-OUT-LENGTH                          
                    MOVE 04             TO TRNS-RETURN                          
                    SET WS-FINISHED     TO TRUE                                 
                    SET TRNS-NO-CHG     TO TRUE                                 
                    GO TO 1000-INIT-EXIT                                        
              END-EVALUATE                                                      
           END-IF.                                                              
                                                                                
                                                                                
           IF TRNS-ACTION-PUT                                                   
              PERFORM 1100-SET-DISPLAY-OUTPUT THRU                              
                      1100-SET-DISPLAY-OUTPUT-EXIT                              
           END-IF.                                                              
                                                                                
           MOVE TRNS-OUT-TYPE   TO WS-OUT-TYPE.                                 
                                                                                
           IF NOT TRNS-ACTION-PUT                                               
           AND (WS-IN-SEGMENT OR WS-OUT-SEGMENT)                                
           AND TRNS-IN-SEGMENT-VERSION NOT = TRNS-OUT-SEGMENT-VERSION           
              SET WS-FINISHED TO TRUE                                           
              MOVE 99 TO TRNS-RETURN                                            
              GO TO 1000-INIT-EXIT                                              
           END-IF.                                                              
                                                                                
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET OUTPUT TYPE TO DISPLAY VERSION OF INPUT TYPE                     
      ****************************************************************          
       1100-SET-DISPLAY-OUTPUT.                                                 
           EVALUATE TRNS-IN-TYPE                                                
              WHEN 1 THRU 17                                                    
                 MOVE TRNS-IN-TYPE TO TRNS-OUT-TYPE                             
              WHEN 21 THRU 22                                                   
                 MOVE TRNS-IN-TYPE   TO TRNS-OUT-TYPE                           
                 MOVE TRNS-IN-LENGTH TO TRNS-OUT-LENGTH                         
                 MOVE TRNS-IN-SEGMENT-VERSION                                   
                                     TO TRNS-OUT-SEGMENT-VERSION                
                 SET WS-FINISHED     TO TRUE                                    
                 SET TRNS-NO-CHG     TO TRUE                                    
                 GO TO 1100-SET-DISPLAY-OUTPUT-EXIT                             
              WHEN 32                                                           
                 MOVE 12 TO TRNS-OUT-TYPE                                       
              WHEN 41 THRU 42                                                   
                 COMPUTE TRNS-OUT-TYPE = TRNS-IN-TYPE - 40                      
              WHEN 51 THRU 58                                                   
                 COMPUTE TRNS-OUT-TYPE = TRNS-IN-TYPE - 50                      
              WHEN OTHER                                                        
                 MOVE 99 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
                 GO TO 1100-SET-DISPLAY-OUTPUT-EXIT                             
           END-EVALUATE.                                                        
       1100-SET-DISPLAY-OUTPUT-EXIT.                                            
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CAST FROM ONE TYPE TO ANOTHER                                        
      ****************************************************************          
       2000-CAST.                                                               
           EVALUATE TRUE                                                        
              WHEN WS-IN-DATE                                                   
                 PERFORM 2200-DATE-CAST THRU                                    
                         2200-DATE-CAST-EXIT                                    
              WHEN WS-IN-NUMERIC                                                
                 PERFORM 2400-NUMERIC-CAST THRU                                 
                         2400-NUMERIC-CAST-EXIT                                 
              WHEN WS-IN-ALPHA                                                  
                 PERFORM 2600-ALPHA-CAST THRU                                   
                         2600-ALPHA-CAST-EXIT                                   
              WHEN OTHER                                                        
                 MOVE 90 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
                 GO TO 2000-CAST-EXIT                                           
           END-EVALUATE.                                                        
       2000-CAST-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CAST FROM DATE TYPE                                                  
      ****************************************************************          
       2200-DATE-CAST.                                                          
           IF INPUT-VALUE(1:TRNS-IN-LENGTH) = SPACE                             
           OR INPUT-VALUE(1:TRNS-IN-LENGTH) = ZERO                              
              MOVE 07 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 2200-DATE-CAST-EXIT                                         
           END-IF.                                                              
                                                                                
           IF NOT WS-IN-CYYMMDD-P                                               
           AND INPUT-VALUE(1:TRNS-IN-LENGTH) NOT NUMERIC                        
              MOVE 06 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 2200-DATE-CAST-EXIT                                         
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
              WHEN WS-OUT-ALPHANUM                                              
                 IF TRNS-IN-LENGTH > LENGTH OF TRNS-WORK-AREA                   
                    MOVE 95 TO TRNS-RETURN                                      
                    SET WS-FINISHED TO TRUE                                     
                    GO TO 2200-DATE-CAST-EXIT                                   
                 END-IF                                                         
                 MOVE SPACE TO TRNS-WORK-AREA                                   
                 MOVE INPUT-VALUE(1:TRNS-IN-LENGTH)                             
                      TO TRNS-WORK-AREA(1:TRNS-OUT-LENGTH)                      
                 IF TRNS-IN-LENGTH > TRNS-OUT-LENGTH                            
                    MOVE 04 TO TRNS-RETURN                                      
                 END-IF                                                         
              WHEN WS-IN-TYPE = WS-OUT-TYPE                                     
                 SET WS-FINISHED     TO TRUE                                    
                 SET TRNS-NO-CHG     TO TRUE                                    
                 MOVE TRNS-IN-LENGTH TO TRNS-OUT-LENGTH                         
                 GO TO 2200-DATE-CAST-EXIT                                      
              WHEN WS-IN-CYYMMDD-P                                              
               AND WS-OUT-YYYYMMDD                                              
                 IF TRNS-IN-LENGTH NOT = 4                                      
                    MOVE 92 TO TRNS-RETURN                                      
                    SET WS-FINISHED TO TRUE                                     
                    GO TO 2200-DATE-CAST-EXIT                                   
                 END-IF                                                         
                 IF TRNS-ACTION-PUT                                             
                    MOVE 8 TO TRNS-OUT-LENGTH                                   
                 END-IF                                                         
                 PERFORM 2240-GC2DATE THRU                                      
                         2240-GC2DATE-EXIT                                      
              WHEN WS-IN-YYYYMMDD                                               
               AND WS-OUT-CYYMMDD-P                                             
                 IF TRNS-IN-LENGTH NOT = 8                                      
                    MOVE 92 TO TRNS-RETURN                                      
                    SET WS-FINISHED TO TRUE                                     
                    GO TO 2200-DATE-CAST-EXIT                                   
                 END-IF                                                         
                 IF TRNS-ACTION-PUT                                             
                    MOVE 4 TO TRNS-OUT-LENGTH                                   
                 END-IF                                                         
                 PERFORM 2240-GC2DATE THRU                                      
                         2240-GC2DATE-EXIT                                      
              WHEN OTHER                                                        
                 MOVE 99 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
                 GO TO 2200-DATE-CAST-EXIT                                      
           END-EVALUATE.                                                        
                                                                                
       2200-DATE-CAST-EXIT.                                                     
           EXIT.                                                                
     /                                                                          
      ****************************************************************          
      *    DATE UTILITY CALL                                                    
      ****************************************************************          
       2240-GC2DATE.                                                            
           MOVE 'A' TO VDATE-REQ-BASIS                                          
           MOVE 'E' TO VDATE-REQ-LANGUAGE                                       
           MOVE '1' TO VDATE-REQ-DETAIL                                         
                                                                                
           EVALUATE TRUE                                                        
               WHEN WS-IN-YYYYMMDD                                              
                  MOVE 'B'                       TO VDATE-REQ-SERVICE           
                  MOVE 8 TO WS-LENGTH                                           
                  MOVE INPUT-VALUE(1:WS-LENGTH)  TO VDATE1-YYYYMMDD             
               WHEN WS-IN-CYYMMDD-P                                             
                  MOVE 'C'                       TO VDATE-REQ-SERVICE           
                  MOVE 4 TO WS-LENGTH                                           
                  MOVE INPUT-VALUE(1:WS-LENGTH)  TO WS-CYYMMDD-X                
                  MOVE WS-CYYMMDD                TO VDATE-CII-DATE              
           END-EVALUATE.                                                        
                                                                                
           COPY MLPGC2D.                                                        
                                                                                
           IF VDATE-RET-FAIL                                                    
              MOVE 90 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 2240-GC2DATE-EXIT                                           
           END-IF.                                                              
                                                                                
            EVALUATE TRUE                                                       
                WHEN WS-OUT-YYYYMMDD                                            
                   MOVE VDATE1-YYYYMMDD TO TRNS-WORK-AREA(1:8)                  
                   IF TRNS-OUT-LENGTH NOT = 8                                   
                      MOVE 95 TO TRNS-RETURN                                    
                      SET WS-FINISHED TO TRUE                                   
                      GO TO 2240-GC2DATE-EXIT                                   
                   END-IF                                                       
                WHEN WS-OUT-CYYMMDD-P                                           
                   MOVE VDATE-CII-DATE  TO WS-CYYMMDD                           
                   MOVE WS-CYYMMDD-X    TO TRNS-WORK-AREA(1:4)                  
                   IF TRNS-OUT-LENGTH NOT = 4                                   
                      MOVE 95 TO TRNS-RETURN                                    
                      SET WS-FINISHED TO TRUE                                   
                      GO TO 2240-GC2DATE-EXIT                                   
                   END-IF                                                       
            END-EVALUATE.                                                       
                                                                                
       2240-GC2DATE-EXIT.                                                       
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CAST FROM NUMERIC TYPE                                               
      ****************************************************************          
       2400-NUMERIC-CAST.                                                       
           PERFORM 4000-DECOMPOSE THRU                                          
                   4000-DECOMPOSE-EXIT.                                         
                                                                                
           IF WS-FINISHED                                                       
              GO TO 2400-NUMERIC-CAST-EXIT                                      
           END-IF.                                                              
                                                                                
           IF TRNS-ACTION-PUT                                                   
              PERFORM 2450-CALC-NUMERIC-LENGTH THRU                             
                      2450-CALC-NUMERIC-LENGTH-EXIT                             
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
              WHEN WS-OUT-NUMERIC                                               
                 PERFORM 5000-RECOMPOSE-NUM THRU                                
                         5000-RECOMPOSE-NUM-EXIT                                
              WHEN WS-OUT-ALPHANUM                                              
                 PERFORM 6000-RECOMPOSE-ALPHA THRU                              
                         6000-RECOMPOSE-ALPHA-EXIT                              
              WHEN OTHER                                                        
                 MOVE 99 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
                 GO TO 2400-NUMERIC-CAST-EXIT                                   
           END-EVALUATE.                                                        
                                                                                
       2400-NUMERIC-CAST-EXIT.                                                  
           EXIT.                                                                
     /                                                                          
      ****************************************************************          
      *    FOR PUT CALL, CALCULATE MINIMUM OUTPUT LENGTH                        
      ****************************************************************          
       2450-CALC-NUMERIC-LENGTH.                                                
           EVALUATE WS-OUT-TYPE                                                 
              WHEN 41 THRU 42                                                   
              WHEN 51 THRU 52                                                   
                 MOVE 0 TO WS-DECIMALS                                          
              WHEN 53                                                           
                 MOVE 2 TO WS-DECIMALS                                          
              WHEN 54                                                           
                 MOVE 1 TO WS-DECIMALS                                          
              WHEN 55                                                           
                 MOVE 3 TO WS-DECIMALS                                          
              WHEN 56                                                           
                 MOVE 4 TO WS-DECIMALS                                          
              WHEN 57                                                           
                 MOVE 5 TO WS-DECIMALS                                          
              WHEN 58                                                           
                 MOVE 6 TO WS-DECIMALS                                          
           END-EVALUATE.                                                        
                                                                                
           PERFORM VARYING XXX FROM 1 BY 1                                      
           UNTIL XXX > LENGTH OF WS-WHOLE                                       
           OR WS-WHOLE(XXX:1) NOT = '0'                                         
           END-PERFORM.                                                         
                                                                                
           IF XXX > LENGTH OF WS-WHOLE                                          
              MOVE WS-DECIMALS TO TRNS-OUT-LENGTH                               
           ELSE                                                                 
              COMPUTE TRNS-OUT-LENGTH = LENGTH OF WS-WHOLE - XXX + 1            
                                        + WS-DECIMALS                           
           END-IF.                                                              
                                                                                
           IF WS-NEGATIVE                                                       
              ADD 1 TO TRNS-OUT-LENGTH                                          
           END-IF.                                                              
                                                                                
       2450-CALC-NUMERIC-LENGTH-EXIT.                                           
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CAST FROM ALPHA TYPE                                                 
      ****************************************************************          
       2600-ALPHA-CAST.                                                         
           IF WS-OUT-NUMERIC                                                    
              IF  INPUT-VALUE(1:1) NOT = '+'                                    
              AND INPUT-VALUE(1:1) NOT = '-'                                    
              AND INPUT-VALUE(1:1) NOT = ' '                                    
              AND INPUT-VALUE(1:1) NOT NUMERIC                                  
                 MOVE 99 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
              ELSE                                                              
                 PERFORM 2400-NUMERIC-CAST                                      
                    THRU 2400-NUMERIC-CAST-EXIT                                 
              END-IF                                                            
                                                                                
              GO TO 2600-ALPHA-CAST-EXIT                                        
           END-IF.                                                              
                                                                                
           IF NOT WS-OUT-ALPHA                                                  
              MOVE 99 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 2600-ALPHA-CAST-EXIT                                        
           END-IF.                                                              
                                                                                
           IF TRNS-ACTION-PUT                                                   
              PERFORM VARYING XXX FROM TRNS-IN-LENGTH BY -1                     
              UNTIL XXX < 1                                                     
              OR INPUT-VALUE(XXX:1) NOT = SPACE                                 
              END-PERFORM                                                       
              MOVE XXX TO TRNS-OUT-LENGTH                                       
              SET TRNS-NO-CHG TO TRUE                                           
              GO TO 2600-ALPHA-CAST-EXIT                                        
           END-IF.                                                              
                                                                                
           IF TRNS-OUT-LENGTH < TRNS-IN-LENGTH                                  
              COMPUTE WS-OFFSET = TRNS-OUT-LENGTH + 1                           
              PERFORM VARYING XXX FROM WS-OFFSET BY 1                           
              UNTIL XXX > TRNS-IN-LENGTH                                        
              OR INPUT-VALUE(XXX:1) NOT = SPACE                                 
              END-PERFORM                                                       
              IF XXX NOT > TRNS-IN-LENGTH                                       
                 MOVE 04 TO TRNS-RETURN                                         
                 MOVE SPACE TO TRNS-WORK-AREA                                   
                 MOVE INPUT-VALUE(1:TRNS-OUT-LENGTH)                            
                           TO TRNS-WORK-AREA(1:TRNS-OUT-LENGTH)                 
              ELSE                                                              
                 SET TRNS-NO-CHG TO TRUE                                        
              END-IF                                                            
           ELSE                                                                 
              IF TRNS-OUT-LENGTH > LENGTH OF TRNS-WORK-AREA                     
                 MOVE 95 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
                 GO TO 2600-ALPHA-CAST-EXIT                                     
              END-IF                                                            
              MOVE SPACE TO TRNS-WORK-AREA                                      
              MOVE INPUT-VALUE(1:TRNS-IN-LENGTH)                                
                        TO TRNS-WORK-AREA(1:TRNS-IN-LENGTH)                     
           END-IF.                                                              
                                                                                
       2600-ALPHA-CAST-EXIT.                                                    
           EXIT.                                                                
     /                                                                          
      ****************************************************************          
      *    DO ANY CHARACTER SUBSTITUTIONS                                       
      ****************************************************************          
       3000-CHAR-SUBSTITUTION.                                                  
            IF NOT WS-IN-ALPHA                                                  
               GO TO 3000-CHAR-SUBSTITUTION-EXIT                                
            END-IF.                                                             
                                                                                
            EVALUATE TRUE                                                       
               WHEN TRNS-CONVERT-ACCENTS-AND-LOWER                              
                  INSPECT INPUT-VALUE(1:TRNS-IN-LENGTH)                         
                          CONVERTING SET-A-CHARS-IN                             
                          TO         SET-A-CHARS-OUT                            
               WHEN TRNS-CONVERT-ACCENTS                                        
                  INSPECT INPUT-VALUE(1:TRNS-IN-LENGTH)                         
                          CONVERTING SET-B-CHARS-IN                             
                          TO         SET-B-CHARS-OUT                            
           END-EVALUATE.                                                        
                                                                                
       3000-CHAR-SUBSTITUTION-EXIT.                                             
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    DECOMPOSE NUMERIC INPUT INTO WHOLE AND DECIMAL DIGITS                
      ****************************************************************          
       4000-DECOMPOSE.                                                          
           EVALUATE TRUE                                                        
              WHEN WS-IN-PACKED                                                 
                 PERFORM 4100-UNPACK THRU                                       
                         4100-UNPACK-EXIT                                       
              WHEN WS-IN-ALPHA                                                  
                 PERFORM 4040-ALPHA-PROCESS                                     
                    THRU 4040-ALPHA-PROCESS-EXIT                                
              WHEN OTHER                                                        
                 PERFORM 4020-NUMERIC-PROCESS                                   
                    THRU 4020-NUMERIC-PROCESS-EXIT                              
           END-EVALUATE.                                                        
                                                                                
           IF WS-FINISHED                                                       
              GO TO 4000-DECOMPOSE-EXIT                                         
           END-IF.                                                              
                                                                                
           PERFORM 4200-SEPARATE THRU                                           
                   4200-SEPARATE-EXIT.                                          
                                                                                
       4000-DECOMPOSE-EXIT.                                                     
           EXIT.                                                                
     /                                                                          
      ****************************************************************          
      *    MOVE NUMERIC VALUES TO NUMERIC FIELD                                 
      ****************************************************************          
       4020-NUMERIC-PROCESS.                                                    
           IF TRNS-IN-LENGTH > 999                                              
              MOVE 95 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 4020-NUMERIC-PROCESS-EXIT                                   
           END-IF.                                                              
                                                                                
           IF INPUT-VALUE(1:1) = '-'                                            
              SET WS-NEGATIVE TO TRUE                                           
              MOVE 2 TO WS-OFFSET                                               
              IF INPUT-VALUE(WS-OFFSET:TRNS-IN-LENGTH - 1)                      
                                 NOT NUMERIC                                    
                 MOVE 77 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
                 GO TO 4020-NUMERIC-PROCESS-EXIT                                
              END-IF                                                            
              MOVE INPUT-VALUE(WS-OFFSET:TRNS-IN-LENGTH - 1)                    
                           TO WS-NUMERIC                                        
           ELSE                                                                 
              MOVE 1 TO WS-OFFSET                                               
              IF INPUT-VALUE(WS-OFFSET:TRNS-IN-LENGTH)                          
                                 NOT NUMERIC                                    
                 MOVE 77 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
                 GO TO 4020-NUMERIC-PROCESS-EXIT                                
              END-IF                                                            
              MOVE INPUT-VALUE(WS-OFFSET:TRNS-IN-LENGTH)                        
                           TO WS-NUMERIC                                        
           END-IF.                                                              
       4020-NUMERIC-PROCESS-EXIT.                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    MOVE NUMERIC VALUES FROM ALPHA FIELD TO NUMERIC FIELD                
      ****************************************************************          
       4040-ALPHA-PROCESS.                                                      
           MOVE 0 TO WS-NUMERIC.                                                
                                                                                
           IF TRNS-IN-LENGTH > 999                                              
              MOVE 95 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 4040-ALPHA-PROCESS-EXIT                                     
           END-IF.                                                              
                                                                                
           IF INPUT-VALUE(1:1) = '-'                                            
              SET WS-NEGATIVE TO TRUE                                           
              MOVE 2 TO WS-OFFSET                                               
           ELSE                                                                 
              IF INPUT-VALUE(1:1) = ' '                                         
              MOVE 2 TO WS-OFFSET                                               
              ELSE                                                              
                 MOVE 1 TO WS-OFFSET                                            
              END-IF                                                            
           END-IF.                                                              
                                                                                
           MOVE WS-OFFSET TO WS-WHOLE-OFFSET.                                   
           MOVE +0        TO WS-WHOLE-LENGTH.                                   
           PERFORM VARYING XXX FROM WS-WHOLE-OFFSET BY 1                        
                     UNTIL XXX > TRNS-IN-LENGTH                                 
                        OR INPUT-VALUE(XXX:1) = '.'                             
                        OR INPUT-VALUE(XXX:1) NOT NUMERIC                       
              ADD +1 TO WS-WHOLE-LENGTH                                         
           END-PERFORM.                                                         
           IF WS-WHOLE-LENGTH > LENGTH OF WS-NUMERIC                            
              MOVE 99 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 4040-ALPHA-PROCESS-EXIT                                     
           END-IF.                                                              
                                                                                
           MOVE +0 TO WS-DECIMAL-LENGTH.                                        
           IF INPUT-VALUE(XXX:1) = '.'                                          
              COMPUTE WS-DECIMAL-OFFSET = XXX + 1                               
              PERFORM VARYING XXX FROM WS-DECIMAL-OFFSET BY 1                   
                        UNTIL XXX > TRNS-IN-LENGTH                              
                           OR INPUT-VALUE(XXX:1) NOT NUMERIC                    
                 ADD +1 TO WS-DECIMAL-LENGTH                                    
              END-PERFORM                                                       
              IF (WS-WHOLE-LENGTH + WS-DECIMAL-LENGTH)                          
               > LENGTH OF WS-NUMERIC                                           
                 MOVE 99 TO TRNS-RETURN                                         
                 SET WS-FINISHED TO TRUE                                        
                 GO TO 4040-ALPHA-PROCESS-EXIT                                  
              END-IF                                                            
           END-IF.                                                              
                                                                                
           COMPUTE WS-HOLD-OFFSET = LENGTH OF WS-NUMERIC                        
                                  - WS-DECIMAL-LENGTH                           
                                  + 1.                                          
                                                                                
           IF WS-DECIMAL-LENGTH > 0                                             
              MOVE INPUT-VALUE(WS-DECIMAL-OFFSET:WS-DECIMAL-LENGTH)             
                   TO WS-NUMERIC(WS-HOLD-OFFSET:WS-DECIMAL-LENGTH)              
           END-IF.                                                              
                                                                                
           COMPUTE WS-HOLD-OFFSET = WS-HOLD-OFFSET                              
                                  - WS-WHOLE-LENGTH.                            
           MOVE INPUT-VALUE(WS-WHOLE-OFFSET:WS-WHOLE-LENGTH)                    
                TO WS-NUMERIC(WS-HOLD-OFFSET:WS-WHOLE-LENGTH).                  
                                                                                
           EVALUATE TRUE                                                        
              WHEN WS-DECIMAL-LENGTH = 0                                        
                 MOVE 02 TO WS-IN-TYPE                                          
              WHEN WS-DECIMAL-LENGTH = 1                                        
                 MOVE 04 TO WS-IN-TYPE                                          
              WHEN WS-DECIMAL-LENGTH = 2                                        
                 MOVE 03 TO WS-IN-TYPE                                          
              WHEN WS-DECIMAL-LENGTH = 3                                        
                 MOVE 05 TO WS-IN-TYPE                                          
              WHEN WS-DECIMAL-LENGTH = 4                                        
                 MOVE 06 TO WS-IN-TYPE                                          
              WHEN WS-DECIMAL-LENGTH = 5                                        
                 MOVE 07 TO WS-IN-TYPE                                          
              WHEN WS-DECIMAL-LENGTH = 6                                        
                 MOVE 08 TO WS-IN-TYPE                                          
              WHEN OTHER                                                        
                 MOVE 08 TO WS-IN-TYPE                                          
                 MOVE '04' TO TRNS-RETURN                                       
           END-EVALUATE.                                                        
       4040-ALPHA-PROCESS-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    UNPACK INPUT VALUES                                                  
      ****************************************************************          
       4100-UNPACK.                                                             
           IF WS-IN-COMP                                                        
              PERFORM 4120-UNPACK-COMP THRU                                     
                      4120-UNPACK-COMP-EXIT                                     
           ELSE                                                                 
              PERFORM 4140-UNPACK-COMP-3 THRU                                   
                      4140-UNPACK-COMP-3-EXIT                                   
           END-IF.                                                              
                                                                                
       4100-UNPACK-EXIT.                                                        
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *   UNPACK THE INPUT DATA BY MOVING IT THROUGH A SERIES OF                
      *     WORKING STORAGE AREAS.                                              
      *                                                                         
      *     THE POSITIVE VALUE WILL BE PASSED THROUGH, AND THE SIGN             
      *     WILL BE RETAINED IN WS-SIGN.                                        
      ****************************************************************          
       4120-UNPACK-COMP.                                                        
           MOVE INPUT-VALUE(1:TRNS-IN-LENGTH)                                   
               TO WS-COMP-X((8 - TRNS-IN-LENGTH + 1):TRNS-IN-LENGTH).           
           MOVE WS-COMP            TO WS-NUMERIC.                               
                                                                                
           COMPUTE WS-IN-TYPE = TRNS-IN-TYPE - 40.                              
                                                                                
           IF WS-COMP < 0                                                       
              SET WS-NEGATIVE TO TRUE                                           
           END-IF.                                                              
                                                                                
       4120-UNPACK-COMP-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *   UNPACK THE INPUT DATA BY MOVING IT THROUGH A SERIES OF                
      *     WORKING STORAGE AREAS.                                              
      *                                                                         
      *     THE POSITIVE VALUE WILL BE PASSED THROUGH, AND THE SIGN             
      *     WILL BE RETAINED IN WS-SIGN.                                        
      ****************************************************************          
       4140-UNPACK-COMP-3.                                                      
           MOVE INPUT-VALUE(1:TRNS-IN-LENGTH)                                   
                 TO WS-COMP-3-X((9 - TRNS-IN-LENGTH + 1):                       
                                                 TRNS-IN-LENGTH).               
           MOVE WS-COMP-3          TO WS-NUMERIC.                               
                                                                                
           COMPUTE WS-IN-TYPE = TRNS-IN-TYPE - 50.                              
                                                                                
           IF WS-COMP-3 < 0                                                     
              SET WS-NEGATIVE TO TRUE                                           
           END-IF.                                                              
                                                                                
       4140-UNPACK-COMP-3-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SEPARATE VALUE INTO WHOLE AND DECIMAL DIGITS                         
      ****************************************************************          
       4200-SEPARATE.                                                           
           MOVE 0   TO WS-WHOLE                                                 
                       WS-FRACTION.                                             
                                                                                
           EVALUATE WS-IN-TYPE                                                  
              WHEN 1 THRU 2                                                     
                 MOVE 0 TO WS-DECIMALS                                          
              WHEN 3                                                            
                 MOVE 2 TO WS-DECIMALS                                          
              WHEN 4                                                            
                 MOVE 1 TO WS-DECIMALS                                          
              WHEN 5                                                            
                 MOVE 3 TO WS-DECIMALS                                          
              WHEN 6                                                            
                 MOVE 4 TO WS-DECIMALS                                          
              WHEN 7                                                            
                 MOVE 5 TO WS-DECIMALS                                          
              WHEN 8                                                            
                 MOVE 6 TO WS-DECIMALS                                          
           END-EVALUATE.                                                        
                                                                                
           MOVE WS-NUMERIC(LENGTH OF WS-NUMERIC - WS-DECIMALS + 1:              
                                   WS-DECIMALS)                                 
                    TO WS-FRACTION(1:WS-DECIMALS).                              
           MOVE WS-NUMERIC(1:LENGTH OF WS-NUMERIC - WS-DECIMALS)                
                    TO WS-WHOLE.                                                
                                                                                
       4200-SEPARATE-EXIT.                                                      
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    RECONSTRUCT TARGET FIELD BASED ON TARGET TYPE AND LENGTH             
      ****************************************************************          
       5000-RECOMPOSE-NUM.                                                      
           IF WS-OUT-PACKED                                                     
              PERFORM 5200-CREATE-PACKED THRU                                   
                      5200-CREATE-PACKED-EXIT                                   
           ELSE                                                                 
              PERFORM 5400-CREATE-DISP THRU                                     
                      5400-CREATE-DISP-EXIT                                     
           END-IF.                                                              
                                                                                
       5000-RECOMPOSE-NUM-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    RECONSTRUCT TARGET FIELD AS A PACKED VALUE                           
      ****************************************************************          
       5200-CREATE-PACKED.                                                      
           MOVE 0 TO WS-NUMERIC.                                                
                                                                                
           EVALUATE WS-OUT-TYPE                                                 
              WHEN 41 THRU 42                                                   
              WHEN 51 THRU 52                                                   
                 MOVE 0 TO WS-DECIMALS                                          
              WHEN 53                                                           
                 MOVE 2 TO WS-DECIMALS                                          
              WHEN 54                                                           
                 MOVE 1 TO WS-DECIMALS                                          
              WHEN 55                                                           
                 MOVE 3 TO WS-DECIMALS                                          
              WHEN 56                                                           
                 MOVE 4 TO WS-DECIMALS                                          
              WHEN 57                                                           
                 MOVE 5 TO WS-DECIMALS                                          
              WHEN 58                                                           
                 MOVE 6 TO WS-DECIMALS                                          
           END-EVALUATE.                                                        
                                                                                
           COMPUTE WS-OFFSET = LENGTH OF WS-NUMERIC -                           
                                    WS-DECIMALS + 1.                            
                                                                                
           MOVE WS-FRACTION(1:WS-DECIMALS)                                      
                 TO WS-NUMERIC(WS-OFFSET:WS-DECIMALS).                          
                                                                                
           IF WS-FRACTION(WS-DECIMALS + 1:                                      
                        LENGTH OF WS-FRACTION - WS-DECIMALS)                    
                                     NOT = ZEROES                               
              MOVE 04 TO TRNS-RETURN                                            
           END-IF.                                                              
                                                                                
           COMPUTE WS-LENGTH = LENGTH OF WS-NUMERIC - WS-DECIMALS.              
                                                                                
           MOVE WS-WHOLE(LENGTH OF WS-WHOLE - WS-LENGTH + 1: WS-LENGTH)         
                  TO WS-NUMERIC(1:WS-LENGTH).                                   
                                                                                
           IF (LENGTH OF WS-WHOLE > WS-LENGTH)                                  
              IF WS-WHOLE(1:LENGTH OF WS-WHOLE - WS-LENGTH)                     
                            NOT = ZEROES                                        
                 MOVE 04 TO TRNS-RETURN                                         
              END-IF                                                            
           END-IF.                                                              
                                                                                
           IF WS-OUT-COMP                                                       
              PERFORM 5220-PACK-COMP THRU                                       
                      5220-PACK-COMP-EXIT                                       
           ELSE                                                                 
              PERFORM 5240-PACK-COMP-3 THRU                                     
                      5240-PACK-COMP-3-EXIT                                     
           END-IF.                                                              
                                                                                
       5200-CREATE-PACKED-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    PACK NUMERIC VALUE TO COMP AND MOVE TO TARGET                        
      ****************************************************************          
       5220-PACK-COMP.                                                          
           COMPUTE WS-LENGTH = TRNS-OUT-LENGTH * 2.                             
                                                                                
           MOVE WS-NUMERIC(LENGTH OF WS-NUMERIC - WS-LENGTH + 1:                
                                   WS-LENGTH)                                   
                             TO WS-COMP.                                        
                                                                                
           IF WS-NEGATIVE                                                       
              COMPUTE WS-COMP = WS-COMP * -1                                    
           END-IF.                                                              
                                                                                
           MOVE WS-COMP-X((LENGTH OF WS-COMP-X - TRNS-OUT-LENGTH + 1):          
                                  TRNS-OUT-LENGTH)                              
                             TO TRNS-WORK-AREA(1:TRNS-OUT-LENGTH).              
                                                                                
           IF WS-COMP-X(1:LENGTH OF WS-COMP-X - TRNS-OUT-LENGTH)                
                        NOT = LOW-VALUES                                        
              MOVE 04 TO TRNS-RETURN                                            
           END-IF.                                                              
       5220-PACK-COMP-EXIT.                                                     
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    PACK NUMERIC VALUE TO COMP-3 AND MOVE TO TARGET                      
      ****************************************************************          
       5240-PACK-COMP-3.                                                        
           COMPUTE WS-LENGTH = TRNS-OUT-LENGTH * 2 - 1.                         
                                                                                
           MOVE WS-NUMERIC(LENGTH OF WS-NUMERIC - WS-LENGTH + 1:                
                                   WS-LENGTH)                                   
                             TO WS-COMP-3.                                      
                                                                                
           IF WS-NEGATIVE                                                       
              COMPUTE WS-COMP-3 = WS-COMP-3 * -1                                
           END-IF.                                                              
                                                                                
           MOVE WS-COMP-3-X((LENGTH OF WS-COMP-3-X -                            
                                    TRNS-OUT-LENGTH + 1):                       
                                    TRNS-OUT-LENGTH)                            
                             TO TRNS-WORK-AREA(1:TRNS-OUT-LENGTH).              
                                                                                
           IF WS-COMP-3-X(1:LENGTH OF WS-COMP-3-X - TRNS-OUT-LENGTH)            
                        NOT = LOW-VALUES                                        
              MOVE 04 TO TRNS-RETURN                                            
           END-IF.                                                              
       5240-PACK-COMP-3-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    MOVE NUMERIC VALUE TO TARGET FIELD                                   
      ****************************************************************          
       5400-CREATE-DISP.                                                        
           EVALUATE WS-OUT-TYPE                                                 
              WHEN 1 THRU 2                                                     
                 MOVE 0 TO WS-DECIMALS                                          
              WHEN 3                                                            
                 MOVE 2 TO WS-DECIMALS                                          
              WHEN 4                                                            
                 MOVE 1 TO WS-DECIMALS                                          
              WHEN 5                                                            
                 MOVE 3 TO WS-DECIMALS                                          
              WHEN 6                                                            
                 MOVE 4 TO WS-DECIMALS                                          
              WHEN 7                                                            
                 MOVE 5 TO WS-DECIMALS                                          
              WHEN 8                                                            
                 MOVE 6 TO WS-DECIMALS                                          
           END-EVALUATE.                                                        
                                                                                
           IF TRNS-ACTION-PUT                                                   
           AND WS-DECIMALS > TRNS-OUT-LENGTH                                    
              MOVE WS-FRACTION(WS-DECIMALS - TRNS-OUT-LENGTH + 1:               
                               TRNS-OUT-LENGTH)                                 
                 TO TRNS-WORK-AREA(1:TRNS-OUT-LENGTH)                           
              IF WS-NEGATIVE                                                    
                 IF TRNS-WORK-AREA(1:1) NOT = 0                                 
                    MOVE 04 TO TRNS-RETURN                                      
                 END-IF                                                         
                 MOVE '-' TO TRNS-WORK-AREA(1:1)                                
              END-IF                                                            
              GO TO 5400-CREATE-DISP-EXIT                                       
           END-IF.                                                              
                                                                                
           COMPUTE WS-OFFSET = TRNS-OUT-LENGTH - WS-DECIMALS + 1.               
           COMPUTE WS-HOLD-OFFSET = WS-OFFSET + WS-DECIMALS.                    
           IF WS-HOLD-OFFSET > LENGTH OF TRNS-WORK-AREA                         
              MOVE 73 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 5400-CREATE-DISP-EXIT                                       
           END-IF.                                                              
                                                                                
           MOVE WS-FRACTION(1:WS-DECIMALS)                                      
                 TO TRNS-WORK-AREA(WS-OFFSET:WS-DECIMALS).                      
                                                                                
           IF WS-FRACTION(WS-DECIMALS + 1:                                      
                        LENGTH OF WS-FRACTION - WS-DECIMALS)                    
                      NOT = ZEROES                                              
              MOVE 04 TO TRNS-RETURN                                            
           END-IF.                                                              
                                                                                
           COMPUTE WS-OFFSET = LENGTH OF WS-WHOLE -                             
                                   TRNS-OUT-LENGTH + WS-DECIMALS + 1.           
           COMPUTE WS-HOLD-OFFSET = TRNS-OUT-LENGTH - WS-DECIMALS.              
           IF WS-HOLD-OFFSET > LENGTH OF TRNS-WORK-AREA                         
              MOVE 73 TO TRNS-RETURN                                            
              SET WS-FINISHED TO TRUE                                           
              GO TO 5400-CREATE-DISP-EXIT                                       
           END-IF.                                                              
                                                                                
           MOVE WS-WHOLE(WS-OFFSET:TRNS-OUT-LENGTH - WS-DECIMALS)               
                  TO TRNS-WORK-AREA(1:TRNS-OUT-LENGTH - WS-DECIMALS).           
                                                                                
           IF WS-WHOLE(1:WS-OFFSET - 1) NOT = ZEROES                            
              MOVE 04 TO TRNS-RETURN                                            
           END-IF.                                                              
                                                                                
           IF WS-NEGATIVE                                                       
              IF TRNS-WORK-AREA(1:1) NOT = 0                                    
                 MOVE 04 TO TRNS-RETURN                                         
              END-IF                                                            
              MOVE '-' TO TRNS-WORK-AREA(1:1)                                   
           END-IF.                                                              
                                                                                
           IF TRNS-OUT-LENGTH = 0                                               
              MOVE 0          TO TRNS-WORK-AREA                                 
              MOVE +1         TO TRNS-OUT-LENGTH                                
              SET TRNS-CHG    TO TRUE                                           
           END-IF.                                                              
                                                                                
       5400-CREATE-DISP-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    RECONSTRUCT OUTPUT VALUE AS A DISPLAY FIELD                          
      ****************************************************************          
       6000-RECOMPOSE-ALPHA.                                                    
           MOVE SPACE TO TRNS-WORK-AREA.                                        
                                                                                
           MOVE 1 TO WS-OFFSET.                                                 
                                                                                
           IF WS-NEGATIVE                                                       
              MOVE '-' TO TRNS-WORK-AREA(1:1)                                   
              ADD 1 TO WS-OFFSET                                                
           END-IF.                                                              
                                                                                
           PERFORM VARYING XXX FROM 1 BY 1                                      
           UNTIL XXX > LENGTH OF WS-NUMERIC                                     
           OR WS-NUMERIC(XXX:1) NOT = '0'                                       
           END-PERFORM.                                                         
                                                                                
           IF XXX > LENGTH OF WS-NUMERIC                                        
              NEXT SENTENCE                                                     
           ELSE                                                                 
              MOVE WS-NUMERIC(XXX:LENGTH OF WS-NUMERIC - XXX + 1)               
                      TO TRNS-WORK-AREA(WS-OFFSET:                              
                                  LENGTH OF WS-NUMERIC - XXX + 1)               
              COMPUTE WS-OFFSET = WS-OFFSET +                                   
                                 (LENGTH OF WS-NUMERIC - XXX + 1)               
           END-IF.                                                              
                                                                                
           IF WS-FRACTION = 0                                                   
              MOVE SPACES TO TRNS-WORK-AREA(WS-OFFSET:                          
                                       LENGTH OF TRNS-WORK-AREA -               
                                       WS-OFFSET + 1)                           
           ELSE                                                                 
              MOVE '.' TO TRNS-WORK-AREA(WS-OFFSET:1)                           
              ADD 1 TO WS-OFFSET                                                
                                                                                
              PERFORM VARYING XXX FROM 1 BY 1                                   
              UNTIL XXX > LENGTH OF WS-FRACTION                                 
              OR WS-FRACTION(XXX:LENGTH OF WS-FRACTION - XXX + 1) = ZERO        
                 MOVE WS-FRACTION(XXX:1)                                        
                         TO TRNS-WORK-AREA(WS-OFFSET:1)                         
                 ADD 1 TO WS-OFFSET                                             
              END-PERFORM                                                       
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM WS-OFFSET.                                           
           IF WS-OFFSET > TRNS-OUT-LENGTH                                       
              MOVE 04 TO TRNS-RETURN                                            
           END-IF.                                                              
                                                                                
       6000-RECOMPOSE-ALPHA-EXIT.                                               
           EXIT.                                                                
      /                                                                         
