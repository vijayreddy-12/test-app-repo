CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PBIX.                                                 
      *              PROGRAM CONVERTED BY                                       
      *              CCCA FOR OS/390 & MVS & VM 5648-B05                        
      *              CONVERSION DATE 07/24/08 07:29:07.                         
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
      *  THIS MODULE WILL TAKE A MESSAGE AND BUILD AN INDEX OF THE              
      *  OFFSETS OF ALL OF THE INDIVIDUAL PACKETS CONTAINED IN THE              
      *  MESSAGE.                                                               
      *                                                                         
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  MAR 1998   J KLAPWYK  CREATED                                     
      *  002  AUG 2002   TRUDY      PERFORMANCE CHANGES                         
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
               '**   MLX2PBIX WORKING STORAGE BEGINS  **'.                      
       01  FIXED-DATA.                                                          
           05  WS-V2-START-OFFSET             PIC S9(8) COMP                    
                                            VALUE +175.                         
           05  WS-V2-GROUP-TYPE               PIC 9(2)                          
                                            VALUE 20.                           
       01  VARIABLES.                                                           
           05  WS-OFFSET                      PIC S9(8) COMP.                   
           05  WS-LENGTH                      PIC S9(8) COMP.                   
           05  WS-PACKET-LENGTH   VALUE +34   PIC S9(5) COMP-3.                 
           05  WS-GROUP-INFO.                                                   
               10  WS-GROUP-INDEX-LENGTH      PIC 9(3).                         
               10  WS-GROUP-INDEX-VALUE       PIC S9(4) COMP.                   
           05  WS-INDEX-VALUE                 PIC S9(4) COMP.                   
           05  WS-PACKET-MAX   VALUE +999     PIC S9(4) COMP.                   
           05  WS-OCCUR                       PIC S9(5) COMP-3.                 
           05  WS-MOVE-OFFSET                 PIC S9(8) COMP.                   
           05  WS-MOVE-LENGTH                 PIC S9(8) COMP.                   
           05  WS-SEARCH-START                PIC S9(04) COMP.                  
           COPY MLX2WSVR.                                                       
           05  WS-HOLD-INDEX                  PIC X(35000).                     
                                                                                
           05  WS-LVL-ENDED.                                                    
               10  WS-LVL-1-ENDED                 PIC X.                        
                   88  LVL-1-ENDED              VALUE 'Y'.                      
                   88  LVL-1-NOT-ENDED          VALUE 'N'.                      
               10  WS-LVL-2-ENDED                 PIC X.                        
                   88  LVL-2-ENDED              VALUE 'Y'.                      
                   88  LVL-2-NOT-ENDED          VALUE 'N'.                      
               10  WS-LVL-3-ENDED                 PIC X.                        
                   88  LVL-3-ENDED              VALUE 'Y'.                      
                   88  LVL-3-NOT-ENDED          VALUE 'Y'.                      
           05  WS-LOOP-DONE-IND               PIC X.                            
               88  LOOP-NOT-DONE            VALUE 'N'.                          
               88  LOOP-DONE                VALUE 'Y'.                          
           05  WS-LOOP-ERROR-IND              PIC X.                            
               88  NO-ERROR                 VALUE 'N'.                          
               88  LOOP-ERROR               VALUE 'Y'.                          
           05  WS-FOUND-IND                   PIC X.                            
               88  WS-NOT-FOUND             VALUE 'N'.                          
               88  WS-FOUND                 VALUE 'Y'.                          
                                                                                
      *----------------------------------------------------------------*        
      *  SINGLE INDEX ENTRY INITIALIZATION                                      
      *----------------------------------------------------------------*        
       01  INDEX-ENTRY-INIT.                                                    
           COPY MLX2NDXI.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  SINGLE INDEX ENTRY                                                     
      *----------------------------------------------------------------*        
       01  INDEX-ENTRY.                                                         
           COPY MLX2NDXE.                                                       
                                                                                
       01  FILLER                             PIC X(40) VALUE                   
               '***  MLX2PBIX WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  INPUT PARAMETERS                                                       
      *----------------------------------------------------------------*        
       01  INPUT-PARMS.                                                         
           COPY MLX2INPT.                                                       
                                                                                
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
                                                                                
       01  MSG-AREA                            PIC X(1).                        
       01  PBIX-RETURN                         PIC 9(2).                        
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING INPUT-PARMS                                     
                                COMM-FIELDS                                     
                                INDEX-DEFN                                      
                                MSG-AREA                                        
                                PBIX-RETURN.                                    
      *----------------------------------------------------------------*        
       0000-MAINLINE.                                                           
      *    DISPLAY 'MLX2PBIX BEGINS'                                            
           PERFORM 1000-INIT THRU                                               
                   1000-INIT-EXIT.                                              
                                                                                
           PERFORM 2100-SETUP-INDEX THRU                                        
                   2100-SETUP-INDEX-EXIT                                        
                    VARYING WS-OFFSET                                           
                    FROM    WS-OFFSET                                           
                    BY      WS-LENGTH                                           
                    UNTIL   WS-OFFSET > INPT-MAX-LENGTH-AREA1                   
                    OR      LOOP-DONE                                           
                    OR      LOOP-ERROR.                                         
                                                                                
      ****************************************************************          
      *    SET UP FINAL RETURN CODE                                             
      ****************************************************************          
           IF WS-OFFSET > INPT-MAX-LENGTH-AREA1                                 
              MOVE 40 TO PBIX-RETURN                                            
           END-IF.                                                              
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      * INITIALIZATION ROUTINE.                                                 
      ****************************************************************          
       1000-INIT.                                                               
           MOVE 0     TO PBIX-RETURN.                                           
           MOVE WS-V2-START-OFFSET TO WS-OFFSET.                                
           MOVE 0 TO INDX-PACKET-COUNT.                                         
                                                                                
           MOVE NDXE-KEY-INIT TO NDXE-KEY-SUBSET.                               
           MOVE SPACES TO WS-LVL-ENDED.                                         
                                                                                
           SET LOOP-NOT-DONE TO TRUE.                                           
           SET NO-ERROR      TO TRUE.                                           
           MOVE +0           TO WS-LENGTH.                                      
           SET INDX-XXX      TO +1.                                             
           MOVE +1           TO WS-SEARCH-START.                                
                                                                                
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * BUILD INDEX - THIS IS PROCESSED FOR EACH TAG IN THE MESSAGE             
      *               THIS GETS EXECUTED LITERALLY MILLIONS OF TIMES            
      *               SO BE VERY CAREFUL NOT TO IMPACT PERFORMANCE              
      ****************************************************************          
       2100-SETUP-INDEX.                                                        
      ****************************************************************          
      *    DETERMINE PACKET TYPE                                                
      ****************************************************************          
           MOVE MSG-AREA(WS-OFFSET:6) TO WS-HOLD-PACKET-TYPE                    
                                                                                
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
                                                                                
           IF WS-HOLD-TYPE NOT NUMERIC                                          
              SET LOOP-ERROR            TO TRUE                                 
              MOVE 91                   TO PBIX-RETURN                          
              GO TO 2100-SETUP-INDEX-EXIT                                       
           END-IF.                                                              
                                                                                
      ****************************************************************          
      *    DETERMINE PACKET LENGTH AND PROCESS THE TAG                          
      ****************************************************************          
              IF FIELD-LEVEL                                                    
                 MOVE MSG-AREA(WS-OFFSET + 6:3)  TO WS-LENGTH                   
                 ADD  9                          TO WS-LENGTH                   
                 PERFORM 2180-FIELD-LEVEL THRU                                  
                         2180-FIELD-LEVEL-EXIT                                  
              ELSE                                                              
              IF PREDEF-SEGMENT                                                 
                 MOVE MSG-AREA(WS-OFFSET + 12:9) TO WS-LENGTH                   
                 ADD  21                         TO WS-LENGTH                   
                 PERFORM 2180-FIELD-LEVEL THRU                                  
                         2180-FIELD-LEVEL-EXIT                                  
              ELSE                                                              
              IF GROUP-START                                                    
                 MOVE MSG-AREA(WS-OFFSET + 6:3)  TO WS-LENGTH                   
                 ADD  9                          TO WS-LENGTH                   
                 PERFORM 2130-GROUP-START THRU                                  
                         2130-GROUP-START-EXIT                                  
              ELSE                                                              
              IF GROUP-END                                                      
                 MOVE +4                         TO WS-LENGTH                   
                 PERFORM 2140-GROUP-END THRU                                    
                         2140-GROUP-END-EXIT                                    
              ELSE                                                              
              IF SECTION-START                                                  
                 MOVE +4                         TO WS-LENGTH                   
                 PERFORM 2150-SECTION-START THRU                                
                         2150-SECTION-START-EXIT                                
              ELSE                                                              
              IF SECTION-END                                                    
                 MOVE +4                         TO WS-LENGTH                   
                 PERFORM 2160-SECTION-END THRU                                  
                         2160-SECTION-END-EXIT                                  
              ELSE                                                              
              IF MESSAGE-END                                                    
                 MOVE +4                         TO WS-LENGTH                   
                 PERFORM 2170-MESSAGE-END THRU                                  
                         2170-MESSAGE-END-EXIT.                                 
                                                                                
           IF LOOP-ERROR                                                        
              GO TO 2100-SETUP-INDEX-EXIT                                       
           END-IF.                                                              
                                                                                
           MOVE WS-HOLD-PACKET      TO NDXE-PACKET-ID.                          
           MOVE WS-OFFSET           TO NDXE-PACKET-OFFSET.                      
           SET NDXE-AREA1           TO TRUE.                                    
                                                                                
           PERFORM 4000-UPDATE-INDEX THRU                                       
                   4000-UPDATE-INDEX-EXIT.                                      
                                                                                
           EVALUATE TRUE                                                        
              WHEN FIELD-LEVEL                                                  
              WHEN PREDEF-SEGMENT                                               
                 CONTINUE                                                       
              WHEN GROUP-START                                                  
                 IF NDXE-LVL-2-ID = LOW-VALUES                                  
                    MOVE SPACE TO NDXE-LVL-2-ID                                 
                 END-IF                                                         
                 IF NDXE-LVL-3-ID = LOW-VALUES                                  
                    MOVE SPACE TO NDXE-LVL-3-ID                                 
                 END-IF                                                         
              WHEN LVL-1-ENDED                                                  
                 MOVE SPACE          TO NDXE-LVL-1-ID                           
                                        NDXE-LVL-2-ID                           
                                        NDXE-LVL-3-ID                           
                                        NDXE-LVL-3-X                            
                 MOVE +0             TO NDXE-LVL-1-OCCUR                        
                 SET LVL-1-NOT-ENDED TO TRUE                                    
              WHEN LVL-2-ENDED                                                  
                 MOVE SPACE          TO NDXE-LVL-2-ID                           
                                        NDXE-LVL-3-ID                           
                                        NDXE-LVL-3-X                            
                 MOVE +0             TO NDXE-LVL-2-OCCUR                        
                 SET LVL-2-NOT-ENDED TO TRUE                                    
              WHEN LVL-3-ENDED                                                  
                 MOVE SPACE          TO NDXE-LVL-3-ID                           
                                        NDXE-LVL-3-X                            
                 MOVE +0             TO NDXE-LVL-3-OCCUR                        
                 SET LVL-3-NOT-ENDED TO TRUE                                    
              WHEN SECTION-START                                                
                 SET WS-SEARCH-START TO INDX-XXX                                
                 MOVE NDXE-LVLS-SP-INIT TO NDXE-LVLS                            
              WHEN SECTION-END                                                  
                 MOVE NDXE-KEY-INIT TO NDXE-KEY-SUBSET                          
           END-EVALUATE.                                                        
                                                                                
       2100-SETUP-INDEX-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      /                                                                         
      ****************************************************************          
      *    SET UP GROUP START INFORMATION                                       
      ****************************************************************          
       2130-GROUP-START.                                                        
           MOVE MSG-AREA(WS-OFFSET + 6:3)     TO WS-GROUP-INDEX-LENGTH.         
           MOVE MSG-AREA(WS-OFFSET + 9:WS-GROUP-INDEX-LENGTH)                   
                                              TO WS-GROUP-INDEX-VALUE.          
                                                                                
           MOVE LOW-VALUES                    TO NDXE-LVL-3-X.                  
                                                                                
           EVALUATE TRUE                                                        
              WHEN NDXE-LVL-3-ID NOT = SPACE                                    
                 IF WS-HOLD-PACKET = NDXE-LVL-3-ID                              
                 AND WS-GROUP-INDEX-LENGTH NOT = 0                              
                    MOVE WS-GROUP-INDEX-VALUE    TO NDXE-LVL-3-OCCUR            
                 ELSE                                                           
                    SET LOOP-ERROR               TO TRUE                        
                    MOVE 30                      TO PBIX-RETURN                 
                    GO TO 2130-GROUP-START-EXIT                                 
                 END-IF                                                         
              WHEN NDXE-LVL-2-ID NOT = SPACE                                    
                 IF WS-HOLD-PACKET NOT = NDXE-LVL-2-ID                          
                    MOVE WS-HOLD-PACKET          TO NDXE-LVL-3-ID               
                    MOVE WS-GROUP-INDEX-VALUE    TO NDXE-LVL-3-OCCUR            
                 ELSE                                                           
                    IF WS-GROUP-INDEX-LENGTH NOT = 0                            
                       MOVE WS-GROUP-INDEX-VALUE TO NDXE-LVL-2-OCCUR            
                       MOVE LOW-VALUES           TO NDXE-LVL-3-ID               
                       MOVE +0                   TO NDXE-LVL-3-OCCUR            
                    ELSE                                                        
                       SET LOOP-ERROR            TO TRUE                        
                       MOVE 30                   TO PBIX-RETURN                 
                    END-IF                                                      
                 END-IF                                                         
              WHEN NDXE-LVL-1-ID NOT = SPACE                                    
                 IF WS-HOLD-PACKET NOT = NDXE-LVL-1-ID                          
                    MOVE WS-HOLD-PACKET          TO NDXE-LVL-2-ID               
                    MOVE WS-GROUP-INDEX-VALUE    TO NDXE-LVL-2-OCCUR            
                    MOVE LOW-VALUES              TO NDXE-LVL-3-ID               
                    MOVE +0                      TO NDXE-LVL-3-OCCUR            
                 ELSE                                                           
                    IF WS-GROUP-INDEX-LENGTH NOT = 0                            
                       MOVE WS-GROUP-INDEX-VALUE TO NDXE-LVL-1-OCCUR            
                       MOVE LOW-VALUES           TO NDXE-LVL-2-ID               
                                                    NDXE-LVL-3-ID               
                       MOVE +0                   TO NDXE-LVL-2-OCCUR            
                                                    NDXE-LVL-3-OCCUR            
                    ELSE                                                        
                       SET LOOP-ERROR            TO TRUE                        
                       MOVE 30                   TO PBIX-RETURN                 
                    END-IF                                                      
                 END-IF                                                         
              WHEN OTHER                                                        
                 MOVE WS-HOLD-PACKET             TO NDXE-LVL-1-ID               
                 MOVE WS-GROUP-INDEX-VALUE       TO NDXE-LVL-1-OCCUR            
                 MOVE LOW-VALUES                 TO NDXE-LVL-2-ID               
                                                    NDXE-LVL-3-ID               
                 MOVE +0                         TO NDXE-LVL-2-OCCUR            
                                                    NDXE-LVL-3-OCCUR            
           END-EVALUATE.                                                        
       2130-GROUP-START-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP GROUP END INFORMATION                                         
      ****************************************************************          
       2140-GROUP-END.                                                          
           MOVE HIGH-VALUES TO NDXE-LVL-3-X.                                    
                                                                                
           IF NDXE-LVL-3-ID NOT = SPACE                                         
                 SET LVL-3-ENDED  TO TRUE                                       
           ELSE                                                                 
              IF NDXE-LVL-2-ID NOT = SPACE                                      
                 SET LVL-2-ENDED  TO TRUE                                       
                 MOVE HIGH-VALUES TO NDXE-LVL-3-ID                              
              ELSE                                                              
              IF NDXE-LVL-1-ID NOT = SPACE                                      
                 SET LVL-1-ENDED  TO TRUE                                       
                 MOVE HIGH-VALUES TO NDXE-LVL-2-ID                              
                                     NDXE-LVL-3-ID                              
              ELSE                                                              
                 SET LOOP-ERROR   TO TRUE                                       
                 MOVE 20          TO PBIX-RETURN                                
           END-IF.                                                              
       2140-GROUP-END-EXIT.                                                     
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP SECTION START INFORMATION                                     
      ****************************************************************          
       2150-SECTION-START.                                                      
           IF WS-HOLD-PACKET = NDXE-SECTION-ID                                  
              ADD 1 TO NDXE-SECTION-OCCUR                                       
           ELSE                                                                 
              MOVE +1 TO NDXE-SECTION-OCCUR                                     
           END-IF.                                                              
                                                                                
           MOVE NDXE-LVLS-LV-INIT TO NDXE-LVLS.                                 
           MOVE WS-HOLD-PACKET TO NDXE-SECTION-ID.                              
       2150-SECTION-START-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP SECTION END INFORMATION                                       
      ****************************************************************          
       2160-SECTION-END.                                                        
           MOVE HIGH-VALUES TO NDXE-LVL-1-ID                                    
                               NDXE-LVL-2-ID                                    
                               NDXE-LVL-3-ID                                    
                               NDXE-LVL-3-X.                                    
       2160-SECTION-END-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP MESSAGE END INFORMATION                                       
      ****************************************************************          
       2170-MESSAGE-END.                                                        
           MOVE HIGH-VALUES TO NDXE-SECTION-ID                                  
                               NDXE-LVL-1-ID                                    
                               NDXE-LVL-2-ID                                    
                               NDXE-LVL-3-ID                                    
                               NDXE-LVL-3-X.                                    
                                                                                
           SET LOOP-DONE    TO TRUE.                                            
       2170-MESSAGE-END-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    SET UP FIELD-LEVEL INFORMATION                                       
      ****************************************************************          
       2180-FIELD-LEVEL.                                                        
           MOVE SPACE                         TO NDXE-LVL-3-X.                  
                                                                                
           IF NDXE-LVL-3-ID = LOW-VALUES                                        
              MOVE SPACE TO NDXE-LVL-3-ID                                       
           END-IF.                                                              
                                                                                
           IF NDXE-LVL-2-ID = LOW-VALUES                                        
              MOVE SPACE TO NDXE-LVL-2-ID                                       
           END-IF.                                                              
                                                                                
           IF NDXE-LVL-1-ID = LOW-VALUES                                        
              MOVE SPACE TO NDXE-LVL-1-ID                                       
           END-IF.                                                              
                                                                                
       2180-FIELD-LEVEL-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    INSERT INDEX ENTRY IN PROPER POSITION OF INDEX.                      
      ****************************************************************          
       4000-UPDATE-INDEX.                                                       
           SET INDX-XXX TO INDX-PACKET-COUNT.                                   
                                                                                
           IF NDXE-KEY > INDX-KEY(INDX-XXX)                                     
              SET WS-NOT-FOUND TO TRUE                                          
           ELSE                                                                 
              SET INDX-XXX TO WS-SEARCH-START                                   
                                                                                
              SEARCH INDX-PACKETS                                               
                 AT END                                                         
                    SET WS-NOT-FOUND TO TRUE                                    
                 WHEN INDX-KEY(INDX-XXX) > NDXE-KEY                             
                    SET WS-FOUND TO TRUE                                        
              END-SEARCH                                                        
           END-IF.                                                              
                                                                                
           ADD +1 TO INDX-PACKET-COUNT.                                         
           IF INDX-PACKET-COUNT > WS-PACKET-MAX                                 
              MOVE 42 TO PBIX-RETURN                                            
              GO TO 4000-UPDATE-INDEX-EXIT                                      
           END-IF.                                                              
                                                                                
      ****************************************************************          
      *    IF THE TAG IS NOT ALREADY IN THE INDEX, THEN ADD IT,                 
      *    OTHERWISE UPDATE IT.                                                 
      ****************************************************************          
           IF WS-NOT-FOUND                                                      
              SET INDX-XXX TO INDX-PACKET-COUNT                                 
              MOVE INDEX-ENTRY TO INDX-PACKETS(INDX-XXX)                        
           ELSE                                                                 
              PERFORM 4200-INDEX-INSERT THRU                                    
                      4200-INDEX-INSERT-EXIT                                    
           END-IF.                                                              
                                                                                
           IF PBIX-RETURN NOT = 0                                               
              SET LOOP-ERROR TO TRUE                                            
           END-IF.                                                              
                                                                                
       4000-UPDATE-INDEX-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    INSERT INDEX ENTRY INTO INDEX                                        
      ****************************************************************          
       4200-INDEX-INSERT.                                                       
                                                                                
           SET WS-OCCUR TO INDX-XXX.                                            
                                                                                
           COMPUTE WS-MOVE-OFFSET = (LENGTH OF INDX-PACKETS *                   
                                    (WS-OCCUR - 1)) + 1.                        
                                                                                
           COMPUTE WS-MOVE-LENGTH = LENGTH OF INDX-PACKETS-X -                  
                                    (WS-MOVE-OFFSET - 1).                       
                                                                                
           MOVE INDX-PACKETS-X(WS-MOVE-OFFSET:WS-MOVE-LENGTH)                   
                    TO WS-HOLD-INDEX.                                           
                                                                                
           MOVE INDEX-ENTRY    TO INDX-PACKETS(INDX-XXX).                       
           COMPUTE WS-MOVE-OFFSET = WS-MOVE-OFFSET +                            
                                    LENGTH OF INDX-PACKETS.                     
                                                                                
           MOVE WS-HOLD-INDEX(1:WS-MOVE-LENGTH)                                 
                    TO INDX-PACKETS-X(WS-MOVE-OFFSET:                           
                             (WS-MOVE-LENGTH - LENGTH OF INDX-PACKETS)).        
                                                                                
       4200-INDEX-INSERT-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
