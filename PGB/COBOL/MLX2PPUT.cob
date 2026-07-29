CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PPUT.                                                 
      *              PROGRAM CONVERTED BY                                       
      *              CCCA FOR OS/390 & MVS & VM 5648-B05                        
      *              CONVERSION DATE 07/24/08 07:28:11.                         
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
      *  THIS MODULE WILL TAKE A MESSAGE AND INSERT A PACKET                    
      *  RETURNING THE FULL NEW MESSAGE                                         
      *                                                                         
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  MAR 1998   J KLAPWYK  CREATED                                     
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
               '**   MLX2PPUT WORKING STORAGE BEGINS  **'.                      
       01  FIXED.                                                               
           05  WS-HEADER-START                PIC X(4) VALUE '9991'.            
           05  WS-HEADER-END                  PIC X(4) VALUE '9995'.            
           05  WS-BODY-START                  PIC X(4) VALUE '9992'.            
           05  WS-BODY-END                    PIC X(4) VALUE '9996'.            
           05  WS-TRAILER-START               PIC X(4) VALUE '9993'.            
           05  WS-TRAILER-END                 PIC X(4) VALUE '9997'.            
           05  WS-TYPE-CLOB                   PIC X(2) VALUE '21'.              
           05  WS-TYPE-BLOB                   PIC X(2) VALUE '22'.              
                                                                                
       01  VARIABLES.                                                           
           05  XXX                            PIC S9(4) COMP.                   
           05  WS-INDEX-VALUE                 PIC S9(4) COMP.                   
           05  WS-HOLD-LENGTH                 PIC S9(4) COMP.                   
           05  WS-HOLD-PACKET                 PIC X(4).                         
               88  P-SPECIAL-VALUE          VALUE '9995' THRU '9999'.           
           05  WS-WORK-OFFSET                 PIC S9(4) COMP.                   
           05  WS-TEMP-PACKET                 PIC X(4).                         
           05  WS-TEMP-TYPE                   PIC X(2).                         
           05  WS-TEMP-LENGTH                 PIC S9(9) COMP.                   
           05  WS-TEMP-VALUE                  PIC S9(9) COMP.                   
           05  WS-TEMP-LENGTH-9               PIC 9(9).                         
           05  WS-TEMP-VALUE-9                PIC 9(9).                         
           05  WS-HOLD-ENTRY                  PIC X(50).                        
           05  WS-OCCUR                       PIC S9(4) COMP.                   
           05  WS-MOVE-OFFSET                 PIC S9(4) COMP.                   
           05  WS-MOVE-LENGTH                 PIC S9(4) COMP.                   
           05  WS-LOW-VALUE-COUNT             PIC S9(4) COMP.                   
           05  WS-HOLD-INDEX                  PIC X(35000).                     
           05  WS-LEVEL-PUT-IND               PIC X(1).                         
               88  WS-SECTION-PUT           VALUE 'S'.                          
               88  WS-LVL-3-PUT             VALUE '3'.                          
               88  WS-LVL-2-PUT             VALUE '2'.                          
               88  WS-LVL-1-PUT             VALUE '1'.                          
               88  WS-NOT-LVL-PUT           VALUE 'N'.                          
           05  WS-PACKET-PUT-IND              PIC X(1).                         
               88  WS-PACKET-PUT            VALUE 'Y'.                          
               88  WS-NOT-PACKET-PUT        VALUE 'N'.                          
           05  WS-FOUND-SW                    PIC X(1).                         
               88  WS-FOUND                 VALUE 'Y'.                          
               88  WS-NOT-FOUND             VALUE 'N'.                          
           05  WS-FINISH-IND                  PIC X(1).                         
               88  WS-DONE                  VALUE 'Y'.                          
               88  WS-NOT-DONE              VALUE 'N'.                          
           05  WS-SECTION-IND                 PIC X(1).                         
               88  WS-SECTION               VALUE 'Y'.                          
               88  WS-NOT-SECTION           VALUE 'N'.                          
           05  WS-ADD-INSERT-IND              PIC X(1).                         
               88  WS-ADD                   VALUE 'A'.                          
               88  WS-INSERT                VALUE 'I'.                          
                                                                                
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
       01  PPUT-RETURN                         PIC 9(2).                        
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING INPUT-PARMS                                     
                                PARSER-RETURNS                                  
                                COMM-FIELDS                                     
                                INDEX-DEFN                                      
                                AREA1                                           
                                AREA2                                           
                                AREA3                                           
                                PPUT-RETURN.                                    
      *----------------------------------------------------------------*        
                                                                                
      *                                                                         
       0000-MAINLINE.                                                           
           PERFORM 1000-INIT THRU                                               
                   1000-INIT-EXIT.                                              
                                                                                
           IF PPUT-RETURN NOT = 0                                               
              GO TO 0000-MAINLINE-EXIT                                          
           END-IF.                                                              
                                                                                
           PERFORM 2000-PUT THRU                                                
                   2000-PUT-EXIT.                                               
                                                                                
           IF PPUT-RETURN NOT = 0                                               
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
           MOVE +1                    TO WS-WORK-OFFSET.                        
           MOVE SPACE                 TO WS-LEVEL-PUT-IND                       
                                         COMM-WORK-AREA.                        
           SET WS-NOT-PACKET-PUT      TO TRUE.                                  
           SET WS-NOT-LVL-PUT         TO TRUE.                                  
           MOVE 0                     TO PPUT-RETURN.                           
                                                                                
           MOVE INPT-PACKET-ID        TO WS-HOLD-PACKET.                        
           IF P-SPECIAL-VALUE                                                   
              MOVE 54                 TO PPUT-RETURN                            
              GO TO 1000-INIT-EXIT                                              
           END-IF.                                                              
                                                                                
           MOVE 0 TO WS-LOW-VALUE-COUNT.                                        
           INSPECT AREA2(1:INPT-PACKET-LENGTH)                                  
           TALLYING WS-LOW-VALUE-COUNT                                          
           FOR ALL LOW-VALUES.                                                  
           IF WS-LOW-VALUE-COUNT > 0                                            
              MOVE 78   TO PPUT-RETURN                                          
              GO TO 1000-INIT-EXIT                                              
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
              WHEN INPT-SECTION-HEADER                                          
                 MOVE WS-HEADER-START        TO NDXE-SECTION-ID                 
                 IF INPT-GRP-LVL-1-ID NOT = SPACE                               
                 OR INPT-PACKET-TYPE = WS-TYPE-BLOB                             
                 OR INPT-PACKET-TYPE = WS-TYPE-CLOB                             
                    MOVE 58 TO PPUT-RETURN                                      
                    GO TO 1000-INIT-EXIT                                        
                 END-IF                                                         
              WHEN INPT-SECTION-BODY                                            
                 MOVE WS-BODY-START          TO NDXE-SECTION-ID                 
              WHEN INPT-SECTION-TRAILER                                         
                 MOVE WS-TRAILER-START       TO NDXE-SECTION-ID                 
                 IF INPT-SECTION-OCCUR = 0                                      
                    IF INDX-PACKET-COUNT <= 1                                   
                       MOVE +1 TO NDXE-SECTION-OCCUR                            
                    ELSE                                                        
                       IF INDX-SECTION-ID(INDX-PACKET-COUNT - 1)                
                              = INPT-SECTION                                    
                          COMPUTE NDXE-SECTION-OCCUR =                          
                           INDX-SECTION-OCCUR(INDX-PACKET-COUNT - 1) + 1        
                       ELSE                                                     
                          MOVE +1 TO NDXE-SECTION-OCCUR                         
                       END-IF                                                   
                    END-IF                                                      
                    MOVE NDXE-SECTION-OCCUR  TO RTRN-TRAILER-OCCUR              
                 ELSE                                                           
                    IF INPT-SECTION-OCCUR >                                     
                            INDX-SECTION-OCCUR(INDX-PACKET-COUNT)               
                       MOVE 51 TO PPUT-RETURN                                   
                       GO TO 1000-INIT-EXIT                                     
                    END-IF                                                      
                 END-IF                                                         
              WHEN OTHER                                                        
                 MOVE 50                     TO PPUT-RETURN                     
                 GO TO 1000-INIT-EXIT                                           
           END-EVALUATE.                                                        
                                                                                
           IF NOT INPT-SECTION-TRAILER                                          
              MOVE INPT-SECTION-OCCUR           TO NDXE-SECTION-OCCUR           
           END-IF.                                                              
           MOVE INPT-GRP-LVL-1-ID            TO NDXE-LVL-1-ID.                  
           MOVE INPT-GRP-LVL-1-OCCUR         TO NDXE-LVL-1-OCCUR.               
           MOVE INPT-GRP-LVL-2-ID            TO NDXE-LVL-2-ID.                  
           MOVE INPT-GRP-LVL-2-OCCUR         TO NDXE-LVL-2-OCCUR.               
           MOVE INPT-GRP-LVL-3-ID            TO NDXE-LVL-3-ID.                  
           MOVE INPT-GRP-LVL-3-OCCUR         TO NDXE-LVL-3-OCCUR.               
           MOVE INPT-PACKET-ID               TO NDXE-PACKET-ID.                 
           SET NDXE-AREA2                    TO TRUE.                           
                                                                                
           EVALUATE TRUE                                                        
              WHEN INPT-GRP-LVL-3-ID NOT = SPACE                                
                 SET WS-LVL-3-PUT   TO TRUE                                     
              WHEN INPT-GRP-LVL-2-ID NOT = SPACE                                
                 SET WS-LVL-2-PUT   TO TRUE                                     
              WHEN INPT-GRP-LVL-1-ID NOT = SPACE                                
                 SET WS-LVL-1-PUT   TO TRUE                                     
              WHEN OTHER                                                        
                 SET WS-SECTION-PUT TO TRUE                                     
           END-EVALUATE.                                                        
                                                                                
           IF INPT-PACKET-ID NOT = SPACE                                        
              SET WS-PACKET-PUT TO TRUE                                         
           END-IF.                                                              
                                                                                
           MOVE INDEX-ENTRY  TO WS-HOLD-ENTRY.                                  
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * INSERT ENTRIES IN INDEX                                                 
      ****************************************************************          
       2000-PUT.                                                                
           EVALUATE TRUE                                                        
              WHEN WS-LVL-3-PUT                                                 
                 PERFORM 2100-LVL-3-PUT THRU                                    
                         2100-LVL-3-PUT-EXIT                                    
              WHEN WS-LVL-2-PUT                                                 
                 PERFORM 2200-LVL-2-PUT THRU                                    
                         2200-LVL-2-PUT-EXIT                                    
              WHEN WS-LVL-1-PUT                                                 
                 PERFORM 2300-LVL-1-PUT THRU                                    
                         2300-LVL-1-PUT-EXIT                                    
              WHEN WS-SECTION-PUT                                               
                 PERFORM 2400-SECTION-PUT THRU                                  
                         2400-SECTION-PUT-EXIT                                  
           END-EVALUATE.                                                        
                                                                                
           IF WS-PACKET-PUT                                                     
              PERFORM 2500-PACKET-PUT THRU                                      
                      2500-PACKET-PUT-EXIT                                      
           END-IF.                                                              
                                                                                
       2000-PUT-EXIT.                                                           
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      * SEARCH FOR EXISTENCE OF LEVEL 3 GROUP                                   
      *  - IF DOESN'T EXIST ADD IT TO INDEX                                     
      ****************************************************************          
       2100-LVL-3-PUT.                                                          
           PERFORM 2200-LVL-2-PUT THRU                                          
                   2200-LVL-2-PUT-EXIT.                                         
                                                                                
           MOVE WS-HOLD-ENTRY TO INDEX-ENTRY.                                   
                                                                                
           COMPUTE WS-HOLD-LENGTH = WS-WORK-OFFSET + 4.                         
           IF WS-HOLD-LENGTH > LENGTH OF COMM-WORK-AREA                         
              MOVE 44 TO PPUT-RETURN                                            
              GO TO 2100-LVL-3-PUT-EXIT                                         
           END-IF.                                                              
                                                                                
           MOVE '9999'        TO NDXE-PACKET-ID                                 
                                 COMM-WORK-AREA(WS-WORK-OFFSET:4).              
           MOVE HIGH-VALUES   TO NDXE-LVL-3-X.                                  
           SET INDX-XXX TO +1.                                                  
           PERFORM 7000-SEARCH THRU                                             
                   7000-SEARCH-EXIT.                                            
                                                                                
           IF WS-FOUND                                                          
              GO TO 2100-LVL-3-PUT-EXIT                                         
           END-IF.                                                              
                                                                                
           MOVE WS-WORK-OFFSET       TO NDXE-PACKET-OFFSET.                     
           ADD +4                    TO WS-WORK-OFFSET.                         
                                                                                
           SET NDXE-WORK             TO TRUE.                                   
           PERFORM 8000-ADD-TO-INDEX THRU                                       
                   8000-ADD-TO-INDEX-EXIT.                                      
                                                                                
           MOVE WS-HOLD-ENTRY        TO INDEX-ENTRY.                            
           MOVE NDXE-LVL-3-ID        TO NDXE-PACKET-ID.                         
           MOVE LOW-VALUES           TO NDXE-LVL-3-X.                           
           MOVE INPT-GRP-LVL-3-OCCUR TO WS-TEMP-VALUE.                          
           PERFORM 2800-ADD-GROUP THRU                                          
                   2800-ADD-GROUP-EXIT.                                         
                                                                                
       2100-LVL-3-PUT-EXIT.                                                     
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SEARCH FOR EXISTENCE OF LEVEL 2 GROUP                                   
      *  - IF DOESN'T EXIST ADD IT TO INDEX                                     
      ****************************************************************          
       2200-LVL-2-PUT.                                                          
           PERFORM 2300-LVL-1-PUT THRU                                          
                   2300-LVL-1-PUT-EXIT.                                         
                                                                                
           MOVE WS-HOLD-ENTRY TO INDEX-ENTRY.                                   
                                                                                
           COMPUTE WS-HOLD-LENGTH = WS-WORK-OFFSET + 4.                         
           IF WS-HOLD-LENGTH > LENGTH OF COMM-WORK-AREA                         
              MOVE 44 TO PPUT-RETURN                                            
              GO TO 2200-LVL-2-PUT-EXIT                                         
           END-IF.                                                              
                                                                                
           MOVE '9999'        TO NDXE-PACKET-ID                                 
                                 COMM-WORK-AREA(WS-WORK-OFFSET:4).              
           MOVE HIGH-VALUES   TO NDXE-LVL-3-ID                                  
                                 NDXE-LVL-3-X.                                  
           MOVE 0             TO NDXE-LVL-3-OCCUR.                              
           SET INDX-XXX TO +1.                                                  
           PERFORM 7000-SEARCH THRU                                             
                   7000-SEARCH-EXIT.                                            
                                                                                
           IF WS-FOUND                                                          
              GO TO 2200-LVL-2-PUT-EXIT                                         
           END-IF.                                                              
                                                                                
           MOVE WS-WORK-OFFSET       TO NDXE-PACKET-OFFSET.                     
           ADD +4                    TO WS-WORK-OFFSET.                         
                                                                                
           SET NDXE-WORK             TO TRUE.                                   
           PERFORM 8000-ADD-TO-INDEX THRU                                       
                   8000-ADD-TO-INDEX-EXIT.                                      
                                                                                
           MOVE WS-HOLD-ENTRY        TO INDEX-ENTRY.                            
           MOVE NDXE-LVL-2-ID        TO NDXE-PACKET-ID.                         
           MOVE LOW-VALUES           TO NDXE-LVL-3-ID                           
                                        NDXE-LVL-3-X.                           
           MOVE 0                    TO NDXE-LVL-3-OCCUR.                       
           MOVE INPT-GRP-LVL-2-OCCUR TO WS-TEMP-VALUE.                          
                                                                                
           PERFORM 2800-ADD-GROUP THRU                                          
                   2800-ADD-GROUP-EXIT.                                         
                                                                                
       2200-LVL-2-PUT-EXIT.                                                     
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SEARCH FOR EXISTENCE OF LEVEL 1 GROUP                                   
      *  - IF DOESN'T EXIST ADD IT TO INDEX                                     
      ****************************************************************          
       2300-LVL-1-PUT.                                                          
           PERFORM 2400-SECTION-PUT THRU                                        
                   2400-SECTION-PUT-EXIT.                                       
                                                                                
           MOVE WS-HOLD-ENTRY TO INDEX-ENTRY.                                   
                                                                                
           COMPUTE WS-HOLD-LENGTH = WS-WORK-OFFSET + 4.                         
           IF WS-HOLD-LENGTH > LENGTH OF COMM-WORK-AREA                         
              MOVE 44 TO PPUT-RETURN                                            
              GO TO 2300-LVL-1-PUT-EXIT                                         
           END-IF.                                                              
                                                                                
           MOVE '9999'        TO NDXE-PACKET-ID                                 
                                 COMM-WORK-AREA(WS-WORK-OFFSET:4).              
           MOVE HIGH-VALUES   TO NDXE-LVL-3-ID                                  
                                 NDXE-LVL-3-X                                   
                                 NDXE-LVL-2-ID.                                 
           MOVE 0             TO NDXE-LVL-3-OCCUR                               
                                 NDXE-LVL-2-OCCUR.                              
           SET INDX-XXX TO +1.                                                  
           PERFORM 7000-SEARCH THRU                                             
                   7000-SEARCH-EXIT.                                            
                                                                                
           IF WS-FOUND                                                          
              GO TO 2300-LVL-1-PUT-EXIT                                         
           END-IF.                                                              
                                                                                
           MOVE WS-WORK-OFFSET       TO NDXE-PACKET-OFFSET.                     
           ADD +4                    TO WS-WORK-OFFSET.                         
                                                                                
           SET NDXE-WORK             TO TRUE.                                   
           PERFORM 8000-ADD-TO-INDEX THRU                                       
                   8000-ADD-TO-INDEX-EXIT.                                      
                                                                                
           MOVE WS-HOLD-ENTRY        TO INDEX-ENTRY.                            
           MOVE LOW-VALUES           TO NDXE-LVL-3-ID                           
                                        NDXE-LVL-3-X                            
                                        NDXE-LVL-2-ID.                          
           MOVE 0                    TO NDXE-LVL-3-OCCUR                        
                                        NDXE-LVL-2-OCCUR.                       
           MOVE NDXE-LVL-1-ID        TO NDXE-PACKET-ID.                         
           MOVE INPT-GRP-LVL-1-OCCUR TO WS-TEMP-VALUE.                          
                                                                                
           PERFORM 2800-ADD-GROUP THRU                                          
                   2800-ADD-GROUP-EXIT.                                         
                                                                                
       2300-LVL-1-PUT-EXIT.                                                     
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SEARCH FOR EXISTENCE OF SECTION                                         
      *  - IF DOESN'T EXIST ADD IT TO INDEX                                     
      ****************************************************************          
       2400-SECTION-PUT.                                                        
           MOVE WS-HOLD-ENTRY   TO INDEX-ENTRY.                                 
                                                                                
           MOVE NDXE-SECTION-ID TO NDXE-PACKET-ID.                              
           MOVE HIGH-VALUES     TO NDXE-LVL-3-ID                                
                                   NDXE-LVL-3-X                                 
                                   NDXE-LVL-2-ID                                
                                   NDXE-LVL-1-ID.                               
           MOVE 0               TO NDXE-LVL-3-OCCUR                             
                                   NDXE-LVL-2-OCCUR                             
                                   NDXE-LVL-1-OCCUR.                            
                                                                                
           COMPUTE WS-HOLD-LENGTH = WS-WORK-OFFSET + 4.                         
           IF WS-HOLD-LENGTH > LENGTH OF COMM-WORK-AREA                         
              MOVE 44 TO PPUT-RETURN                                            
              GO TO 2400-SECTION-PUT-EXIT                                       
           END-IF.                                                              
                                                                                
           EVALUATE NDXE-SECTION-ID                                             
              WHEN WS-HEADER-START                                              
                 MOVE WS-HEADER-END  TO NDXE-PACKET-ID                          
                                     COMM-WORK-AREA(WS-WORK-OFFSET:4)           
              WHEN WS-BODY-START                                                
                 MOVE WS-BODY-END    TO NDXE-PACKET-ID                          
                                     COMM-WORK-AREA(WS-WORK-OFFSET:4)           
              WHEN WS-TRAILER-START                                             
                 MOVE WS-TRAILER-END TO NDXE-PACKET-ID                          
                                     COMM-WORK-AREA(WS-WORK-OFFSET:4)           
           END-EVALUATE.                                                        
                                                                                
           SET INDX-XXX TO +1.                                                  
           PERFORM 7000-SEARCH THRU                                             
                   7000-SEARCH-EXIT.                                            
                                                                                
           IF WS-FOUND                                                          
              GO TO 2400-SECTION-PUT-EXIT                                       
           END-IF.                                                              
                                                                                
           SET NDXE-WORK       TO TRUE.                                         
           MOVE WS-WORK-OFFSET TO NDXE-PACKET-OFFSET.                           
           ADD +4 TO WS-WORK-OFFSET.                                            
                                                                                
           PERFORM 8000-ADD-TO-INDEX THRU                                       
                   8000-ADD-TO-INDEX-EXIT.                                      
                                                                                
           MOVE WS-HOLD-ENTRY           TO INDEX-ENTRY.                         
           MOVE LOW-VALUES              TO NDXE-LVL-3-ID                        
                                           NDXE-LVL-3-X                         
                                           NDXE-LVL-2-ID                        
                                           NDXE-LVL-1-ID.                       
           MOVE 0                       TO NDXE-LVL-3-OCCUR                     
                                           NDXE-LVL-2-OCCUR                     
                                           NDXE-LVL-1-OCCUR.                    
           MOVE NDXE-SECTION-ID         TO NDXE-PACKET-ID.                      
           MOVE INPT-SECTION-OCCUR      TO WS-TEMP-VALUE.                       
           PERFORM 2800-ADD-GROUP THRU                                          
                   2800-ADD-GROUP-EXIT.                                         
                                                                                
       2400-SECTION-PUT-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * ADD PACKET TO INDEX                                                     
      ****************************************************************          
       2500-PACKET-PUT.                                                         
           MOVE WS-HOLD-ENTRY        TO INDEX-ENTRY.                            
           MOVE SPACE                TO NDXE-LVL-3-X.                           
           SET INDX-XXX TO +1.                                                  
           PERFORM 7000-SEARCH THRU                                             
                   7000-SEARCH-EXIT.                                            
                                                                                
           IF WS-FOUND                                                          
              MOVE +1                   TO INDX-PACKET-OFFSET(INDX-XXX)         
              SET INDX-AREA2(INDX-XXX)  TO TRUE                                 
           ELSE                                                                 
              MOVE +1                   TO NDXE-PACKET-OFFSET                   
              SET NDXE-AREA2            TO TRUE                                 
              PERFORM 8000-ADD-TO-INDEX THRU                                    
                      8000-ADD-TO-INDEX-EXIT                                    
           END-IF.                                                              
       2500-PACKET-PUT-EXIT.                                                    
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * ADD GROUP INFORMATION TO INDEX                                          
      ****************************************************************          
       2800-ADD-GROUP.                                                          
           MOVE WS-TEMP-VALUE TO WS-TEMP-VALUE-9.                               
           PERFORM VARYING XXX FROM 1 BY 1                                      
           UNTIL WS-TEMP-VALUE-9(XXX:1) NOT = '0'                               
           OR XXX > 9                                                           
           END-PERFORM.                                                         
                                                                                
TCB   *    COMPUTE WS-TEMP-LENGTH = (LENGTH OF WS-TEMP-VALUE - XXX + 1).        
           COMPUTE WS-TEMP-LENGTH = (10 - XXX).                                 
                                                                                
           MOVE WS-WORK-OFFSET TO NDXE-PACKET-OFFSET.                           
           SET NDXE-WORK       TO TRUE.                                         
                                                                                
           COMPUTE WS-HOLD-LENGTH = WS-WORK-OFFSET + 4.                         
           IF WS-HOLD-LENGTH > LENGTH OF COMM-WORK-AREA                         
              MOVE 44 TO PPUT-RETURN                                            
              GO TO 2800-ADD-GROUP-EXIT                                         
           END-IF.                                                              
                                                                                
           MOVE NDXE-PACKET-ID TO                                               
                      COMM-WORK-AREA(WS-WORK-OFFSET:4).                         
           MOVE WS-WORK-OFFSET TO NDXE-PACKET-OFFSET.                           
           ADD 4 TO WS-WORK-OFFSET.                                             
                                                                                
           IF WS-SECTION                                                        
              NEXT SENTENCE                                                     
           ELSE                                                                 
                                                                                
              COMPUTE WS-HOLD-LENGTH = WS-WORK-OFFSET                           
                                     + 5 + WS-TEMP-LENGTH                       
              IF WS-HOLD-LENGTH > LENGTH OF COMM-WORK-AREA                      
                 MOVE 44 TO PPUT-RETURN                                         
                 GO TO 2800-ADD-GROUP-EXIT                                      
              END-IF                                                            
                                                                                
              MOVE '20' TO COMM-WORK-AREA(WS-WORK-OFFSET:2)                     
              ADD 2 TO WS-WORK-OFFSET                                           
                                                                                
              MOVE WS-TEMP-LENGTH TO WS-TEMP-LENGTH-9                           
              MOVE WS-TEMP-LENGTH-9(7:3)                                        
                        TO COMM-WORK-AREA(WS-WORK-OFFSET:3)                     
              ADD 3 TO WS-WORK-OFFSET                                           
                                                                                
      *       ACTUAL CALCULATION IS 9 - WS-TEMP-LENGTH + 1                      
              MOVE                                                              
                WS-TEMP-VALUE-9(10 - WS-TEMP-LENGTH:WS-TEMP-LENGTH)             
                TO COMM-WORK-AREA(WS-WORK-OFFSET:WS-TEMP-LENGTH)                
              ADD WS-TEMP-LENGTH TO WS-WORK-OFFSET                              
           END-IF.                                                              
                                                                                
           PERFORM 8000-ADD-TO-INDEX THRU                                       
                   8000-ADD-TO-INDEX-EXIT.                                      
                                                                                
       2800-ADD-GROUP-EXIT.                                                     
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * REBUILD MESSAGE USING UPDATED INDEX                                     
      ****************************************************************          
       3000-REBUILD.                                                            
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
              MOVE PBMG-RETURN TO PPUT-RETURN                                   
           END-IF.                                                              
                                                                                
           SET COMM-REBUILD-INDEX TO TRUE.                                      
      *    SET COMM-INDEX-BUILT TO TRUE.                                        
       3000-REBUILD-EXIT.                                                       
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * SEQUENTIAL SEARCH FOR ITEM                                              
      ****************************************************************          
       7000-SEARCH.                                                             
           SET WS-NOT-FOUND    TO TRUE.                                         
                                                                                
           SEARCH INDX-PACKETS                                                  
              AT END                                                            
                 SET WS-ADD    TO TRUE                                          
              WHEN INDX-KEY(INDX-XXX) = NDXE-KEY                                
                 SET WS-FOUND  TO TRUE                                          
              WHEN INDX-KEY(INDX-XXX) > NDXE-KEY                                
                 SET WS-INSERT TO TRUE                                          
           END-SEARCH.                                                          
                                                                                
       7000-SEARCH-EXIT.                                                        
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * IF INDEX ENTRY FITS AT END OF INDEX, PERFORM ADD ROUTINE                
      * OTHERWISE PERFORM INSERT ROUTINE                                        
      ****************************************************************          
       8000-ADD-TO-INDEX.                                                       
           ADD 1        TO INDX-PACKET-COUNT.                                   
           IF INDX-PACKET-COUNT > 999                                           
              MOVE 42 TO PPUT-RETURN                                            
              GO TO 8000-ADD-TO-INDEX-EXIT                                      
           END-IF.                                                              
                                                                                
           IF WS-ADD                                                            
               SET INDX-XXX TO INDX-PACKET-COUNT                                
               MOVE INDEX-ENTRY TO INDX-PACKETS(INDX-PACKET-COUNT)              
           ELSE                                                                 
              PERFORM 8400-INSERT THRU                                          
                      8400-INSERT-EXIT                                          
           END-IF.                                                              
                                                                                
       8000-ADD-TO-INDEX-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * INSERT INDEX ENTRY INTO INDEX                                           
      ****************************************************************          
       8400-INSERT.                                                             
                                                                                
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
                    TO INDX-PACKETS-X(WS-MOVE-OFFSET:WS-MOVE-LENGTH).           
       8400-INSERT-EXIT.                                                        
           EXIT.                                                                
      /                                                                         
