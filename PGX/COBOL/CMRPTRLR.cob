       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    CMRPTRLR.                                                 
      *AUTHOR.        HEATHER BUERKLE.                                          
      *DATE-WRITTEN.  MAY, 1999.                                                
      *                                                                         
      *----------------------------------------------------------------*        
      *                                                                *        
      *  PROGRAM DESCRIPTION:                                          *        
      *      ADD TRAILER RECORD TO FILE PRIOR TO FTP TO NT             *        
      *      SERVER FOR CMRP PROCESSES.                                *        
      *                                                                *        
      *                 * * * * * * * * * * * * * * * *                *        
      *                                                                *        
      *  CALLED MODULES                                                *        
      *      GC2DATE  - DATE ROUTINE                                   *        
      *      GAEDATSR - DATA SERVER                                    *        
      *                                                                *        
      *  COPYBOOKS                                                     *        
      *      GARDATEP - DATE ROUTINE FIELDS                            *        
      *      GARDSVRB - VERBS FOR DATA SERVER                          *        
      *      CMRCTRLR - TRAILER RECORD LAYOUT                          *        
      *      CMRCGDG  - GENERATION RECORD LAYOUT                       *        
      *      ICBM     - CONTROL BLOCK TO CALL GAEDATSR                 *        
      *                                                                *        
      *   INPUT       - CMR EXTRACT FILE                               *        
      *                                                                *        
      *   OUTPUT      - CMR EXTRACT FILE WITH TRAILER RECORD           *        
      *                                                                *        
      *----------------------------------------------------------------*        
      *----------------------------------------------------------------*        
      *                                                                *        
      *  CHANGE LOG                                                    *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     NAME      DESCRIPTION                           *        
      *  --   -------  ----------------------------------------------  *        
      *                                                                *        
      *  01   MAY1999  BUERKHE   NEW PROGRAM.                          *        
      *                                                                *        
      *  02   AUG1999  CURRIKA   ALLOW FOR EMPTY INPUT FEED.           *        
      *                          ADD FILENAME AND JOBNAME TO TRAILER.  *        
      *                                                                *        
      *  03   OCT2008  ECU       COMPILER UPGRADE PROJECT              *        
      *----------------------------------------------------------------*        
      *                                                                         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
                                                                                
       WORKING-STORAGE SECTION.                                                 
      *                                                                         
       01 WS-CONSTANTS.                                                         
          05 WS-START                   PIC  X(46)  VALUE                       
           '*** CMRP001  WORKING STORAGE STARTS      *****'.                    
          05 WS-GAEDATSR                PIC  X(08) VALUE 'GAEDATSR'.            
          05 WS-CMRPTRLR                PIC  X(08) VALUE 'CMRPTRLR'.            
          05 WS-GC2DATE                 PIC  X(08) VALUE 'GC2DATE '.            
          05 WS-ZPARDUMP-FUNCTION       PIC  X(1)  VALUE '1'.                   
      *                                                                         
       01  WS-COUNTERS.                                                         
           05  WS-REC-COUNT              PIC  9(08) COMP-3 VALUE 0.             
      *                                                                         
       01  WS-MISC.                                                             
           05  WS-GAEDATSR-VERB            PIC X(16)  VALUE SPACES.             
      *                                                                         
       01  WTL-MESSAGE-PARAMETERS.                                              
           05  WTL-MESSAGES.                                                    
               10  WTL-MESS-LENGTH         PIC S9(3)  VALUE +133 COMP-3.        
               10  WTL-MESS-CC             PIC X(1)   VALUE SPACES.             
               10  WTL-MESS-TEXT           PIC X(132) VALUE SPACES.             
           05  WTL-FLAG                    PIC X(1)   VALUE 'B'.                
           05  WTL-REPLY                   PIC X(3)   VALUE 'NO '.              
           05  WTL-REPLY-AREA              PIC X(80)  VALUE SPACES.             
                                                                                
       01  WTL-MESSAGE-LIST.                                                    
           05  WTL-1                       PIC X(80)  VALUE                     
               'CMRPTRLR - PROGRAM IS ABENDING                        '.        
           05  WTL-2                       PIC X(80)  VALUE                     
               'CMRPTRLR - PROBLEM READING EXTRACT FILE               '.        
           05  WTL-3                       PIC X(80)  VALUE                     
               'CMRPTRLR - ERROR READING CONTROL CARD                 '.        
           05  WTL-4                       PIC X(80)  VALUE                     
               'CMRPTRLR - ERROR WRITING TO EXTRACT FILE              '.        
           05  WTL-5                       PIC X(80)  VALUE                     
               'CMRPTRLR - ERROR WRITING TRAILER RECORD               '.        
           05  WTL-6                       PIC X(80)  VALUE                     
               'CMRPTRLR - EXPECTED GDG NUMBER WAS NOT READ           '.        
           05  WTL-7                       PIC X(80)  VALUE                     
               'CMRPTRLR - ERROR WRITING OUT EXPECTED GENERATION      '.        
                                                                                
       01  WTL-MESSAGE-TABLE REDEFINES WTL-MESSAGE-LIST.                        
           05  WTL-MESSAGE-ENTRY OCCURS 7 TIMES.                                
               10  WTL-MESSAGE             PIC X(80).                           
                                                                                
       01  WTL-SUB                         PIC S9(4)  VALUE +0  COMP.           
                                                                        00800069
      *                                                                         
      *-----------------------------------------------------------------        
      *    DATE ROUTINE PARAMETERS                                              
      *-----------------------------------------------------------------        
       01  GAC-DATE-PARAMETERS.                                                 
           COPY GARDATEP.                                                       
      *-----------------------------------------------------------------        
      *    EXTRACT SPACE.  THE EXTRACT AREA IS DEFINED AS 32476 TO              
      *    ALLOW FOR LARGER RECORD LENGTH FEEDS IN THE FUTURE IF IT             
      *    IS NECESARY.  THIS METHOD OF READING DIFFERENT RECORD                
      *    LENGTHS INTO A LARGE AREA COMES FROM THE ESI PROGRAMS:               
      *    INCCDEE1 AND INCGOCOM WHICH FUNCTION SIMILARLY.                      
      *-----------------------------------------------------------------        
       01  EXTRACT-DATA-AREA               PIC X(32476).                        
      *-----------------------------------------------------------------        
      *    TRAILER RECORD LAYOUT                                                
      *-----------------------------------------------------------------        
       01  CMR-TRAILER-REC.                                                     
           COPY CMRCTRLR.                                                       
      *-----------------------------------------------------------------        
      *    CURRENT GENERATION RECORD LAYOUT                                     
      *-----------------------------------------------------------------        
       01  CURR-GENERATION.                                                     
           COPY CMRCGDG.                                                        
      *-----------------------------------------------------------------        
      *    EXPECTED GENERATION RECORD LAYOUT                                    
      *-----------------------------------------------------------------        
       01  EXPECTED-GENERATION.                                                 
           COPY CMRCGDG.                                                        
      *-----------------------------------------------------------------        
      *    CONTROL CARD INFO (CONTAINS JOBNAME AND FILENAME)                    
      *-----------------------------------------------------------------        
       01  CMR-ADDITIONAL-INFO.                                                 
           COPY CMRCINFO.                                                       
      *-----------------------------------------------------------------        
      *    ACTION VERBS USED TO CALL GAEDATSR                                   
      *-----------------------------------------------------------------        
       01  DATA-SERVER-VERBS.                                                   
           COPY GARDSVRB.                                                       
      *-----------------------------------------------------------------        
      * ICBM                                                                    
      *-----------------------------------------------------------------        
       01  ICBM-AREA.                                                           
           COPY ICBM.                                                           
                                                                                
       01  WS-END                          PIC X(24) VALUE                      
           'CMRPTRLR MWS ENDS  HERE '.                                          
       01  WS-END-BYTE-X                   PIC X(1)  VALUE SPACES.              
           EJECT                                                                
      *                                                                         
       PROCEDURE DIVISION.                                                      
      *----------------------------------------------------------------*        
      *    MAINLINE.                                                   *        
      *----------------------------------------------------------------*        
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INITIALIZATION THRU 1000-INITIALIZATION-EXIT.           
                                                                                
           PERFORM 2000-PROCESS-RECORDS THRU 2000-PROCESS-RECORDS-EXIT          
                   UNTIL LR-NOT-FOUND OR                                        
                   NOT LR-STATUS-OK.                                            
                                                                                
           PERFORM 1900-FINALIZATION    THRU 1900-FINALIZATION-EXIT.    02060069
                                                                        02070069
       0000-MAINLINE-EXIT.                                              02080069
           STOP RUN.                                                    02120069
                                                                        02130069
                                                                                
      *----------------------------------------------------------------*        
      *    INITIALIZE THE PROGRAM VARIABLES.                           *        
      *----------------------------------------------------------------*        
       1000-INITIALIZATION.                                                     
                                                                                
           INITIALIZE ICBM-AREA.                                                
                                                                                
           MOVE LOW-VALUES                TO LINKAGE-CONTROL.                   
           MOVE SPACES                    TO EXTRACT-DATA-AREA.                 
           MOVE WS-CMRPTRLR               TO ICBM-PROGRAM-NAME.         02210069
                                                                                
           MOVE OBTAIN-FIRST              TO WS-GAEDATSR-VERB.                  
      *----------------------------------------------------------------*        
      *    READ THE CURRENT GENERATION OF THE EXTRACT FILE.            *        
      *----------------------------------------------------------------*        
           PERFORM 9010-READ-CURR-GENERATION THRU                               
                   9010-READ-CURR-GENERATION-EXIT.                              
                                                                                
      *----------------------------------------------------------------*        
      *    READ THE EXPECTED GENERATION OF THE EXTRACT FILE.           *        
      *----------------------------------------------------------------*        
           PERFORM 9020-READ-EXP-GENERATION THRU                                
                   9020-READ-EXP-GENERATION-EXIT.                               
                                                                                
           IF  CMR-GENERATION-NUMBER OF CURR-GENERATION NOT =                   
               CMR-GENERATION-NUMBER OF EXPECTED-GENERATION                     
               MOVE +6                    TO WTL-SUB                            
               GO TO 9999-ABANDON-SHIP.                                         
                                                                                
      *----------------------------------------------------------------*        
      *    READ THE CONTROL CARD WITH THE JOB AND FILE NAME.           *        
      *----------------------------------------------------------------*        
           PERFORM 9030-READ-CONTROL-CARD THRU                                  
                   9030-READ-CONTROL-CARD-EXIT.                                 
                                                                                
           IF  LR-NOT-FOUND OR NOT LR-STATUS-OK                                 
               MOVE +3                    TO WTL-SUB                            
               GO TO 9999-ABANDON-SHIP.                                         
                                                                                
      *----------------------------------------------------------------*        
      *    READ THE FIRST RECORD FROM THE EXTRACT.                     *        
      *----------------------------------------------------------------*        
           PERFORM 9000-READ-EXTRACT THRU                                       
                   9000-READ-EXTRACT-EXIT.                                      
                                                                                
           IF  LR-NOT-FOUND                                                     
               NEXT SENTENCE                                                    
           ELSE                                                                 
           IF NOT LR-STATUS-OK                                                  
               MOVE +2                    TO WTL-SUB                            
               GO TO 9999-ABANDON-SHIP.                                         
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      *    SET UP THE CURRENT DATE AND TIME.                           *        
      *----------------------------------------------------------------*        
           MOVE 'EA1E' TO VDATE-REQUEST-AREA.                                   
           CALL WS-GC2DATE USING GAC-DATE-PARAMETERS.                           
                                                                                
           MOVE VDATE1-YYYYMMDD TO CMR-TRAILER-RUN-DATE.                        
           ACCEPT CMR-TRAILER-RUN-TIME FROM TIME.                               
                                                                                
      *----------------------------------------------------------------*        
      *    SET UP THE VERB FOR SUBSEQUENT READS.                       *        
      *----------------------------------------------------------------*        
           MOVE OBTAIN-NEXT               TO WS-GAEDATSR-VERB.                  
                                                                                
       1000-INITIALIZATION-EXIT.                                                
           EXIT.                                                                
                                                                                
       1900-FINALIZATION.                                                  CL*14
                                                                                
      *----------------------------------------------------------------*        
      *    WRITE OUT TRAILER RECORD AND CLOSE THE FILE.                *        
      *----------------------------------------------------------------*        
                                                                                
           ADD 1 TO WS-REC-COUNT.                                               
           MOVE WS-REC-COUNT      TO   CMR-TRAILER-REC-COUNT.                   
           MOVE CMR-GENERATION    OF   CURR-GENERATION                          
                                  TO   CMR-TRAILER-GDG.                         
           MOVE CMR-INFO-JOBNAME  TO   CMR-TRAILER-JOBNAME.                     
           MOVE CMR-INFO-FILENAME TO   CMR-TRAILER-FILENAME.                    
                                                                                
           PERFORM 9200-WRITE-TRAILER THRU                                      
                   9200-WRITE-TRAILER-EXIT.                                     
                                                                                
           MOVE SPACES            TO   EXTRACT-DATA-AREA.                       
           CALL WS-GAEDATSR            USING FINISH-LR                          
                                             EXTRACT-DATA-AREA                  
                                             ICBM-AREA.                         
                                                                                
           IF  NOT LR-STATUS-OK                                                 
               DISPLAY 'CMRPTRLR: ERROR CLOSING FILES WITH GAEDATSR'            
               DISPLAY ICBM-AREA                                                
           END-IF.                                                              
                                                                             CL*
      *----------------------------------------------------------------*        
      *    INCREMENT THE EXPECTED GENERATION NUMBER.                   *        
      *----------------------------------------------------------------*        
           ADD 1 TO CMR-GENERATION-NUMBER OF EXPECTED-GENERATION.               
                                                                             CL*
           PERFORM 9300-WRITE-EXP-GENERATION THRU                               
                   9300-WRITE-EXP-GENERATION-EXIT.                              
                                                                                
       1900-FINALIZATION-EXIT.                                             CL*  
           EXIT.                                                           CL*  
                                                                                
       2000-PROCESS-RECORDS.                                                    
                                                                                
           ADD 1 TO WS-REC-COUNT.                                               
      *----------------------------------------------------------------*        
      *    WRITE OUT THE EXTRACT RECORD                                *        
      *----------------------------------------------------------------*        
           PERFORM 9100-WRITE-EXTRACT THRU                                      
                   9100-WRITE-EXTRACT-EXIT.                                     
                                                                                
      *----------------------------------------------------------------*        
      *    READ THE NEXT RECORD                                        *        
      *----------------------------------------------------------------*        
           MOVE SPACES               TO EXTRACT-DATA-AREA.                      
           PERFORM 9000-READ-EXTRACT THRU                                       
                   9000-READ-EXTRACT-EXIT.                                      
                                                                                
                                                                                
       2000-PROCESS-RECORDS-EXIT.                                               
           EXIT.                                                                
                                                                                
       9000-READ-EXTRACT.                                                       
      *----------------------------------------------------------------*        
      *    READ RECORD ON THE EXTRACT FILE                             *        
      *----------------------------------------------------------------*        
                                                                                
           MOVE 'CARD-DATA-010'           TO LOGICAL-RECORD-NAME.               
           CALL WS-GAEDATSR            USING WS-GAEDATSR-VERB                   
                                             EXTRACT-DATA-AREA                  
                                             ICBM-AREA.                         
                                                                                
       9000-READ-EXTRACT-EXIT.                                                  
           EXIT.                                                                
                                                                                
       9010-READ-CURR-GENERATION.                                               
      *----------------------------------------------------------------*        
      *    READ THE CURRENT GENERATION OF THE EXTRACT FILE.            *        
      *----------------------------------------------------------------*        
                                                                                
           MOVE 'CARD-DATA-071'           TO LOGICAL-RECORD-NAME.               
           CALL WS-GAEDATSR            USING WS-GAEDATSR-VERB                   
                                             CURR-GENERATION                    
                                             ICBM-AREA.                         
                                                                                
       9010-READ-CURR-GENERATION-EXIT.                                          
           EXIT.                                                                
                                                                                
       9020-READ-EXP-GENERATION.                                                
      *----------------------------------------------------------------*        
      *    READ THE EXPECTED GENERATION OF THE EXTRACT FILE.           *        
      *----------------------------------------------------------------*        
                                                                                
           MOVE 'CARD-DATA-072'           TO LOGICAL-RECORD-NAME.               
           CALL WS-GAEDATSR            USING WS-GAEDATSR-VERB                   
                                             EXPECTED-GENERATION                
                                             ICBM-AREA.                         
                                                                                
       9020-READ-EXP-GENERATION-EXIT.                                           
           EXIT.                                                                
                                                                                
       9030-READ-CONTROL-CARD.                                                  
      *----------------------------------------------------------------*        
      *    READ THE EXPECTED GENERATION OF THE EXTRACT FILE.           *        
      *----------------------------------------------------------------*        
                                                                                
           MOVE 'CARD-DATA-013'           TO LOGICAL-RECORD-NAME.               
           CALL WS-GAEDATSR            USING WS-GAEDATSR-VERB                   
                                             CMR-ADDITIONAL-INFO                
                                             ICBM-AREA.                         
                                                                                
       9030-READ-CONTROL-CARD-EXIT.                                             
           EXIT.                                                                
                                                                                
       9100-WRITE-EXTRACT.                                                      
      *----------------------------------------------------------------*        
      *    WRITE RECORD TO THE EXTRACT FILE.                           *        
      *----------------------------------------------------------------*        
                                                                                
           MOVE 'PRINT-DATA-020'          TO LOGICAL-RECORD-NAME.               
           CALL WS-GAEDATSR            USING STORE-LR                           
                                             EXTRACT-DATA-AREA                  
                                             ICBM-AREA.                         
                                                                                
           IF  NOT LR-STATUS-OK                                                 
               MOVE +4                    TO WTL-SUB                            
               GO TO 9999-ABANDON-SHIP.                                         
                                                                                
       9100-WRITE-EXTRACT-EXIT.                                                 
           EXIT.                                                                
                                                                                
       9200-WRITE-TRAILER.                                                      
      *----------------------------------------------------------------*        
      *    WRITE TRAILER RECORD TO THE EXTRACT FILE                    *        
      *----------------------------------------------------------------*        
           MOVE SPACES                    TO EXTRACT-DATA-AREA.                 
           MOVE CMR-TRAILER-REC           TO EXTRACT-DATA-AREA.                 
                                                                                
           MOVE 'PRINT-DATA-020'          TO LOGICAL-RECORD-NAME.               
           CALL WS-GAEDATSR            USING STORE-LR                           
                                             EXTRACT-DATA-AREA                  
                                             ICBM-AREA.                         
                                                                                
           IF  NOT LR-STATUS-OK                                                 
               MOVE +5                    TO WTL-SUB                            
               GO TO 9999-ABANDON-SHIP.                                         
                                                                                
       9200-WRITE-TRAILER-EXIT.                                                 
           EXIT.                                                                
                                                                                
       9300-WRITE-EXP-GENERATION.                                               
      *----------------------------------------------------------------*        
      *    WRITE OUT A NEW EXPECTED GENERATION FILE.                   *        
      *----------------------------------------------------------------*        
           MOVE 'PRINT-DATA-070'          TO LOGICAL-RECORD-NAME.               
           CALL WS-GAEDATSR            USING STORE-LR                           
                                             EXPECTED-GENERATION                
                                             ICBM-AREA.                         
                                                                                
           IF  NOT LR-STATUS-OK                                                 
               MOVE +7                    TO WTL-SUB                            
               GO TO 9999-ABANDON-SHIP.                                         
                                                                                
       9300-WRITE-EXP-GENERATION-EXIT.                                          
           EXIT.                                                                
                                                                                
       9999-ABANDON-SHIP.                                                       
      *----------------------------------------------------------------*        
      *  SOMETHING SERIOUS HAS OCCURRED WHICH SHOULD STOP THE PROGRAM. *        
      *----------------------------------------------------------------*        
           MOVE WTL-MESSAGE (WTL-SUB)     TO WTL-MESS-TEXT.                     
           CALL 'WTL' USING  WTL-MESSAGES                                       
                             WTL-FLAG                                           
                             WTL-REPLY                                          
                             WTL-REPLY-AREA.                                    
           MOVE WTL-MESSAGE (+1)          TO WTL-MESS-TEXT.                     
           CALL 'WTL' USING  WTL-MESSAGES                                       
                             WTL-FLAG                                           
                             WTL-REPLY                                          
                             WTL-REPLY-AREA.                                    
           CALL 'ZPARDUMP' USING WS-ZPARDUMP-FUNCTION                           
                                 WS-START                                       
                                 WS-END-BYTE-X.                                 
