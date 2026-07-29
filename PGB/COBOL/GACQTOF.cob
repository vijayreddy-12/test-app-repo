       IDENTIFICATION DIVISION.                                                 
GNE                                                                             
       PROGRAM-ID.    GACQTOF.                                                  
      *AUTHOR.        GRAHAM WRIGHT.                                            
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.                                                            
      *DATE-COMPILED.                                                           
      *                                                                *        
      *----------------------------------------------------------------*        
      *                                                                *        
      *  SYSTEM    : GROUP ELIGIBILITY                                 *        
      *                           - QUEUE TO FILE PROCESS              *        
      *  LANGUAGE  : COBOL II                                          *        
      *                                                                *        
      *  TYPE      : BATCH                                             *        
      *                                                                *        
      *  COPYBOOKS : MLX2PRSI - V2 PARSER CONTROL AREA                 *        
      *            : MLX2PRSO - V2 PARSER RETURN AREA                  *        
      *            : GDYNALLO - DYNAMIC FILE ALLOCATION                *        
      *            : CMQV     - MQ COPYBOOK                            *        
      *            : CMQODV   - MQ COPYBOOK                            *        
      *            : CMQMDV   - MQ COPYBOOK                            *        
      *            : CMQGMOV  - MQ COPYBOOK                            *        
      *            : CMQPMOV  - MQ COPYBOOK                            *        
      *            : MLMV2HDR - V2 MLI FIXED MESSAGE HEADER DEFINITION *        
      *                                                                *        
      *  CALLS     : GAEDATSR - DATA SERVER                            *        
      *            : GDYNALLO - DYNAMIC FILE ALLOCATION                *        
      *            : MLX2GET  - PARSER GET FROM V2 TAGGED STREAM       *        
      *            : MLX2PUT  - PARSER PUT TO V2 TAGGED STREAM         *        
      *            : MLX2CLR  - PARSER CLEAR A V2 TAGGED STREAM        *        
      *            : CSQBCONN - CONNECT TO QUEUE MANAGER               *        
      *            : CSQBDISC - DISCONNECT FROM QUEUE MANAGER          *        
      *            : CSQBOPEN - OPEN QUEUE                             *        
      *            : CSQBCLOS - CLOSE QUEUE                            *        
      *            : CSQBGET  - READ MESSAGE FROM QUEUE                *        
      *            : CSQBPUT  - WRITE MESSAGE TO QUEUE                 *        
      *                                                                *        
      ******************************************************************        
      *   DIAGRAM OF PROCESS - (MAINFRAME-CICS IS ANOTHER JOB, WE      *        
      *                        (READ THE Q THAT IS INPUT + BUILD)      *        
      *                        (FILE & SEND MSG TO REPLY QUEUE.).      *        
      ******************************************************************        
      *                   ___________                                  *        
      *                  |           |                                 *        
      *                  |           |------------------               *        
      *                  | MAINFRAME |                 |               *        
      *                  |   (CICS)  |                 |               *        
      *                  |___________|                 |               *        
      *                                                |               *        
      *                    ___________                 |               *        
      * REQUEST _   _     |           |              _ \/_             *        
      *    TX    |_|----->|QUEUETOFILE|<------------- |_|              *        
      *                   |___________|           DYNAMIC REPLY        *        
      *                     |       |             (AT PROCESS END...   *        
      *   REPLY _   _       |       |              IF RECORD COUNT =   *        
      *    TX    |_|<--------       |              EXPECTED COUNT      *        
      *                             |                  THEN            *        
      *                       ______\/_____        DELETE QUEUE)       *        
      *                       |===========|                            *        
      *           FTP         |  DYNAMIC  |                            *        
      *  <--------------------|   FILE    |                            *        
      *                       |===========|                            *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *  INPUT     : MQ TRIGGER MESSAGE                                *        
      *            : MQ DYNAMIC REPLY Q (OUTPUT FROM MAINFRAME PROCESS)*        
      *                                                                *        
      *  OUTPUT    : MQ OUTPUT REQUEST Q                               *        
      *            : TRANSACTION FILE (ALLOCATED)                      *        
      *            : MQ PROCESSING FINISHED MESSAGE                    *        
      *              -  MQ REPLY Q TO ORIGINAL TRIGGER MSG.            *        
      *                                                                *        
      *--HISTORY LOG---------------------------------------------------*        
      *  SEQ  DATE       DESIGNER   DESCRIPTION                        *        
      *  ---  ---------  ---------  -----------------------------------*        
      *  001  JUL 1998   G WRIGHT   CREATED                            *        
      *  002  AUG 1998   F MUELLER  GENERIC PARM REQUIRED TO SPECIFY   *        
      *                             MQ SERIES QUEUE MANAGER AND REQUESTQ        
      *  003  AUG 1998   F MUELLER  IF REQUEST SHOULD ENCOUNTER FATAL  *        
      *                             ERROR, STOP PROCESSING IMMEDIATELY *        
      *                             INSTEAD OF PROCESSING TO THE END OF*        
      *                             THE REQUEST QUEUE. ALSO, REPLY QUEUE        
      *                             SHOULD BE READ AND SYNCPOINTED AFTER        
      *                             REPLY MESSAGE IS WRITTEN TO FILE.  *        
      *----------------------------------------------------------------*        
      *  004  SEP 1998   F MUELLER  QUEUE TO FILE PROCESS CHANGED TO   *        
      *                             HANDLE INPUT FROM TRIGGER REQUEST  *        
      *                             MESSAGE OR JOB SPECIFIC PARAMETERS.*        
      *                             GENERIC LEADCARD DATA RETRIEVAL TO *        
      *                             INCLUDE REPLYTOQ, REPLYTOQ MANAGER,*        
      *                             ACTION, AND DATASET ALLOC INFO.    *        
      *                           *------------------------------------*        
      *                           * UPGRADED TO USE VERSION 2 PARSER.  *        
      *                           *------------------------------------*        
      *----------------------------------------------------------------*        
      *  005  APR 1999   F MUELLER  BUSINESS CONTINUITY - BUG #9       *        
      *                             ADDED NEW OPTION FOR MLI VERSION 2 *        
      *                             MESSAGES THAT WILL CONDITION UPDATE*        
      *                             TO ORIGINALRECEIVEDDATE/TIME MSG   *        
      *                             HEADER PROPERTIES USING THE MQMD   *        
      *                             PUT DATE/TIME ON THE MESSAGE.      *        
      *----------------------------------------------------------------*        
      * 006   AUG 2008   IBM GR     UPGRADED IN ECU PROJECT                     
      *---------------------------------------------------------------*         
      *----------------------------------------------------------------*        
                                                                                
       ENVIRONMENT DIVISION.                                                    
                                                                                
       CONFIGURATION SECTION.                                                   
                                                                                
       SOURCE-COMPUTER. IBM-370.                                                
       OBJECT-COMPUTER. IBM-370.                                                
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                                 
       01  FILLER                     PIC X(40) VALUE                           
               '**   GACQTOF WORKING STORAGE BEGINS  **'.                       
                                                                                
       01  WS-FINAL-RETURN-CODE           PIC 9(02).                            
       01  WS-RETURN-CODE                 PIC 9(02).                            
           88  SUCCESSFUL-COMPLETION               VALUE 00.                    
           88  EXPECTED-LEADCARD-MISSING           VALUE 12.                    
           88  LEADCARD-FILE-READ-ERROR            VALUE 16.                    
           88  LEADCARD-FILE-CLOSE-ERROR           VALUE 16.                    
           88  QUEUE-MGR-CONNECT-FAILED            VALUE 30.                    
           88  QUEUE-MGR-DISCONNECT-FAILED         VALUE 31.                    
           88  REQUEST-QUEUE-OPEN-FAILED           VALUE 32.                    
           88  REQUEST-QUEUE-READ-ERROR            VALUE 33.                    
           88  REQUEST-QUEUE-CLOSE-FAILED          VALUE 34.                    
           88  QUEUE-MGR-COMMIT-FAILED             VALUE 35.                    
           88  REPLY-QUEUE-OPEN-FAILED             VALUE 36.                    
           88  REPLY-QUEUE-WRITE-FAILED            VALUE 37.                    
           88  REPLY-QUEUE-CLOSE-FAILED            VALUE 38.                    
           88  STREAM-LENGTH-ERROR                 VALUE 39.                    
           88  PARSER-FILE-OPEN-FAILED             VALUE 40.                    
           88  PARSER-RETRIEVE-ERROR               VALUE 41.                    
           88  PARSER-FILE-CLOSE-FAILED            VALUE 42.                    
           88  PARSER-UPDATE-ERROR                 VALUE 43.                    
           88  MF-FILE-READ-ERROR                  VALUE 44.                    
           88  MF-FILE-CLOSE-ERROR                 VALUE 45.                    
           88  REPLY-QUEUE-PUT-ERROR               VALUE 46.                    
           88  DYNAMIC-ALLOCATION-ERROR            VALUE 48.                    
           88  DYNAMIC-UNALLOCATION-ERROR          VALUE 49.                    
           88  MESSAGE-VERSION-ERROR               VALUE 50.                    
      *************************************************************             
      * NEED CODES FOR DYNAMIC REPLY Q + OUTPUT REQUEST Q + PRIOR Q             
      *************************************************************             
           88  OUTPUT-QUEUE-OPEN-FAILED            VALUE 51.                    
           88  OUTPUT-QUEUE-PUT-ERROR              VALUE 52.                    
           88  OUTPUT-QUEUE-CLOSE-FAILED           VALUE 53.                    
           88  INPUT-QUEUE-OPEN-FAILED             VALUE 54.                    
           88  INPUT-QUEUE-READ-ERROR              VALUE 55.                    
           88  INPUT-QUEUE-CLOSE-FAILED            VALUE 56.                    
           88  INPUT-QUEUE-DELETE-FAILED           VALUE 57.                    
           88  MF-FILE-WRITE-ERROR                 VALUE 60.                    
                                                                                
       01  WS-LEADCARD-TABLE.                                                   
           05  WS-LEADCARD-TOKEN          PIC X(30).                            
           05  WS-LEADCARD-DETAIL         OCCURS 4 TIMES.                       
               10 WS-LEADCARD-SIZE        PIC S9(4) COMP.                       
               10 WS-LEADCARD-DATA        PIC X(48).                            
                                                                                
       01  WS-PACKET-CONSTANTS.                                                 
           05  WS-FIELD-PACKETS.                                                
               10  WS-FLD-OUTPUT-DSN      PIC X(04) VALUE '0576'.               
               10  WS-FLD-SPACE-TYPE      PIC X(04) VALUE '1300'.               
               10  WS-FLD-PRIMARY-SPACE   PIC X(04) VALUE '1301'.               
               10  WS-FLD-SECONDARY-SPACE PIC X(04) VALUE '1302'.               
               10  WS-FLD-UNIT-NAME       PIC X(04) VALUE '1306'.               
               10  WS-FLD-DSORG           PIC X(04) VALUE '1308'.               
               10  WS-FLD-DISP-STATUS     PIC X(04) VALUE '1309'.               
               10  WS-FLD-DISP-NORMAL     PIC X(04) VALUE '1310'.               
               10  WS-FLD-DISP-ABEND      PIC X(04) VALUE '1311'.               
               10  WS-FLD-RETURN-CODE     PIC X(04) VALUE '1312'.               
               10  WS-FLD-RECORD-COUNT    PIC X(04) VALUE '1313'.               
               10  WS-FLD-QUEUE-ACTION    PIC X(04) VALUE '1314'.               
               10  WS-FLD-REPLY-QUEUE     PIC X(04) VALUE '1322'.               
                                                                                
       01  WS-VARIABLES.                                                        
           05  SUB                        PIC 9(02).                            
           05  WS-PARSER-GET-ERR-CODE     PIC 9(02).                            
               88 TAG-TRUNCATED           VALUE 04.                             
               88 TAG-NOT-FOUND           VALUE 08.                             
           05  WS-PARSER-PUT-ERR-CODE     PIC 9(02).                            
           05  WS-PARSE-ERR-PGM           PIC X(08).                            
           05  WS-GAEDATSR-VERB           PIC X(16).                            
           05  WS-MF-FILE-NAME            PIC X(54).                            
           05  WS-REQUEST-QMGRNAME        PIC X(48).                            
           05  WS-REQUEST-QNAME           PIC X(48).                            
           05  WS-REPLY-QMGRNAME          PIC X(48).                            
           05  WS-REPLY-QNAME             PIC X(48).                            
           05  WS-REQUEST-MSGID           PIC X(24).                            
           05  WS-REQUEST-MESSAGE         PIC X(16000).                         
           05  WS-REPLY-MSG-VERSION       REDEFINES WS-REQUEST-MESSAGE          
                                          PIC X(06).                            
           05  FILLER                     REDEFINES WS-REPLY-MSG-VERSION        
                                          PIC 9(04)V99.                         
               88  REPLY-MSG-VERSION-2    VALUE 2.                              
               88  REPLY-MSG-VERSION-1    VALUE 1.                              
           05  WS-REQUEST-MESSAGE-LENGTH  PIC S9(04) COMP.                      
           05  WS-INPUT-QMGRNAME          PIC X(48).                            
           05  WS-INPUT-QNAME             PIC X(48).                            
           05  WS-INPUT-QACTION           PIC X(03).                            
               88  DELETE-PURGE-Q         VALUE 'PU '.                          
               88  DELETE-PURGE-Q-EQ      VALUE 'PE '.                          
               88  DELETE-PURGE-Q-GE      VALUE 'PGE'.                          
           05  WS-TS-OPTION               PIC X(06).                            
               88  UPDATE-ORIG-TS         VALUE 'UPDATE'.                       
               88  RETAIN-ORIG-TS         VALUE 'RETAIN'.                       
           05  WS-IN-RECORD-COUNT         PIC 9(9).                             
           05  WS-RECNO-X.                                                      
               10  WS-RECNO               PIC S9(08) COMP.                      
                                                                                
       01  WS-INDICATORS.                                                       
           05  PROCESS-IND                PIC X(01).                            
               88  LEADCARD-REQUEST                 VALUE 'N'.                  
               88  QUEUED-MSG-REQUEST               VALUE 'Y'.                  
           05  ERROR-IND                  PIC X(01).                            
               88  NO-ERRORS                        VALUE 'N'.                  
               88  FATAL-ERROR                      VALUE 'Y'.                  
           05  PARSE-ERROR-IND            PIC X(01).                            
               88  PARSE-NO-ERRORS                  VALUE ' '.                  
               88  PARSE-WARNINGS                   VALUE 'W'.                  
               88  PARSE-ERRORS                     VALUE 'E'.                  
           05  REQUEST-Q-EMPTY-IND        PIC X(01).                            
               88  REQUEST-Q-NOT-EMPTY              VALUE 'N'.                  
               88  REQUEST-Q-EMPTY                  VALUE 'Y'.                  
           05  INPUT-Q-EMPTY-IND          PIC X(01).                            
               88  INPUT-Q-NOT-EMPTY                VALUE 'N'.                  
               88  INPUT-Q-EMPTY                    VALUE 'Y'.                  
           05  MF-FILE-IND                PIC X(01).                            
               88  MF-FILE-CLOSED                   VALUE 'N'.                  
               88  MF-FILE-OPEN                     VALUE 'Y'.                  
           05  MF-ALLOC-IND               PIC X(01).                            
               88  MF-FILE-UNALLOC                  VALUE 'N'.                  
               88  MF-FILE-ALLOC                    VALUE 'Y'.                  
           05  REPLY-MSG-IND              PIC X(01).                            
               88  REPLY-MSG-NOT-EXPECTED           VALUE 'N'.                  
               88  REPLY-MSG-EXPECTED               VALUE 'Y'.                  
                                                                                
       01  WS-CALLED-MODULES.                                                   
           05  GAEDATSR                   PIC X(08) VALUE 'GAEDATSR'.           
           05  PARSER-PUT                 PIC X(08) VALUE 'MLX2PUT '.           
           05  PARSER-GET                 PIC X(08) VALUE 'MLX2GET '.           
           05  PARSER-CLEAR               PIC X(08) VALUE 'MLX2CLR '.           
           05  PARSER-TAGGED-TO-COPY      PIC X(08) VALUE 'MLX2TTOC'.           
           05  PARSER-COPY-TO-TAGGED      PIC X(08) VALUE 'MLX2CTOT'.           
                                                                                
       01  WS-MQ-CALLING-MODULES.                                               
           05  MQCMIT-BATCH               PIC X(08) VALUE 'CSQBCOMM'.           
           05  MQCONN-BATCH               PIC X(08) VALUE 'CSQBCONN'.           
           05  MQDISC-BATCH               PIC X(08) VALUE 'CSQBDISC'.           
           05  MQOPEN-BATCH               PIC X(08) VALUE 'CSQBOPEN'.           
           05  MQCLOSE-BATCH              PIC X(08) VALUE 'CSQBCLOS'.           
           05  MQGET-BATCH                PIC X(08) VALUE 'CSQBGET '.           
           05  MQPUT-BATCH                PIC X(08) VALUE 'CSQBPUT '.           
           05  MQPUT1-BATCH               PIC X(08) VALUE 'CSQBPUT1'.           
                                                                                
      *-----------------------------------------------------------------        
      * MAINFRAME FILE RECORD LAYOUT                                            
      *-----------------------------------------------------------------        
       01  MF-RECORD.                                                           
           10  OUTPUT-LENGTH               PIC S9(04) COMP.                     
           10  FILLER                      PIC X(02).                           
           10  OUTPUT-RECORD               PIC X(16000) VALUE SPACES.           
                                                                                
      *-----------------------------------------------------------------        
      * LEADCARD FILE RECORD LAYOUT                                             
      *-----------------------------------------------------------------        
       01  LEADCARD.                                                            
           10  LEADCARD-RECORD            PIC X(80).                            
                                                                                
      *-----------------------------------------------------------------        
      *    PARSER CONTROL AREA                                                  
      *-----------------------------------------------------------------        
       01  PARSER-CONTROL-AREA.                                                 
           COPY MLX2PRSI.                                                       
                                                                                
      *-----------------------------------------------------------------        
      *    PARSER RETURN AREA                                                   
      *-----------------------------------------------------------------        
       01  PARSER-RETURN-AREA.                                                  
           COPY MLX2PRSO.                                                       
                                                                                
      *-----------------------------------------------------------------        
      *    PARSER TARGET AREA                                                   
      *-----------------------------------------------------------------        
       01  TARGET-AREA                    PIC X(16000).                         
                                                                                
      *----------------------------------------------------------------*        
      *    DYNAMIC FILE ALLOCATION PARAMETERS                                   
      *----------------------------------------------------------------*        
           COPY GDYNALLO.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *    ACTION VERBS USED TO CALL GAEDATSR                                   
      *----------------------------------------------------------------*        
       01  DATA-SERVER-VERBS.                                                   
           COPY GARDSVRB.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *    LOGICAL RECORD NAMES                                                 
      *----------------------------------------------------------------*        
       01  LEADCARD-LR                   PIC X(16)                              
                               VALUE 'CARD-DATA-010   '.                        
       01  OUTPUT-LR                      PIC X(16)                             
                               VALUE 'PRINT-DATA-071  '.                        
                                                                                
       01  ICBM-AREA.                                                           
           COPY ICBM.                                                           
                                                                                
      *----------------------------------------------------------------*        
      *   MESSAGING VARIABLES (FOR MQSERIES)                                    
      *----------------------------------------------------------------*        
       01  MQ-CONSTANTS.                                                        
           COPY CMQV.                                                           
                                                                                
       01  MQ-OBJECT-DESCRIPTOR.                                                
           COPY CMQODV.                                                         
                                                                                
       01  MQ-MESSAGE-DESCRIPTOR.                                               
           COPY CMQMDV.                                                         
                                                                                
       01  MQ-GET-MESSAGE-OPTIONS.                                              
           COPY CMQGMOV.                                                        
                                                                                
       01  MQ-PUT-MESSAGE-OPTIONS.                                              
           COPY CMQPMOV.                                                        
                                                                                
       01  MQ-VARIABLES.                                                        
           05  QMGR-NAME             PIC X(48)         VALUE SPACES.            
           05  HCONN                 PIC S9(09) BINARY VALUE ZERO.              
           05  COMPLETION-CODE       PIC S9(09) BINARY VALUE ZERO.              
           05  REASON                PIC S9(09) BINARY VALUE ZERO.              
           05  CON-REASON            PIC S9(09) BINARY VALUE ZERO.              
           05  OPTIONS               PIC S9(09) BINARY VALUE ZERO.              
           05  GQ-HANDLE             PIC S9(09) BINARY VALUE ZERO.              
           05  PQ-HANDLE             PIC S9(09) BINARY VALUE ZERO.              
           05  GQ-HANDLE-2           PIC S9(09) BINARY VALUE ZERO.              
           05  DATA-LENGTH           PIC S9(09) BINARY VALUE ZERO.              
           05  BUFFER-LENGTH         PIC S9(09) BINARY VALUE ZERO.              
                                                                                
      *-----------------------------------------------------------------        
      *    MESSAGE BUFFER AREA                                                  
      *-----------------------------------------------------------------        
       01  BUFFER-AREA                    PIC X(16000).                         
       01  MLI-MSG-HEADER-V2              REDEFINES BUFFER-AREA.                
           COPY MLMV2HDR.                                                       
                                                                                
       01  FILLER                    PIC X(40) VALUE                            
               '***  GACQTOF WORKING STORAGE ENDS   ***'.                       
                                                                                
       LINKAGE SECTION.                                                         
       01  PARM.                                                                
          05  PARM-LENGTH              PIC S9(04) COMP.                         
          05  PARM-DATA                PIC X(100).                              
                                                                                
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING PARM.                                           
      *----------------------------------------------------------------*        
                                                                                
      ******************************************************************        
      *  1. READ TAGGED DATA STREAM, GET NAME & PARAMETERS FOR FILE.            
      *     - PARSE THE TAG DATASTREAM FOR EXPECTED RECORD COUNT.               
      *     - ALLOCATE MF OUTPUT FILE                                           
      *                                                                         
      *  2. OPEN QUEUES.                                                        
      *     - OPEN REPLY QUEUE SPECIFIED IN THE REQUEST MESSAGE.                
      *     - READ FIRST REPLY FROM QUEUE WITH SYNCPOINT OPTION.                
      *                                                                         
      *  3. TRANSFER MSGS FROM DYNAMIC REPLY QUEUE TO MF FILE.                  
      *     - PUT REPLY TO ALLOCATED FILE. COMMIT MQ TO LAST SYNCPOINT.         
      *     - READ NEXT REPLY WITH SYNCPOINT OPTION.                            
      *     - REPEAT ... UNTIL EOQ OR FATAL ERROR.                              
      *                                                                         
      *  4. CLOSE DYNAMIC REPLY QUEUES.                                         
      *     - IF EXPECTED MSG. COUNT = OR > MSGS. DOWNLOADED =>                 
      *          DELETE DYNAMIC REPLY QUEUE... ELSE JUST CLOSE IT.              
      *                                                                         
      *  5. WRITE ONE REPLY MSG.                                                
      *     - THERE'S ONE REQUEST THAT LEADS TO OFFLOADING THE QUEUE            
      *     - TO A FILE, THEREFORE THERE'S ONE REPLY EXPECTED.                  
      *     - USE MQPUT1...OPENS/PUTS/CLOSES ALL IN ONE STEP.                   
      *                                                                         
      *  6. READ REQUEST QUEUE (UNLESS FATAL ERROR ENCOUNTERED)                 
      *     - SEE IF THERE'S A NEW MSG. TO BE PROCESSED. IF YES, REPEAT         
      *     - PROCESS, ELSE CLOSE DOWN, SAY BYE-BYE, EXIT, STOP, QUIT...        
      ******************************************************************        
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INITIALIZATION THRU                                     
                   1000-INITIALIZATION-EXIT.                                    
                                                                                
           PERFORM UNTIL REQUEST-Q-EMPTY                                        
                      OR FATAL-ERROR                                            
                                                                                
              PERFORM 2000-MAIN-PROCESS THRU                                    
                      2000-MAIN-PROCESS-EXIT                                    
                                                                                
              PERFORM 3000-CLOSE-MF-FILE THRU                                   
                      3000-CLOSE-MF-FILE-EXIT                                   
                                                                                
              PERFORM 4000-UNALLOCATE THRU                                      
                      4000-UNALLOCATE-EXIT                                      
                                                                                
              PERFORM 5700-CLOSE-INPUT-QUEUE THRU                               
                      5700-CLOSE-INPUT-QUEUE-EXIT                               
                                                                                
              PERFORM 8000-END-PROCESS THRU                                     
                      8000-END-PROCESS-EXIT                                     
                                                                                
              IF NO-ERRORS AND QUEUED-MSG-REQUEST                               
                 PERFORM 5100-READ-REQUEST-QUEUE THRU                           
                         5100-READ-REQUEST-QUEUE-EXIT                           
              END-IF                                                            
                                                                                
           END-PERFORM.                                                         
                                                                                
           PERFORM 9000-FINAL THRU                                              
                   9000-FINAL-EXIT.                                             
                                                                                
       0000-MAINLINE-EXIT.                                                      
           EXIT.                                                                
           STOP RUN.                                                            
                                                                                
      ******************************************************************        
      * INITIALIZATION                                                          
      ******************************************************************        
       1000-INITIALIZATION.                                                     
                                                                                
           MOVE 0                         TO WS-FINAL-RETURN-CODE.              
           SET NO-ERRORS                  TO TRUE.                              
           SET SUCCESSFUL-COMPLETION      TO TRUE.                              
           SET MF-FILE-UNALLOC            TO TRUE.                              
           SET MF-FILE-CLOSED             TO TRUE.                              
           MOVE 'GACQTOF'                 TO ICBM-PROGRAM-NAME.                 
           MOVE LOW-VALUES                TO LINKAGE-CONTROL.                   
           MOVE ZERO                      TO WS-RECNO.                          
           SET REQUEST-Q-NOT-EMPTY        TO TRUE.                              
           SET UPDATE-ORIG-TS             TO TRUE.                              
           MOVE SPACES                    TO GDYNALLO-PARM1.                    
      *                                                                         
      *    IF NO PARAMETERS PASSED TO PROGRAM THEN READ                         
      *    FOR LEADCARD INFORMATION.                                            
      *                                                                         
           IF PARM-LENGTH = 0 THEN                                              
              SET LEADCARD-REQUEST        TO TRUE                               
              SET REPLY-MSG-NOT-EXPECTED  TO TRUE                               
              PERFORM 1500-PROCESS-LEADCARD THRU                                
                      1500-PROCESS-LEADCARD-EXIT                                
              IF FATAL-ERROR                                                    
                 GO TO 1000-INITIALIZATION-EXIT                                 
              END-IF                                                            
           ELSE                                                                 
              SET QUEUED-MSG-REQUEST      TO TRUE                               
              SET REPLY-MSG-EXPECTED      TO TRUE                               
              UNSTRING PARM-DATA DELIMITED BY ','                               
                  INTO WS-REQUEST-QMGRNAME                                      
                       WS-REQUEST-QNAME                                         
              MOVE WS-REQUEST-QMGRNAME    TO WS-INPUT-QMGRNAME                  
           END-IF.                                                              
                                                                                
      *                                                                         
      *    DISPLAY PARAMETERS TO BE USED IN THE PROGRAM                         
      *                                                                         
           DISPLAY '=================================================='.        
           DISPLAY '=                  INPUT PARMS                   ='.        
           DISPLAY '=================================================='.        
           DISPLAY '= QTOF TRIGGERED?   - ', PROCESS-IND.                       
           IF QUEUED-MSG-REQUEST                                                
              DISPLAY '= REQUEST QUEUE (TO BE PROCESSED): '                     
              DISPLAY '=  QUEUE MANAGER   - ', WS-REQUEST-QMGRNAME              
              DISPLAY '=  QUEUE NAME      - ', WS-REQUEST-QNAME                 
           ELSE                                                                 
              PERFORM 2110-DISPLAY-PARMS THRU                                   
                      2110-DISPLAY-PARMS-EXIT                                   
           END-IF.                                                              
                                                                                
      *                                                                         
      * CONNECT TO QUEUE MANAGER                                                
      *                                                                         
           MOVE WS-INPUT-QMGRNAME         TO QMGR-NAME.                         
           MOVE ZERO                      TO HCONN                              
                                             COMPLETION-CODE                    
                                             REASON.                            
                                                                                
           CALL MQCONN-BATCH           USING QMGR-NAME                          
                                             HCONN                              
                                             COMPLETION-CODE                    
                                             REASON.                            
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              DISPLAY 'Q MANAGER CONNECTION FAILED, REASON ', REASON            
              SET QUEUE-MGR-CONNECT-FAILED TO TRUE                              
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR              TO TRUE                              
              GO TO 1000-INITIALIZATION-EXIT                                    
           END-IF.                                                              
                                                                                
           IF LEADCARD-REQUEST                                                  
              GO TO 1000-INITIALIZATION-EXIT.                                   
                                                                                
      *                                                                         
      * OPEN REQUEST QUEUE                                                      
      *                                                                         
           PERFORM 5000-OPEN-REQUEST-QUEUE THRU                                 
                   5000-OPEN-REQUEST-QUEUE-EXIT.                                
           IF FATAL-ERROR                                                       
              GO TO 1000-INITIALIZATION-EXIT.                                   
                                                                                
      *                                                                         
      * READ REQUEST QUEUE                                                      
      *                                                                         
           PERFORM 5100-READ-REQUEST-QUEUE THRU                                 
                   5100-READ-REQUEST-QUEUE-EXIT                                 
           IF FATAL-ERROR                                                       
              GO TO 1000-INITIALIZATION-EXIT.                                   
                                                                                
           IF REQUEST-Q-EMPTY                                                   
              DISPLAY 'NO MESSAGES IN INPUT QUEUE'                              
              SET REQUEST-QUEUE-READ-ERROR  TO TRUE                             
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR               TO TRUE                             
              GO TO 1000-INITIALIZATION-EXIT                                    
           END-IF.                                                              
                                                                                
       1000-INITIALIZATION-EXIT.                                                
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * OBTAIN LEADCARD REQUEST INFO FROM DLSI10 AND STORE THEM.                
      ******************************************************************        
       1500-PROCESS-LEADCARD.                                                   
                                                                                
      *                                                                         
      * OPEN LEADCARD FILE, READ FIRST RECORD                                   
      *                                                                         
           MOVE LEADCARD-LR       TO LOGICAL-RECORD-NAME.                       
           MOVE OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                          
           PERFORM 1510-LEADCARD-DATASRVR THRU                                  
                   1510-LEADCARD-DATASRVR-EXIT.                                 
                                                                                
      *                                                                         
      * WHILE NOT EOF                                                           
      *   PARSE EACH RECORD INTO WORKING STORAGE                                
      *   READ NEXT RECORD                                                      
      *                                                                         
           PERFORM UNTIL LR-NOT-FOUND OR FATAL-ERROR                            
                                                                                
              INITIALIZE WS-LEADCARD-TABLE                                      
              UNSTRING LEADCARD-RECORD                                          
              DELIMITED '=(' OR '=' OR '(' OR ',' OR ')' OR ALL SPACES          
                   INTO WS-LEADCARD-TOKEN                                       
                        WS-LEADCARD-DATA(1) COUNT IN WS-LEADCARD-SIZE(1)        
                        WS-LEADCARD-DATA(2) COUNT IN WS-LEADCARD-SIZE(2)        
                        WS-LEADCARD-DATA(3) COUNT IN WS-LEADCARD-SIZE(3)        
                                                                                
              EVALUATE WS-LEADCARD-TOKEN                                        
                                                                                
               WHEN 'REPLYQ'                                                    
                  MOVE WS-LEADCARD-DATA(1)     TO WS-REPLY-QMGRNAME             
                  MOVE WS-LEADCARD-DATA(2)     TO WS-REPLY-QNAME                
                  SET REPLY-MSG-EXPECTED       TO TRUE                          
               WHEN 'INPUTQ'                                                    
                  MOVE WS-LEADCARD-DATA(1)     TO WS-INPUT-QACTION              
                  MOVE WS-LEADCARD-DATA(2)     TO WS-INPUT-QMGRNAME             
                  MOVE WS-LEADCARD-DATA(3)     TO WS-INPUT-QNAME                
               WHEN 'MESSAGECOUNT'                                              
                  MOVE WS-LEADCARD-DATA(1) (1:WS-LEADCARD-SIZE(1))              
                                               TO WS-IN-RECORD-COUNT            
               WHEN 'DSN'                                                       
                  MOVE WS-LEADCARD-DATA(1)     TO GDYN-DSN                      
                  MOVE WS-LEADCARD-DATA(2)     TO GDYN-MEMBER                   
                  UNSTRING LEADCARD-RECORD DELIMITED '='                        
                      INTO WS-LEADCARD-TOKEN                                    
                           WS-MF-FILE-NAME                                      
               WHEN 'DSORG'                                                     
                  MOVE WS-LEADCARD-DATA(1)     TO GDYN-DATA-SET-ORG             
               WHEN 'DISP'                                                      
                  MOVE WS-LEADCARD-DATA(1)     TO GDYN-STATUS                   
                  MOVE WS-LEADCARD-DATA(2)     TO GDYN-NORMAL-DISP              
                  MOVE WS-LEADCARD-DATA(3)     TO GDYN-COND-DISP                
               WHEN 'UNIT'                                                      
                  MOVE WS-LEADCARD-DATA(1)     TO GDYN-UNIT-NAME                
               WHEN 'SPACE'                                                     
                  MOVE WS-LEADCARD-DATA(1)     TO GDYN-SPACE-TYPE               
                  MOVE WS-LEADCARD-DATA(2) (1:WS-LEADCARD-SIZE(2))              
                                               TO GDYN-PRIMARY-SPACE            
                  MOVE WS-LEADCARD-DATA(3) (1:WS-LEADCARD-SIZE(3))              
                                               TO GDYN-SECONDARY-SPACE          
               WHEN 'TSOPT'                                                     
                  MOVE WS-LEADCARD-DATA(1)     TO WS-TS-OPTION                  
               WHEN OTHER                                                       
                  DISPLAY '** CARD DATA IGNORED --> ' LEADCARD-RECORD           
                                                                                
              END-EVALUATE                                                      
                                                                                
              MOVE OBTAIN-NEXT    TO WS-GAEDATSR-VERB                           
              PERFORM 1510-LEADCARD-DATASRVR THRU                               
                      1510-LEADCARD-DATASRVR-EXIT                               
                                                                                
           END-PERFORM.                                                         
                                                                                
      *                                                                         
      * CLOSE LEADCARD FILE                                                     
      *                                                                         
           MOVE FINISH-LR      TO WS-GAEDATSR-VERB                              
           PERFORM 1510-LEADCARD-DATASRVR THRU                                  
                   1510-LEADCARD-DATASRVR-EXIT.                                 
                                                                                
       1500-PROCESS-LEADCARD-EXIT.                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * HANDLE LEADCARD DATA SERVER CALLS                                       
      ******************************************************************        
       1510-LEADCARD-DATASRVR.                                                  
                                                                                
           CALL GAEDATSR          USING WS-GAEDATSR-VERB                        
                                        LEADCARD-RECORD                         
                                        ICBM-AREA.                              
           IF LR-NOT-FOUND                                                      
           OR KEYED-LR-NOT-FOUND                                                
              IF WS-GAEDATSR-VERB = OBTAIN-FIRST                                
                 DISPLAY '**LEADCARD FILE EMPTY: '                              
                            PROGRAM-LINKAGE-STATUS                              
                 SET EXPECTED-LEADCARD-MISSING TO TRUE                          
                 PERFORM 9100-SET-RETURN-CODE THRU                              
                         9100-SET-RETURN-CODE-EXIT                              
                 SET FATAL-ERROR          TO TRUE                               
              END-IF                                                            
           ELSE                                                                 
              IF NOT LR-STATUS-OK                                               
                 IF WS-GAEDATSR-VERB = FINISH-LR                                
                    DISPLAY '**LEADCARD FILE CLOSE ERROR:'                      
                            PROGRAM-LINKAGE-STATUS                              
                    SET LEADCARD-FILE-CLOSE-ERROR TO TRUE                       
                 ELSE                                                           
                    DISPLAY '**LEADCARD FILE READ ERROR: '                      
                    SET LEADCARD-FILE-READ-ERROR   TO TRUE                      
                 END-IF                                                         
                 PERFORM 9100-SET-RETURN-CODE THRU                              
                         9100-SET-RETURN-CODE-EXIT                              
                 SET FATAL-ERROR          TO TRUE                               
              END-IF                                                            
           END-IF.                                                              
                                                                                
       1510-LEADCARD-DATASRVR-EXIT.                                             
           EXIT.                                                                
                                                                                
       2000-MAIN-PROCESS.                                                       
                                                                                
           IF QUEUED-MSG-REQUEST                                                
              PERFORM 2100-RETRIEVE-TAG-INFO THRU                               
                      2100-RETRIEVE-TAG-INFO-EXIT                               
              PERFORM 2110-DISPLAY-PARMS THRU                                   
                      2110-DISPLAY-PARMS-EXIT                                   
           END-IF.                                                              
                                                                                
           IF WS-RETURN-CODE > ZERO OR FATAL-ERROR                              
              GO TO 2000-MAIN-PROCESS-EXIT.                                     
                                                                                
           PERFORM 2200-DYNAMIC-FILE THRU                                       
                   2200-DYNAMIC-FILE-EXIT.                                      
                                                                                
           IF WS-RETURN-CODE > ZERO OR FATAL-ERROR                              
              GO TO 2000-MAIN-PROCESS-EXIT.                                     
                                                                                
           PERFORM 5500-OPEN-INPUT-QUEUE THRU                                   
                   5500-OPEN-INPUT-QUEUE-EXIT.                                  
                                                                                
           IF WS-RETURN-CODE > ZERO OR FATAL-ERROR                              
              GO TO 2000-MAIN-PROCESS-EXIT.                                     
                                                                                
           PERFORM 5600-READ-INPUT-QUEUE THRU                                   
                   5600-READ-INPUT-QUEUE-EXIT.                                  
                                                                                
           IF WS-RETURN-CODE > ZERO OR FATAL-ERROR                              
              GO TO 2000-MAIN-PROCESS-EXIT.                                     
                                                                                
           PERFORM 2300-TRANSFER-MESSAGES THRU                                  
                   2300-TRANSFER-MESSAGES-EXIT                                  
             UNTIL INPUT-Q-EMPTY                                                
                OR FATAL-ERROR.                                                 
                                                                                
       2000-MAIN-PROCESS-EXIT.                                                  
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * RETRIEVE TAGS FROM THE REQUEST MESSAGE AND STORE THEM.                  
      ******************************************************************        
       2100-RETRIEVE-TAG-INFO.                                                  
                                                                                
           INITIALIZE PARSER-CONTROL-AREA.                                      
           INITIALIZE PARSER-RETURN-AREA.                                       
           SET  PRSI-SECTION-BODY             TO TRUE.                          
           SET  PRSI-NORMAL                   TO TRUE.                          
           SET  PRSI-NO-OPTIONS               TO TRUE.                          
           MOVE WS-REQUEST-MESSAGE            TO BUFFER-AREA.                   
                                                                                
      *                                                                         
      * GET OUTPUT DATASET NAME TAG                                             
      *                                                                         
           INITIALIZE WS-MF-FILE-NAME.                                          
           MOVE WS-FLD-OUTPUT-DSN             TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF WS-MF-FILE-NAME     TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              WS-MF-FILE-NAME                   
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
           UNSTRING WS-MF-FILE-NAME DELIMITED '(' OR ')' OR ALL SPACES          
               INTO GDYN-DSN                                                    
                    GDYN-MEMBER.                                                
      *                                                                         
      * GET DATASET ALLOC SPACE TYPE                                            
      *                                                                         
           INITIALIZE GDYN-SPACE-TYPE.                                          
           MOVE WS-FLD-SPACE-TYPE             TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF GDYN-SPACE-TYPE     TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              GDYN-SPACE-TYPE                   
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET DATASET PRIMARY SPACE AMOUNT                                        
      *                                                                         
           INITIALIZE GDYN-PRIMARY-SPACE.                                       
           MOVE WS-FLD-PRIMARY-SPACE          TO PRSI-PACKET-ID.                
           SET  TYPE-LONG-INT-PACKED          TO TRUE.                          
           MOVE LENGTH OF GDYN-PRIMARY-SPACE  TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              GDYN-PRIMARY-SPACE                
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET DATASET SECONDARY SPACE AMOUNT                                      
      *                                                                         
           INITIALIZE GDYN-SECONDARY-SPACE.                                     
           MOVE WS-FLD-SECONDARY-SPACE        TO PRSI-PACKET-ID.                
           SET  TYPE-LONG-INT-PACKED          TO TRUE.                          
           MOVE LENGTH OF GDYN-SECONDARY-SPACE TO PRSI-PACKET-LENGTH.           
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              GDYN-SECONDARY-SPACE              
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET ALLOC UNIT                                                          
      *                                                                         
           INITIALIZE GDYN-UNIT-NAME.                                           
           MOVE WS-FLD-UNIT-NAME              TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF GDYN-UNIT-NAME      TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              GDYN-UNIT-NAME                    
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET DATASET ORGANIZATION                                                
      *                                                                         
           INITIALIZE GDYN-DATA-SET-ORG.                                        
           MOVE WS-FLD-DSORG                  TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF GDYN-DATA-SET-ORG   TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              GDYN-DATA-SET-ORG                 
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET DATASET ALLOC STATUS                                                
      *                                                                         
           INITIALIZE GDYN-STATUS.                                              
           MOVE WS-FLD-DISP-STATUS            TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF GDYN-STATUS         TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              GDYN-STATUS                       
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET DATASET NORMAL DISPOSITION                                          
      *                                                                         
           INITIALIZE GDYN-NORMAL-DISP.                                         
           MOVE WS-FLD-DISP-NORMAL            TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF GDYN-NORMAL-DISP    TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              GDYN-NORMAL-DISP                  
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET DATASET CONDITIONAL DISPOSITION                                     
      *                                                                         
           INITIALIZE GDYN-COND-DISP.                                           
           MOVE WS-FLD-DISP-ABEND             TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF GDYN-COND-DISP      TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              GDYN-COND-DISP                    
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET QUEUE NAME FROM WHICH MESSAGES ARE TO BE UNLOADED                   
      *                                                                         
           INITIALIZE WS-INPUT-QNAME.                                           
           MOVE WS-FLD-REPLY-QUEUE            TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF WS-INPUT-QNAME      TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              WS-INPUT-QNAME                    
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *****************************************************************         
      * FOR THE 'QUEUE TO FILE' PROCESS THIS TAG TELLS YOU WHAT ACTION          
      * SHOULD BE APPLIED TO THE INPUT QUEUE:                                   
      *  IF ACTION = 'PU ' THEN DELETE/PURGE AFTER PROCESSING TO EOQ.           
      *  IF ACTION = 'PE ' THEN DELETE/PURGE AFTER PROCESSING IFF               
      *                   EXPECTED RECORD COUNT 'EQ' UNLOADED RECORD CNT        
      *  IF ACTION = 'PGE' THEN DELETE/PURGE AFTER PROCESSING IFF               
      *                   EXPECTED RECORD COUNT 'GE' UNLOADED RECORD CNT        
      *****************************************************************         
           INITIALIZE WS-INPUT-QACTION.                                         
           MOVE WS-FLD-QUEUE-ACTION           TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF WS-INPUT-QACTION    TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              WS-INPUT-QACTION                  
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
      *                                                                         
      * GET EXPECTED RECORD COUNT                                               
      *                                                                         
           INITIALIZE WS-IN-RECORD-COUNT.                                       
           MOVE WS-FLD-RECORD-COUNT           TO PRSI-PACKET-ID.                
           SET  TYPE-LONG-INT                 TO TRUE.                          
           MOVE LENGTH OF WS-IN-RECORD-COUNT  TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7100-SETUP-PARS-GET-CALL THRU                                
                   7100-SETUP-PARS-GET-CALL-EXIT.                               
                                                                                
           CALL PARSER-GET              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              WS-IN-RECORD-COUNT                
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7110-CHECK-PARS-GET-CALL THRU                                
                   7110-CHECK-PARS-GET-CALL-EXIT.                               
                                                                                
           IF WS-RETURN-CODE NOT = ZERO                                         
              GO TO 2100-RETRIEVE-TAG-INFO-EXIT                                 
           END-IF.                                                              
                                                                                
       2100-RETRIEVE-TAG-INFO-EXIT.                                             
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * DISPLAY THE PARAMETERIZED INPUT FOR EACH QUEUE TO FILE PROCESS          
      ******************************************************************        
       2110-DISPLAY-PARMS.                                                      
                                                                                
           DISPLAY '=------------------------------------------------='.        
           DISPLAY '= INPUT QUEUE (TO BE UNLOADED): '.                          
           DISPLAY '=  QUEUE MANAGER   - ', WS-INPUT-QMGRNAME.                  
           DISPLAY '=  QUEUE NAME      - ', WS-INPUT-QNAME.                     
           DISPLAY '=  QUEUE ACTION    - ', WS-INPUT-QACTION.                   
           DISPLAY '=  MESSAGE COUNT   - ', WS-IN-RECORD-COUNT.                 
           DISPLAY '=  TIMESTAMP OPT   - ', WS-TS-OPTION.                       
           DISPLAY '= OUTPUT DATASET ALLOCATION: '.                             
           DISPLAY '=  DSN             - ', WS-MF-FILE-NAME.                    
           DISPLAY '=  DSORG           - ', GDYN-DATA-SET-ORG.                  
           DISPLAY '=  UNIT            - ', GDYN-UNIT-NAME.                     
           DISPLAY '=  SPACE TYPE      - ', GDYN-SPACE-TYPE.                    
           DISPLAY '=  PRIMARY SPACE   - ', GDYN-PRIMARY-SPACE.                 
           DISPLAY '=  SECONDARY SPACE - ', GDYN-SECONDARY-SPACE.               
           DISPLAY '=  DATASET DISP    - ', GDYN-STATUS.                        
           DISPLAY '=                    ', GDYN-NORMAL-DISP.                   
           DISPLAY '=                    ', GDYN-COND-DISP.                     
           DISPLAY '= REPLY QUEUE (FOR RESPONSE): '.                            
           DISPLAY '=  QUEUE MANAGER   - ', WS-REPLY-QMGRNAME.                  
           DISPLAY '=  QUEUE NAME      - ', WS-REPLY-QNAME.                     
           DISPLAY '=------------------------------------------------='.        
                                                                                
       2110-DISPLAY-PARMS-EXIT.                                                 
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * DYNAMIC FILE ALLOCATION - (OUTPUT)                                      
      ******************************************************************        
       2200-DYNAMIC-FILE.                                                       
                                                                                
           IF WS-MF-FILE-NAME = LOW-VALUES                                      
              DISPLAY '**MF FILE ALLOCATION ERROR'                              
              SET DYNAMIC-ALLOCATION-ERROR TO TRUE                              
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR              TO TRUE                              
              GO TO 2200-DYNAMIC-FILE-EXIT                                      
           END-IF.                                                              
                                                                                
           MOVE 'A'                       TO GDYN-ACTION.                       
           MOVE 'DLSO71'                  TO GDYN-DDNAME.                       
      *                                                                         
      *    ASSUME THAT THE FILE ALLOCATED MUST BE VARIABLE LENGTH               
      *    WITH MAXIMUM RECORD LENGTH OF 16000 BYTES.                           
      *                                                                         
           MOVE 'VB'                      TO GDYN-RECORD-FORMAT.                
           MOVE +16000                    TO GDYN-RECORD-LENGTH.                
           MOVE +16004                    TO GDYN-BLOCK-SIZE.                   
           MOVE 'Y'                       TO GDYN-RLSE.                         
           MOVE SPACE                     TO GDYN-UNCLOSE.                      
                                                                                
           CALL 'GDYNALLO' USING GDYNALLO-PARM1                                 
                                 GDYNALLO-PARM2.                                
                                                                                
           IF GDYN-RETURN-CODE NOT = ZERO                                       
              DISPLAY '**MF FILE ALLOC ERROR: ' GDYN-RETURN-CODE                
              DISPLAY '** FILE NAME ' WS-MF-FILE-NAME                           
              DISPLAY '** SPACE     ' GDYN-SPACE-TYPE                           
              DISPLAY '** PRIMARY   ' GDYN-PRIMARY-SPACE                        
              DISPLAY '** SECONDARY ' GDYN-SECONDARY-SPACE                      
              DISPLAY '** RECFM     ' GDYN-RECORD-FORMAT                        
              DISPLAY '** LRECL     ' GDYN-RECORD-LENGTH                        
              DISPLAY '** BLKSIZE   ' GDYN-BLOCK-SIZE                           
              DISPLAY '** UNIT      ' GDYN-UNIT-NAME                            
              DISPLAY '** STATUS    ' GDYN-STATUS                               
              DISPLAY '** NORMAL    ' GDYN-NORMAL-DISP                          
              DISPLAY '** ABEND     ' GDYN-COND-DISP                            
              DISPLAY '** DSORG     ' GDYN-DATA-SET-ORG                         
              SET DYNAMIC-ALLOCATION-ERROR TO TRUE                              
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR              TO TRUE                              
              GO TO 2200-DYNAMIC-FILE-EXIT                                      
           END-IF.                                                              
                                                                                
           SET MF-FILE-ALLOC TO TRUE.                                           
                                                                                
       2200-DYNAMIC-FILE-EXIT.                                                  
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * TRANSFER RECORDS FROM QUEUE TO ALLOCATED FILE...                        
      ******************************************************************        
       2300-TRANSFER-MESSAGES.                                                  
                                                                                
           SET MF-FILE-OPEN       TO TRUE.                                      
      *                                                                         
      * UPDATE THE MESSAGE HEADER FOR VERSION 2 MESSAGES AS FOLLOWS:            
      * THE ORIGINAL RECEIVED DATE/TIME WILL BE CHANGED TO THE                  
      * PUT DATE/TIME ON THE MQ MESSAGE DESCRIPTOR UNLESS THIS ACTION IS        
      * SUPPRESSED BY USING TSOPT=RETAIN IN THE LEADCARD.                       
      *                                                                         
           IF HEADER-VERSION-NUMBER < 2.00                                      
              CONTINUE                                                          
           ELSE                                                                 
              IF UPDATE-ORIG-TS                                                 
                 MOVE MQMD-PUTDATE TO ORIGINAL-RECEIVED-DATE                    
                 MOVE MQMD-PUTTIME TO ORIGINAL-RECEIVED-TIME                    
              END-IF                                                            
           END-IF.                                                              
                                                                                
           MOVE BUFFER-AREA       TO OUTPUT-RECORD.                             
           MOVE DATA-LENGTH       TO OUTPUT-LENGTH.                             
           ADD 4                  TO OUTPUT-LENGTH.                             
                                                                                
           MOVE STORE-LR          TO  WS-GAEDATSR-VERB.                         
           MOVE OUTPUT-LR         TO LOGICAL-RECORD-NAME.                       
           CALL GAEDATSR       USING WS-GAEDATSR-VERB                           
                                     MF-RECORD                                  
                                     ICBM-AREA.                                 
                                                                                
           IF NOT LR-STATUS-OK                                                  
              DISPLAY '**MF FILE WRITE ERROR: ' PROGRAM-LINKAGE-STATUS          
              DISPLAY '** ' WS-MF-FILE-NAME                                     
              SET MF-FILE-WRITE-ERROR  TO TRUE                                  
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR          TO TRUE                                  
              GO TO 2300-TRANSFER-MESSAGES-EXIT                                 
           END-IF.                                                              
                                                                                
           ADD  1  TO WS-RECNO.                                                 
      *                                                                         
      * BEFORE READING THE NEXT REPLY MESSAGE, COMMIT THE LAST READ             
      * AS THE LAST REPLY MESSAGE WAS SUCCESSFULLY WRITTEN TO FILE.             
      *                                                                         
           CALL MQCMIT-BATCH   USING HCONN                                      
                                     COMPLETION-CODE                            
                                     REASON.                                    
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              DISPLAY 'Q MANAGER COMMIT FAILED, REASON ', REASON                
              SET QUEUE-MGR-COMMIT-FAILED TO TRUE                               
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR             TO TRUE                               
              GO TO 2300-TRANSFER-MESSAGES-EXIT                                 
           END-IF.                                                              
                                                                                
           PERFORM 5600-READ-INPUT-QUEUE THRU                                   
                   5600-READ-INPUT-QUEUE-EXIT.                                  
                                                                                
       2300-TRANSFER-MESSAGES-EXIT.                                             
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * CLOSE MAINFRAME FILE                                                    
      ******************************************************************        
       3000-CLOSE-MF-FILE.                                                      
                                                                                
           IF MF-FILE-OPEN                                                      
              MOVE SPACES       TO LOGICAL-RECORD-NAME                          
              MOVE FINISH-LR    TO WS-GAEDATSR-VERB                             
              CALL GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           MF-RECORD                            
                                           ICBM-AREA                            
              IF NOT LR-STATUS-OK                                               
                 DISPLAY '**MF FILE CLOSE ERROR'                                
                 SET MF-FILE-CLOSE-ERROR TO TRUE                                
                 PERFORM 9100-SET-RETURN-CODE THRU                              
                         9100-SET-RETURN-CODE-EXIT                              
                 SET FATAL-ERROR         TO TRUE                                
                 GO TO 3000-CLOSE-MF-FILE-EXIT                                  
              END-IF                                                            
           END-IF.                                                              
                                                                                
           SET MF-FILE-CLOSED  TO TRUE.                                         
                                                                                
       3000-CLOSE-MF-FILE-EXIT.                                                 
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * UNALLOCATE DYNAMIC DD                                                   
      ******************************************************************        
       4000-UNALLOCATE.                                                         
                                                                                
           IF MF-FILE-UNALLOC                                                   
              GO TO 4000-UNALLOCATE-EXIT.                                       
                                                                                
           MOVE 'U'                       TO GDYN-ACTION.                       
           MOVE 'DLSO71'                  TO GDYN-DDNAME.                       
                                                                                
           CALL 'GDYNALLO' USING GDYNALLO-PARM1                                 
                                 GDYNALLO-PARM2.                                
           IF GDYN-RETURN-CODE NOT = ZERO                                       
              DISPLAY '**MF FILE UNALLOCATE ERROR'                              
              SET DYNAMIC-UNALLOCATION-ERROR TO TRUE                            
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR                TO TRUE                            
              GO TO 4000-UNALLOCATE-EXIT                                        
           END-IF.                                                              
                                                                                
           SET MF-FILE-UNALLOC TO TRUE.                                         
                                                                                
       4000-UNALLOCATE-EXIT.                                                    
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * OPEN REQUEST QUEUE                                                      
      ******************************************************************        
       5000-OPEN-REQUEST-QUEUE.                                                 
                                                                                
           MOVE MQOT-Q                   TO MQOD-OBJECTTYPE.                    
           MOVE WS-REQUEST-QNAME         TO MQOD-OBJECTNAME.                    
           MOVE WS-REQUEST-QMGRNAME      TO MQOD-OBJECTQMGRNAME.                
           COMPUTE OPTIONS = MQOO-INPUT-SHARED +                                
                             MQOO-FAIL-IF-QUIESCING.                            
                                                                                
           CALL MQOPEN-BATCH          USING HCONN                               
                                            MQ-OBJECT-DESCRIPTOR                
                                            OPTIONS                             
                                            GQ-HANDLE                           
                                            COMPLETION-CODE                     
                                            REASON.                             
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              DISPLAY 'OPEN REQUEST QUEUE FAILED, REASON ', REASON              
              SET REQUEST-QUEUE-OPEN-FAILED TO TRUE                             
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR               TO TRUE                             
           END-IF.                                                              
                                                                                
       5000-OPEN-REQUEST-QUEUE-EXIT.                                            
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * READ MESSAGE FROM REQUEST QUEUE                                         
      ******************************************************************        
       5100-READ-REQUEST-QUEUE.                                                 
                                                                                
           MOVE SPACES                TO BUFFER-AREA.                           
           MOVE LENGTH OF BUFFER-AREA TO BUFFER-LENGTH.                         
           MOVE MQMI-NONE             TO MQMD-MSGID.                            
           MOVE MQCI-NONE             TO MQMD-CORRELID.                         
           COMPUTE MQGMO-OPTIONS = MQGMO-NO-SYNCPOINT +                         
                                   MQGMO-FAIL-IF-QUIESCING.                     
                                                                                
           CALL MQGET-BATCH        USING HCONN                                  
                                         GQ-HANDLE                              
                                         MQ-MESSAGE-DESCRIPTOR                  
                                         MQ-GET-MESSAGE-OPTIONS                 
                                         BUFFER-LENGTH                          
                                         BUFFER-AREA                            
                                         DATA-LENGTH                            
                                         COMPLETION-CODE                        
                                         REASON.                                
                                                                                
           IF MQMD-BACKOUTCOUNT > ZERO                                          
              DISPLAY '** PROCESSING ROLLED BACK REQUEST MESSAGE'               
              DISPLAY '** BACKOUT COUNT: ' MQMD-BACKOUTCOUNT                    
           END-IF.                                                              
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              IF REASON = MQRC-NO-MSG-AVAILABLE                                 
                 SET REQUEST-Q-EMPTY TO TRUE                                    
              ELSE                                                              
                 DISPLAY 'READ REQUEST QUEUE FAILED, REASON ', REASON           
                 SET REQUEST-QUEUE-READ-ERROR TO TRUE                           
                 PERFORM 9100-SET-RETURN-CODE THRU                              
                         9100-SET-RETURN-CODE-EXIT                              
                 SET FATAL-ERROR              TO TRUE                           
                 GO TO 5100-READ-REQUEST-QUEUE-EXIT                             
              END-IF                                                            
           END-IF.                                                              
                                                                                
           MOVE MQMD-REPLYTOQ        TO WS-REPLY-QNAME.                         
           MOVE MQMD-REPLYTOQMGR     TO WS-REPLY-QMGRNAME.                      
           MOVE MQMD-MSGID           TO WS-REQUEST-MSGID.                       
           MOVE BUFFER-AREA          TO WS-REQUEST-MESSAGE.                     
           MOVE DATA-LENGTH          TO WS-REQUEST-MESSAGE-LENGTH.              
                                                                                
       5100-READ-REQUEST-QUEUE-EXIT.                                            
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * CLOSE REQUEST QUEUE                                                     
      ******************************************************************        
       5200-CLOSE-REQUEST-QUEUE.                                                
                                                                                
           IF GQ-HANDLE = ZERO                                                  
              GO TO 5200-CLOSE-REQUEST-QUEUE-EXIT.                              
                                                                                
           MOVE MQCO-NONE           TO OPTIONS.                                 
                                                                                
           CALL MQCLOSE-BATCH    USING HCONN                                    
                                       GQ-HANDLE                                
                                       OPTIONS                                  
                                       COMPLETION-CODE                          
                                       REASON.                                  
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              DISPLAY 'CLOSE REQUEST QUEUE FAILED, REASON ', REASON             
              SET REQUEST-QUEUE-CLOSE-FAILED TO TRUE                            
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR                TO TRUE                            
           END-IF.                                                              
                                                                                
       5200-CLOSE-REQUEST-QUEUE-EXIT.                                           
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * OPEN INPUT QUEUE SPECIFIED IN THE REQUEST MESSAGE.                      
      ******************************************************************        
       5500-OPEN-INPUT-QUEUE.                                                   
                                                                                
           SET INPUT-Q-NOT-EMPTY         TO TRUE.                               
                                                                                
           MOVE MQOT-Q                   TO MQOD-OBJECTTYPE.                    
           MOVE WS-INPUT-QNAME           TO MQOD-OBJECTNAME.                    
           MOVE WS-INPUT-QMGRNAME        TO MQOD-OBJECTQMGRNAME.                
           COMPUTE OPTIONS = MQOO-INPUT-SHARED +                                
                             MQOO-FAIL-IF-QUIESCING.                            
                                                                                
           CALL MQOPEN-BATCH          USING HCONN                               
                                            MQ-OBJECT-DESCRIPTOR                
                                            OPTIONS                             
                                            GQ-HANDLE-2                         
                                            COMPLETION-CODE                     
                                            REASON.                             
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              DISPLAY 'OPEN INPUT QUEUE FAILED, REASON ', REASON                
              SET INPUT-QUEUE-OPEN-FAILED TO TRUE                               
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR                TO TRUE                            
           END-IF.                                                              
                                                                                
       5500-OPEN-INPUT-QUEUE-EXIT.                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * READ MESSAGE FROM INPUT QUEUE                                           
      ******************************************************************        
       5600-READ-INPUT-QUEUE.                                                   
                                                                                
           MOVE SPACES                TO BUFFER-AREA.                           
           MOVE LENGTH OF BUFFER-AREA TO BUFFER-LENGTH.                         
           MOVE MQMI-NONE             TO MQMD-MSGID.                            
           MOVE MQCI-NONE             TO MQMD-CORRELID.                         
           COMPUTE MQGMO-OPTIONS = MQGMO-SYNCPOINT +                            
                                   MQGMO-FAIL-IF-QUIESCING.                     
                                                                                
           CALL MQGET-BATCH        USING HCONN                                  
                                         GQ-HANDLE-2                            
                                         MQ-MESSAGE-DESCRIPTOR                  
                                         MQ-GET-MESSAGE-OPTIONS                 
                                         BUFFER-LENGTH                          
                                         BUFFER-AREA                            
                                         DATA-LENGTH                            
                                         COMPLETION-CODE                        
                                         REASON.                                
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK  AND                                
              REASON NOT = MQRC-NO-MSG-AVAILABLE                                
              DISPLAY 'INPUT QUEUE READ FAILED, REASON ', REASON                
              SET INPUT-QUEUE-READ-ERROR TO TRUE                                
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR              TO TRUE                              
           ELSE                                                                 
              IF COMPLETION-CODE NOT = MQCC-OK  AND                             
                 REASON = MQRC-NO-MSG-AVAILABLE                                 
                 SET INPUT-Q-EMPTY TO TRUE                                      
              END-IF                                                            
           END-IF.                                                              
                                                                                
       5600-READ-INPUT-QUEUE-EXIT.                                              
           EXIT.                                                                
                                                                                
       5700-CLOSE-INPUT-QUEUE.                                                  
      *****************************************************************         
      * DETERMINE THE QUEUE CLOSE OPTIONS BASED ON ACTION PASSED:               
      * IF ACTION = 'PU ' THEN DELETE/PURGE UNCONDITIONALLY.                    
      * IF ACTION = 'PE ' THEN DELETE/PURGE IFF                                 
      *                   EXPECTED RECORD COUNT 'EQ' UNLOADED REC COUNT         
      * IF ACTION = 'PGE' THEN DELETE/PURGE IFF                                 
      *                   EXPECTED RECORD COUNT 'GE' UNLOADED REC COUNT         
      * OTHERWISE, JUST CLOSE IT.                                               
      *****************************************************************         
                                                                                
           IF GQ-HANDLE-2 = ZERO                                                
              GO TO 5700-CLOSE-INPUT-QUEUE-EXIT.                                
                                                                                
           EVALUATE TRUE                                                        
            WHEN DELETE-PURGE-Q                                                 
               MOVE MQCO-DELETE-PURGE    TO OPTIONS                             
            WHEN DELETE-PURGE-Q-EQ                                              
               IF WS-RECNO = WS-IN-RECORD-COUNT                                 
                  MOVE MQCO-DELETE-PURGE TO OPTIONS                             
               ELSE                                                             
                  MOVE MQCO-NONE         TO OPTIONS                             
               END-IF                                                           
            WHEN DELETE-PURGE-Q-GE                                              
               IF WS-RECNO NOT < WS-IN-RECORD-COUNT                             
                  MOVE MQCO-DELETE-PURGE TO OPTIONS                             
               ELSE                                                             
                  MOVE MQCO-NONE         TO OPTIONS                             
               END-IF                                                           
            WHEN OTHER                                                          
               MOVE MQCO-NONE            TO OPTIONS                             
           END-EVALUATE.                                                        
                                                                                
           CALL MQCLOSE-BATCH    USING HCONN                                    
                                       GQ-HANDLE-2                              
                                       OPTIONS                                  
                                       COMPLETION-CODE                          
                                       REASON.                                  
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              IF OPTIONS = MQCO-DELETE-PURGE                                    
                DISPLAY 'DELETE INPUT QUEUE FAILED, REASON ', REASON            
                 SET INPUT-QUEUE-DELETE-FAILED TO TRUE                          
                 PERFORM 9100-SET-RETURN-CODE THRU                              
                         9100-SET-RETURN-CODE-EXIT                              
                 SET FATAL-ERROR                 TO TRUE                        
              ELSE                                                              
                 DISPLAY 'CLOSE INPUT QUEUE FAILED, REASON ', REASON            
                 SET INPUT-QUEUE-CLOSE-FAILED TO TRUE                           
                 PERFORM 9100-SET-RETURN-CODE THRU                              
                         9100-SET-RETURN-CODE-EXIT                              
                 SET FATAL-ERROR                TO TRUE                         
              END-IF                                                            
           END-IF.                                                              
                                                                                
       5700-CLOSE-INPUT-QUEUE-EXIT.                                             
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * SET PARSER UP TO RETRIEVE A TAG                                         
      ******************************************************************        
       7100-SETUP-PARS-GET-CALL.                                                
                                                                                
           MOVE LENGTH OF BUFFER-AREA      TO PRSI-MAX-LENGTH-AREA1.            
           MOVE PRSI-PACKET-LENGTH         TO PRSI-MAX-LENGTH-AREA3.            
           MOVE PRSO-INDEX-HANDLE          TO PRSI-INDEX-HANDLE.                
                                                                                
       7100-SETUP-PARS-GET-CALL-EXIT.                                           
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * CHECK RESULT OF PARSER GET                                              
      ******************************************************************        
       7110-CHECK-PARS-GET-CALL.                                                
                                                                                
           EVALUATE TRUE                                                        
           WHEN PRSO-NO-ERROR                                                   
              CONTINUE                                                          
           WHEN OTHER                                                           
              SET PARSER-RETRIEVE-ERROR    TO TRUE                              
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR              TO TRUE                              
              MOVE PARSER-GET              TO WS-PARSE-ERR-PGM                  
              PERFORM 7500-DISPLAY-PARSE-ERROR THRU                             
                      7500-DISPLAY-PARSE-ERROR-EXIT                             
           END-EVALUATE.                                                        
                                                                                
       7110-CHECK-PARS-GET-CALL-EXIT.                                           
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * SET PARSER UP TO UPDATE A TAGGED STREAM                                 
      ******************************************************************        
       7200-SETUP-PARS-PUT-CALL.                                                
                                                                                
           SET  PRSI-SECTION-BODY          TO TRUE.                             
           MOVE TARGET-AREA                TO BUFFER-AREA.                      
           MOVE LENGTH OF BUFFER-AREA      TO PRSI-MAX-LENGTH-AREA1.            
           MOVE PRSI-PACKET-LENGTH         TO PRSI-MAX-LENGTH-AREA2.            
           MOVE LENGTH OF TARGET-AREA      TO PRSI-MAX-LENGTH-AREA3.            
           MOVE PRSO-INDEX-HANDLE          TO PRSI-INDEX-HANDLE.                
                                                                                
       7200-SETUP-PARS-PUT-CALL-EXIT.                                           
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * CHECK RESULT OF PARSER PUT                                              
      ******************************************************************        
       7210-CHECK-PARS-PUT-CALL.                                                
                                                                                
           EVALUATE TRUE                                                        
           WHEN PRSO-NO-ERROR                                                   
              CONTINUE                                                          
           WHEN OTHER                                                           
              SET PARSER-UPDATE-ERROR      TO TRUE                              
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR              TO TRUE                              
              MOVE PARSER-PUT              TO WS-PARSE-ERR-PGM                  
              PERFORM 7500-DISPLAY-PARSE-ERROR THRU                             
                      7500-DISPLAY-PARSE-ERROR-EXIT                             
           END-EVALUATE.                                                        
                                                                                
       7210-CHECK-PARS-PUT-CALL-EXIT.                                           
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * DISPLAY PARSER ERROR TABLE INFO                                         
      ******************************************************************        
       7500-DISPLAY-PARSE-ERROR.                                                
                                                                                
           EVALUATE TRUE                                                        
            WHEN PRSO-NO-ERROR                                                  
               CONTINUE                                                         
            WHEN PRSO-NON-CRITICAL-ERROR                                        
               SET PARSE-WARNINGS TO TRUE                                       
               DISPLAY '?? PARSER WARNINGS: ' WS-PARSE-ERR-PGM                  
            WHEN PRSO-NOT-FOUND                                                 
            WHEN PRSO-CRITICAL-ERROR                                            
               SET PARSE-ERRORS   TO TRUE                                       
               DISPLAY '## PARSER ERRORS: ' WS-PARSE-ERR-PGM                    
           END-EVALUATE.                                                        
                                                                                
           IF PRSO-ERROR-COUNT = ZERO                                           
              CONTINUE                                                          
           ELSE                                                                 
                                                                                
              DISPLAY '  ----------'                                            
                      '------------------------------------------------'        
              DISPLAY '            '                                            
                      'GRP-LVL-1    GRP-LVL-2    GRP-LVL-3  PACKET  ERR'        
              DISPLAY '            '                                            
                      ' (ID,OCC)     (ID,OCC)     (ID,OCC)    ID   CODE'        
              DISPLAY '  ----------'                                            
                      '------------------------------------------------'        
              PERFORM  VARYING SUB                                              
                       FROM    1                                                
                       BY      1                                                
                       UNTIL   SUB > PRSO-ERROR-COUNT                           
                                                                                
                 DISPLAY '   ' PARSE-ERROR-IND '(' SUB ') '                     
                         '  (' PRSO-LVL1-ID (SUB) ','                           
                                PRSO-LVL1-OCCUR (SUB) ')'                       
                         '  (' PRSO-LVL2-ID (SUB) ','                           
                                PRSO-LVL2-OCCUR (SUB) ')'                       
                         '  (' PRSO-LVL3-ID (SUB) ','                           
                                PRSO-LVL3-OCCUR (SUB) ')'                       
                         '  '  PRSO-ERROR-PACKET (SUB) '  '                     
                         '  '  PRSO-ERROR-CODE (SUB)                            
                                                                                
                 IF PRSO-MESSAGE-VERSION < 2                                    
                    DISPLAY '        ' PRSO-V1-MESSAGE (SUB)                    
                 END-IF                                                         
                                                                                
              END-PERFORM                                                       
                                                                                
              DISPLAY '  ----------'                                            
                      '------------------------------------------------'        
           END-IF.                                                              
                                                                                
       7500-DISPLAY-PARSE-ERROR-EXIT.                                           
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * BASED ON HOW THE QUEUE TO FILE PROCESS WAS INITIATED                    
      * END THE PROCESS ACCORDINGLY:                                            
      * 1. FOR ANY QUEUED REQUESTS - GENERATE REPLY MESSAGE                     
      * 2. FOR LEADCARD REQUESTS - GENERATE REPLY MESSAGE IF OPTIONAL           
      *                            REPLYQ PARAMETERS ARE PROVIDED.              
      *    USE THE MESSAGE FORMAT OF THE QUEUE RECORDS UNLOADED TO              
      *    BUILD THE REPLY MESSAGE (DEFAULT TO VERSION 2 IF REQUIRED)           
      ******************************************************************        
       8000-END-PROCESS.                                                        
                                                                                
           IF LEADCARD-REQUEST                                                  
              SET REQUEST-Q-EMPTY TO TRUE                                       
              IF REPLY-MSG-EXPECTED                                             
                 MOVE OUTPUT-RECORD TO WS-REQUEST-MESSAGE                       
                 IF WS-REPLY-MSG-VERSION NOT NUMERIC                            
                    SET REPLY-MSG-VERSION-2 TO TRUE                             
                 END-IF                                                         
                                                                                
                 PERFORM 8100-COMPLETION-MESSAGE THRU                           
                         8100-COMPLETION-MESSAGE-EXIT                           
              END-IF                                                            
           ELSE                                                                 
              PERFORM 8100-COMPLETION-MESSAGE THRU                              
                      8100-COMPLETION-MESSAGE-EXIT                              
           END-IF.                                                              
                                                                                
           DISPLAY '=------------------------------------------------='.        
           DISPLAY '=             QTOF REQUEST COMPLETE              ='.        
           DISPLAY '=------------------------------------------------='.        
                                                                                
      *                                                                         
      *    RESET WORKING STORAGE AREA FOR NEXT REQUEST                          
      *                                                                         
           MOVE ZERO              TO WS-RECNO.                                  
                                                                                
       8000-END-PROCESS-EXIT.                                                   
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * WRITE PROCESSING INFORMATION IN SINGLE REPLY MESSAGE                    
      * INCLUDING RECORD COUNT, AND RETURN CODE                                 
      ******************************************************************        
       8100-COMPLETION-MESSAGE.                                                 
                                                                                
           INITIALIZE PARSER-CONTROL-AREA.                                      
           INITIALIZE PARSER-RETURN-AREA.                                       
           INITIALIZE TARGET-AREA.                                              
      *                                                                         
      * CLEAR ALL SECTIONS (HEADER, BODY, AND TRAILER) OF THE                   
      * REQUEST MESSAGE IN ORDER TO BUILD THE SKELETON REPLY MESSAGE            
      * --> FIXED HEADER + '9998'                                               
      *                                                                         
           SET  PRSI-SECTION-ALL              TO TRUE.                          
           SET  PRSI-NORMAL                   TO TRUE.                          
           SET  PRSI-NO-OPTIONS               TO TRUE.                          
           MOVE WS-REQUEST-MESSAGE            TO BUFFER-AREA.                   
           MOVE LENGTH OF BUFFER-AREA         TO PRSI-MAX-LENGTH-AREA1.         
           MOVE LENGTH OF TARGET-AREA         TO PRSI-MAX-LENGTH-AREA3.         
                                                                                
           CALL PARSER-CLEAR            USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              TARGET-AREA                       
                                              PARSER-RETURN-AREA.               
      *                                                                         
      * PUT RECORD COUNT ON REPLY MESSAGE                                       
      *                                                                         
           MOVE WS-FLD-RECORD-COUNT           TO PRSI-PACKET-ID.                
           SET  TYPE-LONG-INT-COMP            TO TRUE.                          
           MOVE LENGTH OF WS-RECNO            TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7200-SETUP-PARS-PUT-CALL THRU                                
                   7200-SETUP-PARS-PUT-CALL-EXIT.                               
                                                                                
           CALL PARSER-PUT              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              WS-RECNO-X                        
                                              TARGET-AREA                       
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7210-CHECK-PARS-PUT-CALL THRU                                
                   7210-CHECK-PARS-PUT-CALL-EXIT.                               
                                                                                
      *                                                                         
      * PUT RETURN CODE ON REPLY MESSAGE                                        
      *                                                                         
           MOVE WS-FLD-RETURN-CODE            TO PRSI-PACKET-ID.                
           SET  TYPE-ALPHANUMERIC             TO TRUE.                          
           MOVE LENGTH OF WS-RETURN-CODE      TO PRSI-PACKET-LENGTH.            
                                                                                
           PERFORM 7200-SETUP-PARS-PUT-CALL THRU                                
                   7200-SETUP-PARS-PUT-CALL-EXIT.                               
                                                                                
           CALL PARSER-PUT              USING PARSER-CONTROL-AREA               
                                              BUFFER-AREA                       
                                              WS-RETURN-CODE                    
                                              TARGET-AREA                       
                                              PARSER-RETURN-AREA.               
                                                                                
           PERFORM 7210-CHECK-PARS-PUT-CALL THRU                                
                   7210-CHECK-PARS-PUT-CALL-EXIT.                               
                                                                                
           MOVE TARGET-AREA               TO BUFFER-AREA.                       
           MOVE PRSO-DATA-LENGTH          TO BUFFER-LENGTH.                     
           MOVE WS-REQUEST-MSGID          TO MQMD-CORRELID.                     
           MOVE LOW-VALUES                TO MQMD-MSGID.                        
           MOVE MQPER-PERSISTENT          TO MQMD-PERSISTENCE.                  
           COMPUTE MQPMO-OPTIONS = MQPMO-NO-SYNCPOINT +                         
                                   MQPMO-FAIL-IF-QUIESCING.                     
           MOVE 1                         TO MQMD-PRIORITY.                     
                                                                                
           MOVE MQOT-Q                    TO MQOD-OBJECTTYPE.                   
           MOVE WS-REPLY-QNAME            TO MQOD-OBJECTNAME.                   
           MOVE WS-REPLY-QMGRNAME         TO MQOD-OBJECTQMGRNAME.               
                                                                                
           CALL MQPUT1-BATCH           USING HCONN                              
                                             MQ-OBJECT-DESCRIPTOR               
                                             MQ-MESSAGE-DESCRIPTOR              
                                             MQ-PUT-MESSAGE-OPTIONS             
                                             BUFFER-LENGTH                      
                                             BUFFER-AREA                        
                                             COMPLETION-CODE                    
                                             REASON.                            
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              DISPLAY 'WRITE TO REPLY QUEUE FAILED, REASON', REASON             
              SET REPLY-QUEUE-WRITE-FAILED TO TRUE                              
              PERFORM 9100-SET-RETURN-CODE THRU                                 
                      9100-SET-RETURN-CODE-EXIT                                 
              SET FATAL-ERROR              TO TRUE                              
           END-IF.                                                              
                                                                                
       8100-COMPLETION-MESSAGE-EXIT.                                            
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * CLOSE REQUEST QUEUE                                                     
      * DISCONNECT FROM QUEUE MANAGER                                           
      ******************************************************************        
       9000-FINAL.                                                              
                                                                                
      *                                                                         
      * CLOSE REQUEST QUEUE                                                     
      *                                                                         
           PERFORM 5200-CLOSE-REQUEST-QUEUE THRU                                
                   5200-CLOSE-REQUEST-QUEUE-EXIT.                               
                                                                                
      *                                                                         
      * DISCONNECT FROM QUEUE MANAGER                                           
      *                                                                         
           CALL MQDISC-BATCH         USING HCONN                                
                                           COMPLETION-CODE                      
                                           REASON.                              
                                                                                
           IF COMPLETION-CODE NOT = MQCC-OK                                     
              SET QUEUE-MGR-DISCONNECT-FAILED TO TRUE                           
              DISPLAY 'Q MANAGER DISCONNECT FAILED, REASON ', REASON            
           END-IF.                                                              
                                                                                
           MOVE WS-FINAL-RETURN-CODE TO RETURN-CODE.                            
                                                                                
       9000-FINAL-EXIT.                                                         
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * SET FINAL COMPLETION CODE                                               
      * - ENSURE THE FIRST ERROR ENCOUNTERED IS THE ERROR-CODE DISPLAYED        
      *   OTHER MESSAGES MAY BE DISPLAYED BUT ONLY ONE ERROR-CODE               
      ******************************************************************        
       9100-SET-RETURN-CODE.                                                    
           IF WS-FINAL-RETURN-CODE = ZERO                                       
              MOVE WS-RETURN-CODE TO WS-FINAL-RETURN-CODE                       
           END-IF.                                                              
       9100-SET-RETURN-CODE-EXIT.                                               
           EXIT.                                                                
                                                                                
