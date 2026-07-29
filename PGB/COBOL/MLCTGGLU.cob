CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLCTGGLU.                                                 
      *AUTHOR.        FRED MUELLER.                                             
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.                                                            
      *DATE-COMPILED.                                                           
      *----------------------------------------------------------------*        
      *                                                                         
      *  SYSTEM    : GROUP BENEFITS BUSINESS CONTENT LOOKUP SERVICES            
      *                                                                         
      *  LANGUAGE  : COBOL II                                                   
      *                                                                         
      *  THIS MODULE WILL PERFORM ALL CONTENT LOOKUP SERVICES FOR THE           
      *  GENERAL CONTENT SERVICE.  THIS PROGRAM CAN BE CALLED DIRECTLY          
      *  BUT IT IS RECOMMENDED THAT IT BE INVOKED THROUGH MLCTOCLU.             
      *  THIS PROGRAM WILL CALL MLCTGCIO TO ACCESS THE TABLE LOOKUP             
      *  DATA STORE (LOGICAL RECORD NAME PASSED IN ITS PROTOCOL).  IT           
      *  CAN BE INVOKED FROM BOTH CICS AND BATCH TO ACCESS CONTENT              
      *  BOTH DYNAMICALLY AND VIRTUALLY AFTER LOADING DATA INTO PROGRAM         
      *  WORKING STORAGE.  IN GENERAL, CICS ACCESS WILL LIKELY BE               
      *  DYNAMIC WHILE BATCH ACCESS WILL BE VIRTUAL TO IMPROVE CPU              
      *  AND I/O PERFORMANCE DURING HIGH VOLUME ACCESS WITHIN A SINGLE          
      *  RUN-UNIT.  TO DISPLAY THE CURRENT STATS FOR A SINGLE RUN-UNIT          
      *  CALL THIS PROGRAM WITH THE CONTENT-NAME = PROGRAM-ID.                  
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  MAY 2002   F MUELLER  CREATED                                     
      *  002  OCT 2002   F MUELLER  CHANGES TO REDUCE BUFFER SPACE WHEN         
      *                             CALLING THE I/O MODULE MLCTGCIO             
      *  003  JAN 2003   N JEFFREY  CHANGE TO IMPROVE PERFORMANCE AND           
      *                             REDUCE I/O. ONLY SPECIFIC TABLES            
      *                             CAN HAVE DEFAULTS AS PER MLCTRRLU           
      *  004  JAN 2003   F MUELLER  ADD TERM DATE CHECKING LOGIC.               
      *                             FIX LOGIC FOR SEARCHING HISTORY.            
      *  005  AUG 2008   IBM        ENTERPRISE COMPILER UPGRADE(ECU)            
      *----------------------------------------------------------------*        
      /                                                                         
       ENVIRONMENT DIVISION.                                                    
                                                                                
       CONFIGURATION SECTION.                                                   
                                                                                
       SOURCE-COMPUTER. IBM-370.                                                
       OBJECT-COMPUTER. IBM-370.                                                
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
      /                                                                         
       WORKING-STORAGE SECTION.                                                 
       01  FILLER                             PIC X(40) VALUE                   
               '**   MLCTGGLU WORKING STORAGE BEGINS  **'.                      
                                                                                
       01  WS-VARIABLES.                                                        
           05  SUB                          PIC S9(4) COMP.                     
           05  SUB2                         PIC S9(4) COMP.                     
           05  WS-CALL-TYPE-SW              PIC X(1) VALUE 'F'.                 
               88  FIRST-CALL               VALUE 'F'.                          
               88  NORMAL-CALL              VALUE 'N'.                          
               88  STATUS-CALL              VALUE 'S'.                          
           05  WS-SAVE-EFF-DATE-COMP        PIC 9(08).                          
           05  WS-CURR-SEARCH-KEY           PIC X(64).                          
           05  FILLER  REDEFINES  WS-CURR-SEARCH-KEY.                           
               10  FILLER                   PIC X(48).                          
               10  WS-CURR-EFF-DATE-COMP    PIC 9(08).                          
               10  WS-CURR-PROC-DATE-COMP   PIC 9(08).                          
           05  WS-SAVE-SEARCH-KEY           PIC X(64).                          
           05  WS-CURR-TABLE                PIC S9(04) COMP.                    
           05  WS-RESULT-DATA-OFFSET        PIC S9(04) COMP.                    
           05  WS-NUMERIC-DISPLAY           PIC ZZZZZZZZ9.                      
           05  WS-HISTORY-BROWSE-FLAG       PIC X(1).                           
               88  HIST-BROWSE-START        VALUE 'B'.                          
               88  HIST-BROWSE-END          VALUE 'E'.                          
       01  SEARCH-WORK-VARIABLES.                                               
           05  SWV-LOW-ADDRESS              PIC S9(04) COMP.                    
           05  SWV-HIGH-ADDRESS             PIC S9(04) COMP.                    
           05  SWV-ENTRY                    PIC S9(04) COMP.                    
           05  SWV-SEARCH-FLAG              PIC X(1).                           
               88  SWV-BEGIN-SEARCH         VALUE 'B'.                          
               88  SWV-END-SEARCH           VALUE 'E'.                          
                                                                                
       01  WS-CONSTANTS.                                                        
           05  WS-PROGRAM-ID                PIC X(08) VALUE 'MLCTGGLU'.         
           05  WS-CALLED-MODULES.                                               
               10  MLCTGCIO                 PIC X(08) VALUE 'MLCTGCIO'.         
               10  GC2DATE                  PIC X(08) VALUE 'GC2DATE '.         
               10  CGC2DATE                 PIC X(08) VALUE 'CGC2DATE'.         
           05  WS-HIGH-DATE                 PIC 9(08) VALUE 99999999.           
           05  WS-MAX-LOOKUP-COUNT          PIC S9(04) COMP.                    
           05  WS-MAX-INDEX-COUNT           PIC S9(04) COMP.                    
           05  WS-MAX-RESULTS-SIZE          PIC S9(08) COMP.                    
           05  WS-ADD-STORAGE-BLOCKS        PIC S9(04) COMP.                    
           05  WS-MAX-STORAGE-BLOCKS        PIC S9(04) COMP.                    
                                                                                
       01  WS-STATUS-DETAIL.                                                    
           05  FILLER                       PIC X(03) VALUE SPACES.             
           05  WS-LOOKUP-NAME               PIC X(08).                          
           05  FILLER                       PIC X(06) VALUE SPACES.             
           05  WS-LOOKUP-ALGORITHM          PIC X(08).                          
           05  FILLER                       PIC X(06) VALUE SPACES.             
           05  WS-NUM-ENTRIES               PIC ZZZZZZZZ9.                      
           05  FILLER                       PIC X(06) VALUE SPACES.             
           05  WS-NUM-HITS                  PIC ZZZZZZZZ9.                      
                                                                                
       01  WS-STATUS-TITLE.                                                     
           05  FILLER                       PIC X(03) VALUE SPACES.             
           05  FILLER                       PIC X(08) VALUE '  NAME  '.         
           05  FILLER                       PIC X(06) VALUE SPACES.             
           05  FILLER                       PIC X(08) VALUE ' ACCESS '.         
           05  FILLER                       PIC X(06) VALUE SPACES.             
           05  FILLER                       PIC X(09) VALUE 'ROW COUNT'.        
           05  FILLER                       PIC X(06) VALUE SPACES.             
           05  FILLER                       PIC X(09) VALUE 'HIT COUNT'.        
      *-----------------------------------------------------------------        
      *   GACDATE PROTOCOL                                                      
      *-----------------------------------------------------------------        
       01  GAC-DATE-PARAMETERS.             COPY GARDATEP.                      
                                                                                
      *----------------------------------------------------------------*        
      *  MLCTGCIO INPUT/OUTPUT PARAMETERS                                       
      *----------------------------------------------------------------*        
       01  MLCTGCIO-PROTOCOL.           COPY MLCTRCIO.                          
       01  MLCTGCIO-DUMMY-RECORD        PIC X(01).                              
       01  MLCTGCIO-DATA-RECORD         PIC X(15100).                           
       01  CONTENT-RECORD     REDEFINES MLCTGCIO-DATA-RECORD.                   
                                        COPY XC4CFCNT.                          
                                                                                
      *----------------------------------------------------------------*        
      *  INTERNAL INPUTS                                                        
      *----------------------------------------------------------------*        
       01  CONTENT-INPUT-AREA.                                                  
           05  MLCT-CONTENT-NAME           PIC X(08).                           
           05  MLCT-CONTENT-ARGUMENT       PIC X(40).                           
           05  MLCT-CONTENT-EFF-DATE       PIC 9(08).                           
           05  MLCT-CONTENT-PROC-DATE      PIC 9(08).                           
           05  MLCT-LOOKUP-OPTION          PIC X(01).                           
               88  MLCT-NORMAL-LOOKUP      VALUE ' '.                           
               88  MLCT-OPTIMIZED-LOOKUP   VALUE 'O'.                           
           05  MLCT-DEFAULT-OPTION         PIC X(01).                           
               88  MLCT-LOOKUP-DEF-NONE    VALUE ' '.                           
               88  MLCT-LOOKUP-DEFAULT     VALUE 'D'.                           
           05  MLCT-HISTORY-OPTION         PIC X(01).                           
               88  MLCT-HISTORY-CURRENT    VALUE ' '.                           
               88  MLCT-HISTORY-BROWSE     VALUE 'H'.                           
           05  MLCT-MAX-DATA-LENGTH        PIC S9(04) COMP.                     
           05  MLCT-HEADER-LENGTH          PIC S9(04) COMP.                     
                                                                                
      *----------------------------------------------------------------*        
      *  INTERNAL RETURNS                                                       
      *----------------------------------------------------------------*        
       01  CONTENT-OUTPUT-AREA.                                                 
           05  MLCT-RETURN-STATUS.                                              
               10  MLCT-RETURN-CODE        PIC X(02).                           
                   88  MLCT-RET-OK         VALUE '00'.                          
                   88  MLCT-RET-NOT-FOUND  VALUE '02'.                          
                   88  MLCT-RET-ERROR      VALUE '99'.                          
               10  MLCT-RETURN-SUB-CODE    PIC X(02).                           
           05  MLCT-OK-STATUS    REDEFINES MLCT-RETURN-STATUS                   
                                           PIC X(04).                           
                   88  MLCT-OK                  VALUE '00  '.                   
                   88  MLCT-DEFAULT             VALUE '0001'.                   
                   88  MLCT-TRUNCATED           VALUE '0002'.                   
                   88  MLCT-TRUNC-DEFAULT       VALUE '0003'.                   
                   88  MLCT-NON-CRITICAL-ERROR  VALUE '0099'.                   
           05  MLCT-NF-STATUS    REDEFINES MLCT-RETURN-STATUS                   
                                           PIC X(04).                           
                   88  MLCT-NOT-FOUND           VALUE '02  '.                   
                   88  MLCT-TABLE-NOT-FOUND     VALUE '0201'.                   
                   88  MLCT-ENTRY-NOT-FOUND     VALUE '0202'.                   
                   88  MLCT-ENTRY-TERMINATED    VALUE '0203'.                   
           05  MLCT-ERROR-STATUS REDEFINES MLCT-RETURN-STATUS                   
                                           PIC X(04).                           
                   88  MLCT-ERROR               VALUE '99  '.                   
                   88  MLCT-INVALID-EFF-DATE    VALUE '9901'.                   
                   88  MLCT-INVALID-PROC-DATE   VALUE '9902'.                   
                   88  MLCT-INVALID-DATA-LENGTH VALUE '9903'.                   
                   88  MLCT-IO-ERROR            VALUE '9904'.                   
           05  MLCT-OUTPUT-LENGTH          PIC S9(04) COMP.                     
           05  MLCT-ERROR-DETAILS.                                              
               10  MLCT-ERR-PGM-ID         PIC X(8).                            
               10  MLCT-ERR-PGM-STATUS     PIC X(8).                            
               10  MLCT-ERR-PGM-LVL        PIC X(30).                           
               10  MLCT-ERR-PGM-DESC       PIC X(60).                           
                                                                                
      *----------------------------------------------------------------*        
      *  INTERNAL REGISTRY OF LOOKUPS PREVIOUSLY ACCESSED.                      
      *----------------------------------------------------------------*        
      *  BATCH OR CICS STORAGE REQUIREMENTS ARE CUSTOMIZED HERE                 
      *  - ALWAYS LEAVE AT THE END DUE TO OCCURS DEPENDING ON.                  
      *----------------------------------------------------------------*        
           COPY MLCTRREG.                                                       
                                                                                
       01  FILLER                              PIC X(40) VALUE                  
               '***  MLCTGGLU WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  LOOKUP PARMS                                                           
      *----------------------------------------------------------------*        
       01  MLCTGGLU-PROTOCOL.                                                   
           COPY MLCTRGLU.                                                       
                                                                                
       01  MLCTGGLU-CONTENT                    PIC X(01).                       
                                                                                
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING MLCTGGLU-PROTOCOL                               
                                MLCTGGLU-CONTENT.                               
      *----------------------------------------------------------------*        
      ****************************************************************          
      *    MAINLINE                                                             
      ****************************************************************          
       0000-MAINLINE.                                                           
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
                                                                                
           PERFORM 2000-PROCESS-REQUEST THRU 2000-EXIT.                         
                                                                                
           PERFORM 3000-FINALIZATION    THRU 3000-EXIT.                         
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      ****************************************************************          
      *    INITIALIZE RETURNS. SETUP INITIAL STORAGE (FIRST CALL).              
      *    SEARCH THE INTERNAL REGISTRY TO DETERMINE WHAT ALGORITHM             
      *    TO USE FOR ACCESSING THE CONTENT SPECIFIED.                          
      ****************************************************************          
       1000-INITIALIZATION.                                                     
           SET MLCT-OK                    TO TRUE.                              
           MOVE ZERO                      TO MLCT-OUTPUT-LENGTH.                
           MOVE SPACES                    TO MLCT-ERROR-DETAILS.                
                                                                                
           INITIALIZE MLCTGGLU-OUTPUT.                                          
                                                                                
           IF FIRST-CALL                                                        
              MOVE ZERO                   TO WS-CURR-TABLE                      
              PERFORM 1100-SETUP-INTERNAL-STORAGE  THRU 1100-EXIT               
              PERFORM 6000-SET-CURRENT-CAPACITY    THRU 6000-EXIT               
           END-IF.                                                              
                                                                                
           PERFORM 1110-SETUP-PARAMETERS           THRU 1110-EXIT.              
           IF MLCT-CONTENT-NAME = WS-PROGRAM-ID                                 
              SET STATUS-CALL  TO TRUE                                          
              PERFORM 5000-DISPLAY-CURRENT-STATUS  THRU 5000-EXIT               
           ELSE                                                                 
              SET NORMAL-CALL TO TRUE                                           
              PERFORM 1200-OBTAIN-CURRENT-REGISTRY THRU 1200-EXIT               
           END-IF.                                                              
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
      ****************************************************************          
      *    INITIALIZE THE SIZE OF EACH TABLE INCLUDING MAXIMUMS THAT            
      *    CAN NOT BE SPECIFIED USING A VALUE CLAUSE.                           
      *    INITIALLY, ALLOCATE 10 BLOCKS OF STORAGE (4K) FOR RESULTS            
      *    UP TO A MAXIMUM CAPACITY OF 1000 BLOCKS.                             
      ****************************************************************          
       1100-SETUP-INTERNAL-STORAGE.                                             
      *                                                                         
      *  BATCH OR CICS STORAGE INITIALIZATION                                   
      *                                                                         
           COPY MLCTCRIN.                                                       
                                                                                
           INITIALIZE LOOKUP-REGISTRY-TABLE.                                    
           INITIALIZE LOOKUP-SEARCH-INDEX.                                      
           MOVE ZERO                   TO LUSI-COUNT.                           
           MOVE ZERO                   TO LUSR-SIZE.                            
           MOVE WS-ADD-STORAGE-BLOCKS  TO LUSR-DATA-BLOCK-COUNT.                
           ADD  LENGTH OF CONTENT-KEY  TO LENGTH OF CONTENT-AUDIT-INFO          
                                   GIVING MLCT-HEADER-LENGTH                    
           MOVE LENGTH OF CONTENT-DATA TO MLCT-MAX-DATA-LENGTH.                 
           ADD  1                      TO MLCT-HEADER-LENGTH                    
                                   GIVING WS-RESULT-DATA-OFFSET.                
                                                                                
       1100-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
      ****************************************************************          
      *    SETUP INTERNAL PARMS BASED ON THOSE PASSED FROM CALLING PGM          
      ****************************************************************          
       1110-SETUP-PARAMETERS.                                                   
           MOVE GGLU-LOOKUP-NAME              TO MLCT-CONTENT-NAME.             
           MOVE GGLU-LOOKUP-ARGUMENT          TO MLCT-CONTENT-ARGUMENT.         
                                                                                
           IF  GGLU-LOOKUP-EFF-DATE NUMERIC                                     
               MOVE GGLU-LOOKUP-EFF-DATE      TO MLCT-CONTENT-EFF-DATE          
           ELSE                                                                 
               SET  MLCT-INVALID-EFF-DATE     TO TRUE                           
               MOVE 'MLCTGGLU'                TO MLCT-ERR-PGM-ID                
               MOVE 'BAD DATE'                TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTGGLU - 1110'         TO MLCT-ERR-PGM-LVL               
               MOVE 'EFFECTIVE DATE NOT NUMERIC' TO MLCT-ERR-PGM-DESC           
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
           END-IF.                                                              
                                                                                
           IF  GGLU-LOOKUP-PROC-DATE NUMERIC                                    
               MOVE GGLU-LOOKUP-PROC-DATE     TO MLCT-CONTENT-PROC-DATE         
           ELSE                                                                 
               SET  MLCT-INVALID-PROC-DATE    TO TRUE                           
               MOVE 'MLCTGGLU'                TO MLCT-ERR-PGM-ID                
               MOVE 'BAD DATE'                TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTGGLU - 1110'         TO MLCT-ERR-PGM-LVL               
               MOVE 'PROCESS DATE NOT NUMERIC' TO MLCT-ERR-PGM-DESC             
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
           END-IF.                                                              
                                                                                
           IF  GGLU-MAX-DATA-LENGTH > ZERO                                      
               CONTINUE                                                         
           ELSE                                                                 
               SET  MLCT-INVALID-DATA-LENGTH  TO TRUE                           
               MOVE 'MLCTGGLU'                TO MLCT-ERR-PGM-ID                
               MOVE 'BAD LEN '                TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTGGLU - 1110'         TO MLCT-ERR-PGM-LVL               
               MOVE 'MAX DATA LENGTH NOT > ZERO' TO MLCT-ERR-PGM-DESC           
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
           END-IF.                                                              
                                                                                
           MOVE GGLU-LOOKUP-OPTION          TO MLCT-LOOKUP-OPTION.              
           MOVE GGLU-DEFAULT-OPTION         TO MLCT-DEFAULT-OPTION.             
           MOVE GGLU-HISTORY-OPTION         TO MLCT-HISTORY-OPTION.             
                                                                                
       1110-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
      ****************************************************************          
      *    SEARCH THE INTERNAL REGISTRY TO DETERMINE WHAT ALGORITHM             
      *    TO USE FOR ACCESSING THE LOOKUP SPECIFIED.   IF REGISTRY             
      *    IS NOT FOUND, SETUP THE TABLE AND REGISTER IT FOR USE.               
      ****************************************************************          
       1200-OBTAIN-CURRENT-REGISTRY.                                            
                                                                                
           IF WS-CURR-TABLE > ZERO  AND                                         
              LURT-LOOKUP-NAME (WS-CURR-TABLE) = MLCT-CONTENT-NAME              
               CONTINUE                                                         
           ELSE                                                                 
               MOVE +1 TO SUB                                                   
               PERFORM                                                          
                  UNTIL SUB > LURT-COUNT OR                                     
                        LURT-LOOKUP-NAME (SUB) = MLCT-CONTENT-NAME              
                                                                                
                    ADD +1 TO SUB                                               
               END-PERFORM                                                      
               MOVE SUB    TO WS-CURR-TABLE                                     
                                                                                
               EVALUATE TRUE                                                    
               WHEN  WS-CURR-TABLE > WS-MAX-LOOKUP-COUNT                        
                  MOVE WS-MAX-LOOKUP-COUNT  TO WS-CURR-TABLE                    
                  SET  MLCT-NORMAL-LOOKUP   TO TRUE                             
                  PERFORM 1300-SETUP-CURRENT-TABLE THRU 1300-EXIT               
               WHEN  WS-CURR-TABLE > LURT-COUNT                                 
                  PERFORM 1300-SETUP-CURRENT-TABLE THRU 1300-EXIT               
               END-EVALUATE                                                     
           END-IF.                                                              
                                                                                
                                                                                
       1200-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      ****************************************************************          
      *    REGISTER TABLE ACCESS AND LOAD CURRENT TABLE (IF REQUIRED)           
      ****************************************************************          
       1300-SETUP-CURRENT-TABLE.                                                
                                                                                
           PERFORM 1310-UPDATE-LOOKUP-REGISTRY THRU 1310-EXIT.                  
                                                                                
           IF LURT-DYNAMIC-LOOKUP (WS-CURR-TABLE)                               
              CONTINUE                                                          
           ELSE                                                                 
              PERFORM 1320-LOAD-CURRENT-TABLE  THRU 1320-EXIT                   
           END-IF.                                                              
       1300-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       1310-UPDATE-LOOKUP-REGISTRY.                                             
                                                                                
           MOVE WS-CURR-TABLE     TO LURT-COUNT.                                
           MOVE MLCT-CONTENT-NAME TO LURT-LOOKUP-NAME (WS-CURR-TABLE).          
           IF MLCT-OPTIMIZED-LOOKUP                                             
              SET LURT-CACHED-LOOKUP  (WS-CURR-TABLE) TO TRUE                   
           ELSE                                                                 
              SET LURT-DYNAMIC-LOOKUP (WS-CURR-TABLE) TO TRUE                   
           END-IF.                                                              
                                                                                
       1310-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
       1320-LOAD-CURRENT-TABLE.                                                 
      *                                                                         
      * LURT-DEFAULT-ENTRY OF -1 MEANS WE DO NOT WANT A DEFAULT                 
      * LURT-DEFAULT-ENTRY OF 0 MEANS WE WILL TAKE A DEFAULT BUT                
      *    WE DON'T KNOW WHICH ONE IT IS YET                                    
      * LURT-DEFAULT-ENTRY ANYTHING ELSE POINTS TO THE DEF ENTRY                
      *                                                                         
           IF MLCT-LOOKUP-DEFAULT                                               
              MOVE ZERO TO       LURT-DEFAULT-ENTRY (WS-CURR-TABLE)             
           ELSE                                                                 
              MOVE -1   TO       LURT-DEFAULT-ENTRY (WS-CURR-TABLE)             
           END-IF.                                                              
                                                                                
           MOVE ZEROS   TO       LURT-FIRST-ENTRY   (WS-CURR-TABLE)             
                                 LURT-LAST-ENTRY    (WS-CURR-TABLE)             
                                 LURT-NUM-ENTRIES   (WS-CURR-TABLE)             
                                 LURT-NUM-HITS      (WS-CURR-TABLE).            
           PERFORM 1321-OBTAIN-FIRST-TABLE-ENTRY THRU 1321-EXIT.                
           PERFORM 1322-LOAD-CURRENT-TABLE-ENTRY THRU 1322-EXIT                 
              UNTIL GCIO-NOT-FOUND                                              
                 OR LURT-DYNAMIC-LOOKUP (WS-CURR-TABLE).                        
                                                                                
           PERFORM 1323-SET-LAST-TABLE-ENTRY     THRU 1323-EXIT.                
                                                                                
           SET  GCIO-OBTAIN-FINISH      TO TRUE.                                
           PERFORM 7000-CALL-MLCTGCIO THRU 7000-EXIT.                           
                                                                                
       1320-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
       1321-OBTAIN-FIRST-TABLE-ENTRY.                                           
                                                                                
           IF  GGLU-LOOKUP-LR-NAME = SPACES                                     
               SET  GCIO-CONTENT-LR   TO TRUE                                   
           ELSE                                                                 
               MOVE GGLU-LOOKUP-LR-NAME TO MLCTGCIO-LR-NAME                     
           END-IF.                                                              
                                                                                
           SET  GCIO-OBTAIN-FIRST     TO TRUE.                                  
                                                                                
           MOVE MLCT-CONTENT-NAME     TO CONTENT-KEY-NAME.                      
           MOVE LOW-VALUES            TO CONTENT-KEY-DATA.                      
                                                                                
           MOVE ZEROES                TO CONTENT-KEY-EFF-DATE-COMP              
                                         CONTENT-KEY-PROC-DATE-COMP.            
                                                                                
           MOVE CONTENT-KEY           TO MLCTGCIO-KEY-IN.                       
                                                                                
           PERFORM 1325-OBTAIN-TABLE-ENTRY THRU 1325-EXIT.                      
                                                                                
           IF  GCIO-NOT-FOUND                                                   
               SET  MLCT-TABLE-NOT-FOUND TO TRUE                                
           ELSE                                                                 
               SET  GCIO-OBTAIN-NEXT  TO TRUE                                   
           END-IF.                                                              
                                                                                
       1321-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
       1322-LOAD-CURRENT-TABLE-ENTRY.                                           
                                                                                
      *                                                                         
      * ADD A NEW ENTRY TO THE SEARCH INDEX. IF NO MORE ROOM,                   
      * SWITCH TO DYNAMIC ACCESS.                                               
      *                                                                         
           IF  LUSI-COUNT > WS-MAX-INDEX-COUNT                                  
               SET  MLCT-NON-CRITICAL-ERROR      TO TRUE                        
               MOVE 'MLCTGGLU'                   TO MLCT-ERR-PGM-ID             
               MOVE 'STRESSED'                   TO MLCT-ERR-PGM-STATUS         
               MOVE '1322-LOAD-CURRENT-TABLE'    TO MLCT-ERR-PGM-LVL            
               MOVE 'SEARCH INDEX FULL - REVERT TO DYNAMIC ACCESS'              
                                                 TO MLCT-ERR-PGM-DESC           
               SET  LURT-DYNAMIC-LOOKUP (WS-CURR-TABLE) TO TRUE                 
               GO TO 1322-EXIT                                                  
           END-IF.                                                              
                                                                                
           ADD +1             TO LUSI-COUNT.                                    
           MOVE CONTENT-KEY   TO LUSI-SEARCH-KEY (LUSI-COUNT).                  
           MOVE CONTENT-TERM-DATE                                               
                              TO LUSI-SEARCH-RESULT-TERMDT (LUSI-COUNT).        
           ADD +1 TO LUSR-SIZE GIVING                                           
                                 LUSI-SEARCH-RESULT-OFFSET (LUSI-COUNT).        
           MOVE MLCTGCIO-DATA-LENGTH TO                                         
                                 LUSI-SEARCH-RESULT-SIZE (LUSI-COUNT).          
           IF  LURT-NUM-ENTRIES (WS-CURR-TABLE) = ZERO                          
               MOVE LUSI-COUNT  TO LURT-FIRST-ENTRY (WS-CURR-TABLE)             
           END-IF.                                                              
                                                                                
           ADD +1               TO LURT-NUM-ENTRIES (WS-CURR-TABLE).            
                                                                                
      *                                                                         
      * SAVE THE LOCATION OF THE FIRST DEFAULT-VALUE FOR LATER.                 
      *                                                                         
           IF  LURT-DEFAULT-ENTRY (WS-CURR-TABLE) = ZERO  AND                   
               CONTENT-KEY-DATA                   = HIGH-VALUES                 
               MOVE LUSI-COUNT  TO LURT-DEFAULT-ENTRY (WS-CURR-TABLE)           
           END-IF.                                                              
      *                                                                         
      * ADD A NEW ENTRY TO THE RESULT AREA. IF NO MORE ROOM,                    
      * SWITCH TO DYNAMIC ACCESS.                                               
      *                                                                         
           IF  LUSR-SIZE > WS-MAX-RESULTS-SIZE                                  
               SET  MLCT-NON-CRITICAL-ERROR      TO TRUE                        
               MOVE 'MLCTGGLU'                   TO MLCT-ERR-PGM-ID             
               MOVE 'STRESSED'                   TO MLCT-ERR-PGM-STATUS         
               MOVE '1322-LOAD-CURRENT-TABLE'    TO MLCT-ERR-PGM-LVL            
               MOVE 'RESULTS AREA FULL - REVERT TO DYNAMIC ACCESS'              
                                                 TO MLCT-ERR-PGM-DESC           
               SET  LURT-DYNAMIC-LOOKUP (WS-CURR-TABLE) TO TRUE                 
               GO TO 1322-EXIT                                                  
           END-IF.                                                              
                                                                                
      *                                                                         
      * ADD ANOTHER STORAGE BLOCK IF WE ARE EXCEEDING CAPACITY.                 
      * IF NO MORE STORAGE BLOCKS AVAILABLE, SWITCH TO DYNAMIC ACCESS.          
      *                                                                         
           SUBTRACT LUSR-SIZE FROM LUSR-CAPACITY GIVING LUSR-AVAILABLE.         
                                                                                
           IF  LUSI-SEARCH-RESULT-SIZE (LUSI-COUNT) > LUSR-AVAILABLE            
               IF  LUSR-DATA-BLOCK-COUNT = WS-MAX-STORAGE-BLOCKS                
                   SET  MLCT-NON-CRITICAL-ERROR  TO TRUE                        
                   MOVE 'MLCTGGLU'               TO MLCT-ERR-PGM-ID             
                   MOVE 'STRESSED'               TO MLCT-ERR-PGM-STATUS         
                   MOVE '1322-LOAD-CURR-TABLE'   TO MLCT-ERR-PGM-LVL            
                   MOVE 'RESULTS AREA FULL - REVERT TO DYNAMIC ACCESS'          
                                                 TO MLCT-ERR-PGM-DESC           
                   SET  LURT-DYNAMIC-LOOKUP (WS-CURR-TABLE) TO TRUE             
                   GO TO 1322-EXIT                                              
               ELSE                                                             
                   ADD  WS-ADD-STORAGE-BLOCKS TO LUSR-DATA-BLOCK-COUNT          
                   MOVE LUSR-DATA-BLOCK-COUNT TO LUSR-DATA-BLOCK-COUNT          
                   PERFORM 6000-SET-CURRENT-CAPACITY THRU 6000-EXIT             
               END-IF                                                           
           END-IF.                                                              
                                                                                
      *                                                                         
      * STORE THE CONTENT IN RESULTS AREA AND UPDATE THE CURRENT SIZE           
      *                                                                         
           MOVE MLCTGCIO-DATA-RECORD                                            
             TO LUSR-DATA (LUSI-SEARCH-RESULT-OFFSET (LUSI-COUNT) :             
                           LUSI-SEARCH-RESULT-SIZE  (LUSI-COUNT)).              
           ADD  LUSI-SEARCH-RESULT-SIZE (LUSI-COUNT) TO LUSR-SIZE.              
      *                                                                         
      * GET THE NEXT TABLE ENTRY                                                
      *                                                                         
           MOVE MLCTGCIO-KEY-OUT             TO MLCTGCIO-KEY-IN.                
           PERFORM 1325-OBTAIN-TABLE-ENTRY THRU 1325-EXIT.                      
                                                                                
       1322-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
       1323-SET-LAST-TABLE-ENTRY.                                               
                                                                                
           IF LURT-NUM-ENTRIES (WS-CURR-TABLE) > 0                              
              COMPUTE                                                           
                   LURT-LAST-ENTRY  (WS-CURR-TABLE) =                           
                   LURT-FIRST-ENTRY (WS-CURR-TABLE) +                           
                   LURT-NUM-ENTRIES (WS-CURR-TABLE) -                           
                   1                                                            
              END-COMPUTE                                                       
           ELSE                                                                 
              MOVE LURT-FIRST-ENTRY (WS-CURR-TABLE) TO                          
                   LURT-LAST-ENTRY  (WS-CURR-TABLE)                             
           END-IF.                                                              
                                                                                
       1323-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       1325-OBTAIN-TABLE-ENTRY.                                                 
                                                                                
           PERFORM 7000-CALL-MLCTGCIO THRU 7000-EXIT.                           
                                                                                
           IF  GCIO-OK                                                          
               IF MLCT-CONTENT-NAME =                                           
                  MLCTGCIO-KEY-OUT (1:LENGTH OF CONTENT-KEY-NAME)               
                   CONTINUE                                                     
               ELSE                                                             
                   SET GCIO-NOT-FOUND TO TRUE                                   
               END-IF                                                           
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
           WHEN  GCIO-OK                                                        
           WHEN  GCIO-NOT-FOUND                                                 
               CONTINUE                                                         
           WHEN OTHER                                                           
               SET  MLCT-IO-ERROR             TO TRUE                           
               MOVE 'MLCTGCIO'                TO MLCT-ERR-PGM-ID                
               MOVE MLCTGCIO-ERROR-STATUS     TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTGGLU - 1325'         TO MLCT-ERR-PGM-LVL               
               STRING 'LOOKUP I/O ERROR FOR NAME=' DELIMITED BY SIZE            
                       MLCT-CONTENT-NAME           DELIMITED BY SIZE            
                       ' RC='                      DELIMITED BY SIZE            
                       MLCTGCIO-RETURN-CODE        DELIMITED BY SIZE            
                 INTO  MLCT-ERR-PGM-DESC                                        
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
           END-EVALUATE.                                                        
                                                                                
       1325-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    PROCESS LOOKUP REQUEST                                               
      *    IF DYNAMIC LOOKUP, CALL IO ROUTINE TO RETURN THE RESULT              
      *    OTHERWISE, SEARCH THE TABLE WITHIN WORKING STORAGE.                  
      *    SAVE THE LAST SEARCH ARGUMENT AND RESULTS.                           
      ****************************************************************          
       2000-PROCESS-REQUEST.                                                    
           PERFORM 2010-SETUP-SEARCH-KEY THRU 2010-EXIT.                        
                                                                                
           ADD +1   TO LURT-NUM-HITS (WS-CURR-TABLE).                           
                                                                                
           EVALUATE TRUE                                                        
           WHEN  LURT-DYNAMIC-LOOKUP (WS-CURR-TABLE)                            
              PERFORM 2100-DYNAMIC-REQUEST THRU 2100-EXIT                       
           WHEN  LURT-CACHED-LOOKUP (WS-CURR-TABLE)                             
              PERFORM 2200-CACHED-REQUEST  THRU 2200-EXIT                       
           END-EVALUATE.                                                        
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
       2010-SETUP-SEARCH-KEY.                                                   
                                                                                
           MOVE MLCT-CONTENT-NAME     TO CONTENT-KEY-NAME.                      
           MOVE MLCT-CONTENT-ARGUMENT TO CONTENT-KEY-DATA.                      
                                                                                
           SUBTRACT MLCT-CONTENT-EFF-DATE FROM WS-HIGH-DATE                     
                    GIVING CONTENT-KEY-EFF-DATE-COMP.                           
                                                                                
           SUBTRACT MLCT-CONTENT-PROC-DATE FROM WS-HIGH-DATE                    
                    GIVING CONTENT-KEY-PROC-DATE-COMP.                          
                                                                                
           MOVE CONTENT-KEY          TO WS-CURR-SEARCH-KEY                      
                                        WS-SAVE-SEARCH-KEY.                     
       2010-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *    PROCESS DYNAMIC LOOKUP REQUEST                                       
      ****************************************************************          
       2100-DYNAMIC-REQUEST.                                                    
                                                                                
           IF  GGLU-LOOKUP-LR-NAME = SPACES                                     
               SET  GCIO-CONTENT-LR   TO TRUE                                   
           ELSE                                                                 
               MOVE GGLU-LOOKUP-LR-NAME TO MLCTGCIO-LR-NAME                     
           END-IF.                                                              
                                                                                
           IF  WS-SAVE-SEARCH-KEY = LURT-LAST-SEARCH-KEY (WS-CURR-TABLE)        
               MOVE LURT-LAST-SEARCH-RESULT (WS-CURR-TABLE)                     
                                         TO MLCTGCIO-KEY-IN                     
                                            CONTENT-KEY                         
               IF CONTENT-KEY-DATA = HIGH-VALUES                                
                  SET MLCT-DEFAULT TO TRUE                                      
               END-IF                                                           
               PERFORM 2106-OBTAIN-KEYED-LOOKUP THRU 2106-EXIT                  
           ELSE                                                                 
               MOVE WS-CURR-SEARCH-KEY   TO MLCTGCIO-KEY-IN                     
               IF  MLCT-HISTORY-BROWSE                                          
                   PERFORM 2102-OBTAIN-GTEQ-HISTORY THRU 2102-EXIT              
               ELSE                                                             
                   PERFORM 2105-OBTAIN-GTEQ-LOOKUP  THRU 2105-EXIT              
               END-IF                                                           
               IF  MLCT-RET-NOT-FOUND AND MLCT-LOOKUP-DEFAULT                   
                   PERFORM 2110-DYNAMIC-DEFAULT THRU 2110-EXIT                  
               END-IF                                                           
               PERFORM 2120-SAVE-DYNAMIC-RESULT THRU 2120-EXIT                  
           END-IF.                                                              
                                                                                
           IF  MLCT-RET-NOT-FOUND                                               
               GO TO 2100-EXIT.                                                 
                                                                                
           SUBTRACT MLCT-HEADER-LENGTH FROM MLCTGCIO-DATA-LENGTH                
                                     GIVING MLCT-OUTPUT-LENGTH.                 
                                                                                
           IF  MLCT-OUTPUT-LENGTH > GGLU-MAX-DATA-LENGTH                        
               IF  MLCT-DEFAULT                                                 
                   SET MLCT-TRUNC-DEFAULT TO TRUE                               
               ELSE                                                             
                   SET MLCT-TRUNCATED    TO TRUE                                
               END-IF                                                           
               MOVE GGLU-MAX-DATA-LENGTH TO MLCT-OUTPUT-LENGTH                  
           END-IF.                                                              
                                                                                
           MOVE MLCTGCIO-DATA-RECORD (WS-RESULT-DATA-OFFSET:                    
                                       MLCT-OUTPUT-LENGTH)                      
             TO MLCTGGLU-CONTENT    (1:MLCT-OUTPUT-LENGTH).                     
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *  START A BROWSE FOR CONTENT THAT IS IN EFFECT FOR THE DATES             
      *  SPECIFIED IN THE LOOKUP REQUEST (HISTORY OPTION = BROWSE)              
      ****************************************************************          
       2102-OBTAIN-GTEQ-HISTORY.                                                
                                                                                
           SET GCIO-OBTAIN-FIRST        TO TRUE.                                
           PERFORM 7000-CALL-MLCTGCIO THRU 7000-EXIT.                           
                                                                                
           IF  GCIO-OK                                                          
               SET HIST-BROWSE-START    TO TRUE                                 
               MOVE CONTENT-KEY-EFF-DATE-COMP   TO WS-SAVE-EFF-DATE-COMP        
               PERFORM 2103-ENUMERATE-HISTORY THRU 2103-EXIT                    
                   UNTIL HIST-BROWSE-END                                        
                      OR NOT GCIO-OK                                            
           END-IF.                                                              
                                                                                
           PERFORM 2104-OBTAIN-GCIO-STATUS THRU 2104-EXIT.                      
                                                                                
           SET GCIO-OBTAIN-FINISH       TO TRUE.                                
           PERFORM 7000-CALL-MLCTGCIO THRU 7000-EXIT.                           
           MOVE MLCTGCIO-KEY-IN         TO MLCTGCIO-KEY-OUT.                    
                                                                                
       2102-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *  CHECK IF WE HAVE FOUND THE CORRECT HISTORY SEGMENT                     
      *  SPECIFIED IN THE LOOKUP REQUEST (HISTORY OPTION = CURRENT)             
      ****************************************************************          
       2103-ENUMERATE-HISTORY.                                                  
                                                                                
           IF CONTENT-KEY-EFF-DATE-COMP  >= WS-SAVE-EFF-DATE-COMP AND           
              MLCTGCIO-KEY-IN  (1:LENGTH OF CONTENT-LOOKUP-KEY) =               
              MLCTGCIO-KEY-OUT (1:LENGTH OF CONTENT-LOOKUP-KEY)                 
               MOVE MLCTGCIO-KEY-OUT    TO MLCTGCIO-KEY-IN                      
               IF  CONTENT-KEY-PROC-DATE-COMP >= WS-CURR-PROC-DATE-COMP         
                   SET HIST-BROWSE-END  TO TRUE                                 
               ELSE                                                             
                   SET GCIO-OBTAIN-NEXT TO TRUE                                 
                   PERFORM 7000-CALL-MLCTGCIO THRU 7000-EXIT                    
               END-IF                                                           
           ELSE                                                                 
               SET GCIO-NOT-FOUND       TO TRUE                                 
           END-IF.                                                              
                                                                                
       2103-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *  SET THE RETURN STATUS FOR ANY I/O PROCESSING.                          
      ****************************************************************          
       2104-OBTAIN-GCIO-STATUS.                                                 
                                                                                
           EVALUATE TRUE                                                        
           WHEN  GCIO-OK                                                        
               PERFORM 2107-CHECK-TERM-DATE THRU 2107-EXIT                      
           WHEN  GCIO-NOT-FOUND                                                 
               SET MLCT-ENTRY-NOT-FOUND TO TRUE                                 
           WHEN OTHER                                                           
               SET  MLCT-IO-ERROR             TO TRUE                           
               MOVE 'MLCTGCIO'                TO MLCT-ERR-PGM-ID                
               MOVE MLCTGCIO-ERROR-STATUS     TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTGGLU - 2104'         TO MLCT-ERR-PGM-LVL               
               STRING 'LOOKUP I/O ERR: NAME='  DELIMITED BY SIZE                
                       MLCT-CONTENT-NAME       DELIMITED BY SIZE                
                       ',RC='                  DELIMITED BY SIZE                
                       MLCTGCIO-RETURN-CODE    DELIMITED BY SIZE                
                       ',VB='                  DELIMITED BY SIZE                
                       MLCTGCIO-VERB           DELIMITED BY SIZE                
                 INTO  MLCT-ERR-PGM-DESC                                        
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
           END-EVALUATE.                                                        
       2104-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *  START A BROWSE FOR CONTENT THAT IS IN EFFECT FOR THE DATES             
      *  SPECIFIED IN THE LOOKUP REQUEST (HISTORY OPTION = CURRENT)             
      ****************************************************************          
       2105-OBTAIN-GTEQ-LOOKUP.                                                 
                                                                                
           SET  GCIO-OBTAIN-GTEQ      TO TRUE.                                  
           PERFORM 7000-CALL-MLCTGCIO THRU 7000-EXIT.                           
                                                                                
           IF  GCIO-OK                                                          
               IF MLCTGCIO-KEY-IN  (1:LENGTH OF CONTENT-LOOKUP-KEY) =           
                  MLCTGCIO-KEY-OUT (1:LENGTH OF CONTENT-LOOKUP-KEY)             
                   CONTINUE                                                     
               ELSE                                                             
                   SET GCIO-NOT-FOUND TO TRUE                                   
               END-IF                                                           
           END-IF.                                                              
                                                                                
           PERFORM 2104-OBTAIN-GCIO-STATUS THRU 2104-EXIT.                      
                                                                                
       2105-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *  PROCESS A KEYED READ FOR CONTENT THAT IS IN EFFECT FOR THE             
      *  DATES SPECIFIED IN THE LOOKUP REQUEST.                                 
      ****************************************************************          
       2106-OBTAIN-KEYED-LOOKUP.                                                
                                                                                
           SET  GCIO-OBTAIN-KEYED     TO TRUE.                                  
           PERFORM 7000-CALL-MLCTGCIO THRU 7000-EXIT.                           
                                                                                
           PERFORM 2104-OBTAIN-GCIO-STATUS THRU 2104-EXIT.                      
                                                                                
       2106-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *  COMPARE THE EFFECTIVE DATE OF REQUEST TO TERMINATION DATE OF           
      *  THE CONTENT LOCATED.  IF IT IS TERMINATED, THIS CONTENT IS NO          
      *  LONGER VALID AND CONSIDERED NOT FOUND.                                 
      ****************************************************************          
       2107-CHECK-TERM-DATE.                                                    
           IF MLCT-CONTENT-EFF-DATE > CONTENT-TERM-DATE                         
              SET MLCT-ENTRY-TERMINATED TO TRUE                                 
           END-IF.                                                              
                                                                                
       2107-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
       2110-DYNAMIC-DEFAULT.                                                    
                                                                                
           SET  MLCT-OK              TO TRUE.                                   
           MOVE WS-CURR-SEARCH-KEY   TO CONTENT-KEY.                            
           MOVE HIGH-VALUES          TO CONTENT-KEY-DATA.                       
                                                                                
           MOVE CONTENT-KEY          TO MLCTGCIO-KEY-IN.                        
                                                                                
           IF  MLCT-HISTORY-BROWSE                                              
               PERFORM 2102-OBTAIN-GTEQ-HISTORY THRU 2102-EXIT                  
           ELSE                                                                 
               PERFORM 2105-OBTAIN-GTEQ-LOOKUP  THRU 2105-EXIT                  
           END-IF                                                               
                                                                                
           IF  MLCT-RET-NOT-FOUND                                               
               CONTINUE                                                         
           ELSE                                                                 
               SET  MLCT-DEFAULT     TO TRUE                                    
           END-IF.                                                              
                                                                                
       2110-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       2120-SAVE-DYNAMIC-RESULT.                                                
           MOVE  WS-SAVE-SEARCH-KEY                                             
                 TO LURT-LAST-SEARCH-KEY   (WS-CURR-TABLE).                     
           MOVE ZERO                                                            
                 TO LURT-LAST-SEARCH-ENTRY (WS-CURR-TABLE).                     
                                                                                
           IF MLCT-RET-NOT-FOUND AND NOT MLCT-ENTRY-TERMINATED                  
              MOVE SPACES TO  LURT-LAST-SEARCH-RESULT (WS-CURR-TABLE)           
           ELSE                                                                 
              MOVE MLCTGCIO-KEY-OUT                                             
                          TO  LURT-LAST-SEARCH-RESULT (WS-CURR-TABLE)           
           END-IF.                                                              
                                                                                
       2120-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ******************************************************************        
      *    SEARCH THE WORKING STORAGE TABLE. SWV-ENTRY IS RETURNED     *        
      *    AS EITHER THE ENTRY WITH AN EXACT MATCH OR ONE OF           *        
      *    THE CLOSEST.  IF NO MATCH FOUND, SEARCH FOR THE DEFAULT     *        
      *    BEFORE RETURNING A NOT FOUND CONDITION.  IN THIS CASE,      *        
      *    SWV-ENTRY WILL BE ASSOCIATED WITH THE DEFAULT VALUE.                 
      ******************************************************************        
                                                                                
       2200-CACHED-REQUEST.                                                     
                                                                                
           IF  WS-SAVE-SEARCH-KEY = LURT-LAST-SEARCH-KEY (WS-CURR-TABLE)        
               MOVE LURT-LAST-SEARCH-ENTRY (WS-CURR-TABLE)                      
                                  TO SWV-ENTRY                                  
               IF SWV-ENTRY = ZERO                                              
                  IF LURT-NUM-ENTRIES (WS-CURR-TABLE) > 0                       
                     SET MLCT-ENTRY-NOT-FOUND TO TRUE                           
                  ELSE                                                          
                     SET MLCT-TABLE-NOT-FOUND TO TRUE                           
                  END-IF                                                        
               ELSE                                                             
                  MOVE LURT-LAST-SEARCH-RESULT (WS-CURR-TABLE)                  
                                      TO CONTENT-KEY                            
                  IF CONTENT-KEY-DATA = HIGH-VALUES                             
                     SET MLCT-DEFAULT TO TRUE                                   
                  END-IF                                                        
                  PERFORM 2217-CHECK-TERM-ENTRY  THRU 2217-EXIT                 
               END-IF                                                           
           ELSE                                                                 
               MOVE ZERO          TO SWV-ENTRY                                  
               MOVE LURT-FIRST-ENTRY (WS-CURR-TABLE)                            
                                  TO SWV-LOW-ADDRESS                            
               MOVE LURT-LAST-ENTRY  (WS-CURR-TABLE)                            
                                  TO SWV-HIGH-ADDRESS                           
                                                                                
               EVALUATE TRUE                                                    
               WHEN  LURT-NUM-ENTRIES (WS-CURR-TABLE) > 30                      
                   PERFORM 2210-BINARY-SEARCH  THRU 2210-EXIT                   
               WHEN  LURT-NUM-ENTRIES (WS-CURR-TABLE) > 0                       
                   PERFORM 2220-SERIAL-SEARCH  THRU 2220-EXIT                   
               WHEN  OTHER                                                      
                   SET MLCT-TABLE-NOT-FOUND TO TRUE                             
               END-EVALUATE                                                     
                                                                                
               PERFORM 2230-SAVE-SEARCH-RESULT THRU 2230-EXIT                   
           END-IF.                                                              
                                                                                
           IF  MLCT-RET-NOT-FOUND                                               
               GO TO 2200-EXIT.                                                 
                                                                                
           SUBTRACT MLCT-HEADER-LENGTH                                          
               FROM LUSI-SEARCH-RESULT-SIZE (SWV-ENTRY)                         
                                     GIVING MLCT-OUTPUT-LENGTH.                 
                                                                                
           IF  MLCT-OUTPUT-LENGTH > GGLU-MAX-DATA-LENGTH                        
               IF  MLCT-DEFAULT                                                 
                   SET MLCT-TRUNC-DEFAULT TO TRUE                               
               ELSE                                                             
                   SET MLCT-TRUNCATED    TO TRUE                                
               END-IF                                                           
               MOVE GGLU-MAX-DATA-LENGTH TO MLCT-OUTPUT-LENGTH                  
           END-IF.                                                              
                                                                                
           ADD  MLCT-HEADER-LENGTH TO                                           
                LUSI-SEARCH-RESULT-OFFSET (SWV-ENTRY)                           
                                     GIVING LUSR-DATA-OFFSET.                   
                                                                                
           MOVE LUSR-DATA (LUSR-DATA-OFFSET:MLCT-OUTPUT-LENGTH)                 
             TO MLCTGGLU-CONTENT         (1:MLCT-OUTPUT-LENGTH).                
                                                                                
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      *    DO A BINARY SEARCH ON THE TABLE. SWV-ENTRY IS RETURNED      *        
      *    AS EITHER THE ENTRY WITH AN EXACT MATCH OR ONE OF           *        
      *    THE CLOSEST.  IF NO MATCH FOUND, SEARCH FOR THE DEFAULT     *        
      *    BEFORE RETURNING A NOT FOUND CONDITION.                     *        
      ******************************************************************        
       2210-BINARY-SEARCH.                                                      
                                                                                
           SET SWV-BEGIN-SEARCH                TO TRUE.                         
           PERFORM 2211-SET-CURRENT-RANGE    THRU 2211-EXIT.                    
           PERFORM 2212-SEARCH-CURRENT-RANGE THRU 2212-EXIT                     
             UNTIL SUB       < 1                                                
                OR SWV-END-SEARCH.                                              
                                                                                
      *                                                                         
      * WE ARE NOW POSITIONED TO THE NEAREST VALUE WITHIN THE TABLE.            
      * WE MUST CONTINUE A SERIAL SEARCH FORWARD TO PICK UP THE CURRENT         
      * ENTRY FOR THE REQUIRED DATE RANGE, IF THE BINARY SEARCH DID NOT         
      * GIVE AN EXACT MATCH.                                                    
      *                                                                         
           IF  SWV-END-SEARCH                                                   
               MOVE  SWV-HIGH-ADDRESS           TO SWV-ENTRY                    
               PERFORM 2217-CHECK-TERM-ENTRY  THRU 2217-EXIT                    
           ELSE                                                                 
               MOVE  SWV-HIGH-ADDRESS           TO SUB                          
               MOVE  LUSI-SEARCH-EFF-COMP (SUB) TO WS-SAVE-EFF-DATE-COMP        
               PERFORM 2213-F-LOCATE-ENTRY-IN-EFFECT THRU 2213-EXIT             
                 UNTIL SUB       = ZERO                                         
                    OR SUB       > LURT-LAST-ENTRY (WS-CURR-TABLE)              
                    OR SWV-ENTRY > ZERO                                         
           END-IF.                                                              
                                                                                
           IF  SWV-ENTRY = ZERO OR MLCT-ENTRY-TERMINATED                        
               MOVE LURT-DEFAULT-ENTRY (WS-CURR-TABLE)                          
                                 TO  SWV-LOW-ADDRESS                            
               MOVE LURT-LAST-ENTRY  (WS-CURR-TABLE)                            
                                 TO  SWV-HIGH-ADDRESS                           
               PERFORM 2215-SERIAL-DEFAULT THRU 2215-EXIT                       
           END-IF.                                                              
                                                                                
       2210-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       2211-SET-CURRENT-RANGE.                                                  
           SUBTRACT SWV-LOW-ADDRESS    FROM SWV-HIGH-ADDRESS                    
                                     GIVING SUB.                                
       2211-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      * DO A BINARY SEARCH ON FULL KEY TO OBTAIN CLOSEST MATCH FOR              
      * LOOKUP VALUE REQUESTED. END SEARCH WHEN MATCH FOUND AND RETURN          
      * LOCATION AS SWV-HIGH-ADDRESS.                                           
      ****************************************************************          
       2212-SEARCH-CURRENT-RANGE.                                               
                                                                                
           DIVIDE 2                       INTO SUB.                             
           ADD SWV-LOW-ADDRESS              TO SUB.                             
                                                                                
           IF  LUSI-SEARCH-KEY (SUB) > WS-CURR-SEARCH-KEY                       
               MOVE SUB                     TO SWV-HIGH-ADDRESS                 
               PERFORM 2211-SET-CURRENT-RANGE THRU 2211-EXIT                    
               GO TO 2212-EXIT.                                                 
                                                                                
           IF  LUSI-SEARCH-KEY (SUB) < WS-CURR-SEARCH-KEY                       
               ADD   1    TO SUB        GIVING SWV-LOW-ADDRESS                  
               PERFORM 2211-SET-CURRENT-RANGE THRU 2211-EXIT                    
               GO TO 2212-EXIT.                                                 
                                                                                
           MOVE SUB                         TO SWV-HIGH-ADDRESS.                
           SET  SWV-END-SEARCH              TO TRUE.                            
                                                                                
       2212-EXIT.                                                               
           EXIT.                                                                
                                                                                
      ****************************************************************          
      * FIND FORWARD THE FIRST ENTRY THAT MATCHES THE SEARCH REQUEST *          
      * BASED ON ARGUMENT, EFFECTIVE DATE, AND PROCESS DATE.         *          
      ****************************************************************          
       2213-F-LOCATE-ENTRY-IN-EFFECT.                                           
                                                                                
           IF LUSI-SEARCH-EFF-COMP  (SUB) >= WS-SAVE-EFF-DATE-COMP  AND         
              LUSI-SEARCH-KEY (SUB) (1:LENGTH OF CONTENT-LOOKUP-KEY) =          
              WS-CURR-SEARCH-KEY    (1:LENGTH OF CONTENT-LOOKUP-KEY)            
              IF LUSI-SEARCH-PROC-COMP (SUB) >= WS-CURR-PROC-DATE-COMP          
                  MOVE   SUB   TO SWV-ENTRY                                     
                  PERFORM 2217-CHECK-TERM-ENTRY THRU 2217-EXIT                  
              ELSE                                                              
                  ADD  1    TO  SUB                                             
              END-IF                                                            
           ELSE                                                                 
              MOVE  ZERO    TO  SUB                                             
           END-IF.                                                              
                                                                                
       2213-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      * FIND FORWARD THE FIRST ENTRY WHERE FULL KEY GREATER OR EQUAL TO         
      * CURRENT SEARCH KEY (DEFAULT DATA = HIGH VALUES). IF A DEFAULT           
      * VALUE EXISTS, IT WAS FLAGGED WHEN DURING TABLE SETUP.  DON'T            
      * BOTHER LOOKING UP A DEFAULT IF IT DOES NOT EXIST.                       
      ****************************************************************          
       2215-SERIAL-DEFAULT.                                                     
           IF  LURT-DEFAULT-ENTRY (WS-CURR-TABLE) <= ZERO                       
               IF  MLCT-ENTRY-TERMINATED                                        
                   CONTINUE                                                     
               ELSE                                                             
                   SET  MLCT-ENTRY-NOT-FOUND   TO TRUE                          
               END-IF                                                           
               GO TO 2215-EXIT                                                  
           END-IF.                                                              
                                                                                
           MOVE WS-CURR-SEARCH-KEY   TO CONTENT-KEY.                            
           MOVE HIGH-VALUES          TO CONTENT-KEY-DATA.                       
                                                                                
           MOVE CONTENT-KEY          TO WS-CURR-SEARCH-KEY.                     
           MOVE LURT-DEFAULT-ENTRY (WS-CURR-TABLE)  TO SUB.                     
                                                                                
      *                                                                         
      * FIND THE POSITION OF THE NEAREST DEFAULT ENTRY FOR THE                  
      * SPECIFIED CONTENT NAME AND EFFECTIVE DATE                               
      *                                                                         
           PERFORM                                                              
                UNTIL SUB > SWV-HIGH-ADDRESS                                    
                   OR LUSI-SEARCH-KEY (SUB) >= WS-CURR-SEARCH-KEY               
                                                                                
              ADD +1 TO SUB                                                     
           END-PERFORM.                                                         
                                                                                
      *                                                                         
      * FIND THE POSITION OF THE DEFAULT ENTRY FOR THE SPECIFIED                
      * CONTENT NAME, EFFECTIVE DATE AND PROCESS DATE.                          
      *                                                                         
           SET MLCT-ENTRY-NOT-FOUND  TO TRUE.                                   
           IF SUB > SWV-HIGH-ADDRESS                                            
              CONTINUE                                                          
           ELSE                                                                 
              MOVE  ZERO                       TO SWV-ENTRY                     
              MOVE  LUSI-SEARCH-EFF-COMP (SUB) TO WS-SAVE-EFF-DATE-COMP         
              PERFORM 2213-F-LOCATE-ENTRY-IN-EFFECT THRU 2213-EXIT              
                 UNTIL SUB       = ZERO                                         
                    OR SUB       > SWV-HIGH-ADDRESS                             
                    OR SWV-ENTRY > ZERO                                         
                                                                                
              IF  SWV-ENTRY > ZERO                                              
                  IF  MLCT-ENTRY-TERMINATED                                     
                      CONTINUE                                                  
                  ELSE                                                          
                      SET MLCT-DEFAULT     TO TRUE                              
                  END-IF                                                        
              END-IF                                                            
           END-IF.                                                              
       2215-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *  COMPARE THE EFFECTIVE DATE OF REQUEST TO TERMINATION DATE OF           
      *  THE CONTENT LOCATED AT SWV-ENTRY.  IF IT IS TERMINATED, THIS           
      *  CONTENT IS NO LONGER VALID AND CONSIDERED NOT FOUND.                   
      ****************************************************************          
       2217-CHECK-TERM-ENTRY.                                                   
           IF MLCT-CONTENT-EFF-DATE >                                           
                               LUSI-SEARCH-RESULT-TERMDT (SWV-ENTRY)            
              SET MLCT-ENTRY-TERMINATED TO TRUE                                 
           END-IF.                                                              
       2217-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *  DO A SERIAL SEARCH ON THE TABLE. SWV-ENTRY IS RETURNED AS   *          
      *  AS EITHER THE ENTRY WITH AN EXACT MATCH OR THE ENTRY IN     *          
      *  EFFECT FOR THE SPECIFIED DATE RANGE.                        *          
      ****************************************************************          
       2220-SERIAL-SEARCH.                                                      
                                                                                
           MOVE SWV-LOW-ADDRESS TO SUB.                                         
                                                                                
      *                                                                         
      * FIND THE POSITION OF THE NEAREST ENTRY FOR THIS REQUEST                 
      * THAT WILL MATCH THE SPECIFIED ARGUMENT AND EFFECTIVE DATE               
      *                                                                         
           PERFORM                                                              
                UNTIL SUB > SWV-HIGH-ADDRESS                                    
                   OR LUSI-SEARCH-KEY (SUB) >= WS-CURR-SEARCH-KEY               
                                                                                
              ADD +1 TO SUB                                                     
           END-PERFORM.                                                         
                                                                                
      *                                                                         
      * FIND THE EXACT ENTRY FOR THIS REQUEST THAT WILL MATCH THE               
      * SPECIFIED ARGUMENT, EFFECTIVE DATE AND PROCESS DATE                     
      *                                                                         
      * NOTE: IF A DEFAULT VALUE EXISTS, THEN THIS SCAN WILL STOP               
      *       BEFORE THE END OF THE TABLE.                                      
      *                                                                         
           IF SUB > SWV-HIGH-ADDRESS                                            
              SET MLCT-ENTRY-NOT-FOUND TO TRUE                                  
           ELSE                                                                 
              MOVE  LUSI-SEARCH-EFF-COMP (SUB) TO WS-SAVE-EFF-DATE-COMP         
              PERFORM 2213-F-LOCATE-ENTRY-IN-EFFECT THRU 2213-EXIT              
                 UNTIL SUB       = ZERO                                         
                    OR SUB       > SWV-HIGH-ADDRESS                             
                    OR SWV-ENTRY > ZERO                                         
                                                                                
              IF  SWV-ENTRY = ZERO OR  MLCT-ENTRY-TERMINATED                    
                  PERFORM 2215-SERIAL-DEFAULT THRU 2215-EXIT                    
              END-IF                                                            
           END-IF.                                                              
                                                                                
       2220-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
       2230-SAVE-SEARCH-RESULT.                                                 
           MOVE  WS-SAVE-SEARCH-KEY                                             
                 TO LURT-LAST-SEARCH-KEY (WS-CURR-TABLE).                       
                                                                                
           IF MLCT-RET-NOT-FOUND AND NOT MLCT-ENTRY-TERMINATED                  
              MOVE SPACES TO  LURT-LAST-SEARCH-RESULT (WS-CURR-TABLE)           
              MOVE ZERO   TO  LURT-LAST-SEARCH-ENTRY  (WS-CURR-TABLE)           
           ELSE                                                                 
              MOVE LUSI-SEARCH-KEY (SWV-ENTRY)                                  
                          TO  LURT-LAST-SEARCH-RESULT (WS-CURR-TABLE)           
              MOVE SWV-ENTRY                                                    
                          TO  LURT-LAST-SEARCH-ENTRY  (WS-CURR-TABLE)           
           END-IF.                                                              
                                                                                
       2230-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CALL APPROPRIATE MODULE FOR PROCESSING THIS REQUEST                  
      ****************************************************************          
       3000-FINALIZATION.                                                       
                                                                                
           MOVE MLCT-RETURN-STATUS       TO GGLU-RETURN-STATUS.                 
           MOVE MLCT-OUTPUT-LENGTH       TO GGLU-OUTPUT-LENGTH.                 
           MOVE MLCT-ERR-PGM-ID          TO GGLU-ERR-PGM-ID.                    
           MOVE MLCT-ERR-PGM-STATUS      TO GGLU-ERR-PGM-STATUS.                
           MOVE MLCT-ERR-PGM-LVL         TO GGLU-ERR-PGM-LVL.                   
           MOVE MLCT-ERR-PGM-DESC        TO GGLU-ERR-PGM-DESC.                  
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
       5000-DISPLAY-CURRENT-STATUS.                                             
           DISPLAY ' *********************************** '                      
           DISPLAY ' MLCTGGLU - INTERNAL REGISTRY STATUS '                      
           DISPLAY ' *********************************** '                      
           DISPLAY ' '.                                                         
           MOVE LURT-COUNT  TO WS-NUMERIC-DISPLAY.                              
           DISPLAY ' TOTAL NUMBER OF TABLES:  ' WS-NUMERIC-DISPLAY.             
           MOVE LUSI-COUNT  TO WS-NUMERIC-DISPLAY.                              
           DISPLAY ' TOTAL NUMBER OF ENTRIES: ' WS-NUMERIC-DISPLAY.             
           MOVE LUSR-SIZE   TO WS-NUMERIC-DISPLAY.                              
           DISPLAY ' TOTAL BYTES OF STORAGE:  ' WS-NUMERIC-DISPLAY.             
           DISPLAY ' '.                                                         
           MOVE +1 TO SUB.                                                      
           DISPLAY WS-STATUS-TITLE.                                             
           PERFORM UNTIL SUB > LURT-COUNT                                       
                      OR SUB > WS-MAX-LOOKUP-COUNT                              
               MOVE LURT-LOOKUP-NAME (SUB) TO WS-LOOKUP-NAME                    
               IF  LURT-DYNAMIC-LOOKUP (SUB)                                    
                   MOVE 'DYNAMIC' TO WS-LOOKUP-ALGORITHM                        
               ELSE                                                             
                   MOVE 'CACHED'  TO WS-LOOKUP-ALGORITHM                        
               END-IF                                                           
               MOVE LURT-NUM-ENTRIES (SUB) TO WS-NUM-ENTRIES                    
               MOVE LURT-NUM-HITS    (SUB) TO WS-NUM-HITS                       
               DISPLAY WS-STATUS-DETAIL                                         
               ADD +1  TO SUB                                                   
           END-PERFORM.                                                         
           GOBACK.                                                              
                                                                                
       5000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
       6000-SET-CURRENT-CAPACITY.                                               
           MOVE LENGTH OF LUSR-DATA TO LUSR-CAPACITY.                           
       6000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      ****************************************************************          
       7000-CALL-MLCTGCIO.                                                      
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *          CALL MLCTGCIO USING MLCTGCIO-PROTOCOL                          
      *                              MLCTGCIO-DATA-RECORD.                      
      *                                                                         
           COPY MLCTCCIO.                                                       
       7000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       9000-RAISE-EXCEPTION.                                                    
                                                                                
           MOVE MLCT-ERROR-STATUS        TO GGLU-RETURN-STATUS.                 
           MOVE ZERO                     TO GGLU-OUTPUT-LENGTH.                 
           MOVE MLCT-ERR-PGM-ID          TO GGLU-ERR-PGM-ID.                    
           MOVE MLCT-ERR-PGM-STATUS      TO GGLU-ERR-PGM-STATUS.                
           MOVE MLCT-ERR-PGM-LVL         TO GGLU-ERR-PGM-LVL.                   
           MOVE MLCT-ERR-PGM-DESC        TO GGLU-ERR-PGM-DESC.                  
                                                                                
           GO TO 0000-MAINLINE-EXIT.                                            
                                                                                
       9000-EXIT.                                                               
