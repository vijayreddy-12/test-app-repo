       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRMI.                                                 
      *AUTHOR.        KLYN.                                                     
      *DATE-WRITTEN.  NOV 6, 2000.                                              
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS)                                                      
      *   GCCPFRMI - POPULATE SUBMISSION DATA TABLE AND FORM TABLE              
      *              FROM EXTRACT FILE                                          
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *   THE PROGRAM WILL GET CURRENT DATE.                                    
      *   GET MAX KEY FROM TMXKEY TABLE AND INCREMENT BY ONE, TO                
      *   UNIQUELY IDENTIFY THIS REPORT RUN AND EACH FORM CODE.                 
      *   THE INCREMENTED KEY IS WRITTEN BACK TO THE DATABASE AT                
      *   COMPLETION OF THIS PROGRAM. DURING THIS TIME THE KEY MAY              
      *   HAVE BEEN INCREMENTED 1-20 TIMES.                                     
      *   TWO TABLES ARE POPULATED FROM THE INPUT DATA.                         
      *   (1)  TFORM - WHICH CONTAINS FORM-ID AND SQUENCE NUMBER AND            
      *        THE ACTUAL FORM-DATA-STRING.                                     
      *        THIS DATABASE MUST BE UPDATED FIRST DUE TO KEY                   
      *        CONSIDERATIONS.                                                  
      *   (2)  TSD   - WHICH CONTAINS HEADER AND TIMESTAMP INFORMATION          
      *                                                                         
      *   ONE TABLE  IS UPDATED WITH INCREMENTED MAX_KEY_VALUE                  
      *   (1)  TMXKEY- WHICH CONTAINS KEY TYPE AND MAXKEY VALUE                 
      *        USED TO UNIQUELY IDENTIFY PRINT DATA.                            
      *                                                                         
      *   INPUT FILES:  FORMATTED AFP FORM LAYOUTS                              
      *                                                                         
      *   OUTPUT FILES: NONE                                                    
      *                                                                         
      *   DB2 TABLES                                                            
      *   ACCESSED:     TMXKEY  - UNIQUE KEY TABLE     - SELECT/UPDATE          
      *                 TFORM   - FORMS TABLE          - INSERT                 
      *                 TSD     - FORM HEADER TABLE    - INSERT                 
      *                                                                         
      *   CALLS:        GAEDATSR- FILE I/O                                      
      *                                                                         
      *   INCLUDE CODE: SQLCA    - SQL COMMUNICATION AREA                       
      *                                                                         
      *                 TMXKEYD  - TMXKEY  TABLE DECLARATION                    
      *                 TFORMD   - TFORM   TABLE DECLARATION                    
      *                 TSDD     - TSD     TABLE DECLARATION                    
      *                                                                         
      *                 TMXKEY   - TMXKEY  HOST VARIABLES                       
      *                 TFORM    - TFORM   HOST VARIABLES                       
      *                 TSD      - TSD     HOST VARIABLES                       
      *                                                                         
      *                 TMXKEYI  - TMXKEY  INDICATOR VARIABLES                  
      *                 TFORMI   - TFORM   INDICATOR VARIABLES                  
      *                 TSDI     - TSD     INDICATOR VARIABLES                  
      *                                                                         
      *                                                                         
      *                 ZDMSCNTL - CONTROL DATE RECORD LAYOUT                   
      *                 GL       - FORM DETAIL LAYOUT                           
      *                                                                         
      *                 ICBM     - INTERFACE CONTROL BLOCK (DATA SERVER)        
      *                                                                         
      *****************************************************************         
      *****************************************************************         
      *   MODIFICATION LOG                                                      
      ******************************************************************        
      * PROGRAMMER   º  DATE  º             CHANGE                              
      *    NAME      ºDD/MM/YYº           DESCRIPTION                           
      *--------------+--------+-----------------------------------------        
      * KLYN         º06/11/00º ORIGINAL CODE                                   
      *--------------+--------+-----------------------------------------        
      * IBM GR       |29/08/08| UPGRADED IN ECU PROJECT                         
      *--------------+--------+-----------------------------------------        
                                                                                
      /                                                                         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-370-165.                                            
       OBJECT-COMPUTER. IBM-370-165.                                            
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *****************************************************************         
      *** DB2 INCLUDES                                                          
      *****************************************************************         
                                                                                
      *** SQL COMMUNICATION AREA                                                
                                                                                
           EXEC SQL INCLUDE SQLCA   END-EXEC.                                   
                                                                                
      *** DB2 TABLE DECLARATIONS                                                
                                                                                
           EXEC SQL INCLUDE TMXKEYD END-EXEC.                                   
           EXEC SQL INCLUDE TFORMD  END-EXEC.                                   
           EXEC SQL INCLUDE TSDD    END-EXEC.                                   
                                                                                
      *** DB2 HOST & INDICATOR VARIABLES                                        
                                                                                
      *01  DCLMAXKEY.                                                           
           EXEC SQL INCLUDE TMXKEY  END-EXEC.                                   
      *    EXEC SQL INCLUDE TMXKEYI END-EXEC.                                   
                                                                                
      *01  DCLFORM.                                                             
           EXEC SQL INCLUDE TFORM   END-EXEC.                                   
      *    EXEC SQL INCLUDE TFORMI  END-EXEC.                                   
                                                                                
      *01  DCLTSD.                                                              
           EXEC SQL INCLUDE TSD     END-EXEC.                                   
           EXEC SQL INCLUDE TSDI    END-EXEC.                                   
                                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      *    GENERIC LAYOUT OF INPUT FILE                                         
      *----------------------------------------------------------------*        
       01 GCCCGNRK-RECORD.                                                      
           05  GCCCGNRK-CCOUNT.                                                 
               10  GCCCGNRK-RECL                   PIC S9(4)  COMP.             
               10  FILLER                          PIC XX.                      
          COPY GCCCGNRK.                                                        
                                                                                
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                          
                                                                                
      /                                                                         
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-VARIABLES.                                                        
           05  WSVAL                       PIC 9(10)    VALUE 0.                
           05  WS-KEY-TYPE                 PIC X(10)    VALUE 'FORM-ID'.        
           05  WS-SAVED-FORM-NBR           PIC X(8)     VALUE SPACES.           
           05  WS-HELD-CONF-NBR            PIC 9(11).                           
           05  WS-MAX-KEY-VALUE            PIC 9(11)    COMP-3                  
                                                        VALUE 0.                
           05  WS-FORM-SEQ                 PIC 999      VALUE 1.                
           05  WS-TIMESTAMP                PIC X(26)    VALUE SPACES.           
           05  WS-FORM-CHANGE-MKR          PIC X        VALUE 'N'.              
               88  WS-FORM-CHANGE                       VALUE 'Y'.              
           05  WS-INPUT-EOF-MKR            PIC X        VALUE 'N'.              
               88  WS-INPUT-EOF                         VALUE 'Y'.              
           05  WS-INPUT-OPEN-MKR           PIC X        VALUE 'N'.              
               88  WS-INPUT-OPEN                        VALUE 'Y'.              
           05  WS-OBTAIN-FIRST   PIC X(16) VALUE 'OBTAIN  FIRST   '.            
           05  WS-OBTAIN-NEXT    PIC X(16) VALUE 'OBTAIN  NEXT    '.            
                                                                                
                                                                                
                                                                                
                                                                                
           05  WS-WORK-DB-TIMESTAMP.                                            
               10  WS-WKTS-DB-YYYY         PIC 9(4).                            
               10  WS-WKTS-DB-HYP1         PIC X.                               
               10  WS-WKTS-DB-MNTH         PIC 99.                              
               10  WS-WKTS-DB-HYP2         PIC X.                               
               10  WS-WKTS-DB-DD           PIC 99.                              
               10  WS-WKTS-DB-HYP3         PIC X.                               
               10  WS-WKTS-DB-HH           PIC 99.                              
               10  WS-WKTS-DB-PUNKT1       PIC X.                               
               10  WS-WKTS-DB-MIN          PIC 99.                              
               10  WS-WKTS-DB-PUNKT2       PIC X.                               
               10  WS-WKTS-DB-SS           PIC 99.                              
               10  WS-WKTS-DB-PUNKT3       PIC X.                               
               10  WS-WKTS-DB-NNNNNN       PIC 9(6).                            
           05  WS-WORK-WEB-TIMESTAMP.                                           
               10  WS-WKTS-WEB-YYYY        PIC 9(4).                            
               10  WS-WKTS-WEB-MNTH        PIC 99.                              
               10  WS-WKTS-WEB-DD          PIC 99.                              
               10  WS-WKTS-WEB-HH          PIC 99.                              
               10  WS-WKTS-WEB-MIN         PIC 99.                              
               10  WS-WKTS-WEB-SS          PIC 99.                              
               10  WS-WKTS-WEB-NN          PIC 99.                              
                                                                                
      *                                                                         
      *****************************************************************         
      *** CONSTANTS                                                             
      *****************************************************************         
      *                                                                         
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GAEDATSR                 PIC X(08)                            
                                          VALUE 'GAEDATSR'.                     
           05  WS-GAEDATSR-VERB            PIC X(16).                           
           05  WS-INPUT-LR                 PIC X(16)                            
                                          VALUE 'CARD-DATA-070   '.             
      *----------------------------------------------------------------*        
      *    ACTION VERBS USED TO CALL GAEDATSR                                   
      *----------------------------------------------------------------*        
      /                                                                         
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME              PIC X(60) VALUE                      
                'GCCPFRMI - POPULATE FORM DATABASES'.                           
           10  AB-PARAGRAPH-NAME  OCCURS 25 TIMES                               
                                           PIC X(60).                           
           10  AB-MESSAGE.                                                      
               15  AB-MSG1                 PIC X(70).                           
               15  AB-MSG2                 PIC X(70).                           
           10  AB-SQLCODE                  PIC ----9.                           
           10  LVL                         PIC S9(4) COMP.                      
           10  CNT                         PIC S9(4) COMP.                      
                                                                                
      /                                                                         
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
      /                                                                         
      ****************************************************************          
      *********   P R O C E D U R E   D I V I S I O N   **************          
      ****************************************************************          
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           MOVE 1 TO LVL.                                                       
           MOVE '0000-MAINLINE' TO AB-PARAGRAPH-NAME (LVL).                     
                                                                                
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
                                                                                
           PERFORM 2000-PROCESS         THRU 2000-EXIT,                         
               UNTIL WS-INPUT-EOF.                                              
                                                                                
           PERFORM 3000-COMPLETION    THRU  3000-EXIT.                          
                                                                                
           GOBACK.                                                              
      /                                                                         
                                                                                
       1000-INITIALIZATION.                                                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE 'GCCPFRMI'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
      ******************************************************************        
      * GET MAXKEY RECORD                                                       
      ******************************************************************        
                                                                                
           PERFORM 6000-PROCESS-TMXKEY   THRU   6000-EXIT.                      
                                                                                
           INITIALIZE                   GCCCGNRK-RECORD.                        
                                                                                
      ******************************************************************        
      * READ FIRST INPUT RECORD                                                 
      ******************************************************************        
                                                                                
           MOVE WS-OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                       
           PERFORM 6100-READ-INPUT THRU                                         
                   6100-EXIT.                                                   
                                                                                
           MOVE 'Y'                  TO WS-INPUT-OPEN-MKR.                      
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-PROCESS.                                                            
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS' TO AB-PARAGRAPH-NAME (LVL).                      
                                                                                
           MOVE  GCCCGNRK-FORM-NBR   TO  WS-SAVED-FORM-NBR.                     
           MOVE  'N'                 TO  WS-FORM-CHANGE-MKR.                    
                                                                                
           PERFORM 2050-PROCESS-HANDLING  THRU 2050-EXIT                        
               UNTIL WS-FORM-CHANGE                                             
                OR   WS-INPUT-EOF.                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       2050-PROCESS-HANDLING.                                                   
           ADD 1 TO LVL.                                                        
           MOVE '2050-PROCESS' TO AB-PARAGRAPH-NAME (LVL).                      
                                                                                
      *                                                                         
      *    THE MAXKEY VALUE IS INCREMENTED BY 1 FOR NEW ENTRY                   
      *    IN THE TWO TABLES                                                    
      *                                                                         
                                                                                
           ADD  1                    TO  WS-MAX-KEY-VALUE.                      
                                                                                
                                                                                
           PERFORM 2100-POPULATE-TFORM   THRU 2100-EXIT.                        
                                                                                
           PERFORM 2200-POPULATE-TSD     THRU 2200-EXIT.                        
                                                                                
           MOVE GCCCGNRK-CONF-NBR    TO  WS-HELD-CONF-NBR.                      
                                                                                
                                                                                
           PERFORM 2300-GET-NEXT-INPUT   THRU 2300-EXIT.                        
                                                                                
           IF WS-SAVED-FORM-NBR  =  'GLMASS  '                                  
              PERFORM 2075-LOAD-MASS-BALANCE  THRU 2075-EXIT                    
                UNTIL  GCCCGNRK-CONF-NBR NOT =  WS-HELD-CONF-NBR                
                 OR    WS-FORM-CHANGE                                           
                 OR    WS-INPUT-EOF.                                            
                                                                                
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
       2050-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2075-LOAD-MASS-BALANCE.                                                  
           ADD 1 TO LVL.                                                        
           MOVE '2075-LOAD-MASS-BALANCE' TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
                                                                                
           PERFORM 2100-POPULATE-TFORM   THRU 2100-EXIT.                        
                                                                                
                                                                                
           PERFORM 2300-GET-NEXT-INPUT   THRU 2300-EXIT.                        
                                                                                
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
       2075-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       2100-POPULATE-TFORM.                                                     
      *****************************************************************         
      *    BUILD NEW TFORM LINE AND INSERT                                      
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2100-POPULATE-TFORM' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
                                                                                
           MOVE WS-MAX-KEY-VALUE   TO FORM-ID OF DCLTFORM                       
           IF GCCCGNRK-FORM-NBR  =  'GLMASS  '                                  
              MOVE GCCCGNRK-MASS-PAGE                                           
                                 TO FORM-SEQ-NUM         OF DCLTFORM            
              MOVE +3000         TO FORM-DATA-STRING-LEN OF DCLTFORM            
           ELSE                                                                 
              MOVE WS-FORM-SEQ   TO FORM-SEQ-NUM         OF DCLTFORM            
              MOVE +2000         TO FORM-DATA-STRING-LEN OF DCLTFORM.           
                                                                                
           MOVE GCCCGNRK-DETAIL  TO FORM-DATA-STRING-TEXT OF DCLTFORM.          
                                                                                
                                                                                
                                                                                
           MOVE WS-MAX-KEY-VALUE TO WSVAL.                                      
                                                                                
                                                                                
           EXEC SQL                                                             
             INSERT INTO TFORM                                                  
                    (FORM_ID                                                    
                    ,FORM_SEQ_NUM                                               
                    ,FORM_DATA_STRING                                           
                    )                                                           
             VALUES (:DCLTFORM.FORM-ID                                          
                    ,:DCLTFORM.FORM-SEQ-NUM                                     
                    ,:DCLTFORM.FORM-DATA-STRING                                 
                    )                                                           
           END-EXEC.                                                            
                                                                                
      *                                                                         
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       2200-POPULATE-TSD.                                                       
      *****************************************************************         
      *    BUILD NEW TSD LINE AND INSERT                                        
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2200-POPULATE-TSD' TO AB-PARAGRAPH-NAME (LVL).                 
                                                                                
                                                                                
                                                                                
           MOVE  GCCCGNRK-CONF-NBR       TO  CONFIRM-ID OF DCLTSD.              
           MOVE  GCCCGNRK-SEQ-NBR        TO  SEQ-NUM OF DCLTSD.                 
           MOVE  GCCCGNRK-FORM-NBR       TO  FORM-CD OF DCLTSD.                 
           MOVE  GCCCGNRK-LANG           TO  LANG-CD OF DCLTSD.                 
           MOVE  GCCCGNRK-CUST-GROUP-NBR TO  GROUP-ID OF DCLTSD.                
           MOVE  GCCCGNRK-CUST-DIV       TO  DIV-ID OF DCLTSD.                  
           MOVE  GCCCGNRK-CUST-CERT-NBR  TO  CERT-ID OF DCLTSD.                 
           MOVE  WS-WORK-DB-TIMESTAMP    TO  WEB-SENT-TS OF DCLTSD.             
           MOVE  WS-TIMESTAMP            TO  FORM-RECV-TS OF DCLTSD.            
           MOVE  WS-MAX-KEY-VALUE        TO  FORM-ID OF DCLTSD.                 
                                                                                
           IF GCCCGNRK-FORM-NBR  =  'GLMASS  '                                  
              MOVE  GCCCGNRK-MASS-PAGE   TO  FORM-SEQ-NUM OF DCLTSD             
           ELSE                                                                 
              MOVE  WS-FORM-SEQ          TO  FORM-SEQ-NUM OF DCLTSD.            
                                                                                
                                                                                
           MOVE WS-MAX-KEY-VALUE         TO WSVAL.                              
                                                                                
           EXEC SQL                                                             
             INSERT INTO TSD                                                    
                    (CONFIRM_ID                                                 
                    ,SEQ_NUM                                                    
                    ,FORM_CD                                                    
                    ,LANG_CD                                                    
                    ,GROUP_ID                                                   
                    ,DIV_ID                                                     
                    ,CERT_ID                                                    
                    ,WEB_SENT_TS                                                
                    ,FORM_RECV_TS                                               
                    ,SENT_TO_PRINT_TS                                           
                    ,PURGE_TS                                                   
                    ,FORM_ID                                                    
                    ,FORM_SEQ_NUM                                               
                    )                                                           
             VALUES (:DCLTSD.CONFIRM-ID                                         
                    ,:DCLTSD.SEQ-NUM                                            
                    ,:DCLTSD.FORM-CD                                            
                    ,:DCLTSD.LANG-CD                                            
                    ,:DCLTSD.GROUP-ID                                           
                    ,:DCLTSD.DIV-ID                                             
                    ,:DCLTSD.CERT-ID                                            
                    ,:DCLTSD.WEB-SENT-TS                                        
                    ,:DCLTSD.FORM-RECV-TS                                       
                    , NULL                                                      
                    , NULL                                                      
                    ,:DCLTSD.FORM-ID                                            
                    ,:DCLTSD.FORM-SEQ-NUM                                       
                    )                                                           
           END-EXEC.                                                            
                                                                                
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       2300-GET-NEXT-INPUT.                                                     
                                                                                
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2300-GET-NEXT-INPUT' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
                                                                                
           MOVE WS-OBTAIN-NEXT         TO WS-GAEDATSR-VERB.                     
           PERFORM  6100-READ-INPUT THRU 6100-EXIT.                             
                                                                                
           IF   GCCCGNRK-FORM-NBR  NOT =  WS-SAVED-FORM-NBR                     
                MOVE  'Y'              TO  WS-FORM-CHANGE-MKR.                  
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2300-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
       3000-COMPLETION.                                                         
      *****************************************************************         
      *  THIS PARAGRAPH...                                                      
      *    - UPDATES TMXKEY TABLE                                               
      *    - CLOSES INPUT FILE                                                  
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '3000-COMPLETION' TO AB-PARAGRAPH-NAME (LVL).                   
                                                                                
                                                                                
           PERFORM 6200-PUT-TMXKEY  THRU 6200-EXIT.                             
                                                                                
           PERFORM 6300-CLOSE-INPUT THRU 6300-EXIT.                             
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
                                                                                
      *                                                                         
      *    6000 SERIES OF PARAGRAPHS ARE CALLED FROM                            
      *    VARIOUS PARTS OF THE PROGRAM                                         
      *                                                                         
                                                                                
       6000-PROCESS-TMXKEY.                                                     
      *                                                                         
      *    GET MAX KEY VALUE, INCREMENT BY ONE AND REWRITE                      
      *                                                                         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6000-PROCESS-TMXKEY' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           INITIALIZE DCLTMXKEY.                                                
                                                                                
           EXEC SQL                                                             
             SELECT   KEY_TYP                                                   
                     ,MAX_KEY_VALUE                                             
                     ,CURRENT TIMESTAMP                                         
                INTO                                                            
                      :DCLTMXKEY.KEY-TYP                                        
                     ,:DCLTMXKEY.MAX-KEY-VALUE                                  
                     ,:WS-TIMESTAMP                                             
                FROM                                                            
                      TMXKEY                                                    
                WHERE                                                           
                      KEY_TYP         = :WS-KEY-TYPE                            
           END-EXEC.                                                            
                                                                                
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
                                                                                
           MOVE MAX-KEY-VALUE OF DCLTMXKEY TO  WS-MAX-KEY-VALUE.                
           MOVE WS-MAX-KEY-VALUE TO WSVAL.                                      
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       6000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6100-READ-INPUT.                                                         
      ******************************************************************        
      *    READ NEXT INPUT RECORD.                                     *        
      ******************************************************************        
                                                                                
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6100-READ-INPUT' TO AB-PARAGRAPH-NAME (LVL).                   
                                                                                
                                                                                
           MOVE WS-INPUT-LR            TO LOGICAL-RECORD-NAME.                  
           INITIALIZE GCCCGNRK-RECORD.                                          
                                                                                
           CALL WS-GAEDATSR       USING  WS-GAEDATSR-VERB                       
                                         GCCCGNRK-RECORD                        
                                         ICBM.                                  
                                                                                
           IF NOT LR-STATUS-OK                                                  
               MOVE 'Y'                TO  WS-INPUT-EOF-MKR                     
             ELSE                                                               
               PERFORM    6150-LOAD-TIMESTAMP THRU 6150-EXIT.                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       6100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6150-LOAD-TIMESTAMP.                                                     
      ******************************************************************        
      *    CONVERT WEB TIMESTAMP INPUT TO DB                           *        
      ******************************************************************        
                                                                                
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6150-LOAD-TIMESTAMP' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE GCCCGNRK-WEB-TIMESTAMP                                          
                                  TO  WS-WORK-WEB-TIMESTAMP.                    
                                                                                
           MOVE WS-WKTS-WEB-YYYY  TO  WS-WKTS-DB-YYYY.                          
           MOVE WS-WKTS-WEB-MNTH  TO  WS-WKTS-DB-MNTH.                          
           MOVE WS-WKTS-WEB-DD    TO  WS-WKTS-DB-DD.                            
           MOVE WS-WKTS-WEB-HH    TO  WS-WKTS-DB-HH.                            
           MOVE WS-WKTS-WEB-MIN   TO  WS-WKTS-DB-MIN.                           
           MOVE WS-WKTS-WEB-SS    TO  WS-WKTS-DB-SS.                            
           MOVE WS-WKTS-WEB-NN    TO  WS-WKTS-DB-NNNNNN.                        
                                                                                
           MOVE  '-'              TO  WS-WKTS-DB-HYP1                           
                                      WS-WKTS-DB-HYP2                           
                                      WS-WKTS-DB-HYP3.                          
                                                                                
           MOVE  '.'              TO  WS-WKTS-DB-PUNKT1                         
                                      WS-WKTS-DB-PUNKT2                         
                                      WS-WKTS-DB-PUNKT3.                        
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
       6150-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6200-PUT-TMXKEY.                                                         
      *****************************************************                     
      *    UPDATE TMXKEY WITH THE NEW MAX KEY VALUE                             
      *    AT END OF PROGRAM                                                    
      *****************************************************                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6200-PUT-TMXKEY' TO AB-PARAGRAPH-NAME (LVL).                   
                                                                                
           MOVE WS-MAX-KEY-VALUE  TO MAX-KEY-VALUE OF DCLTMXKEY.                
                                                                                
           EXEC SQL                                                             
               UPDATE TMXKEY                                                    
                 SET                                                            
                  KEY_TYP            = :DCLTMXKEY.KEY-TYP                       
                 ,MAX_KEY_VALUE      = :DCLTMXKEY.MAX-KEY-VALUE                 
               WHERE                                                            
                  KEY_TYP        = :WS-KEY-TYPE                                 
            END-EXEC                                                            
                                                                                
           PERFORM 8900-CHECK-SQL-CODE  THRU  8900-EXIT.                        
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       6200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6300-CLOSE-INPUT.                                                        
                                                                                
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6300-CLOSE-INPUT' TO AB-PARAGRAPH-NAME (LVL).                  
                                                                                
           MOVE WS-INPUT-LR            TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                                  LOGICAL-RECORD-NAME                           
                                  ICBM.                                         
                                                                                
           MOVE 'N'                  TO WS-INPUT-OPEN-MKR.                      
                                                                                
           IF NOT LR-STATUS-OK                                                  
               PERFORM    9999-ABEND THRU 9999-ABEND.                           
                                                                                
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       6300-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
      *****************************************************************         
      * THIS PARAGRAPH CHECKS THE SQL CODE AFTER A DB2 CALL AND HANDLES         
      * ANY ERRORS DETECTED.                                                    
      *****************************************************************         
                                                                                
       8900-CHECK-SQL-CODE.                                                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '8900-CHECK-SQL-CODE' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
                                                                                
           EVALUATE SQLCODE                                                     
                                                                                
              WHEN ZERO                                                         
                 CONTINUE                                                       
              WHEN OTHER                                                        
                 MOVE SQLCODE      TO AB-SQLCODE                                
                 MOVE SQLERRMC     TO AB-MSG2                                   
                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                        
                 PERFORM 9999-ABEND                                             
                                                                                
           END-EVALUATE.                                                        
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
       8900-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      *****************************************************************         
      * THIS PARAGRAPH IS CALLED IF AN EXCEPTIONAL CONDITION, WHICH             
      * CANNOT ALLOW THE PROGRAM TO CONTINUE NORMALLY, IS FOUND.                
      * MESSAGES GIVING DETAILS OF THE ABEND ARE DISPLAYEDAND THE               
      * AND THE PROGRAM WILL TERMINATE WITH A RETURN CODE OF 16.                
      *****************************************************************         
                                                                                
       9999-ABEND.                                                              
                                                                                
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '9999-ABEND' TO AB-PARAGRAPH-NAME (LVL).                        
                                                                                
      ***  DISPLAY ABEND MESSAGE                                                
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '*************************************************'.         
           DISPLAY '***** P R O G R A M   T E R M I N A T E D   *****'.         
           DISPLAY '*****          A B N O R M A L L Y          *****'.         
           DISPLAY '*************************************************'.         
           DISPLAY ' '.                                                         
           DISPLAY 'MODULE NAME    : ' AB-MODULE-NAME.                          
           DISPLAY 'PARAGRAPH NAME : ' AB-PARAGRAPH-NAME (LVL).                 
                                                                                
           IF AB-SQLCODE NOT = ZERO                                             
              DISPLAY ' '                                                       
              DISPLAY 'SQLCODE:  ' AB-SQLCODE                                   
           END-IF.                                                              
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY AB-MSG1.                                                     
           DISPLAY AB-MSG2.                                                     
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '********     P R O G R A M   F L O W     ********'.         
           DISPLAY ' '.                                                         
           PERFORM                                                              
              WITH TEST BEFORE                                                  
              VARYING CNT FROM 1 BY 1                                           
              UNTIL CNT > LVL                                                   
                                                                                
              DISPLAY AB-PARAGRAPH-NAME (CNT)                                   
                                                                                
           END-PERFORM.                                                         
                                                                                
           IF  WS-INPUT-OPEN                                                    
               PERFORM  6300-CLOSE-INPUT   THRU  6300-EXIT.                     
                                                                                
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                        12330000
           EXEC SQL                                                     12340000
             ROLLBACK                                                   12350000
           END-EXEC.                                                    12360000
                                                                        12370000
           MOVE +16 TO RETURN-CODE.                                     12380000
                                                                        12370000
           GOBACK.                                                              
                                                                                
       9999-EXIT.                                                               
           EXIT.                                                                
