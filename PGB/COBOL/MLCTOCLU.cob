CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLCTOCLU.                                                 
      *AUTHOR.        FRED MUELLER.                                             
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.                                                            
      *DATE-COMPILED.                                                           
      *----------------------------------------------------------------*        
      *                                                                         
      *  SYSTEM    : GROUP BENEFITS BUSINESS CONTENT LOOKUP DRIVER              
      *                                                                         
      *  LANGUAGE  : COBOL II                                                   
      *                                                                         
      *  THIS MODULE WILL PERFORM ALL CONTENT LOOKUP SERVICES REQUIRED.         
      *  IT WILL HIDE ANY OPTIMIZATION AND DATA ACCESS LOGIC FOR LOOKUPS        
      *  FROM THE CALLING ROUTINES SINCE THE PROTOCOL FOR MLCTGGLU              
      *  SPECIFIES LR-NAME AND LOOKUP OPTIMIZATION OPTION THAT MAY              
      *  DIFFER DEPENDING ON THE RUNTIME ENVIRONMENT (CICS OR BATCH).           
      *  FOR HARDCORE USERS, THEY CAN CALL THE GENERIC LOOKUP SERVICE           
      *  DIRECTLY BUT THIS ROUTINE IS INTENDED TO PROVIDE THE MOST              
      *  COMMON LOOKUP SERVICES WITH A SIMPLE API. THIS OBJECT LAYER            
      *  ALSO CREATES A LOGICAL DATA SEPARATION FROM THE CALLING                
      *  PROGRAM TO ALLOW FOR FUTURE ENHANCEMENTS THAT MAY ACCESS               
      *  GROUP BENEFITS CONTENT FROM OTHER STORES WITHIN OUR DOMAIN.            
      *                                                                         
      *--HISTORY LOG---------------------------------------------------*        
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  -----------------------------------*        
      *  001  MAY 2002   F MUELLER  CREATED FOR IEL PROJECT                     
      *  002  JAN 2003   F MUELLER  ADDED HISTORY OPTION TO GENERIC             
      *                             LOOKUP PROTOCOL (MLCTRGLU) IN ORDER         
      *                             TO CORRECT PROBLEM WITH LOCATING            
      *                             CONTENT IN EFFECT AS OF REQUESTED           
      *                             PROCESS DATE.                               
      *  003  AUG 2008   IBM        ENTERPRISE COMPILER UPGRADE(ECU)            
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
               '**   MLCTOCLU WORKING STORAGE BEGINS  **'.                      
                                                                                
       01  WS-VARIABLES.                                                        
           05  SUB                          PIC S9(4) COMP.                     
           05  WS-CALL-TYPE-SW              PIC X(1) VALUE 'F'.                 
               88  FIRST-CALL               VALUE 'F'.                          
               88  NORMAL-CALL              VALUE 'N'.                          
               88  STATUS-CALL              VALUE 'S'.                          
           05  WS-LOOKUP-SW                 PIC X(1) VALUE 'S'.                 
               88  SIMPLE-LOOKUP            VALUE 'S'.                          
               88  COMPLEX-LOOKUP           VALUE 'C'.                          
                                                                                
       01  WS-CONSTANTS.                                                        
           05  WS-PROGRAM-ID                PIC X(08) VALUE 'MLCTOCLU'.         
           05  WS-CALLED-MODULES.                                               
               10  MLCTGGLU                 PIC X(08) VALUE 'MLCTGGLU'.         
               10  GC2DATE                  PIC X(08) VALUE 'GC2DATE '.         
               10  CGC2DATE                 PIC X(08) VALUE 'CGC2DATE'.         
           05  WS-HIGH-DATE                 PIC 9(08) VALUE 99999999.           
                                                                                
      *-----------------------------------------------------------------        
      *   GACDATE PROTOCOL                                                      
      *-----------------------------------------------------------------        
       01  GAC-DATE-PARAMETERS.             COPY GARDATEP.                      
                                                                                
      *----------------------------------------------------------------*        
      *  INTERNAL LOOKUP OVERRIDE REGISTRY                                      
      *----------------------------------------------------------------*        
       01  LOOKUP-OVERRIDE-REGISTRY.        COPY MLCTRRLU.                      
                                                                                
      *----------------------------------------------------------------*        
      *  INTERNAL INPUTS                                                        
      *----------------------------------------------------------------*        
       01  CONTENT-INPUT-AREA.                                                  
           05  MLCT-CONTENT-NAME           PIC X(08).                           
               88  MLCT-CONTENT-CHEQLOGO   VALUE 'CHEQLOGO'.                    
               88  MLCT-CONTENT-FLASHFRM   VALUE 'FLASHFRM'.                    
               88  MLCT-CONTENT-MEMPHONE   VALUE 'MEMPHONE'.                    
               88  MLCT-CONTENT-PDTODATE   VALUE 'PDTODATE'.                    
               88  MLCT-CONTENT-PRTYCLNT   VALUE 'PRTYCLNT'.                    
           05  MLCT-CONTENT-ARGUMENT       PIC X(40).                           
           05  MLCT-CONTENT-EFF-DATE       PIC 9(08).                           
           05  MLCT-CONTENT-PROC-DATE      PIC 9(08).                           
           05  MLCT-MAX-DATA-LENGTH        PIC S9(04) COMP.                     
           05  MLCT-SYSTEM-DATE            PIC 9(08).                           
           05  MLCT-LOOKUP-LR-NAME         PIC X(16).                           
           05  MLCT-LOOKUP-OPTION          PIC X(01).                           
           05  MLCT-DEFAULT-OPTION         PIC X(01).                           
           05  MLCT-HISTORY-OPTION         PIC X(01).                           
               88  MLCT-HISTORY-CURRENT    VALUE ' '.                           
               88  MLCT-HISTORY-BROWSE     VALUE 'H'.                           
                                                                                
      *-----------------------------------------------------------------        
      *    SPECIAL CONTENT LOOKUP INPUT PROTOCOLS                               
      *-----------------------------------------------------------------        
       01  MLCT-CONTENT-INPUT               PIC X(40).                          
       01  MLCT-CHEQLOGO-INPUT REDEFINES    MLCT-CONTENT-INPUT.                 
                                            COPY MLCT002I.                      
       01  MLCT-FLASHFRM-INPUT REDEFINES    MLCT-CONTENT-INPUT.                 
                                            COPY MLCT026I.                      
       01  MLCT-MEMPHONE-INPUT REDEFINES    MLCT-CONTENT-INPUT.                 
                                            COPY MLCT004I.                      
       01  MLCT-PRTYCLNT-INPUT REDEFINES    MLCT-CONTENT-INPUT.                 
                                            COPY MLCT015I.                      
       01  MLCT-PDTODATE-INPUT REDEFINES    MLCT-CONTENT-INPUT.                 
                                            COPY MLCT034I.                      
       01  SAVE-CHEQLOGO-REQUEST.                                               
           05  SAVE-CHEQLOGO-ARGUMENT       PIC X(40).                          
           05  SAVE-CHEQLOGO-EFF-DATE       PIC 9(08).                          
           05  SAVE-CHEQLOGO-PROC-DATE      PIC 9(08).                          
       01  SAVE-CHEQLOGO-INPUT              PIC X(40).                          
       01  SAVE-FLASHFRM-REQUEST.                                               
           05  SAVE-FLASHFRM-ARGUMENT       PIC X(40).                          
           05  SAVE-FLASHFRM-EFF-DATE       PIC 9(08).                          
           05  SAVE-FLASHFRM-PROC-DATE      PIC 9(08).                          
       01  SAVE-FLASHFRM-INPUT              PIC X(40).                          
       01  SAVE-MEMPHONE-REQUEST.                                               
           05  SAVE-MEMPHONE-ARGUMENT       PIC X(40).                          
           05  SAVE-MEMPHONE-EFF-DATE       PIC 9(08).                          
           05  SAVE-MEMPHONE-PROC-DATE      PIC 9(08).                          
       01  SAVE-MEMPHONE-INPUT              PIC X(40).                          
       01  SAVE-PRTYCLNT-REQUEST.                                               
           05  SAVE-PRTYCLNT-ARGUMENT       PIC X(40).                          
           05  SAVE-PRTYCLNT-EFF-DATE       PIC 9(08).                          
           05  SAVE-PRTYCLNT-PROC-DATE      PIC 9(08).                          
       01  SAVE-PRTYCLNT-INPUT              PIC X(40).                          
       01  SAVE-PDTODATE-REQUEST.                                               
           05  SAVE-PDTODATE-ARGUMENT       PIC X(40).                          
           05  SAVE-PDTODATE-EFF-DATE       PIC 9(08).                          
           05  SAVE-PDTODATE-PROC-DATE      PIC 9(08).                          
       01  SAVE-PDTODATE-INPUT              PIC X(40).                          
                                                                                
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
      *  LOOKUP PARMS                                                           
      *----------------------------------------------------------------*        
       01  MLCTGGLU-PROTOCOL.                                                   
           COPY MLCTRGLU.                                                       
                                                                                
       01  MLCTGGLU-CONTENT                    PIC X(15000).                    
                                                                                
       01  FILLER                              PIC X(40) VALUE                  
               '***  MLCTOCLU WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  LOOKUP PARMS                                                           
      *----------------------------------------------------------------*        
       01  MLCTOCLU-PROTOCOL.                                                   
           COPY MLCTRCLU.                                                       
                                                                                
       01  MLCTOCLU-CONTENT                    PIC X(01).                       
                                                                                
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING MLCTOCLU-PROTOCOL                               
                                MLCTOCLU-CONTENT.                               
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
           SET SIMPLE-LOOKUP              TO TRUE.                              
                                                                                
           INITIALIZE MLCTOCLU-OUTPUT.                                          
                                                                                
           IF FIRST-CALL                                                        
              PERFORM 1100-GET-SYSTEM-DATE        THRU 1100-EXIT                
           END-IF.                                                              
                                                                                
           PERFORM 1110-SETUP-PARAMETERS          THRU 1110-EXIT.               
                                                                                
           IF MLCT-CONTENT-NAME = WS-PROGRAM-ID                                 
              SET STATUS-CALL  TO TRUE                                          
              MOVE MLCTGGLU    TO MLCT-CONTENT-NAME                             
           ELSE                                                                 
              SET NORMAL-CALL  TO TRUE                                          
           END-IF.                                                              
                                                                                
           PERFORM 1120-SETUP-LOOKUP-CALL         THRU 1120-EXIT.               
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
       1100-GET-SYSTEM-DATE.                                                    
      *****************************************************************         
      * CALL GACDATE TO GET CURRENT SYSTEM DATE                                 
      *****************************************************************         
                                                                                
            MOVE 'E'            TO  VDATE-REQ-LANGUAGE.                         
            MOVE 'A'            TO  VDATE-REQ-BASIS.                            
            MOVE '1'            TO  VDATE-REQ-DETAIL.                           
            MOVE 'E'            TO  VDATE-REQ-SERVICE.                          
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *          CALL GC2DATE USING GAC-DATE-PARAMETERS.                        
      *                                                                         
      *                                                                         
            COPY MLPGC2D.                                                       
                                                                                
            IF VDATE-RET-FAIL                                                   
               SET  MLCT-ERROR                TO TRUE                           
               MOVE 'GACDATE'                 TO MLCT-ERR-PGM-ID                
               MOVE VDATE-RETURN-AREA         TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTOCLU - 1100'         TO MLCT-ERR-PGM-LVL               
               MOVE 'GET SYSTEM DATE FAILED'  TO MLCT-ERR-PGM-DESC              
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
            ELSE                                                                
               MOVE VDATE1-YYYYMMDD           TO MLCT-SYSTEM-DATE               
            END-IF.                                                             
                                                                                
       1100-EXIT.                                                               
           EXIT.                                                                
                                                                                
      ****************************************************************          
      *    SETUP INTERNAL PARMS BASED ON THOSE PASSED FROM CALLING PGM          
      ****************************************************************          
       1110-SETUP-PARAMETERS.                                                   
           MOVE OCLU-CONTENT-NAME              TO MLCT-CONTENT-NAME.            
           MOVE OCLU-CONTENT-ARGUMENT          TO MLCT-CONTENT-ARGUMENT.        
                                                                                
           IF  OCLU-CONTENT-EFF-DATE NUMERIC                                    
               IF  OCLU-CONTENT-EFF-DATE = WS-HIGH-DATE                         
                   MOVE MLCT-SYSTEM-DATE      TO MLCT-CONTENT-EFF-DATE          
               ELSE                                                             
                   MOVE OCLU-CONTENT-EFF-DATE TO MLCT-CONTENT-EFF-DATE          
               END-IF                                                           
           ELSE                                                                 
               SET  MLCT-INVALID-EFF-DATE     TO TRUE                           
               MOVE 'MLCTOCLU'                TO MLCT-ERR-PGM-ID                
               MOVE 'BAD DATE'                TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTOCLU - 1110'         TO MLCT-ERR-PGM-LVL               
               MOVE 'EFFECTIVE DATE NOT NUMERIC' TO MLCT-ERR-PGM-DESC           
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
           END-IF.                                                              
                                                                                
           IF  OCLU-CONTENT-PROC-DATE NUMERIC                                   
               IF  OCLU-CONTENT-PROC-DATE = WS-HIGH-DATE                        
                   MOVE MLCT-SYSTEM-DATE       TO MLCT-CONTENT-PROC-DATE        
               ELSE                                                             
                   MOVE OCLU-CONTENT-PROC-DATE TO MLCT-CONTENT-PROC-DATE        
               END-IF                                                           
           ELSE                                                                 
               SET  MLCT-INVALID-PROC-DATE    TO TRUE                           
               MOVE 'MLCTOCLU'                TO MLCT-ERR-PGM-ID                
               MOVE 'BAD DATE'                TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTOCLU - 1110'         TO MLCT-ERR-PGM-LVL               
               MOVE 'PROCESS DATE NOT NUMERIC' TO MLCT-ERR-PGM-DESC             
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
           END-IF.                                                              
                                                                                
           IF  OCLU-MAX-DATA-LENGTH     > ZERO   AND                            
               OCLU-MAX-DATA-LENGTH NOT > LENGTH OF MLCTGGLU-CONTENT            
               MOVE OCLU-MAX-DATA-LENGTH      TO MLCT-MAX-DATA-LENGTH           
           ELSE                                                                 
               SET  MLCT-INVALID-DATA-LENGTH  TO TRUE                           
               MOVE 'MLCTOCLU'                TO MLCT-ERR-PGM-ID                
               MOVE 'BAD LEN '                TO MLCT-ERR-PGM-STATUS            
               MOVE 'MLCTOCLU - 1110'         TO MLCT-ERR-PGM-LVL               
               MOVE 'MAX DATA LENGTH OUT OF RANGE' TO MLCT-ERR-PGM-DESC         
               PERFORM 9000-RAISE-EXCEPTION THRU 9000-EXIT                      
           END-IF.                                                              
                                                                                
       1110-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
      ****************************************************************          
      *    SEARCH THE INTERNAL REGISTRY TO DETERMINE WHETHER THERE              
      *    ARE ANY OVERRIDES TO THE CALLING PROTOCOL FOR THE CONTENT            
      *    NAME PASSED.  INITIALIZE EACH CALL WITH THE DEFAULTS                 
      *    PLACED IN THE FIRST REGISTRY ENTRY BEFORE SEARCHING.                 
      ****************************************************************          
       1120-SETUP-LOOKUP-CALL.                                                  
                                                                                
           MOVE +1 TO SUB.                                                      
                                                                                
           PERFORM                                                              
                UNTIL SUB > MLCTRRLU-NUM-ENTRIES                                
                   OR RRLU-LOOKUP-ALIAS (SUB) = MLCT-CONTENT-NAME               
              ADD +1 TO SUB                                                     
           END-PERFORM.                                                         
                                                                                
           IF SUB > MLCTRRLU-NUM-ENTRIES                                        
              MOVE MLCTRRLU-NUM-ENTRIES   TO SUB                                
           ELSE                                                                 
              MOVE RRLU-LOOKUP-NAME (SUB) TO MLCT-CONTENT-NAME                  
           END-IF.                                                              
                                                                                
           MOVE RRLU-LOOKUP-LR-NAME (SUB) TO MLCT-LOOKUP-LR-NAME.               
           MOVE RRLU-LOOKUP-OPTION  (SUB) TO MLCT-LOOKUP-OPTION.                
           MOVE RRLU-LOOKUP-DEFAULTS(SUB) TO MLCT-DEFAULT-OPTION.               
      *                                                                         
      *    SET UP HISTORY OPTION BASED ON PROCESS DATE ON REQUEST SO            
      *    WE CAN FIND CONTENT THAT WAS IN EFFECT AS OF THE SPECIFIED           
      *    PROCESS DATE.                                                        
      *                                                                         
           IF MLCT-CONTENT-PROC-DATE < MLCT-SYSTEM-DATE                         
              SET MLCT-HISTORY-BROWSE     TO TRUE                               
           ELSE                                                                 
              SET MLCT-HISTORY-CURRENT    TO TRUE                               
           END-IF.                                                              
                                                                                
       1120-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    PROCESS LOOKUP REQUEST                                               
      ****************************************************************          
       2000-PROCESS-REQUEST.                                                    
                                                                                
           MOVE MLCT-CONTENT-NAME       TO GGLU-LOOKUP-NAME.                    
           MOVE MLCT-CONTENT-ARGUMENT   TO MLCT-CONTENT-INPUT.                  
           MOVE MLCT-MAX-DATA-LENGTH    TO GGLU-MAX-DATA-LENGTH.                
           MOVE MLCT-LOOKUP-LR-NAME     TO GGLU-LOOKUP-LR-NAME.                 
           MOVE MLCT-LOOKUP-OPTION      TO GGLU-LOOKUP-OPTION.                  
           MOVE MLCT-DEFAULT-OPTION     TO GGLU-DEFAULT-OPTION.                 
           MOVE MLCT-HISTORY-OPTION     TO GGLU-HISTORY-OPTION.                 
                                                                                
           EVALUATE TRUE                                                        
           WHEN MLCT-CONTENT-CHEQLOGO                                           
              PERFORM 2200-LOOKUP-CHEQLOGO THRU 2200-EXIT                       
           WHEN MLCT-CONTENT-FLASHFRM                                           
              PERFORM 2300-LOOKUP-FLASHFRM THRU 2300-EXIT                       
           WHEN MLCT-CONTENT-MEMPHONE                                           
              PERFORM 2500-LOOKUP-MEMPHONE THRU 2500-EXIT                       
           WHEN MLCT-CONTENT-PRTYCLNT                                           
              PERFORM 2400-LOOKUP-PRTYCLNT THRU 2400-EXIT                       
           WHEN MLCT-CONTENT-PDTODATE                                           
              PERFORM 2600-LOOKUP-PDTODATE THRU 2600-EXIT                       
           WHEN OTHER                                                           
              MOVE MLCT-CONTENT-ARGUMENT   TO GGLU-LOOKUP-ARGUMENT              
              MOVE MLCT-CONTENT-EFF-DATE   TO GGLU-LOOKUP-EFF-DATE              
              MOVE MLCT-CONTENT-PROC-DATE  TO GGLU-LOOKUP-PROC-DATE             
              PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT                       
           END-EVALUATE.                                                        
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *   FOR SIMPLE LOOKUPS JUST CALL THE GENERIC LOOKUP ROUTINE.              
      ****************************************************************          
       2100-LOOKUP-REQUEST.                                                     
                                                                                
           INITIALIZE MLCTGGLU-OUTPUT.                                          
           PERFORM 7000-CALL-MLCTGGLU THRU 7000-EXIT.                           
                                                                                
           MOVE GGLU-RETURN-STATUS      TO MLCT-RETURN-STATUS.                  
           MOVE GGLU-OUTPUT-LENGTH      TO MLCT-OUTPUT-LENGTH.                  
           MOVE GGLU-ERROR-DETAILS      TO MLCT-ERROR-DETAILS.                  
                                                                                
           IF MLCT-OUTPUT-LENGTH > ZERO                                         
              MOVE MLCTGGLU-CONTENT (1:MLCT-OUTPUT-LENGTH)                      
                TO MLCTOCLU-CONTENT (1:MLCT-OUTPUT-LENGTH)                      
           END-IF.                                                              
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *   FOR CHEQUE TABLE LOOKUPS, WE NEED TO DO MULTIPLE LOOKUPS              
      *   AGAINST THE SAME TABLE BUT CHANGE THE ARGUMENT PASSED                 
      *   WHEN A NOT FOUND IS ENCOUNTERED.  ALSO, SAVE THE INITIAL              
      *   LOOKUP REQUEST AND THE ASSOCIATED ARGUMENT SO THAT REPEATED           
      *   HITS FOR THE SAME ARGUMENT INVOKE MLCTGGLU MORE EFFICIENTLY.          
      ****************************************************************          
       2200-LOOKUP-CHEQLOGO.                                                    
                                                                                
           IF SAVE-CHEQLOGO-ARGUMENT  = MLCT-CONTENT-ARGUMENT AND               
              SAVE-CHEQLOGO-EFF-DATE  = MLCT-CONTENT-EFF-DATE AND               
              SAVE-CHEQLOGO-PROC-DATE = MLCT-CONTENT-PROC-DATE                  
               MOVE SAVE-CHEQLOGO-INPUT     TO GGLU-LOOKUP-ARGUMENT             
           ELSE                                                                 
               SET  COMPLEX-LOOKUP          TO TRUE                             
               MOVE MLCT-CONTENT-ARGUMENT   TO GGLU-LOOKUP-ARGUMENT             
           END-IF.                                                              
                                                                                
           MOVE MLCT-CONTENT-ARGUMENT       TO SAVE-CHEQLOGO-ARGUMENT.          
           MOVE MLCT-CONTENT-EFF-DATE       TO GGLU-LOOKUP-EFF-DATE.            
           MOVE MLCT-CONTENT-PROC-DATE      TO GGLU-LOOKUP-PROC-DATE.           
                                                                                
           IF  SIMPLE-LOOKUP                                                    
               PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT                      
           ELSE                                                                 
               PERFORM 2210-LOCATE-CHEQLOGO THRU 2210-EXIT                      
           END-IF.                                                              
                                                                                
           MOVE GGLU-LOOKUP-ARGUMENT        TO SAVE-CHEQLOGO-INPUT.             
           MOVE GGLU-LOOKUP-EFF-DATE        TO SAVE-CHEQLOGO-EFF-DATE.          
           MOVE GGLU-LOOKUP-PROC-DATE       TO SAVE-CHEQLOGO-PROC-DATE.         
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *    LOCATE THE CHEQLOGO ENTRY BY MANIPULATING THE ORIGINAL               
      *    ARGUMENT AND REPEATING THE LOOKUP UNTIL THE ENTRY IS FOUND.          
      ****************************************************************          
       2210-LOCATE-CHEQLOGO.                                                    
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
           EVALUATE TRUE                                                        
           WHEN  MLCT-RET-NOT-FOUND                                             
           WHEN  MLCT-DEFAULT                                                   
           WHEN  MLCT-TRUNC-DEFAULT                                             
               MOVE SPACES              TO MLCTOCLU-CHEQLOGO-LOCATION           
               MOVE MLCT-CHEQLOGO-INPUT TO GGLU-LOOKUP-ARGUMENT                 
           WHEN  OTHER                                                          
               GO TO 2210-EXIT                                                  
           END-EVALUATE.                                                        
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
           EVALUATE TRUE                                                        
           WHEN  MLCT-RET-NOT-FOUND                                             
           WHEN  MLCT-DEFAULT                                                   
           WHEN  MLCT-TRUNC-DEFAULT                                             
               MOVE SPACES              TO MLCTOCLU-CHEQLOGO-DIVISION           
               MOVE MLCT-CHEQLOGO-INPUT TO GGLU-LOOKUP-ARGUMENT                 
           WHEN  OTHER                                                          
               GO TO 2210-EXIT                                                  
           END-EVALUATE.                                                        
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
       2210-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *   FOR FLASH FORM LOOKUPS, WE NEED TO DO MULTIPLE LOOKUPS                
      *   AGAINST THE SAME TABLE BUT CHANGE THE ARGUMENT PASSED                 
      *   WHEN A NOT FOUND IS ENCOUNTERED.  ALSO, SAVE THE INITIAL              
      *   LOOKUP REQUEST AND THE ASSOCIATED ARGUMENT SO THAT REPEATED           
      *   HITS FOR THE SAME ARGUMENT INVOKE MLCTGGLU MORE EFFICIENTLY.          
      ****************************************************************          
       2300-LOOKUP-FLASHFRM.                                                    
                                                                                
           IF SAVE-FLASHFRM-ARGUMENT  = MLCT-CONTENT-ARGUMENT AND               
              SAVE-FLASHFRM-EFF-DATE  = MLCT-CONTENT-EFF-DATE AND               
              SAVE-FLASHFRM-PROC-DATE = MLCT-CONTENT-PROC-DATE                  
               MOVE SAVE-FLASHFRM-INPUT     TO GGLU-LOOKUP-ARGUMENT             
           ELSE                                                                 
               SET  COMPLEX-LOOKUP          TO TRUE                             
               MOVE MLCT-CONTENT-ARGUMENT   TO GGLU-LOOKUP-ARGUMENT             
           END-IF.                                                              
                                                                                
           MOVE MLCT-CONTENT-ARGUMENT       TO SAVE-FLASHFRM-ARGUMENT.          
           MOVE MLCT-CONTENT-EFF-DATE       TO GGLU-LOOKUP-EFF-DATE.            
           MOVE MLCT-CONTENT-PROC-DATE      TO GGLU-LOOKUP-PROC-DATE.           
                                                                                
           IF  SIMPLE-LOOKUP                                                    
               PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT                      
           ELSE                                                                 
               PERFORM 2310-LOCATE-FLASHFRM THRU 2310-EXIT                      
           END-IF.                                                              
                                                                                
           MOVE GGLU-LOOKUP-ARGUMENT        TO SAVE-FLASHFRM-INPUT.             
           MOVE GGLU-LOOKUP-EFF-DATE        TO SAVE-FLASHFRM-EFF-DATE.          
           MOVE GGLU-LOOKUP-PROC-DATE       TO SAVE-FLASHFRM-PROC-DATE.         
                                                                                
       2300-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *    LOCATE THE FLASHFRM ENTRY BY MANIPULATING THE ORIGINAL               
      *    ARGUMENT AND REPEATING THE LOOKUP UNTIL THE ENTRY IS FOUND.          
      ****************************************************************          
       2310-LOCATE-FLASHFRM.                                                    
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
           EVALUATE TRUE                                                        
           WHEN  MLCT-RET-NOT-FOUND                                             
           WHEN  MLCT-DEFAULT                                                   
           WHEN  MLCT-TRUNC-DEFAULT                                             
               MOVE SPACES              TO MLCTOCLU-FLASHFRM-LOCATION           
               MOVE MLCT-FLASHFRM-INPUT TO GGLU-LOOKUP-ARGUMENT                 
           WHEN  OTHER                                                          
               GO TO 2310-EXIT                                                  
           END-EVALUATE.                                                        
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
           EVALUATE TRUE                                                        
           WHEN  MLCT-RET-NOT-FOUND                                             
           WHEN  MLCT-DEFAULT                                                   
           WHEN  MLCT-TRUNC-DEFAULT                                             
               MOVE SPACES              TO MLCTOCLU-FLASHFRM-DIVISION           
               MOVE MLCT-FLASHFRM-INPUT TO GGLU-LOOKUP-ARGUMENT                 
           WHEN  OTHER                                                          
               GO TO 2310-EXIT                                                  
           END-EVALUATE.                                                        
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
       2310-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *   FOR SPECIAL CLIENT LOOKUPS, WE NEED TO DO MULTIPLE LOOKUPS            
      *   AGAINST THE SAME TABLE BUT CHANGE THE ARGUMENT PASSED                 
      *   WHEN A NOT FOUND IS ENCOUNTERED.  ALSO, SAVE THE INITIAL              
      *   LOOKUP REQUEST AND THE ASSOCIATED ARGUMENT SO THAT REPEATED           
      *   HITS FOR THE SAME ARGUMENT INVOKE MLCTGGLU MORE EFFICIENTLY.          
      ****************************************************************          
       2400-LOOKUP-PRTYCLNT.                                                    
                                                                                
           IF SAVE-PRTYCLNT-ARGUMENT  = MLCT-CONTENT-ARGUMENT AND               
              SAVE-PRTYCLNT-EFF-DATE  = MLCT-CONTENT-EFF-DATE AND               
              SAVE-PRTYCLNT-PROC-DATE = MLCT-CONTENT-PROC-DATE                  
               MOVE SAVE-PRTYCLNT-INPUT     TO GGLU-LOOKUP-ARGUMENT             
           ELSE                                                                 
               SET  COMPLEX-LOOKUP          TO TRUE                             
               MOVE MLCT-CONTENT-ARGUMENT   TO GGLU-LOOKUP-ARGUMENT             
           END-IF.                                                              
                                                                                
           MOVE MLCT-CONTENT-ARGUMENT       TO SAVE-PRTYCLNT-ARGUMENT.          
           MOVE MLCT-CONTENT-EFF-DATE       TO GGLU-LOOKUP-EFF-DATE.            
           MOVE MLCT-CONTENT-PROC-DATE      TO GGLU-LOOKUP-PROC-DATE.           
                                                                                
           IF  SIMPLE-LOOKUP                                                    
               PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT                      
           ELSE                                                                 
               PERFORM 2410-LOCATE-PRTYCLNT THRU 2410-EXIT                      
           END-IF.                                                              
                                                                                
           MOVE GGLU-LOOKUP-ARGUMENT        TO SAVE-PRTYCLNT-INPUT.             
           MOVE GGLU-LOOKUP-EFF-DATE        TO SAVE-PRTYCLNT-EFF-DATE.          
           MOVE GGLU-LOOKUP-PROC-DATE       TO SAVE-PRTYCLNT-PROC-DATE.         
                                                                                
       2400-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *    LOCATE THE PRTYCLNT ENTRY BY MANIPULATING THE ORIGINAL               
      *    ARGUMENT AND REPEATING THE LOOKUP UNTIL THE ENTRY IS FOUND.          
      ****************************************************************          
       2410-LOCATE-PRTYCLNT.                                                    
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
           EVALUATE TRUE                                                        
           WHEN  MLCT-RET-NOT-FOUND                                             
           WHEN  MLCT-DEFAULT                                                   
           WHEN  MLCT-TRUNC-DEFAULT                                             
               MOVE SPACES              TO MLCTOCLU-PRTYCLNT-DIVISION           
               MOVE MLCT-PRTYCLNT-INPUT TO GGLU-LOOKUP-ARGUMENT                 
           WHEN  OTHER                                                          
               GO TO 2410-EXIT                                                  
           END-EVALUATE.                                                        
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
       2410-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *   FOR CHEQUE TABLE LOOKUPS, WE NEED TO DO MULTIPLE LOOKUPS              
      *   AGAINST THE SAME TABLE BUT CHANGE THE ARGUMENT PASSED                 
      *   WHEN A NOT FOUND IS ENCOUNTERED.  ALSO, SAVE THE INITIAL              
      *   LOOKUP REQUEST AND THE ASSOCIATED ARGUMENT SO THAT REPEATED           
      *   HITS FOR THE SAME ARGUMENT INVOKE MLCTGGLU MORE EFFICIENTLY.          
      ****************************************************************          
       2500-LOOKUP-MEMPHONE.                                                    
                                                                                
           IF SAVE-MEMPHONE-ARGUMENT  = MLCT-CONTENT-ARGUMENT AND               
              SAVE-MEMPHONE-EFF-DATE  = MLCT-CONTENT-EFF-DATE AND               
              SAVE-MEMPHONE-PROC-DATE = MLCT-CONTENT-PROC-DATE                  
               MOVE SAVE-MEMPHONE-INPUT     TO GGLU-LOOKUP-ARGUMENT             
           ELSE                                                                 
               SET  COMPLEX-LOOKUP          TO TRUE                             
               MOVE MLCT-CONTENT-ARGUMENT   TO GGLU-LOOKUP-ARGUMENT             
           END-IF.                                                              
                                                                                
           MOVE MLCT-CONTENT-ARGUMENT       TO SAVE-MEMPHONE-ARGUMENT.          
           MOVE MLCT-CONTENT-EFF-DATE       TO GGLU-LOOKUP-EFF-DATE.            
           MOVE MLCT-CONTENT-PROC-DATE      TO GGLU-LOOKUP-PROC-DATE.           
                                                                                
           IF  SIMPLE-LOOKUP                                                    
               PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT                      
           ELSE                                                                 
               PERFORM 2510-LOCATE-MEMPHONE THRU 2510-EXIT                      
           END-IF.                                                              
                                                                                
           MOVE GGLU-LOOKUP-ARGUMENT        TO SAVE-MEMPHONE-INPUT.             
           MOVE GGLU-LOOKUP-EFF-DATE        TO SAVE-MEMPHONE-EFF-DATE.          
           MOVE GGLU-LOOKUP-PROC-DATE       TO SAVE-MEMPHONE-PROC-DATE.         
                                                                                
       2500-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *    LOCATE THE MEMPHONE ENTRY BY MANIPULATING THE ORIGINAL               
      *    ARGUMENT AND REPEATING THE LOOKUP UNTIL THE ENTRY IS FOUND.          
      ****************************************************************          
       2510-LOCATE-MEMPHONE.                                                    
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
           EVALUATE TRUE                                                        
           WHEN  MLCT-RET-NOT-FOUND                                             
           WHEN  MLCT-DEFAULT                                                   
           WHEN  MLCT-TRUNC-DEFAULT                                             
               MOVE SPACES           TO MLCTOCLU-MEMPHONE-DESTINATION           
               MOVE MLCT-MEMPHONE-INPUT TO GGLU-LOOKUP-ARGUMENT                 
           WHEN  OTHER                                                          
               GO TO 2510-EXIT                                                  
           END-EVALUATE.                                                        
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
       2510-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *   FOR SPECIAL CLIENT LOOKUPS, WE NEED TO DO MULTIPLE LOOKUPS            
      *   AGAINST THE SAME TABLE BUT CHANGE THE ARGUMENT PASSED                 
      *   WHEN A NOT FOUND IS ENCOUNTERED.  ALSO, SAVE THE INITIAL              
      *   LOOKUP REQUEST AND THE ASSOCIATED ARGUMENT SO THAT REPEATED           
      *   HITS FOR THE SAME ARGUMENT INVOKE MLCTGGLU MORE EFFICIENTLY.          
      ****************************************************************          
       2600-LOOKUP-PDTODATE.                                                    
                                                                                
           IF SAVE-PDTODATE-ARGUMENT  = MLCT-CONTENT-ARGUMENT AND               
              SAVE-PDTODATE-EFF-DATE  = MLCT-CONTENT-EFF-DATE AND               
              SAVE-PDTODATE-PROC-DATE = MLCT-CONTENT-PROC-DATE                  
               MOVE SAVE-PDTODATE-INPUT     TO GGLU-LOOKUP-ARGUMENT             
           ELSE                                                                 
               SET  COMPLEX-LOOKUP          TO TRUE                             
               MOVE MLCT-CONTENT-ARGUMENT   TO GGLU-LOOKUP-ARGUMENT             
           END-IF.                                                              
                                                                                
           MOVE MLCT-CONTENT-ARGUMENT       TO SAVE-PDTODATE-ARGUMENT.          
           MOVE MLCT-CONTENT-EFF-DATE       TO GGLU-LOOKUP-EFF-DATE.            
           MOVE MLCT-CONTENT-PROC-DATE      TO GGLU-LOOKUP-PROC-DATE.           
                                                                                
           IF  SIMPLE-LOOKUP                                                    
               PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT                      
           ELSE                                                                 
               PERFORM 2610-LOCATE-PDTODATE THRU 2610-EXIT                      
           END-IF.                                                              
                                                                                
           MOVE GGLU-LOOKUP-ARGUMENT        TO SAVE-PDTODATE-INPUT.             
           MOVE GGLU-LOOKUP-EFF-DATE        TO SAVE-PDTODATE-EFF-DATE.          
           MOVE GGLU-LOOKUP-PROC-DATE       TO SAVE-PDTODATE-PROC-DATE.         
                                                                                
       2600-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
      *    LOCATE THE PDTODATE ENTRY BY MANIPULATING THE ORIGINAL               
      *    ARGUMENT AND REPEATING THE LOOKUP UNTIL THE ENTRY IS FOUND.          
      ****************************************************************          
       2610-LOCATE-PDTODATE.                                                    
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
           EVALUATE TRUE                                                        
           WHEN  MLCT-RET-NOT-FOUND                                             
           WHEN  MLCT-DEFAULT                                                   
           WHEN  MLCT-TRUNC-DEFAULT                                             
               MOVE SPACES              TO MLCTOCLU-PDTODATE-DIVISION           
               MOVE MLCT-PDTODATE-INPUT TO GGLU-LOOKUP-ARGUMENT                 
           WHEN  OTHER                                                          
               GO TO 2610-EXIT                                                  
           END-EVALUATE.                                                        
                                                                                
           PERFORM 2100-LOOKUP-REQUEST  THRU 2100-EXIT.                         
                                                                                
       2610-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
      ****************************************************************          
      *    CALL APPROPRIATE MODULE FOR PROCESSING THIS REQUEST                  
      ****************************************************************          
       3000-FINALIZATION.                                                       
                                                                                
           MOVE MLCT-RETURN-STATUS       TO OCLU-RETURN-STATUS.                 
      *                                                                         
      * TABLE NOT FOUND CONDITION PRODUCED FOR OPTIMIZED LOOKUP WHEN            
      * ENTRY NOT FOUND CONDITION PRODUCED FOR NORMAL LOOKUP.                   
      * FOR CONSISTENCY, JUST SEND BACK A NOT FOUND.  ALSO, HIDE                
      * NON-CRITICAL ERRORS FROM THE CALLING PROGRAM.                           
      *                                                                         
           EVALUATE TRUE                                                        
           WHEN MLCT-RET-NOT-FOUND                                              
               SET  OCLU-NOT-FOUND       TO TRUE                                
           WHEN MLCT-NON-CRITICAL-ERROR                                         
               SET  OCLU-OK              TO TRUE                                
           END-EVALUATE.                                                        
                                                                                
           MOVE MLCT-OUTPUT-LENGTH       TO OCLU-OUTPUT-LENGTH.                 
           MOVE MLCT-ERR-PGM-ID          TO OCLU-ERR-PGM-ID.                    
           MOVE MLCT-ERR-PGM-STATUS      TO OCLU-ERR-PGM-STATUS.                
           MOVE MLCT-ERR-PGM-LVL         TO OCLU-ERR-PGM-LVL.                   
           MOVE MLCT-ERR-PGM-DESC        TO OCLU-ERR-PGM-DESC.                  
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ****************************************************************          
       7000-CALL-MLCTGGLU.                                                      
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *          CALL MLCTGGLU USING MLCTGGLU-PROTOCOL                          
      *                              MLCTGGLU-CONTENT.                          
      *                                                                         
           COPY MLCTCGLU.                                                       
       7000-EXIT.                                                               
           EXIT.                                                                
      /                                                                         
                                                                                
       9000-RAISE-EXCEPTION.                                                    
                                                                                
           MOVE MLCT-ERROR-STATUS        TO OCLU-RETURN-STATUS.                 
           MOVE ZERO                     TO OCLU-OUTPUT-LENGTH.                 
           MOVE MLCT-ERR-PGM-ID          TO OCLU-ERR-PGM-ID.                    
           MOVE MLCT-ERR-PGM-STATUS      TO OCLU-ERR-PGM-STATUS.                
           MOVE MLCT-ERR-PGM-LVL         TO OCLU-ERR-PGM-LVL.                   
           MOVE MLCT-ERR-PGM-DESC        TO OCLU-ERR-PGM-DESC.                  
                                                                                
           GO TO 0000-MAINLINE-EXIT.                                            
                                                                                
       9000-EXIT.                                                               
