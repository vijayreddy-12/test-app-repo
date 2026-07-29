       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPGDX3.                                                 
      *AUTHOR.        VAN HEMMEN.                                               
      *DATE-WRITTEN.  JUN 17, 2002.                                             
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS)                                                      
      *   GCCPGDX3 - UPDATE CPD WITH GIPSY/GFM EXTRACTS                         
      *                                                                         
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *   THIS PROGRAM WILL READ THE GROUP/DIVISION EXTRACTS AND                
      *   WILL UPDATE THE RELEVANT INFORMATION ON THE CPD.                      
      *                                                                         
      *   INPUT FILES:  GROUP/DIVISION EXTRACT FILE (DLSI10)                    
      *                                                                         
      *                                                                         
      *   OUTPUT FILES: CPD TABLES TGD, TGDFA, TGDADV, TPAA                     
      *                                                                         
      *   CALLS:                                                                
      *                 DATA SERVER                                             
      *                                                                         
      *****************************************************************         
      *****************************************************************         
      *   MODIFICATION LOG                                                      
      ******************************************************************        
      * PROGRAMMER   :  DATE  :             CHANGE                              
      *    NAME      :DD/MM/YY:           DESCRIPTION                           
      *--------------+--------+-----------------------------------------        
      * VANHEMMEN    :17/06/02: ORIGINAL CODE                                   
      *--------------+--------+-----------------------------------------        
      * KLYN         |27/08/02| RECOMPILE WITH NEW DCLGEN FOR TGDADV            
      *----------------------------------------------------------------         
      * KLYN         |27/08/02| RECOMPILE WITH NEW DCLGEN FOR TGDADV            
      *                       | INCLUDING SUBMIT TYPE                           
      *----------------------------------------------------------------         
      * GORMAN       |14/08/03| ADDED INSERT AND UPDATE OF TPAA                 
      *----------------------------------------------------------------         
WB    * BASHAM       |05/01/06| ADDED ROUTINE TO ADD IN TPAA RECORDS            
WB    *                         FOR TADVME EMPLOYEES                            
      *----------------------------------------------------------------         
WB2   * BASHAM       |11/05/07| ADDED VO CLIENT AND LOCATION NUM                
      *----------------------------------------------------------------         
BC    * CHAPMBR      |20/09/07| ADDED SUBTRACT 1 FROM LVL IN                    
      *                         8510-UPDATE-TCL AND INCREASED                   
      *                         AB-PARAGRAPH-NAME TO 100                        
      *----------------------------------------------------------------         
      * IBM GR       º26/08/08º UPGRADED IN ECU PROJECT                         
      *--------------+--------+-----------------------------------------        
      * IBM          º01/05/14º TL-211403 : CHANGES TO DETECT DB2               
      *              º        º             DEADLOCK CONDITION                  
      *--------------+--------+-----------------------------------------        
                                                                                
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
      *    DB2 INCLUDES                                                         
      *****************************************************************         
                                                                                
           EXEC SQL INCLUDE SQLCA   END-EXEC.                                   
                                                                                
           EXEC SQL INCLUDE TUT     END-EXEC.                                   
           EXEC SQL INCLUDE TGD     END-EXEC.                                   
           EXEC SQL INCLUDE TGDFA   END-EXEC.                                   
           EXEC SQL INCLUDE TGDADV  END-EXEC.                                   
           EXEC SQL INCLUDE TPAA    END-EXEC.                                   
           EXEC SQL INCLUDE TADVSR  END-EXEC.                                   
           EXEC SQL INCLUDE TADVME  END-EXEC.                                   
WB2        EXEC SQL INCLUDE TCL     END-EXEC.                                   
KL         EXEC SQL INCLUDE TPACLA  END-EXEC.                                   
                                                                                
           EXEC SQL INCLUDE TUTD    END-EXEC.                                   
           EXEC SQL INCLUDE TGDD    END-EXEC.                                   
           EXEC SQL INCLUDE TGDFAD  END-EXEC.                                   
           EXEC SQL INCLUDE TGDADVD END-EXEC.                                   
           EXEC SQL INCLUDE TPAAD   END-EXEC.                                   
           EXEC SQL INCLUDE TADVSRD END-EXEC.                                   
           EXEC SQL INCLUDE TADVMED END-EXEC.                                   
WB2        EXEC SQL INCLUDE TCLD    END-EXEC.                                   
KL         EXEC SQL INCLUDE TPACLAD END-EXEC.                                   
                                                                                
      *****************************************************************         
      *   SWITCHES AND COUNTERS                                                 
      *****************************************************************         
       01  WS-SWITCHES-COUNTERS.                                                
           05  WS-EOF-SW                 PIC X      VALUE 'N'.                  
               88  WS-EOF                           VALUE 'Y'.                  
           05  WS-DEADLOCK               PIC X      VALUE 'N'.                  
               88 DEADLOCK-Y                        VALUE 'Y'.                  
               88 DEADLOCK-N                        VALUE 'N'.                  
           05  WS-TEMP-CHAR              PIC X      VALUE SPACE.                
           05  WS-START-TIME             PIC 9(8)   VALUE 0.                    
           05  WS-WORK-REGIS-CD          PIC S9(7)  COMP-3 VALUE +0.            
           05  WS-INPUT-COUNTER          PIC S9(7)  COMP-3 VALUE +0.            
           05  WS-ADDED-COUNTER          PIC S9(7)  COMP-3 VALUE +0.            
           05  WS-TERMED-COUNTER         PIC S9(7)  COMP-3 VALUE +0.            
           05  WS-ACTIVATE-COUNTER       PIC S9(7)  COMP-3 VALUE +0.            
           05  WS-UPDATE-COUNTER         PIC S9(7)  COMP-3 VALUE +0.            
           05  WS-SUB                    PIC S9(4)  COMP   VALUE +0.            
           05  WS-SUB2                   PIC S9(4)  COMP   VALUE +0.            
                                                                                
      *****************************************************************         
      *   VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME            PIC X(60) VALUE                        
                'GCCPGDX3 - UPDATE CPD GROUP/DIVISIONS'.                        
BC         10  AB-PARAGRAPH-NAME  OCCURS 100 TIMES                              
                                         PIC X(60).                             
           10  AB-MESSAGE.                                                      
               15  AB-MSG1               PIC X(70).                             
               15  AB-MSG2               PIC X(70).                             
           10  AB-SQLCODE                PIC ----9.                             
           10  LVL                       PIC S9(4) COMP.                        
           10  CNT                       PIC S9(4) COMP.                        
                                                                                
       01  WS-CUST-ID                    PIC S9(11)V USAGE COMP-3.              
       01  WS-CLIENT-NUM                 PIC X(07).                             
       01  WS-LOC-NUM                    PIC X(03).                             
       01  WS-CUST-ID-DISPLAY            PIC 9(11).                             
       01  WS-BILL-PRSNT-IND             PIC X(1).                              
       01  WS-PAY-HIST-IND               PIC X(1).                              
       01  WS-ADVISOR-ID                 PIC X(6).                              
       01  WS-ADVME-ID                   PIC X(20).                             
       01  WS-GROUP-ID                   PIC X(7).                              
       01  WS-LOC-STAT-CD                PIC X.                                 
       01  WS-TADVSR-END-INDICATOR       PIC X.                                 
           88  WS-TADVSR-START           VALUE 'N'.                             
           88  WS-TADVSR-END             VALUE 'Y'.                             
       01  WS-TADVME-END-INDICATOR       PIC X.                                 
           88  WS-TADVME-START           VALUE 'N'.                             
           88  WS-TADVME-END             VALUE 'Y'.                             
       01  WS-TPACLA-END-INDICATOR       PIC X.                                 
           88  WS-TPACLA-START           VALUE 'N'.                             
           88  WS-TPACLA-END             VALUE 'Y'.                             
                                                                                
       01  GCCCGDXR-RECORD.                                                     
           EXEC SQL INCLUDE GCCCGDXR  END-EXEC.                                 
                                                                                
       01  WS-TERMED-RECORD.                                                    
           05  FILLER                     PIC X(04) VALUE ' '.                  
           05  WS-TR-GCCCGDXR-CONTRACT-ID PIC X(07).                            
           05  FILLER                     PIC X(01) VALUE '/'.                  
           05  WS-TR-GCCCGDXR-DIVISION-ID PIC X(03).                            
           05  FILLER                     PIC X(05) VALUE ' '.                  
           05  WS-TR-CONTRACT-STAT-CD     PIC X(01).                            
           05  FILLER                     PIC X(09) VALUE ' '.                  
           05  WS-TR-GCCCGDXR-STATUS-CD   PIC X(01).                            
           05  FILLER                     PIC X(49) VALUE ' '.                  
                                                                                
      ****************************************************************          
      *   DATA SERVER VARIABLES                                                 
      ****************************************************************          
       01  WS-CALLING-VARIABLE.                                                 
           05  WS-GAEDATSR               PIC X(08)                              
                                         VALUE 'GAEDATSR'.                      
           05  WS-GAEDATSR-VERB          PIC X(16).                             
           05  WS-INPUT-LR               PIC X(16)                              
                                         VALUE 'CARD-DATA-010   '.              
           05  WS-PRINT-LR               PIC X(16)                              
                                         VALUE 'PRINT-DATA-020  '.              
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
       01  DATA-SERVER-VERBS.                                                   
           COPY GARDSVRB.                                                       
                                                                                
      *****************************************************************         
      *    DB2 CURSOR DECLARES                                                  
      *****************************************************************         
                                                                                
           EXEC SQL                                                             
             DECLARE TADVSRCUR CURSOR FOR                                       
             SELECT  CUST_ID                                                    
             FROM    TADVSR                                                     
             WHERE   ADVISOR_ID = :WS-ADVISOR-ID                                
           END-EXEC.                                                            
                                                                                
WB         EXEC SQL                                                             
WB           DECLARE TEMPCUR CURSOR FOR                                         
WB           SELECT  CUST_ID                                                    
WB           FROM    TUT                                                        
WB           WHERE   USER_ID = :WS-ADVME-ID                                     
WB         END-EXEC.                                                            
                                                                                
WB         EXEC SQL                                                             
WB           DECLARE TADVMECUR CURSOR FOR                                       
WB           SELECT  EMP_USER_ID                                                
WB           FROM    TADVME TADVME                                              
WB                  ,TUT TUT                                                    
WB                  ,TADVSR TADVSR                                              
WB           WHERE   TADVSR.ADVISOR_ID = :WS-ADVISOR-ID                         
WB           AND     TADVSR.CUST_ID = TUT.CUST_ID                               
WB           AND     TUT.USER_ID = TADVME.MGR_USER_ID                           
WB           ORDER BY EMP_USER_ID ASC                                           
WB         END-EXEC.                                                            
                                                                                
WB         EXEC SQL                                                             
WB           DECLARE TAUTHCUR CURSOR FOR                                        
WB           SELECT  GROUP_ID                                                   
WB           FROM    TPAA TPAA                                                  
WB                  ,TUT TUT                                                    
WB           WHERE TPAA.GROUP_ID = :GCCCGDXR-CONTRACT-ID                        
WB           AND TUT.USER_ID = :WS-ADVME-ID                                     
WB           AND TPAA.CUST_ID = TUT.CUST_ID                                     
WB         END-EXEC.                                                            
                                                                                
WB2        EXEC SQL                                                             
WB2          DECLARE TCLCUR CURSOR FOR                                          
WB2          SELECT  LOC_STAT_CD                                                
WB2          FROM    TCL                                                        
WB2          WHERE   TCL.CLIENT_NUM = :GCCCGDXR-CLIENT-NUM                      
WB2          AND     TCL.LOC_NUM = :GCCCGDXR-LOCATION-NUM                       
WB2        END-EXEC.                                                            
KL    *                                                                         
KL    *    FIND ALL CUST-IDS IN THE ADVISOR LIST                                
KL    *                                                                         
KL         EXEC SQL                                                             
KL           DECLARE TPACLACUR CURSOR FOR                                       
KL           SELECT DISTINCT TADVSR.CUST_ID                                     
KL           FROM   TADVSR                                                      
KL           WHERE  ADVISOR_ID = :WS-ADVISOR-ID                                 
KL         END-EXEC.                                                            
      ****************************************************************          
      *********   P R O C E D U R E   D I V I S I O N   **************          
      ****************************************************************          
       PROCEDURE DIVISION.                                                      
       0000-MAINLINE.                                                           
           MOVE 1 TO LVL.                                                       
           MOVE '0000-MAINLINE'         TO   AB-PARAGRAPH-NAME (LVL).           
                                                                                
           PERFORM 1000-INITIALIZATION.                                         
                                                                                
           PERFORM 2000-PROCESS                                                 
             UNTIL WS-EOF.                                                      
                                                                                
           PERFORM 3000-COMPLETION.                                             
                                                                                
           GOBACK.                                                              
                                                                                
                                                                                
                                                                                
       1000-INITIALIZATION.                                                     
      *****************************************************************         
      * - READ FIRST GFM RECORD                                                 
      * - GET CURRENT TIME FOR USE WHEN ADDING A NEW TGD RECORD                 
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION'    TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           ACCEPT WS-START-TIME        FROM TIME.                               
           MOVE   WS-START-TIME          TO WS-WORK-REGIS-CD.                   
                                                                                
           MOVE 'GCCPGDX3'               TO ICBM-PROGRAM-NAME.                  
           MOVE LOW-VALUES               TO LINKAGE-CONTROL.                    
                                                                                
           MOVE OBTAIN-FIRST             TO WS-GAEDATSR-VERB.                   
           PERFORM 9000-READ-EXTRACT.                                           
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
       2000-PROCESS.                                                            
      *****************************************************************         
      *  CHECK TO SEE IF THE CURRENT GROUP/DIVISION EXISTS IN TGD               
      *   - IF IT DOES, DO UPDATES                                              
      *   - IF IT DOESN'T, ADD (AS LONG AS EXTRACT ISN'T TERMINATED)            
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS'           TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
      *****************************************************************         
      *   REMOVE DUPLICATE ADVISORS FROM THE EXTRACT BEFORE STARTING            
      *****************************************************************         
           PERFORM VARYING WS-SUB FROM +1 BY +1                                 
                     UNTIL WS-SUB > +3                                          
              ADD WS-SUB, +1 GIVING WS-SUB2                                     
              PERFORM UNTIL WS-SUB2 > +4                                        
                 IF GCCCGDXR-ADVISOR-ID (WS-SUB) =                              
                    GCCCGDXR-ADVISOR-ID (WS-SUB2)                               
                      MOVE SPACES TO GCCCGDXR-ADVISOR-ID (WS-SUB2)              
                 END-IF                                                         
                 ADD +1 TO WS-SUB2                                              
              END-PERFORM                                                       
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
                SELECT  CONTRACT_STAT_CD                                        
                  INTO :CONTRACT-STAT-CD                                        
                  FROM  TGD                                                     
                 WHERE GROUP_ID  = :GCCCGDXR-CONTRACT-ID                        
                   AND DIV_ID    = :GCCCGDXR-DIVISION-ID                        
           END-EXEC.                                                            
                                                                                
           EVALUATE SQLCODE                                                     
              WHEN +0                                                           
                 IF CONTRACT-STAT-CD   EQUAL 'T' AND                            
                    GCCCGDXR-STATUS-CD EQUAL 'T'                                
                    CONTINUE                                                    
                 ELSE                                                           
                    PERFORM 2100-UPDATE-CPD                                     
                 END-IF                                                         
              WHEN +100                                                         
                 IF GCCCGDXR-STATUS-CD EQUAL 'T'                                
                    CONTINUE                                                    
                 ELSE                                                           
                    PERFORM 2200-ADD-CPD                                        
                 END-IF                                                         
              WHEN OTHER                                                        
                 MOVE 'SELECT TGD'  TO AB-MSG1                                  
                 MOVE SQLCODE       TO AB-SQLCODE                               
                 MOVE SQLERRMC      TO AB-MSG2                                  
                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                        
                 PERFORM 9999-ABEND                                             
           END-EVALUATE.                                                        
                                                                                
           MOVE OBTAIN-NEXT              TO WS-GAEDATSR-VERB.                   
           PERFORM 9000-READ-EXTRACT.                                           
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
       2100-UPDATE-CPD.                                                         
      *****************************************************************         
      * UPDATE THE RELEVANT RECORDS ON THE CPD.  UPDATES WILL BE                
      * LIMITED TO THE STATUS ON THE TGD TABLE, AND IF THE EXTRACT IS           
      * NOT TERMINATED, VERIFY TGDFA (EHCRPT, DSBRPT) TGDADV AND TPAA           
      *****************************************************************         
           ADD 1 TO LVL.                                                        
           MOVE '2100-UPDATE-CPD'        TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
      ****************************************************************          
      *  UPDATE THE STATUS IN TGD IF IT IS DIFFERENT                 *          
      ****************************************************************          
           IF CONTRACT-STAT-CD NOT EQUAL GCCCGDXR-STATUS-CD                     
              EXEC SQL                                                          
                 UPDATE TGD                                                     
                    SET CONTRACT_STAT_CD = :GCCCGDXR-STATUS-CD                  
                       ,CHN_USER_ID      = 'GCCPGDX3'                           
                       ,CHN_TS           =  CURRENT TIMESTAMP                   
                  WHERE GROUP_ID         = :GCCCGDXR-CONTRACT-ID                
                    AND DIV_ID           = :GCCCGDXR-DIVISION-ID                
              END-EXEC                                                          
              IF SQLCODE = ZERO                                                 
                 IF GCCCGDXR-STATUS-CD = 'T'                                    
                    ADD +1      TO WS-TERMED-COUNTER                            
                                                                                
                    DISPLAY 'TERMED CONT/DIV: ' GCCCGDXR-CONTRACT-ID '/'        
                                                GCCCGDXR-DIVISION-ID            
                    DISPLAY 'CONTRACT-STAT-CD:   ' CONTRACT-STAT-CD             
                            ' (TGD TABLE)'                                      
                    DISPLAY 'GCCCGDXR-STATUS-CD: ' GCCCGDXR-STATUS-CD           
                            ' (EXTRACT)'                                        
                                                                                
      *****************************************************************         
      *             CALL DATA SERVER TO WRITE MESSAGE TO DLSO20                 
      *****************************************************************         
                                                                                
                    MOVE GCCCGDXR-CONTRACT-ID     TO                            
                         WS-TR-GCCCGDXR-CONTRACT-ID                             
                    MOVE GCCCGDXR-DIVISION-ID     TO                            
                         WS-TR-GCCCGDXR-DIVISION-ID                             
                    MOVE CONTRACT-STAT-CD         TO                            
                         WS-TR-CONTRACT-STAT-CD                                 
                    MOVE GCCCGDXR-STATUS-CD       TO                            
                         WS-TR-GCCCGDXR-STATUS-CD                               
                    MOVE WS-PRINT-LR              TO LOGICAL-RECORD-NAME        
                    MOVE STORE-LR                 TO WS-GAEDATSR-VERB           
                    CALL WS-GAEDATSR              USING WS-GAEDATSR-VERB        
                                                  WS-TERMED-RECORD              
                                                  ICBM                          
                    IF NOT LR-STATUS-OK                                         
                        DISPLAY 'ERROR WRITING TO PRINT FILE DLSO20'            
                        PERFORM 9999-ABEND                                      
                    END-IF                                                      
                                                                                
                 ELSE                                                           
                    ADD +1      TO WS-ACTIVATE-COUNTER                          
                 END-IF                                                         
              ELSE                                                              
                 MOVE 'UPDATE TGD'  TO AB-MSG1                                  
                 MOVE SQLCODE       TO AB-SQLCODE                               
                 MOVE SQLERRMC      TO AB-MSG2                                  
                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                        
                 PERFORM 9999-ABEND                                             
              END-IF                                                            
           END-IF.                                                              
                                                                                
      *********************************************************************     
      *  IF THE EXTRACT IS NOT TERMINATED, VALIDATE TGDFA, TGDADV, TPAA   *     
      *********************************************************************     
           IF GCCCGDXR-STATUS-CD NOT EQUAL 'T'                                  
              PERFORM 2120-UPDATE-TGDFA                                         
              PERFORM 2140-UPDATE-TGDADV                                        
              PERFORM 2160-UPDATE-TPAA                                          
              PERFORM 2190-UPDATE-TCL                                           
KL            PERFORM 2195-UPDATE-TPACLA                                        
              PERFORM VARYING WS-SUB FROM +1 BY +1                              
                        UNTIL WS-SUB > +4                                       
                 IF GCCCGDXR-ADVISOR-ID (WS-SUB) NOT EQUAL SPACES               
                    MOVE GCCCGDXR-ADVISOR-ID (WS-SUB) TO WS-ADVISOR-ID          
WB                  PERFORM 2180-UPDATE-TADVME                                  
WB               END-IF                                                         
WB            END-PERFORM                                                       
                                                                                
              ADD +1  TO WS-UPDATE-COUNTER                                      
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
       2120-UPDATE-TGDFA.                                                       
      *****************************************************************         
      *  ENSURE THAT IF EHCRPT/DSBRPT INDICATORS ARE SET THAT THE               
      *  GROUP/DIVISION HAS ACCESS IN TGDFA (AND VICE-VERSA).                   
      *  ALSO ENSURE THAT ADVRPT EXISTS (Y ACCESS IS THE DEFAULT)               
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2120-UPDATE-TGDFA'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
              SELECT   ACC_IND                                                  
                INTO  :DCLTGDFA.ACC-IND                                         
                FROM   TGDFA                                                    
               WHERE   GROUP_ID      = :GCCCGDXR-CONTRACT-ID                    
                 AND   DIV_ID        = :GCCCGDXR-DIVISION-ID                    
                 AND   SITE          = 'PA  '                                   
                 AND   FUNCTION_NAME = 'EHCRPT  '                               
           END-EXEC.                                                            
                                                                                
           IF SQLCODE EQUAL ZERO                                                
      ** EHCRPT EXISTS - MAKE SURE THE ACCESS IS THE CORRECT ONE                
              IF ACC-IND OF DCLTGDFA NOT EQUAL GCCCGDXR-EHCRPT-IND              
                 EXEC SQL                                                       
                      UPDATE TGDFA                                              
                         SET ACC_IND       = :GCCCGDXR-EHCRPT-IND               
                            ,CHN_USER_ID   = 'GCCPGDX3'                         
                            ,CHN_TS        =  CURRENT TIMESTAMP                 
                       WHERE GROUP_ID      = :GCCCGDXR-CONTRACT-ID              
                         AND DIV_ID        = :GCCCGDXR-DIVISION-ID              
                         AND SITE          = 'PA  '                             
                         AND FUNCTION_NAME = 'EHCRPT  '                         
                 END-EXEC                                                       
                 IF SQLCODE NOT EQUAL ZERO                                      
                    MOVE 'UPDATE TGDFA-EHC'   TO AB-MSG1                        
                    MOVE SQLCODE              TO AB-SQLCODE                     
                    MOVE SQLERRMC             TO AB-MSG2                        
                    INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                     
                    PERFORM 9999-ABEND                                          
                 END-IF                                                         
              END-IF                                                            
           ELSE IF SQLCODE = +100                                               
      ** EHCRPT DOESN'T EXIST - ONLY ADD IF ACCESS IS REQUIRED                  
              IF GCCCGDXR-EHCRPT-IND = 'Y'                                      
                 MOVE 'EHCRPT  '        TO FUNCTION-NAME                        
                 PERFORM 7100-INSERT-TGDFA                                      
              END-IF                                                            
           ELSE                                                                 
              MOVE 'SELECT TGDFA-EHC'   TO AB-MSG1                              
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF.                                                              
                                                                                
           EXEC SQL                                                             
              SELECT   ACC_IND                                                  
                INTO  :DCLTGDFA.ACC-IND                                         
                FROM   TGDFA                                                    
               WHERE   GROUP_ID      = :GCCCGDXR-CONTRACT-ID                    
                 AND   DIV_ID        = :GCCCGDXR-DIVISION-ID                    
                 AND   SITE          = 'PA  '                                   
                 AND   FUNCTION_NAME = 'DSBRPT  '                               
           END-EXEC.                                                            
                                                                                
           IF SQLCODE EQUAL ZERO                                                
      ** DSBRPT EXISTS - MAKE SURE THE ACCESS IS THE CORRECT ONE                
              IF ACC-IND OF DCLTGDFA NOT EQUAL GCCCGDXR-DSBRPT-IND              
                 EXEC SQL                                                       
                      UPDATE TGDFA                                              
                         SET ACC_IND       = :GCCCGDXR-DSBRPT-IND               
                            ,CHN_USER_ID   = 'GCCPGDX3'                         
                            ,CHN_TS        =  CURRENT TIMESTAMP                 
                       WHERE GROUP_ID      = :GCCCGDXR-CONTRACT-ID              
                         AND DIV_ID        = :GCCCGDXR-DIVISION-ID              
                         AND SITE          = 'PA  '                             
                         AND FUNCTION_NAME = 'DSBRPT  '                         
                 END-EXEC                                                       
                 IF SQLCODE NOT EQUAL ZERO                                      
                    MOVE 'UPDATE TGDFA-DSB'   TO AB-MSG1                        
                    MOVE SQLCODE              TO AB-SQLCODE                     
                    MOVE SQLERRMC             TO AB-MSG2                        
                    INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                     
                    PERFORM 9999-ABEND                                          
                 END-IF                                                         
              END-IF                                                            
           ELSE IF SQLCODE = +100                                               
      ** DSBRPT DOESN'T EXIST - ONLY ADD IF ACCESS IS REQUIRED                  
              IF GCCCGDXR-DSBRPT-IND = 'Y'                                      
                 MOVE 'DSBRPT  '        TO FUNCTION-NAME                        
                 PERFORM 7100-INSERT-TGDFA                                      
              END-IF                                                            
           ELSE                                                                 
              MOVE 'SELECT TGDFA-DSB'   TO AB-MSG1                              
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF.                                                              
                                                                                
           EXEC SQL                                                             
              SELECT   ACC_IND                                                  
                INTO  :DCLTGDFA.ACC-IND                                         
                FROM   TGDFA                                                    
               WHERE   GROUP_ID      = :GCCCGDXR-CONTRACT-ID                    
                 AND   DIV_ID        = :GCCCGDXR-DIVISION-ID                    
                 AND   SITE          = 'PA  '                                   
                 AND   FUNCTION_NAME = 'ADVRPT  '                               
           END-EXEC.                                                            
                                                                                
           IF SQLCODE EQUAL ZERO                                                
      ** ADVRPT EXISTS - DO NOTHING                                             
              CONTINUE                                                          
           ELSE IF SQLCODE = +100                                               
      ** ADVRPT DOESN'T EXIST - ADD WITH YES AS DEFAULT ACCESS                  
              MOVE 'ADVRPT  '        TO FUNCTION-NAME                           
              PERFORM 7100-INSERT-TGDFA                                         
           ELSE                                                                 
              MOVE 'SELECT TGDFA-ADV'   TO AB-MSG1                              
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
       2140-UPDATE-TGDADV.                                                      
      *****************************************************************         
      *   ENSURE CORRECT LIST OF ADVISORS IS IN THE TGDADV TABLE                
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2140-UPDATE-TGDADV'     TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
      *****************************************************************         
      *   DELETE ANY ROWS IN TPAA THAT AREN'T IN THE CURRENT LIST               
      *   WE DELETE TPAA BEFORE TGDADV BECAUSE THE FIRST DELETE NEEDS           
      *   TO FIND RECORDS IN TGDADV BEFORE THEY ARE DELETED FROM TGDADV         
      *****************************************************************         
           EXEC SQL                                                             
              DELETE FROM TPAA                                                  
               WHERE GROUP_ID    = :GCCCGDXR-CONTRACT-ID                        
                 AND DIV_ID      = :GCCCGDXR-DIVISION-ID                        
                 AND CUST_ID IN                                                 
                   (SELECT A.CUST_ID                                            
                      FROM TADVSR A, TGDADV B                                   
                         WHERE B.ADVISOR_ID = A.ADVISOR_ID                      
                           AND B.GROUP_ID   = :GCCCGDXR-CONTRACT-ID             
                           AND B.DIV_ID     = :GCCCGDXR-DIVISION-ID             
                           AND B.ADVISOR_ID NOT IN (                            
                                :GCCCGDXR-ADVISOR-ID1,                          
                                :GCCCGDXR-ADVISOR-ID2,                          
                                :GCCCGDXR-ADVISOR-ID3,                          
                                :GCCCGDXR-ADVISOR-ID4)                          
                   )                                                            
           END-EXEC                                                             
                                                                                
           IF SQLCODE NOT EQUAL ZERO                                            
                  AND SQLCODE NOT EQUAL +100                                    
              MOVE 'DELETE TPAA   '     TO AB-MSG1                              
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF                                                               
KL    *****************************************************************         
KL    *   DELETE ANY ROWS IN TPACLA THAT                                        
KL    * 1) THE CONTRACT ID IS THE CLIENT NUMBER IN TPACLA                       
KL    * 2) THE CUST ID IS NOT IN THE CURRENT ADVISOR LIST                       
KL    *****************************************************************         
KL         EXEC SQL                                                             
KL            DELETE FROM TPACLA MAIN                                           
KL            WHERE MAIN.CLIENT_NUM = :GCCCGDXR-CLIENT-NUM                      
KL                  AND LOC_NUM     = :GCCCGDXR-LOCATION-NUM                    
KL             AND CUST_ID IN                                                   
KL                 (SELECT A.CUST_ID                                            
KL                    FROM TADVSR A, TGDADV B                                   
KL                       WHERE B.ADVISOR_ID = A.ADVISOR_ID                      
KL                         AND B.GROUP_ID   = :GCCCGDXR-CONTRACT-ID             
KL                         AND B.DIV_ID     = :GCCCGDXR-DIVISION-ID             
KL                         AND B.ADVISOR_ID NOT IN (                            
KL                              :GCCCGDXR-ADVISOR-ID1,                          
KL                              :GCCCGDXR-ADVISOR-ID2,                          
KL                              :GCCCGDXR-ADVISOR-ID3,                          
KL                              :GCCCGDXR-ADVISOR-ID4)                          
KL                 )                                                            
KL         END-EXEC                                                             
KL         IF SQLCODE NOT EQUAL ZERO                                            
KL                AND SQLCODE NOT EQUAL +100                                    
KL            MOVE 'DELETE TPACLA '     TO AB-MSG1                              
KL            MOVE SQLCODE              TO AB-SQLCODE                           
KL            MOVE SQLERRMC             TO AB-MSG2                              
KL            INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
KL            PERFORM 9999-ABEND                                                
KL         END-IF                                                               
      *****************************************************************         
      *   DELETE ANY ROWS IN TGADV THAT AREN'T IN THE CURRENT LIST              
      *****************************************************************         
           EXEC SQL                                                             
              DELETE FROM TGDADV                                                
               WHERE GROUP_ID    = :GCCCGDXR-CONTRACT-ID                        
                 AND DIV_ID      = :GCCCGDXR-DIVISION-ID                        
                 AND ADVISOR_ID NOT IN (                                        
                      :GCCCGDXR-ADVISOR-ID1, :GCCCGDXR-ADVISOR-ID2,             
                      :GCCCGDXR-ADVISOR-ID3, :GCCCGDXR-ADVISOR-ID4              
                      )                                                         
           END-EXEC.                                                            
                                                                                
           IF SQLCODE = ZERO OR +100                                            
              CONTINUE                                                          
           ELSE                                                                 
              MOVE 'DELETE TGDADV '     TO AB-MSG1                              
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF.                                                              
                                                                                
                                                                                
      *****************************************************************         
      *   INSERT ANY ROWS THAT DO NOT EXIST                                     
      *****************************************************************         
           PERFORM VARYING WS-SUB FROM +1 BY +1                                 
                     UNTIL WS-SUB > +4                                          
              IF GCCCGDXR-ADVISOR-ID (WS-SUB) NOT EQUAL SPACES                  
                   MOVE GCCCGDXR-ADVISOR-ID (WS-SUB) TO WS-ADVISOR-ID           
                   EXEC SQL                                                     
                        SELECT '1' INTO :WS-TEMP-CHAR                           
                          FROM TGDADV                                           
                         WHERE GROUP_ID   = :GCCCGDXR-CONTRACT-ID               
                           AND DIV_ID     = :GCCCGDXR-DIVISION-ID               
                           AND ADVISOR_ID = :WS-ADVISOR-ID                      
                   END-EXEC                                                     
                   EVALUATE SQLCODE                                             
                       WHEN ZERO                                                
                          CONTINUE                                              
                       WHEN +100                                                
                          PERFORM 7200-INSERT-TGDADV                            
                       WHEN OTHER                                               
                          MOVE 'SELECT TGDADV '     TO AB-MSG1                  
                          MOVE SQLCODE              TO AB-SQLCODE               
                          MOVE SQLERRMC             TO AB-MSG2                  
                          INSPECT AB-MSG2 CONVERTING X'FF' TO '-'               
                          PERFORM 9999-ABEND                                    
                   END-EVALUATE                                                 
              END-IF                                                            
           END-PERFORM.                                                         
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
                                                                                
       2160-UPDATE-TPAA.                                                        
      *****************************************************************         
      *   ENSURE CORRECT LIST OF ADVISORS IS IN THE TPAA TABLE                  
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2160-UPDATE-TPAA'     TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
      *****************************************************************         
      *   INSERT ANY ROWS THAT DO NOT EXIST IN TPAA                             
      *****************************************************************         
           PERFORM VARYING WS-SUB FROM +1 BY +1                                 
                     UNTIL WS-SUB > +4                                          
              IF GCCCGDXR-ADVISOR-ID (WS-SUB) NOT EQUAL SPACES                  
                   MOVE GCCCGDXR-ADVISOR-ID (WS-SUB) TO WS-ADVISOR-ID           
                   PERFORM 7300-INSERT-TPAA                                     
              END-IF                                                            
           END-PERFORM.                                                         
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
WB     2180-UPDATE-TADVME.                                                      
WB    *****************************************************************         
WB    *   ENSURE CORRECT LIST OF DIVISIONS IS IN THE TPAA TABLE                 
WB    *   FOR ALL EMPLOYEES IN TADVME TABLE                                     
WB    *****************************************************************         
WB                                                                              
WB         ADD 1 TO LVL.                                                        
WB         MOVE '2180-UPDATE-TADVME'   TO AB-PARAGRAPH-NAME (LVL).              
WB                                                                              
WB         PERFORM 6100-OPEN-TADVME.                                            
WB                                                                              
WB         SUBTRACT 1 FROM LVL.                                                 
WB                                                                              
                                                                                
WB2    2190-UPDATE-TCL.                                                         
WB2   *****************************************************************         
WB2   *   ENSURE VO LOCATION AND CLIENT-NUM AND STAT-CD ARE UPDATED             
WB2   *****************************************************************         
WB2                                                                             
WB2        ADD 1 TO LVL.                                                        
WB2        MOVE '2190-UPDATE-TCL'   TO AB-PARAGRAPH-NAME (LVL).                 
WB2                                                                             
WB2        IF GCCCGDXR-CLIENT-NUM >= ZERO AND                                   
WB2           GCCCGDXR-LOCATION-NUM >= ZERO THEN                                
WB2           PERFORM 8100-OPEN-TCL                                             
WB2        END-IF.                                                              
WB2                                                                             
WB2        SUBTRACT 1 FROM LVL.                                                 
WB2                                                                             
KL     2195-UPDATE-TPACLA.                                                      
KL                                                                              
KL    *****************************************************************         
KL    *   GENERATE THE LOCATION AUTHORITY TAB                                   
KL    *****************************************************************         
KL         ADD 1 TO LVL.                                                        
KL         MOVE '2195-UPDATE-TPACLA'  TO AB-PARAGRAPH-NAME (LVL).               
KL                                                                              
KL    *****************************************************************         
KL    *   INSERT ANY ROWS THAT DO NOT EXIST IN TPAA                             
KL    *****************************************************************         
KL         PERFORM VARYING WS-SUB FROM +1 BY +1                                 
KL                   UNTIL WS-SUB > +4                                          
KL            IF GCCCGDXR-ADVISOR-ID (WS-SUB) NOT EQUAL SPACES                  
KL                 MOVE GCCCGDXR-ADVISOR-ID (WS-SUB) TO WS-ADVISOR-ID           
KL                 PERFORM 8300-OPEN-TPACLA                                     
KL            END-IF                                                            
KL         END-PERFORM.                                                         
KL         SUBTRACT 1 FROM LVL.                                                 
       2200-ADD-CPD.                                                            
      *****************************************************************         
      *  ADD NEW GROUP/DIVISION TO CPD                                          
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2200-ADD-CPD'        TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
      *****************************************************************         
      *  GENERATE A UNIQUE ACC_PM_REGIS_CD                                      
      *****************************************************************         
           MOVE +0 TO SQLCODE.                                                  
           PERFORM UNTIL SQLCODE = +100                                         
              ADD +1  TO WS-WORK-REGIS-CD                                       
              EXEC SQL                                                          
                 SELECT '1'                                                     
                   INTO :WS-TEMP-CHAR                                           
                   FROM  TGD                                                    
                  WHERE  ACC_PM_REGIS_CD = :WS-WORK-REGIS-CD                    
              END-EXEC                                                          
              IF SQLCODE = ZERO OR +100 OR -811                                 
                 CONTINUE                                                       
              ELSE                                                              
                 MOVE 'SELECT TGD'         TO AB-MSG1                           
                 MOVE SQLCODE              TO AB-SQLCODE                        
                 MOVE SQLERRMC             TO AB-MSG2                           
                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                        
                 PERFORM 9999-ABEND                                             
              END-IF                                                            
           END-PERFORM.                                                         
                                                                                
      *****************************************************************         
      *  GENERATE THE MULTI-GROUP-IND                                           
      *****************************************************************         
           IF GCCCGDXR-REGION-CD EQUAL 'G'                                      
              MOVE 'Y' TO MULTI-GROUP-IND                                       
           ELSE                                                                 
              MOVE 'N' TO MULTI-GROUP-IND                                       
           END-IF.                                                              
                                                                                
           PERFORM 7000-INSERT-TGD.                                             
                                                                                
           IF GCCCGDXR-EHCRPT-IND EQUAL 'Y'                                     
              MOVE 'EHCRPT  '         TO FUNCTION-NAME                          
              PERFORM 7100-INSERT-TGDFA                                         
           END-IF.                                                              
                                                                                
           IF GCCCGDXR-DSBRPT-IND EQUAL 'Y'                                     
              MOVE 'DSBRPT  '         TO FUNCTION-NAME                          
              PERFORM 7100-INSERT-TGDFA                                         
           END-IF.                                                              
                                                                                
           MOVE 'ADVRPT  '            TO FUNCTION-NAME.                         
           PERFORM 7100-INSERT-TGDFA.                                           
                                                                                
           PERFORM VARYING WS-SUB FROM +1 BY +1                                 
                     UNTIL WS-SUB > +4                                          
              IF GCCCGDXR-ADVISOR-ID (WS-SUB) NOT EQUAL SPACES                  
                 MOVE GCCCGDXR-ADVISOR-ID (WS-SUB) TO WS-ADVISOR-ID             
                 PERFORM 7200-INSERT-TGDADV                                     
                 PERFORM 7300-INSERT-TPAA                                       
WB               PERFORM 2180-UPDATE-TADVME                                     
              END-IF                                                            
           END-PERFORM.                                                         
                                                                                
           PERFORM 2190-UPDATE-TCL.                                             
KL         PERFORM 2195-UPDATE-TPACLA.                                          
                                                                                
           ADD +1 TO WS-ADDED-COUNTER.                                          
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       3000-COMPLETION.                                                         
      *****************************************************************         
      * CLOSE FILES                                                             
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '3000-COMPLETION'        TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
      * CLOSE DATA SERVER                                                       
                                                                                
           MOVE ' '                       TO LOGICAL-RECORD-NAME.               
           MOVE FINISH-LR                 TO WS-GAEDATSR-VERB.                  
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                                  LOGICAL-RECORD-NAME                           
                                  ICBM.                                         
                                                                                
           DISPLAY 'RUN STATISTICS'.                                            
           DISPLAY '*************************************************'          
           DISPLAY 'RECORDS READ:          ', WS-INPUT-COUNTER.                 
           DISPLAY 'GROUP/DIV ADDED:       ', WS-ADDED-COUNTER                  
           DISPLAY 'GROUP/DIV TERMED:      ', WS-TERMED-COUNTER                 
           DISPLAY 'GROUP/DIV MADE ACTIVE: ', WS-ACTIVATE-COUNTER               
           DISPLAY 'GROUP/DIV UPDATED:     ', WS-UPDATE-COUNTER                 
           DISPLAY '*************************************************'          
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
       7000-INSERT-TGD.                                                         
      *****************************************************************         
      * - INSERT OF TGD ROW                                                     
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7000-INSERT-TGD'        TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
              INSERT INTO TGD                                                   
                   ( GROUP_ID                                                   
                   , DIV_ID                                                     
                   , ACC_PM_REGIS_CD                                            
                   , SPONSOR_NAME                                               
                   , SPONSOR_NAME_CAPS                                          
                   , REG_CD                                                     
                   , CONTRACT_STAT_CD                                           
                   , CONTRIB_IND                                                
                   , AUTH_PMSS_IND                                              
                   , AUTH_PA_IND                                                
                   , AUTH_PM_ENROL_IND                                          
                   , BUS_SEG_CD                                                 
                   , REG_GROUP_OFFICE                                           
                   , CHN_USER_ID                                                
                   , CHN_TS                                                     
                   , MULTI_GROUP_IND                                            
                   , TAT                                                        
                   , CLM_REG_CD )                                               
              VALUES                                                            
                   ( :GCCCGDXR-CONTRACT-ID                                      
                   , :GCCCGDXR-DIVISION-ID                                      
                   , :WS-WORK-REGIS-CD                                          
                   , :GCCCGDXR-SPONSOR-NAME                                     
                   , :GCCCGDXR-SPONSOR-NAME                                     
                   , :GCCCGDXR-REGION-CD                                        
                   , :GCCCGDXR-STATUS-CD                                        
                   , ' '                                                        
                   , 'N'                                                        
                   , 'Y'                                                        
                   , 'N'                                                        
                   , :GCCCGDXR-BUS-SEG-CD                                       
                   , :GCCCGDXR-REG-GROUP-OFFICE                                 
                   , 'GCCPGDX3'                                                 
                   ,  CURRENT TIMESTAMP                                         
                   , :MULTI-GROUP-IND                                           
                   , 'S '                                                       
                   , ' ' )                                                      
           END-EXEC.                                                            
                                                                                
           IF SQLCODE NOT EQUAL ZERO                                            
              MOVE SPACES               TO AB-MSG1                              
              STRING 'INSERT TGD - ' GCCCGDXR-CONTRACT-ID                       
                                '  ' GCCCGDXR-DIVISION-ID                       
                  DELIMITED BY SIZE                                             
                  INTO AB-MSG1                                                  
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
       7100-INSERT-TGDFA.                                                       
      *****************************************************************         
      * - INSERT OF TGDFA ROW                                                   
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7100-INSERT-TGDFA'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
              INSERT INTO TGDFA                                                 
                   ( GROUP_ID                                                   
                   , DIV_ID                                                     
                   , SITE                                                       
                   , FUNCTION_NAME                                              
                   , ACC_IND                                                    
                   , CHN_USER_ID                                                
                   , CHN_TS                                                     
JAK                , SUBMIT_TYP_CD)                                             
              VALUES                                                            
                   ( :GCCCGDXR-CONTRACT-ID                                      
                   , :GCCCGDXR-DIVISION-ID                                      
                   , 'PA  '                                                     
                   , :FUNCTION-NAME                                             
                   , 'Y'                                                        
                   , 'GCCPGDX3'                                                 
                   ,  CURRENT TIMESTAMP                                         
JAK                , ' ')                                                       
           END-EXEC.                                                            
           IF SQLCODE NOT EQUAL ZERO                                            
              MOVE SPACES               TO AB-MSG1                              
              STRING 'INSERT TGDFA - ' FUNCTION-NAME                            
                  DELIMITED BY SIZE                                             
                  INTO AB-MSG1                                                  
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF                                                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
       7200-INSERT-TGDADV.                                                      
      *****************************************************************         
      * - INSERT OF TGDADV ROW                                                  
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7200-INSERT-TGDADV'     TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
              INSERT INTO TGDADV                                                
                   ( GROUP_ID                                                   
                   , DIV_ID                                                     
                   , ADVISOR_ID                                                 
                   , CHN_TS)                                                    
              VALUES                                                            
                   ( :GCCCGDXR-CONTRACT-ID                                      
                   , :GCCCGDXR-DIVISION-ID                                      
                   , :WS-ADVISOR-ID                                             
                   ,  CURRENT TIMESTAMP)                                        
           END-EXEC.                                                            
           IF SQLCODE NOT EQUAL ZERO                                            
              MOVE SPACES               TO AB-MSG1                              
              STRING 'INSERT TGDADV - ' WS-ADVISOR-ID                           
                                '  ' GCCCGDXR-CONTRACT-ID                       
                                '  ' GCCCGDXR-DIVISION-ID                       
                  DELIMITED BY SIZE                                             
                  INTO AB-MSG1                                                  
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF                                                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
WB     6100-OPEN-TADVME.                                                        
WB    *****************************************************************         
WB    * - OPEN TADVME TO ADD IN TPAA RECORD                                     
WB    *****************************************************************         
WB                                                                              
WB         ADD 1 TO LVL.                                                        
WB         MOVE '6100-OPEN-TADVME'     TO AB-PARAGRAPH-NAME (LVL).              
WB                                                                              
WB         EXEC SQL                                                             
WB             OPEN TADVMECUR                                                   
WB         END-EXEC.                                                            
WB                                                                              
WB         IF  SQLCODE NOT EQUAL +0 AND SQLCODE NOT EQUAL +100                  
WB             MOVE 'OPEN TADVME   '     TO AB-MSG1                             
WB             MOVE SQLCODE              TO AB-SQLCODE                          
WB             MOVE SQLERRMC             TO AB-MSG2                             
WB             INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
WB             PERFORM 9999-ABEND                                               
WB         END-IF                                                               
WB                                                                              
WB         SET WS-TADVME-START TO TRUE.                                         
WB         PERFORM 6200-READ-TADVME UNTIL WS-TADVME-END.                        
WB                                                                              
WB         EXEC SQL                                                             
WB             CLOSE TADVMECUR                                                  
WB         END-EXEC.                                                            
WB                                                                              
WB         IF  SQLCODE NOT EQUAL +0                                             
WB             MOVE 'CLOSE TADVME  '     TO AB-MSG1                             
WB             MOVE SQLCODE              TO AB-SQLCODE                          
WB             MOVE SQLERRMC             TO AB-MSG2                             
WB             INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
WB             PERFORM 9999-ABEND                                               
WB         END-IF                                                               
WB                                                                              
WB         SUBTRACT 1 FROM LVL.                                                 
WB                                                                              
WB                                                                              
WB     6200-READ-TADVME.                                                        
WB    *****************************************************************         
WB    *   READ ALL TADVME RECORDS FOR THIS ADVISOR AND ADD A TPAA               
WB    *   RECORD FOR EACH TADVME RECORD.                                        
WB    *****************************************************************         
WB                                                                              
WB         ADD 1 TO LVL.                                                        
WB         MOVE '6200-READ-TADVME'     TO AB-PARAGRAPH-NAME (LVL).              
WB                                                                              
WB         EXEC SQL                                                             
WB             FETCH TADVMECUR                                                  
WB             INTO  :WS-ADVME-ID                                               
WB         END-EXEC.                                                            
WB                                                                              
WB         EVALUATE SQLCODE                                                     
WB             WHEN ZERO                                                        
WB                 PERFORM 6300-CHECK-AUTH                                      
WB             WHEN +100                                                        
WB                 SET WS-TADVME-END         TO TRUE                            
WB             WHEN OTHER                                                       
WB                 MOVE 'FETCH TADVSR  '     TO AB-MSG1                         
WB                 MOVE SQLCODE              TO AB-SQLCODE                      
WB                 MOVE SQLERRMC             TO AB-MSG2                         
WB                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
WB                 PERFORM 9999-ABEND                                           
WB         END-EVALUATE.                                                        
WB                                                                              
WB         SUBTRACT 1 FROM LVL.                                                 
WB                                                                              
WB                                                                              
WB     6300-CHECK-AUTH.                                                         
WB    *****************************************************************         
WB    * - OPEN TADVME TO ADD IN TPAA RECORD                                     
WB    *****************************************************************         
WB                                                                              
WB         ADD 1 TO LVL.                                                        
WB         MOVE '6300-CHECK-AUTH'     TO AB-PARAGRAPH-NAME (LVL).               
WB                                                                              
WB         EXEC SQL                                                             
WB             OPEN TAUTHCUR                                                    
WB         END-EXEC.                                                            
WB                                                                              
WB         IF  SQLCODE NOT EQUAL +0 AND SQLCODE NOT EQUAL +100                  
WB             MOVE 'OPEN TAUTHCUR '     TO AB-MSG1                             
WB             MOVE SQLCODE              TO AB-SQLCODE                          
WB             MOVE SQLERRMC             TO AB-MSG2                             
WB             INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
WB             PERFORM 9999-ABEND                                               
WB         END-IF                                                               
WB                                                                              
WB    *    SET WS-TAUTH-START TO TRUE.                                          
WB                                                                              
WB         EXEC SQL                                                             
WB             FETCH TAUTHCUR                                                   
WB             INTO  :WS-GROUP-ID                                               
WB         END-EXEC.                                                            
WB                                                                              
WB         EVALUATE SQLCODE                                                     
WB             WHEN ZERO                                                        
WB                 PERFORM 6400-READ-EMP-CUSTID                                 
WB             WHEN +100                                                        
WB                 CONTINUE                                                     
WB             WHEN OTHER                                                       
WB                 MOVE 'FETCH TADVSR  '     TO AB-MSG1                         
WB                 MOVE SQLCODE              TO AB-SQLCODE                      
WB                 MOVE SQLERRMC             TO AB-MSG2                         
WB                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
WB                 PERFORM 9999-ABEND                                           
WB         END-EVALUATE.                                                        
WB                                                                              
WB         EXEC SQL                                                             
WB             CLOSE TAUTHCUR                                                   
WB         END-EXEC.                                                            
WB                                                                              
WB         IF  SQLCODE NOT EQUAL +0                                             
WB             MOVE 'CLOSE TADVME  '     TO AB-MSG1                             
WB             MOVE SQLCODE              TO AB-SQLCODE                          
WB             MOVE SQLERRMC             TO AB-MSG2                             
WB             INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
WB             PERFORM 9999-ABEND                                               
WB         END-IF                                                               
WB                                                                              
WB         SUBTRACT 1 FROM LVL.                                                 
                                                                                
WB     6400-READ-EMP-CUSTID.                                                    
WB    *****************************************************************         
WB    * - OPEN TADVME TO ADD IN TPAA RECORD                                     
WB    *****************************************************************         
WB                                                                              
WB         ADD 1 TO LVL.                                                        
WB         MOVE '6400-READ-EMP-CUSTID' TO AB-PARAGRAPH-NAME (LVL).              
WB                                                                              
WB         EXEC SQL                                                             
WB             OPEN TEMPCUR                                                     
WB         END-EXEC.                                                            
WB                                                                              
WB         IF  SQLCODE NOT EQUAL +0 AND SQLCODE NOT EQUAL +100                  
WB             MOVE 'OPEN TEMPCUR '     TO AB-MSG1                              
WB             MOVE SQLCODE              TO AB-SQLCODE                          
WB             MOVE SQLERRMC             TO AB-MSG2                             
WB             INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
WB             PERFORM 9999-ABEND                                               
WB         END-IF                                                               
WB                                                                              
WB         EXEC SQL                                                             
WB             FETCH TEMPCUR                                                    
WB             INTO  :WS-CUST-ID                                                
WB         END-EXEC.                                                            
WB                                                                              
WB         EVALUATE SQLCODE                                                     
WB             WHEN ZERO                                                        
WB                 PERFORM 7500-ADD-TPAA-REC                                    
WB             WHEN +100                                                        
WB                 CONTINUE                                                     
WB             WHEN OTHER                                                       
WB                 MOVE 'FETCH TEMPCUR '     TO AB-MSG1                         
WB                 MOVE SQLCODE              TO AB-SQLCODE                      
WB                 MOVE SQLERRMC             TO AB-MSG2                         
WB                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
WB                 PERFORM 9999-ABEND                                           
WB         END-EVALUATE.                                                        
WB                                                                              
WB         EXEC SQL                                                             
WB             CLOSE TEMPCUR                                                    
WB         END-EXEC.                                                            
WB                                                                              
WB         IF  SQLCODE NOT EQUAL +0                                             
WB             MOVE 'CLOSE TEMPCUR '     TO AB-MSG1                             
WB             MOVE SQLCODE              TO AB-SQLCODE                          
WB             MOVE SQLERRMC             TO AB-MSG2                             
WB             INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
WB             PERFORM 9999-ABEND                                               
WB         END-IF                                                               
WB                                                                              
WB         SUBTRACT 1 FROM LVL.                                                 
                                                                                
WB2    8100-OPEN-TCL.                                                           
WB2   *****************************************************************         
WB2   * - OPEN TCL TO ADD IN TCL RECORD                                         
WB2   *****************************************************************         
WB2                                                                             
WB2        ADD 1 TO LVL.                                                        
WB2        MOVE '8100-OPEN-TCL'     TO AB-PARAGRAPH-NAME (LVL).                 
WB2                                                                             
WB2        EXEC SQL                                                             
WB2            OPEN TCLCUR                                                      
WB2        END-EXEC.                                                            
WB2                                                                             
WB2        IF  SQLCODE NOT EQUAL +0 AND SQLCODE NOT EQUAL +100                  
WB2            MOVE 'OPEN TCL      '     TO AB-MSG1                             
WB2            MOVE SQLCODE              TO AB-SQLCODE                          
WB2            MOVE SQLERRMC             TO AB-MSG2                             
WB2            INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
WB2            PERFORM 9999-ABEND                                               
WB2        END-IF                                                               
WB2                                                                             
WB2   *    SET WS-TCL-START TO TRUE.                                            
WB2        PERFORM 8200-READ-TCL.                                               
WB2                                                                             
WB2        EXEC SQL                                                             
WB2            CLOSE TCLCUR                                                     
WB2        END-EXEC.                                                            
WB2                                                                             
WB2        IF  SQLCODE NOT EQUAL +0                                             
WB2            MOVE 'CLOSE TCL     '     TO AB-MSG1                             
WB2            MOVE SQLCODE              TO AB-SQLCODE                          
WB2            MOVE SQLERRMC             TO AB-MSG2                             
WB2            INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
WB2            PERFORM 9999-ABEND                                               
WB2        END-IF                                                               
WB2                                                                             
WB2        SUBTRACT 1 FROM LVL.                                                 
WB2                                                                             
WB2                                                                             
WB2    8200-READ-TCL.                                                           
WB2   *****************************************************************         
WB2   *   LOOK FOR A TCL RECORD IF IT EXISTS - CHECK LOC-STAT-CD ELSE           
WB2   *   INSERT RECORD                                                         
WB2   *****************************************************************         
WB2                                                                             
WB2        ADD 1 TO LVL.                                                        
WB2        MOVE '8200-READ-TCL '     TO AB-PARAGRAPH-NAME (LVL).                
WB2                                                                             
WB2        MOVE SPACES TO WS-LOC-STAT-CD.                                       
WB2                                                                             
WB2        EXEC SQL                                                             
WB2            FETCH TCLCUR                                                     
WB2            INTO  :WS-LOC-STAT-CD                                            
WB2        END-EXEC.                                                            
WB2                                                                             
WB2        EVALUATE SQLCODE                                                     
WB2            WHEN ZERO                                                        
WB2               IF WS-LOC-STAT-CD NOT EQUAL GCCCGDXR-LOC-STATUS-CD            
WB2                  PERFORM 8210-UPDATE-TCL                                    
WB2               END-IF                                                        
WB2            WHEN +100                                                        
WB2               PERFORM 8220-INSERT-TCL                                       
WB2            WHEN OTHER                                                       
WB2                MOVE 'FETCH TCL     '     TO AB-MSG1                         
WB2                MOVE SQLCODE              TO AB-SQLCODE                      
WB2                MOVE SQLERRMC             TO AB-MSG2                         
WB2                INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
WB2                PERFORM 9999-ABEND                                           
WB2        END-EVALUATE.                                                        
WB2                                                                             
WB2        SUBTRACT 1 FROM LVL.                                                 
WB2                                                                             
WB2    8210-UPDATE-TCL.                                                         
WB2   *****************************************************************         
WB2   * UPDATE THE RELEVANT RECORDS ON TCL TABLE                                
WB2   *****************************************************************         
WB2        ADD 1 TO LVL.                                                        
WB2        MOVE '8210-UPDATE-TCL'        TO AB-PARAGRAPH-NAME (LVL).            
WB2                                                                             
WB2           EXEC SQL                                                          
WB2              UPDATE TCL                                                     
WB2                 SET LOC_STAT_CD      = :GCCCGDXR-LOC-STATUS-CD              
WB2                    ,CHN_USER_ID      = 'GCCPGDX3'                           
WB2                    ,CHN_TS           =  CURRENT TIMESTAMP                   
WB2               WHERE CLIENT_NUM       = :GCCCGDXR-CLIENT-NUM                 
                    AND LOC_NUM          = :GCCCGDXR-LOCATION-NUM               
              END-EXEC.                                                         
              IF SQLCODE NOT EQUAL ZERO                                         
                                                                                
                 MOVE 'UPDATE TCL'  TO AB-MSG1                                  
                 MOVE SQLCODE       TO AB-SQLCODE                               
                 MOVE SQLERRMC      TO AB-MSG2                                  
                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                        
                 PERFORM 9999-ABEND                                             
              END-IF.                                                           
                                                                                
BC         SUBTRACT 1 FROM LVL.                                                 
                                                                                
WB2    8220-INSERT-TCL.                                                         
WB2   *****************************************************************         
      * - INSERT OF TCL ROW                                                     
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '8220-INSERT-TCL'     TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           EXEC SQL                                                             
              INSERT INTO TCL                                                   
                   ( CLIENT_NUM                                                 
                   , LOC_NUM                                                    
                   , LOC_STAT_CD                                                
                   , CHN_USER_ID                                                
                   , CHN_TS)                                                    
              VALUES                                                            
                   ( :GCCCGDXR-CLIENT-NUM                                       
                   , :GCCCGDXR-LOCATION-NUM                                     
                   , :GCCCGDXR-LOC-STATUS-CD                                    
                   ,  'GCCPGDX3'                                                
                   ,  CURRENT TIMESTAMP)                                        
           END-EXEC.                                                            
           IF SQLCODE NOT EQUAL ZERO                                            
              MOVE SPACES               TO AB-MSG1                              
              STRING 'INSERT TCL    - ' GCCCGDXR-CLIENT-NUM                     
                                '  ' GCCCGDXR-LOCATION-NUM                      
                                '  ' GCCCGDXR-LOC-STATUS-CD                     
                  DELIMITED BY SIZE                                             
                  INTO AB-MSG1                                                  
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF                                                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       7300-INSERT-TPAA.                                                        
      *****************************************************************         
      * - INSERT OF TPAA ROW                                                    
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7300-INSERT-TPAA'     TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
           EXEC SQL                                                             
               OPEN TADVSRCUR                                                   
           END-EXEC.                                                            
                                                                                
           IF  SQLCODE NOT EQUAL +0 AND SQLCODE NOT EQUAL +100                  
               MOVE 'OPEN TADVSR   '     TO AB-MSG1                             
               MOVE SQLCODE              TO AB-SQLCODE                          
               MOVE SQLERRMC             TO AB-MSG2                             
               INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
               PERFORM 9999-ABEND                                               
           END-IF                                                               
                                                                                
           SET WS-TADVSR-START TO TRUE.                                         
           PERFORM 7400-READ-TADVSR UNTIL WS-TADVSR-END.                        
                                                                                
           EXEC SQL                                                             
               CLOSE TADVSRCUR                                                  
           END-EXEC.                                                            
                                                                                
           IF  SQLCODE NOT EQUAL +0                                             
               MOVE 'CLOSE TADVSR  '     TO AB-MSG1                             
               MOVE SQLCODE              TO AB-SQLCODE                          
               MOVE SQLERRMC             TO AB-MSG2                             
               INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
               PERFORM 9999-ABEND                                               
           END-IF                                                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
       7400-READ-TADVSR.                                                        
      *****************************************************************         
      *   READ ALL TADVSR RECORDS FOR THIS ADVISOR AND ADD A TPAA               
      *   RECORD FOR EACH TADVSR RECORD.                                        
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7400-READ-TADVSR'     TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
           EXEC SQL                                                             
               FETCH TADVSRCUR                                                  
               INTO  :WS-CUST-ID                                                
           END-EXEC.                                                            
                                                                                
           EVALUATE SQLCODE                                                     
               WHEN ZERO                                                        
                   PERFORM 7500-ADD-TPAA-REC                                    
               WHEN +100                                                        
                   SET WS-TADVSR-END         TO TRUE                            
               WHEN OTHER                                                       
                   MOVE 'FETCH TADVSR  '     TO AB-MSG1                         
                   MOVE SQLCODE              TO AB-SQLCODE                      
                   MOVE SQLERRMC             TO AB-MSG2                         
                   INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
                   PERFORM 9999-ABEND                                           
           END-EVALUATE.                                                        
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
       7500-ADD-TPAA-REC.                                                       
      *****************************************************************         
      *  INSERT OF TPAA ROW                                                     
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7500-ADD-TPAA-REC'     TO AB-PARAGRAPH-NAME (LVL).             
                                                                                
           IF GCCCGDXR-REGION-CD EQUAL 'G'                                      
              MOVE 'N' TO WS-BILL-PRSNT-IND                                     
              MOVE 'N' TO WS-PAY-HIST-IND                                       
           ELSE                                                                 
              MOVE 'Y' TO WS-BILL-PRSNT-IND                                     
              MOVE 'Y' TO WS-PAY-HIST-IND                                       
           END-IF.                                                              
                                                                                
           EXEC SQL                                                             
               INSERT INTO TPAA                                                 
               ( CUST_ID                                                        
               , GROUP_ID                                                       
               , DIV_ID                                                         
               , PA_STAT_CD                                                     
               , CHN_USER_ID                                                    
               , CHN_TS                                                         
               , BILL_PRSNT_IND                                                 
               , PAY_HIST_IND )                                                 
               VALUES                                                           
               ( :WS-CUST-ID                                                    
               , :GCCCGDXR-CONTRACT-ID                                          
               , :GCCCGDXR-DIVISION-ID                                          
               , 'A'                                                            
               , 'GCCPGDX3'                                                     
               ,  CURRENT TIMESTAMP                                             
               , :WS-BILL-PRSNT-IND                                             
               , :WS-PAY-HIST-IND )                                             
           END-EXEC.                                                            
                                                                                
      *****************************************************************         
      * RECORD ALREADY THERE IS OK                                              
      *****************************************************************         
           IF SQLCODE NOT EQUAL ZERO AND SQLCODE NOT EQUAL -803                 
              MOVE SPACES               TO AB-MSG1                              
              MOVE WS-CUST-ID           TO WS-CUST-ID-DISPLAY                   
              STRING 'INSERT TPAA - ' WS-CUST-ID-DISPLAY                        
                  DELIMITED BY SIZE                                             
                  INTO AB-MSG1                                                  
              MOVE SQLCODE              TO AB-SQLCODE                           
              MOVE SQLERRMC             TO AB-MSG2                              
              INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
              PERFORM 9999-ABEND                                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
KL     8300-OPEN-TPACLA.                                                        
KL    *****************************************************************         
KL    * - OPEN TPACLA TO ADD TPACLA RECORD                                      
KL    *****************************************************************         
KL                                                                              
KL         ADD 1 TO LVL.                                                        
KL         MOVE '8300-OPEN-TPACLA'  TO AB-PARAGRAPH-NAME (LVL).                 
KL                                                                              
KL         EXEC SQL                                                             
KL             OPEN TPACLACUR                                                   
KL         END-EXEC.                                                            
KL                                                                              
KL         IF  SQLCODE NOT EQUAL +0 AND SQLCODE NOT EQUAL +100                  
KL             MOVE 'OPEN TCL      '     TO AB-MSG1                             
KL             MOVE SQLCODE              TO AB-SQLCODE                          
KL             MOVE SQLERRMC             TO AB-MSG2                             
KL             INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
KL             PERFORM 9999-ABEND                                               
KL         END-IF                                                               
KL                                                                              
KL         SET WS-TPACLA-START TO TRUE.                                         
KL         PERFORM 8350-READ-TPACLA UNTIL WS-TPACLA-END.                        
KL                                                                              
KL         EXEC SQL                                                             
KL             CLOSE TPACLACUR                                                  
KL         END-EXEC.                                                            
KL                                                                              
KL         IF  SQLCODE NOT EQUAL +0                                             
KL             MOVE 'CLOSE TCL     '     TO AB-MSG1                             
KL             MOVE SQLCODE              TO AB-SQLCODE                          
KL             MOVE SQLERRMC             TO AB-MSG2                             
KL             INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
KL             PERFORM 9999-ABEND                                               
KL         END-IF                                                               
KL                                                                              
KL         SUBTRACT 1 FROM LVL.                                                 
KL                                                                              
KL    *                                                                         
KL    * INSERT TPACLA FOR ALL CUST IDS THAT BELONG TO THIS SELLING CODE         
KL    *                                                                         
KL     8350-READ-TPACLA.                                                        
KL                                                                              
KL         ADD 1 TO LVL.                                                        
KL         MOVE '8350-READ-TPACLA'   TO AB-PARAGRAPH-NAME (LVL).                
KL         EXEC SQL                                                             
KL             FETCH TPACLACUR                                                  
KL             INTO  :WS-CUST-ID                                                
KL         END-EXEC.                                                            
KL                                                                              
KL         EVALUATE SQLCODE                                                     
KL             WHEN ZERO                                                        
KL                PERFORM 8370-ADD-TPACLA-REC                                   
KL             WHEN +100                                                        
KL                SET WS-TPACLA-END          TO TRUE                            
KL             WHEN OTHER                                                       
KL                 MOVE 'FETCH TPACLA  '     TO AB-MSG1                         
KL                 MOVE SQLCODE              TO AB-SQLCODE                      
KL                 MOVE SQLERRMC             TO AB-MSG2                         
KL                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
KL                 PERFORM 9999-ABEND                                           
KL         END-EVALUATE.                                                        
KL                                                                              
KL         SUBTRACT 1 FROM LVL.                                                 
KL                                                                              
KL    *****************************************************************         
KL    *  INSERT OF TPACLA REC                                                   
KL    *  THE CLIENT AND LOCATION NUMBERS ARE FROM THE INPUT RECORD              
KL    *****************************************************************         
KL     8370-ADD-TPACLA-REC.                                                     
KL                                                                              
KL         ADD 1 TO LVL.                                                        
KL         MOVE '8370-ADD-TPACLA-REC'   TO AB-PARAGRAPH-NAME (LVL).             
KL                                                                              
KL         EXEC SQL                                                             
KL             INSERT INTO TPACLA                                               
KL             ( CUST_ID                                                        
KL             , CLIENT_NUM                                                     
KL             , LOC_NUM                                                        
KL             , PA_STAT_CD                                                     
KL             , CHN_USER_ID                                                    
KL             , CHN_TS                                                         
KL             , BILL_PRSNT_IND                                                 
KL             , PAY_HIST_IND                                                   
KL             , EXP_RPT_IND                                                    
KL            )                                                                 
KL             VALUES                                                           
KL             ( :WS-CUST-ID                                                    
KL             , :GCCCGDXR-CONTRACT-ID                                          
KL             , :GCCCGDXR-LOCATION-NUM                                         
KL             , 'A'                                                            
KL             , 'GCCPGDX3'                                                     
KL             ,  CURRENT TIMESTAMP                                             
KL             , 'Y'                                                            
KL             , 'N'                                                            
KL             , 'Y' )                                                          
KL         END-EXEC.                                                            
KL                                                                              
KL    *****************************************************************         
KL    * RECORD ALREADY THERE IS OK                                              
KL    *****************************************************************         
KL         IF SQLCODE NOT EQUAL ZERO AND SQLCODE NOT EQUAL -803                 
KL            MOVE SPACES               TO AB-MSG1                              
KL            MOVE WS-CUST-ID           TO WS-CUST-ID-DISPLAY                   
KL            STRING 'INSERT TPACLA - ' WS-CUST-ID-DISPLAY                      
KL                DELIMITED BY SIZE                                             
KL                INTO AB-MSG1                                                  
KL            MOVE SQLCODE              TO AB-SQLCODE                           
KL            MOVE SQLERRMC             TO AB-MSG2                              
KL            INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                           
KL            PERFORM 9999-ABEND                                                
KL         END-IF.                                                              
KL                                                                              
KL         SUBTRACT 1 FROM LVL.                                                 
       9000-READ-EXTRACT.                                                       
      *****************************************************************         
      * - CALL MODULE TO READ GFM MASTER SEQUENTIALLY                           
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '9000-READ-EXTRACT'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-INPUT-LR              TO LOGICAL-RECORD-NAME.                
                                                                                
           CALL WS-GAEDATSR              USING WS-GAEDATSR-VERB                 
                                               GCCCGDXR-RECORD                  
                                               ICBM.                            
                                                                                
           IF NOT LR-STATUS-OK                                                  
               SET WS-EOF TO TRUE                                               
           ELSE                                                                 
               ADD +1 TO WS-INPUT-COUNTER                                       
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
                                                                                
                                                                                
      *****************************************************************         
      * THIS PARAGRAPH IS CALLED IF AN EXCEPTIONAL CONDITION, WHICH             
      * CANNOT ALLOW THE PROGRAM TO CONTINUE NORMALLY, IS FOUND.                
      * MESSAGES GIVING DETAILS OF THE ABEND ARE DISPLAYEDAND THE               
      * AND THE PROGRAM WILL TERMINATE WITH A RETURN CODE OF 16.                
      *****************************************************************         
                                                                                
       9999-ABEND.                                                              
                                                                                
      ***  DISPLAY ABEND MESSAGE                                                
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '*************************************************'.         
           DISPLAY '***** P R O G R A M   T E R M I N A T E D   *****'.         
           DISPLAY '*****          A B N O R M A L L Y          *****'.         
           DISPLAY '*************************************************'.         
           DISPLAY ' '.                                                         
           DISPLAY 'MODULE NAME    : ' AB-MODULE-NAME.                          
                                                                                
                                                                                
           IF SQLCODE = -911                                                    
              SET DEADLOCK-Y    TO TRUE                                         
           ELSE                                                                 
              SET DEADLOCK-N    TO TRUE                                         
           END-IF.                                                              
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '9999-ABEND' TO AB-PARAGRAPH-NAME (LVL).                        
                                                                                
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
           PERFORM VARYING CNT FROM 1 BY 1                                      
                     UNTIL CNT > LVL                                            
              DISPLAY AB-PARAGRAPH-NAME (CNT)                                   
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
                ROLLBACK                                                        
           END-EXEC.                                                            
                                                                                
           IF SQLCODE NOT EQUAL ZERO                                            
              DISPLAY '*********************************'                       
              DISPLAY '******   ROLLBACK FAILED   ******'                       
              DISPLAY '*********************************'                       
              MOVE +16 TO RETURN-CODE                                           
           ELSE                                                                 
              IF DEADLOCK-Y                                                     
                 MOVE +911 TO RETURN-CODE                                       
              ELSE                                                              
                 MOVE +16  TO RETURN-CODE                                       
              END-IF                                                            
           END-IF.                                                              
                                                                                
           GOBACK.                                                              
