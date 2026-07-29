       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPGDAG.                                                 
      *AUTHOR.        M HUNTER.                                                 
      *DATE-WRITTEN.  MAY 27, 2003.                                             
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS)                                                      
      *   GCCPGDAG                                                              
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *   UPDATES IDENT CODE ON GRP/DIV/ADVISOR TABLE AND                       
      *   UPDATES ADDRESS INFO ON CUST TABLE                                    
      *                                                                         
      *   INPUT FILES : PGW.ADMIN.PROD.AGTADDR.VSAM                             
      *                                                                         
      *   OUTPUT FILES: NONE                                                    
      *                                                                         
      *   CALLS       : ICBM     - INTERFACE CONTROL BLOCK (DATA SERVER)        
      *                                                                         
      *   DB2 TABLES                                                            
      *                                                                         
      *       ACCESSED: TGDADV   - GROUP/DIV/ADVISOR TABLE                      
      *                 TADVSR   - ADVISOR/CUST TABLE                           
      *                 TCUST    - CUSTOMER TABLE                               
      *                                                                         
      *   INCLUDE CODE: SQLCA    - SQL COMMUNICATION AREA                       
      *                                                                         
      *     TABLE DECLARATIONS   - TGDADVD                                      
      *                                                                         
      *     HOST VARIABLES       - TGDADV                                       
      *                                                                         
      *****************************************************************         
      *****************************************************************         
      *   MODIFICATION LOG                                                      
      ******************************************************************        
      * PROGRAMMER   º  DATE  º             CHANGE                              
      *    NAME      ºDD/MM/YYº           DESCRIPTION                           
      *--------------+--------+-----------------------------------------        
      * M HUNTER     º27/05/03º ORIGINAL CODE                                   
      *--------------+--------+-----------------------------------------        
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
                                                                                
           EXEC SQL INCLUDE TGDADV  END-EXEC.                                   
           EXEC SQL INCLUDE TGDADVD END-EXEC.                                   
                                                                                
           EXEC SQL INCLUDE TADVSR  END-EXEC.                                   
           EXEC SQL INCLUDE TADVSRD END-EXEC.                                   
                                                                                
           EXEC SQL INCLUDE TCUST   END-EXEC.                                   
           EXEC SQL INCLUDE TCUSTD  END-EXEC.                                   
                                                                                
      *****************************************************************         
      *   SWITCHES AND COUNTERS                                                 
      *****************************************************************         
       01  WS-SWITCHES-COUNTERS.                                                
           05  WS-EOF-SW                 PIC X      VALUE 'N'.                  
               88  WS-EOF                           VALUE 'Y'.                  
           05  WS-EOF-ADVSR-SW           PIC X      VALUE 'N'.                  
               88  WS-EOF-ADVSR                     VALUE 'Y'.                  
           05  WS-DEADLOCK               PIC X      VALUE 'N'.                  
               88 DEADLOCK-Y                        VALUE 'Y'.                  
               88 DEADLOCK-N                        VALUE 'N'.                  
           05  WS-INPUT-COUNTER          PIC S9(7)  COMP-3 VALUE +0.            
           05  WS-UPDATE-COUNTER         PIC S9(7)  COMP-3 VALUE +0.            
                                                                                
      *****************************************************************         
      *   VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME            PIC X(60) VALUE                        
                'GCCPGDAG - UPDATE CPD GRP/DIV/ADVISOR'.                        
           10  AB-PARAGRAPH-NAME  OCCURS 25 TIMES                               
                                         PIC X(60).                             
           10  AB-MESSAGE.                                                      
               15  AB-MSG1               PIC X(70).                             
               15  AB-MSG2               PIC X(70).                             
           10  AB-SQLCODE                PIC ----9.                             
           10  LVL                       PIC S9(4) COMP.                        
           10  CNT                       PIC S9(4) COMP.                        
                                                                                
      ****************************************************************          
      *   AGENCY ADDRESS RECORD                                                 
      ****************************************************************          
           COPY AGTADDR.                                                        
                                                                                
      ****************************************************************          
      *   DATA SERVER VARIABLES                                                 
      ****************************************************************          
       01  WS-CALLING-VARIABLE.                                                 
           05  WS-GAEDATSR               PIC X(08)                              
                                         VALUE 'GAEDATSR'.                      
           05  WS-GAEDATSR-VERB          PIC X(16).                             
           05  WS-INPUT-LR               PIC X(16)                              
                                         VALUE 'CARD-DATA-010   '.              
           05  AGT-ADDR                  PIC X(16)                              
                                         VALUE 'AGT-ADDR        '.              
                                                                                
       01  WS-OTHER-VARIABLES.                                                  
           05  WS-SAVE-ADVISOR-ID    PIC X(6) VALUE SPACES.                     
           05  WS-GROUP-ID           PIC X(7) VALUE SPACES.                     
           05  WS-DIV-ID             PIC X(3) VALUE SPACES.                     
           05  WS-ADVISOR-ID         PIC X(6) VALUE SPACES.                     
           05  WS-ADVISOR-AGENCY-ID  PIC X(6) VALUE SPACES.                     
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
       01  DATA-SERVER-VERBS.                                                   
           COPY GARDSVRB.                                                       
                                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      *                                                                *        
      *   DB2 CURSORS.                                                 *        
      *                                                                *        
      *----------------------------------------------------------------*        
      *                                                                         
      *   RETRIEVE GRP/DIV/ADVISOR                                              
      *                                                                         
           EXEC SQL                                                             
               DECLARE GD_ADV CURSOR FOR                                        
                  SELECT GROUP_ID                                               
                       , DIV_ID                                                 
                       , ADVISOR_ID                                             
                       , ADVISOR_AGENCY_ID                                      
                     FROM TGDADV                                                
           END-EXEC.                                                            
                                                                                
      *                                                                         
      *   RETRIEVE CUST FOR ADVISOR                                             
      *                                                                         
           EXEC SQL                                                             
               DECLARE ADVSR  CURSOR FOR                                        
                  SELECT CUST_ID                                                
                    FROM TADVSR                                                 
                   WHERE ADVISOR_ID = :WS-ADVISOR-ID                            
           END-EXEC.                                                            
                                                                                
      ****************************************************************          
      *********   P R O C E D U R E   D I V I S I O N   **************          
      ****************************************************************          
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           MOVE 1 TO LVL                                                        
           MOVE '0000-MAINLINE'         TO   AB-PARAGRAPH-NAME (LVL)            
                                                                                
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT                          
                                                                                
           PERFORM 2000-PROCESS         THRU 2000-EXIT                          
             UNTIL WS-EOF.                                                      
                                                                                
           PERFORM 3000-COMPLETION      THRU 3000-EXIT                          
                                                                                
           GOBACK.                                                              
                                                                                
       0000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1000-INITIALIZATION.                                                     
      *****************************************************************         
      * - READ FIRST GFM RECORD                                                 
      * - GET CURRENT TIME FOR USE WHEN ADDING A NEW TGD RECORD                 
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION'    TO AB-PARAGRAPH-NAME (LVL)             
                                                                                
           MOVE 'GCCPGDAG'               TO ICBM-PROGRAM-NAME                   
           MOVE LOW-VALUES               TO LINKAGE-CONTROL                     
                                                                                
           EXEC SQL                                                             
               OPEN GD_ADV                                                      
           END-EXEC                                                             
                                                                                
           IF SQLCODE NOT = ZERO                                                
               MOVE 'OPEN GD_ADV' TO AB-MSG1                                    
               MOVE SQLCODE       TO AB-SQLCODE                                 
               MOVE SQLERRMC      TO AB-MSG2                                    
               INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
               PERFORM 9999-ABEND THRU 9999-EXIT                                
           END-IF                                                               
                                                                                
           PERFORM 2100-FETCH-TGDADV                                            
              THRU 2100-EXIT                                                    
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2000-PROCESS.                                                            
      *****************************************************************         
      *  CHECK TO SEE IF THE CURRENT GROUP/DIVISION EXISTS IN TGD               
      *   - IF IT DOES, DO UPDATES                                              
      *   - IF IT DOESN'T, ADD (AS LONG AS EXTRACT ISN'T TERMINATED)            
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS'           TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           IF WS-ADVISOR-ID NOT = WS-SAVE-ADVISOR-ID                            
               MOVE WS-ADVISOR-ID TO AGENT-CODE                                 
               PERFORM  9000-READ-AGTADDR                                       
                  THRU  9000-EXIT                                               
               MOVE WS-ADVISOR-ID TO WS-SAVE-ADVISOR-ID                         
           END-IF                                                               
                                                                                
           IF  LR-STATUS-OK                                                     
               PERFORM 2200-UPDATE-TGDADV THRU 2200-EXIT                        
           ELSE                                                                 
               DISPLAY '******************************************'             
               DISPLAY 'NO IDENT CODE FOUND FOR:'                               
               DISPLAY '          ADVISOR ID   : ', WS-ADVISOR-ID               
               DISPLAY '          GROUP/DIV    : ', WS-GROUP-ID,                
                                                    WS-DIV-ID                   
           END-IF                                                               
                                                                                
           PERFORM 2100-FETCH-TGDADV                                            
              THRU 2100-EXIT                                                    
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2100-FETCH-TGDADV.                                                       
      *****************************************************************         
      * FETCH THE NEXT TGDADV RECORD                                            
      *****************************************************************         
           ADD 1 TO LVL                                                         
           MOVE '2100-FETCH-TGDADV'     TO AB-PARAGRAPH-NAME (LVL)              
                                                                                
           EXEC SQL                                                             
               FETCH GD_ADV                                                     
                   INTO :WS-GROUP-ID                                            
                       ,:WS-DIV-ID                                              
                       ,:WS-ADVISOR-ID                                          
                       ,:WS-ADVISOR-AGENCY-ID                                   
           END-EXEC.                                                            
                                                                                
           IF SQLCODE = ZERO                                                    
               ADD +1      TO WS-INPUT-COUNTER                                  
           ELSE                                                                 
               IF SQLCODE = +100                                                
                   SET WS-EOF TO TRUE                                           
               ELSE                                                             
                   MOVE 'FETCH GD_ADV' TO AB-MSG1                               
                   MOVE SQLCODE        TO AB-SQLCODE                            
                   MOVE SQLERRMC       TO AB-MSG2                               
                   INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
                   PERFORM 9999-ABEND THRU 9999-EXIT                            
               END-IF                                                           
           END-IF                                                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2200-UPDATE-TGDADV.                                                      
      *****************************************************************         
      * UPDATE THE ADVISOR-AGENCY-ID ON TGDADV.                                 
      *****************************************************************         
           ADD 1 TO LVL                                                         
           MOVE '2200-UPDATE-TGDADV'     TO AB-PARAGRAPH-NAME (LVL)             
                                                                                
           MOVE AGENT-IDENT-CODE         TO ADVISOR-AGENCY-ID                   
                                                                                
           EXEC SQL                                                             
               UPDATE TGDADV                                                    
                   SET ADVISOR_AGENCY_ID = :ADVISOR-AGENCY-ID                   
                   WHERE GROUP_ID        = :WS-GROUP-ID                         
                     AND DIV_ID          = :WS-DIV-ID                           
           END-EXEC                                                             
                                                                                
           IF SQLCODE = ZERO                                                    
               ADD +1      TO WS-UPDATE-COUNTER                                 
           ELSE                                                                 
               MOVE 'UPDATE TGDADV'  TO AB-MSG1                                 
               MOVE SQLCODE          TO AB-SQLCODE                              
               MOVE SQLERRMC         TO AB-MSG2                                 
               INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
               PERFORM 9999-ABEND THRU 9999-EXIT                                
           END-IF                                                               
                                                                                
           MOVE SPACE TO WS-EOF-ADVSR-SW                                        
                                                                                
           EXEC SQL                                                             
               OPEN ADVSR                                                       
           END-EXEC                                                             
                                                                                
           IF SQLCODE NOT = ZERO                                                
               MOVE 'OPEN ADVSR'  TO AB-MSG1                                    
               MOVE SQLCODE       TO AB-SQLCODE                                 
               MOVE SQLERRMC      TO AB-MSG2                                    
               INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
               PERFORM 9999-ABEND THRU 9999-EXIT                                
           END-IF                                                               
                                                                                
           PERFORM 2400-FETCH-TADVSR  THRU 2400-EXIT                            
           PERFORM 2300-UPDATE-TCUST  THRU 2300-EXIT                            
             UNTIL WS-EOF-ADVSR                                                 
                                                                                
           EXEC SQL                                                             
               CLOSE ADVSR                                                      
           END-EXEC                                                             
                                                                                
           IF SQLCODE NOT = ZERO                                                
               MOVE 'CLOSE ADVSR' TO AB-MSG1                                    
               MOVE SQLCODE       TO AB-SQLCODE                                 
               MOVE SQLERRMC      TO AB-MSG2                                    
               INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                          
               PERFORM 9999-ABEND THRU 9999-EXIT                                
           END-IF                                                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2300-UPDATE-TCUST.                                                       
      *****************************************************************         
      * UPDATE THE ADVISOR ADDRESS ON TCUST.                                    
      *****************************************************************         
           ADD 1 TO LVL                                                         
           MOVE '2300-UPDATE-TCUST'      TO AB-PARAGRAPH-NAME (LVL)             
                                                                                
           MOVE AGENT-ADDRESS-LINE1      TO CUST-ADDR-1                         
           MOVE AGENT-ADDRESS-LINE2      TO CUST-ADDR-2                         
           MOVE AGENT-CITY               TO CUST-ADDR-CITY                      
           MOVE AGENT-POSTAL-SYMBOL      TO CUST-ADDR-PROV                      
           MOVE AGENT-POSTAL-CODE        TO CUST-ADDR-POST-CD                   
                                                                                
           EXEC SQL                                                             
               UPDATE TCUST                                                     
                   SET CUST_ADDR_1       = :CUST-ADDR-1                         
                      ,CUST_ADDR_2       = :CUST-ADDR-2                         
                      ,CUST_ADDR_CITY    = :CUST-ADDR-CITY                      
                      ,CUST_ADDR_PROV    = :CUST-ADDR-PROV                      
                      ,CUST_ADDR_POST_CD = :CUST-ADDR-POST-CD                   
                   WHERE CUST_ID         = :DCLTCUST.CUST-ID                    
           END-EXEC                                                             
                                                                                
           IF SQLCODE = ZERO                                                    
               CONTINUE                                                         
           ELSE                                                                 
               IF SQLCODE = +100                                                
                   CONTINUE                                                     
               ELSE                                                             
                   MOVE 'UPDATE TCUST'   TO AB-MSG1                             
                   MOVE SQLCODE          TO AB-SQLCODE                          
                   MOVE SQLERRMC         TO AB-MSG2                             
                   INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
                   PERFORM 9999-ABEND THRU 9999-EXIT                            
               END-IF                                                           
           END-IF                                                               
                                                                                
           PERFORM 2400-FETCH-TADVSR  THRU 2400-EXIT                            
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2300-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2400-FETCH-TADVSR.                                                       
      *****************************************************************         
      * FETCH THE NEXT TADVSR RECORD                                            
      *****************************************************************         
           ADD 1 TO LVL                                                         
           MOVE '2400-FETCH-TADVSR'     TO AB-PARAGRAPH-NAME (LVL)              
                                                                                
           EXEC SQL                                                             
               FETCH ADVSR                                                      
                   INTO :DCLTCUST.CUST-ID                                       
           END-EXEC.                                                            
                                                                                
           IF SQLCODE = ZERO                                                    
               CONTINUE                                                         
           ELSE                                                                 
               IF SQLCODE = +100                                                
                   SET WS-EOF-ADVSR TO TRUE                                     
               ELSE                                                             
                   MOVE 'FETCH ADVSR'  TO AB-MSG1                               
                   MOVE SQLCODE        TO AB-SQLCODE                            
                   MOVE SQLERRMC       TO AB-MSG2                               
                   INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                      
                   PERFORM 9999-ABEND THRU 9999-EXIT                            
               END-IF                                                           
           END-IF                                                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2400-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3000-COMPLETION.                                                         
      *****************************************************************         
      * CLOSE FILES                                                             
      *****************************************************************         
                                                                                
           ADD 1 TO LVL                                                         
           MOVE '3000-COMPLETION'        TO AB-PARAGRAPH-NAME (LVL)             
                                                                                
      * CLOSE DATA SERVER                                                       
                                                                                
           MOVE SPACES                    TO LOGICAL-RECORD-NAME                
           MOVE FINISH-LR                 TO WS-GAEDATSR-VERB                   
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                                  LOGICAL-RECORD-NAME                           
                                  ICBM                                          
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       9000-READ-AGTADDR.                                                       
                                                                                
           MOVE AGT-ADDR                  TO LOGICAL-RECORD-NAME                
           MOVE OBTAIN-KEYED              TO WS-GAEDATSR-VERB                   
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                                  AGENT-ADDRESS-RECORD                          
                                  ICBM                                          
                                                                                
           EVALUATE TRUE                                                        
               WHEN LR-STATUS-OK                                                
                   CONTINUE                                                     
               WHEN LR-NOT-FOUND                                                
                   CONTINUE                                                     
               WHEN KEYED-LR-NOT-FOUND                                          
                   CONTINUE                                                     
               WHEN OTHER                                                       
                   MOVE 'READ AGT ADDR FILE'    TO AB-MSG1                      
                   MOVE PROGRAM-LINKAGE-STATUS  TO AB-SQLCODE                   
                   MOVE SPACES                  TO AB-MSG2                      
                   PERFORM 9999-ABEND THRU 9999-EXIT                            
           END-EVALUATE.                                                        
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                                
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
       9999-EXIT.                                                               
           EXIT.                                                                
