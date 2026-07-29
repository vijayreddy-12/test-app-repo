       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRMD.                                                 
      *AUTHOR.        KLYN.                                                     
      *DATE-WRITTEN.  NOV 15, 2000.                                             
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS)                                                      
      *   GCCPFRMD - PURGE FORMS FROM FORM TABLE                                
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *   THE PROGRAM WILL READ A SELECTION PARAMETER AND GET                   
      *   CURRENT TIMESTAMP.                                                    
      *   RETURN ALL DB2 LINES FROM TSD TABLE WHERE THE SENT TO PRINT           
      *   TIMESTAMP IS OLDER THAN THE NUMBER OF DAYS INPUT ON THE PARM.         
      *   THE MATCHING LINES WILL THEN BE DELETED FROM THE TFORM TABLE.         
      *   A COUNT OF ALL DELETIONS WILL BE DISPLAYED AT THE END ALONG           
      *   WITH THE PARAMETER AND CURRENT TIMESTAMP.                             
      *                                                                         
      *                                                                         
      *   INPUT FILES:  SELECTION CRITERIA  (PARM)                              
      *                 TFORM   - FORMS TABLE                                   
      *                 TSD     - SUBMISSION TABLE                              
      *                                                                         
      *   OUTPUT FILES: DISPLAY                                                 
      *                                                                         
      *   DB2 TABLES                                                            
      *   ACCESSED:     TFORM   - FORMS TABLE          - DELETE                 
      *                 TSD     - SUBMISSION TABLE     - UPDATE                 
      *                                                                         
      *   CALLS:        GAEDATSR- FILE I/O                                      
      *                                                                         
      *   INCLUDE CODE: SQLCA    - SQL COMMUNICATION AREA                       
      *                                                                         
      *                 TFORMD   - TFORM   TABLE DECLARATION                    
      *                 TSDD     - TSD     TABLE DECLARATION                    
      *                                                                         
      *                 TFORM    - TFORM   HOST VARIABLES                       
      *                 TSD      - TSD     HOST VARIABLES                       
      *                                                                         
      *                 TGDI     - TGD     INDICATOR VARIABLES                  
      *                 TSDI     - TSD     INDICATOR VARIABLES                  
      *                                                                         
      *                                                                         
      *                 GCCCDCNT - PURGE PARM NUMBER OF DAYS                    
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
      * KLYN         º15/11/00º ORIGINAL CODE                                   
      *--------------+--------+-----------------------------------------        
      * J ELKINS     |28/02/01| ADD DISPLAY FOR AUDIT TRAIL                     
      *              |        | CHANGE CONTROL CARD INPUT OF THE                
      *              |        | NUMBER OF DAYS FROM NNN TO NNNN                 
      *--------------+--------+-----------------------------------------        
      * IBM GR       |29/08/08| UPGRADED IN ECU PROJECT                         
      *--------------+--------+-----------------------------------------        
      * MINARUL ISLAM|15/12/17| TL324209 APPLYING INTERMITEN COMMIT             
      * MINARUL ISLAM|02/03/18| TL324209 CURSOR WITH HOLD TO ALLOW              
      *              |        | THE CURSOR REMAIN OPEN AFTER PERFORMING         
      *              |        | INTERMITTENT COMMIT.                            
      *              |        |                                                 
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
                                                                                
           EXEC SQL INCLUDE TFORMD  END-EXEC.                                   
           EXEC SQL INCLUDE TSDD    END-EXEC.                                   
                                                                                
      *** DB2 HOST & INDICATOR VARIABLES                                        
                                                                                
                                                                                
      *01  DCLFORM.                                                             
           EXEC SQL INCLUDE TFORM   END-EXEC.                                   
                                                                                
      *01  DCLTSD.                                                              
           EXEC SQL INCLUDE TSD     END-EXEC.                                   
           EXEC SQL INCLUDE TSDI    END-EXEC.                                   
                                                                                
                                                                                
      *01 DUMMY-DATE-TIME.                                                      
           EXEC SQL INCLUDE TCTFRMD END-EXEC.                                   
           EXEC SQL INCLUDE TCTFRM  END-EXEC.                                   
                                                                                
                                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      *    REPRINT PARM FILE LAYOUT                                             
      *----------------------------------------------------------------*        
       01  GCCCDCNT-RECORD.             COPY  GCCCDCNT.                         
      /                                                                         
                                                                                
      /                                                                         
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-VARIABLES.                                                        
           05  WS-TIMESTAMP                PIC X(26)    VALUE SPACES.           
           05  WS-TS-MINUS-DAYS            PIC X(26)    VALUE SPACES.           
JE001      05  WS-AGED                     PIC S9(4)V COMP-3 VALUE 90.          
           05  WS-COUNT-UPDATED            PIC S9(12) COMP-3 VALUE +0.          
           05  WS-COUNT-DELETED            PIC S9(12) COMP-3 VALUE +0.          
           05  WS-OBTAIN-FIRST             PIC X(16)   VALUE                    
                                               'OBTAIN  FIRST   '.              
           05  WS-INPUT-LR                 PIC X(16) VALUE                      
                                               'CARD-DATA-010   '.              
           05  WS-DELIMITER                PIC X     VALUE '%'.                 
           05  WS-CUST-CNT                 PIC S9(9)  COMP-3 VALUE +0.          
           05  WS-NUM-COMMIT               PIC S9(9)  COMP-3 VALUE +0.          
                                                                                
      *                                                                         
      *****************************************************************         
      *** DISPLAY FIELDS FOR END OF RUN                                         
      *****************************************************************         
      *                                                                         
       01  WS-DISPLAY-FIELDS.                                                   
           05  WS-DISPLAY1                 PIC X(40)  VALUE                     
               'PROGRAM GCCCFRMD AGED PRINT DATA PURGE'.                        
           05  WS-DISPLAY2.                                                     
               10  FILLER                  PIC X(17)  VALUE                     
               'AGED PARAMETER ='.                                              
JE001          10  WS-DISPLAY-AGED         PIC 9999.                            
           05  WS-DISPLAY3.                                                     
               10  FILLER                  PIC X(19)  VALUE                     
               'CURRENT TIMESTAMP'.                                             
               10  WS-DISPLAY-TS           PIC X(16).                           
           05  WS-DISPLAY4.                                                     
               10  FILLER                  PIC X(19)  VALUE                     
               'DELETE TIMESTAMP'.                                              
               10  WS-DISDELT-TS           PIC X(16).                           
           05  WS-DISPLAY5.                                                     
               10  FILLER                  PIC X(22)  VALUE                     
               'TSD   LINES UPDATED'.                                           
               10  WS-DISPLAY-UPD          PIC ZZZ999.                          
           05  WS-DISPLAY6.                                                     
               10  FILLER                  PIC X(22)  VALUE                     
               'TFORM LINES DELETED'.                                           
               10  WS-DISPLAY-DEL          PIC ZZZ999.                          
JE001      05  WS-DISPLAYA.                                                     
JE001          10  FILLER                  PIC X(35)  VALUE                     
JE001          'ENTRY DELETED FROM TFORM TABLE FOR '.                           
JE001          10  WS-DISPLAYA-FORM-ID     PIC 9(11).                           
JE001          10  FILLER                  PIC X      VALUE SPACES.             
JE001          10  WS-DISPLAYA-FORM-SEQ-NUM PIC 9(3).                           
JE001          10  FILLER                  PIC X(23)  VALUE                     
JE001          ' (FORM-ID,FORM-SEQ-NUM)'.                                       
      *                                                                         
      *****************************************************************         
      *** CONSTANTS                                                             
      *****************************************************************         
      *                                                                         
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GAEDATSR                 PIC X(08)                            
                                          VALUE 'GAEDATSR'.                     
           05  WS-GAEDATSR-VERB            PIC X(16).                           
      *----------------------------------------------------------------*        
      *    ACTION VERBS USED TO CALL GAEDATSR                                   
      *----------------------------------------------------------------*        
       01  DATA-SERVER-VERBS.                                                   
           COPY GARDSVRB.                                                       
      /                                                                         
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME              PIC X(60) VALUE                      
                'GCCPFRMD - POPULATE FORM DATABASE'.                            
           10  AB-PARAGRAPH-NAME  OCCURS 25 TIMES                               
                                           PIC X(60).                           
           10  AB-MESSAGE.                                                      
               15  AB-MSG1                 PIC X(70).                           
               15  AB-MSG2                 PIC X(70).                           
           10  AB-SQLCODE                  PIC ----9.                           
           10  LVL                         PIC S9(4) COMP.                      
           10  CNT                         PIC S9(4) COMP.                      
                                                                                
      /                                                                         
      *****************************************************************         
      *** DB2 CURSOR DECLARE                                                    
      *****************************************************************         
                                                                                
      * RETRIEVE ALL OUTSTANDING RECORDS WHICH HAVE BEEN SENT TO PRINT          
      * PRIOR TO THE NUMBER OF DAYS ENTERED ON THE PARAMETER                    
                                                                                
      *                                                                         
           EXEC SQL                                                             
             DECLARE PURG_FORMS CURSOR WITH HOLD FOR                            
             SELECT     FORM_ID                                                 
                       ,FORM_SEQ_NUM                                            
              FROM TSD                                                          
              WHERE SENT_TO_PRINT_TS  <  :WS-TS-MINUS-DAYS                      
                AND SENT_TO_PRINT_TS  IS NOT NULL                               
                AND PURGE_TS  IS NULL                                           
              FOR UPDATE OF PURGE_TS                                            
                           ,FORM_ID                                             
                           ,FORM_SEQ_NUM                                        
           END-EXEC.                                                            
                                                                                
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
                                                                                
           PERFORM 2000-PROCESS         THRU 2000-EXIT.                         
                                                                                
           PERFORM 3000-COMPLETION      THRU 3000-EXIT.                         
                                                                                
           GOBACK.                                                              
      /                                                                         
                                                                                
       1000-INITIALIZATION.                                                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE 'GCCPFRMD'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
                                                                                
           INITIALIZE                   GCCCDCNT-RECORD.                        
                                                                                
                                                                                
      ******************************************************************        
      * HANDLE CONTROL CARD                                                     
      ******************************************************************        
                                                                                
                                                                                
           MOVE WS-OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                       
                                                                                
           PERFORM 6100-READ-INPUT  THRU 6100-EXIT.                             
                                                                                
           MOVE  GCCCDCNT-NUMBER-OF-DAYS                                        
                                     TO  WS-AGED.                               
                                                                                
           PERFORM 6300-CLOSE-INPUT THRU 6300-EXIT.                             
                                                                                
                                                                                
           EXEC SQL                                                             
                 SELECT                                                         
                      CURRENT TIMESTAMP                                         
                     ,CURRENT TIMESTAMP  - :WS-AGED DAYS                        
                   INTO                                                         
                      :WS-TIMESTAMP                                             
                     ,:WS-TS-MINUS-DAYS                                         
                   FROM                                                         
                      TCTFRM                                                    
                   WHERE                                                        
                      CODE_VALUE   = 'GL0003  '                                 
           END-EXEC.                                                            
                                                                                
           MOVE 'GET TIMESTAMP' TO AB-MESSAGE.                                  
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
JE001      DISPLAY WS-DISPLAY1.                                                 
JE001      DISPLAY ' '.                                                         
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
                                                                                
       2000-PROCESS.                                                            
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS' TO AB-PARAGRAPH-NAME (LVL).                      
                                                                                
           EXEC SQL                                                             
             OPEN PURG_FORMS                                                    
           END-EXEC.                                                            
                                                                                
           MOVE 'OPEN PURG_FORMS CURSOR' TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
                                                                                
           PERFORM                                                              
              WITH TEST BEFORE                                                  
              UNTIL SQLCODE = +100                                              
                                                                                
              EXEC SQL                                                          
                FETCH  PURG_FORMS                                               
                 INTO  :DCLTSD.FORM-ID                                          
                      ,:DCLTSD.FORM-SEQ-NUM                                     
              END-EXEC                                                          
                                                                                
              MOVE 'FETCH EXTRACT RECORD CURSOR' TO AB-MESSAGE                  
              PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT                        
                                                                                
              IF SQLCODE = ZERO                                                 
                 PERFORM 2100-UPDATE-TS       THRU  2100-EXIT                   
                 PERFORM 2200-DELETE-TS       THRU  2200-EXIT                   
              END-IF                                                            
                                                                                
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
             CLOSE PURG_FORMS                                                   
           END-EXEC.                                                            
                                                                                
           DISPLAY 'NO. OF COMMITS     : ' WS-NUM-COMMIT.                       
                                                                                
           MOVE 'CLOSE EXTR_FORMS CURSOR' TO AB-MESSAGE.                        
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2000-EXIT.                                                               
            EXIT.                                                               
      /                                                                         
       2100-UPDATE-TS.                                                          
      *****************************************************                     
      *    UPDATE TIMESTAMP FOR PRINT ON SUBMISSION TABLE                       
      *    TSD                                                                  
      *****************************************************                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2200-UPDATE-TS' TO AB-PARAGRAPH-NAME (LVL).                    
                                                                                
           EXEC SQL                                                             
               UPDATE TSD                                                       
                 SET  PURGE_TS      = :WS-TIMESTAMP                             
                     ,FORM_ID       = NULL                                      
                     ,FORM_SEQ_NUM  = NULL                                      
               WHERE                                                            
                  CURRENT OF       PURG_FORMS                                   
            END-EXEC                                                            
                                                                                
                                                                                
           ADD  1    TO  WS-COUNT-UPDATED.                                      
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
       2200-DELETE-TS.                                                          
      *****************************************************                     
      *    DELETE AGED FORMS FROM TFORM TABLE                                   
      *****************************************************                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2300-DELETE-TS' TO AB-PARAGRAPH-NAME (LVL).                    
                                                                                
           MOVE 0 TO FORM-DATA-STRING-LEN                                       
                                                                                
                                                                                
           EXEC SQL                                                             
               DELETE FROM TFORM                                                
               WHERE                                                            
                  FORM_ID       = :DCLTSD.FORM-ID                               
             AND  FORM_SEQ_NUM  = :DCLTSD.FORM-SEQ-NUM                          
           END-EXEC.                                                            
                                                                                
           MOVE 'DELETE OLD PRINT FORMS' TO AB-MESSAGE                          
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
                                                                                
      ****************************************************************          
      * COMMIT AFTER DELETING 100 RECORDS                                       
      ****************************************************************          
           ADD  1    TO  WS-CUST-CNT.                                           
           IF WS-CUST-CNT >= 100                                                
               MOVE 0 TO WS-CUST-CNT                                            
               ADD 1  TO WS-NUM-COMMIT                                          
               EXEC SQL                                                         
                COMMIT                                                          
               END-EXEC                                                         
           END-IF.                                                              
      ****************************************************************          
                                                                                
           ADD  1    TO  WS-COUNT-DELETED.                                      
JE001      MOVE FORM-ID OF DCLTSD       TO WS-DISPLAYA-FORM-ID.                 
JE001      MOVE FORM-SEQ-NUM OF DCLTSD  TO WS-DISPLAYA-FORM-SEQ-NUM.            
JE001      DISPLAY WS-DISPLAYA.                                                 
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
       3000-COMPLETION.                                                         
      *****************************************************************         
      *  THIS PARAGRAPH...                                                      
      *    - CLOSES OUTPUT FILE                                                 
      *****************************************************************         
                                                                                
                                                                                
                                                                                
                                                                                
           MOVE   WS-COUNT-UPDATED     TO WS-DISPLAY-UPD.                       
           MOVE   WS-COUNT-DELETED     TO WS-DISPLAY-DEL.                       
           MOVE   WS-TIMESTAMP         TO WS-DISPLAY-TS.                        
           MOVE   WS-TS-MINUS-DAYS     TO WS-DISDELT-TS.                        
           MOVE   WS-AGED              TO WS-DISPLAY-AGED.                      
                                                                                
JE001      DISPLAY ' '.                                                         
           DISPLAY WS-DISPLAY2.                                                 
           DISPLAY WS-DISPLAY3.                                                 
           DISPLAY WS-DISPLAY4.                                                 
           DISPLAY WS-DISPLAY5.                                                 
           DISPLAY WS-DISPLAY6.                                                 
                                                                                
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
                                                                                
      /                                                                         
                                                                                
                                                                                
       6100-READ-INPUT.                                                         
      ******************************************************************        
      *    READ NEXT INPUT RECORD.                                     *        
      ******************************************************************        
      *                                                                         
                                                                                
           MOVE WS-INPUT-LR            TO LOGICAL-RECORD-NAME.                  
           INITIALIZE GCCCDCNT-RECORD.                                          
                                                                                
                                                                                
           CALL WS-GAEDATSR          USING  WS-GAEDATSR-VERB                    
                                         GCCCDCNT-RECORD                        
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               MOVE  0                       TO AB-SQLCODE                      
               MOVE 'ERROR READING INPUT '   TO AB-MSG1                         
               MOVE GCCCDCNT-RECORD          TO AB-MSG2                         
               DISPLAY 'LR-STATUS ' LINKAGE-STATUS                              
               PERFORM 9999-ABEND THRU 9999-EXIT.                               
                                                                                
                                                                                
       6100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6300-CLOSE-INPUT.                                                        
                                                                                
           MOVE WS-INPUT-LR            TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               MOVE  0                       TO AB-SQLCODE                      
               MOVE 'ERROR CLOSING INPUT '   TO AB-MSG1                         
               MOVE GCCCDCNT-RECORD          TO AB-MSG2                         
               DISPLAY 'LR-STATUS ' LINKAGE-STATUS                              
               PERFORM 9999-ABEND THRU 9999-EXIT.                               
                                                                                
                                                                                
       6300-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
      *****************************************************************         
      * THIS PARAGRAPH CHECKS THE SQL CODE AFTER A DB2 CALL AND HANDLES         
      * ANY ERRORS DETECTED.                                                    
      *****************************************************************         
                                                                                
       8900-CHECK-SQL-CODE.                                                     
                                                                                
           EVALUATE SQLCODE                                                     
                                                                                
              WHEN ZERO                                                         
                 CONTINUE                                                       
              WHEN +100                                                         
                 CONTINUE                                                       
              WHEN OTHER                                                        
                 MOVE SQLCODE      TO AB-SQLCODE                                
                 MOVE SQLERRMC     TO AB-MSG2                                   
                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                        
                 PERFORM 9999-ABEND THRU 9999-EXIT                              
                                                                                
           END-EVALUATE.                                                        
                                                                                
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
                                                                                
                                                                        12330000
           EXEC SQL                                                     12340000
             ROLLBACK                                                   12350000
           END-EXEC.                                                    12360000
                                                                        12370000
           MOVE +16 TO RETURN-CODE.                                     12380000
                                                                                
           GOBACK.                                                              
                                                                                
       9999-EXIT.                                                               
           EXIT.                                                                
