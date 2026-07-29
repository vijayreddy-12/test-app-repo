CBL FLAG(I)                                                                     
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GACPAUDR.                                                 
      *AUTHOR.        WARREN FINN.                                              
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.  APR-2001.                                                 
      *--------------------------------------------------------------*          
      *                                                              *          
      *  THIS BATCH MODULE PRINTS THE DEPARTMENT AUDIT REPORT        *          
      *--------------------------------------------------------------*          
      *---------------------------------------------------------------*         
      *  UPDATE HISTORY                                                         
      *---------------------------------------------------------------*         
      *   Aug 2008 - IBM GR     - ECU Project                                   
      *            - Upgraded in ECU Project                                    
      *---------------------------------------------------------------*         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
         SPECIAL-NAMES.                                                         
             C01 IS NEW-PAGE.                                                   
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
           SELECT AUDIT-REPORT-OUT ASSIGN TO AUDROUT.                           
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
                                                                                
       FD  AUDIT-REPORT-OUT                                                     
           BLOCK  CONTAINS 0 RECORDS                                            
           RECORDING MODE F                                                     
           .                                                                    
                                                                                
       01  AUDIT-REPORT-RECORD.                                         00009805
           05 FILLER PIC X(133).                                                
                                                                                
                                                                                
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *----------------------------------------------------------------*        
      *    DB2 INCLUDES                                                *        
      *----------------------------------------------------------------*        
                                                                                
           EXEC SQL INCLUDE SQLCA END-EXEC.                                     
                                                                                
      *----------------------------------------------------------------*        
      *    TCTDPT DEPARTMENT TABLE                                              
      *----------------------------------------------------------------*        
                                                                                
           EXEC SQL INCLUDE TCTDPT  END-EXEC.                                   
           EXEC SQL INCLUDE TCTDPTD END-EXEC.                                   
                                                                                
      *----------------------------------------------------------------*        
      *    TCPMUS CPM USER TABLE                                                
      *----------------------------------------------------------------*        
                                                                                
           EXEC SQL INCLUDE TCPMUS  END-EXEC.                                   
           EXEC SQL INCLUDE TCPMUSD END-EXEC.                                   
                                                                                
      *----------------------------------------------------------------*        
      *    TCPMU CPM USER ACCESS TABLE                                 *        
      *----------------------------------------------------------------*        
                                                                                
           EXEC SQL INCLUDE TCPMU  END-EXEC.                                    
           EXEC SQL INCLUDE TCPMUD END-EXEC.                                    
                                                                                
      *----------------------------------------------------------------*        
      *    TCTCAT CPM ACCESS TYPE CODES                                         
      *----------------------------------------------------------------*        
                                                                                
           EXEC SQL INCLUDE TCTCAT   END-EXEC.                                  
           EXEC SQL INCLUDE TCTCATD  END-EXEC.                                  
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      *    HELPFUL DEBUGGING DATA.                                     *        
      *----------------------------------------------------------------*        
                                                                                
       01  WS-ABEND-INFO.                                                       
           05 WS-DUMP-CODE.                                                     
              10 WS-DUMP-C1        PIC X(2)  VALUE SPACES.                      
              10 WS-DUMP-C2        PIC X(2)  VALUE SPACES.                      
           05 FILLER               PIC X(18) VALUE '*** ABEND REASON: '.        
           05 WS-ABEND-REASON      PIC 9(13) VALUE ZERO.                        
           05  WS-ERROR-TEXT       PIC X(50).                                   
                                                                                
       01  WS-WORK-FIELDS.                                                      
           05  WS-DB2-ERROR-SW    PIC  X(01) VALUE 'N'.                         
               88  DB2-ERROR-SW-ON  VALUE 'Y'.                                  
               88  DB2-ERROR-SW-OFF VALUE 'N'.                                  
           05  WS-END-SW    PIC  X(01) VALUE 'N'.                               
               88  END-SW-ON  VALUE 'Y'.                                        
               88  END-SW-OFF VALUE 'N'.                                        
           05  WS-SPACES         PIC X(30)    VALUE SPACES.                     
           05  OFF-VALUE         PIC X VALUE 'N'.                               
           05  ON-VALUE          PIC X VALUE 'Y'.                               
           05  WS-SQLCODE-NUMERIC  PIC 9(09).                                   
           05  WS-SQLCODE-ALPHA    REDEFINES WS-SQLCODE-NUMERIC.                
               10  WS-SQLCODE-1-5  PIC X(05).                                   
               10  WS-SQLCODE-6-9  PIC X(04).                                   
                                                                                
           05  WS-PAGE-CNT         PIC 9(3) COMP-3.                             
           05  WS-LINE-CNT         PIC 9(3) COMP-3.                             
           05  WS-MAX-LINES        PIC 9(3) COMP-3 VALUE 56.                    
                                                                                
           05  WS-OLD-KEY.                                                      
               10  WS-OLD-DEPT      PIC X(50).                                  
               10  WS-OLD-USER-KEY.                                             
                   15  WS-OLD-LAST-NAME PIC X(30).                              
                   15  WS-OLD-USER-ID   PIC X(10).                              
               10  WS-OLD-ACCESS        PIC X(50).                              
           05  WS-NEW-KEY.                                                      
               10  WS-NEW-DEPT      PIC X(50).                                  
               10  WS-NEW-USER-KEY.                                             
                   15  WS-NEW-LAST-NAME PIC X(30).                              
                   15  WS-NEW-USER-ID   PIC X(10).                              
               10  WS-NEW-ACCESS        PIC X(50).                              
                                                                                
           05  WS-FETCH-CNT     PIC 9(9) COMP-3 VALUE ZEROES.                   
                                                                                
       01  WS-HDR1.                                                             
           05  FILLER            PIC X(30) VALUE SPACES.                        
           05  FILLER            PIC X(65) VALUE 'CPM AUDIT REPORT'.            
           05  FILLER            PIC X(5)  VALUE 'PAGE '.                       
           05  WS-HDR1-PAGE      PIC Z(2)9.                                     
       01  WS-HDR2.                                                             
           05  FILLER            PIC X(1) VALUE SPACE.                          
           05  FILLER            PIC X(103) VALUE SPACES.                       
           05  FILLER            PIC X(10) VALUE 'GACPAUDR'.                    
       01  WS-HDR3.                                                             
           05  FILLER            PIC X(1) VALUE SPACE.                          
           05  FILLER            PIC X(12) VALUE 'DEPARTMENT: '.                
           05  HDR3-DEPT         PIC X(50).                                     
       01  WS-HDR4.                                                             
           05  FILLER            PIC X(1) VALUE SPACE.                          
           05  FILLER            PIC X(31) VALUE 'LAST NAME'.                   
           05  FILLER            PIC X(31) VALUE 'FIRST NAME'.                  
           05  FILLER            PIC X(12) VALUE 'USER ID'.                     
           05  FILLER            PIC X(12) VALUE 'AUTHORITY'.                   
       01  WS-DTL.                                                              
           05  FILLER             PIC X(1).                                     
           05  WS-DTL-LAST-NAME   PIC X(31).                                    
           05  WS-DTL-FIRST-NAME  PIC X(31).                                    
           05  WS-DTL-USER-ID     PIC X(12).                                    
           05  WS-DTL-ACCESS      PIC X(50).                                    
                                                                                
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
      *------------------------------------------------------------*            
      * PROGRAM MAINLINE                                                        
      *------------------------------------------------------------*            
           OPEN OUTPUT AUDIT-REPORT-OUT.                                        
           MOVE LOW-VALUES TO WS-NEW-KEY.                                       
                                                                                
           PERFORM 2100-PROCESS-RTN                                             
              THRU 2100-EXIT.                                                   
                                                                                
           IF DB2-ERROR-SW-ON                                                   
              DISPLAY '*** DB2 ERROR DETECTED'                                  
              DISPLAY '*** DB2 ABEND REASON: ' WS-ABEND-REASON                  
              DISPLAY WS-ERROR-TEXT                                             
           END-IF.                                                              
                                                                                
           CLOSE AUDIT-REPORT-OUT.                                              
           DISPLAY '********* DB2 COUNTS **********'                            
           DISPLAY 'FETCHED ROWS = ' WS-FETCH-CNT.                              
                                                                                
           GOBACK.                                                              
                                                                                
                                                                                
       2100-PROCESS-RTN.                                                        
                                                                                
           EXEC SQL                                                             
             DECLARE TCTDPTCUR CURSOR FOR                                       
              SELECT   A.CODE_VALUE                                             
                     , A.CODE_ENG_DESC                                          
                     , B.CPM_LAST_NAME                                          
                     , B.CPM_FIRST_NAME                                         
                     , B.CPM_USER_ID                                            
                     , C.CPM_ACC_TYP_CD                                         
                     , D.CODE_ENG_DESC                                          
               FROM  TCTDPT A                                                   
                   , TCPMUS B                                                   
                   , TCPMU  C                                                   
                   , TCTCAT D                                                   
               WHERE A.CODE_VALUE = B.DEPT                                      
                 AND B.CPM_USER_ID = C.CPM_USER_ID                              
                 AND C.CPM_USER_STAT_CD = 'Y'                                   
                 AND C.CPM_ACC_TYP_CD = D.CODE_VALUE                            
               UNION                                                            
               SELECT  :WS-SPACES                                               
                      ,:WS-SPACES                                               
                      ,:WS-SPACES                                               
                      ,:WS-SPACES                                               
                      ,C.CPM_USER_ID                                            
                      ,C.CPM_ACC_TYP_CD                                         
                      ,D.CODE_ENG_DESC                                          
               FROM  TCTDPT A                                                   
                   , TCPMUS B                                                   
                   , TCPMU  C                                                   
                   , TCTCAT D                                                   
               WHERE C.CPM_USER_ID NOT IN                                       
                     (SELECT B.CPM_USER_ID                                      
                      FROM TCPMUS B)                                            
                 AND C.CPM_USER_STAT_CD = 'Y'                                   
                 AND C.CPM_ACC_TYP_CD = D.CODE_VALUE                            
               ORDER BY 1 ASC                                                   
                      , 3 ASC                                                   
                      , 5 ASC                                                   
                      , 6 ASC                                                   
           END-EXEC.                                                            
                                                                                
                                                                                
           EXEC SQL                                                             
               OPEN TCTDPTCUR                                                   
           END-EXEC.                                                            
                                                                                
                                                                                
           PERFORM 2200-FETCH-DEPT                                              
              THRU 2200-EXIT                                                    
              UNTIL END-SW-ON.                                                  
                                                                                
           IF  DB2-ERROR-SW-ON                                                  
               GO TO 2100-EXIT                                                  
           END-IF.                                                              
                                                                                
           EXEC SQL                                                             
               CLOSE TCTDPTCUR                                                  
           END-EXEC.                                                            
                                                                                
           IF  SQLCODE NOT EQUAL +0                                             
               MOVE ON-VALUE          TO WS-DB2-ERROR-SW                        
               MOVE SQLCODE             TO WS-SQLCODE-NUMERIC                   
               MOVE WS-SQLCODE-6-9      TO WS-ABEND-REASON                      
               MOVE 'TCTDPT CLOSE 2100'  TO WS-ERROR-TEXT                       
           END-IF.                                                              
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2200-FETCH-DEPT.                                                         
                                                                                
      *------------------------------------------------------------*            
      * FETCH EACH DEPT ROW                                                     
      *------------------------------------------------------------*            
           MOVE WS-NEW-KEY TO WS-OLD-KEY.                                       
           EXEC SQL                                                             
               FETCH TCTDPTCUR                                                  
               INTO                                                             
                 :DCLTCTDPT.CODE-VALUE                                          
               , :DCLTCTDPT.CODE-ENG-DESC                                       
               , :DCLTCPMUS.CPM-LAST-NAME                                       
               , :DCLTCPMUS.CPM-FIRST-NAME                                      
               , :DCLTCPMUS.CPM-USER-ID                                         
               , :DCLTCPMU.CPM-ACC-TYP-CD                                       
               , :DCLTCTCAT.CODE-ENG-DESC                                       
           END-EXEC.                                                            
                                                                                
           IF  SQLCODE EQUAL +100                                               
               MOVE ON-VALUE  TO WS-END-SW                                      
               GO TO 2200-EXIT                                                  
           END-IF.                                                              
                                                                                
           IF  SQLCODE NOT = +0                                                 
               MOVE ON-VALUE            TO WS-DB2-ERROR-SW                      
               MOVE SQLCODE             TO WS-SQLCODE-NUMERIC                   
               MOVE WS-SQLCODE-6-9      TO WS-ABEND-REASON                      
               MOVE 'TCTDPT FETCH 2200' TO WS-ERROR-TEXT                        
               GO TO 2200-EXIT                                                  
           END-IF.                                                              
                                                                                
           ADD 1 TO WS-FETCH-CNT.                                               
                                                                                
           MOVE CODE-ENG-DESC OF DCLTCTDPT TO WS-NEW-DEPT                       
           MOVE CPM-LAST-NAME OF DCLTCPMUS TO WS-NEW-LAST-NAME                  
           MOVE CPM-USER-ID   OF DCLTCPMUS TO WS-NEW-USER-ID                    
           MOVE CODE-ENG-DESC OF DCLTCTCAT TO WS-NEW-ACCESS                     
                                                                                
                                                                                
           IF WS-NEW-DEPT NOT = WS-OLD-DEPT                                     
              MOVE ZEROES TO WS-PAGE-CNT                                        
              PERFORM 2220-PRINT-DEPT-HEADING                                   
                 THRU 2220-EXIT                                                 
           END-IF.                                                              
                                                                                
           IF WS-LINE-CNT > WS-MAX-LINES                                        
              PERFORM 2220-PRINT-DEPT-HEADING                                   
                 THRU 2220-EXIT                                                 
           END-IF.                                                              
                                                                                
           MOVE SPACES TO WS-DTL.                                               
                                                                                
           IF WS-NEW-USER-KEY NOT = WS-OLD-USER-KEY                             
              MOVE WS-NEW-LAST-NAME TO WS-DTL-LAST-NAME                         
              MOVE WS-NEW-USER-ID   TO WS-DTL-USER-ID                           
              MOVE CPM-FIRST-NAME OF DCLTCPMUS TO WS-DTL-FIRST-NAME             
              MOVE WS-NEW-ACCESS TO WS-DTL-ACCESS                               
              WRITE AUDIT-REPORT-RECORD FROM WS-DTL                             
                 AFTER ADVANCING 2 LINES                                        
              ADD 2 TO WS-LINE-CNT                                              
              GO TO 2200-EXIT                                                   
           END-IF.                                                              
                                                                                
           IF WS-LINE-CNT = 7                                                   
              MOVE WS-NEW-LAST-NAME TO WS-DTL-LAST-NAME                         
              MOVE WS-NEW-USER-ID   TO WS-DTL-USER-ID                           
              MOVE CPM-FIRST-NAME OF DCLTCPMUS TO WS-DTL-FIRST-NAME             
           END-IF.                                                              
                                                                                
           MOVE WS-NEW-ACCESS  TO WS-DTL-ACCESS                                 
           WRITE AUDIT-REPORT-RECORD FROM WS-DTL                                
           ADD 1 TO WS-LINE-CNT.                                                
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2220-PRINT-DEPT-HEADING.                                                 
                                                                                
           ADD 1 TO WS-PAGE-CNT.                                                
           MOVE WS-PAGE-CNT TO WS-HDR1-PAGE.                                    
           MOVE WS-NEW-DEPT TO  HDR3-DEPT.                                      
           WRITE AUDIT-REPORT-RECORD FROM WS-HDR1                               
                 AFTER ADVANCING NEW-PAGE                                       
           WRITE AUDIT-REPORT-RECORD FROM WS-HDR2                               
                 AFTER ADVANCING 2 LINES.                                       
           WRITE AUDIT-REPORT-RECORD FROM WS-HDR3                               
                 AFTER ADVANCING 1 LINES.                                       
           WRITE AUDIT-REPORT-RECORD FROM WS-HDR4                               
                 AFTER ADVANCING 2 LINES                                        
           MOVE SPACES TO AUDIT-REPORT-RECORD                                   
           WRITE AUDIT-REPORT-RECORD                                            
                 AFTER ADVANCING 1 LINES                                        
           MOVE 7 TO WS-LINE-CNT.                                               
                                                                                
       2220-EXIT.                                                               
           EXIT.                                                                
                                                                                
