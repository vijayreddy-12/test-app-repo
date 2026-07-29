       IDENTIFICATION DIVISION.                                                 
      *=======================*                                                 
       PROGRAM-ID.   GACBMAS7.                                                  
       AUTHOR.       AL TAYLOR.                                                 
       DATE-WRITTEN. FEB 2013.                                                  
      *****************************************************************         
      *****************************************************************         
      *                                                               *         
      * THIS PROGRAM MANAGES INPUT-OUTPUT OPERATIONS                  *         
      * TO THE GIPSY MASTER FILE; THE FOLLOWING OPERATIONS ARE        *         
      * SUPPORTED:                                                    *         
      *  - READ DIRECT/KEYED FOR A SPECIFIC GROUP/ACCOUNT             *         
      *  - SKIP-SEQUENTIAL FOR A SINGLE GROUP                         *         
      *  - SKIP-SEQUENTIAL STARTING AT A SPECIFIC GROUP THROUGH       *         
      *    TO THE END OF THE FILE                                     *         
      *                                                               *         
      * POINT-IN-SCALE RECORDS (ACCOUNT = HIGH VALUES) ARE BYPASSED   *         
      * DURING SEQUENTIAL PROCESSING; I.E.: THEY CAN ONLY             *         
      * BE RETRIEVED BY READ DIRECT/KEYED OPERATIONS                  *         
      *                                                               *         
      * MAINTENANCE HISTORY                                           *         
      * -------------------                                           *         
      *                                                               *         
      * DATE       AUTHOR    DESCRIPTION                              *         
      * SEPT.2018  SANGKYUN  RECOMPILED TO APPLY PSMASTE7 CHANGES     *         
      * --------------------------------                              *         
      *****************************************************************         
      *****************************************************************         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
                                                                                
           SELECT GACBMAS ASSIGN TO GACBMAS7                                    
                  ORGANIZATION IS INDEXED                                       
                  ACCESS IS DYNAMIC                                             
                  RECORD KEY IS MASTER-KEY OF MASTER-REC                        
                  FILE STATUS IS GACBMAS-STATUS                                 
                                 GACBMAS-VSAM-STATUS.                           
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
                                                                                
       FD GACBMAS                                                               
            RECORD IS VARYING IN SIZE FROM 7 TO 16276                           
            DEPENDING ON WS-RECLEN.                                             
                                                                                
       01  MASTER-REC.                                                          
           05  MASTER-KEY.                                                      
               10  MASTER-GROUP      COMP-3 PIC S9(7).                          
               10  MASTER-ACCOUNT           PIC X(03).                          
           05  MASTER-DATA  PIC X(16269).                                       
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WMISC-STORAGE.                                                       
           05  GACBMAS-OPEN-STATUS         PIC X(01) VALUE 'C'.                 
               88  GACBMAS-IS-OPEN         VALUE 'O'.                           
               88  GACBMAS-IS-CLOSED       VALUE 'C'.                           
           05  WS-PREV-KEY.                                                     
               10  WS-PREV-GROUP     COMP-3 PIC S9(07) VALUE ZERO.              
               10  WS-PREV-ACCT             PIC X9(03) VALUE SPACES.            
           05  WS-PREV-FUNC         COMP   PIC S9(4) VALUE ZERO.                
               88 PREV-FUNC-READ-GROUP-ACCTS         VALUE +3, +19.             
               88 PREV-FUNC-READ-DIRECT              VALUE +2, +18.             
           05  WS-PREV-006-GROUP-ACCOUNT.                                       
               10  WS-PREV-0006-GROUP     COMP-3 PIC S9(07) VALUE ZERO.         
               10  WS-PREV-0006-ACCT       PIC X9(03) VALUE SPACES.             
           05  WS-PREV-0003-ACCT           PIC X9(03) VALUE SPACES.             
           05  WS-RECLEN                   PIC 9(8).                            
           05  GACBMAS-STATUS               PIC X(2)  VALUE '00'.               
               88  GACBMAS-OK              VALUE '00', '02', '41'.              
               88  GACBMAS-END-OF-FILE     VALUE '10'.                          
               88  GACBMAS-REC-NOT-FND     VALUE '23'.                          
           05  GACBMAS-VSAM-STATUS.                                             
               10  GASBMAS-VSAM-RETURN     COMP    PIC S9(4).                   
               10  GACBMAS-VSAM-FUNCTION   COMP    PIC S9(4).                   
               10  GACBMAS-VSAM-FEEDBACK   COMP    PIC S9(4).                   
                                                                                
      *****************************************************************         
      * POINT-IN-SCALE RECORD DEFINITION; THIS IS REQUIRED SO THAT    *         
      * WE CAN INITIALIZE THE OUTPUT AREA WHEN THE PIS RECORD         *         
      * IS NOT FOUND ON A READ DIRECT REQUEST                         *         
      *****************************************************************         
       COPY PSMASTE7.                                                           
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  GACBMAS7-PARMS.                                                      
           03  GACBMAS7-FUNC         COMP   PIC S9(4).                          
               88 GACBMAS7-FUNC-CLOSE                  VALUE +1.                
               88 GACBMAS7-FUNC-READ-DIRECT            VALUE +2, +18.           
               88 GACBMAS7-FUNC-READ-GROUP-ACCTS       VALUE +3, +19.           
               88 GACBMAS7-FUNC-READ-TO-END            VALUE +6, +22.           
           03  GACBMAS7-SEARCH-KEY.                                             
               05  GACBMAS7-GROUP    COMP-3 PIC S9(7).                          
               05  GACBMAS7-ACCT            PIC X(3).                           
               05  FILLER            COMP-3 PIC S9(9).                          
           03  GACBMAS7-WORK                PIC X(9).                           
       01  GACBMAS7-PARMS-CHECK REDEFINES GACBMAS7-PARMS.                       
           03  F                            PIC X(2).                           
           03  CHECK-SEARCH-KEY.                                                
               05  CHECK-GROUP              PIC X(4).                           
               05  F                        PIC X(3).                           
           03  F                            PIC X(9).                           
                                                                                
       01  GACBMAS7-RETURN                  PIC X(3)   VALUE SPACES.            
           88  GACBMAS7-RET-OK                         VALUE '   '.             
           88  GACBMAS7-EOF                            VALUE 'R01'.             
           88  GACBMAS7-NOT-FOUND                      VALUE 'R01'.             
           88  GACBMAS7-DISK-ERROR                     VALUE 'R03'.             
           88  GACBMAS7-DUP-RECORD                     VALUE 'R08'.             
           88  GACBMAS7-FUNC-INVALID                   VALUE 'R09'.             
                                                                                
       COPY GMASTER7.                                                           
                                                                                
       PROCEDURE DIVISION USING GACBMAS7-PARMS                                  
                                GACBMAS7-RETURN                                 
                                GMASTER7.                                       
                                                                                
       A100-CONTROL.                                                            
      *****************************************************************         
      *****************************************************************         
      * CONTROLLER                                                    *         
      *****************************************************************         
      *****************************************************************         
           SET GACBMAS7-RET-OK TO TRUE.                                         
                                                                                
           IF GACBMAS7-FUNC-CLOSE                                               
           OR GACBMAS7-FUNC-READ-DIRECT                                         
           OR GACBMAS7-FUNC-READ-GROUP-ACCTS                                    
           OR GACBMAS7-FUNC-READ-TO-END                                         
              PERFORM B100-INIT THRU B100-EXIT                                  
              IF  ( NOT GACBMAS7-FUNC-CLOSE )                                   
              AND ( GACBMAS7-RET-OK )                                           
                    PERFORM C100-PROCESS THRU C100-EXIT                         
              END-IF                                                            
           ELSE                                                                 
              SET GACBMAS7-FUNC-INVALID TO TRUE                                 
           END-IF.                                                              
                                                                                
           GOBACK.                                                              
                                                                                
       A100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       B100-INIT.                                                               
      *****************************************************************         
      *****************************************************************         
      * IF THE REQUEST IS TO CLOSE THE FILE THEN CLOSE                *         
      * OTHERWISE OPEN THE FILE IF NECESSARY                          *         
      *****************************************************************         
      *****************************************************************         
                                                                                
           SET GACBMAS-OK TO TRUE.                                              
                                                                                
           IF CHECK-GROUP = LOW-VALUES                                          
              MOVE ZEROS TO GACBMAS7-GROUP                                      
           END-IF.                                                              
                                                                                
           IF  GACBMAS7-FUNC-CLOSE                                              
                IF GACBMAS-IS-OPEN                                              
                   CLOSE GACBMAS                                                
                   SET GACBMAS-IS-CLOSED TO TRUE                                
                   IF NOT GACBMAS-OK                                            
                      SET GACBMAS7-DISK-ERROR TO TRUE                           
                   END-IF                                                       
                END-IF                                                          
           ELSE                                                                 
              IF GACBMAS-IS-CLOSED                                              
                 PERFORM B120-OPEN THRU B120-EXIT                               
                 IF GACBMAS-OK                                                  
                    SET GACBMAS-IS-OPEN TO TRUE                                 
                 END-IF                                                         
              END-IF                                                            
           END-IF.                                                              
                                                                                
       B100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       B120-OPEN.                                                               
      *****************************************************************         
      *****************************************************************         
      * THIS PARAGRAPH OPENS THE FILE                                 *         
      *****************************************************************         
      *****************************************************************         
           OPEN INPUT GACBMAS.                                                  
                                                                                
           IF NOT GACBMAS-OK                                                    
              SET GACBMAS7-DISK-ERROR TO TRUE                                   
           END-IF.                                                              
                                                                                
       B120-EXIT.                                                               
           EXIT.                                                                
                                                                                
       C100-PROCESS.                                                            
      *****************************************************************         
      *****************************************************************         
      * THIS PARAGRAPH EVALUATES THE REQUEST AND RESPONDS             *         
      * TO READ DIRECT/KEYED REQUESTS                                 *         
      *****************************************************************         
      *****************************************************************         
           EVALUATE TRUE                                                        
                                                                                
           WHEN GACBMAS7-FUNC-READ-DIRECT                                       
                                                                                
                MOVE GACBMAS7-GROUP   TO MASTER-GROUP                           
                MOVE GACBMAS7-ACCT    TO MASTER-ACCOUNT                         
                READ GACBMAS KEY IS MASTER-KEY OF MASTER-REC                    
                                                                                
      *****************************************************************         
      * IF THE POINT-IN-SCALE RECORD IS NOT FOUND THEN INITIALIZE     *         
      * THE INTERNAL AREA PSMASTE7 AND MOVE IT TO THE OUTPUT BUFFER   *         
      *****************************************************************         
                IF GACBMAS-OK                                                   
                   PERFORM U100-RETURN-DATA THRU U100-EXIT                      
                ELSE   IF GACBMAS-REC-NOT-FND                                   
                          SET GACBMAS7-NOT-FOUND TO TRUE                        
                          IF GACBMAS7-ACCT = HIGH-VALUES                        
                             INITIALIZE PSMASTE7                                
                             MOVE PSMASTE7                                      
                             TO   GMASTER7(1: LENGTH OF PSMASTE7)               
                          END-IF                                                
                       ELSE                                                     
                          SET GACBMAS7-DISK-ERROR TO TRUE                       
                       END-IF                                                   
                END-IF                                                          
                                                                                
           WHEN GACBMAS7-FUNC-READ-GROUP-ACCTS                                  
                                                                                
                PERFORM D100-READ-ACCOUNTS-FOR-GROUP THRU D100-EXIT             
                                                                                
           WHEN GACBMAS7-FUNC-READ-TO-END                                       
                                                                                
                PERFORM E100-READ-ACCOUNTS-TO-END THRU E100-EXIT                
                                                                                
           WHEN OTHER                                                           
                                                                                
                SET GACBMAS7-FUNC-INVALID TO TRUE                               
                                                                                
           END-EVALUATE.                                                        
                                                                                
           MOVE GACBMAS7-FUNC  TO WS-PREV-FUNC.                                 
           MOVE GACBMAS7-GROUP TO WS-PREV-GROUP.                                
           MOVE GACBMAS7-ACCT  TO WS-PREV-ACCT.                                 
                                                                                
      *****************************************************************         
      * IF WE ARE READING FROM A START GROUP ALL THE WAY TO THE END   *         
      * SAVE THE CURRENT GROUP/ACCOUNT IN CASE WE NEED TO RECOVER     *         
      * FILE POSITION FROM A NESTED FUNCTION CODE CHANGE; E.G.:       *         
      *   KEXEMAIN HAS AN OUTER LOOP OF 0006 CALLS WITH NEXTED        *         
      *            0003 AND 0018 CALLS                                *         
      *****************************************************************         
           IF  GACBMAS7-FUNC-READ-TO-END                                        
           AND GACBMAS-OK                                                       
              MOVE GGIGROUP-NUMBER TO WS-PREV-0006-GROUP                        
              MOVE GGIACCOUNT      TO WS-PREV-0006-ACCT                         
           END-IF.                                                              
                                                                                
       C100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       D100-READ-ACCOUNTS-FOR-GROUP.                                            
      *****************************************************************         
      *****************************************************************         
      * THIS PARAGRAPH RESPONDS TO THE REQUEST TO RETURN EACH ACCOUNT *         
      * IN TURN FOR A SPECIFIC GROUP                                  *         
      *                                                               *         
      * IF THE GROUP OR FUNCTION HAS CHANGED OR WE ARE REPEATING      *         
      * A SWEEP OF ACCOUNTS FOR THE SAME GROUP THEN WE WANT GET       *         
      * TO THE FIRST (LOWEST ACCOUNT) FOR THE NEW OR REPEATED GROUP   *         
      *****************************************************************         
      *****************************************************************         
           IF  GACBMAS7-GROUP        = WS-PREV-GROUP                            
           AND GACBMAS7-FUNC         = WS-PREV-FUNC                             
           AND WS-PREV-0003-ACCT NOT = HIGH-VALUES                              
              READ GACBMAS NEXT                                                 
           ELSE                                                                 
              MOVE GACBMAS7-GROUP TO MASTER-GROUP                               
              MOVE LOW-VALUES     TO MASTER-ACCOUNT                             
              MOVE LOW-VALUES     TO WS-PREV-0003-ACCT                          
              START GACBMAS KEY IS >= MASTER-KEY                                
              IF NOT GACBMAS-OK                                                 
                 SET GACBMAS7-NOT-FOUND TO TRUE                                 
              ELSE                                                              
                 READ GACBMAS NEXT                                              
              END-IF                                                            
           END-IF.                                                              
                                                                                
      *****************************************************************         
      * WE KNOW THAT THE POINT-IN-SCALE RECORD IS THE LAST IN THE     *         
      * SET OF RECORDS FOR THE GROUP SO IF WE ENCOUNTER ONE           *         
      * (THEY ARE OPTIONAL) WE ARE DONE WITH THE CURRENT GROUP        *         
      *****************************************************************         
           IF  GACBMAS-OK                                                       
           AND MASTER-GROUP       = GACBMAS7-GROUP                              
           AND MASTER-ACCOUNT NOT = HIGH-VALUES                                 
               PERFORM U100-RETURN-DATA THRU U100-EXIT                          
           ELSE                                                                 
               SET  GACBMAS7-NOT-FOUND TO TRUE                                  
               MOVE HIGH-VALUES TO WS-PREV-0003-ACCT                            
           END-IF.                                                              
                                                                                
       D100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       E100-READ-ACCOUNTS-TO-END.                                               
      *****************************************************************         
      *****************************************************************         
      * THIS PARAGRAPH RESPONDS TO THE REQUEST TO RETURN EACH         *         
      * GROUP/ACCOUNT IN TURN STARTING AT A SPECIFIC GROUP/ACCOUNT    *         
      * AND THROUGH TO THE END OF FILE                                *         
      *****************************************************************         
      *****************************************************************         
                                                                                
           EVALUATE TRUE                                                        
      *****************************************************************         
      *****************************************************************         
      * SAME FUNCTION, NO CHANGE IN THE GROUP PARM                    *         
      *****************************************************************         
      *****************************************************************         
              WHEN GACBMAS7-FUNC      = WS-PREV-FUNC                            
                   AND GACBMAS7-GROUP = WS-PREV-GROUP                           
                   MOVE HIGH-VALUES   TO MASTER-ACCOUNT                         
                   PERFORM UNTIL MASTER-ACCOUNT NOT = HIGH-VALUES               
                                 OR NOT GACBMAS-OK                              
                       READ GACBMAS NEXT                                        
                   END-PERFORM                                                  
                                                                                
      *****************************************************************         
      *****************************************************************         
      * NESTED FUNCTION CODE SWITCHES                                 *         
      *                                                               *         
      * E.G.: KXEXMAIN DOES AN OUTER ITERATION USING FNCTION 0006 TO  *         
      *       READ THRU ALL GRACTS IN THE FILE BUT FOR EACH GRACT     *         
      *       DOES AN INNER ITERATION THRU THE ACCOUNTS IN THE GROUP  *         
      *       USING FUNCTION CODE 0019; TO THIS SECTION IS REQUIRED   *         
      *       TO DEAL WITH THE SWITCH FROM 0019 BACK TO 0006          *         
      *****************************************************************         
      *****************************************************************         
              WHEN (    PREV-FUNC-READ-GROUP-ACCTS                              
                     OR PREV-FUNC-READ-DIRECT )                                 
                                                                                
      *****************************************************************         
      * USE THE GROUP/ACCOUNT RETURNED BY THE LAST 0006 CALL          *         
      *****************************************************************         
                MOVE WS-PREV-0006-GROUP TO MASTER-GROUP                         
                MOVE WS-PREV-0006-ACCT  TO MASTER-ACCOUNT                       
                                                                                
                START GACBMAS KEY IS >        MASTER-KEY                        
                IF NOT GACBMAS-OK                                               
                   SET GACBMAS7-NOT-FOUND TO TRUE                               
                ELSE                                                            
                   READ GACBMAS NEXT                                            
                   PERFORM UNTIL                                                
                      MASTER-ACCOUNT NOT = HIGH-VALUES                          
                      OR NOT GACBMAS-OK                                         
                         READ GACBMAS NEXT                                      
                   END-PERFORM                                                  
                END-IF                                                          
                                                                                
      *****************************************************************         
      *****************************************************************         
      * DEFAULT - START NEW 0006 STREAM...                            *         
      *                                                               *         
      * CALLER SENDS A SPECIFIC GROUP/ACCOUNT, START AND LOCATE       *         
      * FIRST NON-PIS GROUP/ACCOUNT WITH KEY EQUAL TO OR GREATER      *         
      * THE SPECIFIED GROUP/ACCOUNT                                   *         
      *                                                               *         
      * TO GET THE DATE RECORD ON A SEQUENTIAL READ THE CALLER        *         
      * IS EXPECTED TO PUT ZEROS IN GACBMAS7-GROUP AND                *         
      * LOW-VALUES IN GACBMAS7-ACCT                                   *         
      *****************************************************************         
      *****************************************************************         
              WHEN OTHER                                                        
                                                                                
                MOVE GACBMAS7-GROUP   TO MASTER-GROUP                           
                MOVE GACBMAS7-ACCT    TO MASTER-ACCOUNT                         
                START GACBMAS KEY IS >= MASTER-KEY                              
                IF NOT GACBMAS-OK                                               
                   SET GACBMAS7-NOT-FOUND TO TRUE                               
                ELSE                                                            
                   READ GACBMAS NEXT                                            
                   PERFORM UNTIL MASTER-ACCOUNT NOT = HIGH-VALUES               
                           OR NOT GACBMAS-OK                                    
                      READ GACBMAS NEXT                                         
                   END-PERFORM                                                  
                END-IF                                                          
                                                                                
              END-EVALUATE.                                                     
                                                                                
           IF  GACBMAS-OK                                                       
               PERFORM U100-RETURN-DATA THRU U100-EXIT                          
           ELSE                                                                 
               SET GACBMAS7-NOT-FOUND TO TRUE                                   
           END-IF.                                                              
                                                                                
       E100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       U100-RETURN-DATA.                                                        
      *****************************************************************         
      *****************************************************************         
      * THIS PARAGRAPH RETURNS THE RETRIEVED MASTER RECORD AND ITS    *         
      * LENGTH TO THE CALLING PROGRAM                                 *         
      *****************************************************************         
      *****************************************************************         
           MOVE WS-RECLEN                TO GGIRECORD-LENGTH.                   
           MOVE MASTER-REC(1:WS-RECLEN)  TO GMASTER7 (5:WS-RECLEN).             
                                                                                
       U100-EXIT.                                                               
           EXIT.                                                                
