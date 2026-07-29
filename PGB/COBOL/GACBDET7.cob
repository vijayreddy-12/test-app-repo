       IDENTIFICATION DIVISION.                                                 
      *=======================*                                                 
       PROGRAM-ID.   GACBDET7.                                                  
       AUTHOR.       LIVINGSTONE INBARAJ.                                       
       DATE-WRITTEN. FEB 2013.                                                  
      *****************************************************************         
      *****************************************************************         
      *                                                               *         
      * THIS PROGRAM MANAGES READ OPERATIONS                          *         
      * ON THE GIPSY DETAIL FILE                                      *         
      *                                                               *         
      * MAINTENANCE HISTORY                                           *         
      * -------------------                                           *         
      *                                                               *         
      * DATE       AUTHOR    DESCRIPTION                              *         
      * --------------------------------                              *         
      *****************************************************************         
      *****************************************************************         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
                                                                                
           SELECT GACBDET ASSIGN TO GACBDET7                                    
                  ORGANIZATION IS INDEXED                                       
                  ACCESS IS DYNAMIC                                             
                  RECORD KEY IS DETAIL-KEY OF DETAIL-REC                        
                  FILE STATUS IS GACBDET-FILE-STATUS                            
                                 GACBDET-VSAM-STATUS.                           
                                                                                
           SELECT GACBAIX ASSIGN TO GACBAIX7                                    
                  ORGANIZATION IS INDEXED                                       
                  ACCESS IS DYNAMIC                                             
                  RECORD KEY IS DETAIL-ALTKEY OF DETAIL-ALTKEY-REC              
                  FILE STATUS IS GACBAIX-FILE-STATUS                            
                                 GACBAIX-VSAM-STATUS.                           
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
                                                                                
       FD GACBDET                                                               
            RECORD IS VARYING IN SIZE FROM 13 TO 8676                           
            DEPENDING ON WS-DETAIL-RECLEN.                                      
                                                                                
       01  DETAIL-REC.                                                          
             10  DETAIL-KEY.                                                    
                 15  DETAIL-GROUP      COMP-3 PIC S9(7).                        
                 15  DETAIL-ACCOUNT           PIC X(03).                        
                 15  DETAIL-IDENT             PIC X(01).                        
                 15  DETAIL-CERT       COMP-3 PIC S9(9).                        
             10  DETAIL-DATA  PIC X(8659).                                      
                                                                                
       FD GACBAIX                                                               
            RECORD IS VARYING IN SIZE FROM 13 TO 8676                           
            DEPENDING ON WS-DETAIX-RECLEN.                                      
                                                                                
       01  DETAIL-ALTKEY-REC.                                                   
             10  FILLER                       PIC X(08).                        
             10  DETAIL-ALTKEY.                                                 
                 15  ALTKEY-CERT       COMP-3 PIC S9(9).                        
                 15  ALTKEY-GROUP      COMP-3 PIC S9(7).                        
             10  DETAIL-DATA  PIC X(8655).                                      
                                                                                
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-GDETEXP7.                                                         
               10  WS-GDET-RECLEN                  PIC S9(4) COMP.              
               10  FILLER                          PIC X(2).                    
               10  WS-GDET-RECORD                  PIC X(8672).                 
                                                                                
       01  WS-DETAIL-ALTKEY-REC-NEXT.                                           
             10  FILLER                       PIC X(08).                        
             10  WS-DETAIL-ALTKEY-NXT.                                          
                 15 WS-ALTKEY-CERT-NXT   COMP-3 PIC S9(9).                      
                 15 WS-ALTKEY-GROUP-NXT  COMP-3 PIC S9(7).                      
             10  WS-DETAIL-DATA-NXT             PIC X(8655).                    
                                                                                
       01  WS-MISC-STORAGE.                                                     
           05  GACBDET-OPEN-STATUS         PIC X(01) VALUE 'C'.                 
               88  GACBDET-IS-OPEN         VALUE 'O'.                           
               88  GACBDET-IS-CLOSED       VALUE 'C'.                           
           05  GACBAIX-OPEN-STATUS         PIC X(01) VALUE 'C'.                 
               88  GACBAIX-IS-OPEN         VALUE 'O'.                           
               88  GACBAIX-IS-CLOSED       VALUE 'C'.                           
           05  WS-DETAIL-RECLEN          PIC 9(4) COMP.                         
           05  WS-DETAIX-RECLEN          PIC 9(4) COMP.                         
           05  WS-DETAIX-RECLEN-NEXT     PIC 9(4) COMP.                         
           05  WS-LAST-FUNC    COMP   PIC S9(4)  VALUE +0.                      
           05  WS-LAST-SEARCH-KEY.                                              
               10  WS-LAST-GROUP    COMP-3 PIC S9(7).                           
               10  WS-LAST-ACCT            PIC X(3)   VALUE LOW-VALUES.         
               10  WS-LAST-CERT     COMP-3 PIC S9(9).                           
           05  WS-LAST-GACBDET7-RETURN     PIC X(3)   VALUE SPACES.             
           05  GACBDET-FILE-STATUS         PIC X(2)  VALUE '00'.                
               88  GACBDET-OK              VALUE '00', '02', '97'.              
               88  GACBDET-END-OF-FILE     VALUE '10'.                          
               88  GACBDET-REC-NOT-FND     VALUE '23'.                          
               88  GACBDET-NOT-OPEN        VALUE '42'.                          
               88  GACBDET-ALREADY-OPEN    VALUE '41'.                          
           05  GACBAIX-FILE-STATUS         PIC X(2)  VALUE '00'.                
               88  GACBAIX-OK              VALUE '00', '02', '97'.              
               88  GACBAIX-END-OF-FILE     VALUE '10'.                          
               88  GACBAIX-REC-NOT-FND     VALUE '23'.                          
               88  GACBAIX-NOT-OPEN        VALUE '42'.                          
               88  GACBAIX-ALREADY-OPEN    VALUE '41'.                          
           05  GACBDET-VSAM-STATUS.                                             
               10  GACBDET-VSAM-RETURN     PIC XX.                              
               10  GACBDET-VSAM-FUNCTION   PIC XX.                              
               10  GACBDET-VSAM-FEEDBACK   PIC XX.                              
           05  GACBAIX-VSAM-STATUS.                                             
               10  GACBAIX-VSAM-RETURN     PIC XX.                              
               10  GACBAIX-VSAM-FUNCTION   PIC XX.                              
               10  GACBAIX-VSAM-FEEDBACK   PIC XX.                              
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  GACBDET7-PARMS.                                                      
           03  GACBDET7-FUNC         COMP   PIC S9(4)  VALUE +0.                
               88 GACBDET7-FUNC-CLOSE                  VALUE +1.                
               88 GACBDET7-FUNC-READ-GRPACTCERT        VALUE +2, +18.           
               88 GACBDET7-FUNC-READ-GROUP             VALUE +3, +19.           
               88 GACBDET7-FUNC-READ-GRP-ACT           VALUE +4, +20.           
               88 GACBDET7-FUNC-READ-GRP-CERT          VALUE +5, +21.           
               88 GACBDET7-FUNC-READ-TO-END            VALUE +6, +22.           
           03  GACBDET7-SEARCH-KEY.                                             
               05  GACBDET7-GROUP    COMP-3 PIC S9(7).                          
               05  GACBDET7-ACCT            PIC X(3)   VALUE LOW-VALUES.        
               05  GACBDET7-CERT     COMP-3 PIC S9(9).                          
           03  GACBDET7-WORK                PIC X(9)   VALUE LOW-VALUES.        
       01  GACBDET7-PARMS-CHECK REDEFINES GACBDET7-PARMS.                       
           03  CHECK-FUNC                   PIC X(2).                           
           03  CHECK-SEARCH-KEY.                                                
               05  CHECK-GROUP              PIC X(4).                           
               05  FILLER                   PIC X(3).                           
               05  CHECK-CERT               PIC X(5).                           
                                                                                
       01  GACBDET7-RETURN                  PIC X(3)   VALUE SPACES.            
           88  GACBDET7-RET-OK                         VALUE '   '.             
           88  GACBDET7-NOT-FOUND                      VALUE 'R01'.             
           88  GACBDET7-DISK-ERROR                     VALUE 'R03'.             
           88  GACBDET7-DUP-RECORD                     VALUE 'R08'.             
           88  GACBDET7-FUNC-INVALID                   VALUE 'R09'.             
                                                                                
       COPY GDETEXP7.                                                           
                                                                                
                                                                                
       PROCEDURE DIVISION USING GACBDET7-PARMS                                  
                                GACBDET7-RETURN                                 
                                GDETEXP7.                                       
                                                                                
       A100-MAINLINE.                                                           
      *****************************************************************         
      * CONTROLLER                                                    *         
      *****************************************************************         
                                                                                
           INITIALIZE WS-GDET-RECORD.                                           
           PERFORM B100-CHECK-FUNC THRU B100-EXIT.                              
                                                                                
      * SAVE THE CURRENT SEARCH ARGUMENT AND RETURN CODE FOR NEXT FETCH         
           MOVE GACBDET7-SEARCH-KEY TO WS-LAST-SEARCH-KEY.                      
           MOVE GACBDET7-RETURN     TO WS-LAST-GACBDET7-RETURN.                 
           MOVE WS-GDETEXP7         TO GDETEXP7(1:WS-GDET-RECLEN).              
                                                                                
           GOBACK.                                                              
                                                                                
       A100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       B100-CHECK-FUNC.                                                         
           IF GACBDET7-FUNC-READ-GRP-ACT                                        
           OR GACBDET7-FUNC-READ-GRPACTCERT                                     
           OR GACBDET7-FUNC-READ-GRP-CERT                                       
           OR GACBDET7-FUNC-READ-GROUP                                          
           OR GACBDET7-FUNC-READ-TO-END                                         
           THEN                                                                 
              SET GACBDET7-RET-OK TO TRUE                                       
              MOVE 'D' TO DETAIL-IDENT                                          
              IF CHECK-GROUP = LOW-VALUES                                       
                 MOVE ZEROS TO GACBDET7-GROUP                                   
              END-IF                                                            
              IF CHECK-CERT = LOW-VALUES                                        
                 MOVE ZEROS TO GACBDET7-CERT                                    
              END-IF                                                            
              IF GACBDET7-WORK = LOW-VALUES OR SPACES                           
                 MOVE HIGH-VALUES TO GACBDET7-WORK                              
              END-IF                                                            
              PERFORM B200-OPEN-FILE THRU B200-EXIT                             
               IF  ( NOT GACBDET7-FUNC-CLOSE )                                  
               AND ( GACBDET7-RET-OK ) THEN                                     
                   PERFORM C100-PROCESS THRU C100-EXIT                          
               END-IF                                                           
      * IF THE REQUEST IS TO CLOSE THE FILE THEN CLOSE                          
           ELSE IF GACBDET7-FUNC-CLOSE                                          
              SET GACBDET7-RET-OK TO TRUE                                       
              IF GACBDET-IS-OPEN                                                
                  CLOSE GACBDET                                                 
                   IF GACBDET-OK                                                
                     SET GACBDET-IS-CLOSED TO TRUE                              
                   ELSE                                                         
                     SET GACBDET7-DISK-ERROR TO TRUE                            
                  END-IF                                                        
              END-IF                                                            
              IF GACBAIX-IS-OPEN                                                
                  CLOSE GACBAIX                                                 
                   IF GACBAIX-OK                                                
                     SET GACBAIX-IS-CLOSED TO TRUE                              
                   ELSE                                                         
                     SET GACBDET7-DISK-ERROR TO TRUE                            
                  END-IF                                                        
              END-IF                                                            
           ELSE                                                                 
               SET GACBDET7-FUNC-INVALID TO TRUE                                
           END-IF.                                                              
                                                                                
       B100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       B200-OPEN-FILE.                                                          
      *****************************************************************         
      * OPEN THE FILE IF ITS CLOSED                                   *         
      *****************************************************************         
              IF GACBDET-IS-CLOSED                                              
                  OPEN INPUT GACBDET                                            
                  IF GACBDET-OK                                                 
                     SET GACBDET-IS-OPEN TO TRUE                                
                  ELSE                                                          
                     SET GACBDET7-DISK-ERROR TO TRUE                            
                  END-IF                                                        
              END-IF                                                            
              IF GACBDET7-FUNC-READ-GRP-CERT AND GACBAIX-IS-CLOSED              
                  OPEN INPUT GACBAIX                                            
                  IF GACBAIX-OK                                                 
                     SET GACBAIX-IS-OPEN TO TRUE                                
                  ELSE                                                          
                     SET GACBDET7-DISK-ERROR TO TRUE                            
                  END-IF                                                        
              END-IF.                                                           
                                                                                
       B200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       C100-PROCESS.                                                            
      *****************************************************************         
      * THIS PARAGRAPH EVALUATES THE REQUEST AND RESPONDS             *         
      * TO READ DIRECT/KEYED REQUESTS                                 *         
      *****************************************************************         
      *                                                                         
      *SKIP SEQUENTIAL READ.                                                    
      *****************************************************************         
      * IF THE CURRENT VS. PREVIOUS FUNC CODE & SEARCH KEY ARE SAME   *         
      * AND LAST READ IS A SUCCESS THEN PERFORM SKIP SEQUENTIAL READ  *         
      * (EXCEPT FOR READ-DIRECT AND READ-TO-EOF)                      *         
      *****************************************************************         
           IF  NOT GACBDET7-FUNC-READ-GRPACTCERT        AND                     
               GACBDET7-FUNC  = WS-LAST-FUNC            AND                     
               GACBDET7-SEARCH-KEY = WS-LAST-SEARCH-KEY AND                     
               WS-LAST-GACBDET7-RETURN = SPACES                                 
                                                                                
            IF NOT GACBDET7-FUNC-READ-GRP-CERT                                  
                READ GACBDET NEXT                                               
                                                                                
                EVALUATE TRUE                                                   
                                                                                
                WHEN GACBDET7-FUNC-READ-GROUP                                   
                                                                                
                   IF NOT GACBDET-OK                                            
                   OR DETAIL-GROUP NOT = GACBDET7-GROUP                         
                      SET GACBDET7-NOT-FOUND TO TRUE                            
                   ELSE                                                         
                      PERFORM I100-RETURN-DATA THRU I100-EXIT                   
                   END-IF                                                       
                WHEN GACBDET7-FUNC-READ-GRP-ACT                                 
                                                                                
                   IF NOT GACBDET-OK                                            
                   OR ( DETAIL-GROUP   NOT = GACBDET7-GROUP OR                  
                        DETAIL-ACCOUNT NOT = GACBDET7-ACCT    )                 
                      SET GACBDET7-NOT-FOUND TO TRUE                            
                   ELSE                                                         
                      PERFORM I100-RETURN-DATA THRU I100-EXIT                   
                   END-IF                                                       
                WHEN GACBDET7-FUNC-READ-TO-END                                  
                                                                                
                   IF NOT GACBDET-OK                                            
                      SET GACBDET7-NOT-FOUND TO TRUE                            
                   ELSE                                                         
                      PERFORM I100-RETURN-DATA THRU I100-EXIT                   
                   END-IF                                                       
                WHEN OTHER                                                      
                                                                                
                   SET GACBDET7-FUNC-INVALID TO TRUE                            
                                                                                
                END-EVALUATE                                                    
            ELSE                                                                
               READ GACBAIX NEXT                                                
                                                                                
                IF NOT GACBAIX-OK                                               
                OR ( ALTKEY-GROUP   NOT = GACBDET7-GROUP OR                     
                     ALTKEY-CERT    NOT = GACBDET7-CERT    )                    
                   SET GACBDET7-DUP-RECORD TO TRUE                              
                   MOVE WS-DETAIX-RECLEN-NEXT TO WS-GDET-RECLEN                 
                   MOVE WS-DETAIL-ALTKEY-REC-NEXT TO                            
                                   WS-GDET-RECORD                               
                ELSE                                                            
                   MOVE WS-DETAIX-RECLEN-NEXT TO WS-GDET-RECLEN                 
                   MOVE WS-DETAIL-ALTKEY-REC-NEXT TO                            
                                   WS-GDET-RECORD                               
                   MOVE WS-DETAIX-RECLEN TO WS-DETAIX-RECLEN-NEXT               
                   MOVE DETAIL-ALTKEY-REC TO WS-DETAIL-ALTKEY-REC-NEXT          
                END-IF                                                          
            END-IF                                                              
                                                                                
           ELSE                                                                 
      *****************************************************************         
      * DON'T START OVER IF READ PAST END OF FILE FOR FUNC = +6 OR +22*         
      *****************************************************************         
             IF GACBDET7-FUNC-READ-TO-END    AND                                
                GACBDET7-FUNC = WS-LAST-FUNC AND                                
                GACBDET7-SEARCH-KEY = WS-LAST-SEARCH-KEY                        
               THEN                                                             
                  READ GACBDET NEXT                                             
                  IF NOT GACBDET-OK                                             
                     SET GACBDET7-NOT-FOUND TO TRUE                             
                  ELSE                                                          
                     PERFORM I100-RETURN-DATA THRU I100-EXIT                    
                  END-IF                                                        
           ELSE                                                                 
      *****************************************************************         
      * EVALUATE THE FUNCTION CODE AND START OVER THE READ OPERATION  *         
      * FOR THE SEARCH KEY PROVIDED                                   *         
      *****************************************************************         
              EVALUATE TRUE                                                     
                                                                                
              WHEN GACBDET7-FUNC-READ-GRP-ACT                                   
                                                                                
                PERFORM D100-READ-GROUP-ACCOUNT THRU D100-EXIT                  
                                                                                
              WHEN GACBDET7-FUNC-READ-GRPACTCERT                                
                                                                                
                PERFORM E100-READ-FOR-GROUP-ACT-CERT THRU E100-EXIT             
                                                                                
              WHEN GACBDET7-FUNC-READ-GRP-CERT                                  
                                                                                
                PERFORM F100-READ-CERT-FOR-GROUP THRU F100-EXIT                 
                                                                                
              WHEN GACBDET7-FUNC-READ-GROUP                                     
                                                                                
                PERFORM G100-READ-ACCOUNTS-FOR-GROUP THRU G100-EXIT             
                                                                                
              WHEN GACBDET7-FUNC-READ-TO-END                                    
                                                                                
                PERFORM H100-READ-GRPACTCERT-TO-END THRU H100-EXIT              
                                                                                
              WHEN OTHER                                                        
                                                                                
                SET GACBDET7-FUNC-INVALID TO TRUE                               
                                                                                
              END-EVALUATE                                                      
                                                                                
           END-IF.                                                              
                                                                                
       C100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       D100-READ-GROUP-ACCOUNT.                                                 
      *****************************************************************         
      * THIS PARAGRAPH RESPONDS TO THE REQUEST TO RETURN EACH CERT    *         
      * IN TURN FOR A SPECIFIC GROUP/ACCOUNT                          *         
      *****************************************************************         
              MOVE GACBDET7-GROUP TO DETAIL-GROUP                               
              MOVE GACBDET7-ACCT  TO DETAIL-ACCOUNT                             
              MOVE ZEROS          TO DETAIL-CERT                                
              MOVE GACBDET7-FUNC  TO WS-LAST-FUNC                               
              START GACBDET KEY IS >= DETAIL-KEY                                
              IF NOT GACBDET-OK                                                 
                 SET GACBDET7-NOT-FOUND TO TRUE                                 
              ELSE                                                              
                 READ GACBDET NEXT                                              
                 IF NOT GACBDET-OK                                              
                 OR ( DETAIL-GROUP   NOT = GACBDET7-GROUP OR                    
                      DETAIL-ACCOUNT NOT = GACBDET7-ACCT    )                   
                   SET GACBDET7-NOT-FOUND TO TRUE                               
                 ELSE                                                           
                   PERFORM I100-RETURN-DATA THRU I100-EXIT                      
                 END-IF                                                         
              END-IF.                                                           
                                                                                
       D100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       E100-READ-FOR-GROUP-ACT-CERT.                                            
      *****************************************************************         
      * READ DIRECT                                                   *         
      *****************************************************************         
                                                                                
              MOVE GACBDET7-GROUP   TO DETAIL-GROUP                             
              MOVE GACBDET7-ACCT    TO DETAIL-ACCOUNT                           
              MOVE GACBDET7-CERT    TO DETAIL-CERT                              
              MOVE GACBDET7-FUNC    TO WS-LAST-FUNC                             
              READ GACBDET KEY IS DETAIL-KEY OF DETAIL-REC                      
                                                                                
              IF GACBDET-OK                                                     
              THEN   PERFORM I100-RETURN-DATA THRU I100-EXIT                    
              ELSE   IF GACBDET-REC-NOT-FND                                     
                     THEN   SET GACBDET7-NOT-FOUND TO TRUE                      
                     ELSE   SET GACBDET7-DISK-ERROR TO TRUE                     
                     END-IF                                                     
              END-IF.                                                           
                                                                                
       E100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       F100-READ-CERT-FOR-GROUP.                                                
      *****************************************************************         
      * THIS PARAGRAPH RESPONDS TO THE REQUEST TO RETURN EACH CERT    *         
      * IN TURN FOR A SPECIFIC GROUP                                  *         
      *****************************************************************         
              MOVE GACBDET7-GROUP TO ALTKEY-GROUP                               
              MOVE GACBDET7-CERT  TO ALTKEY-CERT                                
              MOVE GACBDET7-FUNC  TO WS-LAST-FUNC                               
              START GACBAIX KEY EQUAL TO DETAIL-ALTKEY                          
              IF NOT GACBAIX-OK                                                 
                 SET GACBDET7-NOT-FOUND TO TRUE                                 
              ELSE                                                              
                 READ GACBAIX                                                   
                 IF GACBAIX-OK                                                  
                   MOVE WS-DETAIX-RECLEN TO WS-GDET-RECLEN                      
                   MOVE DETAIL-ALTKEY-REC                                       
                                        TO WS-GDET-RECORD                       
                 END-IF                                                         
      * PREFETCH THE NEXT RECORD TO CHECK THE CURRENT RECORD IS THE   *         
      *  LAST RECORD FOR THE GIVEN KEY                                *         
                 READ GACBAIX NEXT                                              
                 IF NOT GACBAIX-OK                                              
                 OR ( ALTKEY-GROUP   NOT = GACBDET7-GROUP OR                    
                      ALTKEY-CERT    NOT = GACBDET7-CERT    )                   
                    SET GACBDET7-DUP-RECORD TO TRUE                             
                 ELSE                                                           
                    INITIALIZE  WS-DETAIL-ALTKEY-REC-NEXT                       
                    MOVE WS-DETAIX-RECLEN TO WS-DETAIX-RECLEN-NEXT              
                    MOVE DETAIL-ALTKEY-REC TO WS-DETAIL-ALTKEY-REC-NEXT         
                 END-IF                                                         
              END-IF.                                                           
                                                                                
       F100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       G100-READ-ACCOUNTS-FOR-GROUP.                                            
      *****************************************************************         
      * THIS PARAGRAPH RESPONDS TO THE REQUEST TO RETURN ACCT/CERTS   *         
      * IN TURN FOR A SPECIFIC GROUP                                  *         
      *****************************************************************         
              MOVE GACBDET7-GROUP TO DETAIL-GROUP                               
              MOVE LOW-VALUES     TO DETAIL-ACCOUNT                             
              MOVE ZEROS          TO DETAIL-CERT                                
              MOVE GACBDET7-FUNC  TO WS-LAST-FUNC                               
              START GACBDET KEY IS >= DETAIL-KEY                                
              IF NOT GACBDET-OK                                                 
                 SET GACBDET7-NOT-FOUND TO TRUE                                 
              ELSE                                                              
                 READ GACBDET NEXT                                              
                 IF NOT GACBDET-OK                                              
                 OR DETAIL-GROUP NOT = GACBDET7-GROUP                           
                   SET GACBDET7-NOT-FOUND TO TRUE                               
                 ELSE                                                           
                   PERFORM I100-RETURN-DATA THRU I100-EXIT                      
                 END-IF                                                         
              END-IF.                                                           
                                                                                
       G100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       H100-READ-GRPACTCERT-TO-END.                                             
      *****************************************************************         
      * THIS PARAGRAPH RESPONDS TO THE REQUEST TO RETURN EACH         *         
      * GROUP/ACCOUNT/CERT IN TURN STARTING AT A SPECIFIC GROUP       *         
      * AND THROUGH TO THE END OF FILE                                *         
      *****************************************************************         
              MOVE GACBDET7-GROUP TO DETAIL-GROUP                               
              MOVE GACBDET7-ACCT  TO DETAIL-ACCOUNT                             
              MOVE GACBDET7-CERT  TO DETAIL-CERT                                
              MOVE GACBDET7-FUNC  TO WS-LAST-FUNC                               
              READ GACBDET KEY IS DETAIL-KEY OF DETAIL-REC                      
              IF GACBDET-OK                                                     
                 PERFORM I100-RETURN-DATA THRU I100-EXIT                        
              ELSE                                                              
                 START GACBDET KEY IS > DETAIL-KEY                              
                 READ GACBDET NEXT                                              
                   IF GACBDET-OK                                                
                      PERFORM I100-RETURN-DATA THRU I100-EXIT                   
                   ELSE                                                         
                      SET GACBDET7-NOT-FOUND TO TRUE                            
                   END-IF                                                       
              END-IF.                                                           
                                                                                
       H100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       I100-RETURN-DATA.                                                        
      *****************************************************************         
      * THIS PARAGRAPH RETURNS TO RETRIEVED DETAIL RECORD AND ITS     *         
      * LENGTH TO THE CALLING PROGRAM                                 *         
      *****************************************************************         
           MOVE WS-DETAIL-RECLEN TO WS-GDET-RECLEN.                             
           MOVE DETAIL-REC       TO WS-GDET-RECORD.                             
                                                                                
       I100-EXIT.                                                               
           EXIT.                                                                
