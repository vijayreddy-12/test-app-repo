CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLCTGCIO.                                                 
      *AUTHOR.        F. MUELLER.                                               
      *DATE-WRITTEN.  MAY 2002.                                                 
      *DATE-COMPILED.                                                           
      *============================================================*            
      *    DATA SERVICE ROUTINE FOR THE GENERAL CONTENT SERVICE.                
      *------------------------------------------------------------*            
      *    MAY/02 F MUELLER   - CREATED                                         
      *    OCT/02 F MUELLER   - CHANGES TO MINIMIZE BUFFER SPACE IN             
      *                         THE CALLING PROGRAM.                            
      *    JAN/03 N JEFFREY   - CHANGE CICS LOGIC FOR READ GTEQ TO              
      *                         REDUCE I/O.                                     
      *    JAN/03 F MUELLER   - DON'T SMEAR THE DATA RECORD ON A                
      *                         FINISH REQUEST.                                 
      *                                                                         
      *    AUG/08 IBM         - ENTERPRISE COMPILER UPGRADE(ECU)                
      *------------------------------------------------------------*            
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
           COPY MLCTSLCT.                                                       
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
           COPY MLCTFCNT.                                                       
      /                                                                         
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-START                 PIC X(16) VALUE 'MLCTGCIO START=>'.         
                                                                                
       01  WS-FIELDS.                                                           
           05  WS-SUB                   PIC S9(4)    VALUE +0 COMP.             
           05  WS-SUB1                  PIC S9(4)    VALUE +0 COMP.             
           05  WS-CICS-RESP             PIC S9(8)    VALUE +0 COMP.             
           05  WS-CICS-RESP2            PIC S9(8)    VALUE +0 COMP.             
           05  WS-READ-KEY              PIC X(64).                              
           05  WS-PART-KEY              REDEFINES WS-READ-KEY                   
                                        PIC X(48).                              
           05  WS-READ-KEY-LENGTH       PIC S9(4)    VALUE +0 COMP.             
           05  WS-ACCESS-DDNAME         PIC X(08)    VALUE SPACES.              
           05  WS-FILES-OPEN-SW         PIC X(01)    VALUE 'N'.                 
               88  FILES-OPEN             VALUE 'Y'.                            
               88  FILES-NOT-OPEN         VALUE 'N'.                            
           05  WS-FILE-STATUS           PIC X(02).                              
               88  FSTAT-OK             VALUES '00'.                            
               88  FSTAT-DUPKEY         VALUE  '02'.                            
               88  FSTAT-SUCCESSFUL     VALUES '00' '02' '04' '97'.             
               88  FSTAT-NOT-FOUND      VALUE '23'.                             
               88  FSTAT-INVALID-KEY    VALUE '21'  THRU '24'.                  
               88  FSTAT-END-OF-FILE    VALUE '10' '46'.                        
               88  FSTAT-CICS-ERROR     VALUE 'XX'.                             
           05  WS-CONTENT-FILE-STAT     REDEFINES  WS-FILE-STATUS               
                                        PIC X(02).                              
           05  WS-VSAM-STATUS.                                                  
               10  WS-VSAM-RETCODE      PIC 9(02)  COMP.                        
               10  WS-VSAM-FUNCTION     PIC 9(01)  COMP.                        
               10  WS-VSAM-FEEDBACK     PIC 9(03)  COMP.                        
           05  WS-CONTENT-VSAM-STAT     REDEFINES  WS-VSAM-STATUS.              
               10  FILLER               PIC X(06).                              
                                                                                
       01  WS-CONTENT-FILE-DETAILS.                                             
           05  WS-CONTENT-FILE-DDNAME   PIC X(08) VALUE 'MLCTDB01'.             
           05  WS-CONTENT-FILE-SW       PIC X(01).                              
               88  CONTENT-FILE-OPEN      VALUE 'Y'.                            
               88  CONTENT-FILE-NOT-OPEN  VALUE 'N'.                            
                                                                                
       01  WS-RECORD.                                                           
           05  WS-RECORD-RDW.                                                   
               10 WS-RECORD-LENGTH      PIC 9(04)  COMP.                        
               10 FILLER                PIC X(02)    VALUE LOW-VALUES.          
           05  WS-RECORD-BUFFER         PIC X(32760) VALUE SPACES.              
                                                                                
       01  WS-END                   PIC X(16) VALUE '<===END MLCTGCIO'.         
      *---------------------------------------------------------------          
       LINKAGE SECTION.                                                         
       01  MLCTGCIO-PROTOCOL.           COPY MLCTRCIO.                          
       01  MLCTGCIO-DATA-RECORD         PIC X(01).                              
                                                                                
      *---------------------------------------------------------------*         
      *  RECORD DEFINITIONS FOR EACH LR REQUIRED SHOULD BE DECLARED IN*         
      *  LINKAGE. THE ADDRESS FOR EACH RECORD SHOULD BE ASSIGNED THE  *         
      *  SAME ADDRESS AS THE INCOMING DATA RECORD AT INITIALIZATION.  *         
      *---------------------------------------------------------------*         
       01  FCNT-CONTENT-RECORD.         COPY XC4CFCNT.                          
                                                                                
       PROCEDURE DIVISION USING MLCTGCIO-PROTOCOL                               
                                MLCTGCIO-DATA-RECORD.                           
                                                                                
       1000-MLCTGCIO-START.                                                     
      *============================================================*            
      *  MAINLINE PROCESSING - PERFORM ROUTINE BASED ON IO VERB                 
      *------------------------------------------------------------*            
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
           PERFORM 2000-PROCESS-REQUEST THRU 2000-EXIT.                         
                                                                                
           IF GCIO-OK  AND NOT GCIO-OBTAIN-FINISH                               
              EVALUATE TRUE                                                     
                  WHEN GCIO-CONTENT-LR                                          
                      MOVE WS-RECORD-BUFFER TO MLCTGCIO-KEY-OUT                 
                                               FCNT-CONTENT-RECORD              
                      MOVE WS-RECORD-LENGTH TO MLCTGCIO-DATA-LENGTH             
                  WHEN OTHER                                                    
                      MOVE WS-RECORD-BUFFER TO MLCTGCIO-KEY-OUT                 
                      MOVE WS-RECORD-BUFFER TO                                  
                           MLCTGCIO-DATA-RECORD (1:WS-RECORD-LENGTH)            
                      MOVE WS-RECORD-LENGTH TO MLCTGCIO-DATA-LENGTH             
              END-EVALUATE                                                      
           END-IF.                                                              
                                                                                
           GOBACK.                                                              
                                                                                
       1000-INITIALIZATION.                                                     
      *============================================================*            
      *  INITIALIZE AS NECESSARY AND LOCATE LOGICAL RECORD                      
      *------------------------------------------------------------*            
                                                                                
           SET  GCIO-OK              TO  TRUE.                                  
           INITIALIZE                WS-RECORD-BUFFER.                          
                                                                                
           IF GCIO-CONTENT-LR                                                   
              SET ADDRESS OF FCNT-CONTENT-RECORD  TO                            
                  ADDRESS OF MLCTGCIO-DATA-RECORD                               
           END-IF.                                                              
                                                                                
           IF NOT FILES-OPEN                                                    
              PERFORM 1100-OPEN-FILES THRU 1100-EXIT                            
                                                                                
              IF NOT FSTAT-SUCCESSFUL                                           
                 SET GCIO-ERROR TO TRUE                                         
                 MOVE  WS-FILE-STATUS TO GCIO-FILE-STATUS                       
                 MOVE  WS-VSAM-STATUS TO GCIO-VSAM-STATUS                       
                 GO TO 1000-EXIT                                                
              END-IF                                                            
           END-IF.                                                              
                                                                                
      * COPY THE INPUT KEY AND INITIALIZE THE RETURN KEY                        
                                                                                
           INITIALIZE WS-READ-KEY.                                              
                                                                                
           MOVE MLCTGCIO-KEY-IN (1 : LENGTH OF WS-READ-KEY)                     
                      TO WS-READ-KEY.                                           
                                                                                
           INITIALIZE MLCTGCIO-KEY-OUT.                                         
                                                                                
           MOVE WS-CONTENT-FILE-DDNAME TO  WS-ACCESS-DDNAME.                    
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *  OPEN FILE (BATCH ONLY), IF IT IS NOT ALREADY OPEN.                     
      *                                                                         
       1100-OPEN-FILES.                                                         
                                                                                
           COPY MLCTOPEN.                                                       
                                                                                
       1100-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2000-PROCESS-REQUEST.                                                    
                                                                                
           IF  NOT GCIO-ERROR                                                   
               EVALUATE TRUE                                                    
                                                                                
                   WHEN GCIO-OBTAIN-GTEQ                                        
                       PERFORM 2100-READ-GTEQ   THRU 2100-EXIT                  
                                                                                
                   WHEN GCIO-OBTAIN-KEYED                                       
                       PERFORM 2110-READ-KEYED  THRU 2110-EXIT                  
                                                                                
                   WHEN GCIO-OBTAIN-FIRST                                       
                       PERFORM 2120-READ-FIRST  THRU 2120-EXIT                  
                                                                                
                   WHEN GCIO-OBTAIN-NEXT                                        
                       PERFORM 2130-READ-NEXT   THRU 2130-EXIT                  
                                                                                
                   WHEN GCIO-STORE                                              
                       PERFORM 2140-INSERT      THRU 2140-EXIT                  
                                                                                
                   WHEN GCIO-MODIFY                                             
                       PERFORM 2150-UPDATE      THRU 2150-EXIT                  
                                                                                
                   WHEN GCIO-DELETE                                             
                       PERFORM 2160-DELETE      THRU 2160-EXIT                  
                                                                                
                   WHEN GCIO-OBTAIN-FINISH                                      
                       PERFORM 2170-FINISH      THRU 2170-EXIT                  
                                                                                
                   WHEN OTHER                                                   
                       SET GCIO-INVALID-FUNCTION TO TRUE                        
                                                                                
               END-EVALUATE                                                     
           END-IF.                                                              
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2100-READ-GTEQ.                                                          
      *============================================================*            
      * BASED ON LOGICAL RECORD, PERFORM THE IO                                 
      *------------------------------------------------------------*            
           EVALUATE TRUE                                                        
                                                                                
               WHEN GCIO-CONTENT-LR                                             
                   PERFORM 2240-CONTENT-READ-GTEQ   THRU 2240-EXIT              
                                                                                
               WHEN OTHER                                                       
                   SET GCIO-INVALID-FUNC-LR TO TRUE                             
                                                                                
           END-EVALUATE.                                                        
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2110-READ-KEYED.                                                         
      *============================================================*            
      * BASED ON LOGICAL RECORD, PERFORM THE IO                                 
      *------------------------------------------------------------*            
           EVALUATE TRUE                                                        
                                                                                
               WHEN GCIO-CONTENT-LR                                             
                   PERFORM 2210-CONTENT-READ-KEYED THRU 2210-EXIT               
                                                                                
               WHEN OTHER                                                       
                   SET GCIO-INVALID-FUNC-LR TO TRUE                             
                                                                                
           END-EVALUATE.                                                        
                                                                                
       2110-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2120-READ-FIRST.                                                         
      *============================================================*            
      * BASED ON LOGICAL RECORD, PERFORM THE IO                                 
      *------------------------------------------------------------*            
           EVALUATE TRUE                                                        
                                                                                
               WHEN GCIO-CONTENT-LR                                             
                   PERFORM 2220-CONTENT-READ-FIRST  THRU 2220-EXIT              
                                                                                
               WHEN OTHER                                                       
                   SET GCIO-INVALID-FUNC-LR TO TRUE                             
                                                                                
           END-EVALUATE.                                                        
                                                                                
       2120-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2130-READ-NEXT.                                                          
      *============================================================*            
      * BASED ON LOGICAL RECORD, PERFORM THE IO                                 
      *------------------------------------------------------------*            
           EVALUATE TRUE                                                        
                                                                                
               WHEN GCIO-CONTENT-LR                                             
                   PERFORM 2230-CONTENT-READ-NEXT  THRU 2230-EXIT               
                                                                                
               WHEN OTHER                                                       
                   SET GCIO-INVALID-FUNC-LR TO TRUE                             
                                                                                
           END-EVALUATE.                                                        
                                                                                
       2130-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2140-INSERT.                                                             
      *============================================================*            
      * BASED ON LOGICAL RECORD, PERFORM THE IO                                 
      *------------------------------------------------------------*            
           SET GCIO-INVALID-FUNC-LR TO TRUE.                                    
                                                                                
       2140-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2150-UPDATE.                                                             
      *============================================================*            
      * BASED ON LOGICAL RECORD, PERFORM THE IO                                 
      *------------------------------------------------------------*            
           SET GCIO-INVALID-FUNC-LR TO TRUE.                                    
                                                                                
       2150-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2160-DELETE.                                                             
      *============================================================*            
      * BASED ON LOGICAL RECORD, PERFORM THE IO                                 
      *------------------------------------------------------------*            
           SET GCIO-INVALID-FUNC-LR TO TRUE.                                    
                                                                                
       2160-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2170-FINISH.                                                             
      *============================================================*            
      * BASED ON LOGICAL RECORD, PERFORM THE IO                                 
      *------------------------------------------------------------*            
           EVALUATE TRUE                                                        
                                                                                
               WHEN GCIO-CONTENT-LR                                             
                   PERFORM 2900-BROWSE-FINISH  THRU 2900-EXIT                   
                                                                                
               WHEN OTHER                                                       
                   SET GCIO-INVALID-FUNC-LR TO TRUE                             
                                                                                
           END-EVALUATE.                                                        
                                                                                
                                                                                
       2170-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2210-CONTENT-READ-KEYED.                                                 
      *============================================================*            
      *  READ A SPECIFIC RECORD FROM THE CONTENT FILE.                          
      *------------------------------------------------------------*            
                                                                                
           MOVE LENGTH OF WS-READ-KEY      TO  WS-READ-KEY-LENGTH.              
           MOVE LENGTH OF WS-RECORD-BUFFER TO  WS-RECORD-LENGTH.                
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *  KEYED READ OF CONTENT VSAM FILE                                        
      *                                                                         
           COPY MLCTCRDK.                                                       
                                                                                
       2210-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2220-CONTENT-READ-FIRST.                                                 
      *============================================================*            
      *  BROWSE CONTENT RECORDS FOR A SPECIFIC NAME.                            
      *------------------------------------------------------------*            
           MOVE LENGTH OF WS-READ-KEY      TO  WS-READ-KEY-LENGTH.              
           MOVE LENGTH OF WS-RECORD-BUFFER TO  WS-RECORD-LENGTH.                
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *  START/READ OF CONTENT VSAM FILE                                        
      *                                                                         
           COPY MLCTCRDF.                                                       
                                                                                
       2220-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2230-CONTENT-READ-NEXT.                                                  
      *============================================================*            
      *  READ THE NEXT CONTENT RECORD FOR A SPECIFIED BROWSE                    
      *------------------------------------------------------------*            
                                                                                
           MOVE LENGTH OF WS-RECORD-BUFFER TO  WS-RECORD-LENGTH.                
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *  READ NEXT CONTENT VSAM RECORD                                          
      *                                                                         
           COPY MLCTCRDN.                                                       
                                                                                
       2230-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2240-CONTENT-READ-GTEQ.                                                  
      *============================================================*            
      *  READ A SINGLE RECORD WITH KEY GREATER OR EQUAL                         
      *------------------------------------------------------------*            
           MOVE LENGTH OF WS-READ-KEY      TO  WS-READ-KEY-LENGTH.              
           MOVE LENGTH OF WS-RECORD-BUFFER TO  WS-RECORD-LENGTH.                
                                                                                
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *  READ GTEQ  OF CONTENT VSAM FILE                                        
      *                                                                         
           COPY MLCTCRDG.                                                       
                                                                                
       2240-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2900-BROWSE-FINISH.                                                      
      *============================================================*            
      *  TERMINATE A BROWSE OPERATION                                           
      *------------------------------------------------------------*            
      *                                                                         
      *  BATCH OR CICS CALL                                                     
      *                                                                         
      *                                                                         
                                                                                
           COPY MLCTCFIN.                                                       
                                                                                
       2900-EXIT.                                                               
           EXIT.                                                                
                                                                                
