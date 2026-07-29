1*******************************************************************************
 *******************************************************************************
 **                                                                           **
 ** ELEMENT BROWSE                                            14NOV25  10:57  **
 **                                                                           **
 **    ENVIRONMENT:   ITSPROD    SYSTEM:    ITS       SUBSYSTEM:  STANDARD    **
 **    TYPE:          CIIB       STAGE ID:  P                                 **
 **    ELEMENT:       CXMQTRGY                                                **
 **                                                                           **
 **                                                   DELTA TYPE: REVERSE     **
 **                                                                           **
1*******************************************************************************
 *******************************************************************************
                                                                                
 -------------------------- SOURCE LEVEL INFORMATION ---------------------------
                                                                                
1  VVLL SYNC USER     DATE    TIME     STMTS CCID         COMMENT               
   ---- ---- -------- ------- ----- -------- ------------ ----------------------
   0100      HUTCH    05NOV97 16:17      344 HUTCH1105971 SET TRIGGER TO YES ON 
 GENERATED   HUTCH    05NOV97 16:17          HUTCH1105971 SET TRIGGER TO YES ON 
                                                                                
  +0100  CBL NODYNAM,LIB,OBJECT,RENT,RES,APOST                                  
  +0100        *                                                               *
  +0100        * ------------------------------------------------------------- *
  +0100         IDENTIFICATION DIVISION.                                        
  +0100        * ------------------------------------------------------------- *
  +0100         PROGRAM-ID. CXMQTRGY.                                           
  +0100        *REMARKS                                                         
  +0100        *                                                                
  +0100        * AUTHOR: GRAHAM HUTCHISON                                       
  +0100        * RELEASE: 1.0 - NOV. 5, 1997                                    
  +0100        *                                                                
  +0100        *                                                                
  +0100        * THIS PROGRAM WILL TURN TRIGGERING ON FOR A QUEUE.  IT TAKES    
  +0100        * TWO PARMS, THE QUEUE MANAGER AND THE QUEUE.                    
  +0100        *                                                                
  +0100        * ------------------------------------------------------------- *
  +0100         ENVIRONMENT DIVISION.                                           
  +0100        * ------------------------------------------------------------- *
  +0100         INPUT-OUTPUT SECTION.                                           
  +0100         FILE-CONTROL.                                                   
  +0100             SELECT SYSPRINT ASSIGN TO UT-S-SYSPRINT.                    
  +0100        * ------------------------------------------------------------- *
  +0100         DATA DIVISION.                                                  
  +0100        * ------------------------------------------------------------- *
  +0100         FILE SECTION.                                                   
  +0100         FD  SYSPRINT                                                    
  +0100             BLOCK CONTAINS 0 RECORDS                                    
  +0100             RECORDING MODE IS F.                                        
  +0100         01  PRINT-REC.                                                  
  +0100             05  CARRIAGE-CONTROL        PIC X.                          
  +0100             05  PRINT-DATA              PIC X(132).                     
  +0100        * ------------------------------------------------------------- *
  +0100         WORKING-STORAGE SECTION.                                        
  +0100        * ------------------------------------------------------------- *
  +0100        *                                                                
  +0100        *    W00 - GENERAL WORK FIELDS                                   
  +0100        *                                                                
  +0100         01  W00-RETURN-CODE             PIC S9(04) BINARY  VALUE ZERO.  
  +0100        *                                                                
  +0100        *    W01 - LINES OF THE PRINT REPORT                             
  +0100        *                                                                
  +0100         01  W01-HEADER-1.                                               
  +0100             05  FILLER                  PIC X(10) VALUE SPACES.         
  +0100             05  W01-MM                  PIC X(100).                     
  +0100             05  W01-DD                  PIC X(100).                     
  +0100             05  W01-YY                  PIC 99.                         
  +0100             05  FILLER                  PIC X(8)  VALUE SPACES.         
  +0100             05  FILLER                  PIC X(21) VALUE                 
  +0100                                                  'TRIGGER TURNED ON'.   
  +0100             05  FILLER                  PIC X(85) VALUE SPACES.         
  +0100         01  W01-HEADER-2.                                               
  +0100             05  FILLER                  PIC X(5)  VALUE SPACES.         
  +0100             05  FILLER                  PIC X(29) VALUE                 
  +0100                                         '        QUEUE MANAGER NAME : '.
  +0100             05  W01-MQM-NAME            PIC X(48) VALUE SPACES.         
  +0100             05  FILLER                  PIC X(50) VALUE SPACES.         
  +0100         01  W01-HEADER-3.                                               
  +0100             05  FILLER                  PIC X(17) VALUE SPACES.         
  +0100             05  FILLER                  PIC X(17) VALUE                 
  +0100                                                      '  QUEUE : '.      
  +0100             05  W01-QUEUE-NAME          PIC X(48) VALUE SPACES.         
  +0100             05  FILLER                  PIC X(50) VALUE SPACES.         
  +0100        *                                                                
  +0100        *    W02 - DATA FIELDS DERIVED FROM THE PARM FIELD               
  +0100        *                                                                
  +0100         01  W02-MQM                     PIC X(48) VALUE SPACES.         
  +0100         01  W02-OBJECT                  PIC X(48) VALUE SPACES.         
  +0100        *                                                                
  +0100        *    W03 - MQM API FIELDS                                        
  +0100        *                                                                
  +0100         01  W03-SELECTOR-COUNT          PIC S9(9) BINARY  VALUE 1.      
  +0100         01  W03-SELECTOR-ARRAY.                                         
  +0100           02 W03-SELECTORS              PIC S9(9) BINARY OCCURS 1 TIMES.
  +0100         01  W03-INT-ATTR-COUNT          PIC S9(9) BINARY VALUE 1.       
  +0100         01  W03-INT-ATTR-ARRAY.                                         
  +0100           02 W03-INT-ATTRS              PIC S9(9) BINARY OCCURS 1 TIMES.
  +0100         01  W03-CHAR-ATTR-LENGTH        PIC S9(9) BINARY VALUE 0.       
  +0100         01  W03-CHAR-ATTRS              PIC X(1).                       
  +0100         01  W03-HCONN                   PIC S9(9) BINARY.               
  +0100         01  W03-OPTIONS                 PIC S9(9) BINARY.               
  +0100         01  W03-HOBJ                    PIC S9(9) BINARY.               
  +0100         01  W03-COMPCODE                PIC S9(9) BINARY.               
  +0100         01  W03-REASON                  PIC S9(9) BINARY.               
  +0100        *                                                                
  +0100        *    W04 - ERROR AND INFORMATION MESSAGES                        
  +0100        *                                                                
  +0100         01  W04-MESSAGE-1.                                              
  +0100             05  FILLER              PIC X(10)  VALUE SPACES.            
  +0100             05  FILLER              PIC X(122) VALUE                    
  +0100                '********** NO DATA PASSED TO PROGRAM. PROGRAM REQUIRES A
  +0100        -       'QUEUE MANAGER NAME AND A QUEUE NAME. **********'.       
  +0100         01  W04-MESSAGE-2.                                              
  +0100             05  FILLER              PIC X(25)  VALUE SPACES.            
  +0100             05  FILLER              PIC X(107) VALUE                    
  +0100                '********** NO QUEUE MANAGER NAME PASSED TO PROGRAM - DEF
  +0100        -       'ULT USED *****'.                                        
  +0100         01  W04-MESSAGE-3.                                              
  +0100             05  FILLER              PIC X(38) VALUE SPACES.             
  +0100             05  FILLER              PIC X(94) VALUE                     
  +0100                '********** NO QUEUE NAME PASSED TO PROGRAM. **********'.
  +0100         01  W04-MESSAGE-4.                                              
  +0100             05  FILLER              PIC X(13) VALUE SPACES.             
  +0100             05  FILLER              PIC X(32) VALUE                     
  +0100                     '********** AN ERROR OCCURRED IN '.                 
  +0100             05  W04-MSG4-TYPE       PIC X(10).                          
  +0100             05  FILLER              PIC X(20) VALUE                     
  +0100                     '. COMPLETION CODE = '.                             
  +0100             05  W04-MSG4-COMPCODE   PIC Z(8)9.                          
  +0100             05  FILLER              PIC X(15) VALUE ' REASON CODE ='.   
  +0100             05  W04-MSG4-REASON     PIC Z(8)9.                          
  +0100             05  FILLER              PIC X(24) VALUE ' **********'.      
  +0100        *                                                                
  +0100        *    THE FOLLOWING COPY FILES DEFINE API CONTROL BLOCKS.         
  +0100        *                                                                
  +0100         01  W05-MQM-OBJECT-DESCRIPTOR.                                  
  +0100             COPY CMQODV.                                                
  +0100        *                                                                
  +0100        *    COPY FILE OF CONSTANTS (FOR FILLING IN THE CONTROL BLOCKS)  
  +0100        *    AND RETURN CODES (FOR TESTING THE RESULT OF A CALL)         
  +0100        *                                                                
  +0100         01  W05-MQM-CONSTANTS.                                          
  +0100             COPY CMQV.                                                  
  +0100        *                                                                
  +0100        *    W06 - RETURN VALUES                                         
  +0100        *                                                                
  +0100         01  W06-CSQ4-OK             PIC S9(4) VALUE 0.                  
  +0100         01  W06-CSQ4-WARNING        PIC S9(4) VALUE 4.                  
  +0100         01  W06-CSQ4-ERROR          PIC S9(4) VALUE 8.                  
  +0100        * ------------------------------------------------------------- *
  +0100        *                                                                
  +0100        * ------------------------------------------------------------- *
  +0100         LINKAGE SECTION.                                                
  +0100        * ------------------------------------------------------------- *
  +0100         01  PARMDATA.                                                   
  +0100             05  PARM-LEN                PIC S9(03) BINARY.              
  +0100             05  PARM-STRING             PIC X(100).                     
  +0100        *                                                                
  +0100             EJECT                                                       
  +0100        * ------------------------------------------------------------- *
  +0100         PROCEDURE DIVISION USING PARMDATA.                              
  +0100        * ------------------------------------------------------------- *
  +0100        * ------------------------------------------------------------- *
  +0100         A-MAIN SECTION.                                                 
  +0100        * ------------------------------------------------------------- *
  +0100        *                                                               *
  +0100        * THIS SECTION RECEIVES THE NAMES OF THE QUEUE MANAGER AND THE  *
  +0100        * QUEUE FROM THE PARM STATEMENT IN THE JCL. IT OPENS THE QUEUE, *
  +0100        * AND SETS TRIGGERING ON.                                       *
  +0100        *                                                               *
  +0100        * ------------------------------------------------------------- *
  +0100        *                                                                
  +0100        *    OPEN THE PRINT FILE, INITIALIZE THE FIELDS FOR THE          
  +0100        *    AND PRINT THE FIRST                                         
  +0100        *    LINE OF THE HEADER                                          
  +0100        *                                                                
  +0100             OPEN OUTPUT SYSPRINT.                                       
  +0100        *                                                                
  +0100             MOVE W01-HEADER-1 TO PRINT-DATA.                            
  +0100             WRITE PRINT-REC AFTER ADVANCING PAGE.                       
  +0100        *                                                                
  +0100        *    IF NO DATA WAS PASSED, CREATE A MESSAGE, PRINT IT, AND      
  +0100        *    EXIT                                                        
  +0100        *                                                                
  +0100             IF PARM-LEN = 0 THEN                                        
  +0100                MOVE W04-MESSAGE-1 TO PRINT-DATA                         
  +0100                WRITE PRINT-REC                                          
  +0100                MOVE W06-CSQ4-WARNING TO W00-RETURN-CODE                 
  +0100                GO TO A-MAIN-END                                         
  +0100             END-IF.                                                     
  +0100        *                                                                
  +0100        *    SEPARATE INTO THE RELEVANT FIELDS ANY DATA PASSED IN THE    
  +0100        *    PARM STATEMENT                                              
  +0100        *                                                                
  +0100             UNSTRING PARM-STRING DELIMITED BY ALL ','                   
  +0100                                     INTO W02-MQM                        
  +0100                                          W02-OBJECT.                    
  +0100        *                                                                
  +0100        *    MOVE THE DATA (SPACES IF NOTHING IS ENTERED) INTO THE       
  +0100        *    RELEVANT PRINT FIELDS                                       
  +0100        *                                                                
  +0100             MOVE W02-MQM    TO W01-MQM-NAME.                            
  +0100             MOVE W02-OBJECT TO W01-QUEUE-NAME.                          
  +0100        *                                                                
  +0100        *    PRINT A MESSAGE IF THE QUEUE MANAGER NAME IS MISSING, THE   
  +0100        *    DEFAULT QUEUE MANAGER WILL BE USED                          
  +0100        *                                                                
  +0100             IF W02-MQM = SPACES OR W02-MQM = LOW-VALUES THEN            
  +0100                MOVE W04-MESSAGE-2 TO PRINT-DATA                         
  +0100                WRITE PRINT-REC                                          
  +0100             END-IF.                                                     
  +0100        *                                                                
  +0100        *    PRINT A MESSAGE IF THE QUEUE NAME IS MISSING AND EXIT FROM  
  +0100        *    PROGRAM                                                     
  +0100        *                                                                
  +0100             IF W02-OBJECT = SPACES OR W02-OBJECT = LOW-VALUES THEN      
  +0100                MOVE W04-MESSAGE-3 TO PRINT-DATA                         
  +0100                WRITE PRINT-REC                                          
  +0100                MOVE W06-CSQ4-WARNING TO W00-RETURN-CODE                 
  +0100                GO TO A-MAIN-END                                         
  +0100             END-IF.                                                     
  +0100        *                                                                
  +0100        *    PRINT THE REMAINING HEADER LINES                            
  +0100        *                                                                
  +0100             MOVE W01-HEADER-2 TO PRINT-DATA.                            
  +0100             WRITE PRINT-REC AFTER ADVANCING 2.                          
  +0100        *                                                                
  +0100             MOVE W01-HEADER-3 TO PRINT-DATA.                            
  +0100             WRITE PRINT-REC AFTER ADVANCING 1.                          
  +0100        *                                                                
  +0100        *    CONNECT TO THE SPECIFIED QUEUE MANAGER.                     
  +0100        *                                                                
  +0100             CALL 'MQCONN' USING W02-MQM                                 
  +0100                                 W03-HCONN                               
  +0100                                 W03-COMPCODE                            
  +0100                                 W03-REASON.                             
  +0100        *                                                                
  +0100        *    TEST THE OUTPUT OF THE CONNECT CALL.  IF THE CALL FAILED,   
  +0100        *    PRINT AN ERROR MESSAGE SHOWING THE COMPLETION CODE AND      
  +0100        *    REASON CODE                                                 
  +0100        *                                                                
  +0100             IF (W03-COMPCODE NOT = MQCC-OK) THEN                        
  +0100                MOVE 'CONNECT'     TO W04-MSG4-TYPE                      
  +0100                MOVE W03-COMPCODE  TO W04-MSG4-COMPCODE                  
  +0100                MOVE W03-REASON    TO W04-MSG4-REASON                    
  +0100                MOVE W04-MESSAGE-4 TO PRINT-DATA                         
  +0100                WRITE PRINT-REC                                          
  +0100                MOVE W06-CSQ4-ERROR TO W00-RETURN-CODE                   
  +0100                GO TO A-MAIN-END                                         
  +0100             END-IF.                                                     
  +0100        *                                                                
  +0100        *    INITIALIZE THE OBJECT DESCRIPTOR (MQOD) CONTROL BLOCK.      
  +0100        *    (THE COPY FILE INITIALIZES ALL THE OTHER FIELDS)            
  +0100        *                                                                
  +0100              MOVE W02-OBJECT        TO MQOD-OBJECTNAME.                 
  +0100        *                                                                
  +0100        *    INITIALIZE THE WORKING STORAGE FIELDS REQUIRED TO OPEN      
  +0100        *    THE QUEUE                                                   
  +0100        *                                                                
  +0100        *      W03-OPTIONS IS SET TO OPEN THE QUEUE FOR SET              
  +0100        *      W03-HOBJ    IS SET BY THE MQOPEN CALL AND IS USED BY THE  
  +0100        *                  MQGET AND MQCLOSE CALLS                       
  +0100        *                                                                
  +0100             MOVE MQOO-SET TO W03-OPTIONS.                               
  +0100        *                                                                
  +0100        *    OPEN THE QUEUE.                                             
  +0100        *                                                                
  +0100             CALL 'MQOPEN' USING W03-HCONN                               
  +0100                                 MQOD                                    
  +0100                                 W03-OPTIONS                             
  +0100                                 W03-HOBJ                                
  +0100                                 W03-COMPCODE                            
  +0100                                 W03-REASON.                             
  +0100        *                                                                
  +0100        *    TEST THE OUTPUT OF THE OPEN CALL.  IF THE CALL FAILED, PRINT
  +0100        *    AN ERROR MESSAGE SHOWING THE COMPLETION CODE AND REASON CODE
  +0100        *                                                                
  +0100             IF (W03-COMPCODE NOT = MQCC-OK) THEN                        
  +0100                MOVE 'OPEN'        TO W04-MSG4-TYPE                      
  +0100                MOVE W03-COMPCODE  TO W04-MSG4-COMPCODE                  
  +0100                MOVE W03-REASON    TO W04-MSG4-REASON                    
  +0100                MOVE W04-MESSAGE-4 TO PRINT-DATA                         
  +0100                WRITE PRINT-REC                                          
  +0100                MOVE W06-CSQ4-ERROR TO W00-RETURN-CODE                   
  +0100                GO TO A-MAIN-DISCONNECT                                  
  +0100             END-IF.                                                     
  +0100        *                                                                
  +0100             MOVE MQIA-TRIGGER-CONTROL TO W03-SELECTORS(1).              
  +0100             MOVE MQTC-ON TO W03-INT-ATTRS(1).                           
  +0100                CALL 'MQSET' USING W03-HCONN                             
  +0100                                   W03-HOBJ                              
  +0100                                   W03-SELECTOR-COUNT                    
  +0100                                   W03-SELECTOR-ARRAY                    
  +0100                                   W03-INT-ATTR-COUNT                    
  +0100                                   W03-INT-ATTR-ARRAY                    
  +0100                                   W03-CHAR-ATTR-LENGTH                  
  +0100                                   W03-CHAR-ATTRS                        
  +0100                                   W03-COMPCODE                          
  +0100                                   W03-REASON                            
  +0100             IF NOT W03-COMPCODE = MQCC-OK                               
  +0100             THEN                                                        
  +0100                MOVE 'SET'         TO W04-MSG4-TYPE                      
  +0100                MOVE W03-COMPCODE  TO W04-MSG4-COMPCODE                  
  +0100                MOVE W03-REASON    TO W04-MSG4-REASON                    
  +0100                MOVE W04-MESSAGE-4 TO PRINT-DATA                         
  +0100             END-IF.                                                     
  +0100        *                                                                
  +0100        *                                                                
  +0100        * CLOSE THE QUEUE                                                
  +0100        *                                                                
  +0100             MOVE MQCO-NONE TO W03-OPTIONS.                              
  +0100        *                                                                
  +0100             CALL 'MQCLOSE' USING W03-HCONN                              
  +0100                                  W03-HOBJ                               
  +0100                                  W03-OPTIONS                            
  +0100                                  W03-COMPCODE                           
  +0100                                  W03-REASON.                            
  +0100        *                                                                
  +0100        *    TEST THE OUTPUT OF THE MQCLOSE CALL.  IF THE CALL FAILED,   
  +0100        *    PRINT AN ERROR MESSAGE SHOWING THE COMPLETION CODE AND REASO
  +0100        *    CODE                                                        
  +0100        *                                                                
  +0100             IF (W03-COMPCODE NOT = MQCC-OK) THEN                        
  +0100                MOVE 'CLOSE'       TO W04-MSG4-TYPE                      
  +0100                MOVE W03-COMPCODE  TO W04-MSG4-COMPCODE                  
  +0100                MOVE W03-REASON    TO W04-MSG4-REASON                    
  +0100                MOVE W04-MESSAGE-4 TO PRINT-DATA                         
  +0100                WRITE PRINT-REC                                          
  +0100                MOVE W06-CSQ4-ERROR TO W00-RETURN-CODE                   
  +0100             END-IF.                                                     
  +0100        *                                                                
  +0100         A-MAIN-DISCONNECT.                                              
  +0100        *                                                                
  +0100        * DISCONNECT FROM THE QUEUE MANAGER                              
  +0100        *                                                                
  +0100             CALL 'MQDISC' USING W03-HCONN                               
  +0100                                 W03-COMPCODE                            
  +0100                                 W03-REASON.                             
  +0100        *                                                                
  +0100        *    TEST THE OUTPUT OF THE DISCONNECT CALL.  IF THE CALL FAILED,
  +0100        *    PRINT AN ERROR MESSAGE SHOWING THE COMPLETION CODE AND      
  +0100        *    REASON CODE                                                 
  +0100        *                                                                
  +0100             IF (W03-COMPCODE NOT = MQCC-OK) THEN                        
  +0100                MOVE 'DISCONNECT'  TO W04-MSG4-TYPE                      
  +0100                MOVE W03-COMPCODE  TO W04-MSG4-COMPCODE                  
  +0100                MOVE W03-REASON    TO W04-MSG4-REASON                    
  +0100                MOVE W04-MESSAGE-4 TO PRINT-DATA                         
  +0100                MOVE W06-CSQ4-ERROR TO W00-RETURN-CODE                   
  +0100                WRITE PRINT-REC                                          
  +0100             END-IF.                                                     
  +0100        *                                                                
  +0100         A-MAIN-END.                                                     
  +0100        *                                                                
  +0100        *    SET THE RETURN CODE                                         
  +0100        *                                                                
  +0100             MOVE W00-RETURN-CODE TO RETURN-CODE.                        
  +0100        *                                                                
  +0100        *    CLOSE THE PRINT FILE AND STOP                               
  +0100        *                                                                
  +0100             CLOSE SYSPRINT.                                             
  +0100             STOP RUN.                                                   
  +0100        * ---------------------------------------------------------------
  +0100        *                  END OF PROGRAM                                
  +0100        * ---------------------------------------------------------------
