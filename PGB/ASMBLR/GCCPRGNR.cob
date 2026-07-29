       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPRGNR.                                                 
      *AUTHOR.        JUDY ELKINS.                                              
      ******************************************************************        
      *         << GROUP BENEFITS INTERNET REGISTRATION >>                      
      *         << CONFIRMATION LETTER MAIL CODE CHECK >>                       
      *                                                                         
      * PROGRAM DESCRIPTION:                                                    
      *   THIS PROGRAM READS A FLAT FILE CREATED BY GCCPRGNA THAT               
      *   CONTAINS THE REGISTRATION LETTER REQUESTS WHICH INCLUDES              
      *   APPROPRIATE EMPLOYEE ADDRESS.  THESE RECORDS CONTAIN A                
      *   MAILING CODE THAT HAS BEEN SET ACCORDINGLY.                           
      *                                                                         
      *   C - SEND BY COURIER   TO PLAN ADMINISTRATOR                           
      *   B - SEND BY BULK MAIL TO PLAN ADMINISTRATOR                           
      *   D - SEND TO INDIVIDUAL EMPLOYEES BY DIRECT MAIL                       
      *                                                                         
      *   FOR LETTERS TO BE SENT BULK MAIL THERE MUST BE A LEAST                
      *   2 FOR A GROUP/DIVISION, OTHERWISE THE LETTERS SHOULD                  
      *   BE SEND BY DIRECT MAIL.                                               
      *   THIS PROGRAM WILL CHANGE THE MAIL CODE FROM BULK TO                   
      *   DIRECT FOR GROUP/DIVISIONS WITH 2 OR LESS                             
      *                                                                         
      * CALLING MODULES                                                         
      *     THIS IS THE MAINLINE PROGRAM                                        
      *                                                                         
      * CALLED MODULES                                                          
      *     GAEDATSR - DATA SERVER                                              
      *                                                                         
      * COPYBOOKS                                                               
      *     GCCCCEXT - REGISTRATION LETTER REQUESTS (WITH ADDRESSES)            
      *                                                                         
      * INPUT  - FLAT FILE OF REGISTRATION LETTER REQUESTS                      
      *                                                                         
      * OUTPUT - FLAT FILE OF REGISTRATION LETTER REQUESTS WITH CORRECT         
      *          MAIL CODE                                                      
      *                                                                         
      ******************************************************************        
      * DATE       NAME      DESCRIPTION                                        
      * ---------  --------  -------------------------------------------        
      * 01NOV2001  J.ELKINS  CREATION.                                          
      * 12APR2002  J.ELKINS  FOR LETTERS BEING MAILED TO THE COMPANY            
      *                      PUT ATTN IN FRONT OF EE'S NAME AND C/O             
      *                      IN FRONT OF COMPANY NAME                           
      * 09JUN2006  M.COOPER  INCREASED LENGTH FOR WS-SAVE1 AND WS-SAVE2.        
      *                      PARTS OF RECORDS WERE BEING CUT OFF.               
      * 26JUL2007B CHAPMANINCREASE LENGTH OF WS-SAVE1 AND WS-SAVE2              
      * 26AUG2008  IBM GR    UPGRADED IN ECU PROJECT                            
      ******************************************************************        
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-370-165.                                            
       OBJECT-COMPUTER. IBM-370-165.                                            
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-CONSTANTS.                                                        
           05  WS-START                  PIC X(32) VALUE                        
               '*** GCCPRGNR WORKING STORAGE ***'.                              
                                                                                
           05  WS-GAEDATSR-VERB          PIC X(16).                             
           05  GAEDATSR                  PIC X(8)  VALUE 'GAEDATSR'.            
                                                                                
           05  WS-NO-INP-RECORDS         PIC 9(7)  VALUE ZERO.                  
           05  WS-NO-OUT-RECORDS         PIC 9(7)  VALUE ZERO.                  
                                                                                
           05  WS-GROUP-DIV-CTR          PIC S9(7)  VALUE ZERO.                 
                                                                                
           05  WS-PREV-GROUP             PIC 9(7).                              
           05  WS-PREV-DIVISION          PIC X(3).                              
                                                                                
           05  WS-SAVE-1                 PIC X(486).                            
           05  WS-SAVE-2                 PIC X(486).                            
                                                                                
           05  WS-OBTAIN-FIRST    PIC X(16)   VALUE 'OBTAIN  FIRST   '.         
           05  WS-OBTAIN-NEXT     PIC X(16)   VALUE 'OBTAIN  NEXT    '.         
                                                                                
           05  WS-INPUT-LR        PIC X(16)   VALUE 'CARD-DATA-010   '.         
           05  WS-OUTPUT-LR       PIC X(16)   VALUE 'PRINT-DATA-020  '.         
                                                                                
                                                                                
           05  WS-ZPARDUMP-FUNCTION      PIC X(1)  VALUE '1'.                   
                                                                                
           05  WS-NAME                   PIC X(79).                             
                                                                                
           05  WS-ADDR                   PIC X(30) OCCURS 4 TIMES.              
                                                                                
           05  WS-ADDR-WORK-AREA.                                               
               10  WS-ADDR-CHAR          PIC X     OCCURS 30 TIMES.             
                                                                                
           05  WS-ATTN-TEXT              PIC X(10).                             
           05  WS-ATTN-TEXT-LEN    COMP  PIC S9(4).                             
                                                                                
           05  CARE-OF-REPL.                                                    
               10  FILLER                PIC X(6)  VALUE SPACES.                
               10  CARE-OF-TEXT          PIC X(4).                              
                                                                                
           05  LINE-SUB            COMP  PIC S9(4).                             
           05  ATTN-SUB            COMP  PIC S9(4).                             
           05  CHAR-SUB            COMP  PIC S9(4).                             
                                                                                
       01  ATTENTION-REPLACEMENT-INFO.                                          
           05  FILLER            VALUE 'ATTENTION:'     PIC X(10).              
           05  FILLER            COMP  VALUE +10        PIC S9(4).              
           05  FILLER            VALUE 'ATTEN:'         PIC X(10).              
           05  FILLER            COMP  VALUE +6         PIC S9(4).              
           05  FILLER            VALUE 'ATTN:'          PIC X(10).              
           05  FILLER            COMP  VALUE +5         PIC S9(4).              
           05  FILLER            VALUE 'ATTN :'         PIC X(10).              
           05  FILLER            COMP  VALUE +6         PIC S9(4).              
           05  FILLER            VALUE 'ATTN  :'        PIC X(10).              
           05  FILLER            COMP  VALUE +7         PIC S9(4).              
           05  FILLER            VALUE 'ATTN '          PIC X(10).              
           05  FILLER            COMP  VALUE +5         PIC S9(4).              
           05  FILLER            VALUE 'ATTN.:'         PIC X(10).              
           05  FILLER            COMP  VALUE +6         PIC S9(4).              
           05  FILLER            VALUE 'ATTN.'          PIC X(10).              
           05  FILLER            COMP  VALUE +5         PIC S9(4).              
           05  FILLER            VALUE 'ATTN;'          PIC X(10).              
           05  FILLER            COMP  VALUE +5         PIC S9(4).              
           05  FILLER            VALUE 'ATTN,:'         PIC X(10).              
           05  FILLER            COMP  VALUE +6         PIC S9(4).              
           05  FILLER            VALUE 'ATT.:'          PIC X(10).              
           05  FILLER            COMP  VALUE +5         PIC S9(4).              
           05  FILLER            VALUE 'ATT.;'          PIC X(10).              
           05  FILLER            COMP  VALUE +5         PIC S9(4).              
           05  FILLER            VALUE 'ATT.'           PIC X(10).              
           05  FILLER            COMP  VALUE +4         PIC S9(4).              
                                                                                
       01  ATTENTION-REPLACEMENT-TABLE        REDEFINES                         
                     ATTENTION-REPLACEMENT-INFO.                                
           05  FILLER            OCCURS 13.                                     
               10  ATTN-TEXT                            PIC X(10).              
               10  ATTN-TEXT-LEN     COMP               PIC S9(4).              
                                                                                
                                                                                
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                          
                                                                                
       01  GAC-DATE-PARAMETERS.         COPY GARDATEP.                          
                                                                                
       01  LOGICAL-RECORD-NAMES.        COPY HCSLRNAM.                          
                                                                                
       01  INP-RECORD.                                                          
             COPY GCCCCEXT.                                                     
                                                                                
       01  OUT-RECORD.                                                          
             COPY GCCCCEXT.                                                     
                                                                                
       01  WS-RECORD.                                                           
             COPY GCCCCEXT.                                                     
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
                                                                                
       01  WS-END-BYTE-X             PIC X(1)       VALUE SPACES.               
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
                                                                                
                                                                                
           PERFORM 2000-PROCESS-INPUT                                           
              THRU 2000-EXIT                                                    
                   UNTIL NOT LR-STATUS-OK.                                      
                                                                                
           PERFORM 9000-FINISH          THRU 9000-EXIT.                         
                                                                                
       0000-EXIT.                                                               
           GOBACK.                                                              
                                                                                
       1000-INITIALIZATION.                                                     
                                                                                
           MOVE 'GCCPRGNR'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
      ******************************************************************        
      * READ FIRST INPUT RECORD                                                 
      *  - FOR BULK MAIL THERE MUST BE 3 OR MORE RECORDS FOR EACH               
      *    GROUP DIVISION.  SET COUNTER TO 0 AND BEGIN COUNTING                 
      *  - IF NOT BULK MAIL SET COUNTER TO 3 SO RECORDS WILL BE                 
      *    READ IN AND WRITTEN OUT WITHOUT ANY CHANGES                          
      ******************************************************************        
                                                                                
           MOVE WS-OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                       
           PERFORM 3000-READ-INPUT THRU                                         
                   3000-EXIT.                                                   
                                                                                
           IF LR-STATUS-OK                                                      
               MOVE GCCCCEXT-CUST-GROUP OF WS-RECORD                            
                                         TO WS-PREV-GROUP                       
               MOVE GCCCCEXT-CUST-DIVISION OF WS-RECORD                         
                                         TO WS-PREV-DIVISION                    
               IF GCCCCEXT-MAIL-INSTRUCTION OF WS-RECORD                        
                                         NOT = 'B'                              
                   MOVE 3                TO WS-GROUP-DIV-CTR                    
               ELSE                                                             
                   MOVE 0                TO WS-GROUP-DIV-CTR                    
               END-IF                                                           
           END-IF.                                                              
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-PROCESS-INPUT.                                                      
      ******************************************************************        
      * PROCESS INPUT RECORDS                                                   
      * IF NEW GROUP/DIV                                                        
      *    AND WS-GROUP-DIV-CTR = 2 CHANGE GCCCCEXT-MAIL-INSTRUCTION            
      *      TO DIRECT AND WRITE THE 2 SAVED RECORDS                            
      *    AND WS-GROUP-DIV-CTR = 1 CHANGE GCCCCEXT-MAIL-INSTRUCTION            
      *      TO DIRECT AND WRITE THE 1 SAVED RECORD                             
      *    FOR BULK SAVE THE NEW RECORD                                         
      * IF NOT A NEW GROUP/DIV                                                  
      *    COUNT THE NEWLY READ RECORD                                          
      *    IF IT IS THE FIRST RECORD FOR THIS GROUP/DIV SAVE IT IN              
      *    SAVE AREA 1                                                          
      *    IF IT IS THE SECOND RECORD FOR THIS GROUP/DIV SAVE IT IN             
      *    SAVE AREA 2                                                          
      *    IF IT IS THE THIRD RECORD FOR THIS GROUP/DIV WRITE THE               
      *    TWO SAVED RECORDS AND THE INPUT RECORD                               
      *                                                                         
      ******************************************************************        
                                                                                
                                                                                
           IF GCCCCEXT-CUST-GROUP    OF INP-RECORD                              
                                         =  WS-PREV-GROUP AND                   
              GCCCCEXT-CUST-DIVISION OF WS-RECORD                               
                                         =  WS-PREV-DIVISION                    
               ADD 1                     TO WS-GROUP-DIV-CTR                    
                                                                                
               EVALUATE TRUE                                                    
                 WHEN WS-GROUP-DIV-CTR   = +3                                   
                     MOVE WS-SAVE-1      TO OUT-RECORD                          
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
                     MOVE SPACES         TO WS-SAVE-1                           
                     MOVE WS-SAVE-2      TO OUT-RECORD                          
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
                     MOVE WS-RECORD     TO OUT-RECORD                           
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
                     MOVE SPACES         TO WS-SAVE-2                           
                 WHEN WS-GROUP-DIV-CTR   > +3                                   
                     MOVE WS-RECORD     TO OUT-RECORD                           
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
                 WHEN WS-GROUP-DIV-CTR   = +2                                   
                     MOVE WS-RECORD     TO WS-SAVE-2                            
                 WHEN WS-GROUP-DIV-CTR   = +1                                   
                     MOVE WS-RECORD     TO WS-SAVE-1                            
                                                                                
               END-EVALUATE                                                     
           ELSE                                                                 
               MOVE GCCCCEXT-CUST-GROUP    OF WS-RECORD                         
                                         TO WS-PREV-GROUP                       
               MOVE GCCCCEXT-CUST-DIVISION OF WS-RECORD                         
                                         TO WS-PREV-DIVISION                    
               EVALUATE TRUE                                                    
                 WHEN WS-GROUP-DIV-CTR   = +1                                   
                     MOVE WS-SAVE-1      TO OUT-RECORD                          
                     MOVE 'D'            TO GCCCCEXT-MAIL-INSTRUCTION           
                                         OF OUT-RECORD                          
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
                     MOVE SPACES         TO WS-SAVE-1                           
                 WHEN WS-GROUP-DIV-CTR   = +2                                   
                     MOVE WS-SAVE-1      TO OUT-RECORD                          
                     MOVE 'D'            TO GCCCCEXT-MAIL-INSTRUCTION           
                                         OF OUT-RECORD                          
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
                     MOVE SPACES         TO WS-SAVE-1                           
                     MOVE WS-SAVE-2      TO OUT-RECORD                          
                     MOVE 'D'            TO GCCCCEXT-MAIL-INSTRUCTION           
                                         OF OUT-RECORD                          
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
                     MOVE SPACES         TO WS-SAVE-2                           
               END-EVALUATE                                                     
               IF GCCCCEXT-MAIL-INSTRUCTION OF WS-RECORD                        
                                         NOT = 'B'                              
                   MOVE +3               TO WS-GROUP-DIV-CTR                    
                   MOVE WS-RECORD       TO OUT-RECORD                           
                   PERFORM 4000-WRITE-OUTPUT                                    
                      THRU 4000-EXIT                                            
               ELSE                                                             
                   MOVE +1               TO WS-GROUP-DIV-CTR                    
                   MOVE WS-RECORD       TO WS-SAVE-1                            
               END-IF                                                           
           END-IF.                                                              
                                                                                
           PERFORM 3100-GET-NEXT-INP-RECORD                                     
              THRU 3100-EXIT.                                                   
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       3000-READ-INPUT.                                                         
      ******************************************************************        
      *    READ INPUT RECORD.                                          *        
      ******************************************************************        
           MOVE WS-INPUT-LR          TO LOGICAL-RECORD-NAME.                    
           INITIALIZE INP-RECORD.                                               
                                                                                
           CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
                                         INP-RECORD                             
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               ADD +1 TO WS-NO-INP-RECORDS                                      
               MOVE INP-RECORD           TO WS-RECORD                           
               IF GCCCCEXT-CO-NAME1 OF INP-RECORD                               
                                         =  SPACES                              
                   NEXT SENTENCE                                                
               ELSE                                                             
                   PERFORM 5000-FORMAT-ADDRESS                                  
                      THRU 5000-EXIT                                            
               END-IF                                                           
           END-IF.                                                              
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3100-GET-NEXT-INP-RECORD.                                                
      *****************************************************************         
      * READ NEXT INPUT RECORD                                        *         
      *****************************************************************         
                                                                                
           MOVE WS-OBTAIN-NEXT       TO WS-GAEDATSR-VERB.                       
           PERFORM 3000-READ-INPUT                                              
              THRU 3000-EXIT.                                                   
                                                                                
       3100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       4000-WRITE-OUTPUT.                                                       
      *****************************************************************         
      * WRITE OUTPUT RECORD                                           *         
      *****************************************************************         
                                                                                
           MOVE WS-OUTPUT-LR             TO LOGICAL-RECORD-NAME.                
                                                                                
           MOVE STORE-LR                 TO     WS-GAEDATSR-VERB.               
           CALL GAEDATSR                 USING  WS-GAEDATSR-VERB                
                                         OUT-RECORD                             
                                         ICBM.                                  
           IF LR-STATUS-OK                                                      
               ADD +1                    TO WS-NO-OUT-RECORDS                   
           ELSE                                                                 
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
       4000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       5000-FORMAT-ADDRESS.                                                     
      *****************************************************************         
      * PUT ATTN: IN FRONT OF CUSTOMER NAME                                     
      * LOOK AT THE 4 CUST-ADDR LINES TO SEE IF THEY CONTAIN                    
      * ANY OF THE 13 FORMS OF "ATTENTION".  REPLACE THIS WITH                  
      * C/O OR A/S DEPENDING ON LANGUAGE                                        
      *****************************************************************         
                                                                                
           MOVE GCCCCEXT-CUST-NAME OF WS-RECORD                                 
                                         TO WS-NAME.                            
                                                                                
           STRING 'ATTN: ' WS-NAME DELIMITED BY SIZE                            
                                   INTO  GCCCCEXT-CUST-NAME                     
                                         OF WS-RECORD.                          
                                                                                
WB         IF GCCCCEXT-STAT-REAS-CD   OF INP-RECORD =  'V' OR 'O'               
WB            MOVE GCCCCEXT-CUST-NAME OF WS-RECORD TO  WS-NAME                  
WB            MOVE GCCCCEXT-CO-NAME1  OF INP-RECORD                             
WB                                   TO  GCCCCEXT-CUST-NAME OF WS-RECORD        
WB            MOVE WS-NAME  TO  GCCCCEXT-CO-NAME1 OF WS-RECORD                  
WB         END-IF.                                                              
                                                                                
           MOVE GCCCCEXT-CUST-ADDR1      OF WS-RECORD                           
                                         TO WS-ADDR (1).                        
           MOVE GCCCCEXT-CUST-ADDR2      OF WS-RECORD                           
                                         TO WS-ADDR (2).                        
           MOVE GCCCCEXT-CUST-ADDR3      OF WS-RECORD                           
                                         TO WS-ADDR (3).                        
           MOVE GCCCCEXT-CUST-ADDR4      OF WS-RECORD                           
                                         TO WS-ADDR (4).                        
                                                                                
           IF GCCCCEXT-CUST-REPLY-LANG   OF WS-RECORD                           
                                         =  'F'                                 
               MOVE 'A/S:'               TO CARE-OF-TEXT                        
           ELSE                                                                 
               MOVE 'C/O:'               TO CARE-OF-TEXT                        
           END-IF.                                                              
                                                                                
           PERFORM VARYING     LINE-SUB FROM 1 BY 1                             
             UNTIL             LINE-SUB > 4                                     
                                                                                
               PERFORM VARYING ATTN-SUB FROM 1 BY 1                             
                 UNTIL         ATTN-SUB > 13                                    
                                                                                
                   MOVE ATTN-TEXT (ATTN-SUB)                                    
                                         TO WS-ATTN-TEXT                        
                   MOVE ATTN-TEXT-LEN (ATTN-SUB)                                
                                         TO WS-ATTN-TEXT-LEN                    
                   INSPECT WS-ADDR (LINE-SUB)                                   
                     REPLACING ALL WS-ATTN-TEXT (1:WS-ATTN-TEXT-LEN)            
                     BY CARE-OF-REPL (11 - WS-ATTN-TEXT-LEN:                    
                                         WS-ATTN-TEXT-LEN )                     
                                                                                
               END-PERFORM                                                      
                                                                                
      * REMOVE LEADING BLANKS FROM LINE                                         
                                                                                
                   MOVE WS-ADDR (LINE-SUB)                                      
                                         TO WS-ADDR-WORK-AREA                   
                                                                                
                   PERFORM VARYING CHAR-SUB FROM 1 BY 1                         
                     UNTIL         CHAR-SUB > 30                                
                                                                                
                       IF WS-ADDR-CHAR (CHAR-SUB) NOT = SPACES                  
                           MOVE WS-ADDR-WORK-AREA (CHAR-SUB :                   
                                              31 - CHAR-SUB)                    
                                         TO WS-ADDR (LINE-SUB)                  
                           MOVE 31       TO CHAR-SUB                            
                       END-IF                                                   
                   END-PERFORM                                                  
           END-PERFORM.                                                         
                                                                                
           MOVE WS-ADDR (1)              TO GCCCCEXT-CUST-ADDR1                 
                                         OF WS-RECORD.                          
           MOVE WS-ADDR (2)              TO GCCCCEXT-CUST-ADDR2                 
                                         OF WS-RECORD.                          
           MOVE WS-ADDR (3)              TO GCCCCEXT-CUST-ADDR3                 
                                         OF WS-RECORD.                          
           MOVE WS-ADDR (4)              TO GCCCCEXT-CUST-ADDR4                 
                                         OF WS-RECORD.                          
                                                                                
       5000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       9000-FINISH.                                                             
      ******************************************************************        
      * IF WS-GROUP-DIV-CTR = 2                                                 
      *   CHANGE THE GCCCCEXT-MAIL-INSTRUCTION TO DIRECT AND                    
      *   WRITE THE SAVED RECORDS                                               
      * IF WS-GROUP-DIV-CTR =1                                                  
      *   CHANGE THE GCCCCEXT-MAIL-INSTRUCTIONTO DIRECT AND                     
      *   WRITE THE SAVED RECORD                                                
      * CLOSE FILES THAT ARE OPEN...                                            
      ******************************************************************        
                                                                                
           EVALUATE TRUE                                                        
             WHEN WS-GROUP-DIV-CTR       = +2                                   
                     MOVE WS-SAVE-1      TO OUT-RECORD                          
                     MOVE 'D'            TO GCCCCEXT-MAIL-INSTRUCTION           
                                         OF OUT-RECORD                          
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
                     MOVE WS-SAVE-2      TO OUT-RECORD                          
                     MOVE 'D'            TO GCCCCEXT-MAIL-INSTRUCTION           
                                         OF OUT-RECORD                          
                     PERFORM 4000-WRITE-OUTPUT                                  
                        THRU 4000-EXIT                                          
             WHEN WS-GROUP-DIV-CTR       = +1                                   
                 MOVE WS-SAVE-1          TO OUT-RECORD                          
                 MOVE 'D'                TO GCCCCEXT-MAIL-INSTRUCTION           
                                         OF OUT-RECORD                          
                 PERFORM 4000-WRITE-OUTPUT                                      
                    THRU 4000-EXIT                                              
           END-EVALUATE.                                                        
                                                                                
           MOVE WS-INPUT-LR TO LOGICAL-RECORD-NAME.                             
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB.                                
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
           IF LR-STATUS-OK                                                      
                 NEXT SENTENCE                                                  
              ELSE                                                              
                 DISPLAY 'ERROR CLOSING INPUT LETTER FILE'                      
                 DISPLAY PROGRAM-LINKAGE-STATUS                                 
                 PERFORM 9999-ABEND                                             
                    THRU 9999-EXIT.                                             
                                                                                
                                                                                
           MOVE WS-OUTPUT-LR             TO LOGICAL-RECORD-NAME.                
           MOVE FINISH-LR                TO WS-GAEDATSR-VERB.                   
           CALL GAEDATSR USING           WS-GAEDATSR-VERB                       
                                         LOGICAL-RECORD-NAME                    
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING OUTPUT AFP FILE'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           DISPLAY 'REGISTRATION LETTERS IN : ' WS-NO-INP-RECORDS.              
           DISPLAY 'REGISTRATION LETTERS OUT: ' WS-NO-OUT-RECORDS.      ECS.    
                                                                                
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9999-ABEND.                                                              
      ******************************************************************        
      *    TIME TO ABEND.                                                       
      ******************************************************************        
                                                                                
           CALL 'ZPARDUMP' USING WS-ZPARDUMP-FUNCTION                           
                                 WS-START                                       
                                 WS-END-BYTE-X                                  
           END-CALL.                                                            
                                                                                
           MOVE +16 TO RETURN-CODE.                                             
                                                                                
           GOBACK.                                                              
                                                                                
                                                                                
       9999-EXIT.                                                               
           EXIT.                                                                
