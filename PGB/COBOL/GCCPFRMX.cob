       CBL DATA(24)                                                             
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRMX.                                                 
      *AUTHOR.        ANGELA YEATES                                             
      ******************************************************************        
      * PROGRAM DESCRIPTION:                                                    
      *   THIS PROGRAM READS THE &&SRTEXTNS FILE IN PGB1100 AND                 
      *      SEPARATES THE TERMINATION RECORDS FROM ANY OTHER RECORDS.          
      *   IT CREATES                                                            
      *      PGB.REGN.PROD.FORMOUT.OTHER = OTHER RECORDS                        
      *      PGB.REGN.PROD.FORMOUT.TERM  = GIPSY TERMINATION RECORDS            
      *   (THE SALARY CHANGE RECORDS WERE SEPARATED OUT DURING CREATION         
      *      OF &&SRTEXTNS)                                                     
      *                                                                         
      *   THE NEW FILES WILL BE INPUT FILES OF OTHER BATCH JOBS.                
      *      PGB.REGN.PROD.FORMOUT.OTHER                                        
      *                 TO PGB1100A: OTHER FILE REPORTING                       
      *      PGB.REGN.PROD.FORMOUT.TERM                                         
      *                 TO PGB1100C: GIPSY TERMINATIONS REPORTING               
      *                                                                         
      * CALLING MODULES                                                         
      *     NOT APPLICABLE.                                                     
      *                                                                         
      * CALLED MODULES                                                          
      *     GAEDATSR - DATA SERVER                                              
      *     GACCDMA7 - READ GIPSY MASTER                                        
      *                                                                         
      * COPYBOOKS                                                               
      *     GARDSVRB - VERBS FOR GAEDATSR                                       
      *     GCCCFEXT - REPORT EXTRACT LAYOUT DETAIL                             
      *     GCCCMASS - INPUT FORM LAYOUT                                        
      *     GMASTER7 - GIPSY MASTER LAYOUT                                      
      *                                                                         
      * INPUT  - SORTED FLAT FILE OF PRINT EXTRACT                              
      *                                                                         
      * OUTPUT - GIPSY TERMINATION SORTED EXTRACT FILE                          
      *        - NON SALARY CHANGE/NON TERMINATION/NON GIPSY                    
      *          SORTED EXTRACT FILE                                            
      *                                                                         
      ******************************************************************        
      * DATE       NAME      DESCRIPTION                                        
      * ---------  --------  -------------------------------------------        
      * 05JUN2017  YEATEAN   CREATED                                            
      * 31JUL2017  YEATEAN   ADD READ OF GIPSY MASTER TO SORT OUT               
      *                      NON-GIPSY TRANSACTIONS                             
      * 03MAY2018  YEATEAN   JIRA GBE331: SPLIT TERM INDEX IF THERE             
      *                      IS NON-TERM INFORMATION IN THE INDEX.              
      *                      CHG LOWER-CASE ACCOUNT TO UPPER CASE BEFORE        
      *                      READ GIPSY MASTER.                                 
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
                                                                                
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GAEDATSR-VERB            PIC X(16).                           
           05  WS-GAEDATSR                 PIC X(8)  VALUE 'GAEDATSR'.          
           05  WS-GACCDMA7                 PIC X(8)  VALUE 'GACCDMA7'.          
                                                                                
       01  WS-MARKERS.                                                          
           05  WS-EOF-MKR                  PIC X     VALUE 'N'.                 
               88  WS-EOF                            VALUE 'Y'.                 
           05  WS-TERM-MKR                 PIC X     VALUE 'N'.                 
               88  WS-TERM                           VALUE 'Y'.                 
           05  WS-OTHER-MKR                PIC X     VALUE 'N'.                 
               88  WS-OTHER                          VALUE 'Y'.                 
           05  WS-GIPSY-MKR                PIC X     VALUE 'N'.                 
               88  WS-GIPSY                          VALUE 'Y'.                 
                                                                                
       01  OCCURS-INDEXES.                                                      
      *****INDEX OF CURRENT POSITION ON INPUT RECORD                            
           05  ICP                         PIC 9(2)  VALUE ZERO.                
      *****INDEX FOR A NON-TERMINATION ITEM                                     
           05  IOT                         PIC 9(2)  VALUE ZERO.                
      *****INDEX FOR A TERMINATION ITEM                                         
           05  ITM                         PIC 9(2)  VALUE ZERO.                
                                                                                
       01  WS-VARIABLES.                                                        
           05  WS-SORT-IN-LR               PIC X(16) VALUE                      
                                               'CARD-DATA-070   '.              
           05  WS-SORTOT-OUT-LR            PIC X(16) VALUE                      
                                               'PRINT-DATA-071  '.              
           05  WS-SORTTM-OUT-LR            PIC X(16) VALUE                      
                                               'PRINT-DATA-072  '.              
           05  WS-GEN-INFO-HEADER          PIC X(47).                           
           05  WS-GEN-INFO-TMSTP           PIC X(60).                           
                                                                                
      *****THIS RECORD WILL BE USED AS BOTH INPUT AND OUTPUT                    
       01  INOUT-RECORD.                                                        
           03  GCCCFEXT-RECL               PIC S9(4)  COMP.                     
           03  FILLER                      PIC XX     VALUE  SPACES.            
           03  GCCCFEXT-DETAIL.                                                 
               COPY GCCCFEXT.                                                   
                                                                                
       01  WS-TEMP-RECORD.                                                      
                                                                                
      *****NOTE THIS EXTRA CHARACTER WAS ADDED TO HIGHLIGHT                     
      *****THE FACT THAT THE HEADERS ARE DIFFERENT LENGTHS                      
      *****ON THE EXTRACT AND THE DETAIL.                                       
                                                                                
           03  WS-TEMP-EXTRA-HEAD-CHAR     PIC X.                               
           03  WS-TEMP-GCCC-HEAD-AND-DETL.                                      
               05  WS-TEMP-GCCC-HEADER     PIC X(61).                           
               05  WS-TEMP-GCCC-DETAIL     PIC X(3956).                         
           03 FILLER             REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.          
              COPY GCCCMASS.                                                    
                                                                                
      ***** NOTE: IF GCCCMASS CHANGES DEFINITION, MAY NEED TO ALSO              
      *****       CHANGE THE FOLLOWING REDEFINE (SHOULD MATCH)                  
       01  WS-TERM-RECORD.                                                      
           03  WS-TERM-HEADER              PIC X(61).                           
           03  WS-TERM-GEN-INFO.                                                
               05  WS-TERM-GEN-INFO-TMSTP  PIC X(60).                           
               05  WS-TERM-GEN-INFO-CLNM   PIC X(60).                           
               05  WS-TERM-GEN-INFO-DTL    OCCURS 15.                           
                   10  WS-TERM-GEN-CERTINFO    PIC X(96).                       
                   10  WS-TERM-GEN-EFF-DATE    PIC X(8).                        
                   10  WS-TERM-GEN-TERM-REASON PIC XX.                          
                   10  WS-TERM-GEN-RTN-DATE    PIC X(8).                        
                   10  WS-TERM-GEN-SAL-AMT     PIC 9(11)V99.                    
                   10  WS-TERM-GEN-SAL-FREQ    PIC X.                           
                   10  WS-TERM-GEN-OCC         PIC X(29).                       
                   10  WS-TERM-GEN-CLASS-NEW   PIC X(3).                        
                   10  WS-TERM-GEN-CLASS-OLD   PIC X(3).                        
                   10  WS-TERM-GEN-ACCT-CHG    PIC X(3).                        
                   10  WS-TERM-GEN-DIV-NEW     PIC X(3).                        
                   10  WS-TERM-GEN-DIV-OLD     PIC X(3).                        
                   10  WS-TERM-GEN-EVIDENCE    PIC X.                           
                   10  WS-TERM-GEN-MAILED      PIC X.                           
                   10  WS-TERM-GEN-WORK-HRS    PIC 9(11)V99.                    
               05  WS-TERM-GEN-INFO-CRTAUTH PIC X(9).                           
                                                                                
      ***** NOTE: IF GCCCMASS CHANGES DEFINITION, MAY NEED TO ALSO              
      *****       CHANGE THE FOLLOWING REDEFINE (SHOULD MATCH)                  
       01  WS-OTHER-RECORD.                                                     
           03  WS-OTH-HEADER               PIC X(61).                           
           03  WS-OTH-GEN-INFO.                                                 
               05  WS-OTH-GEN-INFO-TMSTP   PIC X(60).                           
               05  WS-OTH-GEN-INFO-CLNM    PIC X(60).                           
               05  WS-OTH-GEN-INFO-DTL     OCCURS 15.                           
                   10  WS-OTH-GEN-CERTINFO    PIC X(96).                        
                   10  WS-OTH-GEN-EFF-DATE    PIC X(8).                         
                   10  WS-OTH-GEN-TERM-REASON PIC XX.                           
                   10  WS-OTH-GEN-RTN-DATE    PIC X(8).                         
                   10  WS-OTH-GEN-SAL-AMT     PIC 9(11)V99.                     
                   10  WS-OTH-GEN-SAL-FREQ    PIC X.                            
                   10  WS-OTH-GEN-OCC         PIC X(29).                        
                   10  WS-OTH-GEN-CLASS-NEW   PIC X(3).                         
                   10  WS-OTH-GEN-CLASS-OLD   PIC X(3).                         
                   10  WS-OTH-GEN-ACCT-CHG    PIC X(3).                         
                   10  WS-OTH-GEN-DIV-NEW     PIC X(3).                         
                   10  WS-OTH-GEN-DIV-OLD     PIC X(3).                         
                   10  WS-OTH-GEN-EVIDENCE    PIC X.                            
                   10  WS-OTH-GEN-MAILED      PIC X.                            
                   10  WS-OTH-GEN-WORK-HRS    PIC 9(11)V99.                     
               05  WS-OTH-GEN-INFO-CRTAUTH PIC X(9).                            
                                                                                
       01  GAEDATSR-PARMS.                 COPY GARDSVRB.                       
                                                                                
       01  GACCDMA7-PARMS.                                                      
           03 GACCDMA7-PARM1.                                                   
              05 GACCDMA7-FUNC-CODE   PIC S9(4) COMP.                           
              05 GACCDMA7-GRP-INFO.                                             
                 07 GACCDMA7-GROUP    PIC S9(7) VALUE ZERO COMP-3.              
                 07 GACCDMA7-ACCT     PIC XXX   VALUE LOW-VALUES.               
                 07 FILLER            PIC S9(9) VALUE ZERO COMP-3.              
              05 GACCDMA7-FILL        PIC X(9)  VALUE LOW-VALUES.               
           03 GACCDMA7-ERR            PIC X(3)  VALUE SPACES.                   
       01  FILLER VALUE 'GIPSY EXPANDED MASTER   ' PIC X(24).                   
      * GMASTER                                                                 
           COPY GMASTER7.                                                       
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INIT               THRU 1000-EXIT.                      
                                                                                
           PERFORM 2000-PROCESS            THRU 2000-EXIT                       
                   UNTIL WS-EOF.                                                
                                                                                
           PERFORM 9000-FINISH             THRU 9000-EXIT.                      
                                                                                
           GOBACK.                                                              
                                                                                
       0000-EXIT. EXIT.                                                         
                                                                                
       1000-INIT.                                                               
                                                                                
           MOVE 'GCCPFRMX'                 TO ICBM-PROGRAM-NAME.                
           MOVE LOW-VALUES                 TO LINKAGE-CONTROL.                  
                                                                                
           INITIALIZE INOUT-RECORD.                                             
           MOVE OBTAIN-FIRST               TO WS-GAEDATSR-VERB.                 
           PERFORM 8100-READ-INPUT         THRU 8100-EXIT.                      
                                                                                
       1000-EXIT. EXIT.                                                         
                                                                                
       2000-PROCESS.                                                            
                                                                                
      *****                                                                     
      *****IF IT IS A GLMASS RECORD, READ THE DETAIL DATA/INDEXES.              
      *****   TERMINATION INDEXES ARE FIRST CHECKED TO MAKE SURE THEY           
      *****   ARE GIPSY POLICIES. IF THEY ARE, THEY ARE COPIED TO A NEW         
      *****   'TERM ONLY' RECORD.                                               
      *****   NON-TERMINATION INDEXES OR INDEXES THAT ARE FOR                   
      *****   MANUCONNECT POLICIES ARE COPIED TO A NEW 'NON TERM'               
      *****     RECORD.                                                         
      *****   'TERM ONLY' RECORD IS WRITTEN TO THE 'TERM' FILE.                 
      *****   'NON TERM' RECORD IS WRITTEN TO THE 'OTHER' FILE.                 
      *****                                                                     
      *****IF IT IS NOT A GLMASS RECORD                                         
      *****   CANNOT CONTAIN TERMINATIONS                                       
      *****   COPY TO THE 'OTHER' FILE.                                         
      *****                                                                     
           MOVE 'N'                        TO WS-TERM-MKR.                      
           MOVE 'N'                        TO WS-OTHER-MKR.                     
                                                                                
           IF GCCCFEXT-FORM-NBR            = 'GLMASS  '                         
              MOVE GCCCFEXT-TIMESTAMP      TO WS-GEN-INFO-TMSTP                 
                                                                                
              MOVE GCCCFEXT-FORM-DATA      TO WS-TEMP-GCCC-DETAIL               
      *****   INITIALIZE THE CURRENT, TERM AND NON-TERM INDEX COUNTS            
              MOVE 1                       TO ICP                               
              MOVE 1                       TO ITM                               
              MOVE 1                       TO IOT                               
      *****   READ ALL THE INDEXES ON THIS RECORD                               
              PERFORM 2100-READ-INDEXES    THRU 2100-EXIT                       
                      UNTIL ICP > 15                                            
      *****IF THERE ARE ANY TERMINATION INDEXES,                                
      *****   WRITE THE TERM RECORD TO THE TERM OUTPUT FILE                     
              IF WS-TERM                                                        
      **         MOVE GCCCFEXT-TIMESTAMP   TO WS-TERM-GEN-INFO-TMSTP            
                 MOVE WS-GEN-INFO-TMSTP    TO WS-TERM-GEN-INFO-TMSTP            
                 MOVE GCCCMASS-CLIENT-NAME TO WS-TERM-GEN-INFO-CLNM             
                 MOVE GCCCMASS-CERT-AUTH   TO WS-TERM-GEN-INFO-CRTAUTH          
                 MOVE WS-TERM-GEN-INFO     TO GCCCFEXT-FORM-DATA                
                 DISPLAY 'TERM INFO:' WS-TERM-GEN-INFO                          
                 PERFORM 8020-WRITE-TERM-OUTPUT THRU 8020-EXIT                  
              END-IF                                                            
      *****IF THERE ARE ANY NON-TERM INDEXES,                                   
      *****   WRITE THE OTHER RECORD TO THE OTHER OUTPUT FILE                   
              IF WS-OTHER                                                       
      **         MOVE GCCCFEXT-TIMESTAMP   TO WS-OTH-GEN-INFO-TMSTP             
                 MOVE WS-GEN-INFO-TMSTP    TO WS-OTH-GEN-INFO-TMSTP             
                 MOVE GCCCMASS-CLIENT-NAME TO WS-OTH-GEN-INFO-CLNM              
                 MOVE GCCCMASS-CERT-AUTH   TO WS-OTH-GEN-INFO-CRTAUTH           
                 MOVE WS-OTH-GEN-INFO      TO GCCCFEXT-FORM-DATA                
                 DISPLAY 'OTH INFO:' WS-OTH-GEN-INFO                            
                 PERFORM 8025-WRITE-OTHER-OUTPUT THRU 8025-EXIT                 
              END-IF                                                            
           ELSE                                                                 
      ***** WRITE TO THE OTHER OUTPUT FILE                                      
              PERFORM 8025-WRITE-OTHER-OUTPUT THRU 8025-EXIT                    
           END-IF.                                                              
                                                                                
           MOVE OBTAIN-NEXT                TO WS-GAEDATSR-VERB.                 
           PERFORM 8100-READ-INPUT         THRU 8100-EXIT.                      
                                                                                
       2000-EXIT. EXIT.                                                         
                                                                                
       2100-READ-INDEXES.                                                       
                                                                                
      ***** READ THRU ALL THE INDEXES.                                          
      ***** IF TERMINATION FOR A GIPSY POLICY, WRITE THE INDEX                  
      ***** TO A TERM RECORD.                                                   
      ***** NON-TERM OR NON-GIPSY POLICY INDEXES ARE WRITTEN                    
      ***** TO A NON-TERM RECORD.                                               
      *****                                                                     
      ***** TERM INDEXES CAN CONTAIN NON-TERM CHANGE INFO -                     
      ***** IN THAT CASE INDEXES ARE WRITTEN TO BOTH TERM AND NON-TERM          
      ***** RECORDS (WITH THE CORRECT TERM/NON-TERM INFORMATION IN              
      ***** EACH RECORD'S INDEX).                                               
      *****   CAN COMBINE TERMINATIONS WITH:                                    
      *****       CLASS CHANGE                                                  
      *****       ACCOUNT CHANGE                                                
      *****       BILLING DIVISION CHANGE                                       
      *****       OCCUPATION CHANGE                                             
      *****                                                                     
      ***** THIS SERVES THE PURPOSE OF SEPARATING THE GIPSY                     
      ***** TERMINATIONS FROM OTHER MASS TRANSACTIONS.                          
                                                                                
                                                                                
           MOVE 'N'                        TO WS-GIPSY-MKR.                     
                                                                                
           IF ICP                          >  15                                
           OR GCCCMASS-PLAN(ICP)           = SPACES OR LOW-VALUES               
      ***** IF IT IS THE LAST INDEX OR IS AN EMPTY INDEX                        
      ***** THEN MAKE EMPTY TERM AND NON-TERM INDEXES                           
              INITIALIZE WS-TERM-GEN-INFO-DTL(ITM)                              
              ADD 1                        TO ITM                               
              INITIALIZE WS-OTH-GEN-INFO-DTL(IOT)                               
              ADD 1                        TO IOT                               
           ELSE                                                                 
      ***** IF IT CONTAINS TERMINATION INFORMATION (TERMINATION REASON)         
              IF GCCCMASS-TERM-REASON(ICP) = 'T' OR 'R' OR 'M'                  
                                           OR 'LA' OR 'LB' OR 'LC'              
                                           OR 'LD' OR 'LE' OR 'C'               
                                           OR 'L' OR 'MM' OR 'MC'               
                                           OR 'MI' OR 'CA'                      
      ***** READ GIPSY MASTER TO SEE IF THIS IS A GIPSY POLICY                  
                 MOVE GCCCMASS-PLAN(ICP)        TO GACCDMA7-GROUP               
      ***** CONVERT LOWER-CASE ALPHA TO UPPER-CASE ALPHA                        
                 MOVE FUNCTION UPPER-CASE(GCCCMASS-ACCOUNT-NBR(ICP))            
                                                TO GACCDMA7-ACCT                
                 MOVE LOW-VALUES                TO GACCDMA7-FILL                
                 PERFORM 8000-READ-GIPSY-MASTER THRU 8000-EXIT                  
                                                                                
                 IF WS-GIPSY                                                    
                                                                                
      ***** IF THERE IS *ONLY* TERMINATION INFORMATION                          
                    IF (GCCCMASS-CLASS-NEW(ICP) = SPACES OR LOW-VALUES)         
                    AND (GCCCMASS-DIV-NEW(ICP) = SPACES OR LOW-VALUES)          
                    AND (GCCCMASS-ACCT-CHG(ICP) = SPACES OR LOW-VALUES)         
                    AND (GCCCMASS-OCC(ICP) = SPACES OR LOW-VALUES)              
      ***** WRITE JUST A TERM INDEX                                             
                        MOVE GCCCMASS-GEN-INFO(ICP)                             
                             TO WS-TERM-GEN-INFO-DTL(ITM)                       
                        MOVE 'Y'           TO WS-TERM-MKR                       
                        ADD 1              TO ITM                               
                    ELSE                                                        
                                                                                
      ***** IF THERE IS NON-TERMINATION INFO, NEED TO SPLIT THE                 
      ***** INFORMATION INTO A TERM INDEX (WITH TERM INFO) AND                  
      ***** A NON-TERM INDEX (WITH NON-TERM INFO).                              
                                                                                
      ***** CLEAR OUT NON-TERM INFO AND WRITE TERM INDEX                        
                        MOVE GCCCMASS-GEN-INFO(ICP)                             
                             TO WS-TERM-GEN-INFO-DTL(ITM)                       
                        MOVE SPACES TO WS-TERM-GEN-CLASS-NEW(ITM)               
                                       WS-TERM-GEN-CLASS-OLD(ITM)               
                                       WS-TERM-GEN-ACCT-CHG(ITM)                
                                       WS-TERM-GEN-DIV-NEW(ITM)                 
                                       WS-TERM-GEN-DIV-OLD(ITM)                 
                                       WS-TERM-GEN-OCC(ITM)                     
                        MOVE ZEROES TO WS-TERM-GEN-WORK-HRS(ITM)                
                        MOVE 'Y'           TO WS-TERM-MKR                       
                        ADD 1              TO ITM                               
      ***** WRITE A NON-TERM INDEX ALSO                                         
                        MOVE GCCCMASS-GEN-INFO(ICP)                             
                             TO WS-OTH-GEN-INFO-DTL(IOT)                        
                        MOVE SPACES TO WS-OTH-GEN-TERM-REASON(IOT)              
                        MOVE ZEROES TO WS-OTH-GEN-RTN-DATE(IOT)                 
                        MOVE 'Y'           TO WS-OTHER-MKR                      
                        ADD 1              TO IOT                               
                    END-IF                                                      
                 ELSE                                                           
      ***** IF NOT A GIPSY POLICY CREATE AN OTHER INDEX                         
      ***** (THIS IS MOST LIKELY A MANUCONNECT POLICY THAT NEEDS                
      ***** MANUAL PROCESSING FOR THIS TRANSACTION)                             
                    MOVE GCCCMASS-GEN-INFO(ICP)                                 
                         TO WS-OTH-GEN-INFO-DTL(IOT)                            
                    MOVE 'Y'           TO WS-OTHER-MKR                          
                    ADD 1              TO IOT                                   
                 END-IF                                                         
              ELSE                                                              
      ***** NOT A TERMINATION - CREATE AN OTHER INDEX                           
                 MOVE GCCCMASS-GEN-INFO(ICP)                                    
                      TO WS-OTH-GEN-INFO-DTL(IOT)                               
                 MOVE 'Y'           TO WS-OTHER-MKR                             
                 ADD 1              TO IOT                                      
              END-IF                                                            
           END-IF.                                                              
           ADD 1                    TO ICP.                                     
                                                                                
       2100-EXIT. EXIT.                                                         
                                                                                
       8000-READ-GIPSY-MASTER.                                                  
                                                                                
      ***** DIRECT READ THE GIPSY MASTER                                        
                                                                                
           MOVE SPACES TO GACCDMA7-ERR.                                         
           MOVE +2     TO GACCDMA7-FUNC-CODE.                                   
           CALL WS-GACCDMA7 USING GACCDMA7-PARM1                                
                                  GACCDMA7-ERR                                  
                                  GMASTER7.                                     
                                                                                
      ***** IF FOUND, SET WS-GIPSY TO Y                                         
           IF GACCDMA7-ERR EQUAL SPACES                                         
              MOVE 'Y'             TO WS-GIPSY-MKR                              
              DISPLAY 'GIPSY POL:' GACCDMA7-GROUP                               
              DISPLAY 'ACCOUNT:' GACCDMA7-ACCT                                  
           ELSE                                                                 
      ***** IF NOT FOUND, SET WS-GIPSY TO N                                     
              IF GACCDMA7-ERR EQUAL 'R01'                                       
                 MOVE 'N'          TO WS-GIPSY-MKR                              
                 DISPLAY 'MC POL:' GCCCMASS-PLAN(ICP)                           
                 DISPLAY 'ACCOUNT:' GCCCMASS-ACCOUNT-NBR(ICP)                   
              ELSE                                                              
      ***** IF OTHER ERROR, ABEND                                               
                 DISPLAY 'ERROR READING GIPSY MASTER'                           
                 DISPLAY 'STATUS : ' PROGRAM-LINKAGE-STATUS                     
                 DISPLAY 'RECORD : ' INOUT-RECORD                               
                 PERFORM 9999-ABEND THRU 9999-EXIT                              
              END-IF                                                            
           END-IF.                                                              
                                                                                
       8000-EXIT. EXIT.                                                         
                                                                                
       8100-READ-INPUT.                                                         
                                                                                
           MOVE WS-SORT-IN-LR              TO LOGICAL-RECORD-NAME.              
           INITIALIZE INOUT-RECORD.                                             
                                                                                
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           INOUT-RECORD                         
                                           ICBM.                                
                                                                                
           IF LR-NOT-FOUND                                                      
              MOVE 'Y'                     TO WS-EOF-MKR                        
              DISPLAY 'HIT EOF OF THE INPUT FILE SUCCESSFULLY.'                 
           ELSE IF NOT LR-STATUS-OK                                             
              MOVE 'Y'                     TO WS-EOF-MKR                        
              DISPLAY 'ERROR READING &&SRTEXT'                                  
              DISPLAY 'STATUS : ' PROGRAM-LINKAGE-STATUS                        
              PERFORM 9999-ABEND THRU 9999-EXIT                                 
           END-IF.                                                              
                                                                                
       8100-EXIT. EXIT.                                                         
                                                                                
       8020-WRITE-TERM-OUTPUT.                                                  
                                                                                
           MOVE WS-SORTTM-OUT-LR           TO LOGICAL-RECORD-NAME.              
           MOVE STORE-LR                   TO WS-GAEDATSR-VERB.                 
                                                                                
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           INOUT-RECORD                         
                                           ICBM.                                
                                                                                
           IF NOT LR-STATUS-OK                                                  
              MOVE  'Y'                    TO WS-EOF-MKR                        
              DISPLAY 'ERROR WRITING TO TERM OUT FILE'                          
              DISPLAY 'STATUS : ' PROGRAM-LINKAGE-STATUS                        
              DISPLAY 'RECORD : ' INOUT-RECORD                                  
              PERFORM 9999-ABEND THRU 9999-EXIT.                                
                                                                                
      ***  INITIALIZE INOUT-RECORD.                                             
           INITIALIZE GCCCFEXT-FORM-DATA.                                       
                                                                                
       8020-EXIT. EXIT.                                                         
                                                                                
       8025-WRITE-OTHER-OUTPUT.                                                 
                                                                                
           MOVE WS-SORTOT-OUT-LR           TO LOGICAL-RECORD-NAME.              
           MOVE STORE-LR                   TO WS-GAEDATSR-VERB.                 
                                                                                
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           INOUT-RECORD                         
                                           ICBM.                                
                                                                                
           IF NOT LR-STATUS-OK                                                  
              MOVE  'Y'                    TO WS-EOF-MKR                        
              DISPLAY 'ERROR WRITING TO NON-TERM OUT FILE'                      
              DISPLAY 'STATUS : ' PROGRAM-LINKAGE-STATUS                        
              DISPLAY 'RECORD : ' INOUT-RECORD                                  
              PERFORM 9999-ABEND THRU 9999-EXIT.                                
                                                                                
      ***  INITIALIZE INOUT-RECORD.                                             
           INITIALIZE GCCCFEXT-FORM-DATA.                                       
                                                                                
       8025-EXIT. EXIT.                                                         
                                                                                
       9000-FINISH.                                                             
                                                                                
      *****CLOSING FILES - SHOULD NOT PERFORM 9999-ABEND PARAGRAPH FROM         
      *****                HERE TO PREVENT AN INFINITE LOOP                     
                                                                                
           MOVE WS-SORT-IN-LR              TO LOGICAL-RECORD-NAME.              
           MOVE FINISH-LR                  TO WS-GAEDATSR-VERB.                 
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           LOGICAL-RECORD-NAME                  
                                           ICBM.                                
           IF LR-STATUS-OK                                                      
              NEXT SENTENCE                                                     
           ELSE                                                                 
              DISPLAY 'ERROR CLOSING INPUT FILE'                                
              DISPLAY PROGRAM-LINKAGE-STATUS                                    
           END-IF.                                                              
                                                                                
           MOVE WS-SORTTM-OUT-LR           TO LOGICAL-RECORD-NAME.              
           MOVE FINISH-LR                  TO WS-GAEDATSR-VERB.                 
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           LOGICAL-RECORD-NAME                  
                                           ICBM.                                
           IF LR-STATUS-OK                                                      
              NEXT SENTENCE                                                     
           ELSE                                                                 
              DISPLAY 'ERROR CLOSING TERM OUTPUT FILE'                          
              DISPLAY PROGRAM-LINKAGE-STATUS                                    
           END-IF.                                                              
                                                                                
           MOVE WS-SORTOT-OUT-LR           TO LOGICAL-RECORD-NAME.              
           MOVE FINISH-LR                  TO WS-GAEDATSR-VERB.                 
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           LOGICAL-RECORD-NAME                  
                                           ICBM.                                
           IF LR-STATUS-OK                                                      
              NEXT SENTENCE                                                     
           ELSE                                                                 
              DISPLAY 'ERROR CLOSING OTHER OUTPUT FILE'                         
              DISPLAY PROGRAM-LINKAGE-STATUS                                    
           END-IF.                                                              
                                                                                
           MOVE +1                         TO GACCDMA7-FUNC-CODE.               
           CALL WS-GACCDMA7          USING GACCDMA7-PARM1                       
                                           GACCDMA7-ERR                         
                                           GMASTER7.                            
           IF GACCDMA7-ERR NOT EQUAL SPACES                                     
              DISPLAY 'ERROR CLOSING GIPSY MASTER'                              
              DISPLAY 'STATUS : ' PROGRAM-LINKAGE-STATUS                        
           END-IF.                                                              
                                                                                
       9000-EXIT. EXIT.                                                         
                                                                                
       9999-ABEND.                                                              
                                                                        12370000
           PERFORM 9000-FINISH THRU 9000-EXIT.                                  
                                                                                
           MOVE +16 TO RETURN-CODE.                                     12380000
                                                                                
           GOBACK.                                                              
                                                                                
       9999-EXIT. EXIT.                                                         
