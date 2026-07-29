       CBL DATA(24)                                                             
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRMT.                                                 
      *AUTHOR.        SANGKYUN PARK                                             
      ******************************************************************        
      * PROGRAM DESCRIPTION:                                                    
      *     THIS PROGRAM READS &&SRTEXT FILE AND LEFT JUSTFY IT                 
      *   BY REMOVING ALL SALARY CHANGE INDEXES THEN WRITE &&SRTEXTNS.          
      *   THE NEW FILE WILL BE AN INPUT FILE OF ANOTHER BATCH PROGRAM           
      *   (GCCPFRMF). ONE(&&MASEXTNS) OF THE OUTPUT FILES WILL BE A             
      *   SOURCE FILE OF A RDS(PGBPR002) REPORT. THE REASON DOING THIS          
      *   IS FOR REMOVING SALARY CHANGE REPORT FROM THE RDS REPORT.             
      *                                                                         
      * CALLING MODULES                                                         
      *     NOT APPLICABLE.                                                     
      *                                                                         
      * CALLED MODULES                                                          
      *     GAEDATSR - DATA SERVER                                              
      *                                                                         
      * COPYBOOKS                                                               
      *     GARDSVRB - VERBS FOR GAEDATSR                                       
      *     GCCCFEXT - REPORT EXTRACT LAYOUT DETAIL                             
      *     GCCCMASS - INPUT FORM LAYOUT                                        
      *                                                                         
      * INPUT  - SORTED FLAT FILE OF PRINT EXTRACT                              
      *                                                                         
      * OUTPUT - PRINT SORTED EXTRACT FILE                                      
      *          EXCEPT SALARY CHANGE INDEXES                                   
      *                                                                         
      ******************************************************************        
      * DATE       NAME      DESCRIPTION                                        
      * ---------  --------  -------------------------------------------        
      * XXDEC2016  PARSANG   CREATED                                            
      * 03MAY2018  YEATEAN   GBE331: FIX WHEN NON-SALARY CHG INFO               
      *                      IN THE SAME INDEX WITH SALARY-CHG                  
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
                                                                                
       01  WS-MARKERS.                                                          
           05  WS-EOF-MKR                  PIC X     VALUE 'N'.                 
               88  WS-EOF                            VALUE 'Y'.                 
           05  WS-SKIP-MKR                 PIC X     VALUE 'N'.                 
               88  WS-SKIP                           VALUE 'Y'.                 
                                                                                
       01  OCCURS-INDEXES.                                                      
      *****INDEX OF CURRENT POSITION                                            
           05  ICP                         PIC 9(2)  VALUE ZERO.                
      *****INDEX FOR A NEW NON-SALARY ITEM                                      
           05  INS                         PIC 9(2)  VALUE ZERO.                
                                                                                
       01  WS-VARIABLES.                                                        
           05  WS-SORT-IN-LR               PIC X(16) VALUE                      
                                               'CARD-DATA-070   '.              
           05  WS-SORTNS-OUT-LR            PIC X(16) VALUE                      
                                               'PRINT-DATA-071  '.              
                                                                                
      *****THIS RECORD WILL BE USED AS BOTH INPUT AND OUTPUT                    
       01  INO-RECORD.                                                          
           03 GCCCFEXT-RECL                PIC S9(4)  COMP.                     
           03 FILLER                       PIC XX     VALUE  SPACES.            
           03 GCCCFEXT-DETAIL.                                                  
              COPY GCCCFEXT.                                                    
                                                                                
       01  WS-TEMP-RECORD.                                                      
                                                                                
      *****NOTE THIS EXTRA CHARACTER WAS ADDED TO HIGHLIGHT                     
      *****THE FACT THAT THE HEADERS ARE DIFFERENT LENGTHS                      
      *****ON THE EXTRACT AND THE DETAIL.                                       
                                                                                
           03  WS-TEMP-EXTRA-HEAD-CHAR     PIC X.                               
           03  WS-TEMP-GCCC-HEAD-AND-DETL.                                      
               05  WS-GCCC-HEADER          PIC X(61).                           
               05  WS-GCCC-DETAIL          PIC X(3939).                         
                                                                                
           03 FILLER             REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.          
              COPY GCCCMASS.                                                    
                                                                                
       01  GAEDATSR-PARMS.                 COPY GARDSVRB.                       
                                                                                
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
                                                                                
           MOVE 'GCCPFRMT'                 TO ICBM-PROGRAM-NAME.                
           MOVE LOW-VALUES                 TO LINKAGE-CONTROL.                  
                                                                                
           INITIALIZE INO-RECORD.                                               
           MOVE OBTAIN-FIRST               TO WS-GAEDATSR-VERB.                 
           PERFORM 8100-READ-INPUT         THRU 8100-EXIT.                      
                                                                                
       1000-EXIT. EXIT.                                                         
                                                                                
       2000-PROCESS.                                                            
                                                                                
      *****IF A RECORD IS FOR MASS CHANGE REPORT, READ THE DETAIL DATA.         
      *****AS GCCCMASS LAYOUT MAY HAVE MULTIPLE INDEXES, ALL OF THEM            
      *****ARE INSPECTED. ANY SALARY CHANGE INDEXES ARE REMOVED WHILE           
      *****ALL TERMINATION INDEXES ARE KEPT. IF ALL INDEXES ARE SALARY          
      *****CHANGES, THE ENTIRE RECORD IS REMOVED.                               
      *****THIS PROCESS LOOKS LIKE "LEFT JUSTIFY".                              
      *****IF A RECORD IS NOT FOR MASS CHANGE REPORT, IT IS JUST COPIED         
      *****WITHOUT ANY MODIFICATION                                             
                                                                                
           MOVE 'N'                        TO WS-SKIP-MKR.                      
                                                                                
           IF GCCCFEXT-FORM-NBR            = 'GLMASS  '                         
              MOVE GCCCFEXT-FORM-DATA      TO WS-GCCC-DETAIL                    
              MOVE 1                       TO ICP                               
              MOVE 1                       TO INS                               
              PERFORM 2100-LEFT-JUSTIFY    THRU 2100-EXIT                       
                      UNTIL INS            > 15                                 
              IF GCCCMASS-PLAN(1)          = SPACES                             
                 MOVE 'Y'                  TO WS-SKIP-MKR                       
              ELSE                                                              
                 MOVE WS-GCCC-DETAIL       TO GCCCFEXT-FORM-DATA                
              END-IF                                                            
           END-IF.                                                              
                                                                                
           IF NOT WS-SKIP                                                       
              PERFORM 8020-PRINT-OUTPUT    THRU 8020-EXIT.                      
                                                                                
           MOVE OBTAIN-NEXT                TO WS-GAEDATSR-VERB.                 
           PERFORM 8100-READ-INPUT         THRU 8100-EXIT.                      
                                                                                
       2000-EXIT. EXIT.                                                         
                                                                                
       2100-LEFT-JUSTIFY.                                                       
      ***** READ THRU ALL THE INDEXES AND DECIDE IF THAT ONE                    
      ***** SHOULD BE WRITTEN TO THE NON-SALARY CHANGE FILE.                    
                                                                                
           IF ICP                          >  15                                
           OR GCCCMASS-PLAN(ICP)           = SPACES OR LOW-VALUES               
      ********IF THIS IS THE LAST INDEX, OR IS AN EMPTY INDEX                   
      ********WRITE AN EMPTY NON-SAL CHG INDEX                                  
              INITIALIZE GCCCMASS-GEN-INFO(INS)                                 
              ADD 1                        TO INS                               
           ELSE IF (GCCCMASS-SALARY-AMT(ICP) = ZERO)                            
                AND (GCCCMASS-SALARY-FREQ(ICP) = SPACE OR LOW-VALUES)           
                AND (GCCCMASS-WORKING-HOURS(ICP) = ZERO)                        
      ********IT IS IDENTIFIED AS A NON SALARY-CHANGE BY                        
      ********SALARY INFORMATION CONTEXT, WRITE TO THE                          
      ********NON-SALARY CHANGE FILE.                                           
              MOVE GCCCMASS-GEN-INFO(ICP)  TO GCCCMASS-GEN-INFO(INS)            
              ADD 1                        TO ICP                               
              ADD 1                        TO INS                               
                                                                                
      ********SOME SALARY CHANGES CAN BE COMBINED WITH OTHER CHANGES            
      ********   TERMINATIONS                                                   
      ********   CLASS, OCCUPATION, ACCOUNT, BILLING DIVISION CHGS              
                                                                                
           ELSE IF (GCCCMASS-CLASS-NEW(ICP) = SPACES OR LOW-VALUES)             
                AND (GCCCMASS-ACCT-CHG(ICP) = SPACES OR LOW-VALUES)             
                AND (GCCCMASS-DIV-NEW(ICP) = SPACES OR LOW-VALUES)              
                AND (GCCCMASS-OCC(ICP) = SPACES OR LOW-VALUES)                  
             AND (GCCCMASS-TERM-REASON(ICP) = SPACES OR LOW-VALUES)             
      ********THIS IS TRULY JUST A SALARY CHANGE - DON'T WRITE TO THE           
      ********NON-SALARY CHANGE FILE                                            
              ADD 1                        TO ICP                               
                                                                                
           ELSE                                                                 
      ********IT IS A SALARY CHANGE ITEM AND THERE IS ALSO ANOTHER              
      ********CHANGE IN THAT INDEX.                                             
      ********CLEAR OUT ALL SALARY CHANGE INFORMATION AND WRITE ALL             
      ********THE NON-SALARY CHANGE INFO TO THE NON-SALARY CHANGE FILE          
              MOVE GCCCMASS-GEN-INFO(ICP) TO GCCCMASS-GEN-INFO(INS)             
              INITIALIZE                  GCCCMASS-SALARY-AMT(INS)              
                                          GCCCMASS-SALARY-FREQ(INS)             
                                          GCCCMASS-WORKING-HOURS(INS)           
              ADD 1                    TO ICP                                   
              ADD 1                    TO INS                                   
           END-IF.                                                              
                                                                                
       2100-EXIT. EXIT.                                                         
                                                                                
       8100-READ-INPUT.                                                         
                                                                                
           MOVE WS-SORT-IN-LR              TO LOGICAL-RECORD-NAME.              
           INITIALIZE INO-RECORD.                                               
                                                                                
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           INO-RECORD                           
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
                                                                                
       8020-PRINT-OUTPUT.                                                       
                                                                                
           MOVE WS-SORTNS-OUT-LR           TO LOGICAL-RECORD-NAME.              
           MOVE STORE-LR                   TO WS-GAEDATSR-VERB.                 
                                                                                
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           INO-RECORD                           
                                           ICBM.                                
                                                                                
           IF NOT LR-STATUS-OK                                                  
              MOVE  'Y'                    TO WS-EOF-MKR                        
              DISPLAY 'ERROR WRITING TO &&SRTEXTNS'                             
              DISPLAY 'STATUS : ' PROGRAM-LINKAGE-STATUS                        
              DISPLAY 'RECORD : ' INO-RECORD                                    
              PERFORM 9999-ABEND THRU 9999-EXIT.                                
                                                                                
           INITIALIZE INO-RECORD.                                               
                                                                                
       8020-EXIT. EXIT.                                                         
                                                                                
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
              DISPLAY PROGRAM-LINKAGE-STATUS.                                   
                                                                                
           MOVE WS-SORTNS-OUT-LR           TO LOGICAL-RECORD-NAME.              
           MOVE FINISH-LR                  TO WS-GAEDATSR-VERB.                 
                                                                                
           CALL WS-GAEDATSR          USING WS-GAEDATSR-VERB                     
                                           LOGICAL-RECORD-NAME                  
                                           ICBM.                                
                                                                                
           IF LR-STATUS-OK                                                      
              NEXT SENTENCE                                                     
           ELSE                                                                 
              DISPLAY 'ERROR CLOSING OUTPUT FILE'                               
              DISPLAY PROGRAM-LINKAGE-STATUS.                                   
                                                                                
       9000-EXIT. EXIT.                                                         
                                                                                
       9999-ABEND.                                                              
                                                                        12370000
           PERFORM 9000-FINISH THRU 9000-EXIT.                                  
                                                                                
           MOVE +16 TO RETURN-CODE.                                     12380000
                                                                                
           GOBACK.                                                              
                                                                                
       9999-EXIT. EXIT.                                                         
