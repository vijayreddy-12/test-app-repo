       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRMR.                                                 
      *AUTHOR.        KLYN                                                      
      ******************************************************************        
      *   GROUP BENEFITS E-BUSINESS AFP FORM, PRINT                             
      *   DISTRIBUTION REPORT                                                   
      *                                                                         
      * PROGRAM DESCRIPTION:                                                    
      *     THIS PROGRAM READS A FLAT FILE CREATED BY GCCPFRMF THAT             
      *   CONTAINS REFORMATTED RECORDS. BY DISTRIBUTION CATEGORY, GROUP,        
      *   DIVISION AND FORM NAME. THE DETAIL INCLUDES MEMBER NAME,              
      *   CERTIFICATE NUMBER, CONFIRMATION NUMBER AND SEQUENCE NUMBER           
      *                                                                         
      * CALLING MODULES                                                         
      *     NOT APPLICABLE.                                                     
      *                                                                         
      * CALLED MODULES                                                          
      *     GAEDATSR - DATA SERVER                                              
      *                                                                         
      * COPYBOOKS                                                               
      *     GARDSVRB - VERBS FOR GAEDATSR                                       
      *     GCCCRPT  - REPORT EXTRACT LAYOUT                                    
      *                                                                         
      * INPUT  - FLAT FILE OF REPORT EXTRACT                                    
      *        - DATE CONTROL CARD                                              
      *                                                                         
      * OUTPUT - PRINT DISTRIBUTION REPORT                                      
      *                                                                         
      ******************************************************************        
      * DATE       NAME      DESCRIPTION                                        
      * ---------  --------  -------------------------------------------        
      * 01NOV2000  KLYN      CREATION.                                          
      *                                                                         
      * 30JAN2001  KLYN      CHANGES V.3.1                                      
      *                                                                         
      * 26JUN2001  J ELKINS  ADD PRINTING OF TOTAL RECORDS CREATED              
      *                      IN GCCPFRMF FOR THE BANNER TRAILER PAGES           
      *                                                                         
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
           05  FILLER                      PIC X(32) VALUE                      
               '*** GCCPFRMR WORKING STORAGE ***'.                              
                                                                                
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GAEDATSR-VERB            PIC X(16).                           
           05  WS-GAEDATSR                 PIC X(8)  VALUE 'GAEDATSR'.          
           05  WS-GC2DATE                  PIC X(07) VALUE 'GC2DATE'.           
       01  WS-COUNTS.                                                           
           05  WS-LINE-COUNT               PIC S9(6) COMP-3 VALUE ZERO.         
           05  WS-PAGE-COUNT               PIC S9(6) COMP-3 VALUE ZERO.         
           05  WS-FORMS-COUNT-GROUP        PIC S9(6) COMP-3 VALUE ZERO.         
           05  WS-FORMS-COUNT-CAT          PIC S9(6) COMP-3 VALUE ZERO.         
                                                                                
       01  WS-SAVED-COMPARISONS.                                                
           05  WS-SAVE-CATEGORY            PIC X(30) VALUE SPACES.              
           05  WS-SAVE-GROUP               PIC X(7)  VALUE SPACES.              
           05  WS-SAVE-DIVISION            PIC X(3)  VALUE SPACES.              
           05  WS-SAVE-FORM                PIC X(8)  VALUE SPACES.              
                                                                                
       01  WS-MARKERS.                                                          
           05  WS-CATEGORY-MKR             PIC X     VALUE 'N'.                 
               88  WS-CATEGORY-CHANGE                VALUE 'Y'.                 
           05  WS-GROUP-MKR                PIC X     VALUE 'N'.                 
               88  WS-GROUP-CHANGE                   VALUE 'Y'.                 
           05  WS-DIVISION-MKR             PIC X     VALUE 'N'.                 
               88  WS-DIVISION-CHANGE                VALUE 'Y'.                 
           05  WS-FORM-MKR                 PIC X     VALUE 'N'.                 
               88  WS-FORM-CHANGE                    VALUE 'Y'.                 
           05  WS-EOF-MKR                  PIC X     VALUE 'N'.                 
               88  WS-EOF                            VALUE 'Y'.                 
           05  WS-TOTAL-MKR                PIC X     VALUE 'D'.                 
               88  WS-TOTAL-RECORD                   VALUE 'T'.                 
               88  WS-DETAIL-RECORD                  VALUE 'D'.                 
                                                                                
       01  WS-VARIABLES.                                                        
           05  WS-MONTH-NAMES    VALUE                                          
               'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'.                          
               10  WS-MONTH-NAME           PIC X(3)   OCCURS 12.                
           05  WS-TEMP-GROUP               PIC X(7)  VALUE SPACES.              
           05  WS-OBTAIN-FIRST             PIC X(16) VALUE                      
                                               'OBTAIN  FIRST   '.              
           05  WS-OBTAIN-NEXT              PIC X(16) VALUE                      
                                               'OBTAIN  NEXT    '.              
           05  WS-STORE-LR                 PIC X(16) VALUE                      
                                               'STORE           '.              
           05  WS-INPUT-LR                 PIC X(16) VALUE                      
                                               'CARD-DATA-010   '.              
      *    05  WS-CARD-LR                  PIC X(16) VALUE                      
      *                                        'CARD-DATA-011   '.              
           05  WS-PRINT-LR                 PIC X(16) VALUE                      
                                               'PRINT-DATA-022  '.              
           05  WS-DELIMITER                PIC X     VALUE '%'.                 
                                                                                
                                                                                
       01  INP-RECORD.                                                          
           COPY GCCCRPT.                                                        
                                                                                
       01  PRINT-RECORD.                                                        
               05  PRINT-CTL               PIC X(1).                            
               05  PRINT-DATA              PIC X(132).                          
                                                                                
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                          
                                                                                
       01  GAC-DATE-PARAMETERS.         COPY GARDATEP.                          
                                                                                
       01  WS-SPLIT-DATE.                                                       
           05  WS-SPLIT-YYYY               PIC 9999.                            
           05  WS-SPLIT-MM                 PIC 99.                              
           05  WS-SPLIT-DD                 PIC 99.                              
                                                                                
       01  P-NO-INPUT.                                                          
           05  FILLER                      PIC X      VALUE '1'.                
           05  FILLER                      PIC X(30)  VALUE                     
           'NO FORMS RECEIVED OR PRINTED'.                                      
                                                                                
       01  P-HDR-1.                                                             
           05  FILLER                      PIC X      VALUE '1'.                
           05  FILLER                      PIC X(7)   VALUE                     
               'DATE:'.                                                         
           05  P-DD                        PIC 99.                              
           05  FILLER                      PIC X      VALUE '/'.                
           05  P-MMM                       PIC XXX.                             
           05  FILLER                      PIC X      VALUE '/'.                
           05  P-YYYY                      PIC 9(4).                            
           05  FILLER                      PIC X(83)  VALUE SPACES.             
           05  FILLER                      PIC X(6)   VALUE 'PAGE: '.           
           05  P-PGNO                      PIC ZZZ9.                            
                                                                                
       01  P-HDR-2.                                                             
           05  FILLER                      PIC X      VALUE '2'.                
           05  FILLER                      PIC X(14)  VALUE                     
               'PROG: GCCPFRMR'.                                                
           05  FILLER                      PIC X(39)  VALUE SPACES.             
           05  FILLER                      PIC X(25)  VALUE                     
               'PRINT/DISTRIBUTION REPORT'.                                     
                                                                                
       01  P-HDR-3.                                                             
           05  FILLER                      PIC X      VALUE '0'.                
           05  FILLER                      PIC X(50)  VALUE SPACES.             
           05  P-H3-DIST-GROUP             PIC X(30)  VALUE SPACES.             
                                                                                
       01  P-HDR-4.                                                             
           05  FILLER                      PIC X      VALUE '2'.                
           05  FILLER                      PIC X(14)  VALUE SPACES.             
           05  FILLER                      PIC X(11)  VALUE                     
           'FORM'.                                                              
           05  FILLER                      PIC X(50)  VALUE                     
           'MEMBER NAME'.                                                       
           05  FILLER                      PIC X(16)  VALUE                     
           'CERTIFICATE NO'.                                                    
           05  FILLER                      PIC X(15)  VALUE                     
           'CONFIRM NO'.                                                        
           05  FILLER                      PIC X(6)   VALUE                     
           'SEQ NO'.                                                            
                                                                                
       01  P-SHD-GROUP.                                                         
           05  FILLER                      PIC X      VALUE '0'.                
           05  FILLER                      PIC X(14)  VALUE SPACES.             
           05  FILLER                      PIC X(7)   VALUE                     
           'GROUP:'.                                                            
           05  P-SHDG-GROUP                PIC X(7).                            
           05  FILLER                      PIC X(7)  VALUE                      
           '  DIV:'.                                                            
           05  P-SHD-DIV                   PIC X(3).                            
                                                                                
       01  P-SHD-MASS-SUM.                                                      
           05  FILLER                      PIC X      VALUE '0'.                
           05  FILLER                      PIC X(14)  VALUE SPACES.             
           05  FILLER                      PIC X(32)   VALUE                    
           'EMPLOYMENT/SALARY CHANGE SUMMARY'.                                  
                                                                                
       01  P-STOT-GROUP.                                                        
           05  FILLER                      PIC X      VALUE '0'.                
           05  FILLER                      PIC X(14)  VALUE SPACES.             
           05  FILLER                      PIC X(25)  VALUE                     
           'TOTAL NUMBER OF FORMS ='.                                           
           05  P-SG-TOTAL                  PIC Z(6)9.                           
                                                                                
       01  P-STOT-CAT.                                                          
           05  FILLER                      PIC X      VALUE '0'.                
           05  FILLER                      PIC X(14)  VALUE SPACES.             
           05  FILLER                      PIC X(72)  VALUE                     
           'TOTAL NUMBER OF FORMS PRINTED FOR THIS DISTRIBUTION CATEGORY        
      -    '/GROUPING:'.                                                        
           05  P-SC-TOTAL                  PIC Z(6)9.                           
                                                                                
                                                                                
       01  P-DTL-LINE.                                                          
           05  FILLER                      PIC X      VALUE '0'.                
           05  P-DTL-INIT.                                                      
               10 FILLER                   PIC X(14)  VALUE SPACES.             
               10 P-FORM                   PIC X(6).                            
               10 FILLER                   PIC X(3)   VALUE SPACES.             
               10 P-NAME                   PIC X(48).                           
               10 FILLER                   PIC XX     VALUE SPACES.             
               10 P-CERTIFICATE-NO         PIC X(10).                           
               10 FILLER                   PIC X(6)   VALUE SPACES.             
               10 P-CONFIRM-NO             PIC Z(10)9.                          
               10 FILLER                   PIC X(3)   VALUE SPACES.             
               10 P-SEQ-NO                 PIC Z(4)9.                           
       01  P-SUM-LINE.                                                          
           05  FILLER                      PIC X       VALUE '0'.               
           05  P-SUM-INIT                  PIC X(70)   VALUE SPACES.            
                                                                                
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
                                                                                
      *    PRINT DISTRIBUTION REPORT UNTIL                                      
      *    END-OF-FILE IS ENCOUNTERED FOR THE INPUT FILE:                       
                                                                                
           PERFORM 2000-PRINT-DIST-RPT                                          
           THRU    2000-PRINT-DIST-RPT-EXIT                                     
                   UNTIL WS-EOF.                                                
                                                                                
           PERFORM 9000-FINISH          THRU 9000-EXIT.                         
                                                                                
           GOBACK.                                                              
                                                                                
       0000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1000-INITIALIZATION.                                                     
                                                                                
           MOVE 'GCCPFRMR'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
                                                                                
           MOVE WS-OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                       
                                                                                
           PERFORM 6300-USE-TODAYS-DATE THRU                                    
                       6300-USE-TODAYS-DATE-EXIT.                               
                                                                                
           MOVE  WS-SPLIT-YYYY      TO  P-YYYY.                                 
           MOVE  WS-MONTH-NAME(WS-SPLIT-MM)                                     
                                    TO  P-MMM.                                  
           MOVE  WS-SPLIT-DD        TO  P-DD.                                   
                                                                                
           INITIALIZE                   PRINT-RECORD.                           
                                                                                
      ******************************************************************        
      * READ FIRST INPUT RECORD                                                 
      ******************************************************************        
                                                                                
           MOVE WS-OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                       
           PERFORM 6100-READ-INPUT THRU                                         
                   6100-READ-INPUT-EXIT.                                        
                                                                                
           IF WS-EOF                                                            
               MOVE P-NO-INPUT       TO PRINT-RECORD                            
                                                                                
               PERFORM 7900-PRINT-LINE     THRU                                 
                       7900-PRINT-LINE-EXIT                                     
           END-IF.                                                              
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-PRINT-DIST-RPT.                                                     
      ******************************************************************        
      * PRINT DISTRIBUTION REPORT                                               
      ******************************************************************        
                                                                                
           MOVE 'N'               TO  WS-CATEGORY-MKR                           
                                      WS-GROUP-MKR                              
                                      WS-DIVISION-MKR                           
                                      WS-FORM-MKR.                              
           MOVE GCCCRPT-DIST-CAT-GRP                                            
                                  TO  WS-SAVE-CATEGORY.                         
                                                                                
           MOVE SPACES            TO  WS-SAVE-GROUP                             
                                      WS-SAVE-DIVISION                          
                                      WS-SAVE-FORM.                             
                                                                                
           PERFORM 7000-PAGE-HEADING   THRU                                     
                   7000-PAGE-HEADING-EXIT.                                      
                                                                                
           PERFORM 2100-PRINT-CATEGORY THRU                                     
                   2100-PRINT-CATEGORY-EXIT                                     
                     UNTIL  WS-CATEGORY-CHANGE.                                 
                                                                                
           PERFORM 2050-PRODUCE-CAT-TOTALS                                      
             THRU  2050-PRODUCE-CAT-TOTALS-EXIT.                                
                                                                                
           IF WS-TOTAL-RECORD                                                   
               PERFORM 7000-PAGE-HEADING                                        
                  THRU 7000-PAGE-HEADING-EXIT                                   
               PERFORM 2500-PROCESS-TOTAL-RECORDS                               
                  THRU 2500-EXIT                                                
                         UNTIL WS-DETAIL-RECORD                                 
                         OR    WS-EOF.                                          
                                                                                
       2000-PRINT-DIST-RPT-EXIT.                                                
           EXIT.                                                                
                                                                                
       2050-PRODUCE-CAT-TOTALS.                                                 
      ******************************************************************        
      * PRINT CATEGORY TOTALS                                                   
      ******************************************************************        
                                                                                
           MOVE  WS-FORMS-COUNT-CAT    TO  P-SC-TOTAL.                          
           MOVE  P-STOT-CAT            TO  PRINT-RECORD.                        
                                                                                
           PERFORM 7900-PRINT-LINE     THRU                                     
                   7900-PRINT-LINE-EXIT.                                        
                                                                                
           MOVE  0                     TO  WS-FORMS-COUNT-CAT.                  
                                                                                
       2050-PRODUCE-CAT-TOTALS-EXIT.                                            
           EXIT.                                                                
                                                                                
       2100-PRINT-CATEGORY.                                                     
      ******************************************************************        
      * PRINT EACH DISTRIBUTION CATEGORY/GROUPING                               
      ******************************************************************        
                                                                                
           MOVE GCCCRPT-GROUP     TO  WS-SAVE-GROUP                             
                                      P-SHDG-GROUP.                             
                                                                                
           INSPECT P-SHDG-GROUP                                                 
              REPLACING LEADING '0' BY ' '.                                     
                                                                                
           MOVE 'N'               TO  WS-GROUP-MKR                              
                                      WS-DIVISION-MKR                           
                                      WS-FORM-MKR.                              
           MOVE SPACES            TO  WS-SAVE-DIVISION                          
                                      WS-SAVE-FORM.                             
                                                                                
           PERFORM 2200-PRINT-GROUP    THRU                                     
                   2200-PRINT-GROUP-EXIT                                        
                     UNTIL  WS-GROUP-CHANGE.                                    
                                                                                
           PERFORM 2150-PRODUCE-GROUP-TOTALS THRU                               
                   2150-PRODUCE-GROUP-TOTALS-EXIT.                              
                                                                                
       2100-PRINT-CATEGORY-EXIT.                                                
           EXIT.                                                                
                                                                                
       2150-PRODUCE-GROUP-TOTALS.                                               
      ******************************************************************        
      * PRINT GROUP TOTALS                                                      
      ******************************************************************        
                                                                                
           MOVE  WS-FORMS-COUNT-GROUP  TO  P-SG-TOTAL.                          
           MOVE  P-STOT-GROUP          TO  PRINT-RECORD.                        
                                                                                
           PERFORM 7900-PRINT-LINE     THRU                                     
                   7900-PRINT-LINE-EXIT.                                        
                                                                                
           MOVE  0                     TO  WS-FORMS-COUNT-GROUP.                
                                                                                
       2150-PRODUCE-GROUP-TOTALS-EXIT.                                          
           EXIT.                                                                
                                                                                
       2200-PRINT-GROUP.                                                        
      ******************************************************************        
      * PRINT EACH GROUP                                                        
      ******************************************************************        
                                                                                
           MOVE GCCCRPT-DIVISION       TO  WS-SAVE-DIVISION                     
                                           P-SHD-DIV.                           
                                                                                
           MOVE 'N'                    TO  WS-DIVISION-MKR                      
                                           WS-FORM-MKR.                         
           MOVE SPACES                 TO  WS-SAVE-FORM.                        
                                                                                
           MOVE GCCCRPT-GROUP          TO  WS-SAVE-GROUP.                       
           MOVE 'N'                    TO  WS-GROUP-MKR                         
                                           WS-DIVISION-MKR.                     
                                                                                
           PERFORM 2250-PRODUCE-DIV-HEAD  THRU                                  
                   2250-PRODUCE-DIV-HEAD-EXIT.                                  
                                                                                
           PERFORM 2300-PRINT-DIV      THRU                                     
                   2300-PRINT-DIV-EXIT                                          
                     UNTIL  WS-DIVISION-CHANGE.                                 
                                                                                
       2200-PRINT-GROUP-EXIT.                                                   
           EXIT.                                                                
                                                                                
       2250-PRODUCE-DIV-HEAD.                                                   
      ******************************************************************        
      * PRINT DIVISION HEAD AT CHANGE OF DIV OR GROUP                           
      ******************************************************************        
                                                                                
           IF  WS-LINE-COUNT  >  27                                             
               PERFORM  7000-PAGE-HEADING    THRU                               
                        7000-PAGE-HEADING-EXIT.                                 
                                                                                
           IF  GCCCRPT-GROUP     =  '9999999'                                   
             AND                                                                
               GCCCRPT-DIVISION  =  '999'                                       
               MOVE  P-SHD-MASS-SUM        TO  PRINT-RECORD                     
             ELSE                                                               
               MOVE  P-SHD-GROUP           TO  PRINT-RECORD.                    
                                                                                
           PERFORM 7900-PRINT-LINE     THRU                                     
                   7900-PRINT-LINE-EXIT.                                        
                                                                                
                                                                                
       2250-PRODUCE-DIV-HEAD-EXIT.                                              
           EXIT.                                                                
                                                                                
       2300-PRINT-DIV.                                                          
      ******************************************************************        
      * PRINT EACH DIVISION WITHIN THE GROUP                                    
      ******************************************************************        
                                                                                
           MOVE GCCCRPT-FORM-NAME      TO  WS-SAVE-FORM                         
                                           P-FORM.                              
           MOVE 'N'                    TO  WS-FORM-MKR.                         
                                                                                
           PERFORM 2400-PRINT-FORM     THRU                                     
                   2400-PRINT-FORM-EXIT                                         
                     UNTIL  WS-FORM-CHANGE.                                     
                                                                                
       2300-PRINT-DIV-EXIT.                                                     
           EXIT.                                                                
                                                                                
       2400-PRINT-FORM.                                                         
      ******************************************************************        
      * PRINT EACH FORM WITHIN THE DIVISION                                     
      ******************************************************************        
                                                                                
                                                                                
           ADD  1                       TO  WS-FORMS-COUNT-GROUP.               
           ADD  1                       TO  WS-FORMS-COUNT-CAT.                 
           MOVE  GCCCRPT-MEMBER-NAME    TO  P-NAME.                             
           MOVE  GCCCRPT-CERTIFICATE-NO TO  P-CERTIFICATE-NO.                   
           MOVE  GCCCRPT-CONFIRM-NO     TO  P-CONFIRM-NO.                       
           MOVE  GCCCRPT-SEQUENCE-NO    TO  P-SEQ-NO.                           
           MOVE  P-DTL-LINE             TO  PRINT-RECORD.                       
                                                                                
           PERFORM 7900-PRINT-LINE   THRU  7900-PRINT-LINE-EXIT.                
                                                                                
           MOVE SPACES TO P-DTL-INIT.                                           
                                                                                
           PERFORM  6000-GET-NEXT    THRU  6000-GET-NEXT-EXIT.                  
                                                                                
           IF  WS-GROUP-CHANGE                                                  
            OR                                                                  
               WS-CATEGORY-CHANGE                                               
                 NEXT SENTENCE                                                  
             ELSE                                                               
               IF  WS-LINE-COUNT  >  27                                         
                   PERFORM  7000-PAGE-HEADING    THRU                           
                            7000-PAGE-HEADING-EXIT.                             
                                                                                
       2400-PRINT-FORM-EXIT.                                                    
           EXIT.                                                                
                                                                                
                                                                                
       2500-PROCESS-TOTAL-RECORDS.                                              
      ******************************************************************        
      * PRINT THE TOTAL RECORDS CREATED IN GCCCPFRMF                            
      * THESE TOTAL WERE ALSO INCLUDED ON THE BANNER TRAILER PAGES              
      ******************************************************************        
                                                                                
           MOVE GCCCRPT-TOTAL-LINE     TO P-SUM-INIT.                           
           MOVE P-SUM-LINE             TO PRINT-RECORD.                         
                                                                                
           PERFORM 7900-PRINT-LINE                                              
              THRU 7900-PRINT-LINE-EXIT.                                        
                                                                                
           MOVE WS-OBTAIN-NEXT         TO WS-GAEDATSR-VERB.                     
                                                                                
           PERFORM 6100-READ-INPUT                                              
              THRU 6100-READ-INPUT-EXIT.                                        
                                                                                
           IF WS-EOF                                                            
               GO TO 2500-EXIT                                                  
           END-IF.                                                              
                                                                                
           IF GCCCRPT-DETAIL-REC                                                
               SET WS-DETAIL-RECORD    TO TRUE                                  
           END-IF.                                                              
                                                                                
       2500-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6000-GET-NEXT.                                                           
           MOVE WS-OBTAIN-NEXT         TO WS-GAEDATSR-VERB.                     
           PERFORM  6100-READ-INPUT THRU 6100-READ-INPUT-EXIT.                  
                                                                                
           IF  GCCCRPT-FORM-NAME NOT  =  WS-SAVE-FORM                           
               MOVE 'Y'   TO  WS-FORM-MKR.                                      
                                                                                
           IF  GCCCRPT-DIVISION  NOT  =  WS-SAVE-DIVISION                       
               MOVE 'Y'   TO  WS-FORM-MKR                                       
               MOVE 'Y'   TO  WS-DIVISION-MKR.                                  
                                                                                
           IF  GCCCRPT-GROUP     NOT  =  WS-SAVE-GROUP                          
               MOVE 'Y'                TO  WS-FORM-MKR                          
               MOVE 'Y'                TO  WS-DIVISION-MKR                      
               MOVE 'Y'                TO  WS-GROUP-MKR.                        
                                                                                
           IF  GCCCRPT-DIST-CAT-GRP  NOT  =   WS-SAVE-CATEGORY                  
               MOVE 'Y'                TO  WS-FORM-MKR                          
               MOVE 'Y'                TO  WS-DIVISION-MKR                      
               MOVE 'Y'                TO  WS-GROUP-MKR                         
               MOVE 'Y'                TO  WS-CATEGORY-MKR.                     
                                                                                
           IF  GCCCRPT-TOTAL-REC                                                
               SET  WS-TOTAL-RECORD    TO  TRUE                                 
               MOVE 'Y'                TO  WS-FORM-MKR                          
               MOVE 'Y'                TO  WS-DIVISION-MKR                      
               MOVE 'Y'                TO  WS-GROUP-MKR                         
               MOVE 'Y'                TO  WS-CATEGORY-MKR.                     
                                                                                
           IF  WS-EOF                                                           
               MOVE 'Y'                TO  WS-FORM-MKR                          
               MOVE 'Y'                TO  WS-DIVISION-MKR                      
               MOVE 'Y'                TO  WS-GROUP-MKR                         
               MOVE 'Y'                TO  WS-CATEGORY-MKR.                     
                                                                                
       6000-GET-NEXT-EXIT.                                                      
           EXIT.                                                                
                                                                                
       6100-READ-INPUT.                                                         
      ******************************************************************        
      *    READ NEXT INPUT RECORD.                                     *        
      ******************************************************************        
           MOVE WS-INPUT-LR            TO LOGICAL-RECORD-NAME.                  
           INITIALIZE INP-RECORD.                                               
                                                                                
           CALL WS-GAEDATSR          USING  WS-GAEDATSR-VERB                    
                                         INP-RECORD                             
                                         ICBM.                                  
                                                                                
                                                                                
           IF LR-NOT-FOUND                                                      
               MOVE 'Y'                TO  WS-EOF-MKR                           
             ELSE                                                               
           IF NOT LR-STATUS-OK                                                  
               DISPLAY 'ERROR READING INPUT RECORD'                             
               DISPLAY '6100-READ-INPUT'                                        
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
                                                                                
       6100-READ-INPUT-EXIT.                                                    
           EXIT.                                                                
                                                                                
       6300-USE-TODAYS-DATE.                                                    
      ******************************************************************        
      *    USE GC2DATE TO GET CURRENT DATE IF CONTROL CARD READ        *        
      *    FAILED.                                                     *        
      ******************************************************************        
                                                                                
           MOVE 'E'                    TO  VDATE-REQ-SERVICE.                   
           MOVE 'A'                    TO  VDATE-REQ-BASIS.                     
           MOVE '1'                    TO  VDATE-REQ-DETAIL.                    
           MOVE 'E'                    TO  VDATE-REQ-LANGUAGE.                  
                                                                                
           CALL WS-GC2DATE      USING GAC-DATE-PARAMETERS.                      
                                                                                
           IF VDATE-RET-FAIL                                                    
               DISPLAY 'ERROR FINDING CURRENT DATE'                             
               DISPLAY 'GCCPFRMR - VDATE-EXT-DATE  : ' VDATE-EXT-DATE           
               DISPLAY 'GCCPFRMR - VDATE1-YYYYMMDD : ' VDATE1-YYYYMMDD          
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
           MOVE VDATE1-YYYYMMDD        TO  WS-SPLIT-DATE.                       
                                                                                
       6300-USE-TODAYS-DATE-EXIT.                                               
           EXIT.                                                                
                                                                                
                                                                                
       7000-PAGE-HEADING.                                                       
      ******************************************************************        
      *    PRODUCE PAGE HEADINGS                                                
      ******************************************************************        
                                                                                
           ADD   1                     TO  WS-PAGE-COUNT.                       
           MOVE  0                     TO  WS-LINE-COUNT.                       
           MOVE  WS-PAGE-COUNT         TO  P-PGNO.                              
           MOVE  P-HDR-1               TO  PRINT-RECORD.                        
                                                                                
           PERFORM 7900-PRINT-LINE     THRU                                     
                   7900-PRINT-LINE-EXIT.                                        
           MOVE  P-HDR-2               TO  PRINT-RECORD.                        
                                                                                
           PERFORM 7900-PRINT-LINE     THRU                                     
                   7900-PRINT-LINE-EXIT.                                        
           MOVE  WS-SAVE-CATEGORY      TO  P-H3-DIST-GROUP.                     
           MOVE  P-HDR-3               TO  PRINT-RECORD.                        
                                                                                
           PERFORM 7900-PRINT-LINE     THRU                                     
                   7900-PRINT-LINE-EXIT.                                        
                                                                                
           IF WS-DETAIL-RECORD                                                  
               MOVE  P-HDR-4            TO  PRINT-RECORD                        
                                                                                
               PERFORM 7900-PRINT-LINE     THRU                                 
                       7900-PRINT-LINE-EXIT                                     
           END-IF.                                                              
                                                                                
       7000-PAGE-HEADING-EXIT.                                                  
           EXIT.                                                                
                                                                                
       7900-PRINT-LINE.                                                         
      ******************************************************************        
      *    PRODUCE PRINT LINE, MAY BE REPORT HEADING,                           
      *    DETAIL OR SUBTOTAL                                                   
      ******************************************************************        
                                                                                
                                                                                
           MOVE WS-PRINT-LR            TO LOGICAL-RECORD-NAME.                  
           MOVE WS-STORE-LR            TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR          USING  WS-GAEDATSR-VERB                    
                                         PRINT-RECORD                           
                                         ICBM.                                  
           INITIALIZE PRINT-RECORD.                                             
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               DISPLAY 'ERROR WRITING TO PRINT FILE'                            
               DISPLAY PRINT-RECORD                                             
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
           ADD   1                     TO  WS-LINE-COUNT.                       
                                                                                
       7900-PRINT-LINE-EXIT.                                                    
           EXIT.                                                                
                                                                                
                                                                                
       9000-FINISH.                                                             
      ******************************************************************        
      * CLOSE FILES THAT ARE OPEN...                                            
      ******************************************************************        
                                                                                
           MOVE WS-INPUT-LR            TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
      *    MOVE WS-CARD-LR             TO LOGICAL-RECORD-NAME.                  
      *    MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
      *                                                                         
      *    CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
      *                        LOGICAL-RECORD-NAME                              
      *                        ICBM.                                            
                                                                                
           MOVE WS-PRINT-LR            TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9999-ABEND.                                                              
      *                                                                         
      ******************************************************************        
      *    TIME TO ABEND.                                                       
      ******************************************************************        
                                                                                
                                                                                
           MOVE +16 TO RETURN-CODE.                                             
                                                                                
           GOBACK.                                                              
                                                                                
       9999-ABEND-EXIT.                                                         
           EXIT.                                                                
