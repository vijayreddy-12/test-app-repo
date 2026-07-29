       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRMF.                                                 
      *AUTHOR.        KLYN                                                      
      ******************************************************************        
      *   GROUP BENEFITS E-BUSINESS AFP FORM, FORMAT                            
      *                                                                         
      *                                                                         
      *  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|                
      *  | NOTE DO NOT CHANGE PROFILE TO CAPS ON, AS THERE ARE |                
      *  |      HEADINGS PASSED TO APF IN LOWER CASE.          |                
      *  |                                          .          |                
      *  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                
      *                                                                         
      *                                                                         
      * PROGRAM DESCRIPTION:                                                    
      *     THIS PROGRAM READS A VARIABLE LENGTH FLAT FILE CREATED              
      *   IN PROGRAM GCCPFRME WHICH CONTAINS REFORMATTED RECORDS.               
      *   THESE RECORDS ARE AGAIN REFORMATTED TO THE AFP LAYOUT.                
      *   BANNER PAGES ARE INSERTED AT EACH CONTROL BREAK                       
      *   ANOTHER BANNER PAGE IS INSERTED BEFORE EACH CONTROL BREAK             
      *   WITH FORM COUNTS AND CREATES REPORT EXTRACT.                          
      *                                                                         
      * CALLING MODULES                                                         
      *     NOT APPLICABLE.                                                     
      *                                                                         
      * CALLED MODULES                                                          
      *     GAEDATSR - DATA SERVER                                              
      *     GACPOCC  - OCCUPATION CODE TRANSLATION PROGRAM                      
      *                                                                         
      * COPYBOOKS                                                               
      *     GARDSVRB - VERBS FOR GAEDATSR                                       
      *     GCCCFEXT - REPORT EXTRACT LAYOUT DETAIL                             
      *     GCCCRPT  - REPORT EXTRACT LAYOUT DISTRIBUTION                       
      *     GCCC0003 - INPUT FORM LAYOUT                                        
      *     GCCC0005 - INPUT FORM LAYOUT                                        
      *     GCCC0514 - INPUT FORM LAYOUT                                        
      *     GCCC2971 - INPUT FORM LAYOUT                                        
      *     GCCC3187 - INPUT FORM LAYOUT                                        
      *     GCCC3574 - INPUT FORM LAYOUT                                        
      *     GCCCMASS - INPUT FORM LAYOUT                                        
      *     CAFP0003 - GL0003   - AFP OUTPUT FORM LAYOUT                        
      *     CAF00051 - GL0005 PAGE 1 AFP OUTPUT FORM LAYOUT                     
      *     CAF00052 - GL0005 PAGE 2 AFP OUTPUT FORM LAYOUT                     
      *     CAFP0514 - GL0514   - AFP OUTPUT FORM LAYOUT                        
      *     CAF29711 - GL2971 PAGE 1 AFP OUTPUT FORM LAYOUT                     
      *     CAF29712 - GL2971 PAGE 2 AFP OUTPUT FORM LAYOUT                     
      *     CAF31871 - GL3187 PAGE 1 AFP OUTPUT FORM LAYOUT                     
      *     CAF31872 - GL3187 PAGE 2 AFP OUTPUT FORM LAYOUT                     
      *     CAF35741 - GL3574 PAGE 1 AFP OUTPUT FORM LAYOUT                     
      *     CAF35742 - GL3574 PAGE 2 AFP OUTPUT FORM LAYOUT                     
      *     CRPTMASS - MASS CHANGE REPORT OUTPUT FORM LAYOUT                    
      *     GACCOCC  - OCCUPATION CODE TRANSLATION PROGRAM LINKAGE              
      *                                                                         
      *                                                                         
      * INPUT  - SORTED FLAT FILE OF PRINT EXTRACT                              
      *                                                                         
      * OUTPUT - AFP FORMS PRINT EXTRACT                                        
      *        - PRINT/DISTRIBUTION REPORT EXTRACT                              
      *                                                                         
      ******************************************************************        
      * DATE       NAME      DESCRIPTION                                        
      * ---------  --------  -------------------------------------------        
      * 10NOV2000  KLYN      CREATION.                                          
      *                                                                         
      * 17JAN2001  KLYN      ADD RGO 15750 PROCESS/CONTRL BREAK FOR             
      *                      FOR MONTREAL                                       
      * 12APR2001  J.ELKINS  ALPHA AND SIGNATURE PIA MOVING TO                  
      *                      KC-10.  MAKE CHANGES TO WS-PENDING-DIST            
      * 27APR2001  J.ELKINS  ALLOW FOR CONT-STATUS OF 'T'.  CONTRACT            
      *                      STATUS ORIGINATES FROM CONTRACT_STAT_CD            
      *                      ON THE TGD TABLE                                   
      * 19JUN2001  J.ELKINS  CREATE NEW BANNER PAGE FOR INFORCE GROUPS          
      *                      FOR EACH TAT (SORT-CAT-0)                          
      *                      ADD FORM TOTALS ON BANNER TRAILER PAGES            
      *                      CHANGED TO CIIDB TO ACCESS FORM NAMES              
      *                      FROM TCTFRM TABLE                                  
      * 18SEP2001  J.ELKINS  RELEASE 3.2 - REMOVE GL2197-MAILED AND             
      *                      GL2197-MARITAL-STATUS AND ADDED                    
      *                      GL2971-SPOUSE-IND AND GL2971-COMN-LAW-IND          
      *                      REMOVED GL3187-MAILED, GL3187-SPOUSE-GENDER        
      *                      GL3187-MARITAL-STATUS AND ADDED                    
      *                      GL3187-SEC2-CHECKBOX, GL3187-SEC3-CHECKBOX         
      *                      GL3187-SEC4-CHECKBOX, GL3187-SEC5-CHECKBOX         
      *                      GL3187-SEC6-CHECKBOX, GL3187-SEC7-CHECKBOX         
      *                      GL3187-SEC8-CHECKBOX                               
      * 18OCT2001  J.ELKINS  RELEASE 3.2 - ADD GIPSY CODE TO OCCUPATION         
      *                      FIELD USED IN FORMS GL2971 AND GLMASS              
      * 28MAY2002  VANHEMMEN RELEASE 4.2 - ADD WAITING PERIOD FIELDS TO         
      *                      FORM GL2971                                        
      *                      CORRECT DATE TRANSLATION LANGUAGE FOR MASS         
      *                      CHANGE REPORT                                      
      * 07JUN2002  VANHEMMEN RELEASE 4.2 - ADD NEW FORM GL3574                  
      * 18JUL2002  J.ELKINS  RELEASE 4.2 - HCSA DATE ON GL3574 NOT SENT         
      *                      IN DATE FORMAT                                     
      * 29JUL2002  J.ELKINS  BUG FIX - GL3574 PRINTING OVER GL3187 ON           
      *                      BANNER PAGE - CORRECTED SUBSCRIPTING               
      * 22OCT2003  WODNIJA   GBSS TASK 26184 CHANGES TO GL2971                  
WB    * 26APR2004  BASHAWE   GBSS TASK 30069 CHANGES TO DELIVERY LOC            
WB    * 19JUL2004  BASHAWE   GBSS TASK 32308 CHANGE DEL LOC TO PICKUP           
      * 26AUG2008  IBM GR    UPGRADED IN ECU PROJECT                            
      * 05AUG2010  IBM       ADD A NEW FIELD TO GLMASS                          
      * 11MAY2015  IBM       ADD BANK DETAILS AND EMAIL DETAILS FOR FORM        
      *                      2971,3187 AND 3584                                 
      *                      REMOVE GCCC3574-WAITING-PERIOD-COND AND ITS        
      *                      CORRESPONDING LOGIC.                               
      *                      REMOVE GCCC3574-HCSA-ALLOC-TYPE AND ITS            
      *                      CORRESPONDING LOGIC.                               
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
                                                                                
      ******************************************************************        
      *** DB2 INCLUDES                                                          
      ******************************************************************        
                                                                                
      *** SQL COMMUNICATION AREA                                                
                                                                                
           EXEC SQL INCLUDE SQLCA   END-EXEC.                                   
                                                                                
      *** DB2 TABLE DECLARATIONS                                                
                                                                                
           EXEC SQL INCLUDE TCTFRMD END-EXEC.                                   
                                                                                
      *** DB2 HOST & INDICATOR VARAIBLES                                        
                                                                                
      *01  DCLTCTFRM                                                            
           EXEC SQL INCLUDE TCTFRM  END-EXEC.                                   
                                                                                
       01  WS-CONSTANTS.                                                        
           05  FILLER                      PIC X(32) VALUE                      
               '*** GCCPFRMF WORKING STORAGE ***'.                              
                                                                                
       01  WS-RELSHP-VARIABLES.                                                 
           05  WS-RELSHP-PARMS.                                                 
               10  WS-OUT-RELSHP               PIC X.                           
               10  WS-IN-RELSHP                PIC X.                           
               10  WS-RELSHP-LANG              PIC X.                           
           05  WS-CHGCODE-PARMS.                                                
               10  WS-OUT-CHG-CODE             PIC X.                           
               10  WS-IN-CHG-CODE              PIC X.                           
                                                                                
                                                                                
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GAEDATSR-VERB            PIC X(16).                           
           05  WS-GAEDATSR                 PIC X(8)  VALUE 'GAEDATSR'.          
           05  WS-GACPOCC                  PIC X(8)  VALUE 'GACPOCC'.           
       01  WS-COUNTS.                                                           
           05  WS-AFP-COUNT                PIC S9(6) COMP-3 VALUE ZERO.         
           05  WS-BENEFIT-SUBSCRIPT        PIC S9(6) COMP-3 VALUE ZERO.         
           05  WS-CHILD-SUBSCRIPT          PIC S9(6) COMP-3 VALUE ZERO.         
           05  WS-BANNER-SUBSCRIPT         PIC S9(6) COMP-3 VALUE ZERO.         
           05  WS-MASS-SUB                 PIC S9(6) COMP-3 VALUE ZERO.         
           05  WS-TEMP-REGION              PIC x(5)         value ' '.          
           05  WS-TEMP-REG-IN              PIC x            value ' '.          
                                                                                
       01  WS-SAVED-COMPARISONS.                                                
           05  WS-SAVED-MASS-CONF         PIC 9(11).                            
           05  WS-SAVED-MASS-SEQ          PIC 9(5).                             
           05  WS-INTERNAL-KEY            PIC XXXXXX.                           
           05  WS-NEW-INTERNAL-KEY.                                             
               10 WS-NIK-LANG             PIC X.                                
               10 WS-NIK-CONT-ST          PIC X.                                
               10 WS-NIK-SORT-CAT0        PIC XX.                               
               10 WS-NIK-SORT-CAT1        PIC X.                                
               10 WS-NIK-SORT-CAT2        PIC X.                                
                                                                                
       01  WS-MARKERS.                                                          
           05  WS-EOF-MKR                  PIC X     VALUE 'N'.                 
               88  WS-EOF                            VALUE 'Y'.                 
           05  WS-CHANGE-BANNER-MKR        PIC X     VALUE 'N'.                 
               88  WS-CHANGE-BANNER                  VALUE 'Y'.                 
           05  WS-STOP-SUBSCRIPT-MKR       PIC X     VALUE 'N'.                 
               88  WS-STOP-SUBSCRIPT                 VALUE 'Y'.                 
           05  WS-MASS-OVER-MKR            PIC X     VALUE 'N'.                 
               88  WS-MASS-OVER                      VALUE 'Y'.                 
                                                                                
       01  WS-VARIABLES.                                                        
           05  WS-OBTAIN-FIRST             PIC X(16)   VALUE                    
                                               'OBTAIN  FIRST   '.              
           05  WS-OBTAIN-NEXT              PIC X(16)   VALUE                    
                                               'OBTAIN  NEXT    '.              
           05  WS-STORE-LR                 PIC X(16) VALUE                      
                                               'STORE           '.              
           05  WS-SORT-FILE-IN-LR          PIC X(16) VALUE                      
                                               'CARD-DATA-070   '.              
           05  WS-AFP-FILE-OUT-LR          PIC X(16) VALUE                      
                                               'PRINT-DATA-020  '.              
           05  WS-REPORT-EXTRACT-LR        PIC X(16) VALUE                      
                                               'PRINT-DATA-021  '.              
           05  WS-MASS-CHG-OUT-LR          PIC X(16) VALUE                      
                                               'PRINT-DATA-022  '.              
           05  WS-DELIMITER                PIC X     VALUE '%'.                 
                                                                                
                                                                                
       01  WS-DATE-CH-PARM.                                                     
           05  WS-DCP-LANG                 PIC X.                               
               88  WS-DCP-ENGLISH        VALUE 'E'.                             
               88  WS-DCP-FRENCH         VALUE 'F'.                             
           05  WS-DCPI-DATE.                                                    
               10  WS-DCPI-YEAR            PIC XXXX.                            
               10  WS-DCPI-MONTH.                                               
                   15  WS-DCPI-MONTH-NUM   PIC 99.                              
               10  WS-DCPI-DAY             PIC XX.                              
           05  WS-DCPO-DATE                PIC X(12).                           
      *                                                                         
           05  WS-MONTH-FNAMES    VALUE                                         
               'JANVFEVRMARSAVR MAI JUINJUILAOUTSEPTOCT NOV DEC '.              
      *        'JAN FEV MAR AVR MAI JUINJUILAOU SEP OCT NOV DEC '.              
               10  WS-MONTH-NAME-FRENCH    PIC X(4)   OCCURS 12.                
           05  WS-MONTH-ENAMES    VALUE                                         
               'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'.                          
               10  WS-MONTH-NAME-ENGLISH   PIC X(3)   OCCURS 12.                
                                                                                
                                                                                
       01  INP-RECORD.                                                          
          03  GCCCFEXT-RECL                PIC S9(4)  COMP.                     
          03  FILLER                       PIC XX     VALUE  SPACES.            
          03 GCCCFEXT-DETAIL.                                                   
           COPY GCCCFEXT.                                                       
                                                                                
      *    THIS TEMP AREA IS USED TO ELIMINATE THE NEED FOR DUPLICATE           
      *    COPYBOOKS BY USING ONLY THE DETAIL AREA OF EXISTING ONES.            
      *    NOTE THE HEADER DETAIL DOES NOT MATCH UP.                            
                                                                                
                                                                                
       01  WS-TEMP-RECORD.                                                      
      *                                                                         
      *    NOTE THIS EXTRA CHARACTER WAS ADDED TO HIGHLIGHT                     
      *    THE FACT THAT THE HEADERS ARE DIFFERENT LENGTHS                      
      *    ON THE EXTRACT AND THE DETAIL.                                       
      *                                                                         
          03 WS-TEMP-EXTRA-HEAD-CHAR       PIC X.                               
          03 WS-TEMP-GCCC-HEAD-AND-DETL.                                        
              05 WS-GCCC-HEADER            PIC X(61).                           
              05 WS-GCCC-DETAIL            PIC X(3939).                         
                                                                                
          03 FILLER     REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.                   
           COPY GCCC3187.                                                       
                                                                                
          03 FILLER     REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.                   
           COPY GCCC2971.                                                       
                                                                                
          03 FILLER     REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.                   
           COPY GCCC0514.                                                       
                                                                                
          03 FILLER     REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.                   
           COPY GCCC0003.                                                       
                                                                                
          03 FILLER     REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.                   
           COPY GCCC0005.                                                       
                                                                                
          03 FILLER     REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.                   
           COPY GCCCMASS.                                                       
                                                                                
          03 FILLER     REDEFINES WS-TEMP-GCCC-HEAD-AND-DETL.                   
           COPY GCCC3574.                                                       
                                                                                
                                                                                
                                                                                
       01  WS-OUTPUT-EXTRACT-LINE.                                              
           COPY GCCCRPT.                                                        
                                                                                
       01  WS-OUT-MASS-RECORD.                                                  
         02 FILLER                         PIC X(240).                          
                                                                                
       01  WS-OUT-MASS-LAYOUT.                                                  
           COPY CRPTMASS.                                                       
                                                                                
       01  WS-OUT-AFP-RECORD.                                                   
         02 FILLER                         PIC X(2000).                         
                                                                                
                                                                                
       01  WS-OUTPUT-AFP-LAYOUT.                                                
         02 GL3187-RECORD.                                                      
      *    COPY CAFP3187.                                                       
          03 GL3187-RECORD-P1.                                                  
           COPY CAF31871.                                                       
          03 GL3187-RECORD-P2.                                                  
           COPY CAF31872.                                                       
                                                                                
         02 GL2971-RECORD    REDEFINES GL3187-RECORD.                           
      *    COPY CAFP2971.                                                       
          03 GL2971-RECORD-P1.                                                  
           COPY CAF29711.                                                       
          03 GL2971-RECORD-P2.                                                  
           COPY CAF29712.                                                       
                                                                                
         02 GL0514-RECORD    REDEFINES GL3187-RECORD.                           
           COPY CAFP0514.                                                       
                                                                                
         02 GL0003-RECORD    REDEFINES GL3187-RECORD.                           
           COPY CAFP0003.                                                       
                                                                                
         02 GL0005-RECORD    REDEFINES GL3187-RECORD.                           
      *    COPY CAFP0005.                                                       
          03 GL0005-RECORD-P1.                                                  
           COPY CAF00051.                                                       
          03 GL0005-RECORD-P2.                                                  
           COPY CAF00052.                                                       
                                                                                
         02 GLBANR-RECORD    REDEFINES GL3187-RECORD.                           
           COPY GLBANR.                                                         
                                                                                
         02 GL3574-RECORD    REDEFINES GL3187-RECORD.                           
          03 GL3574-RECORD-P1.                                                  
           COPY CAF35741.                                                       
          03 GL3574-RECORD-P2.                                                  
           COPY CAF35742.                                                       
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                          
                                                                                
                                                                                
       01  WS-BANNER-LINE-COMPONENTS.                                           
           05  WS-BANNER-LINES.                                                 
               10  WS-BANNER-LINE             OCCURS 15.                        
                   15  FILLER              PIC X(70)  VALUE  SPACES.            
                                                                                
      *    IN CONSTANT LINES                                                    
                                                                                
           05  WS-BANNER-DIST.                                                  
               10  FILLER                  PIC X(20)  VALUE                     
                   'DISTRIBUTE TO:'.                                            
           05  WS-BANNER-INFORCE-LINES.                                         
               10  WS-IN-FORCE-DIST-LINE.                                       
                   15  FILLER              PIC X(20)  VALUE                     
                       'PLAN MEMBER ADMIN '.                                    
               10  WS-IN-FORCE-TAT-LINE.                                        
                   15  FILLER              PIC X(18)  VALUE                     
                       'TURNAROUND TIME = '.                                    
                   15  WS-IN-FORCE-TAT     PIC X(2)   VALUE SPACES.             
               10  WS-IN-FORCE-DSTN-LINE.                                       
                   15  FILLER              PIC X(20)  VALUE                     
      *                'DEL STN: GB-C'.                                         
WB    *                'DEL STN: 500-G-C'.                                      
WB                     'DEL STN: PICKUP'.                                       
               10  WS-IN-FORCE-TEAM-LINE.                                       
                   15  FILLER              PIC X(7)   VALUE                     
                       'TEAM = '.                                               
                   15  WS-IN-FORCE-TEAM    PIC X(4)   VALUE SPACES.             
                                                                                
           05  WS-BANNER-PENDING-LINES.                                         
               10  WS-PENDING-DIST-LINE.                                        
                   15  FILLER              PIC X(12)  VALUE                     
                       'XEROX ENROL-'.                                          
                   15  WS-PENDING-DIST     PIC X(20).                           
               10  WS-PENDING-DSTN-LINE.                                        
JE001              15  FILLER              PIC X(09)  VALUE                     
JE001                  'DEL STN: '.                                             
JE001              15  WS-PENDING-DSTN     PIC X(11).                           
               10  WS-PENDING-DUMMY        PIC X(20)  VALUE SPACES.             
                                                                                
           05  WS-BANNER-MONTREAL-LINES.                                        
               10  WS-MONTREAL-DIST-LINE.                                       
                   15  FILLER              PIC X(20)  VALUE                     
                       'MTL PIA 4TH FLOOR -'.                                   
                   15  WS-MONTREAL-DIST     PIC X(20).                          
               10  WS-MONTREAL-DSTN-LINE.                                       
                   15  FILLER              PIC X(20)  VALUE                     
                       'DEL STN:  1575'.                                        
               10  WS-MONTREAL-DUMMY        PIC X(20)  VALUE SPACES.            
                                                                                
                                                                                
       01  WS-BANNER-TOT-LINE-COMPONENTS.                                       
           05  WS-BANNER-TOT-DETAIL.                                            
               10  WS-BANNER-TOT-DET1.                                          
                   15  FILLER              PIC X(17)  VALUE                     
                       'FORM COUNT ='.                                          
                   15  WS-BANNER-AFP-COUNT                                      
                                           PIC Z(6)9.                           
               10  WS-BANNER-TOT-DET2.                                          
                   15  FILLER              PIC X(35)  VALUE                     
                       'LAST PAGE OF'.                                          
               10  WS-BANNER-TOT-DET3.                                          
                   15  FILLER              PIC X(35)  VALUE SPACES.             
               10  WS-BANNER-TOT-DET4.                                          
                   15  FILLER              PIC X(35)  VALUE SPACES.             
               10  WS-BANNER-TOT-DET5.                                          
                   15  FILLER              PIC X(35)  VALUE SPACES.             
                                                                                
                                                                                
       01  WS-BANNER-TOT-HEADING.                                               
           05  FILLER                      PIC X(8)   VALUE                     
               'FORM#'.                                                         
           05  FILLER                      PIC X(32)  VALUE                     
               '          FORM NAME'.                                           
           05  FILLER                      PIC X(10)  VALUE                     
               '#ENGLISH'.                                                      
           05  FILLER                      PIC X(10)  VALUE                     
               '#FRENCH'.                                                       
           05  FILLER                      PIC X(10)  VALUE                     
               'TOTAL#'.                                                        
                                                                                
       01  WS-BANNER-TOT-DETAIL-LINE.                                           
           05  WS-BANNER-TOT-FORM          PIC X(6)   VALUE SPACES.             
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-BANNER-TOT-DESC          PIC X(30)  VALUE SPACES.             
           05  FILLER                      PIC X(3)   VALUE SPACES.             
           05  WS-BANNER-TOT-ENG           PIC ZZZZZZ9 VALUE ZERO.              
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-BANNER-TOT-FR            PIC ZZZZZZ9 VALUE ZERO.              
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-BANNER-TOT-TOT           PIC ZZZZZZ9 VALUE ZERO.              
           05  FILLER                      PIC X(4)   VALUE SPACES.             
                                                                                
       01  WS-BANNER-BY-REPORT.                                                 
           05  WS-BANNER-GL0003-NAME       PIC X(30)  VALUE SPACES.             
           05  WS-BANNER-GL0003-ENG-TOT    PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL0003-FR-TOT     PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL0005-NAME       PIC X(30)  VALUE SPACES.             
           05  WS-BANNER-GL0005-ENG-TOT    PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL0005-FR-TOT     PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL0514-NAME       PIC X(30)  VALUE SPACES.             
           05  WS-BANNER-GL0514-ENG-TOT    PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL0514-FR-TOT     PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL2971-NAME       PIC X(30)  VALUE SPACES.             
           05  WS-BANNER-GL2971-ENG-TOT    PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL2971-FR-TOT     PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL3187-NAME       PIC X(30)  VALUE SPACES.             
           05  WS-BANNER-GL3187-ENG-TOT    PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL3187-FR-TOT     PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL3574-NAME       PIC X(30)  VALUE SPACES.             
           05  WS-BANNER-GL3574-ENG-TOT    PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-GL3574-FR-TOT     PIC S9(7)  COMP-3 VALUE 0.           
                                                                                
           05  WS-BANNER-TOTAL-ENG-TOT     PIC S9(7)  COMP-3 VALUE 0.           
           05  WS-BANNER-TOTAL-FR-TOT      PIC S9(7)  COMP-3 VALUE 0.           
                                                                                
           05  WS-BANNER-LINE-TOT          PIC S9(7)  COMP-3 VALUE 0.           
                                                                                
       01  WS-MASS-TRAILER-HEADING.                                             
           05  FILLER                      PIC X(41)  VALUE                     
           'EMPLOYMENT/SALARY CHANGE FORM'.                                     
           05  FILLER                      PIC X(10)  VALUE                     
           '#ENGLISH'.                                                          
           05  FILLER                      PIC X(09)  VALUE                     
           '#FRENCH'.                                                           
           05  FILLER                      PIC X(09)  VALUE                     
           'TOTAL#'.                                                            
                                                                                
       01  WS-MASS-TRAILER-DET1.                                                
           05  FILLER                      PIC X(41)  VALUE                     
           'EMPLOYMENT/SALARY CHANGE FORM COUNT:'.                              
           05  WS-MASS-TRAILER-FORM-COUNT  PIC ZZZZZZ9.                         
                                                                                
       01  WS-MASS-TRAILER-DET2.                                                
           05  FILLER                      PIC X(41)  VALUE                     
           'TERMINATION COUNT'.                                                 
           05  WS-MASS-TRAILER-ENG-TERM    PIC ZZZZZZ9.                         
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-MASS-TRAILER-FR-TERM     PIC ZZZZZZ9.                         
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-MASS-TRAILER-TOT-TERM    PIC ZZZZZZ9.                         
                                                                                
       01  WS-MASS-TRAILER-DET3.                                                
           05  FILLER                      PIC X(41)  VALUE                     
           'SALARY COUNT'.                                                      
           05  WS-MASS-TRAILER-ENG-SAL     PIC ZZZZZZ9.                         
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-MASS-TRAILER-FR-SAL      PIC ZZZZZZ9.                         
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-MASS-TRAILER-TOT-SAL     PIC ZZZZZZ9.                         
                                                                                
       01  WS-MASS-TRAILER-DET4.                                                
           05  FILLER                      PIC X(41)  VALUE                     
           'TOTALS'.                                                            
           05  WS-MASS-TRAILER-ENG-TOT     PIC ZZZZZZ9.                         
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-MASS-TRAILER-FR-TOT      PIC ZZZZZZ9.                         
           05  FILLER                      PIC X(2)   VALUE SPACES.             
           05  WS-MASS-TRAILER-TOT-TOT     PIC ZZZZZZ9.                         
                                                                                
       01  WS-MASS-TRAILER-COUNTERS.                                            
           05  WS-MASS-FORM-COUNT          PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-FR-SAL              PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-FR-TERM             PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-FR-TOT              PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-ENG-SAL             PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-ENG-TERM            PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-ENG-TOT             PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-TOT-SAL             PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-TOT-TERM            PIC 9(7)  COMP-3 VALUE 0.            
           05  WS-MASS-TOT-TOT             PIC 9(7)  COMP-3 VALUE 0.            
                                                                                
       01  WS-DEL-STATIONS.                                                     
           05  WS-DEL-STATION-KC10         PIC X(11)  VALUE 'KC-10'.            
      *    05  WS-DEL-STATION-GBA          PIC X(11)  VALUE 'GB-A'.             
WB    *    05  WS-DEL-STATION-GBA          PIC X(11)  VALUE '500-G-C'.          
WB         05  WS-DEL-STATION-GBA          PIC X(11)  VALUE 'PICKUP'.           
                                                                                
       01  WS-DIST-CAT-TAT-GRP.                                                 
           05  FILLER                      PIC X(5)   VALUE 'TAT: '.            
           05  WS-DIST-CAT-TAT             PIC X(2).                            
                                                                                
       01  WS-BANNER-PRINTED-WS            PIC X      VALUE 'N'.                
           88  WS-BANNER-PRINTED                      VALUE 'Y'.                
           88  WS-BANNER-NOT-PRINTED                  VALUE 'N'.                
                                                                                
       01  WS-OCCUPATION.                                                       
           05  WS-OCCUPATION-CODE        PIC X.                                 
           05  WS-OCCUPATION-SPACE       PIC X        VALUE SPACES.             
           05  WS-OCCUPATION-DESC        PIC X(27).                             
                                                                                
       01  WS-GIPSY-CODE.                                                       
           05  WS-GIPSY-CODE-CHAR1       PIC X.                                 
           05  WS-GIPSY-CODE-CHAR2       PIC X.                                 
                                                                                
       01  GACCOCC-RECORD.               COPY GACCOCC.                          
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
                                                                                
      *    CREATE AFP PRINT AND PRINT DISTRIBUTION REPORT                       
      *    EXTRACTS UNTIL END OF INPUT FILE:                                    
      *                                                                         
                                                                                
           PERFORM 2000-PROCESS-FORMS  THRU 2000-EXIT                           
                   UNTIL WS-EOF.                                                
                                                                                
           PERFORM 9000-FINISH          THRU 9000-EXIT.                         
                                                                                
           GOBACK.                                                              
                                                                                
       0000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1000-INITIALIZATION.                                                     
                                                                                
           MOVE 'GCCPFRMF'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
                                                                                
      ******************************************************************        
      * GET FORM NAMES FROM DB2 TABLE                                           
      ******************************************************************        
      * DB2 CURSOR DECLARE                                                      
      ******************************************************************        
                                                                                
           EXEC SQL                                                             
             DECLARE FORM_NAMES CURSOR FOR                                      
             SELECT   CODE_VALUE                                                
                     ,CODE_ENG_DESC                                             
             FROM    TCTFRM                                                     
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             OPEN FORM_NAMES                                                    
           END-EXEC.                                                            
                                                                                
           PERFORM                                                              
               WITH TEST BEFORE                                                 
               UNTIL SQLCODE = +100                                             
                                                                                
               EXEC SQL                                                         
                 FETCH FORM_NAMES                                               
                  INTO  :DCLTCTFRM.CODE-VALUE                                   
                       ,:DCLTCTFRM.CODE-ENG-DESC                                
               END-EXEC                                                         
                                                                                
               EVALUATE SQLCODE                                                 
                                                                                
                   WHEN ZERO                                                    
                       PERFORM 1010-INIT-REPORT-NAMES                           
                          THRU 1010-EXIT                                        
                   WHEN +100                                                    
                       CONTINUE                                                 
                   WHEN OTHER                                                   
                       DISPLAY 'SQL ERROR ACCESSING TCTFRM'                     
                       DISPLAY SQLCODE                                          
                       PERFORM 9999-ABEND THRU 9999-ABEND-EXIT                  
                                                                                
               END-EVALUATE                                                     
                                                                                
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
             CLOSE FORM_NAMES                                                   
           END-EXEC.                                                            
                                                                                
           INITIALIZE                   INP-RECORD.                             
                                                                                
      ******************************************************************        
      * READ FIRST INPUT RECORD                                                 
      ******************************************************************        
                                                                                
           MOVE WS-OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                       
           PERFORM 6100-READ-INPUT THRU 6100-EXIT.                              
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1010-INIT-REPORT-NAMES.                                                  
      ******************************************************************        
      * SAVE THE REPORT NAMES FROM THE TCTFRM TABLE                             
      ******************************************************************        
                                                                                
           IF CODE-VALUE OF DCLTCTFRM = 'GL0003'                                
               MOVE CODE-ENG-DESC OF DCLTCTFRM                                  
                                      TO WS-BANNER-GL0003-NAME                  
           ELSE                                                                 
           IF CODE-VALUE OF DCLTCTFRM = 'GL0005'                                
               MOVE CODE-ENG-DESC OF DCLTCTFRM                                  
                                      TO WS-BANNER-GL0005-NAME                  
           ELSE                                                                 
           IF CODE-VALUE OF DCLTCTFRM = 'GL0514'                                
               MOVE CODE-ENG-DESC OF DCLTCTFRM                                  
                                      TO WS-BANNER-GL0514-NAME                  
           ELSE                                                                 
           IF CODE-VALUE OF DCLTCTFRM = 'GL2971'                                
               MOVE CODE-ENG-DESC OF DCLTCTFRM                                  
                                      TO WS-BANNER-GL2971-NAME                  
           ELSE                                                                 
           IF CODE-VALUE OF DCLTCTFRM = 'GL3187'                                
               MOVE CODE-ENG-DESC OF DCLTCTFRM                                  
                                      TO WS-BANNER-GL3187-NAME                  
           ELSE                                                                 
           IF CODE-VALUE OF DCLTCTFRM = 'GL3574'                                
               MOVE CODE-ENG-DESC OF DCLTCTFRM                                  
                                      TO WS-BANNER-GL3574-NAME                  
           END-IF.                                                              
                                                                                
       1010-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-PROCESS-FORMS.                                                      
      ******************************************************************        
      * PRINT DISTRIBUTION REPORT                                               
      ******************************************************************        
                                                                                
      ******************************************************************        
      * EACH GROUPING INCLUDES A BANNER PAGE                                    
      * FOLLOWED BY MULTIPLE FORMS OF EACH TYPE                                 
      * FOLLOWED BY A BANNER TOTALS PAGE TO IDENTIFY THE NUMBER                 
      * OF FORMS                                                                
      ******************************************************************        
      *                                                                         
           MOVE  'N'                 TO WS-CHANGE-BANNER-MKR.                   
                                                                                
           PERFORM 2700-PRINT-BANNER   THRU 2700-EXIT.                          
                                                                                
           PERFORM 2800-PRINT-FORMS    THRU 2800-EXIT                           
                UNTIL WS-CHANGE-BANNER                                          
                  OR  WS-EOF.                                                   
                                                                                
           PERFORM 2900-PRINT-TOTALS   THRU 2900-EXIT.                          
                                                                                
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2100-PROCESS-MASS.                                                       
      ******************************************************************        
      * PROCESS MASS CHANGE REPORT                                              
      ******************************************************************        
                                                                                
      ******************************************************************        
      * EACH GROUPING INCLUDES A BANNER PAGE                                    
      * FOLLOWED BY MULTIPLE FORMS OF EACH TYPE                                 
      * FOLLOWED BY A BANNER TOTALS PAGE TO IDENTIFY THE NUMBER                 
      * OF FORMS                                                                
      ******************************************************************        
      *                                                                         
           MOVE 'N'                TO  WS-MASS-OVER-MKR.                        
           MOVE GCCCFEXT-CONF-NBR  TO  WS-SAVED-MASS-CONF.                      
           MOVE GCCCFEXT-SEQ-NBR   TO  WS-SAVED-MASS-SEQ.                       
                                                                                
                                                                                
           PERFORM 2120-PROC-MASS-HEAD THRU 2120-EXIT.                          
                                                                                
           PERFORM 4000-CREATE-EXTRACT THRU 4000-EXIT.                          
                                                                                
           PERFORM 2140-PROC-MASS-DETL THRU 2140-EXIT                           
                UNTIL WS-MASS-OVER                                              
                  OR  WS-EOF.                                                   
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2120-PROC-MASS-HEAD.                                                     
      ******************************************************************        
      * PROCESS MASS CHANGE REPORT HEADING RECORD                               
      ******************************************************************        
                                                                                
      ******************************************************************        
      * THE MASS CHANGE REPORT RECORDS ARE SENT TO BE                           
      * PROCESSED BY BY THE MASS CHANGE REPORT PROGRAM                          
      ******************************************************************        
      *                                                                         
                                                                                
           MOVE SPACES                  TO  GLMASS-HEADING.                     
           MOVE 'H'                     TO  GLMASS-REC-TYPE.                    
           MOVE GCCCFEXT-SORT-CAT0      TO  GLMASS-TAT.                         
           MOVE GCCCFEXT-SORT-CAT2      TO  GLMASS-BUS-SEG.                     
           MOVE GCCCFEXT-CONF-NBR       TO  GLMASS-CONFIRMATION.                
           MOVE GCCCFEXT-SEQ-NBR        TO  GLMASS-SEQ-NO.                      
           MOVE GCCCMASS-PLAN (1)       TO  GLMASS-HEAD-PLAN.                   
           MOVE GCCCFEXT-LANG           TO  GLMASS-LANG.                        
           MOVE GCCCMASS-TIMESTAMP      TO  GLMASS-DATE-TIME.                   
           MOVE GCCCMASS-CLIENT-NAME    TO  GLMASS-SPONSOR.                     
                                                                                
                                                                                
           MOVE GCCCFEXT-LANG           TO  WS-DCP-LANG.                        
           MOVE GCCCMASS-SUBMIT-DATE    TO  WS-DCPI-DATE.                       
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO  GLMASS-ENGLISH-TODAY.               
                                                                                
           MOVE  WS-OUT-MASS-LAYOUT     TO WS-OUT-MASS-RECORD.                  
           PERFORM 8500-WRITE-MASS-OUTPUT THRU 8500-EXIT.                       
                                                                                
      * ACCUMULATE TOTAL TO BE DISPLAYED ON PRINT/DISTRIBUTION REPORT           
      * AND TRAILER PAGE OF MASS CHANGE REPORT                                  
                                                                                
           ADD 1 TO WS-MASS-FORM-COUNT.                                         
                                                                                
                                                                                
                                                                                
       2120-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2140-PROC-MASS-DETL.                                                     
      ******************************************************************        
      * PRINT MASS CHANGE DETAIL AREA                                           
      ******************************************************************        
                                                                                
                                                                                
           PERFORM 2150-PROC-MASS-DETL THRU 2150-EXIT                           
             VARYING WS-MASS-SUB  FROM 1 BY 1                                   
                UNTIL WS-MASS-SUB > 15                                          
                  OR  WS-MASS-OVER.                                             
                                                                                
           PERFORM 6000-GET-NEXT         THRU 6000-EXIT.                        
                                                                                
           MOVE 'Y'  TO  WS-MASS-OVER-MKR.                                      
                                                                                
           IF  GCCCFEXT-FORM-NBR   =  'GLMASS  '                                
               IF  GCCCFEXT-CONF-NBR   =  WS-SAVED-MASS-CONF                    
                   IF  GCCCFEXT-SEQ-NBR   =  WS-SAVED-MASS-SEQ                  
                        MOVE 'N'  TO  WS-MASS-OVER-MKR.                         
                                                                                
                                                                                
                                                                                
       2140-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2150-PROC-MASS-DETL.                                                     
      ******************************************************************        
      * PRINT MASS CHANGE DETAIL LINES                                          
      ******************************************************************        
                                                                                
      *                                                                         
      *  THE FOLLOWING IF CHECKS FOR X'00' AND X'40'                            
      *                                                                         
           IF  GCCCMASS-PLAN  (WS-MASS-SUB) =  '     '                          
              OR                                                                
               GCCCMASS-PLAN  (WS-MASS-SUB) =  '     '                          
               MOVE  'Y'   TO  WS-MASS-OVER-MKR                                 
             ELSE                                                               
               PERFORM   2160-PROC-MASS-DETL.                                   
                                                                                
                                                                                
       2150-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2160-PROC-MASS-DETL.                                                     
      ******************************************************************        
      * POPULATE MASS CHANGE DETAIL LINE FIELDS                                 
      ******************************************************************        
                                                                                
      *                                                                         
           MOVE 'D'                         TO  GLMASS-REC-TYPE.                
           MOVE  GCCCMASS-PLAN            (WS-MASS-SUB)                         
                                            TO  GLMASS-PLAN.                    
           MOVE  GCCCMASS-ACCOUNT-NBR     (WS-MASS-SUB)                         
                                            TO  GLMASS-ACCOUNT.                 
           MOVE  GCCCMASS-CERT-NBR        (WS-MASS-SUB)                         
                                            TO  GLMASS-CERT.                    
           MOVE  GCCCMASS-EMPL-LAST-NME   (WS-MASS-SUB)                         
                                            TO  GLMASS-MBR-SURNAME.             
           MOVE  GCCCMASS-EMPL-FIRST-NME  (WS-MASS-SUB)                         
                                            TO  GLMASS-MBR-FSTNAME.             
           MOVE  GCCCMASS-EMPL-MID-INIT   (WS-MASS-SUB)                         
                                            TO  GLMASS-MBR-INITS.               
                                                                                
           MOVE  GCCCMASS-EFF-DATE        (WS-MASS-SUB)                         
                                            TO WS-DCPI-DATE.                    
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE               TO  GLMASS-CHDT-DATE.               
                                                                                
           MOVE  GCCCMASS-TERM-REASON     (WS-MASS-SUB)                         
                                            TO  GLMASS-TERM-REASON.             
                                                                                
           MOVE  GCCCMASS-RETURN-DATE     (WS-MASS-SUB)                         
                                            TO WS-DCPI-DATE.                    
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE                                                   
                                            TO  GLMASS-RETURN-DATE.             
                                                                                
           MOVE  GCCCMASS-SALARY-AMT      (WS-MASS-SUB)                         
                                            TO  GLMASS-SALARY-CHG.              
           MOVE  GCCCMASS-SALARY-FREQ     (WS-MASS-SUB)                         
                                            TO  GLMASS-SALARY-FREQ.             
           MOVE  GCCCMASS-WORKING-HOURS   (WS-MASS-SUB)                         
                                            TO  GLMASS-WORKING-HOURS.           
           MOVE  GCCCMASS-OCC             (WS-MASS-SUB)                         
                                            TO  GLMASS-OCCUPATION-CHG.          
           MOVE  GCCCMASS-CLASS-NEW       (WS-MASS-SUB)                         
                                            TO  GLMASS-CLASS-NEW.               
           MOVE  GCCCMASS-CLASS-OLD       (WS-MASS-SUB)                         
                                            TO  GLMASS-CLASS-OLD.               
           MOVE  GCCCMASS-ACCT-CHG        (WS-MASS-SUB)                         
                                            TO  GLMASS-ACCT-CHG.                
           MOVE  GCCCMASS-DIV-NEW         (WS-MASS-SUB)                         
                                            TO  GLMASS-DIV-NEW.                 
           MOVE  GCCCMASS-DIV-OLD         (WS-MASS-SUB)                         
                                            TO  GLMASS-DIV-OLD.                 
                                                                                
                                                                                
           move  ' '                        TO  GLMASS-EVIDENCE-REQD            
                                                GLMASS-MAILED.                  
                                                                                
           IF    GCCCMASS-EVIDENCE        (WS-MASS-SUB) = '0'                   
                 MOVE 'N'                   TO  GLMASS-EVIDENCE-REQD            
             ELSE                                                               
           IF    GCCCMASS-EVIDENCE        (WS-MASS-SUB) = '1'                   
                 MOVE 'Y'                   TO  GLMASS-EVIDENCE-REQD.           
                                                                                
           IF    GCCCMASS-MAILED          (WS-MASS-SUB) = '0'                   
                 MOVE 'N'                   TO  GLMASS-MAILED                   
             ELSE                                                               
           IF    GCCCMASS-MAILED          (WS-MASS-SUB) = '1'                   
                 MOVE 'Y'                   TO  GLMASS-MAILED.                  
                                                                                
                                                                                
           MOVE  GCCCMASS-REGION          TO  WS-TEMP-REG-IN.                   
           PERFORM   7500-REGION-FORMAT  THRU  7500-EXIT.                       
           MOVE  WS-TEMP-REGION           TO  GLMASS-REGION.                    
                                                                                
      *                                                                         
                                                                                
           MOVE  WS-OUT-MASS-LAYOUT     TO WS-OUT-MASS-RECORD.                  
           PERFORM 8500-WRITE-MASS-OUTPUT THRU 8500-EXIT.                       
                                                                                
      * ACCUMULATE TOTAL TO BE DISPLAYED ON PRINT/DISTRIBUTION REPORT           
      * AND TRAILER PAGE OF MASS CHANGE REPORT                                  
                                                                                
           IF GCCCFEXT-LANG  =  'F'                                             
               IF (GLMASS-TERM-REASON = 'T' OR 'R' OR 'P' OR 'M' OR             
                                       'LA' OR 'LB' OR 'LC' OR 'LD'             
                                       OR 'LE')                                 
               OR ( (GCCCMASS-SALARY-AMT (WS-MASS-SUB) = ZERO)                  
                    AND (GCCCMASS-SALARY-FREQ (WS-MASS-SUB)                     
                                              = SPACE OR LOW-VALUES)            
                    AND (GCCCMASS-WORKING-HOURS (WS-MASS-SUB)                   
                                              = ZERO) )                         
                   ADD 1 TO WS-MASS-FR-TERM                                     
               ELSE                                                             
                   ADD 1 TO WS-MASS-FR-SAL                                      
               END-IF                                                           
           ELSE                                                                 
               IF (GLMASS-TERM-REASON = 'T' OR 'R' OR 'P' OR 'M' OR             
                                       'LA' OR 'LB' OR 'LC' OR 'LD'             
                                       OR 'LE')                                 
               OR ( (GCCCMASS-SALARY-AMT (WS-MASS-SUB) = ZERO)                  
                    AND (GCCCMASS-SALARY-FREQ (WS-MASS-SUB)                     
                                              = SPACE OR LOW-VALUES)            
                    AND (GCCCMASS-WORKING-HOURS (WS-MASS-SUB)                   
                                              = ZERO) )                         
                   ADD 1 TO WS-MASS-ENG-TERM                                    
               ELSE                                                             
                   ADD 1 TO WS-MASS-ENG-SAL                                     
               END-IF                                                           
           END-IF.                                                              
                                                                                
                                                                                
                                                                                
       2160-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2170-PROC-MASS-TRAILER.                                                  
      ******************************************************************        
      * MOVE THE ACCUMULATED TOTAL FOR MASS CHANGE BY BANNER PAGE               
      * TO DISPLAY LINES.  THESE LINES WILL BE WRITTEN AS TRAILER               
      * RECORDS ON THE MASS CHANGE FILE TO BE PROCESSED IN GCCPFRMM             
      * AND TO BE DISTRIBUTION REPORT FILE TO BE PROCESSED IN GCCPFRMR          
      * IF THERE WERE NO MASS CHANGE REPORTS FOR THIS SORT CATEGORY             
      *  (WS-MASS-FORM-COUNT = 0)                                               
      * DON'T PRINT THE TRAILER RECORDS FOR THE MASS CHANGE FILE                
      ******************************************************************        
                                                                                
           MOVE 'T'                      TO GLMASS-REC-TYPE.                    
           MOVE WS-MASS-TRAILER-HEADING  TO GLMASS-TRAILER-PRTLINE.             
           MOVE WS-OUT-MASS-LAYOUT       TO WS-OUT-MASS-RECORD.                 
           IF WS-MASS-FORM-COUNT > 0                                            
               PERFORM 8500-WRITE-MASS-OUTPUT                                   
                  THRU 8500-EXIT.                                               
                                                                                
           MOVE WS-MASS-FORM-COUNT       TO WS-MASS-TRAILER-FORM-COUNT          
           MOVE WS-MASS-TRAILER-DET1     TO GLMASS-TRAILER-PRTLINE.             
           MOVE WS-OUT-MASS-LAYOUT       TO WS-OUT-MASS-RECORD.                 
           IF WS-MASS-FORM-COUNT > 0                                            
               PERFORM 8500-WRITE-MASS-OUTPUT                                   
                  THRU 8500-EXIT.                                               
           MOVE WS-MASS-TRAILER-DET1     TO GCCCRPT-TOTAL-LINE.                 
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
      *                                                                         
           MOVE WS-MASS-ENG-TERM         TO WS-MASS-TRAILER-ENG-TERM.           
           MOVE WS-MASS-FR-TERM          TO WS-MASS-TRAILER-FR-TERM.            
           COMPUTE WS-MASS-TOT-TERM      =  WS-MASS-ENG-TERM                    
                                         +  WS-MASS-FR-TERM.                    
           COMPUTE WS-MASS-ENG-TOT       =  WS-MASS-ENG-TERM                    
                                         +  WS-MASS-ENG-SAL.                    
           COMPUTE WS-MASS-FR-TOT        =  WS-MASS-FR-TERM                     
                                         +  WS-MASS-FR-SAL.                     
           MOVE WS-MASS-TOT-TERM         TO WS-MASS-TRAILER-TOT-TERM.           
           MOVE 0                        TO WS-MASS-ENG-TERM                    
                                            WS-MASS-FR-TERM.                    
           MOVE WS-MASS-TRAILER-DET2     TO GLMASS-TRAILER-PRTLINE.             
           MOVE WS-OUT-MASS-LAYOUT       TO WS-OUT-MASS-RECORD.                 
           IF WS-MASS-FORM-COUNT > 0                                            
               PERFORM 8500-WRITE-MASS-OUTPUT                                   
                  THRU 8500-EXIT.                                               
           MOVE WS-MASS-TRAILER-DET2     TO GCCCRPT-TOTAL-LINE.                 
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
      *                                                                         
           MOVE WS-MASS-ENG-SAL          TO WS-MASS-TRAILER-ENG-SAL.            
           MOVE WS-MASS-FR-SAL           TO WS-MASS-TRAILER-FR-SAL.             
           COMPUTE WS-MASS-TOT-SAL       =  WS-MASS-ENG-SAL                     
                                         +  WS-MASS-FR-SAL.                     
           MOVE WS-MASS-TOT-SAL          TO WS-MASS-TRAILER-TOT-SAL.            
           MOVE 0                        TO WS-MASS-ENG-SAL                     
                                            WS-MASS-FR-SAL.                     
           MOVE WS-MASS-TRAILER-DET3     TO GLMASS-TRAILER-PRTLINE.             
           MOVE WS-OUT-MASS-LAYOUT       TO WS-OUT-MASS-RECORD.                 
           IF WS-MASS-FORM-COUNT > 0                                            
               PERFORM 8500-WRITE-MASS-OUTPUT                                   
                  THRU 8500-EXIT.                                               
           MOVE WS-MASS-TRAILER-DET3     TO GCCCRPT-TOTAL-LINE.                 
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
      *                                                                         
           MOVE WS-MASS-ENG-TOT          TO WS-MASS-TRAILER-ENG-TOT.            
           MOVE WS-MASS-FR-TOT           TO WS-MASS-TRAILER-FR-TOT.             
           COMPUTE WS-MASS-TOT-TOT       =  WS-MASS-ENG-TOT                     
                                         +  WS-MASS-FR-TOT.                     
           MOVE WS-MASS-TOT-TOT          TO WS-MASS-TRAILER-TOT-TOT.            
           MOVE 0                        TO WS-MASS-ENG-TOT                     
                                            WS-MASS-FR-TOT.                     
           MOVE WS-MASS-TRAILER-DET4     TO GLMASS-TRAILER-PRTLINE.             
           MOVE WS-OUT-MASS-LAYOUT       TO WS-OUT-MASS-RECORD.                 
           IF WS-MASS-FORM-COUNT > 0                                            
               PERFORM 8500-WRITE-MASS-OUTPUT                                   
                  THRU 8500-EXIT.                                               
           MOVE WS-MASS-TRAILER-DET4     TO GCCCRPT-TOTAL-LINE.                 
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
           MOVE 0                        TO WS-MASS-FORM-COUNT.                 
                                                                                
       2170-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2700-PRINT-BANNER.                                                       
      ******************************************************************        
      * PRODUCE THE BANNER PAGE                                                 
      ******************************************************************        
      *                                                                         
      * DON'T PRINT THE BANNER PAGE IF THE FORM BEING PROCESSED IS A            
      * MASS CHANGE (MASS CHANGE ARE NOT PRINTED IN THE AFP FORMS)              
                                                                                
           IF GCCCFEXT-FORM-NBR       =   'GLMASS  '                            
               GO TO 2700-EXIT.                                                 
                                                                                
           MOVE  SPACES               TO  WS-BANNER-LINES.                      
                                                                                
           MOVE  WS-BANNER-DIST       TO  WS-BANNER-LINE (1).                   
                                                                                
           IF  GCCCFEXT-SORT-CAT2    =   'E'                                    
               MOVE  'EAST'           TO  WS-IN-FORCE-TEAM                      
             ELSE                                                               
           IF  GCCCFEXT-SORT-CAT2    =   'W'                                    
               MOVE  'WEST'           TO  WS-IN-FORCE-TEAM                      
             ELSE                                                               
           IF  GCCCFEXT-SORT-CAT2    =   'G'                                    
               MOVE  'GFM '           TO  WS-IN-FORCE-TEAM                      
             ELSE                                                               
           IF  GCCCFEXT-SORT-CAT2    =   'A'                                    
               MOVE WS-DEL-STATION-KC10                                         
                                      TO  WS-PENDING-DSTN                       
               MOVE  'ALPHA'          TO  WS-PENDING-DIST                       
                                          WS-MONTREAL-DIST                      
             ELSE                                                               
           IF  GCCCFEXT-SORT-CAT2    =   'S'                                    
               MOVE WS-DEL-STATION-KC10                                         
                                      TO  WS-PENDING-DSTN                       
               MOVE  'SIGNATURE'      TO  WS-PENDING-DIST                       
                                          WS-MONTREAL-DIST                      
             ELSE                                                               
           IF  GCCCFEXT-SORT-CAT2    =   'C'                                    
 JE001         MOVE WS-DEL-STATION-GBA                                          
 JE001                                TO  WS-PENDING-DSTN                       
               MOVE  'CORP ACCTS'     TO  WS-PENDING-DIST                       
                                          WS-MONTREAL-DIST                      
             ELSE                                                               
               MOVE  SPACES           TO  WS-IN-FORCE-TEAM                      
                                          WS-PENDING-DIST                       
                                          WS-MONTREAL-DIST.                     
                                                                                
                                                                                
           IF  GCCCFEXT-SORT-CAT1 = 'M'                                         
               MOVE  WS-MONTREAL-DIST-LINE  TO  WS-BANNER-LINE (2)              
               MOVE  SPACES                 TO  WS-BANNER-LINE (3)              
               MOVE  WS-MONTREAL-DSTN-LINE  TO  WS-BANNER-LINE (4)              
               MOVE  WS-MONTREAL-DUMMY      TO  WS-BANNER-LINE (5)              
             ELSE                                                               
           IF  GCCCFEXT-CONT-STATUS = 'I' OR 'T'                                
               MOVE  WS-IN-FORCE-DIST-LINE TO  WS-BANNER-LINE (2)               
               MOVE  GCCCFEXT-SORT-CAT0    TO  WS-IN-FORCE-TAT                  
               MOVE  WS-IN-FORCE-TAT-LINE  TO  WS-BANNER-LINE (3)               
               MOVE  WS-IN-FORCE-DSTN-LINE TO  WS-BANNER-LINE (4)               
               MOVE  WS-IN-FORCE-TEAM-LINE TO  WS-BANNER-LINE (5)               
             ELSE                                                               
           IF  GCCCFEXT-CONT-STATUS = 'P'                                       
               MOVE  WS-PENDING-DIST-LINE  TO  WS-BANNER-LINE (2)               
               MOVE  SPACES                TO  WS-BANNER-LINE (3)               
               MOVE  WS-PENDING-DSTN-LINE  TO  WS-BANNER-LINE (4)               
               MOVE  WS-PENDING-DUMMY      TO  WS-BANNER-LINE (5).              
                                                                                
      * SAVE HEADINGS TO BE USED ON THE TRAILER PAGE                            
           MOVE WS-BANNER-LINE (2) TO  WS-BANNER-TOT-DET3.                      
           MOVE WS-BANNER-LINE (3) TO  WS-BANNER-TOT-DET4.                      
      * PRINT TEAM ON TRAILER PAGE FOR INFORCE OR TERMINATED GROUPS             
      * (PLAN MEMBER ADMIN)                                                     
           IF GCCCFEXT-CONT-STATUS = 'I' OR 'T'                                 
               MOVE WS-BANNER-LINE (5) TO WS-BANNER-TOT-DET5                    
           ELSE                                                                 
               MOVE SPACES             TO WS-BANNER-TOT-DET5.                   
                                                                                
           MOVE WS-BANNER-LINES    TO  GLBANR-DETAIL.                           
           MOVE '1'                TO  GLBANR-CC.                               
           MOVE '00'               TO  GLBANR-FORM-TYPE.                        
                                                                                
           MOVE  GLBANR-RECORD          TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
           SET  WS-BANNER-PRINTED  TO  TRUE.                                    
                                                                                
       2700-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2800-PRINT-FORMS.                                                        
      ******************************************************************        
      * PRINT EACH OF THE FORMS                                                 
      ******************************************************************        
      *                                                                         
                                                                                
           IF GCCCFEXT-FORM-NBR  =  'GLMASS  '                                  
               PERFORM 2100-PROCESS-MASS                                        
                  THRU 2100-EXIT                                                
                                                                                
           ELSE                                                                 
               PERFORM 3000-FORMAT-FORMS                                        
                  THRU 3000-EXIT                                                
                                                                                
               PERFORM 6000-GET-NEXT                                            
                  THRU 6000-EXIT                                                
           END-IF.                                                              
                                                                                
       2800-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2900-PRINT-TOTALS.                                                       
      ******************************************************************        
      * PRODUCE THE TOTALS BANNER                                               
      ******************************************************************        
      *                                                                         
      * TOTALS FOR EACH FORM WILL BE WRITTEN ON THE TRAILER                     
      * PAGE.  THESE LINES WILL ALSO BE WRITTEN TO THE EXTRACT FILE             
      * FOR INCLUSION IN THE PRINT DISTRIBUTION REPORT PRODUCED BY              
      * PROGRAM GCCPFRMR                                                        
      ******************************************************************        
                                                                                
           MOVE  SPACES             TO  WS-BANNER-LINES.                        
                                                                                
           MOVE  WS-AFP-COUNT       TO  WS-BANNER-AFP-COUNT.                    
                                                                                
           MOVE  0                  TO  WS-AFP-COUNT.                           
                                                                                
           MOVE  SPACES             TO  GLBANR-DETAIL.                          
           MOVE  WS-BANNER-TOT-DET1 TO  WS-BANNER-LINE (1).                     
           MOVE  WS-BANNER-TOT-DET2 TO  WS-BANNER-LINE (2).                     
           MOVE  WS-BANNER-TOT-DET3 TO  WS-BANNER-LINE (3).                     
           MOVE  WS-BANNER-TOT-DET4 TO  WS-BANNER-LINE (4).                     
           MOVE  WS-BANNER-TOT-DET5 TO  WS-BANNER-LINE (5).                     
                                                                                
           MOVE  WS-BANNER-TOT-HEADING                                          
                                    TO  WS-BANNER-LINE (6).                     
                                                                                
           SET   GCCCRPT-TOTAL-REC  TO  TRUE.                                   
           MOVE  WS-BANNER-TOT-HEADING                                          
                                    TO  GCCCRPT-TOTAL-LINE.                     
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
           MOVE  'GL0003'           TO  WS-BANNER-TOT-FORM.                     
           MOVE  WS-BANNER-GL0003-NAME                                          
                                    TO  WS-BANNER-TOT-DESC.                     
           MOVE  WS-BANNER-GL0003-ENG-TOT                                       
                                    TO  WS-BANNER-TOT-ENG.                      
           MOVE  WS-BANNER-GL0003-FR-TOT                                        
                                    TO  WS-BANNER-TOT-FR.                       
           COMPUTE WS-BANNER-LINE-TOT   =  WS-BANNER-GL0003-ENG-TOT             
                                        +  WS-BANNER-GL0003-FR-TOT.             
           MOVE  WS-BANNER-LINE-TOT TO  WS-BANNER-TOT-TOT.                      
           COMPUTE WS-BANNER-TOTAL-ENG-TOT  =                                   
                                            WS-BANNER-TOTAL-ENG-TOT             
                                        +   WS-BANNER-GL0003-ENG-TOT.           
           COMPUTE WS-BANNER-TOTAL-FR-TOT  =                                    
                                           WS-BANNER-TOTAL-FR-TOT               
                                        +  WS-BANNER-GL0003-FR-TOT.             
           MOVE 0                    TO  WS-BANNER-GL0003-ENG-TOT               
                                         WS-BANNER-GL0003-FR-TOT.               
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  WS-BANNER-LINE (7).                     
                                                                                
                                                                                
           SET   GCCCRPT-TOTAL-REC  TO  TRUE.                                   
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  GCCCRPT-TOTAL-LINE.                     
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
           MOVE  'GL0005'           TO  WS-BANNER-TOT-FORM.                     
           MOVE  WS-BANNER-GL0005-NAME                                          
                                    TO  WS-BANNER-TOT-DESC.                     
           MOVE  WS-BANNER-GL0005-ENG-TOT                                       
                                    TO  WS-BANNER-TOT-ENG.                      
           MOVE  WS-BANNER-GL0005-FR-TOT                                        
                                    TO  WS-BANNER-TOT-FR.                       
           COMPUTE WS-BANNER-LINE-TOT   =  WS-BANNER-GL0005-ENG-TOT             
                                        +  WS-BANNER-GL0005-FR-TOT.             
           MOVE  WS-BANNER-LINE-TOT TO  WS-BANNER-TOT-TOT.                      
           COMPUTE WS-BANNER-TOTAL-ENG-TOT =                                    
                                           WS-BANNER-TOTAL-ENG-TOT              
                                        +  WS-BANNER-GL0005-ENG-TOT.            
           COMPUTE WS-BANNER-TOTAL-FR-TOT       =                               
                                           WS-BANNER-TOTAL-FR-TOT               
                                        +  WS-BANNER-GL0005-FR-TOT.             
           MOVE 0                    TO  WS-BANNER-GL0005-ENG-TOT               
                                         WS-BANNER-GL0005-FR-TOT.               
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  WS-BANNER-LINE (8).                     
                                                                                
           SET   GCCCRPT-TOTAL-REC  TO  TRUE.                                   
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  GCCCRPT-TOTAL-LINE.                     
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
           MOVE  'GL0514'           TO  WS-BANNER-TOT-FORM.                     
           MOVE  WS-BANNER-GL0514-NAME                                          
                                    TO  WS-BANNER-TOT-DESC.                     
           MOVE  WS-BANNER-GL0514-ENG-TOT                                       
                                    TO  WS-BANNER-TOT-ENG.                      
           MOVE  WS-BANNER-GL0514-FR-TOT                                        
                                    TO  WS-BANNER-TOT-FR.                       
           COMPUTE WS-BANNER-LINE-TOT   =  WS-BANNER-GL0514-ENG-TOT             
                                        +  WS-BANNER-GL0514-FR-TOT.             
           MOVE  WS-BANNER-LINE-TOT TO  WS-BANNER-TOT-TOT.                      
           COMPUTE WS-BANNER-TOTAL-ENG-TOT =                                    
                                           WS-BANNER-TOTAL-ENG-TOT              
                                        +  WS-BANNER-GL0514-ENG-TOT.            
           COMPUTE WS-BANNER-TOTAL-FR-TOT       =                               
                                           WS-BANNER-TOTAL-FR-TOT               
                                        +  WS-BANNER-GL0514-FR-TOT.             
           MOVE 0                    TO  WS-BANNER-GL0514-ENG-TOT               
                                         WS-BANNER-GL0514-FR-TOT.               
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  WS-BANNER-LINE (9).                     
                                                                                
           SET   GCCCRPT-TOTAL-REC  TO  TRUE.                                   
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  GCCCRPT-TOTAL-LINE.                     
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
           MOVE  'GL2971'           TO  WS-BANNER-TOT-FORM.                     
           MOVE  WS-BANNER-GL2971-NAME                                          
                                    TO  WS-BANNER-TOT-DESC.                     
           MOVE  WS-BANNER-GL2971-ENG-TOT                                       
                                    TO  WS-BANNER-TOT-ENG.                      
           MOVE  WS-BANNER-GL2971-FR-TOT                                        
                                    TO  WS-BANNER-TOT-FR.                       
           COMPUTE WS-BANNER-LINE-TOT   =  WS-BANNER-GL2971-ENG-TOT             
                                        +  WS-BANNER-GL2971-FR-TOT.             
           MOVE  WS-BANNER-LINE-TOT TO  WS-BANNER-TOT-TOT.                      
           COMPUTE WS-BANNER-TOTAL-ENG-TOT =                                    
                                           WS-BANNER-TOTAL-ENG-TOT              
                                        +  WS-BANNER-GL2971-ENG-TOT.            
           COMPUTE WS-BANNER-TOTAL-FR-TOT       =                               
                                           WS-BANNER-TOTAL-FR-TOT               
                                        +  WS-BANNER-GL2971-FR-TOT.             
           MOVE 0                    TO  WS-BANNER-GL2971-ENG-TOT               
                                         WS-BANNER-GL2971-FR-TOT.               
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  WS-BANNER-LINE (10).                    
                                                                                
           SET   GCCCRPT-TOTAL-REC  TO  TRUE.                                   
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  GCCCRPT-TOTAL-LINE.                     
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
           MOVE  'GL3187'           TO  WS-BANNER-TOT-FORM.                     
           MOVE  WS-BANNER-GL3187-NAME                                          
                                    TO  WS-BANNER-TOT-DESC.                     
           MOVE  WS-BANNER-GL3187-ENG-TOT                                       
                                    TO  WS-BANNER-TOT-ENG.                      
           MOVE  WS-BANNER-GL3187-FR-TOT                                        
                                    TO  WS-BANNER-TOT-FR.                       
           COMPUTE WS-BANNER-LINE-TOT   =  WS-BANNER-GL3187-ENG-TOT             
                                        +  WS-BANNER-GL3187-FR-TOT.             
           MOVE  WS-BANNER-LINE-TOT TO  WS-BANNER-TOT-TOT.                      
           COMPUTE WS-BANNER-TOTAL-ENG-TOT =                                    
                                           WS-BANNER-TOTAL-ENG-TOT              
                                        +  WS-BANNER-GL3187-ENG-TOT.            
           COMPUTE WS-BANNER-TOTAL-FR-TOT  =                                    
                                           WS-BANNER-TOTAL-FR-TOT               
                                        +  WS-BANNER-GL3187-FR-TOT.             
           MOVE 0                    TO  WS-BANNER-GL3187-ENG-TOT               
                                         WS-BANNER-GL3187-FR-TOT.               
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  WS-BANNER-LINE (11).                    
                                                                                
           SET   GCCCRPT-TOTAL-REC  TO  TRUE.                                   
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  GCCCRPT-TOTAL-LINE.                     
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
           MOVE  'GL3574'           TO  WS-BANNER-TOT-FORM.                     
           MOVE  WS-BANNER-GL3574-NAME                                          
                                    TO  WS-BANNER-TOT-DESC.                     
           MOVE  WS-BANNER-GL3574-ENG-TOT                                       
                                    TO  WS-BANNER-TOT-ENG.                      
           MOVE  WS-BANNER-GL3574-FR-TOT                                        
                                    TO  WS-BANNER-TOT-FR.                       
           COMPUTE WS-BANNER-LINE-TOT   =  WS-BANNER-GL3574-ENG-TOT             
                                        +  WS-BANNER-GL3574-FR-TOT.             
           MOVE  WS-BANNER-LINE-TOT TO  WS-BANNER-TOT-TOT.                      
           COMPUTE WS-BANNER-TOTAL-ENG-TOT =                                    
                                           WS-BANNER-TOTAL-ENG-TOT              
                                        +  WS-BANNER-GL3574-ENG-TOT.            
           COMPUTE WS-BANNER-TOTAL-FR-TOT  =                                    
                                           WS-BANNER-TOTAL-FR-TOT               
                                        +  WS-BANNER-GL3574-FR-TOT.             
           MOVE 0                    TO  WS-BANNER-GL3574-ENG-TOT               
                                         WS-BANNER-GL3574-FR-TOT.               
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  WS-BANNER-LINE (12).                    
                                                                                
           SET   GCCCRPT-TOTAL-REC  TO  TRUE.                                   
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  GCCCRPT-TOTAL-LINE.                     
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
           MOVE  'TOTALS'           TO  WS-BANNER-TOT-FORM.                     
           MOVE  SPACES             TO  WS-BANNER-TOT-DESC.                     
           MOVE  WS-BANNER-TOTAL-ENG-TOT                                        
                                    TO  WS-BANNER-TOT-ENG.                      
           MOVE  WS-BANNER-TOTAL-FR-TOT                                         
                                    TO  WS-BANNER-TOT-FR.                       
           COMPUTE WS-BANNER-LINE-TOT   =  WS-BANNER-TOTAL-ENG-TOT              
                                        +  WS-BANNER-TOTAL-FR-TOT.              
           MOVE 0                   TO  WS-BANNER-TOTAL-ENG-TOT                 
                                        WS-BANNER-TOTAL-FR-TOT.                 
           MOVE  WS-BANNER-LINE-TOT TO  WS-BANNER-TOT-TOT.                      
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  WS-BANNER-LINE (13).                    
                                                                                
           SET   GCCCRPT-TOTAL-REC  TO  TRUE.                                   
           MOVE  WS-BANNER-TOT-DETAIL-LINE                                      
                                    TO  GCCCRPT-TOTAL-LINE.                     
           PERFORM 8750-WRITE-EXT-OUTPUT                                        
              THRU 8750-EXIT.                                                   
                                                                                
           PERFORM 2170-PROC-MASS-TRAILER                                       
              THRU 2170-EXIT.                                                   
                                                                                
                                                                                
           IF WS-BANNER-PRINTED                                                 
               MOVE WS-BANNER-LINES     TO  GLBANR-DETAIL                       
               MOVE '1'                 TO  GLBANR-CC                           
               MOVE '00'                TO  GLBANR-FORM-TYPE                    
                                                                                
               MOVE  GLBANR-RECORD      TO WS-OUT-AFP-RECORD                    
                                                                                
               PERFORM 8000-WRITE-AFP-OUTPUT                                    
                  THRU 8000-EXIT                                                
           END-IF.                                                              
                                                                                
      * RESET SWITCH CONTROLING BANNER PRINTING                                 
                                                                                
           SET WS-BANNER-NOT-PRINTED TO  TRUE.                                  
                                                                                
                                                                                
                                                                                
       2900-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       3000-FORMAT-FORMS.                                                       
      ******************************************************************        
      * PRODUCE THE GL0003 FORMAT                                               
      ******************************************************************        
      *                                                                         
      *    INITIALIZE WS-OUTPUT-AFP-LAYOUT.                                     
      *                                                                         
           IF WS-BANNER-NOT-PRINTED                                             
               PERFORM 2700-PRINT-BANNER                                        
                  THRU 2700-EXIT                                                
           END-IF.                                                              
                                                                                
           ADD   1                      TO WS-AFP-COUNT.                        
                                                                                
           IF  GCCCFEXT-FORM-NBR   =  'GL0003  '                                
               PERFORM  3050-FORMAT-GL0003  THRU 3050-EXIT                      
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR   =  'GL0005  '                                
               PERFORM  3100-FORMAT-GL0005  THRU 3100-EXIT                      
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR   =  'GL0514  '                                
               PERFORM  3200-FORMAT-GL0514  THRU 3200-EXIT                      
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR   =  'GL2971  '                                
               PERFORM  3300-FORMAT-GL2971  THRU 3300-EXIT                      
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR   =  'GL3187  '                                
               PERFORM  3600-FORMAT-GL3187  THRU 3600-EXIT                      
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR   =  'GL3574  '                                
               PERFORM  3900-FORMAT-GL3574  THRU 3900-EXIT                      
             ELSE                                                               
               DISPLAY 'INVALID REPORT TYPE ' GCCCFEXT-FORM-NBR                 
               DISPLAY '!' INP-RECORD                                           
               DISPLAY WS-OUTPUT-AFP-LAYOUT                                     
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
           PERFORM  4000-CREATE-EXTRACT       THRU 4000-EXIT.                   
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3050-FORMAT-GL0003.                                                      
      ******************************************************************        
      * PRODUCE THE GL0003 FORMAT                                               
      ******************************************************************        
      *                                                                         
           INITIALIZE GL0003-RECORD.                                            
      *                                                                         
           MOVE  GCCCFEXT-LANG          TO WS-DCP-LANG.                         
           MOVE  '1'                    TO GL0003-CC.                           
      *                                                                         
           IF  GCCCFEXT-LANG   =  'F'                                           
               ADD 1 TO WS-BANNER-GL0003-FR-TOT                                 
               MOVE  '53'               TO GL0003-FORM-TYPE                     
             ELSE                                                               
               ADD 1 TO WS-BANNER-GL0003-ENG-TOT                                
               MOVE  '03'               TO GL0003-FORM-TYPE.                    
                                                                                
                                                                                
           MOVE  GCCC0003-TIMESTAMP     TO GL0003-DATE-TIME.                    
                                                                                
           MOVE  GCCC0003-PLAN-NBR     (1) TO GL0003-PLAN (1).                  
           MOVE  GCCC0003-PLAN-NBR     (2) TO GL0003-PLAN (2).                  
           MOVE  GCCC0003-PLAN-NBR     (3) TO GL0003-PLAN (3).                  
           MOVE  GCCC0003-PLAN-NBR     (4) TO GL0003-PLAN (4).                  
           MOVE  GCCC0003-PLAN-NBR     (5) TO GL0003-PLAN (5).                  
           MOVE  GCCC0003-ACCOUNT-NBR  (1) TO GL0003-ACCOUNT (1).               
           MOVE  GCCC0003-ACCOUNT-NBR  (2) TO GL0003-ACCOUNT (2).               
           MOVE  GCCC0003-ACCOUNT-NBR  (3) TO GL0003-ACCOUNT (3).               
           MOVE  GCCC0003-ACCOUNT-NBR  (4) TO GL0003-ACCOUNT (4).               
           MOVE  GCCC0003-ACCOUNT-NBR  (5) TO GL0003-ACCOUNT (5).               
           MOVE  GCCC0003-DIVISION-NBR (1) TO GL0003-DIV     (1).               
           MOVE  GCCC0003-DIVISION-NBR (2) TO GL0003-DIV     (2).               
           MOVE  GCCC0003-DIVISION-NBR (3) TO GL0003-DIV     (3).               
           MOVE  GCCC0003-DIVISION-NBR (4) TO GL0003-DIV     (4).               
           MOVE  GCCC0003-DIVISION-NBR (5) TO GL0003-DIV     (5).               
                                                                                
           MOVE  GCCC0003-CERT-NBR      TO GL0003-CERT.                         
           MOVE  GCCC0003-PLAN-SPONSOR-NAME                                     
                                        TO GL0003-SPONSOR.                      
           MOVE  GCCC0003-EMPLOYER      TO GL0003-EMPLOYER.                     
                                                                                
           MOVE  GCCC0003-NAME-1        TO GL0003-MEMBER-NAME.                  
                                                                                
           MOVE  GCCC0003-COMMENTS      TO GL0003-COMMENTS.                     
                                                                                
           MOVE  GCCC0003-SUBMIT-DATE   TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0003-ENGLISH-TODAY.                
                                                                                
      *    THE Q IN THE FOLLOWING CODE WAS INSERTED                             
                                                                                
                                                                                
           MOVE  GCCC0003-REGION          TO  WS-TEMP-REG-IN.                   
           PERFORM   7500-REGION-FORMAT  THRU  7500-EXIT.                       
           MOVE  WS-TEMP-REGION           TO  GL0003-REGION.                    
                                                                                
                                                                                
           MOVE  GL0003-RECORD          TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
       3050-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3100-FORMAT-GL0005.                                                      
      ******************************************************************        
      * PRODUCE THE GL0005 FORMAT                                               
      ******************************************************************        
      *                                                                         
           INITIALIZE GL0005-RECORD-P1                                          
                      GL0005-RECORD-P2.                                         
      *                                                                         
           MOVE  GCCCFEXT-LANG          TO WS-DCP-LANG.                         
           MOVE  '1'                    TO GL0005-CC-P1.                        
      *                                                                         
           IF  GCCCFEXT-LANG   =  'F'                                           
               ADD 1 TO WS-BANNER-GL0005-FR-TOT                                 
               MOVE  '55'               TO GL0005-FORM-TYPE-P1                  
                                           GL0005-FORM-TYPE-P2                  
             ELSE                                                               
               ADD 1 TO WS-BANNER-GL0005-ENG-TOT                                
               MOVE  '05'               TO GL0005-FORM-TYPE-P1                  
                                           GL0005-FORM-TYPE-P2.                 
                                                                                
           MOVE  GCCC0005-TIMESTAMP     TO GL0005-DATE-TIME.                    
                                                                                
           IF    GCCC0005-COVERAGE  =  1                                        
                 MOVE 'X'               TO GL0005-COV-MMBR                      
              ELSE                                                              
           IF    GCCC0005-COVERAGE  =  2                                        
                 MOVE 'X'               TO GL0005-COV-MMBR-SPS                  
              ELSE                                                              
           IF    GCCC0005-COVERAGE  =  3                                        
                 MOVE 'X'               TO GL0005-COV-MMBR-DEP                  
              ELSE                                                              
           IF    GCCC0005-COVERAGE  =  4                                        
                 MOVE 'X'               TO GL0005-COV-MBR-SPS-DEP.              
                                                                                
           MOVE  GCCC0005-PLAN-NBR (1)  TO GL0005-PLAN (1).                     
           MOVE  GCCC0005-PLAN-NBR (2)  TO GL0005-PLAN (2).                     
           MOVE  GCCC0005-PLAN-NBR (3)  TO GL0005-PLAN (3).                     
           MOVE  GCCC0005-ACCOUNT-NBR (1) TO GL0005-ACCOUNT (1).                
           MOVE  GCCC0005-ACCOUNT-NBR (2) TO GL0005-ACCOUNT (2).                
           MOVE  GCCC0005-ACCOUNT-NBR (3) TO GL0005-ACCOUNT (3).                
           MOVE  GCCC0005-DIV         (1) TO GL0005-DIVISION (1).               
           MOVE  GCCC0005-DIV         (2) TO GL0005-DIVISION (2).               
           MOVE  GCCC0005-DIV         (3) TO GL0005-DIVISION (3).               
           MOVE  GCCC0005-CLASS       (1) TO GL0005-CLASS   (1).                
           MOVE  GCCC0005-CLASS       (2) TO GL0005-CLASS   (2).                
           MOVE  GCCC0005-CLASS       (3) TO GL0005-CLASS   (3).                
           MOVE  GCCC0005-CERT-NBR      TO GL0005-CERT.                         
           MOVE  GCCC0005-EARNINGS      TO GL0005-EARNINGS.                     
           MOVE  GCCC0005-EMPLOYER      TO GL0005-SPONSOR.                      
                                                                                
           MOVE  GCCC0005-ELIG-DATE     TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0005-ELIG.                         
                                                                                
                                                                                
           IF GCCC0005-MAILED         = '0'                                     
                 MOVE  'X'              TO GL0005-MAILED-NO                     
            ELSE                                                                
           IF GCCC0005-MAILED         = '1'                                     
                 MOVE  'X'              TO GL0005-MAILED-YES.                   
                                                                                
           MOVE  GCCC0005-MBR-NAME      TO GL0005-MBR-NAME.                     
                                                                                
           MOVE  GCCC0005-MBR-DOB       TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0005-MBR-DOB.                      
                                                                                
           IF  GCCC0005-MBR-LANG      =  '2'                                    
               MOVE 'X'                 TO GL0005-LANG-FRC                      
             ELSE                                                               
               MOVE 'X'                 TO GL0005-LANG-ENG.                     
                                                                                
           IF  GCCC0005-MBR-GENDER    =  '0'                                    
               MOVE 'X'                 TO GL0005-MALE                          
             ELSE                                                               
           IF  GCCC0005-MBR-GENDER    =  '1'                                    
               MOVE 'X'                 TO GL0005-FEMALE.                       
                                                                                
           MOVE  GCCC0005-MBR-PROV    TO   GL0005-PROV.                         
           IF    GCCC0005-MBR-SMOKER  =   '1'                                   
              MOVE 'X'                TO   GL0005-SMOKE-YES                     
            ELSE                                                                
           IF    GCCC0005-MBR-SMOKER  =   '0'                                   
              MOVE 'X'                TO   GL0005-SMOKE-NO.                     
                                                                                
           MOVE  GCCC0005-OL-CURR-AMT     TO GL0005-OL-CURR-AMT.                
           MOVE  GCCC0005-OL-XSAL-AMT     TO GL0005-OL-XSAL-AMT.                
           MOVE  GCCC0005-OL-TOTAL-AMT    TO GL0005-OL-TOTAL-AMT.               
                                                                                
           MOVE  GCCC0005-ADTL-CURR-AMT   TO GL0005-ADTL-CURR-AMT.              
           MOVE  GCCC0005-ADTL-XSAL-AMT   TO GL0005-ADTL-XSAL-AMT.              
           MOVE  GCCC0005-ADTL-TOTAL-AMT  TO GL0005-ADTL-TOTAL-AMT.             
                                                                                
           MOVE  GCCC0005-TOT-CURR-AMT    TO GL0005-TOT-CURR-AMT.               
           MOVE  GCCC0005-TOT-XSAL-AMT    TO GL0005-TOT-XSAL-AMT.               
           MOVE  GCCC0005-TOT-TOTAL-AMT   TO GL0005-TOT-TOTAL-AMT.              
                                                                                
           MOVE  GCCC0005-ADD-CURR-AMT    TO GL0005-ADD-CURR-AMT.               
           MOVE  GCCC0005-ADD-XSAL-AMT    TO GL0005-ADD-XSAL-AMT.               
           MOVE  GCCC0005-ADD-TOTAL-AMT   TO GL0005-ADD-TOTAL-AMT.              
                                                                                
           IF    GCCC0005-BENEFICIARY =  '1'                                    
               MOVE 'X'                   TO GL0005-BENEFICIARY.                
                                                                                
           MOVE  GCCC0005-SPS-NAME        TO GL0005-SPOUSE-NAME.                
                                                                                
           IF  GCCC0005-SPS-GENDER    =  '0'                                    
               MOVE 'X'                 TO GL0005-SPOUSE-MALE                   
             ELSE                                                               
           IF  GCCC0005-SPS-GENDER    =  '1'                                    
               MOVE 'X'                 TO GL0005-SPOUSE-FEMALE.                
                                                                                
           MOVE  GCCC0005-SPOUSE-DOB    TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0005-SPOUSE-DOB.                   
                                                                                
           IF    GCCC0005-SPS-SMOKER  =   '1'                                   
              MOVE 'X'                TO   GL0005-SPOUSE-SMOKE-YES              
            ELSE                                                                
           IF    GCCC0005-SPS-SMOKER  =   '0'                                   
              MOVE 'X'                TO   GL0005-SPOUSE-SMOKE-NO.              
                                                                                
                                                                                
           MOVE  GCCC0005-SPS-OL-CURR-AMT                                       
                                        TO GL0005-SPS-OL-CURR-AMT.              
           MOVE  GCCC0005-SPS-OL-XSAL-AMT                                       
                                        TO GL0005-SPS-OL-XSAL-AMT.              
           MOVE  GCCC0005-SPS-OL-TOTAL-AMT                                      
                                        TO GL0005-SPS-OL-TOTAL-AMT.             
                                                                                
           MOVE  GCCC0005-SPS-ADTL-CURR-AMT                                     
                                        TO GL0005-SPS-ADTL-CURR-AMT.            
           MOVE  GCCC0005-SPS-ADTL-XSAL-AMT                                     
                                        TO GL0005-SPS-ADTL-XSAL-AMT.            
           MOVE  GCCC0005-SPS-ADTL-TOTAL-AMT                                    
                                        TO GL0005-SPS-ADTL-TOTAL-AMT.           
                                                                                
           MOVE  GCCC0005-SPS-TOT-CURR-AMT                                      
                                        TO GL0005-SPS-TOT-CURR-AMT.             
           MOVE  GCCC0005-SPS-TOT-XSAL-AMT                                      
                                        TO GL0005-SPS-TOT-XSAL-AMT.             
           MOVE  GCCC0005-SPS-TOT-TOTAL-AMT                                     
                                        TO GL0005-SPS-TOT-TOTAL-AMT.            
                                                                                
           MOVE  GCCC0005-SPS-ADD-CURR-AMT                                      
                                        TO GL0005-SPS-ADD-CURR-AMT.             
           MOVE  GCCC0005-SPS-ADD-ADTL-AMT                                      
                                        TO GL0005-SPS-ADD-ADTL-AMT.             
           MOVE  GCCC0005-SPS-ADD-TOTAL-AMT                                     
                                        TO GL0005-SPS-ADD-TOTAL-AMT.            
                                                                                
           PERFORM 3150-FORMAT-DEP-GL0005 THRU 3150-EXIT                        
             VARYING WS-CHILD-SUBSCRIPT                                         
                 FROM  1  BY  1                                                 
                   UNTIL WS-CHILD-SUBSCRIPT > 5.                                
                                                                                
                                                                                
           MOVE  GCCC0005-DEP-OL-CURR-AMT                                       
                                        TO GL0005-DEP-OL-CURR-AMT.              
           MOVE  GCCC0005-DEP-OL-OL-ADTL-AMT                                    
                                        TO GL0005-DEP-OL-ADTL-AMT.              
           MOVE  GCCC0005-DEP-OL-OL-TOTAL-AMT                                   
                                        TO GL0005-DEP-OL-TOTAL-AMT.             
                                                                                
           MOVE  GCCC0005-DEP-ADD-CURR-AMT                                      
                                        TO GL0005-DEP-ADD-CURR-AMT.             
           MOVE  GCCC0005-DEP-ADD-ADTL-AMT                                      
                                        TO GL0005-DEP-ADD-ADTL-AMT.             
           MOVE  GCCC0005-DEP-ADD-TOTAL-AMT                                     
                                        TO GL0005-DEP-ADD-TOTAL-AMT.            
                                                                                
                                                                                
           MOVE  GCCC0005-SUBMIT-DATE   TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0005-ENGLISH-TODAY.                
                                                                                
      *    THE Q IN THE FOLLOWING CODE WAS INSERTED                             
                                                                                
                                                                                
           MOVE  GCCC0005-REGION          TO  WS-TEMP-REG-IN.                   
           PERFORM   7500-REGION-FORMAT  THRU  7500-EXIT.                       
           MOVE  WS-TEMP-REGION           TO  GL0005-REGION-P1                  
                                              GL0005-REGION-P2.                 
                                                                                
                                                                                
           MOVE  GL0005-RECORD-P1       TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
           MOVE  GL0005-RECORD-P2       TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
       3100-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       3150-FORMAT-DEP-GL0005.                                                  
      ******************************************************************        
      * PRODUCE THE GL0005 FORMAT FOR DEPENDENT                                 
      ******************************************************************        
      *                                                                         
                                                                                
                                                                                
                                                                                
                                                                                
           MOVE  GCCC0005-DEP-NAME (WS-CHILD-SUBSCRIPT)                         
                           TO GL0005-DEP-NAME  (WS-CHILD-SUBSCRIPT).            
                                                                                
           IF  GCCC0005-DEP-GENDER (WS-CHILD-SUBSCRIPT) = '0'                   
               MOVE 'X' TO GL0005-DEP-MALE   (WS-CHILD-SUBSCRIPT)               
             ELSE                                                               
           IF  GCCC0005-DEP-GENDER (WS-CHILD-SUBSCRIPT) = '1'                   
               MOVE 'X' TO GL0005-DEP-FEMALE (WS-CHILD-SUBSCRIPT).              
                                                                                
           MOVE  GCCC0005-DEP-DOB (WS-CHILD-SUBSCRIPT)                          
                                 TO WS-DCPI-DATE.                               
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE  TO GL0005-DEP-DOB (WS-CHILD-SUBSCRIPT).          
                                                                                
           MOVE  GCCC0005-DEP-RELSHP (WS-CHILD-SUBSCRIPT) TO                    
                              GL0005-DEP-RELSHP (WS-CHILD-SUBSCRIPT).           
                                                                                
           IF  GCCC0005-DEP-STUDENT (WS-CHILD-SUBSCRIPT) = '0'                  
               MOVE 'X'  TO GL0005-DEP-STUD-NO  (WS-CHILD-SUBSCRIPT)            
             ELSE                                                               
           IF  GCCC0005-DEP-STUDENT (WS-CHILD-SUBSCRIPT) = '1'                  
               MOVE 'X'  TO GL0005-DEP-STUD-YES (WS-CHILD-SUBSCRIPT).           
                                                                                
                                                                                
       3150-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3200-FORMAT-GL0514.                                                      
      ******************************************************************        
      * PRODUCE THE GL0514 FORMAT                                               
      ******************************************************************        
      *                                                                         
           INITIALIZE GL0514-RECORD.                                            
      *                                                                         
           MOVE  GCCCFEXT-LANG          TO WS-DCP-LANG.                         
           MOVE  '1'                    TO GL0514-CC.                           
      *                                                                         
           IF  GCCCFEXT-LANG   =  'F'                                           
               ADD 1 TO WS-BANNER-GL0514-FR-TOT                                 
               MOVE  '54'               TO GL0514-FORM-TYPE                     
             ELSE                                                               
               ADD 1 TO WS-BANNER-GL0514-ENG-TOT                                
               MOVE  '04'               TO GL0514-FORM-TYPE.                    
                                                                                
                                                                                
           MOVE  GCCC0514-DATE-TIME     TO GL0514-DATE-TIME.                    
                                                                                
           IF  GCCC0514-SELECT    =  '0'                                        
               MOVE 'X'                 TO GL0514-SELECT-REQUEST                
             ELSE                                                               
           IF  GCCC0514-SELECT    =  '1'                                        
               MOVE 'X'                 TO GL0514-SELECT-TERM-REQ.              
                                                                                
           MOVE  GCCC0514-SPONSOR       TO GL0514-SPONSOR.                      
           MOVE  GCCC0514-PLAN (1)      TO GL0514-PLAN (1).                     
           MOVE  GCCC0514-PLAN (2)      TO GL0514-PLAN (2).                     
           MOVE  GCCC0514-ACCT (1)      TO GL0514-ACCT (1).                     
           MOVE  GCCC0514-ACCT (2)      TO GL0514-ACCT (2).                     
           MOVE  GCCC0514-DIV  (1)      TO GL0514-DIV  (1).                     
           MOVE  GCCC0514-DIV  (2)      TO GL0514-DIV  (2).                     
           MOVE  GCCC0514-CERT          TO GL0514-CERT.                         
           MOVE  GCCC0514-MBR-LAST-NME  TO GL0514-MBR-LAST-NAME.                
           MOVE  GCCC0514-MBR-FIRST-NME TO GL0514-MBR-FIRST-NAME.               
           MOVE  GCCC0514-MBR-MID-INIT  TO GL0514-MBR-MID-INIT.                 
           MOVE  GCCC0514-MBR-ADDRESS   TO GL0514-MBR-ADDRESS.                  
           MOVE  GCCC0514-MBR-CITY      TO GL0514-MBR-CITY.                     
           MOVE  GCCC0514-MBR-PROV      TO GL0514-MBR-PROV.                     
           MOVE  GCCC0514-MBR-POSTCODE  TO GL0514-MBR-POSTCODE.                 
           MOVE  GCCC0514-DEP-LAST      TO GL0514-DEP-LAST-NAME.                
           MOVE  GCCC0514-DEP-FIRST     TO GL0514-DEP-FIRST-NAME.               
           MOVE  GCCC0514-DEP-RELSHP    TO GL0514-DEP-RELSHP.                   
                                                                                
           MOVE  GCCC0514-DEP-DOB       TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0514-DEP-DOB.                      
                                                                                
           IF  GCCC0514-DEP-GENDER    =  '0'                                    
               MOVE 'X'                 TO GL0514-DEP-MALE                      
             ELSE                                                               
           IF  GCCC0514-DEP-GENDER    =  '1'                                    
               MOVE 'X'                 TO GL0514-DEP-FEMALE.                   
                                                                                
           MOVE  GCCC0514-DEP-ADDRESS   TO GL0514-DEP-ADDRESS.                  
           MOVE  GCCC0514-DEP-CITY      TO GL0514-DEP-CITY.                     
           MOVE  GCCC0514-DEP-PROV      TO GL0514-DEP-PROV.                     
           MOVE  GCCC0514-DEP-POSTCODE  TO GL0514-DEP-POSTCODE.                 
                                                                                
           IF  GCCC0514-RESIDENT      =  '0'                                    
               MOVE 'X'                 TO GL0514-DEP-RES-NO                    
             ELSE                                                               
           IF  GCCC0514-RESIDENT      =  '1'                                    
               MOVE 'X'                 TO GL0514-DEP-RES-YES.                  
                                                                                
           MOVE  GCCC0514-DDEP-RES-EXPL TO GL0514-DEP-RES-EXPL.                 
                                                                                
           IF  GCCC0514-EMPLOYED      =  '0'                                    
               MOVE 'X'                 TO GL0514-DEP-EMPL-NO                   
             ELSE                                                               
           IF  GCCC0514-EMPLOYED      =  '1'                                    
               MOVE 'X'                 TO GL0514-DEP-EMPL-YES.                 
                                                                                
           MOVE  GCCC0514-DIS-DEP-DATE  TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0514-DEP-DATE-EMPLOYED.            
                                                                                
           MOVE  GCCC0514-TYPE          TO GL0514-DEP-EMPL-TYPE.                
                                                                                
           IF  GCCC0514-GOVMT         =  '0'                                    
               MOVE 'X'                 TO GL0514-DEP-GOV-ELIG-NO               
             ELSE                                                               
           IF  GCCC0514-GOVMT         =  '1'                                    
               MOVE 'X'                 TO GL0514-DEP-GOV-ELIG-YES.             
                                                                                
           IF  GCCC0514-GROUP         =  '0'                                    
               MOVE 'X'                 TO GL0514-DEP-GRP-ELIG-NO               
             ELSE                                                               
           IF  GCCC0514-GROUP         =  '1'                                    
               MOVE 'X'                 TO GL0514-DEP-GRP-ELIG-YES.             
                                                                                
           MOVE  GCCC0514-DDEP-ELIG-EXPL                                        
                                        TO GL0514-DEP-ELIG-EXPL.                
                                                                                
           IF  GCCC0514-SOLE          =  '0'                                    
               MOVE 'X'                 TO GL0514-DEP-SOLE-SUPP-NO              
             ELSE                                                               
           IF  GCCC0514-SOLE          =  '1'                                    
               MOVE 'X'                 TO GL0514-DEP-SOLE-SUPP-YES.            
                                                                                
           MOVE  GCCC0514-DDEP-SOLE-EXPL                                        
                                        TO GL0514-DEP-SOLE-EXPL.                
                                                                                
           IF  GCCC0514-DEP-PHYSIC-MAILED =  '1'                                
               MOVE  'X'                TO GL0514-DEP-PHYSIC-MAILED.            
                                                                                
           MOVE  GCCC0514-SCHOOL        TO GL0514-SCHOOL-NAME.                  
           MOVE  GCCC0514-LOC           TO GL0514-SCHOOL-LOC.                   
                                                                                
           MOVE  GCCC0514-STUD-ST-DATE  TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0514-SCHOOL-START-DATE.            
                                                                                
           MOVE  GCCC0514-STUD-END-DATE TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0514-SCHOOL-END-DATE.              
                                                                                
           IF  GCCC0514-TERMINATE     =  '1'                                    
               MOVE 'X'                 TO GL0514-TERM-ALL-COV.                 
                                                                                
           MOVE  GCCC0514-DEPENDENT     TO GL0514-TERM-COV-DEP-NAME             
                                                                                
           MOVE  GCCC0514-STUD-TERM-DATE                                        
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0514-TERM-COV-DATE.                
                                                                                
           MOVE  GCCC0514-STUD-TERM-REASON                                      
                                        TO GL0514-TERM-COV-REASON               
                                                                                
           MOVE  GCCC0514-TODAY         TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL0514-ENGLISH-TODAY.                
                                                                                
      *    THE Q IN THE FOLLOWING CODE WAS INSERTED                             
                                                                                
                                                                                
           MOVE  GCCC0514-REGION          TO  WS-TEMP-REG-IN.                   
           PERFORM   7500-REGION-FORMAT  THRU  7500-EXIT.                       
           MOVE  WS-TEMP-REGION           TO  GL0514-REGION.                    
                                                                                
                                                                                
                                                                                
           MOVE  GL0514-RECORD          TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
                                                                                
                                                                                
       3200-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
                                                                                
                                                                                
       3300-FORMAT-GL2971.                                                      
      ******************************************************************        
      * PRODUCE THE GL2971 FORMAT                                               
      ******************************************************************        
      *                                                                         
           INITIALIZE GL2971-RECORD.                                            
      *                                                                         
                                                                                
           MOVE  GCCCFEXT-LANG          TO WS-DCP-LANG                          
                                           WS-RELSHP-LANG.                      
           MOVE  '1'                    TO GL2971-CC-P1.                        
      *                                                                         
      *                                                                         
           IF  GCCCFEXT-LANG   =  'F'                                           
               ADD 1 TO WS-BANNER-GL2971-FR-TOT                                 
               MOVE  '52'               TO GL2971-FORM-TYPE-P1                  
                                           GL2971-FORM-TYPE-P2                  
             ELSE                                                               
               ADD 1 TO WS-BANNER-GL2971-ENG-TOT                                
               MOVE  '02'               TO GL2971-FORM-TYPE-P1                  
                                           GL2971-FORM-TYPE-P2.                 
                                                                                
                                                                                
           MOVE  GCCC2971-DATE-TIME     TO GL2971-DATE-TIME.                    
                                                                                
           MOVE  GCCC2971-PLAN    (1)   TO GL2971-PLAN    (1).                  
           MOVE  GCCC2971-PLAN    (2)   TO GL2971-PLAN    (2).                  
           MOVE  GCCC2971-PLAN    (3)   TO GL2971-PLAN    (3).                  
           MOVE  GCCC2971-PLAN    (4)   TO GL2971-PLAN    (4).                  
           MOVE  GCCC2971-PLAN    (5)   TO GL2971-PLAN    (5).                  
           MOVE  GCCC2971-ACCOUNT (1)   TO GL2971-ACCOUNT (1).                  
           MOVE  GCCC2971-ACCOUNT (2)   TO GL2971-ACCOUNT (2).                  
           MOVE  GCCC2971-ACCOUNT (3)   TO GL2971-ACCOUNT (3).                  
           MOVE  GCCC2971-ACCOUNT (4)   TO GL2971-ACCOUNT (4).                  
           MOVE  GCCC2971-ACCOUNT (5)   TO GL2971-ACCOUNT (5).                  
           MOVE  GCCC2971-DIV     (1)   TO GL2971-DIV     (1).                  
           MOVE  GCCC2971-DIV     (2)   TO GL2971-DIV     (2).                  
           MOVE  GCCC2971-DIV     (3)   TO GL2971-DIV     (3).                  
           MOVE  GCCC2971-DIV     (4)   TO GL2971-DIV     (4).                  
           MOVE  GCCC2971-DIV     (5)   TO GL2971-DIV     (5).                  
           MOVE  GCCC2971-CERT          TO GL2971-CERT.                         
           MOVE  GCCC2971-SPONSOR       TO GL2971-SPONSOR.                      
                                                                                
           MOVE  GCCC2971-HIRE-DATE     TO  WS-DCPI-DATE.                       
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO  GL2971-HIRE-DATE.                   
                                                                                
           MOVE  GCCC2971-PREV-EMPL-DATE                                        
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL2971-PREV-EMPL-DATE.               
                                                                                
           MOVE  GCCC2971-REHIRE-DATE   TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL2971-REHIRE-DATE.                  
                                                                                
      * ADDED NEW WAITING PERIOD FIELDS  - RELEASE 4.2                          
                                                                                
           IF GCCC2971-WAITING-PERIOD-IND = '0'                                 
              MOVE 'X'                   TO GL2971-WAITING-PERIOD-NO            
           ELSE IF GCCC2971-WAITING-PERIOD-IND = '1'                            
              MOVE 'X'                   TO GL2971-WAITING-PERIOD-YES.          
                                                                                
      *    IF GCCC2971-WAITING-PERIOD-COND = '1'                                
      *       MOVE 'X'                   TO GL2971-EMPLOY-STATUS-CHG            
      *    ELSE IF GCCC2971-WAITING-PERIOD-COND = '2'                           
      *       MOVE 'X'                   TO GL2971-COV-PREV-EMPLOYER            
      *    ELSE IF GCCC2971-WAITING-PERIOD-COND = '3'                           
      *       MOVE 'X'                   TO GL2971-WAITING-PERIOD-OTHER.        
      ** ADD BANK INFO DETAILS AND EMAIL DETAILS,TL 236472                      
           MOVE GCCC2971-BANK-NAME          TO GL2971-BANK-NAME.                
           MOVE GCCC2971-BANK-TRANSIT       TO GL2971-BANK-TRANSIT.             
           MOVE GCCC2971-BANK-INSTITUTION   TO GL2971-BANK-INSTITUTION.         
           MOVE GCCC2971-BANK-ACCOUNT       TO GL2971-BANK-ACCOUNT.             
           MOVE GCCC2971-EMAIL-ADDRESS-WORK TO GL2971-EMAIL-ADRS-WORK.          
           MOVE GCCC2971-EMAIL-ADDRESS-HOME TO GL2971-EMAIL-ADRS-HOME.          
                                                                                
      * CALL GACCOCC TO FIND GIPSY CODE WHICH MATCHES THE OCCUPATION            
      * SUBMITTED                                                               
                                                                                
           SET GACCOCC-DESC-TO-CODE      TO TRUE.                               
           IF GCCCFEXT-LANG              =  'E'                                 
               MOVE GCCC2971-OCC         TO GACCOCC-ENG-OCC                     
               SET GACCOCC-ENGLISH       TO TRUE                                
           ELSE                                                                 
               MOVE GCCC2971-OCC         TO GACCOCC-FR-OCC                      
               SET GACCOCC-FRENCH        TO TRUE                                
           END-IF.                                                              
                                                                                
           CALL WS-GACPOCC              USING GACCOCC-RECORD.                   
                                                                                
           IF GACCOCC-SUCCESSFUL                                                
               MOVE GACCOCC-GIPSY-CODE  TO WS-GIPSY-CODE                        
               MOVE WS-GIPSY-CODE-CHAR2 TO WS-OCCUPATION-CODE                   
           ELSE                                                                 
               MOVE SPACES              TO WS-OCCUPATION-CODE                   
           END-IF.                                                              
                                                                                
           MOVE  GCCC2971-OCC           TO WS-OCCUPATION-DESC.                  
           MOVE  WS-OCCUPATION          TO GL2971-OCCUPATION.                   
                                                                                
           MOVE  GCCC2971-CLASS   (1)   TO GL2971-CLASS (1).                    
           MOVE  GCCC2971-CLASS   (2)   TO GL2971-CLASS (2).                    
           MOVE  GCCC2971-CLASS   (3)   TO GL2971-CLASS (3).                    
           MOVE  GCCC2971-CLASS   (4)   TO GL2971-CLASS (4).                    
           MOVE  GCCC2971-CLASS   (5)   TO GL2971-CLASS (5).                    
                                                                                
           MOVE  GCCC2971-HOURS         TO GL2971-HOURS                         
           MOVE  GCCC2971-EARNINGS      TO GL2971-EARNINGS              .       
                                                                                
           IF  GCCC2971-EVIDENCE      =  '0'                                    
               MOVE 'X'                 TO GL2971-EVIDENCE-REQD-NO              
             ELSE                                                               
           IF  GCCC2971-EVIDENCE      =  '1'                                    
               MOVE 'X'                 TO GL2971-EVIDENCE-REQD-YES.            
                                                                                
      * RLEASE 3.2 GL2971-MAILED IS NO LONGER USED                              
           MOVE SPACES                  TO GL2971-MAILED.                       
                                                                                
           MOVE  GCCC2971-EMP-NAME      TO GL2971-MBR-NAME.                     
                                                                                
           MOVE  GCCC2971-EMP-DOB       TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL2971-MBR-DOB.                      
                                                                                
           IF  GCCC2971-EMP-GENDER    =  '0'                                    
               MOVE 'X'                 TO GL2971-MBR-MALE                      
             ELSE                                                               
           IF  GCCC2971-EMP-GENDER    =  '1'                                    
               MOVE 'X'                 TO GL2971-MBR-FEMALE.                   
                                                                                
           MOVE  GCCC2971-EMP-PROV      TO GL2971-MBR-PROV.                     
                                                                                
      *                                                                         
           IF  GCCC2971-EMP-LANG      =  '2'                                    
               MOVE 'X'                 TO GL2971-MBR-LANG-FRC                  
             ELSE                                                               
               MOVE 'X'                 TO GL2971-MBR-LANG-ENG.                 
                                                                                
           MOVE GCCC2971-EMP-STREET     TO GL2971-MBR-STREET.                   
           MOVE GCCC2971-EMP-CITY       TO GL2971-MBR-CITY.                     
           MOVE GCCC2971-EMP-ADDR-PROV  TO GL2971-MBR-ADDR-PROV.                
           MOVE GCCC2971-EMP-POSTCODE   TO GL2971-MBR-POSTCODE.                 
                                                                                
           IF  GCCC2971-QUEBEC-AGE    =  '1'                                    
               MOVE 'X'                 TO GL2971-QUEBEC-AGE-BOX1               
             ELSE                                                               
           IF  GCCC2971-QUEBEC-AGE    =  '2'                                    
               MOVE 'X'                 TO GL2971-QUEBEC-AGE-BOX2.              
                                                                                
      * RELEASE 3.2 GL2971-SPOUSE-GENDER IS NO LONGER USED                      
           MOVE SPACES                TO GL2971-SPOUSE-GENDER.                  
                                                                                
                                                                                
           MOVE  GCCC2971-SPOUSE-DOB    TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL2971-SPOUSE-DOB.                   
                                                                                
      * THE FOLLOWING CODE WAS ADDED TO POPULATE THE NEW FIELDS                 
      * ADDED WITH RELEASE 3.2                                                  
                                                                                
           IF GCCC2971-SPOUSE-IND        = '1'                                  
               MOVE 'X'                  TO GL2971-SPOUSE-IND                   
           END-IF.                                                              
                                                                                
           IF GCCC2971-COMN-LAW-IND      = '0'                                  
               MOVE 'X'                  TO GL2971-COMN-LAW-IND-NO              
           ELSE                                                                 
               IF GCCC2971-COMN-LAW-IND  = '1'                                  
                   MOVE 'X'              TO GL2971-COMN-LAW-IND-YES             
               END-IF                                                           
           END-IF.                                                              
                                                                                
      *                                                                         
      * RELEASE 3.2 GL2971-MARITAL-STATUS IS NO LONGER USED                     
           MOVE SPACES                   TO GL2971-MARITAL-STATUS.              
                                                                                
                                                                                
           MOVE GCCC2971-COMMON-LAW-DATE TO WS-DCPI-DATE.                       
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                                   GL2971-RELATIONSHIP-START-DATE.              
                                                                                
      *    th                                                                   
                                                                                
      *    THE Q IN THE FOLLOWING CODE WAS INSERTED                             
                                                                                
                                                                                
           MOVE  GCCC2971-REGION          TO  WS-TEMP-REG-IN.                   
           PERFORM   7500-REGION-FORMAT  THRU  7500-EXIT.                       
           MOVE  WS-TEMP-REGION           TO  GL2971-REGION-P1                  
                                              GL2971-REGION-P2.                 
                                                                                
                                                                                
           MOVE  GCCC2971-SPOUSE-NAME   TO GL2971-SPOUSE-NAME.                  
                                                                                
           MOVE  GCCC2971-FAM-SPOUSE-DOB    TO  WS-DCPI-DATE.                   
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL2971-FAMILY-SPOUSE-DOB.            
                                                                                
           IF  GCCC2971-FAM-SPOUSE-GENDER =  '0'                                
               MOVE 'X'                 TO GL2971-FAM-SPOUSE-MALE               
             ELSE                                                               
           IF  GCCC2971-FAM-SPOUSE-GENDER =  '1'                                
               MOVE 'X'                 TO GL2971-FAM-SPOUSE-FEMALE.            
                                                                                
           MOVE GCCC2971-FAM-SPOUSE-RELSHP                                      
                                         TO WS-IN-RELSHP.                       
           PERFORM 6200-CONV-RELSHP   THRU  6200-EXIT.                          
           MOVE WS-OUT-RELSHP            TO GL2971-FAM-SPOUSE-RELSHP.           
                                                                                
                                                                                
           MOVE 'N'                     TO WS-STOP-SUBSCRIPT-MKR.               
                                                                                
           MOVE  SPACES       TO  GL2971-DOB-CHILD  (1)                         
                                  GL2971-DOB-CHILD  (2)                         
                                  GL2971-DOB-CHILD  (3)                         
                                  GL2971-DOB-CHILD  (4)                         
                                  GL2971-DOB-CHILD  (5)                         
                                  GL2971-DOB-CHILD  (6)                         
                                  GL2971-DOB-CHILD  (7)                         
                                  GL2971-DOB-CHILD  (8).                        
                                                                                
           PERFORM 3400-FORMAT-RECURRING-GL2971 THRU 3400-EXIT                  
             VARYING  WS-CHILD-SUBSCRIPT                                        
                FROM  1  BY  1                                                  
                  UNTIL WS-CHILD-SUBSCRIPT  >   8.                              
                                                                                
           MOVE 'N'                     TO WS-STOP-SUBSCRIPT-MKR.               
                                                                                
           PERFORM 3500-FORMAT-RECURRING-GL2971 THRU 3500-EXIT                  
             VARYING  WS-BENEFIT-SUBSCRIPT                                      
                FROM  1  BY  1                                                  
                  UNTIL WS-BENEFIT-SUBSCRIPT  >   6                             
                    OR  WS-STOP-SUBSCRIPT.                                      
                                                                                
      *    IF  GCCC2971-BEN           =  '1'                                    
      *        MOVE 'X'                 TO GL2971-BENEFICIARY-REQ.              
                                                                                
      *                                                                         
           MOVE GCCC2971-COMMENTS       TO GL2971-COMMENTS.                     
      *                                                                         
                                                                                
           MOVE  GCCC2971-TODAY         TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL2971-ENGLISH-TODAY.                
                                                                                
                                                                                
           MOVE  GL2971-RECORD-P1       TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
           MOVE  GL2971-RECORD-P2       TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
       3300-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
                                                                                
                                                                                
                                                                                
       3400-FORMAT-RECURRING-GL2971.                                            
      ******************************************************************        
      * PRODUCE THE RECURRING GL2971 FORMAT FIELDS                              
      ******************************************************************        
      *                                                                         
      *                                                                         
                                                                                
           PERFORM 3450-CONT-FORMATTING-GL2971  THRU 3450-EXIT.                 
                                                                                
       3400-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
                                                                                
       3450-CONT-FORMATTING-GL2971.                                             
                                                                                
                                                                                
           MOVE  GCCC2971-CHILD-NAME (WS-CHILD-SUBSCRIPT)                       
                   TO  GL2971-NAME-CHILD (WS-CHILD-SUBSCRIPT).                  
                                                                                
                                                                                
           MOVE  GCCC2971-CHILD-DOB (WS-CHILD-SUBSCRIPT)                        
                                          TO  WS-DCPI-DATE.                     
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE                                                   
                   TO  GL2971-DOB-CHILD  (WS-CHILD-SUBSCRIPT).                  
                                                                                
      *                                                                         
           IF   GCCC2971-CHILD-GENDER (WS-CHILD-SUBSCRIPT) = '0'                
               MOVE 'X'  TO  GL2971-CHILD-MALE (WS-CHILD-SUBSCRIPT)             
             ELSE                                                               
           IF   GCCC2971-CHILD-GENDER (WS-CHILD-SUBSCRIPT) = '1'                
               MOVE 'X'  TO  GL2971-CHILD-FEMALE (WS-CHILD-SUBSCRIPT).          
                                                                                
      *                                                                         
      *                                                                         
                                                                                
                                                                                
           MOVE GCCC2971-CHILD-RELSHP (WS-CHILD-SUBSCRIPT)                      
                                         TO WS-IN-RELSHP.                       
           PERFORM 6200-CONV-RELSHP   THRU  6200-EXIT.                          
           MOVE WS-OUT-RELSHP            TO                                     
                             GL2971-CHILD-RELSHP (WS-CHILD-SUBSCRIPT).          
                                                                                
                                                                                
                                                                                
           IF   GCCC2971-CHILD-STUDENT (WS-CHILD-SUBSCRIPT) = '0'               
               MOVE 'X'  TO                                                     
                       GL2971-CHILD-STUDENT-NO  (WS-CHILD-SUBSCRIPT)            
             ELSE                                                               
           IF   GCCC2971-CHILD-STUDENT (WS-CHILD-SUBSCRIPT) = '1'               
               MOVE 'X'  TO                                                     
                       GL2971-CHILD-STUDENT-YES (WS-CHILD-SUBSCRIPT).           
      *                                                                         
           IF   GCCC2971-CHILD-DISAB  (WS-CHILD-SUBSCRIPT)  = '0'               
               MOVE 'X'  TO                                                     
                       GL2971-CHILD-DISABLED-NO  (WS-CHILD-SUBSCRIPT)           
             ELSE                                                               
           IF   GCCC2971-CHILD-DISAB  (WS-CHILD-SUBSCRIPT)  = '1'               
               MOVE 'X'  TO                                                     
                       GL2971-CHILD-DISABLED-YES (WS-CHILD-SUBSCRIPT).          
                                                                                
      *                                                                         
       3450-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
                                                                                
       3500-FORMAT-RECURRING-GL2971.                                            
      *    OCCURS 6                                                             
      *     INITIALLY GCCC2971-COV-BENEFIT WAS TO IDENTIFY                      
      *     DLIF, EHC OR DENT NOW THIS IS IDENTIFIED BY SUBSCTRIPT              
                                                                                
                                                                                
           IF  WS-BENEFIT-SUBSCRIPT   =   1                                     
      *                =  'EHC '                                                
               PERFORM  3520-FORMAT-EHC-BENEFIT  THRU 3520-EXIT                 
             ELSE                                                               
           IF  WS-BENEFIT-SUBSCRIPT   =   2                                     
      *                =  'DENT'                                                
               PERFORM  3530-FORMAT-DENT-BENEFIT THRU 3530-EXIT                 
             ELSE                                                               
           IF  WS-BENEFIT-SUBSCRIPT   =   3                                     
      *                =  'DLIF'                                                
               PERFORM  3510-FORMAT-LIFE-BENEFIT THRU 3510-EXIT                 
             ELSE                                                               
               MOVE 'Y'   TO  WS-STOP-SUBSCRIPT-MKR.                            
                                                                                
       3500-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       3510-FORMAT-LIFE-BENEFIT.                                                
                                                                                
           IF   GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                     
                                      =  '0'                                    
               MOVE 'X'                 TO GL2971-DEP-LIFE-NO                   
             ELSE                                                               
           IF   GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                     
                                      =  '1'                                    
               MOVE 'X'                 TO GL2971-DEP-LIFE-YES.                 
                                                                                
       3510-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       3520-FORMAT-EHC-BENEFIT.                                                 
                                                                                
           IF  GCCC2971-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '0'                                    
               MOVE 'X'                 TO GL2971-SPOUSE-HLTH-COV-NO            
             ELSE                                                               
           IF  GCCC2971-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '1'                                    
               MOVE 'X'                 TO GL2971-SPOUSE-HLTH-COV-YES.          
                                                                                
           IF  GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '1'                                    
               MOVE 'X'                 TO  GL2971-EHC-BOX1                     
             ELSE                                                               
           IF  GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '2'                                    
               MOVE 'X'                 TO  GL2971-EHC-BOX2                     
             ELSE                                                               
           IF  GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '3'                                    
               MOVE 'X'                 TO  GL2971-EHC-BOX3                     
             ELSE                                                               
           IF  GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '4'                                    
               MOVE 'X'                 TO  GL2971-EHC-BOX4.                    
                                                                                
                                                                                
           IF  GCCC2971-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '1'                                    
               MOVE 'X'                 TO  GL2971-SPOUSE-HC-SCOPE-1            
             ELSE                                                               
           IF  GCCC2971-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '2'                                    
               MOVE 'X'                 TO  GL2971-SPOUSE-HC-SCOPE-2            
             ELSE                                                               
           IF  GCCC2971-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '3'                                    
               MOVE 'X'                 TO  GL2971-SPOUSE-HC-SCOPE-3            
             ELSE                                                               
           IF  GCCC2971-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '4'                                    
               MOVE 'X'                 TO  GL2971-SPOUSE-HC-SCOPE-4.           
                                                                                
                                                                                
           MOVE  GCCC2971-COV-SPOUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)             
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                               GL2971-SPOUSE-HC-EFF-DATE.                       
                                                                                
       3520-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3530-FORMAT-DENT-BENEFIT.                                                
                                                                                
           IF  GCCC2971-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '0'                                    
               MOVE 'X'                 TO GL2971-SPOUSE-DENT-COV-N             
             ELSE                                                               
           IF  GCCC2971-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '1'                                    
               MOVE 'X'                 TO GL2971-SPOUSE-DENT-COV-Y.            
                                                                                
                                                                                
           IF  GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '1'                                    
               MOVE 'X'                 TO  GL2971-DENT-BOX1                    
             ELSE                                                               
           IF  GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '2'                                    
               MOVE 'X'                 TO  GL2971-DENT-BOX2                    
             ELSE                                                               
           IF  GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '3'                                    
               MOVE 'X'                 TO  GL2971-DENT-BOX3                    
             ELSE                                                               
           IF  GCCC2971-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '4'                                    
               MOVE 'X'                 TO  GL2971-DENT-BOX4.                   
                                                                                
           MOVE  GCCC2971-COV-SPOUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)             
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                               GL2971-SPOUSE-DC-EFF-DATE.                       
                                                                                
                                                                                
           IF  GCCC2971-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '1'                                    
               MOVE 'X'                 TO  GL2971-SPOUSE-DC-SCOPE-1            
             ELSE                                                               
           IF  GCCC2971-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '2'                                    
               MOVE 'X'                 TO  GL2971-SPOUSE-DC-SCOPE-2            
             ELSE                                                               
           IF  GCCC2971-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '3'                                    
               MOVE 'X'                 TO  GL2971-SPOUSE-DC-SCOPE-3            
             ELSE                                                               
           IF  GCCC2971-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '4'                                    
               MOVE 'X'                 TO  GL2971-SPOUSE-DC-SCOPE-4.           
                                                                                
                                                                                
       3530-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3600-FORMAT-GL3187.                                                      
      ******************************************************************        
      * PRODUCE THE GL3187 FORMAT                                               
      ******************************************************************        
      *                                                                         
           INITIALIZE GL3187-RECORD.                                            
      *                                                                         
                                                                                
           MOVE  GCCCFEXT-LANG          TO WS-DCP-LANG                          
                                           WS-RELSHP-LANG.                      
           MOVE  '1'                    TO GL3187-CC-P1.                        
      *                                                                         
           IF  GCCCFEXT-LANG   =  'F'                                           
               ADD 1 TO WS-BANNER-GL3187-FR-TOT                                 
               MOVE  '51'         TO GL3187-FORM-TYPE-P1                        
                                     GL3187-FORM-TYPE-P2                        
             ELSE                                                               
               ADD 1 TO WS-BANNER-GL3187-ENG-TOT                                
               MOVE  '01'         TO GL3187-FORM-TYPE-P1                        
                                     GL3187-FORM-TYPE-P2.                       
                                                                                
                                                                                
           MOVE  GCCC3187-DATE-TIME     TO GL3187-DATE-TIME.                    
           MOVE  GCCC3187-PLAN    (1)   TO GL3187-PLAN     (1).                 
           MOVE  GCCC3187-PLAN    (2)   TO GL3187-PLAN     (2).                 
           MOVE  GCCC3187-PLAN    (3)   TO GL3187-PLAN     (3).                 
           MOVE  GCCC3187-PLAN    (4)   TO GL3187-PLAN     (4).                 
           MOVE  GCCC3187-PLAN    (5)   TO GL3187-PLAN     (5).                 
           MOVE  GCCC3187-ACCOUNT (1)   TO GL3187-ACCOUNT  (1).                 
           MOVE  GCCC3187-ACCOUNT (2)   TO GL3187-ACCOUNT  (2).                 
           MOVE  GCCC3187-ACCOUNT (3)   TO GL3187-ACCOUNT  (3).                 
           MOVE  GCCC3187-ACCOUNT (4)   TO GL3187-ACCOUNT  (4).                 
           MOVE  GCCC3187-ACCOUNT (5)   TO GL3187-ACCOUNT  (5).                 
           MOVE  GCCC3187-DIV     (1)   TO GL3187-DIV      (1).                 
           MOVE  GCCC3187-DIV     (2)   TO GL3187-DIV      (2).                 
           MOVE  GCCC3187-DIV     (3)   TO GL3187-DIV      (3).                 
           MOVE  GCCC3187-DIV     (4)   TO GL3187-DIV      (4).                 
           MOVE  GCCC3187-DIV     (5)   TO GL3187-DIV      (5).                 
           MOVE  GCCC3187-CERT          TO GL3187-CERT.                         
           MOVE  GCCC3187-SPONSOR       TO GL3187-SPONSOR.                      
                                                                                
           MOVE  GCCC3187-MBR-NAME      TO GL3187-MBR-NAME.                     
           MOVE  GCCC3187-NEWNAME       TO GL3187-MBR-NEWNAME.                  
                                                                                
           MOVE  GCCC3187-MBR-STREET    TO GL3187-MBR-STREET.                   
           MOVE  GCCC3187-MBR-CITY      TO GL3187-MBR-CITY.                     
           MOVE  GCCC3187-MBR-PROV      TO GL3187-MBR-PROV.                     
           MOVE  GCCC3187-MBR-POSTCODE  TO GL3187-MBR-POSTCODE.                 
                                                                                
           IF  GCCC3187-SPOUSE-ADD-REASON      =  '1'                           
               MOVE 'X'                 TO GL3187-ADD-REASON1                   
             ELSE                                                               
           IF  GCCC3187-SPOUSE-ADD-REASON      =  '2'                           
               MOVE 'X'                 TO GL3187-ADD-REASON2                   
             ELSE                                                               
           IF  GCCC3187-SPOUSE-ADD-REASON      =  '3'                           
               MOVE 'X'                 TO GL3187-ADD-REASON3                   
             ELSE                                                               
           IF  GCCC3187-SPOUSE-ADD-REASON      =  '4'                           
               MOVE 'X'                 TO GL3187-ADD-REASON4.                  
                                                                                
                                                                                
           MOVE  GCCC3187-MARRIED-DATE  TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3187-MARRIAGE-DATE.                
                                                                                
                                                                                
           MOVE  GCCC3187-COM-DATE      TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3187-COMMON-LAW-EFF-DATE.          
                                                                                
                                                                                
           MOVE  GCCC3187-COV-TERM-DATE TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3187-TERMINATE-DATE.               
                                                                                
                                                                                
           MOVE  GCCC3187-COV-EFF-DATE  TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3187-EFFECTIVE-DATE.               
                                                                                
           MOVE  GCCC3187-OTHER-DETAILS TO GL3187-OTHER-DETAILS.                
                                                                                
           IF  GCCC3187-EVIDENCE      =  '0'                                    
               MOVE 'X'                 TO GL3187-EVIDENCE-REQD-NO              
             ELSE                                                               
           IF  GCCC3187-EVIDENCE      =  '1'                                    
               MOVE 'X'                 TO GL3187-EVIDENCE-REQD-YES.            
                                                                                
                                                                                
      * RELEASE 3.2 GL3187-MAILED NO LONGER USED                                
           MOVE SPACES                   TO GL3187-MAILED.                      
                                                                                
      * RELEASE 3.2 GL3187-SPOUSE-GENDER NO LONGER USED                         
           MOVE SPACES                   TO GL3187-SPOUSE-GENDER.               
                                                                                
                                                                                
           MOVE  GCCC3187-SPOUSE-DOB    TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3187-SPOUSE-DOB.                   
                                                                                
                                                                                
      * RELEASE 3.2 GL3187-MARITAL-STATUS NO LONGER USED                        
           MOVE SPACES                   TO GL3187-MARITAL-STATUS.              
                                                                                
      * RELEASE 3.2 GL3187-RELATIONSHIP-START-DATE NO LONGER USED               
           MOVE SPACES                                                          
                               TO GL3187-RELATIONSHIP-START-DATE.               
                                                                                
                                                                                
                                                                                
      *    THE Q IN THE FOLLOWING CODE WAS INSERTED                             
                                                                                
                                                                                
           MOVE  GCCC3187-REGION          TO  WS-TEMP-REG-IN.                   
           PERFORM   7500-REGION-FORMAT  THRU  7500-EXIT.                       
           MOVE  WS-TEMP-REGION           TO  GL3187-REGION-P1                  
                                              GL3187-REGION-P2.                 
                                                                                
           MOVE  SPACES                   TO GL3187-FAMILY-INFO.                
                                                                                
           MOVE  GCCC3187-SPOUSE-CHG-CODE TO WS-IN-CHG-CODE.                    
           PERFORM 6300-CONV-CHG-CODE THRU 6300-EXIT.                           
           MOVE  WS-OUT-CHG-CODE           TO GL3187-SPOUSE-CHG-CODE.           
                                                                                
           MOVE  GCCC3187-SPOUSE-CHG-EFF-DATE                                   
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                                      GL3187-SPOUSE-CHG-EFF-DATE.               
                                                                                
           MOVE  GCCC3187-SPOUSE-NAME   TO GL3187-SPOUSE-NAME.                  
                                                                                
           MOVE  GCCC3187-FAM-SPOUSE-DOB   TO WS-DCPI-DATE.                     
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                                       GL3187-FAMILY-SPOUSE-DOB.                
                                                                                
           IF  GCCC3187-FAM-SPOUSE-GENDER  =  '0'                               
               MOVE 'X'                 TO GL3187-FAM-SPOUSE-MALE               
             ELSE                                                               
           IF  GCCC3187-FAM-SPOUSE-GENDER  =  '1'                               
               MOVE 'X'                 TO GL3187-FAM-SPOUSE-FEMALE.            
                                                                                
                                                                                
           MOVE GCCC3187-SPOUSE-RELSHP                                          
                                         TO WS-IN-RELSHP.                       
           PERFORM 6200-CONV-RELSHP     THRU  6200-EXIT.                        
           MOVE WS-OUT-RELSHP            TO                                     
                             GL3187-FAM-SPOUSE-RELSHP.                          
                                                                                
                                                                                
           MOVE 'N'                     TO  WS-STOP-SUBSCRIPT-MKR.              
                                                                                
           MOVE  SPACES       TO  GL3187-DOB-CHILD  (1)                         
                                  GL3187-DOB-CHILD  (2)                         
                                  GL3187-DOB-CHILD  (3)                         
                                  GL3187-DOB-CHILD  (4)                         
                                  GL3187-DOB-CHILD  (5)                         
                                  GL3187-DOB-CHILD  (6)                         
                                  GL3187-DOB-CHILD  (7)                         
                                  GL3187-DOB-CHILD  (8).                        
                                                                                
           IF  GCCC3187-CHILD-SPOUSE-IND  =  '1'                                
               IF GCCCFEXT-LANG  =  'F'                                         
                   MOVE  'conj.'  TO  GL3187-CHILD-SPOUSE-IND                   
               ELSE                                                             
                   MOVE  'spouse' TO  GL3187-CHILD-SPOUSE-IND                   
               END-IF                                                           
             ELSE                                                               
           IF  GCCC3187-CHILD-SPOUSE-IND  =  '0'                                
               IF GCCCFEXT-LANG  =  'F'                                         
                   MOVE  'enfant'  TO  GL3187-CHILD-SPOUSE-IND                  
               ELSE                                                             
                   MOVE  'child'   TO  GL3187-CHILD-SPOUSE-IND                  
               END-IF                                                           
             ELSE                                                               
               MOVE  '??????'  TO  GL3187-CHILD-SPOUSE-IND.                     
                                                                                
           PERFORM 3700-FORMAT-RECURRING-GL3187 THRU 3700-EXIT                  
             VARYING  WS-CHILD-SUBSCRIPT                                        
                FROM  1  BY  1                                                  
                  UNTIL WS-CHILD-SUBSCRIPT  >   8.                              
                                                                                
           MOVE 'N'                     TO  WS-STOP-SUBSCRIPT-MKR.              
                                                                                
           PERFORM 3800-FORMAT-RECURRING-GL3187 THRU 3800-EXIT                  
             VARYING  WS-BENEFIT-SUBSCRIPT                                      
                FROM  1  BY  1                                                  
                  UNTIL WS-BENEFIT-SUBSCRIPT  >   6                             
                    OR  WS-STOP-SUBSCRIPT.                                      
                                                                                
                                                                                
      * RELEASE 3.2 GL3187-TERMINATE-ALL-COV NO LONGER USED                     
           MOVE SPACES                  TO GL3187-TERMINATE-ALL-COV.            
      *                                                                         
                                                                                
           MOVE GCCC3187-TERM-ALL-DATE  TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3187-TERMINATE-ALL-DATE.           
                                                                                
           MOVE  GCCC3187-REASONFOR     TO GL3187-TERMINATE-ALL-REASON.         
                                                                                
      *                                                                         
           IF  GCCC3187-QUEBEC        =  '0'                                    
               MOVE 'X'                 TO GL3187-QUEBEC-AGE-BOX2               
             ELSE                                                               
           IF  GCCC3187-QUEBEC        =  '1'                                    
               MOVE 'X'                 TO GL3187-QUEBEC-AGE-BOX1.              
                                                                                
      *                                                                         
           MOVE  GCCC3187-BANK-NAME          TO GL3187-BANK-NAME.               
           MOVE  GCCC3187-BANK-TRANSIT       TO GL3187-BANK-TRANSIT.            
           MOVE  GCCC3187-BANK-INSTITUTION   TO GL3187-BANK-INSTITUTION.        
           MOVE  GCCC3187-BANK-ACCOUNT       TO GL3187-BANK-ACCOUNT.            
           MOVE  GCCC3187-EMAIL-ADRS-WORK    TO GL3187-EMAIL-ADRS-WORK.         
           MOVE  GCCC3187-EMAIL-ADRS-HOME    TO GL3187-EMAIL-ADRS-HOME.         
      *                                                                         
                                                                                
           MOVE  GCCC3187-TODAY         TO WS-DCPI-DATE.                        
                                                                                
                                                                                
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3187-ENGLISH-TODAY.                
                                                                                
      * RELEASE 3.2 NEW FIELDS ADDED                                            
                                                                                
           IF GCCC3187-SEC2-CHECKBOX     = '1'                                  
               MOVE 'X'                  TO GL3187-SEC2-CHECKBOX                
           END-IF.                                                              
                                                                                
           IF GCCC3187-SEC3-CHECKBOX     = '1'                                  
               MOVE 'X'                  TO GL3187-SEC3-CHECKBOX                
           END-IF.                                                              
                                                                                
           IF GCCC3187-SEC4-CHECKBOX     = '1'                                  
               MOVE 'X'                  TO GL3187-SEC4-CHECKBOX                
           END-IF.                                                              
                                                                                
           IF GCCC3187-SEC5-CHECKBOX     = '1'                                  
               MOVE 'X'                  TO GL3187-SEC5-CHECKBOX                
           END-IF.                                                              
                                                                                
           IF GCCC3187-SEC6-CHECKBOX     = '1'                                  
               MOVE 'X'                  TO GL3187-SEC6-CHECKBOX                
           END-IF.                                                              
                                                                                
           IF GCCC3187-SEC7-CHECKBOX     = '1'                                  
               MOVE 'X'                  TO GL3187-SEC7-CHECKBOX                
           END-IF.                                                              
                                                                                
           IF GCCC3187-SEC8-CHECKBOX     = '1'                                  
               MOVE 'X'                  TO GL3187-SEC8-CHECKBOX                
           END-IF.                                                              
                                                                                
                                                                                
           MOVE  GL3187-RECORD-P1       TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
           MOVE  GL3187-RECORD-P2       TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
                                                                                
      *                                                                         
       3600-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       3700-FORMAT-RECURRING-GL3187.                                            
      *                                                                         
      *                                                                         
                                                                                
           PERFORM 3750-FORMAT-RECURRING-GL3187  THRU  3750-EXIT.               
                                                                                
       3700-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3750-FORMAT-RECURRING-GL3187.                                            
      *                                                                         
                                                                                
                                                                                
           MOVE GCCC3187-CHILD-CHG-CODE (WS-CHILD-SUBSCRIPT)                    
                                          TO WS-IN-CHG-CODE.                    
           PERFORM 6300-CONV-CHG-CODE THRU 6300-EXIT.                           
           MOVE  WS-OUT-CHG-CODE                                                
                    TO GL3187-CHG-CODE-CHILD (WS-CHILD-SUBSCRIPT).              
                                                                                
           MOVE GCCC3187-CHILD-EFF-DATE (WS-CHILD-SUBSCRIPT)                    
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                       GL3187-EFF-CHILD (WS-CHILD-SUBSCRIPT).                   
                                                                                
      *                                                                         
           MOVE  GCCC3187-CHILD-NAME (WS-CHILD-SUBSCRIPT) TO                    
                       GL3187-NAME-CHILD (WS-CHILD-SUBSCRIPT).                  
                                                                                
      *                                                                         
           MOVE  GCCC3187-CHILD-DOB     (WS-CHILD-SUBSCRIPT)                    
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                       GL3187-DOB-CHILD (WS-CHILD-SUBSCRIPT).                   
                                                                                
      *                                                                         
           IF  GCCC3187-CHILD-GENDER  (WS-CHILD-SUBSCRIPT)                      
                                      =  '0'                                    
               MOVE 'X' TO GL3187-CHILD-MALE   (WS-CHILD-SUBSCRIPT)             
             ELSE                                                               
           IF  GCCC3187-CHILD-GENDER  (WS-CHILD-SUBSCRIPT)                      
                                      =  '1'                                    
               MOVE 'X' TO GL3187-CHILD-FEMALE (WS-CHILD-SUBSCRIPT).            
                                                                                
      *                                                                         
                                                                                
                                                                                
           MOVE GCCC3187-CHILD-RELSHP (WS-CHILD-SUBSCRIPT)                      
                                         TO WS-IN-RELSHP.                       
           PERFORM 6200-CONV-RELSHP   THRU  6200-EXIT.                          
           MOVE WS-OUT-RELSHP            TO                                     
                            GL3187-CHILD-RELSHP (WS-CHILD-SUBSCRIPT).           
                                                                                
                                                                                
           IF  GCCC3187-CHILD-STUDENT (WS-CHILD-SUBSCRIPT) =  '0'               
               MOVE 'X' TO                                                      
                    GL3187-CHILD-STUDENT-NO  (WS-CHILD-SUBSCRIPT)               
             ELSE                                                               
           IF  GCCC3187-CHILD-STUDENT (WS-CHILD-SUBSCRIPT) =  '1'               
               MOVE 'X' TO                                                      
                    GL3187-CHILD-STUDENT-YES (WS-CHILD-SUBSCRIPT).              
                                                                                
      *                                                                         
           IF  GCCC3187-CHILD-DISAB (WS-CHILD-SUBSCRIPT) =  '0'                 
               MOVE 'X' TO                                                      
                    GL3187-CHILD-DISABLED-NO (WS-CHILD-SUBSCRIPT)               
             ELSE                                                               
           IF  GCCC3187-CHILD-DISAB (WS-CHILD-SUBSCRIPT) =  '1'                 
               MOVE 'X' TO                                                      
                    GL3187-CHILD-DISABLED-YES (WS-CHILD-SUBSCRIPT).             
      *                                                                         
      *                                                                         
                                                                                
       3750-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       3800-FORMAT-RECURRING-GL3187.                                            
      *     INITIALLY GCCC3187-COV-BENEFIT WAS TO IDENTIFY                      
      *     DLIF, EHC OR DENT NOW THIS IS IDENTIFIED BY SUBSCTRIPT              
                                                                                
                                                                                
           IF  WS-BENEFIT-SUBSCRIPT   =   1                                     
      *                =  'EHC '                                                
               PERFORM  3820-FORMAT-EHC-BENEFIT  THRU 3820-EXIT                 
             ELSE                                                               
           IF  WS-BENEFIT-SUBSCRIPT   =   2                                     
      *                =  'DENT'                                                
               PERFORM  3830-FORMAT-DENT-BENEFIT THRU 3830-EXIT                 
             ELSE                                                               
           IF  WS-BENEFIT-SUBSCRIPT   =   3                                     
      *                =  'DLIF'                                                
               PERFORM  3810-FORMAT-LIFE-BENEFIT THRU 3810-EXIT                 
             ELSE                                                               
               MOVE 'Y'   TO  WS-STOP-SUBSCRIPT-MKR.                            
                                                                                
       3800-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3810-FORMAT-LIFE-BENEFIT.                                                
                                                                                
                                                                                
           IF   GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                     
                                      =  '1'                                    
               MOVE 'X'                 TO GL3187-DEP-LIFE.                     
                                                                                
           IF   GCCC3187-COV-REFUSAL-IND (WS-BENEFIT-SUBSCRIPT)                 
                                      =  '1'                                    
               MOVE 'X'                 TO GL3187-REFUSE-LIFE.                  
                                                                                
           MOVE GCCC3187-COV-REFUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)              
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                                  GL3187-REFUSE-LIFE-DATE-E.                    
                                                                                
                                                                                
                                                                                
       3810-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3820-FORMAT-EHC-BENEFIT.                                                 
                                                                                
           IF  GCCC3187-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '0'                                    
               MOVE 'X'                 TO GL3187-SPOUSE-HLTH-COV-N             
             ELSE                                                               
           IF  GCCC3187-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '1'                                    
               MOVE 'X'                 TO GL3187-SPOUSE-HLTH-COV-Y.            
                                                                                
           IF  GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '1'                                    
               MOVE 'X'                 TO  GL3187-EHC-BOX1                     
             ELSE                                                               
           IF  GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '2'                                    
               MOVE 'X'                 TO  GL3187-EHC-BOX2                     
             ELSE                                                               
           IF  GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '3'                                    
               MOVE 'X'                 TO  GL3187-EHC-BOX3                     
             ELSE                                                               
           IF  GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '4'                                    
               MOVE 'X'                 TO  GL3187-EHC-BOX4.                    
                                                                                
                                                                                
           IF  GCCC3187-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '1'                                    
               MOVE 'X'                 TO  GL3187-SPOUSE-HC-SCOPE-1            
             ELSE                                                               
           IF  GCCC3187-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '2'                                    
               MOVE 'X'                 TO  GL3187-SPOUSE-HC-SCOPE-2            
             ELSE                                                               
           IF  GCCC3187-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '3'                                    
               MOVE 'X'                 TO  GL3187-SPOUSE-HC-SCOPE-3            
             ELSE                                                               
           IF  GCCC3187-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '4'                                    
               MOVE 'X'                 TO  GL3187-SPOUSE-HC-SCOPE-4.           
                                                                                
                                                                                
           MOVE  GCCC3187-COV-SPOUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)             
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                               GL3187-SPOUSE-HC-EFF-DATE.                       
                                                                                
           IF  GCCC3187-COV-REFUSAL-IND (WS-BENEFIT-SUBSCRIPT)                  
                                      =  '1'                                    
               MOVE 'X'                 TO GL3187-REFUSE-EHC-BOX1               
             ELSE                                                               
           IF  GCCC3187-COV-REFUSAL-IND (WS-BENEFIT-SUBSCRIPT)                  
                                      =  '2'                                    
               MOVE 'X'                 TO GL3187-REFUSE-EHC-BOX2.              
                                                                                
           MOVE GCCC3187-COV-REFUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)              
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO  GL3187-REFUSE-EHC-DATE-E.           
                                                                                
                                                                                
       3820-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3830-FORMAT-DENT-BENEFIT.                                                
                                                                                
           IF  GCCC3187-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '0'                                    
               MOVE 'X'                 TO GL3187-SPOUSE-DENT-COV-N             
             ELSE                                                               
           IF  GCCC3187-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '1'                                    
               MOVE 'X'                 TO GL3187-SPOUSE-DENT-COV-Y.            
                                                                                
                                                                                
           IF  GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '1'                                    
               MOVE 'X'                 TO  GL3187-DENT-BOX1                    
             ELSE                                                               
           IF  GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '2'                                    
               MOVE 'X'                 TO  GL3187-DENT-BOX2                    
             ELSE                                                               
           IF  GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '3'                                    
               MOVE 'X'                 TO  GL3187-DENT-BOX3                    
             ELSE                                                               
           IF  GCCC3187-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '4'                                    
               MOVE 'X'                 TO  GL3187-DENT-BOX4.                   
                                                                                
           MOVE  GCCC3187-COV-SPOUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)             
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                               GL3187-SPOUSE-DC-EFF-DATE.                       
                                                                                
                                                                                
           IF  GCCC3187-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '1'                                    
               MOVE 'X'                 TO  GL3187-SPOUSE-DC-SCOPE-1            
             ELSE                                                               
           IF  GCCC3187-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '2'                                    
               MOVE 'X'                 TO  GL3187-SPOUSE-DC-SCOPE-2            
             ELSE                                                               
           IF  GCCC3187-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '3'                                    
               MOVE 'X'                 TO  GL3187-SPOUSE-DC-SCOPE-3            
             ELSE                                                               
           IF  GCCC3187-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '4'                                    
               MOVE 'X'                 TO  GL3187-SPOUSE-DC-SCOPE-4.           
                                                                                
                                                                                
           IF  GCCC3187-COV-REFUSAL-IND (WS-BENEFIT-SUBSCRIPT)                  
                                      =  '1'                                    
               MOVE 'X'                 TO GL3187-REFUSE-DENT-BOX1              
             ELSE                                                               
           IF  GCCC3187-COV-REFUSAL-IND (WS-BENEFIT-SUBSCRIPT)                  
                                      =  '2'                                    
               MOVE 'X'                 TO GL3187-REFUSE-DENT-BOX2.             
                                                                                
           MOVE GCCC3187-COV-REFUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)              
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO  GL3187-REFUSE-DENT-DATE-E.          
                                                                                
      *                                                                         
                                                                                
       3830-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       3900-FORMAT-GL3574.                                                      
      ******************************************************************        
      * PRODUCE THE GL3574 FORMAT                                               
      ******************************************************************        
      *                                                                         
           INITIALIZE GL3574-RECORD.                                            
      *                                                                         
                                                                                
           MOVE  GCCCFEXT-LANG          TO WS-DCP-LANG                          
                                           WS-RELSHP-LANG.                      
           MOVE  '1'                    TO GL3574-CC-P1.                        
      *                                                                         
      *                                                                         
           IF  GCCCFEXT-LANG   =  'F'                                           
               ADD 1 TO WS-BANNER-GL3574-FR-TOT                                 
               MOVE  '56'               TO GL3574-FORM-TYPE-P1                  
                                           GL3574-FORM-TYPE-P2                  
             ELSE                                                               
               ADD 1 TO WS-BANNER-GL3574-ENG-TOT                                
               MOVE  '06'               TO GL3574-FORM-TYPE-P1                  
                                           GL3574-FORM-TYPE-P2.                 
                                                                                
                                                                                
           MOVE  GCCC3574-DATE-TIME     TO GL3574-DATE-TIME.                    
                                                                                
           MOVE  GCCC3574-PLAN    (1)   TO GL3574-PLAN    (1).                  
           MOVE  GCCC3574-PLAN    (2)   TO GL3574-PLAN    (2).                  
           MOVE  GCCC3574-PLAN    (3)   TO GL3574-PLAN    (3).                  
           MOVE  GCCC3574-PLAN    (4)   TO GL3574-PLAN    (4).                  
           MOVE  GCCC3574-PLAN    (5)   TO GL3574-PLAN    (5).                  
           MOVE  GCCC3574-ACCOUNT (1)   TO GL3574-ACCOUNT (1).                  
           MOVE  GCCC3574-ACCOUNT (2)   TO GL3574-ACCOUNT (2).                  
           MOVE  GCCC3574-ACCOUNT (3)   TO GL3574-ACCOUNT (3).                  
           MOVE  GCCC3574-ACCOUNT (4)   TO GL3574-ACCOUNT (4).                  
           MOVE  GCCC3574-ACCOUNT (5)   TO GL3574-ACCOUNT (5).                  
           MOVE  GCCC3574-DIV     (1)   TO GL3574-DIV     (1).                  
           MOVE  GCCC3574-DIV     (2)   TO GL3574-DIV     (2).                  
           MOVE  GCCC3574-DIV     (3)   TO GL3574-DIV     (3).                  
           MOVE  GCCC3574-DIV     (4)   TO GL3574-DIV     (4).                  
           MOVE  GCCC3574-DIV     (5)   TO GL3574-DIV     (5).                  
           MOVE  GCCC3574-CERT          TO GL3574-CERT.                         
           MOVE  GCCC3574-SPONSOR       TO GL3574-SPONSOR.                      
                                                                                
           MOVE  GCCC3574-HIRE-DATE     TO  WS-DCPI-DATE.                       
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO  GL3574-HIRE-DATE.                   
                                                                                
           MOVE  GCCC3574-PREV-EMPL-DATE                                        
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3574-PREV-EMPL-DATE.               
                                                                                
           MOVE  GCCC3574-REHIRE-DATE   TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3574-REHIRE-DATE.                  
                                                                                
           IF GCCC3574-WAITING-PERIOD-IND = '0'                                 
              MOVE 'X'                   TO GL3574-WAITING-PERIOD-NO            
           ELSE IF GCCC3574-WAITING-PERIOD-IND = '1'                            
              MOVE 'X'                   TO GL3574-WAITING-PERIOD-YES.          
      *ADD BANK DETAILS AND EMAIL ADDRESS , TL236472                            
           MOVE GCCC3574-BANK-NAME        TO GL3574-BANK-NAME.                  
           MOVE GCCC3574-BANK-TRANSIT     TO GL3574-BANK-TRANSIT.               
           MOVE GCCC3574-BANK-INSTITUTION TO GL3574-BANK-INSTITUTION.           
           MOVE GCCC3574-BANK-ACCOUNT     TO GL3574-BANK-ACCOUNT.               
           MOVE GCCC3574-EMAIL-ADRS-WORK  TO GL3574-EMAIL-ADRS-WORK.            
           MOVE GCCC3574-EMAIL-ADRS-HOME  TO GL3574-EMAIL-ADRS-HOME.            
      * CALL GACCOCC TO FIND GIPSY CODE WHICH MATCHES THE OCCUPATION            
      * SUBMITTED                                                               
                                                                                
           SET GACCOCC-DESC-TO-CODE      TO TRUE.                               
           IF GCCCFEXT-LANG              =  'E'                                 
               MOVE GCCC3574-OCC         TO GACCOCC-ENG-OCC                     
               SET GACCOCC-ENGLISH       TO TRUE                                
           ELSE                                                                 
               MOVE GCCC3574-OCC         TO GACCOCC-FR-OCC                      
               SET GACCOCC-FRENCH        TO TRUE                                
           END-IF.                                                              
                                                                                
           CALL WS-GACPOCC              USING GACCOCC-RECORD.                   
                                                                                
           IF GACCOCC-SUCCESSFUL                                                
               MOVE GACCOCC-GIPSY-CODE  TO WS-GIPSY-CODE                        
               MOVE WS-GIPSY-CODE-CHAR2 TO WS-OCCUPATION-CODE                   
           ELSE                                                                 
               MOVE SPACES              TO WS-OCCUPATION-CODE                   
           END-IF.                                                              
                                                                                
           MOVE  GCCC3574-OCC           TO WS-OCCUPATION-DESC.                  
           MOVE  WS-OCCUPATION          TO GL3574-OCCUPATION.                   
                                                                                
           MOVE  GCCC3574-CLASS   (1)   TO GL3574-CLASS (1).                    
           MOVE  GCCC3574-CLASS   (2)   TO GL3574-CLASS (2).                    
           MOVE  GCCC3574-CLASS   (3)   TO GL3574-CLASS (3).                    
           MOVE  GCCC3574-CLASS   (4)   TO GL3574-CLASS (4).                    
           MOVE  GCCC3574-CLASS   (5)   TO GL3574-CLASS (5).                    
                                                                                
           MOVE  GCCC3574-HOURS         TO GL3574-HOURS                         
           MOVE  GCCC3574-EARNINGS      TO GL3574-EARNINGS              .       
                                                                                
           IF  GCCC3574-EVIDENCE      =  '0'                                    
               MOVE 'X'                 TO GL3574-EVIDENCE-REQD-NO              
             ELSE                                                               
           IF  GCCC3574-EVIDENCE      =  '1'                                    
               MOVE 'X'                 TO GL3574-EVIDENCE-REQD-YES.            
                                                                                
           MOVE  GCCC3574-EMP-NAME      TO GL3574-MBR-NAME.                     
                                                                                
           MOVE  GCCC3574-EMP-DOB       TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3574-MBR-DOB.                      
                                                                                
           IF  GCCC3574-EMP-GENDER    =  '0'                                    
               MOVE 'X'                 TO GL3574-MBR-MALE                      
             ELSE                                                               
           IF  GCCC3574-EMP-GENDER    =  '1'                                    
               MOVE 'X'                 TO GL3574-MBR-FEMALE.                   
                                                                                
           MOVE  GCCC3574-EMP-PROV      TO GL3574-MBR-PROV.                     
                                                                                
      *                                                                         
           IF  GCCC3574-EMP-LANG      =  '2'                                    
               MOVE 'X'                 TO GL3574-MBR-LANG-FRC                  
             ELSE                                                               
               MOVE 'X'                 TO GL3574-MBR-LANG-ENG.                 
                                                                                
           MOVE GCCC3574-EMP-STREET     TO GL3574-MBR-STREET.                   
           MOVE GCCC3574-EMP-CITY       TO GL3574-MBR-CITY.                     
           MOVE GCCC3574-EMP-ADDR-PROV  TO GL3574-MBR-ADDR-PROV.                
           MOVE GCCC3574-EMP-POSTCODE   TO GL3574-MBR-POSTCODE.                 
                                                                                
           IF  GCCC3574-QUEBEC-AGE    =  '1'                                    
               MOVE 'X'                 TO GL3574-QUEBEC-AGE-BOX1               
             ELSE                                                               
           IF  GCCC3574-QUEBEC-AGE    =  '2'                                    
               MOVE 'X'                 TO GL3574-QUEBEC-AGE-BOX2.              
                                                                                
                                                                                
           MOVE  GCCC3574-SPOUSE-DOB    TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3574-SPOUSE-DOB.                   
                                                                                
           IF GCCC3574-SPOUSE-IND        = '1'                                  
               MOVE 'X'                  TO GL3574-SPOUSE-IND                   
           END-IF.                                                              
                                                                                
           IF GCCC3574-COMN-LAW-IND      = '0'                                  
               MOVE 'X'                  TO GL3574-COMN-LAW-IND-NO              
           ELSE                                                                 
               IF GCCC3574-COMN-LAW-IND  = '1'                                  
                   MOVE 'X'              TO GL3574-COMN-LAW-IND-YES             
               END-IF                                                           
           END-IF.                                                              
                                                                                
           MOVE GCCC3574-COMMON-LAW-DATE TO WS-DCPI-DATE.                       
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                                   GL3574-RELATIONSHIP-START-DATE.              
                                                                                
                                                                                
           MOVE  GCCC3574-REGION          TO  WS-TEMP-REG-IN.                   
           PERFORM   7500-REGION-FORMAT  THRU  7500-EXIT.                       
           MOVE  WS-TEMP-REGION           TO  GL3574-REGION-P1                  
                                              GL3574-REGION-P2.                 
                                                                                
                                                                                
           MOVE  GCCC3574-SPOUSE-NAME   TO GL3574-SPOUSE-NAME.                  
                                                                                
           MOVE  GCCC3574-FAM-SPOUSE-DOB    TO  WS-DCPI-DATE.                   
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3574-FAMILY-SPOUSE-DOB.            
                                                                                
           IF  GCCC3574-FAM-SPOUSE-GENDER =  '0'                                
               MOVE 'X'                 TO GL3574-FAM-SPOUSE-MALE               
             ELSE                                                               
           IF  GCCC3574-FAM-SPOUSE-GENDER =  '1'                                
               MOVE 'X'                 TO GL3574-FAM-SPOUSE-FEMALE.            
                                                                                
           MOVE GCCC3574-FAM-SPOUSE-RELSHP                                      
                                         TO WS-IN-RELSHP.                       
           PERFORM 6200-CONV-RELSHP   THRU  6200-EXIT.                          
           MOVE WS-OUT-RELSHP            TO GL3574-FAM-SPOUSE-RELSHP.           
                                                                                
                                                                                
           MOVE 'N'                     TO WS-STOP-SUBSCRIPT-MKR.               
                                                                                
           MOVE  SPACES       TO  GL3574-DOB-CHILD  (1)                         
                                  GL3574-DOB-CHILD  (2)                         
                                  GL3574-DOB-CHILD  (3)                         
                                  GL3574-DOB-CHILD  (4)                         
                                  GL3574-DOB-CHILD  (5).                        
                                                                                
           PERFORM 3910-FORMAT-CHILD-GL3574 THRU 3910-EXIT                      
             VARYING  WS-CHILD-SUBSCRIPT                                        
                FROM  1  BY  1                                                  
                  UNTIL WS-CHILD-SUBSCRIPT  >   5.                              
                                                                                
           MOVE 'N'                     TO WS-STOP-SUBSCRIPT-MKR.               
                                                                                
           PERFORM 3920-FORMAT-BENEFIT-GL3574 THRU 3920-EXIT                    
             VARYING  WS-BENEFIT-SUBSCRIPT                                      
                FROM  1  BY  1                                                  
                  UNTIL WS-BENEFIT-SUBSCRIPT  >   6                             
                    OR  WS-STOP-SUBSCRIPT.                                      
                                                                                
           PERFORM 3960-FORMAT-HCSA-GL3574 THRU 3960-EXIT.                      
                                                                                
           IF  GCCC3574-BEN           =  '1'                                    
               MOVE 'X'                 TO GL3574-BENEFICIARY-REQ.              
                                                                                
      *                                                                         
           MOVE GCCC3574-COMMENTS       TO GL3574-COMMENTS.                     
      *                                                                         
                                                                                
           MOVE  GCCC3574-TODAY         TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO GL3574-ENGLISH-TODAY.                
                                                                                
                                                                                
           MOVE  GL3574-RECORD-P1       TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
           MOVE  GL3574-RECORD-P2       TO WS-OUT-AFP-RECORD.                   
           PERFORM 8000-WRITE-AFP-OUTPUT THRU 8000-EXIT.                        
                                                                                
       3900-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
                                                                                
                                                                                
                                                                                
       3910-FORMAT-CHILD-GL3574.                                                
      ******************************************************************        
      * PRODUCE THE RECURRING GL3574 FORMAT FIELDS - DEPENDENTS                 
      ******************************************************************        
                                                                                
           MOVE  GCCC3574-CHILD-NAME (WS-CHILD-SUBSCRIPT)                       
                   TO  GL3574-NAME-CHILD (WS-CHILD-SUBSCRIPT).                  
                                                                                
                                                                                
           MOVE  GCCC3574-CHILD-DOB (WS-CHILD-SUBSCRIPT)                        
                                          TO  WS-DCPI-DATE.                     
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE                                                   
                   TO  GL3574-DOB-CHILD  (WS-CHILD-SUBSCRIPT).                  
                                                                                
      *                                                                         
           IF   GCCC3574-CHILD-GENDER (WS-CHILD-SUBSCRIPT) = '0'                
               MOVE 'X'  TO  GL3574-CHILD-MALE (WS-CHILD-SUBSCRIPT)             
             ELSE                                                               
           IF   GCCC3574-CHILD-GENDER (WS-CHILD-SUBSCRIPT) = '1'                
               MOVE 'X'  TO  GL3574-CHILD-FEMALE (WS-CHILD-SUBSCRIPT).          
                                                                                
           MOVE GCCC3574-CHILD-RELSHP (WS-CHILD-SUBSCRIPT)                      
                                         TO WS-IN-RELSHP.                       
           PERFORM 6200-CONV-RELSHP   THRU  6200-EXIT.                          
           MOVE WS-OUT-RELSHP            TO                                     
                             GL3574-CHILD-RELSHP (WS-CHILD-SUBSCRIPT).          
                                                                                
           IF   GCCC3574-CHILD-STUDENT (WS-CHILD-SUBSCRIPT) = '0'               
               MOVE 'X'  TO                                                     
                       GL3574-CHILD-STUDENT-NO  (WS-CHILD-SUBSCRIPT)            
             ELSE                                                               
           IF   GCCC3574-CHILD-STUDENT (WS-CHILD-SUBSCRIPT) = '1'               
               MOVE 'X'  TO                                                     
                       GL3574-CHILD-STUDENT-YES (WS-CHILD-SUBSCRIPT).           
      *                                                                         
           IF   GCCC3574-CHILD-DISAB  (WS-CHILD-SUBSCRIPT)  = '0'               
               MOVE 'X'  TO                                                     
                       GL3574-CHILD-DISABLED-NO  (WS-CHILD-SUBSCRIPT)           
             ELSE                                                               
           IF   GCCC3574-CHILD-DISAB  (WS-CHILD-SUBSCRIPT)  = '1'               
               MOVE 'X'  TO                                                     
                       GL3574-CHILD-DISABLED-YES (WS-CHILD-SUBSCRIPT).          
                                                                                
      *                                                                         
       3910-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
                                                                                
       3920-FORMAT-BENEFIT-GL3574.                                              
      *    OCCURS 6                                                             
      *     INITIALLY GCCC3574-COV-BENEFIT WAS TO IDENTIFY                      
      *     DLIF, EHC OR DENT NOW THIS IS IDENTIFIED BY SUBSCTRIPT              
                                                                                
                                                                                
           IF  WS-BENEFIT-SUBSCRIPT   =   1                                     
      *                =  'EHC '                                                
               PERFORM  3940-FORMAT-EHC-GL3574   THRU 3940-EXIT                 
             ELSE                                                               
           IF  WS-BENEFIT-SUBSCRIPT   =   2                                     
      *                =  'DENT'                                                
               PERFORM  3950-FORMAT-DENT-GL3574  THRU 3950-EXIT                 
             ELSE                                                               
           IF  WS-BENEFIT-SUBSCRIPT   =   3                                     
      *                =  'DLIF'                                                
               PERFORM  3930-FORMAT-LIFE-GL3574  THRU 3930-EXIT                 
             ELSE                                                               
               MOVE 'Y'   TO  WS-STOP-SUBSCRIPT-MKR.                            
                                                                                
       3920-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       3930-FORMAT-LIFE-GL3574.                                                 
                                                                                
           IF   GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                     
                                      =  '0'                                    
               MOVE 'X'                 TO GL3574-DEP-LIFE-NO                   
             ELSE                                                               
           IF   GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                     
                                      =  '1'                                    
               MOVE 'X'                 TO GL3574-DEP-LIFE-YES.                 
                                                                                
       3930-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       3940-FORMAT-EHC-GL3574.                                                  
                                                                                
           IF  GCCC3574-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '0'                                    
               MOVE 'X'                 TO GL3574-SPOUSE-HLTH-COV-NO            
             ELSE                                                               
           IF  GCCC3574-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '1'                                    
               MOVE 'X'                 TO GL3574-SPOUSE-HLTH-COV-YES.          
                                                                                
           IF  GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '1'                                    
               MOVE 'X'                 TO  GL3574-EHC-BOX1                     
             ELSE                                                               
           IF  GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '2'                                    
               MOVE 'X'                 TO  GL3574-EHC-BOX2                     
             ELSE                                                               
           IF  GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '3'                                    
               MOVE 'X'                 TO  GL3574-EHC-BOX3                     
             ELSE                                                               
           IF  GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '4'                                    
               MOVE 'X'                 TO  GL3574-EHC-BOX4.                    
                                                                                
                                                                                
           IF  GCCC3574-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '1'                                    
               MOVE 'X'                 TO  GL3574-SPOUSE-HC-SCOPE-1            
             ELSE                                                               
           IF  GCCC3574-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '2'                                    
               MOVE 'X'                 TO  GL3574-SPOUSE-HC-SCOPE-2            
             ELSE                                                               
           IF  GCCC3574-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '3'                                    
               MOVE 'X'                 TO  GL3574-SPOUSE-HC-SCOPE-3            
             ELSE                                                               
           IF  GCCC3574-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '4'                                    
               MOVE 'X'                 TO  GL3574-SPOUSE-HC-SCOPE-4.           
                                                                                
                                                                                
           MOVE  GCCC3574-COV-SPOUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)             
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                               GL3574-SPOUSE-HC-EFF-DATE.                       
                                                                                
       3940-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       3950-FORMAT-DENT-GL3574.                                                 
                                                                                
           IF  GCCC3574-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '0'                                    
               MOVE 'X'                 TO GL3574-SPOUSE-DENT-COV-N             
             ELSE                                                               
           IF  GCCC3574-COV-SPOUS-IND (WS-BENEFIT-SUBSCRIPT)                    
                                      =  '1'                                    
               MOVE 'X'                 TO GL3574-SPOUSE-DENT-COV-Y.            
                                                                                
                                                                                
           IF  GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '1'                                    
               MOVE 'X'                 TO  GL3574-DENT-BOX1                    
             ELSE                                                               
           IF  GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '2'                                    
               MOVE 'X'                 TO  GL3574-DENT-BOX2                    
             ELSE                                                               
           IF  GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '3'                                    
               MOVE 'X'                 TO  GL3574-DENT-BOX3                    
             ELSE                                                               
           IF  GCCC3574-COV-SEL-IND (WS-BENEFIT-SUBSCRIPT)                      
                                      =  '4'                                    
               MOVE 'X'                 TO  GL3574-DENT-BOX4.                   
                                                                                
           MOVE  GCCC3574-COV-SPOUS-EFF-DATE (WS-BENEFIT-SUBSCRIPT)             
                                        TO WS-DCPI-DATE.                        
           PERFORM  7000-DATE-FORMAT  THRU  7000-EXIT.                          
           MOVE  WS-DCPO-DATE           TO                                      
                               GL3574-SPOUSE-DC-EFF-DATE.                       
                                                                                
                                                                                
           IF  GCCC3574-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '1'                                    
               MOVE 'X'                 TO  GL3574-SPOUSE-DC-SCOPE-1            
             ELSE                                                               
           IF  GCCC3574-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '2'                                    
               MOVE 'X'                 TO  GL3574-SPOUSE-DC-SCOPE-2            
             ELSE                                                               
           IF  GCCC3574-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '3'                                    
               MOVE 'X'                 TO  GL3574-SPOUSE-DC-SCOPE-3            
             ELSE                                                               
           IF  GCCC3574-COV-SPOUS-PLAN (WS-BENEFIT-SUBSCRIPT)                   
                                      =  '4'                                    
               MOVE 'X'                 TO  GL3574-SPOUSE-DC-SCOPE-4.           
                                                                                
                                                                                
       3950-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       3960-FORMAT-HCSA-GL3574.                                                 
                                                                                
           IF GCCC3574-HCSA-IND = '0'                                           
              MOVE 'X'                    TO GL3574-HCSA-NO                     
           ELSE IF GCCC3574-HCSA-IND = '1'                                      
              MOVE 'X'                    TO GL3574-HCSA-YES.                   
                                                                                
           MOVE GCCC3574-HCSA-PLAN-NUMBER TO GL3574-HCSA-PLAN-NUMBER.           
           MOVE GCCC3574-HCSA-ALLOC-AMT   TO GL3574-HCSA-ALLOC-AMT.             
                                                                                
      * DATE CONVERSION NOT NECESSAY                                            
      * DATE BEING RECEIVED IN DDMMMYYYY FORMAT                                 
                                                                                
           MOVE GCCC3574-HCSA-EFF-DATE    TO GL3574-HCSA-EFF-DATE.              
                                                                                
      *    MOVE GCCC3574-HCSA-EFF-DATE    TO WS-DCPI-DATE.                      
      *    PERFORM 7000-DATE-FORMAT  THRU 7000-EXIT.                            
      *    MOVE WS-DCPO-DATE              TO GL3574-HCSA-EFF-DATE.              
                                                                                
                                                                                
       3960-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       4000-CREATE-EXTRACT.                                                     
                                                                                
      *                                                                         
      *      ADD THE FOLLOWING REPORT AS OUTPUT                                 
      *                                                                         
      *                                                                         
      *                                                                         
                                                                                
           IF  GCCCFEXT-FORM-NBR    =  'GLMASS  '                               
               MOVE SPACES                TO  GCCCRPT-CERTIFICATE-NO            
               MOVE ALL '9'               TO  GCCCRPT-GROUP                     
                                              GCCCRPT-DIVISION                  
            ELSE                                                                
               MOVE GCCCFEXT-GROUP        TO  GCCCRPT-GROUP                     
               MOVE GCCCFEXT-DIV          TO  GCCCRPT-DIVISION                  
               MOVE GCCCFEXT-CERT-ID      TO  GCCCRPT-CERTIFICATE-NO.           
                                                                                
           SET  GCCCRPT-DETAIL-REC        TO  TRUE.                             
                                                                                
           MOVE GCCCFEXT-FORM-NBR         TO  GCCCRPT-FORM-NAME.                
           MOVE GCCCFEXT-CONF-NBR         TO  GCCCRPT-CONFIRM-NO.               
           MOVE GCCCFEXT-SEQ-NBR          TO  GCCCRPT-SEQUENCE-NO.              
                                                                                
           IF  GCCCFEXT-FORM-NBR    =  'GL0003  '                               
               MOVE GCCC0003-NAME-1       TO  GCCCRPT-MEMBER-NAME               
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR    =  'GL0005  '                               
               MOVE GCCC0005-MBR-NAME     TO  GCCCRPT-MEMBER-NAME               
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR    =  'GL0514  '                               
               MOVE GCCC0514-MBR-LAST-NME TO  GCCCRPT-MEMBER-NAME               
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR    =  'GL2971  '                               
               MOVE GCCC2971-EMP-NAME     TO  GCCCRPT-MEMBER-NAME               
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR    =  'GL3187  '                               
               MOVE GCCC3187-MBR-NAME     TO  GCCCRPT-MEMBER-NAME               
             ELSE                                                               
           IF  GCCCFEXT-FORM-NBR    =  'GLMASS  '                               
               MOVE GCCCMASS-CLIENT-NAME  TO  GCCCRPT-MEMBER-NAME.              
                                                                                
                                                                                
      * FOR IN-FORCE DISTRIBUTION INCLUDE THE TAT IN THE TITLE                  
                                                                                
           IF  GCCCFEXT-SORT-CAT2    =   'E'                                    
               MOVE  'IN-FORCE EAST'           TO GCCCRPT-DIST-CAT-GRP          
               MOVE  GCCCFEXT-SORT-CAT0        TO WS-DIST-CAT-TAT               
               MOVE  WS-DIST-CAT-TAT-GRP       TO GCCCRPT-DIST-CAT-TAT          
             ELSE                                                               
           IF  GCCCFEXT-SORT-CAT2    =   'W'                                    
               MOVE  'IN-FORCE WEST'           TO GCCCRPT-DIST-CAT-GRP          
               MOVE  GCCCFEXT-SORT-CAT0        TO WS-DIST-CAT-TAT               
               MOVE  WS-DIST-CAT-TAT-GRP       TO GCCCRPT-DIST-CAT-TAT          
             ELSE                                                               
           IF  GCCCFEXT-SORT-CAT2    =   'G'                                    
               MOVE  ' IN-FORCE GFM'           TO GCCCRPT-DIST-CAT-GRP          
               MOVE  GCCCFEXT-SORT-CAT0        TO WS-DIST-CAT-TAT               
               MOVE  WS-DIST-CAT-TAT-GRP       TO GCCCRPT-DIST-CAT-TAT          
             ELSE                                                               
           IF  GCCCFEXT-SORT-CAT1    =   'M'                                    
             IF GCCCFEXT-SORT-CAT2 =     'A'                                    
                 MOVE '       MONTREAL ALPHA'   TO GCCCRPT-DIST-CAT-GRP         
               ELSE                                                             
             IF GCCCFEXT-SORT-CAT2 =     'S'                                    
                 MOVE '     MONTREAL SIGNATURE' TO GCCCRPT-DIST-CAT-GRP         
               ELSE                                                             
             IF GCCCFEXT-SORT-CAT2 =     'C'                                    
                 MOVE '     MONTREAL CORPORATE' TO GCCCRPT-DIST-CAT-GRP         
               ELSE                                                             
                 MOVE '     MONTREAL UNKNOWN'   TO GCCCRPT-DIST-CAT-GRP P.      
           ELSE                                                                 
             IF GCCCFEXT-SORT-CAT2 =     'A'                                    
                 MOVE '        PENDING ALPHA'   TO GCCCRPT-DIST-CAT-GRP         
               ELSE                                                             
             IF GCCCFEXT-SORT-CAT2 =     'S'                                    
                 MOVE '      PENDING SIGNATURE' TO GCCCRPT-DIST-CAT-GRP         
               ELSE                                                             
             IF GCCCFEXT-SORT-CAT2 =     'C'                                    
                 MOVE '      PENDING CORPORATE' TO GCCCRPT-DIST-CAT-GRP.        
                                                                                
           PERFORM 8750-WRITE-EXT-OUTPUT THRU 8750-EXIT.                        
                                                                                
       4000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       6000-GET-NEXT.                                                           
                                                                                
           MOVE WS-OBTAIN-NEXT          TO WS-GAEDATSR-VERB.                    
           PERFORM  6100-READ-INPUT THRU 6100-EXIT.                             
                                                                                
                                                                                
       6000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6100-READ-INPUT.                                                         
      ******************************************************************        
      *    READ NEXT INPUT RECORD.                                     *        
      ******************************************************************        
           MOVE WS-SORT-FILE-IN-LR     TO LOGICAL-RECORD-NAME.                  
           INITIALIZE INP-RECORD.                                               
                                                                                
           CALL WS-GAEDATSR        USING  WS-GAEDATSR-VERB                      
                                         INP-RECORD                             
                                         ICBM.                                  
                                                                                
           IF NOT LR-STATUS-OK                                                  
               MOVE 'Y'                TO  WS-EOF-MKR.                          
                                                                                
      *    NOTE THE DETAIL MOVE TO THE GCCC-DETAIL                              
      *    IS TO REMOVE THE NEED FOR DUPLICATE COPYBOOKS                        
                                                                                
                                                                                
           IF  WS-EOF                                                           
               MOVE SPACES             TO  WS-NEW-INTERNAL-KEY                  
             ELSE                                                               
               MOVE GCCCFEXT-FORM-DATA   TO  WS-GCCC-DETAIL                     
                                                                                
               MOVE GCCCFEXT-CONT-STATUS TO  WS-NIK-CONT-ST                     
                                                                                
               MOVE GCCCFEXT-SORT-CAT1   TO  WS-NIK-SORT-CAT1                   
                                                                                
               MOVE GCCCFEXT-SORT-CAT2   TO  WS-NIK-SORT-CAT2                   
                                                                                
               MOVE GCCCFEXT-SORT-CAT0   TO  WS-NIK-SORT-CAT0.                  
                                                                                
           IF  WS-INTERNAL-KEY  =  WS-NEW-INTERNAL-KEY                          
               NEXT SENTENCE                                                    
             ELSE                                                               
               MOVE WS-NEW-INTERNAL-KEY  TO  WS-INTERNAL-KEY                    
               MOVE 'Y'                  TO  WS-CHANGE-BANNER-MKR.              
                                                                                
       6100-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       6200-CONV-RELSHP.                                                        
      ******************************************************************        
      *    CONVERT RELATIONSHIP TEXT                                   *        
      ******************************************************************        
                                                                                
                                                                                
           MOVE ' '      TO   WS-OUT-RELSHP.                                    
                                                                                
           IF   WS-RELSHP-LANG           = 'F'                                  
               IF   WS-IN-RELSHP                     = '1'                      
                   MOVE 'M'  TO   WS-OUT-RELSHP                                 
                 ELSE                                                           
               IF   WS-IN-RELSHP                     = '2'                      
                   MOVE 'F'  TO   WS-OUT-RELSHP                                 
                 ELSE                                                           
               IF   WS-IN-RELSHP                     = '3'                      
                   MOVE 'C'  TO   WS-OUT-RELSHP                                 
                 ELSE                                                           
               IF   WS-IN-RELSHP                     = '4'                      
                   MOVE 'E'  TO   WS-OUT-RELSHP                                 
                 ELSE                                                           
                   MOVE ' '  TO   WS-OUT-RELSHP                                 
             ELSE                                                               
               IF   WS-IN-RELSHP                     = '1'                      
                   MOVE 'H'  TO   WS-OUT-RELSHP                                 
                 ELSE                                                           
               IF   WS-IN-RELSHP                     = '2'                      
                   MOVE 'W'  TO   WS-OUT-RELSHP                                 
                 ELSE                                                           
               IF   WS-IN-RELSHP                     = '3'                      
                   MOVE 'S'  TO   WS-OUT-RELSHP                                 
                 ELSE                                                           
               IF   WS-IN-RELSHP                     = '4'                      
                   MOVE 'C'  TO   WS-OUT-RELSHP.                                
                                                                                
                                                                                
       6200-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       6300-CONV-CHG-CODE.                                                      
      ******************************************************************        
      *    CONVERT CHANGE CODE TEXT                                   *         
      ******************************************************************        
                                                                                
           MOVE ' '      TO   WS-OUT-CHG-CODE.                                  
           IF   WS-RELSHP-LANG          = 'F'                                   
               IF   WS-IN-CHG-CODE                   = '1'                      
                   MOVE 'A'  TO   WS-OUT-CHG-CODE                               
                 ELSE                                                           
               IF   WS-IN-CHG-CODE                   = '2'                      
                   MOVE 'M'  TO   WS-OUT-CHG-CODE                               
                 ELSE                                                           
               IF   WS-IN-CHG-CODE                   = '3'                      
                   MOVE 'R'  TO   WS-OUT-CHG-CODE                               
                 ELSE                                                           
                   MOVE ' '  TO   WS-OUT-CHG-CODE                               
             ELSE                                                               
               IF   WS-IN-CHG-CODE                   = '1'                      
                   MOVE 'A'  TO   WS-OUT-CHG-CODE                               
                 ELSE                                                           
               IF   WS-IN-CHG-CODE                   = '2'                      
                   MOVE 'C'  TO   WS-OUT-CHG-CODE                               
                 ELSE                                                           
               IF   WS-IN-CHG-CODE                   = '3'                      
                   MOVE 'D'  TO   WS-OUT-CHG-CODE.                              
                                                                                
                                                                                
       6300-EXIT.                                                               
           EXIT.                                                                
                                                                                
       7000-DATE-FORMAT.                                                        
      ******************************************************************        
      *    FORMAT DATE INTO FRENCH OR ENGLISH PRINT FORMAT                      
      ******************************************************************        
                                                                                
           IF  WS-DCPI-DATE   =  SPACES                                         
               MOVE     WS-DCPI-DATE          TO WS-DCPO-DATE                   
             ELSE                                                               
           IF  WS-DCPI-DATE   =  '00000000'                                     
               MOVE     SPACES                TO WS-DCPO-DATE                   
             ELSE                                                               
           IF  WS-DCP-FRENCH                                                    
               STRING   WS-DCPI-DAY         DELIMITED BY SIZE                   
                        '/'                 DELIMITED BY SIZE                   
                        WS-MONTH-NAME-FRENCH  (WS-DCPI-MONTH-NUM)               
                                            DELIMITED BY SIZE                   
                        '/'                 DELIMITED BY SIZE                   
                        WS-DCPI-YEAR        DELIMITED BY SIZE                   
                                            INTO WS-DCPO-DATE                   
             ELSE                                                               
               STRING   WS-DCPI-DAY         DELIMITED BY SIZE                   
                        '/'                 DELIMITED BY SIZE                   
                        WS-MONTH-NAME-ENGLISH (WS-DCPI-MONTH-NUM)               
                                            DELIMITED BY SIZE                   
                        '/'                 DELIMITED BY SIZE                   
                        WS-DCPI-YEAR        DELIMITED BY SIZE                   
                        ' '                 DELIMITED BY SIZE                   
                                            INTO WS-DCPO-DATE.                  
                                                                                
       7000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       7500-REGION-FORMAT.                                                      
      ******************************************************************        
      *    FORMAT REGION INTO FRENCH OR ENGLISH whole FORMAT                    
      ******************************************************************        
                                                                                
           IF  WS-TEMP-REG-IN        =   'G'                                    
               MOVE  'GFM '               TO  WS-TEMP-REGION                    
             ELSE                                                               
           IF  GCCCFEXT-LANG   =  'F'                                           
               IF  WS-TEMP-REG-IN        =   'E'                                
                   MOVE  'EST'            TO  WS-TEMP-REGION                    
                 ELSE                                                           
               IF  WS-TEMP-REG-IN        =   'O'                                
                   MOVE  'OUEST'          TO  WS-TEMP-REGION                    
                 ELSE                                                           
                   MOVE  ' '              TO  WS-TEMP-REGION                    
             ELSE                                                               
               IF  WS-TEMP-REG-IN        =   'E'                                
                   MOVE  'EAST'           TO  WS-TEMP-REGION                    
                 ELSE                                                           
               IF  WS-TEMP-REG-IN        =   'W'                                
                   MOVE  'WEST'           TO  WS-TEMP-REGION.                   
                                                                                
       7500-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
                                                                                
       8000-WRITE-AFP-OUTPUT.                                                   
      ******************************************************************        
      *    PRODUCE AFP PRINT                                                    
      ******************************************************************        
                                                                                
                                                                                
           MOVE WS-AFP-FILE-OUT-LR     TO LOGICAL-RECORD-NAME.                  
           MOVE WS-STORE-LR            TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR       USING  WS-GAEDATSR-VERB                       
                                          WS-OUT-AFP-RECORD                     
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               DISPLAY 'ERROR WRITING TO OUTPUT FILE'                           
               DISPLAY WS-OUTPUT-AFP-LAYOUT                                     
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
                                                                                
       8000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       8500-WRITE-MASS-OUTPUT.                                                  
      ******************************************************************        
      *    PRODUCE MASS PRINT RECORD                                            
      ******************************************************************        
                                                                                
                                                                                
           MOVE WS-MASS-CHG-OUT-LR     TO LOGICAL-RECORD-NAME.                  
           MOVE WS-STORE-LR            TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR       USING  WS-GAEDATSR-VERB                       
                                         WS-OUT-MASS-RECORD                     
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               DISPLAY 'ERROR WRITING TO OUTPUT FILE'                           
               DISPLAY WS-OUT-MASS-RECORD                                       
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
                                                                                
       8500-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       8750-WRITE-EXT-OUTPUT.                                                   
      ******************************************************************        
      *    PRODUCE EXTRACT LINE                                                 
      ******************************************************************        
                                                                                
                                                                                
           MOVE WS-REPORT-EXTRACT-LR   TO LOGICAL-RECORD-NAME.                  
           MOVE WS-STORE-LR            TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR       USING  WS-GAEDATSR-VERB                       
                                         WS-OUTPUT-EXTRACT-LINE                 
                                         ICBM.                                  
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               DISPLAY 'ERROR WRITING TO EXTRACT FILE'                          
               DISPLAY WS-OUTPUT-EXTRACT-LINE                                   
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
           INITIALIZE WS-OUTPUT-EXTRACT-LINE.                                   
                                                                                
       8750-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                        HCSPSCON
       9000-FINISH.                                                             
      ******************************************************************        
      * CLOSE FILES THAT ARE OPEN...                                            
      ******************************************************************        
                                                                                
           MOVE WS-SORT-FILE-IN-LR     TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               DISPLAY 'ERROR CLOSING INPUT FILE'                               
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
           MOVE WS-AFP-FILE-OUT-LR     TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               DISPLAY 'ERROR CLOSING AFP OUTPUT FILE'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
           MOVE WS-REPORT-EXTRACT-LR   TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               DISPLAY 'ERROR CLOSING EXT OUTPUT FILE'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
           MOVE WS-MASS-CHG-OUT-LR     TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               DISPLAY 'ERROR CLOSING MASS OUTPUT FILE'                         
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                         
                                                                                
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9999-ABEND.                                                              
      ******************************************************************        
      *    TIME TO ABEND.                                                       
      ******************************************************************        
                                                                        12330000
                                                                        12370000
           MOVE +16 TO RETURN-CODE.                                     12380000
                                                                                
           GOBACK.                                                              
                                                                                
                                                                                
       9999-ABEND-EXIT.                                                         
           EXIT.                                                                
