       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPRGNA.                                                 
      *AUTHOR.        FRANK MARUCCI.                                            
      ******************************************************************        
      *    << GROUP BENEFITS INTERNET REGISTRATION LETTER PROCESS >>            
      *                                                                         
      * PROGRAM DESCRIPTION:                                                    
      *     THIS PROGRAM READS A FLAT FILE CREATED FROM GCCPRGNE THAT           
      *   HAS SELECTED ALL CUSTOMERS WHICH HAVE NOT HAD A REGISTRATION          
      *   CONFIRMATION LETTER CREATED.                                          
      *   THIS PROGRAM READS THE CLIENTS II LOCATION FILE TO                    
      *   DETERMINE IF THE WELCOME LETTER IS TO BE SENT TO THE EMPLOYEES        
      *   HOME ADDRESS OR THE PLAN ADMINISTRATORS ADDRESS.                      
      *   A FLAT FILE IS CREATED CONTAINING THE CONRACT, DIVISION,              
      *   CERTIFICATE, CUSTOMER NUMBER, MAILING ADDRESS TYPE AND MAILING        
      *   ADDRESS.  THIS FILE IS THEN SORTED AND PROCESSED                      
      *   BY THE AFP FORMATTING PROGRAM (GCCPRGNF).                             
      *                                                                         
      * CALLING MODULES                                                         
      *     NOT APPLICABLE.                                                     
      *                                                                         
      * CALLED MODULES                                                          
      *     GC2TODAY - GET SYSTEM DATE                                          
      *     GAEDATSR - DATA SERVER                                              
      *                                                                         
      * COPYBOOKS                                                               
      *     XC4CFEMP - CLIENTS II EMPLOYEE RECORDS                              
      *     XC4CFMMA - CLIENTS II EMPLOYEE ADDRESS RECORDS                      
      *     XC4CFLOC - CLIENTS II LOCATION RECORDS                              
      *     XC4CFCOV - CLIENTS II COVERAGE RECORD                               
      *     GARDSVRB - VERBS FOR GAEDATSR                                       
      *     GCCCPEXT - REGISTRATION LETTER REQUEST INPUT FILE                   
      *     GCCCCEXT - REGISTRATION LETTER REQUEST OUTPUT FILE                  
      *     HCSLRNAM - LOGICAL RECORD NAMES                                     
      *                                                                         
      * INPUT  - FLAT FILE OF REGISTRATION LETTER REQUESTS (GCCCPEXT)           
      *        - CONTROL CARD CONTAINING GROUP-CSC PHONE NOS.                   
      *        - CLIENTSII EMPLOYEE FILE                                        
      *        - CLIENTSII LOCATION FILE                                        
      *        - CLIENTSII MEMBER   FILE                                        
      *                                                                         
      * OUTPUT - FLAT FILE OF REGISTRATION LETTER REQUESTS                      
      *                     (WITH ADDRESSES) (GCCCCEXT)                         
      *                                                                         
      ******************************************************************        
      * DATE       NAME      DESCRIPTION                                        
      * ---------  --------  -------------------------------------------        
      * 21OCT2001  F.MARUCCI CREATION.                                          
      * 11APR2002  J.ELKINS  CHANGE TO USE EMPLOYEE ADDRESS FOR COURIER         
      *                      GROUPS WITH MAIL CODE 1 OR 2                       
      * ---------  --------  -------------------------------------------        
      * 05NOV2002  KLYN      ADD MOVEMENT OF ACTIVATION CODE                    
      * ---------  --------  -------------------------------------------        
      * 11AUG2003  KLYN      ADD ADDRESS FOR VO PRE REJ ITEMS                   
      *                                                                         
HNS   * 09FEB2004  SPAULDING USE CONTENT MANAGER TO GET WEB ADDRESS             
WB    *            BASHAM    AND CSC PHONE NUMBER FOR ACTIVATION LETTER         
      *                                                                         
WB    * 30MAY2005  BASHAM    ALLOW HEALTHPRO MEMBERS ABILITY TO HAVE LTR        
      *                                                                         
WB2   * 21DEC2005  BASHAM    ALLOW HP-VO MEMBERS ABILITY TO HAVE LTR            
WB2   *                      LETTER CODE 'E'                                    
      *                                                                         
      * 11SEP2006  PRANGE    PROPAGATE REGISTRATION/PREACTIVATION               
      *                      INDICATOR FROM GCCCPEXT TO GCCCCEXT.               
WB3   * 12APR2007  BASHAM    REMOVE CLIENTS II LOCATION LOOKUP FOR VO           
WB3   *                      MEMBERS AS WE WILL BE USING ADDR FROM CPD          
                                                                                
BC    * 14JUN2007  CHAPMAN   PASS SPONSOR NAME FROM GCCCPEXT TO                 
BC    *                      GCCCCEXT                                           
      *                                                                         
      * 26AUG2008  IBM GR    UPGRADED IN ECU PROJECT                            
      *                                                                         
      * 04JUN2009  P.PAIK    READ CSC PHONE NO. FROM CONTROL CARD               
      *                      INSTEAD OF CONTENT MANAGER (REF TL 85138)          
      *                                                                         
      * 24MAR2010  P.PAIK    MKI LETTER ENHANCEMENT                             
      ******************************************************************        
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-370-165.                                            
       OBJECT-COMPUTER. IBM-370-165.                                            
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
             SELECT TABLE-FILE ASSIGN TO TABFILE.                               
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
                                                                                
       FD  TABLE-FILE                                                           
           RECORDING MODE IS F                                                  
           BLOCK CONTAINS 0 RECORDS                                             
           DATA RECORD IS TABLE-RECORD.                                         
                                                                                
       01  TABLE-RECORD.                                                        
           02 TABLE-GROUP            PIC 9(7).                                  
           02 TABLE-CSCNO            PIC X(20).                                 
           02 FILLER                 PIC X(53).                                 
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-START.                                                            
           05  FILLER                      PIC X(32) VALUE                      
               '*** GCCPRGNA WORKING STORAGE ***'.                              
                                                                                
                                                                                
       01 CSC-PHONE-TABLE.                                                      
          02 CSC-PHONE-ITEM OCCURS 1 TO 1000 TIMES                              
                 DEPENDING ON NUMBER-OF-GROUPS                                  
                 ASCENDING KEY IS CSCNO-GROUP                                   
                     INDEXED BY PHONE-INDEX.                                    
             03 CSCNO-GROUP         PIC 9(7).                                   
             03 CSCNO-PHONE         PIC X(20).                                  
                                                                                
       01 NUMBER-OF-GROUPS          PIC 9(4)  VALUE ZERO.                       
                                                                                
       01 EOF-FLAG                  PIC X.                                      
          88 END-OF-FILE            VALUE 'Y'.                                  
                                                                                
       01  WS-CONSTANTS.                                                        
           05  WS-ZPARDUMP-FUNCTION        PIC X(1)  VALUE '1'.                 
           05  WS-GAEDATSR-VERB            PIC X(16) VALUE 'NOT SET'.           
           05  GAEDATSR                    PIC X(8)  VALUE 'GAEDATSR'.          
           05  WS-GC2DATE                  PIC X(8)  VALUE 'GC2DATE'.           
           05  MLCTOCLU                    PIC X(8)  VALUE 'MLCTOCLU'.          
           05  WS-FIRST-NAME               PIC X(30) VALUE SPACES.              
           05  WS-LAST-NAME                PIC X(30) VALUE SPACES.              
           05  WS-CUST-NAME                PIC X(30) VALUE SPACES.              
           05  WS-DELIMITER                PIC X     VALUE '%'.                 
           05  IX                          PIC 99    VALUE ZERO.                
           05  NAME-IX                     PIC 99    VALUE 0.                   
           05  WS-ADDR-IX                  PIC 9     VALUE ZERO.                
           05  WS-INP-FOUND-STATUS         PIC X     VALUE 'N'.                 
               88  WS-INP-FOUND            VALUE 'Y'.                           
               88  WS-INP-NOT-FOUND        VALUE 'N'.                           
           05  WS-CSC-FOUND-STATUS         PIC X     VALUE 'N'.                 
               88  WS-CSC-FOUND            VALUE 'Y'.                           
               88  WS-CSC-NOT-FOUND        VALUE 'N'.                           
JAK                                                                             
JAK    01  WS-WORK-VARIABLES.                                                   
JAK        05  WS-TEMP-MAILING-DIV         PIC  X(03).                          
JAK        05  WS-TEMP-LOCATION-CD         PIC S9(03) COMP-3.                   
JAK        05  WS-TEMP-LANGUAGE-CD         PIC  X.                              
                                                                                
       01  WS-PRINT-RECORDS-READ.                                               
           05  FILLER                      PIC X     VALUE '0'.                 
           05  FILLER                      PIC X(20) VALUE                      
                                           'RECORDS     READ: '.                
           05  WS-NO-INPUT-RECS            PIC 9(7)  VALUE ZERO.                
           05  FILLER                      PIC X(105) VALUE SPACES.             
                                                                                
       01  WS-ADDR-TABLE.                                                       
           05  WS-ADDR            PIC X(30) OCCURS 4 TIMES.                     
                                                                                
       01  WS-PRINT-RECORDS-REJECTED.                                           
           05  FILLER                      PIC X     VALUE ' '.                 
           05  FILLER                      PIC X(20) VALUE                      
                                           'RECORDS REJECTED: '.                
           05  WS-NO-REJECT-RECS           PIC 9(7)  VALUE ZERO.                
           05  FILLER                      PIC X(105) VALUE SPACES.             
                                                                                
       01  WS-PRINT-RECORDS-WRITTEN.                                            
           05  FILLER                      PIC X     VALUE ' '.                 
           05  FILLER                      PIC X(20) VALUE                      
                                           'LETTERS  WRITTEN: '.                
           05  WS-NO-OUTPUT-RECS           PIC 9(7)  VALUE ZERO.                
           05  FILLER                      PIC X(105) VALUE SPACES.             
JAK                                                                             
JAK    01  WS-PRINT-VO-RECS-WRITTEN.                                            
JAK        05  FILLER                      PIC X     VALUE ' '.                 
JAK        05  FILLER                      PIC X(20) VALUE                      
JAK                                        'VO LETTERS WRITTEN: '.              
JAK        05  WS-NO-LISTING-RECS          PIC 9(7)  VALUE ZERO.                
JAK        05  FILLER                      PIC X(105) VALUE SPACES.             
                                                                                
       01  WS-SPLIT-CUST-NO.                                                    
           05  WS-CHAR           PIC X     OCCURS 10 TIMES.                     
                                                                                
       01  WS-SPLIT-NAME.                                                       
           05  WS-SPLIT-CHAR     PIC X     OCCURS 31 TIMES.                     
                                                                                
       01  WS-TEMP-NAME.                                                        
           05  WS-TEMP-NAME1     PIC X.                                         
           05  WS-TEMP-NAME2     PIC X(85).                                     
                                                                                
       01  WS-HIGH-DATE          PIC S9(07) VALUE +9999999.                     
                                                                                
       01  WS-STATUS-FIELDS.                                                    
           05  INWLB-MAIL-INSTRUCTION      PIC X(1).                            
           05  INWLB-EMP-STATUS            PIC X(1).                            
               88 INWLB-EMP-FOUND       VALUE 'Y'.                              
               88 INWLB-EMP-NOT-FOUND   VALUE 'N'.                              
           05  INWLB-LOC-STATUS            PIC X(1).                            
               88 INWLB-LOC-FOUND       VALUE 'Y'.                              
               88 INWLB-LOC-NOT-FOUND   VALUE 'N'.                              
           05  INWLB-ADDR-STATUS           PIC X(1).                            
               88 INWLB-ADDR-FOUND      VALUE 'Y'.                              
               88 INWLB-ADDR-NOT-FOUND  VALUE 'N'.                              
           05  INWLB-COV-STATUS            PIC X(1).                            
               88 INWLB-COV-FOUND       VALUE 'Y'.                              
               88 INWLB-COV-NOT-FOUND   VALUE 'N'.                              
           05  INWLB-CO-ADDR-STATUS        PIC X(1).                            
               88  INWLB-CO-ADDR-FOUND  VALUE 'Y'.                              
               88  INWLB-CO-ADDR-NOT-FOUND VALUE 'N'.                           
           05  INWLB-RECORD-STATUS         PIC X(1).                            
               88 INWLB-PROCESS-REC     VALUE 'Y'.                              
               88 INWLB-NOT-PROCESS-REC VALUE 'N'.                              
           05  WS-MAIL-INSTRUCTIONS        PIC X(1).                            
               88  WS-MAIL-TO-INSURED      VALUE 'A' ' '.                       
               88  WS-MAIL-BULK            VALUE 'B' 'C'.                       
           05  WS-MAIL-CODE                PIC X(1).                            
               88  WS-MAIL-CODE-TO-INSURED VALUE '1' '2'.                       
                                                                                
       01  WS-CO                   PIC X(5) VALUE 'C/O: '.                      
       01  WS-AS                   PIC X(5) VALUE 'A/S: '.                      
                                                                                
       01  WS-TEMP-CO-NAME       PIC X(57).                                     
       01  WS-PRINT-NAME         PIC X(30).                                     
                                                                                
       01  WS-TEMP-CERT.                                                        
           05  WS-TEMP-CERT-CHAR PIC X     OCCURS 10 TIMES.                     
                                                                                
       01  WS-LINE-COUNTER               PIC S9(4)  VALUE +50.                  
       01  WS-MAX-LINE-COUNTER           PIC S9(4)  VALUE +50.                  
       01  WS-DATE-PARMS.                                                       
           COPY GARDATEP.                                                       
                                                                                
                                                                                
       01  PRT-HEADER-LINE1.                                                    
           05  FILLER                    PIC X     VALUE '1'.                   
           05  FILLER                    PIC X(35) VALUE                        
           'JOB: PGB1200'.                                                      
           05  FILLER                    PIC X(50) VALUE                        
           'GROUP BENEFITS INTERNET REGISTRATION CONFIRMATION '.                
           05  FILLER                    PIC X(12) VALUE                        
           'ERROR REPORT'.                                                      
           05  FILLER                    PIC X(9)  VALUE SPACES.                
           05  FILLER                    PIC X(6)  VALUE                        
           'DATE: '.                                                            
           05  PRT-MTH                   PIC X(3).                              
           05  FILLER                    PIC X     VALUE SPACES.                
           05  PRT-DAY                   PIC X(2).                              
           05  FILLER                    PIC X(2)  VALUE ', '.                  
           05  PRT-YEAR                  PIC X(4).                              
           05  FILLER                    PIC X(8).                              
                                                                                
       01  PRT-HEADER-LINE2.                                                    
           05  FILLER                    PIC X     VALUE '0'.                   
           05  FILLER                    PIC X(32) VALUE SPACES.                
           05  FILLER                    PIC X(7)  VALUE                        
               '  GROUP'.                                                       
           05  FILLER                    PIC X(3)  VALUE SPACES.                
           05  FILLER                    PIC X(11) VALUE                        
               'CERTIFICATE'.                                                   
           05  FILLER                    PIC X(3)  VALUE SPACES.                
           05  FILLER                    PIC X(13) VALUE                        
               'ERROR MESSAGE'.                                                 
           05  FILLER                    PIC X(63) VALUE SPACES.                
                                                                                
       01  PRT-BLANK-LINE                PIC X(133) VALUE SPACES.               
                                                                                
       01  PRT-DETAIL-LINE.                                                     
           05  FILLER                    PIC X      VALUE ' '.                  
           05  FILLER                    PIC X(32)  VALUE SPACES.               
           05  PRT-GROUP-ID              PIC X(7).                              
           05  FILLER                    PIC X(3)   VALUE SPACES.               
           05  PRT-CERT-ID               PIC X(11).                             
           05  FILLER                    PIC X(3)   VALUE SPACES.               
           05  PRT-ERROR-MSG             PIC X(40).                             
           05  FILLER                    PIC X(36)  VALUE SPACES.               
                                                                                
       01  PRT-MESSAGES.                                                        
           05  PRT-EMP-NOT-FOUND         PIC X(40)  VALUE                       
               'EMPLOYEE RECORD NOT ON FILE'.                                   
           05  PRT-LOC-NOT-FOUND         PIC X(40)  VALUE                       
               'LOCATION RECORD NOT ON FILE'.                                   
           05  PRT-ADDR-NOT-FOUND        PIC X(40)  VALUE                       
               'EMPLOYEE ADDRESS NOT ON FILE'.                                  
           05  PRT-COV-NOT-FOUND         PIC X(40)  VALUE                       
               'COVERAGE RECORD NOT ON FILE'.                                   
           05  PRT-CO-ADDR-NOT-FOUND     PIC X(40)  VALUE                       
               'MAILING ADDRESS NOT ON LOCATION FILE'.                          
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      *    CL2 LOCATION RECORD                                                  
      *----------------------------------------------------------------*        
       01  LOC-LOCATION-RECORD.                                                 
         03  LOC-LOCATION-AREA.                                                 
             05  FILLER                   PIC X(04).                            
         03  LOC-LOCATION-REC.                                                  
             COPY XC4CFLOC.                                                     
                                                                EJECT           
      *----------------------------------------------------------------*        
      *    CL2 MEMBER RECORD                                                    
      *----------------------------------------------------------------*        
       01  MMA-MEMBER-RECORD.                                                   
         03  MMA-MEMBER-AREA.                                                   
             05  FILLER                   PIC X(04).                            
         03  MMA-MEMBER-REC.                                                    
             COPY XC4CFMMA.                                                     
                                                                EJECT           
      *----------------------------------------------------------------*        
      *    CL2 EMPLOYEE RECORD                                                  
      *----------------------------------------------------------------*        
       01  EMP-EMPLOYEE-RECORD.                                                 
         03  EMP-EMPLOYEE-AREA.                                                 
             05  FILLER                   PIC X(04).                            
         03  EMP-EMPLOYEE-REC.                                                  
             COPY XC4CFEMP.                                                     
                                                                EJECT           
      *----------------------------------------------------------------*        
      *   CL2 COVERAGE RECORD                                                   
      *----------------------------------------------------------------*        
       01  COV-COVERAGE-RECORD.                                                 
         03  COV-COVERAGE-AREA.                                                 
             05  FILLER                   PIC X(04).                            
         03  COV-COVERAGE-REC.                                                  
             COPY XC4CFCOV.                                                     
                                                                                
JAK2                                                                            
JAK2  *----------------------------------------------------------------*        
JAK2  *    FILE CONTIAINING LIST OF CUST-ID ETC                                 
JAK2  *    FOR WHOM LETTERS WERE GENERATED                                      
JAK2  *----------------------------------------------------------------*        
JAK2   01  OUT-LISTING.                                                         
JAK2         COPY GCCCPVOC.                                                     
                                                                                
      *----------------------------------------------------------------*        
      *    INTERNET REGN LETTER OUTPUT REQUEST FILE RECORD LAYOUT               
      *----------------------------------------------------------------*        
       01  OUT-RECORD.                                                          
             COPY GCCCCEXT.                                                     
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      *    INTERNET REGN LETTER INPUT REQUEST FILE RECORD LAYOUT                
      *----------------------------------------------------------------*        
       01  IN-RECORD.                                                           
             COPY GCCCPEXT.                                                     
                                                                EJECT           
                                                                                
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                          
                                                                EJECT           
                                                                EJECT           
       01  LOGICAL-RECORD-NAMES.        COPY HCSLRNAM.                          
                                                                EJECT           
      *----------------------------------------------------------------*        
      *    LOGICAL RECORD NAMES                                                 
      *----------------------------------------------------------------*        
       01  INPUT-LR                      PIC X(16)                              
                               VALUE 'CARD-DATA-010   '.                        
       01  OUTPUT-LR                     PIC X(16)                              
                               VALUE 'PRINT-DATA-020  '.                        
       01  ERROR-RPT-LR                  PIC X(16)                              
                               VALUE 'PRINT-DATA-021  '.                        
JAK    01  LISTING-LR                    PIC X(16)                              
JAK                            VALUE 'PRINT-DATA-022  '.                        
                                                                                
                                                                                
                                                                EJECT           
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
       01  WS-END-BYTE-X             PIC X(1)       VALUE SPACES.               
       01  WS-DEFAULT-CSCNO          PIC X(20)                                  
                               VALUE '1-800-268-6195'.                          
                                                                                
       01  CONTENT-WORK-FIELDS.                                                 
           05  WS-CURRENT-TIMESTAMP       PIC S9(15) PACKED-DECIMAL.            
           05  WS-CURRENT-TIME            PIC 9(6).                             
           05  WS-CURRENT-DATE-A.                                               
               10  WS-CURRENT-DATE-A-YY   PIC X(02).                            
               10  WS-CURRENT-DATE-A-MM   PIC X(02).                            
               10  WS-CURRENT-DATE-A-DD   PIC X(02).                            
           05  WS-CURRENT-DATE.                                                 
               10  FILLER                 PIC X(02) VALUE '20'.                 
               10  WS-CURRENT-DATE-YY     PIC X(02).                            
               10  WS-CURRENT-DATE-MM     PIC X(02).                            
               10  WS-CURRENT-DATE-DD     PIC X(02).                            
           05  WS-DESTINATION             PIC X.                                
                                                                                
           05  CSC-PHONE                  PIC X(20).                            
           05  SITE-URL                   PIC X(100).                           
                                                                                
       01  MLCTOCLU-CONTENT               PIC X(200).                           
       01  MLCTOCLU-PROTOCOL.             COPY MLCTRCLU.                        
       01  MEMPHONE-INPUT.                COPY MLCT004I.                        
       01  MEMPHONE-CONTENT.              COPY MLCT004O.                        
       01  WEBADDRS-INPUT.                COPY MLCT013I.                        
       01  WEBADDRS-CONTENT.              COPY MLCT013O.                        
                                                                                
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           DISPLAY 'START OF JOB - GCCPRGNA'.                                   
                                                                                
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
                                                                                
           PERFORM 2000-GEN-LTR  THRU  2000-EXIT                                
                                 UNTIL WS-INP-NOT-FOUND.                        
                                                                                
           PERFORM 9000-FINISH THRU 9000-EXIT.                                  
                                                                                
           GOBACK.                                                              
                                                                                
       0000-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       1000-INITIALIZATION.                                                     
      *****************************************************************         
      * READ FIRST RECORD                                                       
      * GET TODAYS DATE                                                         
      * PRINT REPORT HEADINGS                                                   
      * LOAD COBOL TABLE WITH CONTENTS OF THE CONTROL CARD                      
      *****************************************************************         
                                                                                
           MOVE ZERO           TO WS-NO-INPUT-RECS                              
                                  WS-NO-REJECT-RECS                             
                                  WS-NO-OUTPUT-RECS.                            
           MOVE 'GCCPRGNA'     TO ICBM-PROGRAM-NAME.                            
           MOVE LOW-VALUES     TO LINKAGE-CONTROL.                              
           MOVE OBTAIN-FIRST   TO WS-GAEDATSR-VERB.                             
           PERFORM 2100-READ-INPUT THRU 2100-EXIT.                              
                                                                                
      * GET TODAY'S DATE FOR THE ERROR REPORT                                   
                                                                                
           MOVE LOW-VALUES               TO WS-DATE-PARMS.                      
           MOVE 'E'                      TO VDATE-REQ-SERVICE.                  
           MOVE 'A'                      TO VDATE-REQ-BASIS.                    
           MOVE 'E'                      TO VDATE-REQ-LANGUAGE.                 
           MOVE 1                        TO VDATE-REQ-DETAIL.                   
                                                                                
           CALL WS-GC2DATE               USING WS-DATE-PARMS.                   
                                                                                
           MOVE VDATE-EXT-YEAR           TO PRT-YEAR.                           
           MOVE VDATE-EXT-MONTH          TO PRT-MTH.                            
           MOVE VDATE-EXT-DAY            TO PRT-DAY.                            
                                                                                
           PERFORM 4510-PRT-HEADINGS                                            
              THRU 4510-EXIT.                                                   
                                                                                
                                                                                
WB         MOVE VDATE1-YYYYMMDD     TO WS-CURRENT-DATE.                         
                                                                                
WB         MOVE WS-CURRENT-DATE-YY TO WS-CURRENT-DATE-A-YY.                     
WB         MOVE WS-CURRENT-DATE-MM TO WS-CURRENT-DATE-A-MM.                     
0B         MOVE WS-CURRENT-DATE-DD TO WS-CURRENT-DATE-A-DD.                     
                                                                                
      * LOAD TABLE WITH CSC DATA                                                
                                                                                
           OPEN INPUT TABLE-FILE.                                               
           MOVE 'N' TO EOF-FLAG.                                                
                                                                                
           READ TABLE-FILE                                                      
                AT END MOVE 'Y' TO EOF-FLAG.                                    
           MOVE SPACES TO CSC-PHONE-TABLE.                                      
                                                                                
           PERFORM 1100-LOAD-TABLE-ITEMS THRU 1100-EXIT                         
              VARYING PHONE-INDEX FROM 1 BY 1                                   
                 UNTIL PHONE-INDEX > 1000                                       
                   OR END-OF-FILE.                                              
                                                                                
           CLOSE TABLE-FILE.                                                    
                                                                                
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1100-LOAD-TABLE-ITEMS.                                                   
                                                                                
           MOVE TABLE-GROUP TO CSCNO-GROUP (PHONE-INDEX)                        
           MOVE TABLE-CSCNO TO CSCNO-PHONE (PHONE-INDEX)                        
           ADD  1          TO NUMBER-OF-GROUPS                                  
           READ TABLE-FILE                                                      
                 AT END MOVE 'Y' TO EOF-FLAG.                                   
                                                                                
                                                                                
       1100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-GEN-LTR.                                                            
                                                                                
      *-----------------------------------------------------------------        
      *    GENERATE CONFIRMATION LETTER FILE EXTRACTS                           
      *-----------------------------------------------------------------        
                                                                                
           PERFORM 2010-INITIALIZATION THRU 2010-EXIT.                          
                                                                                
           PERFORM 2020-EDIT-CUSTNO    THRU 2020-EXIT.                          
JAK                                                                             
WB2        IF GCCCPEXT-STAT-REAS-CD   =   'V'  OR 'O' OR 'H' OR 'E'             
JAK           MOVE GCCCPEXT-MAILING-DIV                                         
JAK                                   TO  WS-TEMP-MAILING-DIV                   
JAK           MOVE  1                 TO  WS-TEMP-LOCATION-CD                   
JAK           MOVE GCCCPEXT-LANG-CD   TO  WS-TEMP-LANGUAGE-CD                   
JAK         ELSE                                                                
JAK           PERFORM 2300-READ-EMP    THRU 2300-EXIT                           
JAK           IF INWLB-EMP-FOUND                                                
JAK              PERFORM 2450-GET-EMP-ADDRESS THRU 2450-EXIT                    
JAK              PERFORM 5000-FIND-DIV                                          
JAK                                    THRU 5000-EXIT                           
JAK              IF INWLB-COV-FOUND                                             
JAK                  MOVE COV-MAILING-DIVISION                                  
JAK                                   TO  WS-TEMP-MAILING-DIV                   
JAK                  MOVE EMP-LOCATION-CD                                       
JAK                                   TO  WS-TEMP-LOCATION-CD                   
JAK                  MOVE EMP-LANGUAGE-CD                                       
JAK                                   TO  WS-TEMP-LANGUAGE-CD                   
JAK                  PERFORM 2400-READ-LOC                                      
JAK                                    THRU 2400-EXIT                           
JAK              END-IF                                                         
JAK           END-IF                                                            
JAK        END-IF.                                                              
                                                                                
      * IF THE LOCATION RECORD WAS FOUND AND MAILING INSTRUCTION ARE            
      * MAIL TO THE INSURED MAKE SURE WE HAVE AN EMPLOYEE ADDRESS               
JAK   * WHICH WILL BE FROM THE EXTRACT FOR VO                                   
      * IF DIRECT MAIL AND NO EMPLOYEE ADDRES SENT TO THE COMPANY               
      * ADDRESS, IF IT IS PRESENT                                               
      * IF IT IS NOT MAIL TO THE INSURED MAKE SURE WE HAVE AN                   
      * ADDRESS ON THE LOCATION RECORD                                          
                                                                                
           IF INWLB-LOC-FOUND                                                   
               MOVE FLOC-CHEQUE-LETTER-INSTRUCT (3) TO                          
                                         WS-MAIL-INSTRUCTIONS                   
               MOVE FLOC-MAILING-CODE    TO WS-MAIL-CODE                        
               IF WS-MAIL-TO-INSURED                                            
                  IF INWLB-ADDR-NOT-FOUND AND                                   
JAK                   GCCCPEXT-STAT-REAS-CD NOT  = 'V'  AND                     
JAK                   GCCCPEXT-STAT-REAS-CD NOT  = 'O'  AND                     
                      FLOC-MAILING-ADDRESS (1) = SPACES AND                     
                      FLOC-MAILING-ADDRESS (2) = SPACES AND                     
                      FLOC-MAILING-ADDRESS (3) = SPACES AND                     
                      FLOC-MAILING-ADDRESS (4) = SPACES                         
                      SET INWLB-NOT-PROCESS-REC TO TRUE                         
                  END-IF                                                        
               ELSE                                                             
                   IF FLOC-MAILING-ADDRESS (1) = SPACES AND                     
                      FLOC-MAILING-ADDRESS (2) = SPACES AND                     
                      FLOC-MAILING-ADDRESS (3) = SPACES AND                     
                      FLOC-MAILING-ADDRESS (4) = SPACES                         
                      SET INWLB-NOT-PROCESS-REC TO TRUE                         
                      SET INWLB-CO-ADDR-NOT-FOUND TO TRUE                       
                   END-IF                                                       
               END-IF                                                           
WB    *ELSE IT'S A HP GUY AND WE NEED IT SENT DIRECT - NO LOC RECORD            
WB         ELSE                                                                 
WB             MOVE  'A'  TO  WS-MAIL-INSTRUCTIONS                              
           END-IF.                                                              
                                                                                
HNS        PERFORM 3000-GET-SITE-URL   THRU 3000-EXIT.                          
HNS        PERFORM 3010-GET-PHONE-NUM  THRU 3010-EXIT.                          
                                                                                
           IF INWLB-NOT-PROCESS-REC                                             
              PERFORM 4500-WRITE-ERROR THRU 4500-EXIT                           
           ELSE                                                                 
              PERFORM 2500-WRITE-REQUEST-FILE  THRU 2500-EXIT                   
           END-IF.                                                              
                                                                                
           MOVE OBTAIN-NEXT TO WS-GAEDATSR-VERB.                                
           PERFORM 2100-READ-INPUT THRU 2100-EXIT.                              
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       2010-INITIALIZATION.                                                     
                                                                                
      *-----------------------------------------------------------------        
      *    SET UP FOR PROCESSING OF NEXT TRANSACTION                            
      *-----------------------------------------------------------------        
                                                                                
           INITIALIZE  WS-ADDR-TABLE.                                           
           MOVE SPACES                   TO LOC-LOCATION-REC                    
                                            EMP-EMPLOYEE-REC                    
                                            COV-COVERAGE-REC                    
                                            MMA-MEMBER-REC                      
                                            OUT-RECORD.                         
                                                                                
       2010-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       2020-EDIT-CUSTNO.                                                        
                                                                                
      *-----------------------------------------------------------------        
      *    RIGHT JUSTIFY CERTIFICATE NUMBER                                     
      *-----------------------------------------------------------------        
                                                                                
           MOVE GCCCPEXT-CERT-ID      TO WS-TEMP-CERT.                          
                                                                                
           PERFORM 4000-RJUST-CERT THRU                                         
                   4000-RJUST-CERT-EXIT                                         
                   VARYING IX FROM 1 BY 1                                       
                   UNTIL IX > 10 OR                                             
                         WS-TEMP-CERT-CHAR (10) NOT = SPACES.                   
                                                                                
                                                                                
       2020-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       2100-READ-INPUT.                                                         
      *-----------------------------------------------------------------        
      *    READ INPUT RECORD                                                    
      *-----------------------------------------------------------------        
                                                                                
           MOVE INPUT-LR             TO LOGICAL-RECORD-NAME.                    
           INITIALIZE                   IN-RECORD.                              
                                                                                
           CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
                                         IN-RECORD                              
                                         ICBM                                   
           END-CALL.                                                            
                                                                                
      *-----------------------------------------------------------------        
      *    IF READ SUCCESSFUL - INITIALIZE THE REFORMATED REQUEST RECORD        
      *-----------------------------------------------------------------        
                                                                                
           IF LR-STATUS-OK                                                      
               INITIALIZE               OUT-RECORD                              
                                                                                
               SET WS-INP-FOUND         TO TRUE                                 
               SET INWLB-LOC-NOT-FOUND  TO TRUE                                 
               SET INWLB-EMP-NOT-FOUND  TO TRUE                                 
               SET INWLB-ADDR-NOT-FOUND TO TRUE                                 
               SET INWLB-COV-NOT-FOUND  TO TRUE                                 
               SET INWLB-PROCESS-REC    TO TRUE                                 
               SET INWLB-CO-ADDR-FOUND  TO TRUE                                 
               ADD +1                   TO WS-NO-INPUT-RECS                     
           ELSE                                                                 
               SET WS-INP-NOT-FOUND     TO TRUE                                 
           END-IF.                                                              
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       2300-READ-EMP.                                                           
                                                                                
           MOVE LR-HCS-EMPLOYEE-RO   TO LOGICAL-RECORD-NAME.                    
                                                                                
           MOVE GCCCPEXT-GROUP-ID    TO EMP-CONTRACT.                           
                                                                                
           MOVE WS-TEMP-CERT         TO EMP-CERT.                               
                                                                                
           MOVE OBTAIN-KEYED      TO     WS-GAEDATSR-VERB.                      
           CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
                                         EMP-EMPLOYEE-RECORD                    
                                         ICBM                                   
           END-CALL.                                                            
                                                                                
           IF LR-STATUS-OK                                                      
               SET INWLB-EMP-FOUND                         TO TRUE              
           ELSE                                                                 
               SET INWLB-NOT-PROCESS-REC                   TO TRUE              
               ADD 1 TO WS-NO-REJECT-RECS                                       
               SET INWLB-EMP-NOT-FOUND                     TO TRUE              
           END-IF.                                                              
                                                                                
       2300-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       2400-READ-LOC.                                                           
                                                                                
           MOVE LR-HCS-LOCATION-RO    TO LOGICAL-RECORD-NAME.                   
                                                                                
           MOVE GCCCPEXT-GROUP-ID     TO FLOC-CONTRACT.                         
JAK        MOVE WS-TEMP-MAILING-DIV   TO FLOC-DIVISION.                         
JAK        MOVE WS-TEMP-LOCATION-CD   TO FLOC-LOCATION.                         
                                                                                
           PERFORM 2410-READ-LOC-RECORD THRU 2410-EXIT.                         
                                                                                
           IF FLOC-LOCATION   NOT = 1  AND                                      
              FLOC-MAILING-ADDRESS (2) = SPACES AND                             
              FLOC-MAILING-ADDRESS (3) = SPACES AND                             
              FLOC-MAILING-ADDRESS (4) = SPACES                                 
               MOVE LR-HCS-LOCATION-RO    TO LOGICAL-RECORD-NAME                
               MOVE GCCCPEXT-GROUP-ID     TO FLOC-CONTRACT                      
               MOVE WS-TEMP-MAILING-DIV   TO FLOC-DIVISION                      
               MOVE 001                   TO FLOC-LOCATION                      
               PERFORM 2410-READ-LOC-RECORD THRU 2410-EXIT.                     
                                                                                
       2400-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       2410-READ-LOC-RECORD.                                                    
                                                                EJECT           
           MOVE OBTAIN-KEYED      TO     WS-GAEDATSR-VERB.                      
           CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
                                         LOC-LOCATION-RECORD                    
                                         ICBM                                   
           END-CALL.                                                            
                                                                                
           IF LR-STATUS-OK                                                      
               SET INWLB-LOC-FOUND            TO TRUE                           
           ELSE                                                                 
               SET INWLB-LOC-NOT-FOUND TO TRUE                                  
               IF FLOC-LOCATION = 001                                           
                  ADD 1 TO WS-NO-REJECT-RECS                                    
                  SET INWLB-NOT-PROCESS-REC   TO TRUE                           
               END-IF                                                           
           END-IF.                                                              
                                                                                
       2410-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       2450-GET-EMP-ADDRESS.                                                    
                                                                                
           MOVE LR-HCS-MEMBER-RO      TO LOGICAL-RECORD-NAME.                   
                                                                                
           MOVE EMP-FAMILY-NUMBER     TO MMA-FAMILY-NUMBER.                     
           MOVE '99'                  TO MMA-FAMILY-MEMBER-ID.                  
                                                                                
           MOVE OBTAIN-KEYED      TO     WS-GAEDATSR-VERB.                      
           CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
                                         MMA-MEMBER-RECORD                      
                                         ICBM                                   
           END-CALL.                                                            
                                                                                
           IF LR-STATUS-OK AND (                                                
                   MMA-EE-ADDRESS1  IS NOT EQUAL TO SPACES OR                   
                   MMA-EE-ADDRESS2  IS NOT EQUAL TO SPACES OR                   
                   MMA-EE-ADDRESS3  IS NOT EQUAL TO SPACES OR                   
                   MMA-EE-ADDRESS4  IS NOT EQUAL TO SPACES )                    
               SET INWLB-ADDR-FOUND        TO TRUE                              
                                                                                
           END-IF.                                                              
                                                                                
       2450-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
       2500-WRITE-REQUEST-FILE.                                                 
                                                                                
      *----------------------------------------------------------               
      *    BUILD THE RECORD CONTENTS                                            
      *----------------------------------------------------------               
                                                                                
           INITIALIZE OUT-RECORD.                                               
                                                                                
WB2        IF GCCCPEXT-STAT-REAS-CD   =   'V' OR 'O' OR 'H' OR 'E'              
              PERFORM 3550-REFORMAT-VO-NAME THRU 3550-EXIT                      
            ELSE                                                                
              PERFORM 3500-REFORMAT-CII-NAME THRU 3500-EXIT                     
           END-IF.                                                              
                                                                                
WB2        IF GCCCPEXT-STAT-REAS-CD   =   'H' OR 'E' OR 'V' OR 'O'              
WB            MOVE  GCCCPEXT-STAT-REAS-CD  TO  GCCCCEXT-STAT-REAS-CD            
WB         END-IF.                                                              
                                                                                
           MOVE WS-CUST-NAME           TO GCCCCEXT-CUST-NAME.                   
                                                                                
           MOVE GCCCPEXT-REGN-DATE     TO GCCCCEXT-REGIST-DATE.                 
                                                                                
           MOVE GCCCPEXT-GROUP-ID      TO GCCCCEXT-CUST-GROUP.                  
JAK        MOVE WS-TEMP-MAILING-DIV    TO GCCCCEXT-CUST-DIVISION.               
JAK        MOVE WS-TEMP-CERT           TO GCCCCEXT-CUST-CERT.                   
                                                                                
JAK        MOVE GCCCPEXT-ACTV-KEY      TO GCCCCEXT-ACTN-KEY.                    
           MOVE GCCCPEXT-REG-PREACT-IND                                         
                                       TO GCCCCEXT-REG-PREACT-IND.              
JAK        MOVE WS-TEMP-LANGUAGE-CD    TO GCCCCEXT-CUST-REPLY-LANG.             
JAK   *    MOVE EMP-LANGUAGE-CD        TO GCCCCEXT-CUST-REPLY-LANG.             
                                                                                
           MOVE SPACES                 TO GCCCCEXT-PLAN-ADMIN-NAME.             
           MOVE SPACES                 TO GCCCCEXT-CO-NAME1.                    
           MOVE SPACES                 TO GCCCCEXT-CO-NAME2.                    
                                                                                
      *----------------------------------------------------------               
      *   LETTERS ARE SENT TO DESTINATIONS AS FOLLOWS:                          
      *                                                                         
      *    - DIRECT TO MEMBER ADDRESS                                           
      *                                                                         
      *    THE CHEQUE/LETTER MAILING INSTRUCTIONS ON THE                        
      *    LOCATION RECORD INDICATE THE MAILING INSTRUCTION.                    
      *    (FIELD FLOC-CHEQUE-LETTER-INSTRUCT)                                  
      *    A = MAIL TO INSURED                                                  
      *    B,C = BULK                                                           
      *    OTHER = COURIER                                                      
      *                                                                         
      *    SET GCCCCEXT-MAIL-INSTRUCTION FOR SORTING:                           
      *                                                                         
      *    COURIER   - C                                                        
      *    BULK MAIL - B                                                        
      *    DIRECT    - D                                                        
      *                                                                         
      *    FOR COURIER GROUPS WITH A MAIL CODE OF '1' OR '2'                    
      *    USE THE EMPLOYEE ADDRESS                                             
      *----------------------------------------------------------               
                                                                                
           IF WS-MAIL-TO-INSURED                                                
               MOVE 'D' TO INWLB-MAIL-INSTRUCTION                               
WB2            IF GCCCPEXT-STAT-REAS-CD   =   'V' OR 'O' OR 'H' OR 'E'          
BC                 IF GCCCPEXT-SPONSOR-NAME NOT = SPACES                        
BC                    MOVE GCCCPEXT-SPONSOR-NAME                                
BC                                   TO GCCCCEXT-SPONSOR-NAME                   
BC                 END-IF                                                       
WB2                IF GCCCPEXT-STAT-REAS-CD = 'V' OR 'O' THEN                   
WB2                   IF GCCCPEXT-CO-NAME  NOT = SPACES                         
WB2                     MOVE GCCCPEXT-CO-NAME     TO GCCCCEXT-CO-NAME1          
WB2                     MOVE GCCCPEXT-CO-NAME     TO WS-TEMP-CO-NAME            
WB2                     IF GCCCPEXT-LANG-CD = 'F'                               
WB2                        STRING WS-AS DELIMITED BY SIZE                       
WB2                        ' ' DELIMITED BY SIZE                                
WB2                        WS-TEMP-CO-NAME DELIMITED BY SIZE                    
WB2                        INTO GCCCCEXT-CO-NAME1                               
WB2                     ELSE                                                    
WB2                        STRING WS-CO DELIMITED BY SIZE                       
WB2                        ' ' DELIMITED BY SIZE                                
WB2                        WS-TEMP-CO-NAME DELIMITED BY SIZE                    
WB2                        INTO GCCCCEXT-CO-NAME1                               
WB2                     END-IF                                                  
WB2                  END-IF                                                     
WB2                END-IF                                                       
JAK                   MOVE GCCCPEXT-ADDR-LINE1    TO WS-ADDR (1)                
JAK                   MOVE GCCCPEXT-ADDR-LINE2    TO WS-ADDR (2)                
JAK                   MOVE GCCCPEXT-ADDR-LINE3    TO WS-ADDR (3)                
JAK                   MOVE GCCCPEXT-ADDR-LINE4    TO WS-ADDR (4)                
JAK            ELSE                                                             
JAK                IF INWLB-ADDR-NOT-FOUND                                      
JAK                    MOVE FLOC-MAILING-NAME (1)                               
JAK                                           TO GCCCCEXT-CO-NAME1              
JAK                    MOVE FLOC-MAILING-NAME (2)                               
JAK                                           TO GCCCCEXT-CO-NAME2              
JAK                    MOVE FLOC-MAILING-ADDRESS (1)                            
JAK                                           TO WS-ADDR (1)                    
JAK                    MOVE FLOC-MAILING-ADDRESS (2)                            
JAK                                           TO WS-ADDR (2)                    
JAK                    MOVE FLOC-MAILING-ADDRESS (3)                            
JAK                                           TO WS-ADDR (3)                    
JAK                    MOVE FLOC-MAILING-ADDRESS (4)                            
JAK                                           TO WS-ADDR (4)                    
JAK                  ELSE                                                       
JAK                    MOVE MMA-EE-ADDRESS1   TO WS-ADDR (1)                    
JAK                    MOVE MMA-EE-ADDRESS2   TO WS-ADDR (2)                    
JAK                    MOVE MMA-EE-ADDRESS3   TO WS-ADDR (3)                    
JAK                    MOVE MMA-EE-ADDRESS4   TO WS-ADDR (4)                    
JAK                END-IF                                                       
JAK            END-IF                                                           
           ELSE                                                                 
               IF WS-MAIL-BULK                                                  
                   MOVE 'B'              TO INWLB-MAIL-INSTRUCTION              
                   MOVE FLOC-MAILING-NAME (1)    TO GCCCCEXT-CO-NAME1           
                   MOVE FLOC-MAILING-NAME (2)    TO GCCCCEXT-CO-NAME2           
                   MOVE FLOC-MAILING-ADDRESS (1) TO WS-ADDR (1)                 
                   MOVE FLOC-MAILING-ADDRESS (2) TO WS-ADDR (2)                 
                   MOVE FLOC-MAILING-ADDRESS (3) TO WS-ADDR (3)                 
                   MOVE FLOC-MAILING-ADDRESS (4) TO WS-ADDR (4)                 
               ELSE                                                             
                   MOVE 'C'              TO INWLB-MAIL-INSTRUCTION              
                   IF WS-MAIL-CODE-TO-INSURED                                   
WB2                  IF GCCCPEXT-STAT-REAS-CD = 'V' OR 'O' OR 'H' OR 'E'        
JAK                       MOVE GCCCPEXT-ADDR-LINE1 TO WS-ADDR (1)               
JAK                       MOVE GCCCPEXT-ADDR-LINE2 TO WS-ADDR (2)               
JAK                       MOVE GCCCPEXT-ADDR-LINE3 TO WS-ADDR (3)               
JAK                       MOVE GCCCPEXT-ADDR-LINE4 TO WS-ADDR (4)               
WB    *            IF GCCCPEXT-CO-NAME EQUALS NOT = SPACES THEN                 
WB    *               MOVE GCCCPEXT-CO-NAME       TO GCCCCEXT-CO-NAME1          
WB    *            END-IF                                                       
JAK                  ELSE                                                       
JAK                       IF INWLB-ADDR-FOUND                                   
JAK                          MOVE MMA-EE-ADDRESS1 TO WS-ADDR (1)                
JAK                          MOVE MMA-EE-ADDRESS2 TO WS-ADDR (2)                
JAK                          MOVE MMA-EE-ADDRESS3 TO WS-ADDR (3)                
JAK                          MOVE MMA-EE-ADDRESS4 TO WS-ADDR (4)                
JAK                        ELSE                                                 
JAK                          MOVE FLOC-MAILING-NAME (1)                         
JAK                                               TO GCCCCEXT-CO-NAME1          
JAK                          MOVE FLOC-MAILING-NAME (2)                         
JAK                                               TO GCCCCEXT-CO-NAME2          
JAK                          MOVE FLOC-MAILING-ADDRESS (1)                      
JAK                                               TO WS-ADDR (1)                
JAK                          MOVE FLOC-MAILING-ADDRESS (2)                      
JAK                                               TO WS-ADDR (2)                
JAK                          MOVE FLOC-MAILING-ADDRESS (3)                      
JAK                                               TO WS-ADDR (3)                
JAK                          MOVE FLOC-MAILING-ADDRESS (4)                      
JAK                                               TO WS-ADDR (4)                
JAK                       END-IF                                                
JAK                    END-IF                                                   
                   ELSE                                                         
                       MOVE FLOC-MAILING-NAME (1) TO GCCCCEXT-CO-NAME1          
                       MOVE FLOC-MAILING-NAME (2) TO GCCCCEXT-CO-NAME2          
                       MOVE FLOC-MAILING-ADDRESS (1)  TO WS-ADDR (1)            
                       MOVE FLOC-MAILING-ADDRESS (2)  TO WS-ADDR (2)            
                       MOVE FLOC-MAILING-ADDRESS (3)  TO WS-ADDR (3)            
                       MOVE FLOC-MAILING-ADDRESS (4)  TO WS-ADDR (4)            
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
                                                                                
                                                                                
                                                                                
           MOVE INWLB-MAIL-INSTRUCTION                                          
                               TO GCCCCEXT-MAIL-INSTRUCTION.                    
                                                                                
           PERFORM 4100-COMPRESS-ADDRESS THRU                                   
                   4100-COMPRESS-ADDRESS-EXIT                                   
                   VARYING WS-ADDR-IX FROM 1 BY 1                               
                   UNTIL   WS-ADDR-IX > 4.                                      
                                                                                
           MOVE WS-ADDR (1)        TO GCCCCEXT-CUST-ADDR1.                      
           MOVE WS-ADDR (2)        TO GCCCCEXT-CUST-ADDR2.                      
           MOVE WS-ADDR (3)        TO GCCCCEXT-CUST-ADDR3.                      
           MOVE WS-ADDR (4)        TO GCCCCEXT-CUST-ADDR4.                      
                                                                                
HNS        MOVE CSC-PHONE          TO GCCCCEXT-CSC-PHONE.                       
HNS        MOVE SITE-URL           TO GCCCCEXT-SITE-URL.                        
                                                                                
           MOVE GCCCPEXT-ENROL-DATE                                             
                                   TO GCCCCEXT-ENROL-DATE.                      
                                                                                
      *----------------------------------------------------------               
      *    WRITE RECORD TO THE FILE                                             
      *----------------------------------------------------------               
                                                                                
           MOVE OUTPUT-LR             TO LOGICAL-RECORD-NAME.                   
                                                                                
           MOVE STORE-LR          TO     WS-GAEDATSR-VERB.                      
                                                                                
           CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
                                         OUT-RECORD                             
                                         ICBM                                   
           END-CALL.                                                            
                                                                                
           IF LR-STATUS-OK                                                      
               ADD +1 TO WS-NO-OUTPUT-RECS                                      
JAK            PERFORM 2800-WRITE-LISTING-FILE THRU 2800-EXIT                   
           ELSE                                                                 
               DISPLAY 'ERROR ADDING TO LETTER FILE'                            
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND THRU 9999-EXIT                                
           END-IF.                                                              
                                                                                
       2500-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
JAK                                                                             
JAK    2800-WRITE-LISTING-FILE.                                                 
JAK                                                                             
JAK   *---------------------------------------------------------                
JAK   *    BUILD THE RECORD CONTENTS                                            
JAK   *---------------------------------------------------------                
JAK                                                                             
JAK        INITIALIZE OUT-LISTING.                                              
JAK                                                                             
JAK                                                                             
JAK        MOVE GCCCPEXT-CUST-ID       TO GCCCPVOC-CUST-ID                      
JAK                                                                             
JAK        MOVE GCCCPEXT-GROUP-ID      TO GCCCPVOC-GROUP-ID                     
JAK        MOVE GCCCPEXT-CERT-ID       TO GCCCPVOC-CERT-ID                      
JAK                                                                             
JAK                                                                             
JAK                                                                             
JAK   *---------------------------------------------------------                
JAK   *    WRITE RECORD TO THE FILE                                             
JAK   *---------------------------------------------------------                
JAK                                                                             
JAK        MOVE LISTING-LR            TO LOGICAL-RECORD-NAME.                   
JAK                                                                             
JAK        MOVE STORE-LR              TO WS-GAEDATSR-VERB.                      
JAK                                                                             
JAK        CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
JAK                                      OUT-LISTING                            
JAK                                      ICBM                                   
JAK        END-CALL.                                                            
JAK                                                                             
JAK        IF LR-STATUS-OK                                                      
JAK            ADD +1 TO WS-NO-LISTING-RECS                                     
JAK        ELSE                                                                 
JAK            DISPLAY 'ERROR ADDING TO VO LIST FILE'                           
JAK            DISPLAY PROGRAM-LINKAGE-STATUS                                   
JAK            PERFORM 9999-ABEND THRU 9999-EXIT                                
JAK        END-IF.                                                              
JAK                                                                             
JAK    2800-EXIT.                                                               
JAK        EXIT.                                                                
                                                                EJECT           
HNS    3000-GET-SITE-URL.                                                       
"     *================================================================*        
"     * DETERMINE WEB SITE ADDRESS (URL)                                        
"     *----------------------------------------------------------------*        
"                                                                               
"          MOVE 'WEBADDRS'           TO OCLU-CONTENT-NAME.                      
"          MOVE GCCCPEXT-GROUP-ID    TO MLCTOCLU-WEBADDRS-PLAN-NUMBER.          
"          MOVE WEBADDRS-INPUT       TO OCLU-CONTENT-ARGUMENT.                  
"                                                                               
"          PERFORM 3020-CALL-LOOKUP  THRU  3020-EXIT.                           
"                                                                               
"          IF OCLU-RET-NOT-FOUND                                                
HNS   *        MOVE '9999999'       TO  MLCTOCLU-WEBADDRS-PLAN-NUMBER           
WB             MOVE SPACES          TO  MLCTOCLU-WEBADDRS-PLAN-NUMBER           
HNS            MOVE WEBADDRS-INPUT  TO  OCLU-CONTENT-ARGUMENT                   
"              PERFORM 3020-CALL-LOOKUP THRU  3020-EXIT                         
"          END-IF.                                                              
"                                                                               
"          MOVE MLCTOCLU-CONTENT               TO  WEBADDRS-CONTENT.            
WB    * MOVE ENGLISH URL                                                        
WB         IF WS-TEMP-LANGUAGE-CD = 'E' THEN                                    
HNS           MOVE MLCTOCLU-WEBADDRS-ADDRESS (1)  TO  SITE-URL                  
WB         ELSE                                                                 
WB    * MOVE FRENCH URL                                                         
WB            IF WS-TEMP-LANGUAGE-CD = 'F' THEN                                 
WB               MOVE MLCTOCLU-WEBADDRS-ADDRESS (2)  TO  SITE-URL               
WB            ELSE                                                              
WB               MOVE MLCTOCLU-WEBADDRS-ADDRESS (1)  TO  SITE-URL               
WB            END-IF                                                            
WB         END-IF.                                                              
WB                                                                              
WB     3000-EXIT.                                                               
WB         EXIT.                                                                
WB     3010-GET-PHONE-NUM.                                                      
      *================================================================*        
      * DETERMINE MEMBER INQUIRY PHONE NUMBER. IF GROUP DOES NOT MATCH          
      * ANY OF THE GROUPS IN THE CONTROL CARD THEN MOVE THE DEFAULT             
      * NO. AS THE CSC NO.                                                      
      *----------------------------------------------------------------*        
                                                                                
           SET PHONE-INDEX TO 1.                                                
           SEARCH ALL CSC-PHONE-ITEM                                            
              AT END MOVE WS-DEFAULT-CSCNO TO CSC-PHONE                         
              WHEN CSCNO-GROUP(PHONE-INDEX) = GCCCPEXT-GROUP-ID                 
              MOVE CSCNO-PHONE (PHONE-INDEX) TO CSC-PHONE.                      
                                                                                
HNS    3010-EXIT.                                                               
HNS        EXIT.                                                                
       3020-CALL-LOOKUP.                                                        
      *===============================================================*         
      *    CALL THE CONTENT SERVICE PROGRAM                                     
      *---------------------------------------------------------------*         
                                                                                
WB         MOVE WS-CURRENT-DATE            TO OCLU-CONTENT-PROC-DATE.           
WB         MOVE WS-CURRENT-DATE            TO OCLU-CONTENT-EFF-DATE.            
           MOVE SPACES                     TO MLCTOCLU-CONTENT.                 
           MOVE LENGTH OF MLCTOCLU-CONTENT TO OCLU-MAX-DATA-LENGTH.             
                                                                                
           COPY MLCTCCLU.                                                       
                                                                                
      *    IF OCLU-RET-ERROR                                                    
      *        MOVE MLCTOCLU           TO WS-WARNING-99002-PROGRAM              
      *        MOVE OCLU-ERROR-DETAILS TO WS-WARNING-99002-TEXT                 
      *        MOVE WS-WARNING-TEXT-MSG99002 TO                                 
      *             XC4CEOBA-LAST-WARNING-TEXT                                  
      *        MOVE '99'               TO XC4CEOBA-LAST-WARNING-AREA            
      *        MOVE '002'              TO XC4CEOBA-LAST-WARNING-NUMBER          
      *        MOVE OCLU-CONTENT-ARGUMENT                                       
      *                                TO XC4CEOBA-LAST-WARNING-KEY             
      *        PERFORM 9999-WARNING  THRU 9999-WARNING-EXIT                     
      *    END-IF.                                                              
                                                                                
                                                                                
       3020-EXIT.                                                               
           EXIT.                                                                
JAK    3500-REFORMAT-CII-NAME.                                                  
                                                                                
           MOVE SPACES TO WS-FIRST-NAME                                         
                          WS-CUST-NAME                                          
                          WS-LAST-NAME.                                         
                                                                                
           MOVE EMP-EE-NAME              TO WS-CUST-NAME.                       
                                                                                
           INSPECT  WS-CUST-NAME REPLACING FIRST ',' BY '@'.                    
           INSPECT  WS-CUST-NAME REPLACING ALL   ',' BY ' '.                    
           UNSTRING WS-CUST-NAME DELIMITED BY '@'                               
                                         INTO WS-LAST-NAME                      
                                              WS-FIRST-NAME.                    
                                                                                
                                                                                
                                                                                
           MOVE WS-FIRST-NAME TO WS-SPLIT-NAME.                                 
HNS   *    PERFORM 3600-FIND-END THRU                                           
HNS   *            3600-EXIT                                                    
HNS        PERFORM                                                              
             VARYING NAME-IX FROM 30 BY -1                                      
             UNTIL NAME-IX = 1 OR                                               
                   WS-SPLIT-CHAR (NAME-IX) NOT = SPACE                          
HNS        END-PERFORM.                                                         
                                                                                
           ADD 1                TO NAME-IX.                                     
           MOVE WS-DELIMITER    TO WS-SPLIT-CHAR (NAME-IX).                     
           MOVE WS-SPLIT-NAME   TO WS-FIRST-NAME.                               
                                                                                
           STRING WS-FIRST-NAME DELIMITED BY WS-DELIMITER                       
                  ' ' DELIMITED BY SIZE                                         
                  WS-LAST-NAME DELIMITED BY SIZE                                
                  INTO WS-CUST-NAME.                                            
                                                                                
       3500-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
JAK    3550-REFORMAT-VO-NAME.                                                   
JAK                                                                             
JAK        MOVE SPACES TO WS-FIRST-NAME                                         
JAK                       WS-CUST-NAME                                          
JAK                       WS-LAST-NAME.                                         
JAK                                                                             
JAK        MOVE GCCCPEXT-NAME-LINE1        TO WS-FIRST-NAME.                    
JAK        MOVE GCCCPEXT-NAME-LINE2        TO WS-LAST-NAME.                     
JAK                                                                             
JAK                                                                             
JAK                                                                             
JAK        MOVE WS-FIRST-NAME TO WS-SPLIT-NAME.                                 
HNSJAK*    PERFORM 3600-FIND-END THRU                                           
HNSJAK*            3600-EXIT                                                    
HNS        PERFORM                                                              
JAK          VARYING NAME-IX FROM 30 BY -1                                      
JAK          UNTIL NAME-IX = 1 OR                                               
JAK                WS-SPLIT-CHAR (NAME-IX) NOT = SPACE                          
HNS        END-PERFORM.                                                         
JAK                                                                             
JAK        ADD 1                TO NAME-IX.                                     
JAK        MOVE WS-DELIMITER    TO WS-SPLIT-CHAR (NAME-IX).                     
JAK        MOVE WS-SPLIT-NAME   TO WS-FIRST-NAME.                               
JAK                                                                             
JAK        STRING WS-FIRST-NAME DELIMITED BY WS-DELIMITER                       
JAK               ' ' DELIMITED BY SIZE                                         
JAK               WS-LAST-NAME DELIMITED BY SIZE                                
JAK               INTO WS-CUST-NAME.                                            
JAK                                                                             
JAK    3550-EXIT.                                                               
JAK        EXIT.                                                                
JAK                                                                             
      *3600-FIND-END.                                                           
      ******************************************************************        
      *    EMPTY ROUTINE FOR LAST NAME REFORMATTING                    *        
      ******************************************************************        
      *3600-EXIT.                                                               
      *    EXIT.                                                                
                                                                EJECT           
       4000-RJUST-CERT.                                                         
      ******************************************************************        
      *    RIGHT JUSTIFY CERT FOR CLIENTS II KEY LOOKUP                *        
      ******************************************************************        
                                                                                
           IF WS-TEMP-CERT-CHAR (10) = SPACES                                   
               MOVE WS-TEMP-CERT-CHAR (9)  TO  WS-TEMP-CERT-CHAR (10)           
               MOVE WS-TEMP-CERT-CHAR (8)  TO  WS-TEMP-CERT-CHAR (9)            
               MOVE WS-TEMP-CERT-CHAR (7)  TO  WS-TEMP-CERT-CHAR (8)            
               MOVE WS-TEMP-CERT-CHAR (6)  TO  WS-TEMP-CERT-CHAR (7)            
               MOVE WS-TEMP-CERT-CHAR (5)  TO  WS-TEMP-CERT-CHAR (6)            
               MOVE WS-TEMP-CERT-CHAR (4)  TO  WS-TEMP-CERT-CHAR (5)            
               MOVE WS-TEMP-CERT-CHAR (3)  TO  WS-TEMP-CERT-CHAR (4)            
               MOVE WS-TEMP-CERT-CHAR (2)  TO  WS-TEMP-CERT-CHAR (3)            
               MOVE WS-TEMP-CERT-CHAR (1)  TO  WS-TEMP-CERT-CHAR (2)            
               MOVE SPACES                 TO  WS-TEMP-CERT-CHAR (1).           
                                                                                
       4000-RJUST-CERT-EXIT.                                                    
           EXIT.                                                                
                                                                EJECT           
       4100-COMPRESS-ADDRESS.                                                   
      *--------------------------------------------------------                 
      *    TAKE OUT BLANK LINES IN ADDRESS                                      
      *--------------------------------------------------------                 
                                                                                
           IF WS-ADDR (1) = SPACES                                              
               MOVE WS-ADDR (2) TO WS-ADDR (1)                                  
               MOVE WS-ADDR (3) TO WS-ADDR (2)                                  
               MOVE WS-ADDR (4) TO WS-ADDR (3)                                  
               MOVE SPACES      TO WS-ADDR (4).                                 
                                                                                
           IF WS-ADDR (2) = SPACES                                              
               MOVE WS-ADDR (3) TO WS-ADDR (2)                                  
               MOVE WS-ADDR (4) TO WS-ADDR (3)                                  
               MOVE SPACES      TO WS-ADDR (4).                                 
                                                                                
           IF WS-ADDR (3) = SPACES                                              
               MOVE WS-ADDR (4) TO WS-ADDR (3)                                  
               MOVE SPACES      TO WS-ADDR (4).                                 
                                                                                
                                                                                
       4100-COMPRESS-ADDRESS-EXIT.                                              
           EXIT.                                                                
                                                                EJECT           
       4500-WRITE-ERROR.                                                        
      *****************************************************************         
      * PROCESS ERROR FOUND                                                     
      * PRINT THE GROUP AND CERTIFICATE OF REJECTED RECORD                      
      * PRINT ERROR MESSAGE DESCRIBING THE ERROR                                
      *****************************************************************         
                                                                                
           IF WS-LINE-COUNTER            =  WS-MAX-LINE-COUNTER                 
               PERFORM 4510-PRT-HEADINGS                                        
                  THRU 4510-EXIT                                                
           END-IF.                                                              
                                                                                
           MOVE GCCCPEXT-GROUP-ID        TO PRT-GROUP-ID.                       
           MOVE GCCCPEXT-CERT-ID         TO PRT-CERT-ID.                        
                                                                                
           EVALUATE TRUE                                                        
             WHEN INWLB-EMP-NOT-FOUND                                           
               MOVE PRT-EMP-NOT-FOUND    TO PRT-ERROR-MSG                       
             WHEN INWLB-COV-NOT-FOUND                                           
               MOVE PRT-COV-NOT-FOUND    TO PRT-ERROR-MSG                       
             WHEN INWLB-LOC-NOT-FOUND                                           
               MOVE PRT-LOC-NOT-FOUND    TO PRT-ERROR-MSG                       
             WHEN INWLB-ADDR-NOT-FOUND                                          
               MOVE PRT-ADDR-NOT-FOUND   TO PRT-ERROR-MSG                       
             WHEN INWLB-CO-ADDR-NOT-FOUND                                       
               MOVE PRT-CO-ADDR-NOT-FOUND                                       
                                         TO PRT-ERROR-MSG                       
           END-EVALUATE.                                                        
                                                                                
           MOVE ERROR-RPT-LR             TO LOGICAL-RECORD-NAME.                
           MOVE STORE-LR                 TO WS-GAEDATSR-VERB.                   
                                                                                
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         PRT-DETAIL-LINE                        
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO ERROR REPORT'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           ADD 1                         TO WS-LINE-COUNTER.                    
                                                                                
       4500-EXIT.                                                               
           EXIT.                                                                
                                                                                
       4510-PRT-HEADINGS.                                                       
      *********************************************************                 
      * PRINT HEADINGS ON THE ERROR REPORT                                      
      *********************************************************                 
                                                                                
           MOVE ERROR-RPT-LR             TO LOGICAL-RECORD-NAME.                
           MOVE STORE-LR                 TO WS-GAEDATSR-VERB.                   
                                                                                
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         PRT-HEADER-LINE1                       
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO ERROR REPORT'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         PRT-HEADER-LINE2                       
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO ERROR REPORT'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         PRT-BLANK-LINE                         
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO ERROR REPORT'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           MOVE 0                        TO WS-LINE-COUNTER.                    
                                                                                
       4510-EXIT.                                                               
           EXIT.                                                                
                                                                                
       5000-FIND-DIV.                                                           
      *********************************************************                 
      * THE DIVISION NUMBER IS FOUND ON THE COVERAGE RECORD                     
      *********************************************************                 
                                                                                
           MOVE LR-HCS-COVERAGE-RO       TO LOGICAL-RECORD-NAME.                
           MOVE EMP-FAMILY-NUMBER        TO COV-FAMILY-NUMBER.                  
           MOVE WS-HIGH-DATE             TO COV-KEY-DATE.                       
           PERFORM 5100-READ-COV                                                
              THRU 5100-EXIT.                                                   
                                                                                
       5000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       5100-READ-COV.                                                           
      *********************************************************                 
      * READ A CURRENT COVERAGE RECORD FOR THIS EMPLOYEE                        
      * THE COV-BEN-CODE MAY EQUAL SPACES OR HC OR DN                           
      * THE CURRENT COVERAGE RECORD WILL HAVE HIGH VALUES                       
      * IN THE COV-KEY-DATE FIELD                                               
      *********************************************************                 
                                                                                
           MOVE 'DN'                     TO COV-BEN-CODE.                       
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         COV-COVERAGE-RECORD                    
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               SET INWLB-COV-FOUND       TO TRUE                                
               GO TO 5100-EXIT                                                  
           END-IF.                                                              
                                                                                
           MOVE 'HC'                     TO COV-BEN-CODE.                       
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         COV-COVERAGE-RECORD                    
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               SET INWLB-COV-FOUND       TO TRUE                                
               GO TO 5100-EXIT                                                  
           END-IF.                                                              
                                                                                
           MOVE '  '                     TO COV-BEN-CODE.                       
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         COV-COVERAGE-RECORD                    
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               SET INWLB-COV-FOUND       TO TRUE                                
               GO TO 5100-EXIT                                                  
           END-IF.                                                              
                                                                                
      * A CURRENT COVERAGE RECORD WAS NOT FOUND                                 
                                                                                
           SET INWLB-NOT-PROCESS-REC     TO TRUE.                               
           SET INWLB-COV-NOT-FOUND       TO TRUE.                               
           ADD 1                         TO WS-NO-REJECT-RECS.                  
                                                                                
       5100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9000-FINISH.                                                             
      *--------------------------------------------------------                 
      *    WRITE CONTROL TOTALS TO ERROR REPORT                                 
      *--------------------------------------------------------                 
                                                                                
                                                                                
           MOVE ERROR-RPT-LR             TO LOGICAL-RECORD-NAME.                
           MOVE STORE-LR                 TO WS-GAEDATSR-VERB.                   
                                                                                
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         WS-PRINT-RECORDS-READ                  
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO ERROR REPORT'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         WS-PRINT-RECORDS-REJECTED              
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO ERROR REPORT'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
           CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
                                         WS-PRINT-RECORDS-WRITTEN               
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO ERROR REPORT'                          
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
JAK                                                                             
JAK        CALL GAEDATSR                 USING WS-GAEDATSR-VERB                 
JAK                                      WS-PRINT-VO-RECS-WRITTEN               
JAK                                      ICBM.                                  
JAK                                                                             
JAK        IF LR-STATUS-OK                                                      
JAK            NEXT SENTENCE                                                    
JAK        ELSE                                                                 
JAK            DISPLAY 'ERROR WRITING TO ERROR REPORT'                          
JAK            DISPLAY PROGRAM-LINKAGE-STATUS                                   
JAK            PERFORM 9999-ABEND                                               
JAK               THRU 9999-EXIT                                                
JAK        END-IF.                                                              
                                                                                
      *--------------------------------------------------------                 
      *    CLOSE FILES                                                          
      *--------------------------------------------------------                 
                                                                                
           MOVE INPUT-LR    TO LOGICAL-RECORD-NAME                              
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB                                 
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM                                             
           END-CALL                                                             
                                                                                
           MOVE OUTPUT-LR   TO LOGICAL-RECORD-NAME                              
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB                                 
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM                                             
           END-CALL                                                             
                                                                                
JAK                                                                             
JAK        MOVE LISTING-LR  TO LOGICAL-RECORD-NAME                              
JAK        MOVE FINISH-LR   TO WS-GAEDATSR-VERB                                 
JAK        CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
JAK                            LOGICAL-RECORD-NAME                              
JAK                            ICBM                                             
JAK        END-CALL                                                             
JAK                                                                             
           MOVE LR-HCS-EMPLOYEE-RO TO LOGICAL-RECORD-NAME                       
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB                                 
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM                                             
           END-CALL                                                             
                                                                                
           MOVE LR-HCS-LOCATION-RO TO LOGICAL-RECORD-NAME                       
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB                                 
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM                                             
           END-CALL                                                             
                                                                                
           MOVE LR-HCS-MEMBER-RO TO LOGICAL-RECORD-NAME                         
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB                                 
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM                                             
           END-CALL                                                             
                                                                                
           MOVE LR-HCS-COVERAGE-RO TO LOGICAL-RECORD-NAME                       
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB                                 
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM                                             
           END-CALL                                                             
                                                                                
           MOVE ERROR-RPT-LR TO LOGICAL-RECORD-NAME                             
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB                                 
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM                                             
           END-CALL                                                             
                                                                                
           IF LR-STATUS-OK                                                      
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING FILES'                                    
           END-IF                                                               
                                                                                
           DISPLAY 'END OF JOB - GCCCPRGNA'.                                    
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                EJECT           
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
