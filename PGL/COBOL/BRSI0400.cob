       CBL FLAG(I),DATA(24)                                                     
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      * (ACCORDING TO EXISTING G* MODULES IN PRODUCTION)                        
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    BRSI0400.                                                 
      *AUTHOR.        TARIQ NOOR.                                               
      *DATE-WRITTEN.  OCT 14, 2004.                                             
      *****************************************************************         
      *                  MODULE DESCRIPTION                           *         
      *****************************************************************         
      *                                                               *         
      *  THIS PROGRAM READS THE EFT FILE (RECEIVED FROM LH CLAIMS)    *         
      *  AND CREATES A DIRECT DEPOSIT CONTROL REPORT AND A PAYMENT    *         
      *  REGISTER REPORT.                                             *         
      *                                                               *         
      *  PROGRAM WILL ABEND UNDER FOLLOWING CONDITIONS                *         
      *                                                               *         
      *      1. FIRST RECORD FROM EFT FILE COULD NOT BE OBTAINED      *         
      *      2. FIRST RECORD FROM EFT FILE WAS NOT A HEADER RECORD    *         
      *      3. UNKNOWN RECORD TYPES ENCOUNTERED (OTHER THAN A,C,Z)   *         
      *      4. NO TRAILER RECORD FOUND ON THE INPUT EFT FILE         *         
      *      5. EXTRA HEADER RECORDS ENCOUNTED IN THE EFT FILE        *         
      *      6. CONTROL TOTALS (REPORT) DO NOT MATCH WITH CONTROL     *         
      *         TOTALS ON THE TRAILER RECORD                          *         
      *      7. DATE CONVERSION PROBLEM                               *         
      *                                                               *         
      *  CALLS     : GAEDATSR - STANDARD I/O DATA SERVICE             *         
      *            : GC2DATE  - DATE MANIPULATION ROUTINE             *         
      *                                                               *         
      *---------------------------------------------------------------*         
      *  CHANGE HISTORY LOG                                           *         
      *---------------------------------------------------------------*         
      *  SNO  DATE       PROGRAMMER DESCRIPTION                       *         
      *  ---  ---------  ---------- --------------------------------  *         
      *  001  14OCT2004  T. NOOR    CREATED THIS PROGRAM              *         
      *                                                               *         
      *  002  06OCT2008  ECU        ENTERPRISE COMPILER UPGRADE       *         
      *                                                               *         
      *****************************************************************         
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                         00081   
                                                                                
       01  WS-START-OF-W.                                                       
           05  FILLER                    PIC X(32) VALUE                        
              '*** BRSI0400 WS STARTS HERE ****'.                               
                                                                                
       01  WS-CONDITIONAL-VARS.                                                 
           05  WS-DATECARD                               PIC X.                 
               88  WS-DATECARD-NOT-FOUND                 VALUE 'N'.             
               88  WS-DATECARD-FOUND                     VALUE 'Y'.             
           05  WS-ERROR-ENCOUNTERED-SW                   PIC X(01).             
               88  WS-NO-ERROR-YET                       VALUE 'N'.             
               88  WS-ERROR-ENCOUNTERED                  VALUE 'Y'.             
           05  WS-TRAILER-REC-SW                         PIC X(01).             
               88  TRAILER-REC-FOUND                     VALUE 'Y'.             
               88  TRAILER-REC-NOT-FOUND                 VALUE 'N'.             
                                                                                
       01  WS-ERROR-MESSAGES.                                                   
           05  WS-OBTAIN-REC-EF-ERROR     PIC X(80)                             
               VALUE 'ERROR OBTAINING EFT RECORD      '.                        
           05  WS-ARRAY-ERROR-SIZE        PIC X(80)                             
               VALUE 'ERROR INTERNAL DATE TABLE - ONLY 346 ALLOWED'.            
           05  WS-OBTAIN-REC-EF-NOHEAD    PIC X(80)                             
               VALUE 'NO HEADER RECORD FOUND          '.                        
           05  WS-DATE-CONV-ERROR         PIC X(80)                             
               VALUE 'ERROR CONVERTING DATE FORMAT    '.                        
           05  WS-BOTH-CREDIT-N-DEBIT     PIC X(80)                             
               VALUE 'ERROR BOTH CREDIT AND DEBIT RECS NOT ALLOWED'.            
           05  WS-TOTALS-ERROR-CT         PIC X(80)                             
               VALUE 'CONTROL TOTALS DO NOT MATCH     '.                        
           05  WS-ERROR-HEADER-EXTRA      PIC X(80)                             
               VALUE 'EXTRA HEADER FOUND ON INPUT EFT FILE '.                   
           05  WS-ERROR-TRAILER-NF        PIC X(80)                             
               VALUE 'TRAILER RECORD NOT FOUND IN EFT FILE '.                   
           05  WS-ERROR-UNKNOWN-REC       PIC X(80)                             
               VALUE 'AN UNKNOWN RECORD TYPE WAS ENCOUNTERED'.                  
           05  WS-ERROR-PARM-CARD         PIC X(80)                             
               VALUE 'INVALID PARM CODE ENTERED FOR APPL.   '.                  
                                                                                
       01  WS-DATE-FORMATTING.                                                  
           05  WS-DATE.                                                         
               10  WS-DATE-Y4             PIC X(04).                            
               10  WS-DATE-M2             PIC X(02).                            
               10  WS-DATE-D2             PIC X(02).                            
           05  WS-DATE-YYYY-MM-DD.                                              
               10  WS-DATE-YYYY           PIC X(04).                            
               10  WS-FILLER1             PIC X(01).                            
               10  WS-DATE-MM             PIC X(02).                            
               10  WS-FILLER2             PIC X(01).                            
               10  WS-DATE-DD             PIC X(02).                            
      *    05  WS-PAYMENT-DATE.                                                 
      *        10  WS-PAYMENT-DATE-CCYY   PIC X(04).                            
      *        10  WS-PAYMENT-DATE-DD     PIC X(03).                            
           05  WS-PAYMENT-DATE            PIC 9(07).                            
                                                                                
      *-----------------------------------------------------------------        
      * STANDARD SERVICES VARIABLES.                                            
      *-----------------------------------------------------------------        
                                                                                
       01  STD-SERVICES-VARS.                                                   
           05  WS-PROGRAM-ID             PIC X(08)  VALUE 'BRSI0400'.           
           05  WS-REPORT-ID1             PIC X(08)  VALUE 'RDSnnnn1'.           
           05  WS-REPORT-ID2             PIC X(08)  VALUE 'RDSnnnn2'.           
           05  WS-ACTION-VERB            PIC X(16)  VALUE SPACES.               
           05  WS-ERROR-MSG              PIC X(80)  VALUE SPACES.               
                                                                                
      *-----------------------------------------------------------------        
      * FLAT FILE RECORD VARIABLES.                                             
      *-----------------------------------------------------------------        
                                                                                
       01  FLAT-FILE-VARS.                                                      
           05  CARD-DATA-010       PIC X(16)  VALUE 'CARD-DATA-010   '.         
           05  PRINT-DATA-022      PIC X(16)  VALUE 'PRINT-DATA-022  '.         
           05  PRINT-DATA-023      PIC X(16)  VALUE 'PRINT-DATA-023  '.         
                                                                                
      *-----------------------------------------------------------------        
      * CALLED MODULES LITERALS                                                 
      *-----------------------------------------------------------------        
                                                                                
       01  CALLED-MOD-LITS.                                                     
           05  GAEDATSR                  PIC X(08)  VALUE 'GAEDATSR'.           
           05  INCGDUMP                  PIC X(08)  VALUE 'INCGDUMP'.           
           05  INCGOERR                  PIC X(08)  VALUE 'INCGOERR'.           
           05  INCGICSS                  PIC X(08)  VALUE 'INCGICSS'.           
           05  GC2DATE                   PIC X(08)  VALUE 'GC2DATE'.            
                                                                                
      *-----------------------------------------------------------------        
      *    GC2DATE PARAMETERS                                           01560069
      *-----------------------------------------------------------------        
                                                                                
       01  GAC-DATE-PARAMETERS.                                                 
           COPY GARDATEP.                                                       
                                                                                
      *---------------------------------------------------------------*         
      * INPUT - EFT FILE                                              *         
      *---------------------------------------------------------------*         
      * ROYAL BANK STD152 - FILE RECORD LAYOUT                                  
            COPY NAVEFTRB.                                                      
                                                                                
       01  RBC-EFT-TRAILER-RECD REDEFINES RBC-EFT-TRAILER.                      
           05 RBC-EFTT-RECD-FILLER1             PIC X(40).                      
           05 RBC-EFTT-RECD-TOT-PAY-TRANS       PIC 9(6).                       
           05 RBC-EFTT-RECD-TOT-PAY-AMT         PIC 9(12)V99.                   
           05 RBC-EFTT-RECD-FILLER2             PIC X(92).                      
                                                                                
      *---------------------------------------------------------------*         
      * OUTPUT - REPORT LINES                                         *         
      *---------------------------------------------------------------*         
       01  PRINT-REC.                                                           
           05 PRINT-LINE                        PIC X(133).                     
                                                                                
      *-----------------------------------------------------------------        
      * DATA FIELDS REQUIRED BY THE DATECARD ROUTINE                            
      *-----------------------------------------------------------------        
       01  GENERIC-DATECARD.                                                    
           COPY GARDTCRD.                                                       
                                                                                
      *-----------------------------------------------------------------        
       01  WS-FIELDS.                                                           
           05 WS-EOF-REQUEST           PIC X.                                   
           05 INPUT-STATUS             PIC X(02) VALUE SPACES.                  
           05 OUTPUT-STATUS            PIC X(02) VALUE SPACES.                  
           05 WS-LINE-COUNT            PIC S9(4)  COMP VALUE +0.                
           05 WS-LINE-COUNTC           PIC S9(4)  COMP VALUE +0.                
           05 WS-PAGE                  PIC S9(4)  COMP VALUE +0.                
           05 WS-PAGEC                 PIC S9(4)  COMP VALUE +0.                
           05 WS-TABLE-COUNT           PIC S9(5)  COMP-3.                       
           05 WS-SUB1                  PIC S9(5)  COMP VALUE +0.                
           05 WS-SUB2                  PIC S9(5)  COMP VALUE +0.                
           05 WS-CURR-POINTER          PIC S9(5)  COMP VALUE +0.                
           05 WS-SORT-SUB2             PIC S9(5)  COMP VALUE +0.                
           05 WS-TOT-ITEM-COUNT        PIC 9(07)  VALUE ZERO.                   
           05 WS-TOT-ITEM-COUNT-CHK    PIC 9(07)  VALUE ZERO.                   
           05 WS-TOT-DOLLAR-AMOUNT     PIC S9(13)V99 COMP-3 VALUE +0.           
           05 WS-HOLD-DATE-LINE        PIC X(22).                               
           05 WS-NULL-RPT-MSG.                                                  
              10 FILLER                PIC X(01).                               
              10 FILLER                PIC X(25) VALUE SPACES.                  
              10 FILLER                PIC X(30)                                
                              VALUE '****  INPUT FILE IS EMPTY ****'.           
              10 FILLER                PIC X(25) VALUE SPACES.                  
           05 WS-INPUT-DATA-INDICATOR  PIC  X(1).                               
              88 WS-INPUT-DATA-FOUND           VALUE 'Y'.                       
              88 WS-INPUT-DATA-NOT-FOUND       VALUE 'N'.                       
           05 WS-DATE-INDICATOR        PIC  X(1).                               
              88 WS-DATE-FOUND                 VALUE 'Y'.                       
              88 WS-DATE-NOT-FOUND             VALUE 'N'.                       
           05 WS-END-OF-TABLE-IND      PIC  X(1).                               
              88 WS-END-OF-TABLE               VALUE 'Y'.                       
              88 WS-NOT-END-OF-TABLE           VALUE 'N'.                       
           05 WS-SORT-COMPLETED-IND    PIC  X(1).                               
              88 WS-SORT-COMPLETED             VALUE 'Y'.                       
              88 WS-SORT-NOT-COMPLETED         VALUE 'N'.                       
           05 WS-FIRST-HEAD            PIC  X(1).                               
              88 FIRST-HEADER                  VALUE 'Y'.                       
              88 NOT-FIRST-HEADER              VALUE 'N'.                       
           05 WS-DEBIT-REC-PROCESS-SW  PIC  X(1).                               
              88 DEBIT-REC-PROCESS             VALUE 'Y'.                       
              88 NOT-DEBIT-REC-PROCESS         VALUE 'N'.                       
                                                                                
           05 WS-SYSTEM-DATE.                                                   
              10 WS-SYSTEM-YY          PIC 9(02).                               
              10 WS-SYSTEM-MM          PIC 9(02).                               
              10 WS-SYSTEM-DD          PIC 9(02).                               
           05 WS-HOLD-DATE.                                                     
              10 WS-HOLD-DD            PIC X(02).                               
              10 FILLER                PIC X(01) VALUE SPACE.                   
              10 WS-HOLD-MMM           PIC X(03).                               
              10 FILLER                PIC X(01) VALUE SPACE.                   
              10 WS-HOLD-CC            PIC X(02).                               
              10 WS-HOLD-YY            PIC X(02).                               
           05 WS-SYSTEM-TIME.                                                   
              10 WS-SYSTEM-HR          PIC X(02).                               
              10 WS-SYSTEM-MIN         PIC X(02).                               
              10 WS-SYSTEM-SEC         PIC X(02).                               
              10 WS-SYSTEM-HUN         PIC X(02).                               
           05 WS-HOLD-TIME.                                                     
              10 WS-HOLD-HR            PIC X(02).                               
              10 FILLER                PIC X(01) VALUE ':'.                     
              10 WS-HOLD-MIN           PIC X(02).                               
              10 FILLER                PIC X(01) VALUE ':'.                     
              10 WS-HOLD-SEC           PIC X(02).                               
                                                                                
           05 MONTH-DATA-R.                                                     
              10 FILLER                PIC X(03) VALUE 'JAN'.                   
              10 FILLER                PIC X(03) VALUE 'FEB'.                   
              10 FILLER                PIC X(03) VALUE 'MAR'.                   
              10 FILLER                PIC X(03) VALUE 'APR'.                   
              10 FILLER                PIC X(03) VALUE 'MAY'.                   
              10 FILLER                PIC X(03) VALUE 'JUN'.                   
              10 FILLER                PIC X(03) VALUE 'JUL'.                   
              10 FILLER                PIC X(03) VALUE 'AUG'.                   
              10 FILLER                PIC X(03) VALUE 'SEP'.                   
              10 FILLER                PIC X(03) VALUE 'OCT'.                   
              10 FILLER                PIC X(03) VALUE 'NOV'.                   
              10 FILLER                PIC X(03) VALUE 'DEC'.                   
           05 MONTH-TABLE REDEFINES MONTH-DATA-R.                               
              10 MTH-DAT OCCURS 12 TIMES.                                       
                 15 MTH-LIT            PIC X(03).                               
                                                                                
           05  WX-BRS-APPLX.                                                    
               10  FILLER         PIC  X(39)                                    
      **************************REGISTER*CONTROL***********************         
                      VALUE 'LH PGLPR002 PGLPR001      LH CLAIMS    '.          
               10  FILLER         PIC  X(39)                                    
                      VALUE 'HP PGLPR024 PGLPR023      HEALTHPRO    '.          
               10  FILLER         PIC  X(39)                                    
                      VALUE 'OP PGLPR033 PGLPR032      OPERA        '.          
           05  WX-BRS-APPL  REDEFINES  WX-BRS-APPLX  OCCURS 3 TIMES             
                                      INDEXED BY WX-IND.                        
               10  WX-APLCD       PIC  XX.                                      
               10  FILLER         PIC  X.                                       
               10  WX-RDS1        PIC  X(08).                                   
               10  FILLER         PIC  X.                                       
               10  WX-RDS2        PIC  X(08).                                   
               10  FILLER         PIC  X.                                       
               10  WX-BRSAPL      PIC  X(18).                                   
      *                                                                         
       01  WS-DATE-ARRAY.                                                       
           05 WS-DA-DATE-LINE OCCURS 346 TIMES.                                 
              10 WS-DA-VALUE-DATE           PIC 9(07).                          
              10 WS-DA-ITEM-COUNT           PIC 9(07).                          
              10 WS-DA-DOLLAR-AMOUNT        PIC S9(13)V99 COMP-3.               
                                                                                
       01  WS-ABEND-AREAS.                                                      
           05  WS-ABEND-PARAGRAPH-NAME  PIC X(12).                              
           05  WS-ABEND-SQL-COMMAND     PIC X(08).                              
      *                                                                         
      *---------------------------------------------------------------*         
      * OUTPUT - REPORT LINES (payment register report)               *         
      *---------------------------------------------------------------*         
                                                                                
       01  WS-HDR-LINE.                                                         
           05 WS-HDR-CTL        PIC X(1)  VALUE '1'.                            
           05 WS-HDR-RDS        PIC X(8)  VALUE SPACES.                         
           05 FILLER            PIC X     VALUE '/'.                            
           05 WS-HDR-PGM        PIC X(8)  VALUE SPACES.                         
           05 FILLER            PIC X(2)  VALUE SPACES.                         
           05 WS-HDR-DATE       PIC X(13).                                      
           05 WS-HDR-TIME       PIC X(08).                                      
           05 FILLER            PIC X(27) VALUE SPACES.                         
           05 FILLER            PIC X(09) VALUE 'PAGE NO. '.                    
           05 WS-PAGE-NO        PIC ZZ9.                                        
                                                                                
       01  WS-HDR-LINE1.                                                        
           05 WS-HDR-CTL1       PIC X(1)  VALUE SPACE.                          
           05 filler            PIC X(12) VALUE SPACES.                         
           05 FILLER            PIC X(15) VALUE SPACES.                         
           05 FILLER            PIC X(30) VALUE '      MANULIFE FINANCIA        
      -    'L      '.                                                           
           05 FILLER            PIC X(30) VALUE SPACES.                         
       01  WS-HDR-LINE1B.                                                       
           05 WS-HDR-CTL1B      PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(33) VALUE SPACES.                         
           05 WS-HDR-APL        PIC X(18).                                      
           05 FILLER            PIC X(36) VALUE SPACES.                         
                                                                                
       01  WS-HDR-LINE2.                                                        
           05 WS-HDR-CTL2       PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(26) VALUE SPACES.                         
           05 FILLER            PIC X(32) VALUE ' DIRECT DEPOSIT PAYMENT        
      -    ' REGISTER'.                                                         
           05 FILLER            PIC X(18) VALUE SPACES.                         
                                                                                
       01  WS-DTL-LINE2.                                                        
           05 WS-DTL-CTL        PIC X(1)  VALUE '0'.                            
           05 DD-DD-REF-ID      PIC X(11).                                      
           05 FILLER            PIC X(01).                                      
           05 DD-DD-AM          PIC ZZZ,ZZZ.99.                                 
           05 FILLER            PIC X(02).                                      
           05 DD-DD-DUE-DT      PIC X(10).                                      
           05 FILLER            PIC X(02).                                      
           05 DD-PYE-DD-INST-NO PIC X(04).                                      
           05 FILLER            PIC X(02).                                      
           05 DD-PYE-DD-BR-NO   PIC X(05).                                      
           05 FILLER            PIC X(02).                                      
           05 DD-PYE-DD-AC-NO   PIC X(12).                                      
           05 FILLER            PIC X(01).                                      
           05 DD-DD-BAT-CREA-DT PIC X(10).                                      
           05 FILLER            PIC X(02).                                      
           05 DD-DD-BAT-NO      PIC X(04).                                      
                                                                                
       01  WS-DTL-LINE0.                                                        
           05 WS-DTL-CTL0       PIC X(1)  VALUE '0'.                            
           05 FILLER            PIC X(10) VALUE 'REF. ID   '.                   
           05 FILLER            PIC X(02) VALUE SPACE.                          
           05 FILLER            PIC X(09) VALUE '  AMOUNT '.                    
           05 FILLER            PIC X(02) VALUE SPACE.                          
           05 FILLER            PIC X(12) VALUE ' DUE DATE   '.                 
           05 FILLER            PIC X(01) VALUE SPACE.                          
           05 FILLER            PIC X(05) VALUE 'INST.'.                        
           05 FILLER            PIC X(01) VALUE SPACE.                          
           05 FILLER            PIC X(06) VALUE 'BRANCH'.                       
           05 FILLER            PIC X(01) VALUE SPACE.                          
           05 FILLER            PIC X(13) VALUE '  ACCT NO    '.                
           05 FILLER            PIC X(10) VALUE 'BATCH DATE'.                   
           05 FILLER            PIC X(11) VALUE '  BATCH NO.'.                  
                                                                                
       01  WS-DTL-LINE1.                                                        
           05 WS-DTL-CTL1       PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(10) VALUE '----------'.                   
           05 FILLER            PIC X(02) VALUE SPACE.                          
           05 FILLER            PIC X(10) VALUE '----------'.                   
           05 FILLER            PIC X(02) VALUE SPACE.                          
           05 FILLER            PIC X(11) VALUE '-----------'.                  
           05 FILLER            PIC X(01) VALUE SPACE.                          
           05 FILLER            PIC X(05) VALUE '-----'.                        
           05 FILLER            PIC X(01) VALUE SPACE.                          
           05 FILLER            PIC X(06) VALUE '------'.                       
           05 FILLER            PIC X(01) VALUE SPACE.                          
           05 FILLER            PIC X(13) VALUE '------------ '.                
           05 FILLER            PIC X(10) VALUE '----------'.                   
           05 FILLER            PIC X(11) VALUE ' ----------'.                  
                                                                                
       01  LAST-LINE.                                                           
           05 LAST-CC                            PIC X     VALUE '0'.           
           05 FILLER                             PIC X(40) VALUE SPACES.        
           05 FILLER                             PIC X(41) VALUE                
               '* * *   E N D   O F   R E P O R T   * * *'.                     
           05 FILLER                             PIC X(51) VALUE SPACES.        
                                                                                
      *---------------------------------------------------------------*         
      * OUTPUT - REPORT LINES (control report)                        *         
      *---------------------------------------------------------------*         
                                                                                
       01  WS-HDR-LINEC.                                                        
           05 FILLER            PIC X(1)  VALUE '1'.                            
           05 WS-HDR-RDSC       PIC X(8)  VALUE SPACES.                         
           05 FILLER            PIC X     VALUE '/'.                            
           05 WS-HDR-PGMC       PIC X(8)  VALUE SPACES.                         
           05 FILLER            PIC X(2)  VALUE SPACES.                         
           05 WS-HDR-DATEC      PIC X(13).                                      
           05 WS-HDR-TIMEC      PIC X(08).                                      
           05 FILLER            PIC X(27) VALUE SPACES.                         
           05 FILLER            PIC X(09) VALUE 'PAGE NO. '.                    
           05 WS-PAGE-NOC       PIC ZZ9.                                        
                                                                                
       01  WS-HDR-LINE1C.                                                       
           05 FILLER            PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(22) VALUE SPACES.                         
           05 FILLER            PIC X(30) VALUE '      MANULIFE FINANCIA        
      -    'L      '.                                                           
           05 FILLER            PIC X(27) VALUE SPACES.                         
       01  WS-HDR-LINE1BC.                                                      
           05 WS-HDR-CTL1BC     PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(28) VALUE SPACES.                         
           05 WS-HDR-APLC       PIC X(18).                                      
           05 FILLER            PIC X(33) VALUE SPACES.                         
       01  WS-HDR-LINE2C.                                                       
           05 WS-HDR-CTL2C      PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(16) VALUE SPACES.                         
           05 FILLER            PIC X(43) VALUE ' DIRECT DEPOSIT PAYMENT        
      -    ' FILE CONTROL REPORT'.                                              
           05 FILLER            PIC X(20) VALUE SPACES.                         
       01  WS-DTL-LINE1C.                                                       
           05 WS-DTL1-CTLC      PIC X(1)  VALUE '0'.                            
           05 FILLER            PIC X(05) VALUE SPACES.                         
           05 FILLER            PIC X(23)                                       
                                    VALUE 'CUSTOMER NUMBER      : '.            
           05 WS-CUST-NOC       PIC X(10).                                      
           05 FILLER            PIC X(02) VALUE SPACES.                         
       01  WS-DTL-LINE2C.                                                       
           05 WS-DTL2-CTLC      PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(05) VALUE SPACES.                         
           05 FILLER            PIC X(23)                                       
                                    VALUE 'FILE CREATION DATE   : '.            
           05 FILLER            PIC X(04) VALUE SPACES.                         
           05 WS-CREAT-DTC      PIC Z99999.                                     
           05 FILLER            PIC X(02) VALUE SPACES.                         
       01  WS-DTL-LINE3C.                                                       
           05 WS-DTL4-CTLC      PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(05) VALUE SPACES.                         
           05 FILLER            PIC X(23)                                       
                                    VALUE 'FILE CREATION NUMBER : '.            
           05 FILLER            PIC X(06) VALUE SPACES.                         
           05 WS-CREAT-NOC      PIC ZZZZ.                                       
           05 WS-CREAT-NOC-x   REDEFINES WS-CREAT-NOC                           
                                PIC X(4).                                       
           05 FILLER            PIC X(02) VALUE SPACES.                         
       01  WS-COL-HEAD-1C.                                                      
           05 WS-CHD1-CTLC      PIC X(1)  VALUE '0'.                            
           05 FILLER            PIC X(10) VALUE SPACES.                         
           05 FILLER            PIC X(10) VALUE 'VALUE DATE'.                   
           05 FILLER            PIC X(05) VALUE SPACES.                         
           05 FILLER            PIC X(10) VALUE 'ITEM COUNT'.                   
           05 FILLER            PIC X(08) VALUE SPACES.                         
           05 FILLER            PIC X(13) VALUE 'DOLLAR AMOUNT'.                
           05 FILLER            PIC X(05) VALUE SPACES.                         
       01  WS-COL-HEAD-2C.                                                      
           05 WS-CHD2-CTLC      PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(10) VALUE SPACES.                         
           05 FILLER            PIC X(10) VALUE '----------'.                   
           05 FILLER            PIC X(05) VALUE SPACES.                         
           05 FILLER            PIC X(10) VALUE '----------'.                   
           05 FILLER            PIC X(08) VALUE SPACES.                         
           05 FILLER            PIC X(13) VALUE '-------------'.                
           05 FILLER            PIC X(05) VALUE SPACES.                         
       01  WS-COL-DETAILC.                                                      
           05 WS-DTL4-CTLA      PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(09) VALUE SPACES.                         
           05 WS-COL-DDC        PIC X(02).                                      
           05 FILLER            PIC X(01) VALUE SPACE.                          
           05 WS-COL-MMMMC      PIC X(04).                                      
           05 FILLER            PIC X(01) VALUE SPACE.                          
           05 WS-COL-YYYYC      PIC X(04).                                      
           05 FILLER            PIC X(03) VALUE SPACES.                         
           05 WS-COL-ITEM-CNTC  PIC ZZZ,ZZZ,ZZ9.                                
           05 FILLER            PIC X(01) VALUE SPACES.                         
           05 WS-COL-AMOUNTC    PIC $,$$$,$$$,$$$,$$9.99-.                      
           05 FILLER            PIC X(05) VALUE SPACES.                         
       01  WS-TOTAL-UNDLC.                                                      
           05 WS-DTL4-CTLB      PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(24) VALUE SPACES.                         
           05 FILLER            PIC X(11) VALUE '-----------'.                  
           05 FILLER            PIC X(01) VALUE SPACES.                         
           05 FILLER            PIC X(20) VALUE '--------------------'.         
           05 FILLER            PIC X(05) VALUE SPACES.                         
       01  WS-TOTAL-LINEC.                                                      
           05 WS-DTL4-CTLD      PIC X(1)  VALUE SPACE.                          
           05 FILLER            PIC X(10) VALUE SPACES.                         
           05 FILLER            PIC X(02) VALUE SPACES.                         
           05 FILLER            PIC X(06) VALUE 'TOTALS'.                       
           05 FILLER            PIC X(06) VALUE SPACES.                         
           05 WS-TOT-ITEM-CNTC  PIC ZZZ,ZZZ,ZZ9.                                
           05 FILLER            PIC X(01) VALUE SPACES.                         
           05 WS-TOT-AMOUNTC    PIC $,$$$,$$$,$$$,$$9.99-.                      
           05 FILLER            PIC X(05) VALUE SPACES.                         
                                                                                
       01  WS-END-MARKER.                                                       
           05  FILLER                   PIC X(32) VALUE                         
               '**** NAVCREFT WS ENDS HERE *****'.                              
                                                                                
      *---------------------------------------------------------------*         
      * ABEND MESSAGE PROCESSING                                                
       01 INRGDUMP-FILLER PIC X(16) VALUE 'INRGDUMP DATA   '.                   
       01 INRGDUMP-RECORD.                                                      
          COPY INRGDUMP.                                                        
                                                                                
       01 INRGCTRL-RECORD.                                                      
          COPY INRGCTRL.                                                        
                                                                                
      *---------------------------------------------------------------*         
      *    COPYBOOK/FIELD/VERB FOR DATA SERVICE VERB                  *         
      *---------------------------------------------------------------*         
       01  DATA-SERVICE-VERB.                                                   
           COPY GARDSVRB.                                                       
                                                                                
      *---------------------------------------------------------------*         
      *    DATASERVER - INTERFACE CONTROL BLOCK                       *         
      *---------------------------------------------------------------*         
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
      *----------------------------------------------------------------         
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------         
                                                                                
       01  PARM-DATA.                                                           
           03  PARM-LEN                     PIC S9(4)      COMP.                
           03  PARM-IND                     PIC XX.                             
           03  FILLER                       PIC X(59).                          
                                                                                
      *----------------------------------------------------------------         
       PROCEDURE DIVISION  USING PARM-DATA.                                     
      *----------------------------------------------------------------         
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INIT THRU 1000-EXIT.                                    
                                                                                
           PERFORM 2000-PROCESS-ROUTINE THRU                                    
                   2000-EXIT                                                    
                   UNTIL WS-EOF-REQUEST = 'Y'                                   
                                                                                
           PERFORM 9000-FINISH THRU 9000-EXIT.                                  
                                                                                
       0000-EXIT.                                                               
           STOP RUN.                                                            
      *----------------------------------------------------------------         
       1000-INIT.                                                               
                                                                                
           DISPLAY '***********************************************'            
           DISPLAY '***        START   BRSI0400                 ***'            
           DISPLAY '***********************************************'.           
                                                                                
      * Initialize DATASERVER                                                   
           MOVE  WS-PROGRAM-ID      TO  ICBM-PROGRAM-NAME                       
                                        WS-HDR-PGM                              
                                        WS-HDR-PGMC.                            
           MOVE  LOW-VALUES         TO  LINKAGE-CONTROL.                        
           MOVE  ZEROES             TO  LINKAGE-STATUS.                         
                                                                                
           SET  WX-IND        TO  +1.                                           
           SEARCH  WX-BRS-APPL                                                  
                   VARYING WX-IND                                               
               AT  END                                                          
                 DISPLAY 'ERROR IN PARM-CARD, INVALID VALUE = ' PARM-IND        
                 DISPLAY '*** REQUIRED VALUE FOR LH CLAIMS = LH '               
                 DISPLAY '*** OR                                '               
                 DISPLAY '*** REQUIRED VALUE FOR HEALTHPRO = HP '               
                 DISPLAY '*** OR                                '               
                 DISPLAY '*** REQUIRED VALUE FOR OPERA = OP     '               
                   MOVE WS-ERROR-PARM-CARD       TO WS-ERROR-MSG                
                   PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                  
                                                                                
              WHEN  WX-APLCD (WX-IND)  =  PARM-IND                              
                    MOVE  WX-BRSAPL (WX-IND)      TO  WS-HDR-APL                
                                                      WS-HDR-APLC               
                    MOVE  WX-RDS1 (WX-IND)        TO  WS-HDR-RDS                
                    MOVE  WX-RDS2 (WX-IND)        TO  WS-HDR-RDSC               
            END-SEARCH.                                                         
                                                                                
      * Read first record from the EFT file                                     
      *      (program abends if the first record was not read)                  
      *      (program abends if the first record is NOT header)                 
           SET  WS-INPUT-DATA-NOT-FOUND                                         
                                    TO  TRUE                                    
           MOVE  OBTAIN-FIRST       TO  WS-ACTION-VERB                          
           PERFORM 8000-READ-EFT-FILE  THRU                                     
                   8000-EXIT.                                                   
           IF  WS-EOF-REQUEST = 'Y'                                             
               MOVE WS-OBTAIN-REC-EF-ERROR TO WS-ERROR-MSG                      
               PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                      
           ELSE                                                                 
               IF  RBC-EFTD-REC-TYPE = 'A'                                      
                   SET FIRST-HEADER          TO TRUE                            
                   SET WS-INPUT-DATA-FOUND   TO TRUE                            
                   SET NOT-DEBIT-REC-PROCESS TO TRUE                            
                   MOVE RBC-EFT-DETAIL TO RBC-EFT-HEADER                        
               ELSE                                                             
                   MOVE WS-OBTAIN-REC-EF-NOHEAD TO WS-ERROR-MSG                 
                   PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                  
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * Getting this far means that the program can proceed                     
                                                                                
      * Initialize working storage variables                                    
           MOVE SPACES              TO WS-EOF-REQUEST                           
           SET  TRAILER-REC-NOT-FOUND                                           
                                    TO TRUE                                     
                                                                                
      * Get system date and format for report                                   
           ACCEPT WS-SYSTEM-DATE     FROM DATE                                  
                                                                                
           IF WS-SYSTEM-YY  >  70                                               
           THEN                                                                 
              MOVE '19'                TO WS-HOLD-CC                            
           ELSE                                                                 
              MOVE '20'                TO WS-HOLD-CC                            
           END-IF                                                               
                                                                                
           MOVE WS-SYSTEM-YY           TO WS-HOLD-YY                            
           MOVE MTH-LIT (WS-SYSTEM-MM) TO WS-HOLD-MMM                           
           MOVE WS-SYSTEM-DD           TO WS-HOLD-DD                            
           MOVE WS-HOLD-DATE           TO WS-HDR-DATE                           
                                          WS-HDR-DATEC                          
                                                                                
      * Get system time and format for report                                   
           ACCEPT WS-SYSTEM-TIME     FROM TIME                                  
                                                                                
           MOVE WS-SYSTEM-HR           TO WS-HOLD-HR                            
           MOVE WS-SYSTEM-MIN          TO WS-HOLD-MIN                           
           MOVE WS-SYSTEM-SEC          TO WS-HOLD-SEC                           
           MOVE WS-HOLD-TIME           TO WS-HDR-TIME                           
                                          WS-HDR-TIMEC                          
                                                                                
           INITIALIZE WS-DATE-ARRAY.                                            
           MOVE +99                    TO WS-LINE-COUNTC                        
           INITIALIZE WS-TABLE-COUNT.                                           
                                                                                
           PERFORM 4000-PRINT-HEADINGS THRU                                     
                   4000-EXIT.                                                   
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
      *2000-PROCESS-ROUTINE.                                                    
      * Select only payment records (record type 'C' or 'D') and trailer        
      * record (record type 'Z'). The payment records are used to build         
      * an array for the control report. The trailer record totals are          
      * used to tally with the total obtained from the control report.          
      * the two totals are then compared and if different, an error             
      * message is displayed.                                                   
      * Abend if:                                                               
      *       (a) header records in addition to the first                       
      *       (b) unknow record type encountered (other than A,C,D,Z)           
      *       (c) NO trailer record was found                                   
      *-----------------------------------------------------------------        
       2000-PROCESS-ROUTINE.                                                    
                                                                                
           IF  RBC-EFTD-REC-TYPE = 'A' OR 'C' OR 'D' OR 'Z'                     
               IF  RBC-EFTD-REC-TYPE = 'C' OR 'D'                               
                   PERFORM 2100-ADD-TO-ARRAY THRU 2100-EXIT                     
                   PERFORM 2900-WRITE-PAYMENT-REGISTER THRU                     
                           2900-EXIT                                            
               ELSE                                                             
                   IF  RBC-EFTD-REC-TYPE = 'Z'                                  
                       MOVE RBC-EFT-DETAIL TO RBC-EFT-TRAILER                   
                       PERFORM 2300-WRITE-CONTROL-REPORT THRU 2300-EXIT         
                       SET TRAILER-REC-FOUND TO TRUE                            
                   ELSE                                                         
                       IF  RBC-EFTD-REC-TYPE = 'A'                              
                           SET NOT-DEBIT-REC-PROCESS TO TRUE                    
                           IF  FIRST-HEADER                                     
                               SET NOT-FIRST-HEADER TO TRUE                     
                           ELSE                                                 
                               MOVE WS-ERROR-HEADER-EXTRA  TO                   
                                    WS-ERROR-MSG                                
                               PERFORM 9999-FATAL-ERROR-RTN THRU                
                                       9999-EXIT                                
                           END-IF                                               
                       END-IF                                                   
                   END-IF                                                       
               END-IF                                                           
           ELSE                                                                 
               DISPLAY ' '                                                      
               DISPLAY '** RBC-EFTD-REC-TYPE ' RBC-EFTD-REC-TYPE                
               DISPLAY ' '                                                      
               MOVE WS-ERROR-UNKNOWN-REC   TO WS-ERROR-MSG                      
               PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                      
           END-IF                                                               
                                                                                
           MOVE  OBTAIN-NEXT       TO  WS-ACTION-VERB                           
                                                                                
           PERFORM 8000-READ-EFT-FILE  THRU                                     
                   8000-EXIT                                                    
                                                                                
           IF  WS-EOF-REQUEST = 'Y'                                             
               IF  TRAILER-REC-NOT-FOUND                                        
                   MOVE WS-ERROR-TRAILER-NF    TO WS-ERROR-MSG                  
                   PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                  
               END-IF                                                           
           END-IF.                                                              
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2100-ADD-TO-ARRAY.                                                       
                                                                                
           IF DEBIT-REC-PROCESS                                                 
               IF RBC-EFTD-REC-TYPE = 'C'                                       
                   MOVE WS-BOTH-CREDIT-N-DEBIT TO WS-ERROR-MSG                  
                   PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                  
               END-IF                                                           
           ELSE                                                                 
               IF RBC-EFTD-REC-TYPE = 'D'                                       
                   SET DEBIT-REC-PROCESS   TO TRUE                              
               END-IF                                                           
           END-IF.                                                              
                                                                                
           PERFORM 2200-SEARCH-DATE-ARRAY THRU 2200-EXIT                        
                                                                                
           IF  WS-DATE-FOUND                                                    
      *        date match in array, update the table entry                      
               ADD 1                 TO WS-DA-ITEM-COUNT (WS-SUB2)              
               ADD RBC-EFTD-PAYMENT-AMT                                         
                                     TO WS-DA-DOLLAR-AMOUNT (WS-SUB2)           
           ELSE                                                                 
      *        no date match in array, create a new table entry                 
                 ADD 1                 TO WS-TABLE-COUNT                        
                 MOVE RBC-EFTD-PAYMENT-DATE                                     
                                       TO WS-DA-VALUE-DATE (WS-SUB2)            
                 MOVE 1                TO WS-DA-ITEM-COUNT (WS-SUB2)            
                 MOVE RBC-EFTD-PAYMENT-AMT                                      
                                       TO WS-DA-DOLLAR-AMOUNT (WS-SUB2)         
           END-IF.                                                              
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
      * The date array has 346 entries (i.e. 346 different dates)               
      * If someone is curious enough to question the number 346, here is        
      *    a brief explanation why I think it should be 346.                    
      *    Explanation: The Royal Bank Standard (STD152) Debit File             
      *                 Formation specifications has a rule for payment         
      *                 date. The rule says that the payment due date           
      *                 cannot be more than 173 days in the past or 173         
      *                 days in the futures. I hope that by now you             
      *                 would have figured out how the array size came          
      *                 to be 346. If not, keep reading :-)                     
      *                 From the rule, it can be inferred that the              
      *                 maximum possible dates in the EFT file are NOT          
      *                 expected to be more than 346 (173 + 173).               
      *-----------------------------------------------------------------        
       2200-SEARCH-DATE-ARRAY.                                                  
                                                                                
           SET WS-DATE-NOT-FOUND       TO TRUE                                  
           MOVE 1                      TO WS-SUB2                               
           PERFORM UNTIL WS-SUB2                        >  346     OR           
                         WS-DA-VALUE-DATE (WS-SUB2) NOT >  ZEROES  OR           
                         WS-DATE-FOUND                                          
                                                                                
              IF  WS-DA-VALUE-DATE (WS-SUB2) = RBC-EFTD-PAYMENT-DATE            
                  SET WS-DATE-FOUND    TO TRUE                                  
              ELSE                                                              
                  ADD 1                TO WS-SUB2                               
              END-IF                                                            
                                                                                
           END-PERFORM                                                          
                                                                                
           IF  WS-SUB2  >  346                                                  
               MOVE WS-ARRAY-ERROR-SIZE TO WS-ERROR-MSG                         
               PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                      
           END-IF.                                                              
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2300-WRITE-CONTROL-REPORT.                                               
                                                                                
           IF  WS-TABLE-COUNT  >  1                                             
               PERFORM 2400-SORT-TABLE-ENTRIES THRU 2400-EXIT                   
           END-IF                                                               
                                                                                
           PERFORM 2500-PRODUCE-REPORT THRU 2500-EXIT                           
                                                                                
      *    control totals do not match, abend the program                       
           IF DEBIT-REC-PROCESS                                                 
               IF WS-TOT-ITEM-COUNT NOT = RBC-EFTT-RECD-TOT-PAY-TRANS           
                  OR                                                            
                  WS-TOT-DOLLAR-AMOUNT NOT = RBC-EFTT-RECD-TOT-PAY-AMT          
                   MOVE WS-TOTALS-ERROR-CT     TO WS-ERROR-MSG                  
                   PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                  
               END-IF                                                           
           ELSE                                                                 
               IF WS-TOT-ITEM-COUNT NOT = RBC-EFTT-TOT-PAYMENT-TRANS            
                  OR                                                            
                  WS-TOT-DOLLAR-AMOUNT NOT = RBC-EFTT-TOT-PAYMENT-AMT           
                   MOVE WS-TOTALS-ERROR-CT     TO WS-ERROR-MSG                  
                   PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                  
               END-IF                                                           
           END-IF.                                                              
                                                                                
       2300-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2400-SORT-TABLE-ENTRIES.                                                 
                                                                                
           MOVE 1                      TO WS-CURR-POINTER                       
           SET WS-SORT-NOT-COMPLETED   TO TRUE                                  
                                                                                
           PERFORM    UNTIL  WS-SORT-COMPLETED                                  
                                                                                
              COMPUTE WS-SORT-SUB2  =                                           
                      WS-CURR-POINTER  +  1                                     
              SET WS-NOT-END-OF-TABLE  TO TRUE                                  
              PERFORM  VARYING  WS-SORT-SUB2                                    
                          FROM  WS-SORT-SUB2  BY  1                             
                         UNTIL  WS-END-OF-TABLE                                 
                                                                                
                 IF WS-DA-VALUE-DATE (WS-SORT-SUB2)  >  ZEROES                  
                    IF WS-DA-VALUE-DATE (WS-SORT-SUB2)  IS LESS THAN            
                       WS-DA-VALUE-DATE (WS-CURR-POINTER)                       
      *                swap the date line values                                
                       MOVE WS-DA-DATE-LINE (WS-SORT-SUB2)                      
                                       TO WS-HOLD-DATE-LINE                     
                       MOVE WS-DA-DATE-LINE (WS-CURR-POINTER)                   
                                       TO WS-DA-DATE-LINE (WS-SORT-SUB2)        
                       MOVE WS-HOLD-DATE-LINE                                   
                                    TO WS-DA-DATE-LINE (WS-CURR-POINTER)        
                    END-IF                                                      
                 ELSE                                                           
      *             end of table for this search has been reached               
                    SET WS-END-OF-TABLE TO TRUE                                 
                 END-IF                                                         
                                                                                
              END-PERFORM                                                       
                                                                                
              ADD  1                    TO WS-CURR-POINTER                      
                                                                                
              IF  WS-DA-VALUE-DATE (WS-CURR-POINTER)  >  ZEROES                 
      *           start the search over for the next entry                      
                  CONTINUE                                                      
              ELSE                                                              
      *           sort is completed                                             
                  SET WS-SORT-COMPLETED TO TRUE                                 
              END-IF                                                            
                                                                                
           END-PERFORM.                                                         
                                                                                
       2400-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
      *2500-PRODUCE-REPORT.                                                     
      * If the input file HAS data    - produce control report                  
      * If the input file has NO data - produce NULL    report                  
      *-----------------------------------------------------------------        
       2500-PRODUCE-REPORT.                                                     
                                                                                
           IF  WS-INPUT-DATA-FOUND                                              
               PERFORM 2600-PRINT-HEADINGS THRU 2600-EXIT                       
               PERFORM 2700-PRINT-DETAILS  THRU 2700-EXIT                       
           ELSE                                                                 
               PERFORM 2800-PRINT-NULL-RPT THRU 2800-EXIT                       
           END-IF.                                                              
                                                                                
       2500-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2600-PRINT-HEADINGS.                                                     
                                                                                
           PERFORM 2610-COMMON-HEADINGS THRU 2610-EXIT                          
                                                                                
           MOVE RBC-EFTH-CLIENT-NUM  TO WS-CUST-NOC                             
           MOVE WS-DTL-LINE1C        TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU                                       
                   8100-EXIT                                                    
                                                                                
           MOVE RBC-EFTH-FILE-CREATION-DATE                                     
                                     TO WS-CREAT-DTC                            
           MOVE WS-DTL-LINE2C        TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU                                       
                   8100-EXIT                                                    
                                                                                
                                                                                
      * check table if there are any entries                                    
           MOVE 1                TO WS-SUB2                                     
           PERFORM UNTIL WS-SUB2  >  346  OR                                    
                         WS-DA-VALUE-DATE (WS-SUB2) NOT >  ZEROES               
               ADD WS-DA-ITEM-COUNT (WS-SUB2)                                   
                                   TO WS-TOT-ITEM-COUNT-CHK                     
               ADD 1               TO WS-SUB2                                   
           END-PERFORM                                                          
                                                                                
           IF  WS-TOT-ITEM-COUNT-CHK = 0                                        
               MOVE ZEROS TO  WS-CREAT-NOC                                      
           ELSE                                                                 
               IF RBC-EFTH-FILE-CREATION-NUM  NUMERIC                           
                   MOVE RBC-EFTH-FILE-CREATION-NUM                              
                              TO WS-CREAT-NOC                                   
               ELSE                                                             
                   MOVE RBC-EFTH-FILE-CREATION-NUM                              
                              TO WS-CREAT-NOC-X                                 
               END-IF                                                           
           END-IF                                                               
                                                                                
           MOVE WS-DTL-LINE3C        TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU                                       
                   8100-EXIT                                                    
                                                                                
           MOVE WS-COL-HEAD-1C       TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU                                       
                   8100-EXIT                                                    
                                                                                
           MOVE WS-COL-HEAD-2C       TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU                                       
                   8100-EXIT                                                    
                                                                                
           MOVE 10       TO WS-LINE-COUNTC.                                     
                                                                                
       2600-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2610-COMMON-HEADINGS.                                                    
           ADD  1                    TO WS-PAGEC                                
           MOVE WS-PAGEC             TO WS-PAGE-NOC                             
           MOVE WS-HDR-LINEC         TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU 8100-EXIT                             
           MOVE WS-HDR-LINE1C        TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU 8100-EXIT                             
           MOVE WS-HDR-LINE1BC       TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU 8100-EXIT                             
           MOVE WS-HDR-LINE2C        TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU 8100-EXIT                             
           MOVE SPACES               TO PRINT-REC                               
           PERFORM 8100-WRITE-REPORT THRU 8100-EXIT.                            
                                                                                
       2610-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2700-PRINT-DETAILS.                                                      
                                                                                
      * print each existing entry in the array                                  
           MOVE 1                TO WS-SUB2                                     
           PERFORM UNTIL WS-SUB2  >  346  OR                                    
                         WS-DA-VALUE-DATE (WS-SUB2) NOT >  ZEROES               
                                                                                
               MOVE WS-DA-VALUE-DATE (WS-SUB2)  TO  WS-PAYMENT-DATE             
      *        convert Julian date to Gregorian date                            
               PERFORM 2710-CONVERT-JULIAN-TO-GREG THRU 2710-EXIT               
               MOVE VDATE1-YYYY    TO WS-COL-YYYYC                              
               MOVE VDATE1-MM      TO WS-COL-MMMMC                              
               MOVE VDATE1-DD      TO WS-COL-DDC                                
               MOVE '-'               TO WS-FILLER1                             
                                         WS-FILLER2                             
                                                                                
                                                                                
                                                                                
               MOVE WS-DA-ITEM-COUNT (WS-SUB2)                                  
                                   TO WS-COL-ITEM-CNTC                          
               ADD WS-DA-ITEM-COUNT (WS-SUB2)                                   
                                   TO WS-TOT-ITEM-COUNT                         
               MOVE WS-DA-DOLLAR-AMOUNT (WS-SUB2)                               
                                   TO WS-COL-AMOUNTC                            
               ADD WS-DA-DOLLAR-AMOUNT (WS-SUB2)                                
                                   TO WS-TOT-DOLLAR-AMOUNT                      
                                                                                
               IF  WS-LINE-COUNTC > 55                                          
                   PERFORM 2610-COMMON-HEADINGS THRU 2610-EXIT                  
                   MOVE 4                    TO WS-LINE-COUNTC                  
               END-IF                                                           
                                                                                
               MOVE WS-COL-DETAILC  TO PRINT-REC                                
               PERFORM 8100-WRITE-REPORT THRU                                   
                       8100-EXIT                                                
               ADD 1               TO WS-SUB2                                   
                                                                                
           END-PERFORM                                                          
                                                                                
           MOVE WS-TOTAL-UNDLC     TO PRINT-REC                                 
           PERFORM 8100-WRITE-REPORT THRU                                       
                   8100-EXIT                                                    
                                                                                
           MOVE WS-TOT-ITEM-COUNT  TO WS-TOT-ITEM-CNTC                          
           MOVE WS-TOT-DOLLAR-AMOUNT                                            
                                   TO WS-TOT-AMOUNTC                            
           MOVE WS-TOTAL-LINEC     TO PRINT-REC                                 
           PERFORM 8100-WRITE-REPORT THRU                                       
                   8100-EXIT.                                                   
                                                                                
           IF  WS-TOT-ITEM-COUNT = 0                                            
               MOVE SPACES TO PRINT-REC                                         
               PERFORM 8100-WRITE-REPORT THRU                                   
                       8100-EXIT                                                
               MOVE                                                             
               '                        THERE ARE NO EFT PAYMENTS TODAY'        
                                         TO PRINT-REC                           
               PERFORM 8100-WRITE-REPORT THRU                                   
                       8100-EXIT                                                
               MOVE SPACES TO PRINT-REC                                         
               PERFORM 8100-WRITE-REPORT THRU                                   
                       8100-EXIT                                                
           END-IF.                                                              
                                                                                
       2700-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2710-CONVERT-JULIAN-TO-GREG.                                             
                                                                                
           MOVE WS-PAYMENT-DATE            TO VDATE-JULIAN-DATE                 
           MOVE 'D'                        TO VDATE-REQ-SERVICE                 
           MOVE 'A'                        TO VDATE-REQ-BASIS                   
           MOVE '1'                        TO VDATE-REQ-DETAIL                  
           MOVE 'E'                        TO VDATE-REQ-LANGUAGE                
                                                                                
           CALL GC2DATE             USING GAC-DATE-PARAMETERS.                  
                                                                                
           IF  VDATE-RET-FAIL                                                   
              MOVE WS-DATE-CONV-ERROR TO WS-ERROR-MSG                           
              PERFORM 9999-FATAL-ERROR-RTN THRU 9999-EXIT                       
           END-IF.                                                              
                                                                                
       2710-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2800-PRINT-NULL-RPT.                                                     
                                                                                
           MOVE SPACES          TO PRINT-REC                                    
           PERFORM 8100-WRITE-REPORT THRU 8100-EXIT                             
                   3 TIMES                                                      
                                                                                
           MOVE WS-NULL-RPT-MSG TO PRINT-REC                                    
           PERFORM 8100-WRITE-REPORT THRU 8100-EXIT                             
                   5 TIMES.                                                     
                                                                                
       2800-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2900-WRITE-PAYMENT-REGISTER.                                             
                                                                                
           MOVE SPACES                  TO WS-DTL-LINE2                         
                                                                                
           MOVE RBC-EFTD-CUSTOMER-NUM   TO DD-DD-REF-ID                         
           MOVE RBC-EFTD-PAYMENT-AMT    TO DD-DD-AM                             
           MOVE RBC-EFTD-PAYMENT-DATE   TO WS-PAYMENT-DATE                      
           PERFORM 2710-CONVERT-JULIAN-TO-GREG THRU 2710-EXIT                   
           MOVE VDATE1-YYYY             TO WS-DATE-YYYY                         
           MOVE VDATE1-MM               TO WS-DATE-MM                           
           MOVE VDATE1-DD               TO WS-DATE-DD                           
           MOVE '-'                     TO WS-FILLER1                           
                                           WS-FILLER2                           
           MOVE WS-DATE-YYYY-MM-DD      TO DD-DD-DUE-DT                         
           MOVE EFTD-INST-NUM           TO DD-PYE-DD-INST-NO                    
           MOVE EFTD-BRANCH-NUM         TO DD-PYE-DD-BR-NO                      
           MOVE RBC-EFTD-ACCOUNT-NUM    TO DD-PYE-DD-AC-NO                      
           MOVE RBC-EFTH-FILE-CREATION-DATE                                     
                                        TO WS-PAYMENT-DATE                      
           PERFORM 2710-CONVERT-JULIAN-TO-GREG THRU 2710-EXIT                   
           MOVE VDATE1-YYYY             TO WS-DATE-YYYY                         
           MOVE VDATE1-MM               TO WS-DATE-MM                           
           MOVE VDATE1-DD               TO WS-DATE-DD                           
           MOVE '-'                     TO WS-FILLER1                           
                                           WS-FILLER2                           
           MOVE WS-DATE-YYYY-MM-DD      TO DD-DD-BAT-CREA-DT                    
           MOVE RBC-EFTH-FILE-CREATION-NUM                                      
                                        TO DD-DD-BAT-NO                         
           IF WS-LINE-COUNT > 55                                                
              PERFORM 4000-PRINT-HEADINGS THRU                                  
                      4000-EXIT                                                 
           END-IF.                                                              
                                                                                
           PERFORM 5000-PRINT-DETAILS THRU                                      
                   5000-EXIT.                                                   
                                                                                
       2900-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       4000-PRINT-HEADINGS.                                                     
                                                                                
           MOVE SPACES                       TO PRINT-REC                       
                                                                                
           ADD  1                            TO WS-PAGE                         
           MOVE WS-PAGE                      TO WS-PAGE-NO                      
                                                                                
           MOVE WS-HDR-LINE                  TO PRINT-REC                       
           PERFORM 8400-WRITE-REPORT THRU                                       
                   8400-EXIT.                                                   
                                                                                
           MOVE WS-HDR-LINE1                 TO PRINT-REC                       
           PERFORM 8400-WRITE-REPORT THRU                                       
                   8400-EXIT.                                                   
                                                                                
           MOVE WS-HDR-LINE1B                TO PRINT-REC                       
           PERFORM 8400-WRITE-REPORT THRU                                       
                   8400-EXIT.                                                   
                                                                                
           MOVE WS-HDR-LINE2                 TO PRINT-REC                       
           PERFORM 8400-WRITE-REPORT THRU                                       
                   8400-EXIT.                                                   
                                                                                
           MOVE  WS-DTL-LINE0                TO PRINT-REC                       
           PERFORM 8400-WRITE-REPORT THRU                                       
                   8400-EXIT.                                                   
                                                                                
           MOVE  WS-DTL-LINE1                TO PRINT-REC                       
           PERFORM 8400-WRITE-REPORT THRU                                       
                   8400-EXIT.                                                   
                                                                                
           MOVE 8                            TO WS-LINE-COUNT.                  
                                                                                
       4000-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       5000-PRINT-DETAILS.                                                      
                                                                                
           MOVE '0'            TO WS-DTL-CTL                                    
           ADD 1               TO WS-LINE-COUNT                                 
           MOVE WS-DTL-LINE2   TO PRINT-REC                                     
                                                                                
           PERFORM 8400-WRITE-REPORT THRU                                       
                   8400-EXIT.                                                   
                                                                                
       5000-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       8000-READ-EFT-FILE.                                                      
                                                                                
           MOVE  CARD-DATA-010      TO  LOGICAL-RECORD-NAME.                    
           CALL  GAEDATSR        USING  WS-ACTION-VERB                          
                                        RBC-EFT-DETAIL                          
                                        ICBM.                                   
                                                                                
           PERFORM 8900-CHECK-READ THRU                                         
                   8900-EXIT.                                                   
                                                                                
       8000-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       8100-WRITE-REPORT.                                                       
                                                                                
           ADD 1 TO WS-LINE-COUNTC.                                             
                                                                                
           MOVE  STORE-LR           TO  WS-ACTION-VERB.                         
           MOVE  PRINT-DATA-022     TO  LOGICAL-RECORD-NAME.                    
           CALL  GAEDATSR        USING  WS-ACTION-VERB                          
                                        PRINT-REC                               
                                        ICBM.                                   
                                                                                
       8100-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       8400-WRITE-REPORT.                                                       
                                                                                
           ADD 1 TO WS-LINE-COUNT.                                              
                                                                                
           MOVE  STORE-LR           TO  WS-ACTION-VERB.                         
           MOVE  PRINT-DATA-023     TO  LOGICAL-RECORD-NAME.                    
           CALL  GAEDATSR        USING  WS-ACTION-VERB                          
                                        PRINT-REC                               
                                        ICBM.                                   
                                                                                
       8400-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       8900-CHECK-READ.                                                         
                                                                                
           IF  LR-STATUS-OK                                                     
               CONTINUE                                                         
           ELSE                                                                 
               MOVE 'Y'  TO  WS-EOF-REQUEST                                     
           END-IF.                                                              
                                                                                
       8900-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
       9000-FINISH.                                                             
                                                                                
      *                                                                         
      * CLOSE FILES                                                             
      *                                                                         
           MOVE  FINISH-LR          TO  WS-ACTION-VERB.                         
           MOVE  SPACES             TO  LOGICAL-RECORD-NAME.                    
           CALL  GAEDATSR        USING  WS-ACTION-VERB                          
                                        PRINT-REC                               
                                        ICBM.                                   
                                                                                
           DISPLAY '***********************************************'            
           DISPLAY '***        END     BRSI0400                 ***'            
           DISPLAY '***********************************************'.           
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
      * WRITE OUT AN ERROR MESSAGE AND DON'T COME BACK                          
      *-----------------------------------------------------------------        
       9999-FATAL-ERROR-RTN.                                                    
                                                                                
           MOVE WS-PROGRAM-ID       TO ID-PROGRAM-NAME                          
           MOVE WS-ACTION-VERB      TO ID-ACTION                                
           MOVE LOGICAL-RECORD-NAME TO ID-LR-NAME                               
           MOVE LINKAGE-STATUS      TO ID-LR-STATUS                             
           MOVE ZERO                TO ID-SQL-CODE                              
           MOVE ZEROS               TO ID-GROUP                                 
           MOVE SPACES              TO ID-CERTIFICATE                           
           MOVE WS-ERROR-MSG        TO ID-ERROR-MESSAGE                         
           SET  ID-FATAL-ERROR      TO TRUE                                     
           CALL INCGDUMP   USING ICBM                                           
                                 INRGCTRL-RECORD                                
                                 INRGDUMP-RECORD.                               
                                                                                
       9999-EXIT.                                                               
           EXIT.                                                                
      *-----------------------------------------------------------------        
      *                     End of Source                              -        
      *-----------------------------------------------------------------        
