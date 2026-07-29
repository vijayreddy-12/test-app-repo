       01  GMASTER7.                                                     0001000
      *****************************************************************         
      *     GIPSY MASTER VSAM FILE                                    *         
      *                                                               *         
      *       CREATED BY MANY                                         *         
      *       USED    BY MANY                                         *         
      *                                                               *         
      *****************************************************************         
      *    DEC/89 M. PRANGE                                           *         
      *           - ADDED FIELDS GGIADMIN-AREA & GGILIVES-RANGE       *         
      *             RENAMED GGIFOGL-SOURCE   TO GGIFOGL-ADMIN-AREA    *         
      *                     GGIFOGL-CONSTANT TO GGIFOGL-LIVES-RANGE   *         
      *    FEB/91 M. PRANGE                                           *         
      *           - ADDED GBN-NAAD-PROC-DATE                          *         
      *    SEP/91 M. PRANGE                                           *         
      *           - ADDED GGIMAIL-INSTRUCTION                         *         
      *    JUN/93 M. PRANGE                                           *         
      *           - CREATED GTX (PROV TAXES) TRLR BY STEALING         *         
      *             UNUSED GAC (ACTUARY) TRLR. IT IS TRLR 150.        *         
      *                                                               *         
      *    OCT/10 C. MUNCAN                                           *         
      *         - IDMS TO DB2 PROJECT 7-DIGIT GROUP NUMBER EXPANSION  *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 000                               *         
      *****************************************************************         
           03  GGIGENERAL-INFORMATION.                                  00020000
               05  GGIRECORD-LENGTH    COMP SYNC   PIC S9(4).           00030000
               05  FILLER                          PIC XX.              00040000
               05  GGIGROUP-NUMBER-X.                                           
                   10  GGIGROUP-NUMBER  COMP-3     PIC S9(7).           00050000
               05  GGIACCOUNT                      PIC XXX.             00060000
               05  GGIMASTER-IDENT                 PIC X.               00070000
               05  GGIIDENTITY                     PIC X(5).            00080000
               05  GGIBRANCH                       PIC X(5).            00090000
               05  GGIGROUP-REP                    PIC 9(5).            00100100
               05  GGIGROUP-REP2                   PIC 9(5).            00101000
               05  GGIGROUP-OFFICE                 PIC X(5).            00110000
               05  GGISHARING-OFFICE               PIC X(5).            00111000
               05  GGICLAIMS-OFFICE                PIC X(5).            00120000
               05  GGICOUNTRY                      PIC X.               00130000
               05  GGICURRENCY                     PIC X.               00140000
               05  GGIRESIDENCE                    PIC XX.              00150000
               05  GGILANGUAGE                     PIC X.               00160000
               05  GGIPRODUCT-CODE                 PIC X.               00161000
               05  GGIPOLICY-ACCT                  PIC X.               00170000
               05  GGIACCT-TYPE.                                        00180000
                   07 GGITYPE                      PIC X.               00190000
                   07 GGIUW-TYPE                   PIC X.               00200000
               05  GGICOVERAGE-CODE                PIC X.               00210000
               05  GGIBENEFIT-SEL                  PIC X.               00220000
               05  GGISTD-IND-CLASS-CODE   COMP-3  PIC S9(5).           00230000
               05  GGICONTRIBUTION                 PIC X.               00240000
               05  GGIBENEFIT-LINK                 PIC X.               00250000
               05  GGITAX                          PIC X.               00260000
               05  GGIWITHHOLDING                  PIC X.               00270000
               05  GGIEMPLOYEE-LIST                PIC X.               00280000
               05  GGIHANDLING-CODE                PIC X.               00290000
               05  GGIARREARS-IND                  PIC X.               00300000
               05  GGIARREARS-COUNTER  COMP-3      PIC S999.            00310000
               05  GGIARREARS-STATUS               PIC X.               00320000
               05  GGIREDUCTION-AGES   COMP-3.                          00330000
                   07  GGIRED-AGE-LIFE             PIC S999.            00340000
                   07  GGIRED-AGE-LTD              PIC S999.            00350000
                   07  GGIRED-AGE-AD-D             PIC S999.            00360000
                   07  GGIRED-AGE-MEDICAL          PIC S999.            00370000
                   07  GGIRED-AGE-DENTAL           PIC S999.            00380000
                   07  GGIRED-AGE-WEEK-INDEM       PIC S999.            00390000
               05  GGITERMINATION-AGES     COMP-3.                      00400000
                   07  GGITERM-AGE-LIFE            PIC S999.            00410000
                   07  GGITERM-AGE-LTD             PIC S999.            00420000
                   07  GGITERM-AGE-AD-D            PIC S999.            00430000
                   07  GGITERM-WAIT-PERIOD-LTD     PIC S999.            00440000
                   07  GGITERM-AGE-MEDICAL         PIC S999.            00450000
                   07  GGITERM-AGE-DENTAL          PIC S999.            00460000
                   07  GGITERM-AGE-WEEK-INDEM      PIC S999.            00470000
               05  GGISTEP-DOWN                    PIC X.               00480000
               05  GGICHANGE-DATE                  PIC X.               00490000
               05  GGIACCOUNT-STATUS               PIC XX.              00500000
               05  GGIDATES            COMP-3.                          00510000
                   07  GGINEXT-ANNIVERSARY.                             00520000
                       09  GGINEXT-ANNIV-YEAR      PIC S9(3).           00530000
                       09  GGINEXT-ANNIV-DAY       PIC S9(3).           00540000
                   07  GGICLAIMS-SUSPENDED.                             00550000
                       09  GGICLAIM-SUSP-YEAR      PIC S9(3).           00560000
                       09  GGICLAIM-SUSP-DAY       PIC S9(3).           00570000
                   07  GGICONTRACT-ISSUE.                               00580000
                       09  GGIISSUE-YEAR           PIC S9(3).           00590000
                       09  GGIISSUE-DAY            PIC S9(3).           00600000
                   07  GGI-ACCT-EFFECTIVE.                              00610000
                       09  GGI-ACCT-EFF-YEAR       PIC S9(3).           00620000
                       09  GGI-ACCT-EFF-DAY        PIC S9(3).           00630000
                   07  GGIPAID-TO-DATE.                                 00640000
                       09  GGIPAID-TO-YEAR         PIC S9(3).           00650000
                       09  GGIPAID-TO-DAY          PIC S9(3).           00660000
                   07  GGILAST-CHANGE-DATE.                             00670000
                       09  GGILAST-CHANGE-YEAR     PIC S9(3).           00680000
                       09  GGILAST-CHANGE-DAY      PIC S9(3).           00690000
                   07  GGILAST-ACCOUNTING-DATE.                         00700000
                       09  GGILAST-ACCTG-YEAR      PIC S9(3).           00710000
                       09  GGILAST-ACCTG-DAY       PIC S9(3).           00720000
               05  GGIPOL-ARREARS          COMP-3  PIC S9(2).           00730000
               05  GGICASHFLOW-LST-POL-YR  COMP-3  PIC S9(3).           00740000
               05  GGICASHFLOW-CURR-POL-YR COMP-3  PIC S9(3).           00750000
               05  GGIBILL-STAT-FORMAT.                                 00760000
                   07  GGIBILLING-AGENT            PIC 9(6).            00761000
                   07  GGIBILLING-TYPE             PIC XX.              00770000
                   07  GGILISTING-CERTIFICATE      PIC X.               00780000
                   07  GGILISTING-ORDER            PIC X.               00790000
                   07  GGISTAT-OPTION.                                  00800000
                       09  GGILIST-BILLING         PIC X.               00810000
                       09  GGIEMP-CHANGE-LIST      PIC X.               00820000
                       09  GGIWORK-SHEET           PIC X.               00830000
                       09  GGISUMMARY              PIC X.               00840000
               05  GGIBENEFIT-COMB.                                     00850000
      *****************************************************************         
      * GIPSY RELEASE #1, SEPTEMBER 1989                                        
      *   TITLES AREA BELOW CHANGED TO INCREASE NUMBER OF COLUMNS FROM          
      *   7 TO 9.  'GGITITLES-AREA' IS *COMPRESSED* DATA CONSISTING OF          
      *   6-BIT CHARACTER CODES.  TRANSLATION TO/FROM AN EXPANSION              
      *   WORK AREA (COPYBOOK 'GGI6BITX') IS ACCOMPLISHED USING RTN             
      *   'GUTBITS6'.  (I. BUCKNELL)                                            
      *****************************************************************         
                   07  GGINUM-COV-CODES-LIST.                                   
                       09  GGINUM-COV-CODES OCCURS 9 TIMES                      
                                   COMP-3          PIC S9(3).                   
                   07  GGITITLES-AREA              PIC X(129).                  
                   07  GGICOV-CODES-LIST.                                       
                       09  GGICOV-CODES OCCURS 36 TIMES                         
                                   COMP-3          PIC S9(3).                   
               05  GGIBASIC-ADMIN-FEE OCCURS 4 TIMES.                   00950000
                   07  GGIBASIC-ADMIN-AMT          PIC S9(7)V99 COMP-3. 00960000
                   07  GGIBASIC-ADMIN-IND          PIC X.               00970000
                   07  GGIBASIC-ADMIN-REASON       PIC XX.              00980000
               05  GGIOTHER-ADMIN-FEE.                                  00990000
                   07  GGIOTHER-ADMIN-AMT          PIC S9(7)V99 COMP-3. 01000000
                   07  GGIOTHER-ADMIN-IND          PIC X.               01010000
                   07  GGIOTHER-ADMIN-REASON       PIC X(25).           01020000
               05  GGIPRACTICE-BILLING.                                 01030000
                   07  GGIPRAC-BILL-REQ-TYPE       PIC X.               01040000
                   07  GGIPRAC-BILL-FROM-DATE      COMP-3.              01050000
                       09  GGIPRAC-BILL-FROM-YR    PIC S9(3).           01060000
                       09  GGIPRAC-BILL-FROM-DAY   PIC S9(3).           01070000
                   07  GGIPRAC-BILL-TO-DATE        COMP-3.              01080000
                       09  GGIPRAC-BILL-TO-YR      PIC S9(3).           01090000
                       09  GGIPRAC-BILL-TO-DAY     PIC S9(3).           01100000
                   07  GGIPRAC-BILL-REQUESTOR      PIC X(15).           01110000
               05  GGICLAIMS-INFORMATION.                               01120000
                   10  GGICARRYOVER-IND.                                01130000
                       15  GGICARRYOVER-HC         PIC X.               01140000
                       15  GGICARRYOVER-DE         PIC X.               01150000
                   10  GGIELIGIBLE-BENEFITS        PIC X.               01160000
                   10  GGIPRIVATE-DUTY-NURSING     PIC 99.              01170000
                   10  GGIDENTAL-PLAN-IND          PIC X.               01180000
                   10  GGILAB-FEES                 PIC XX.              01190000
               05  GGIFOGL-CODE OCCURS 6 TIMES.                         01201000
                   10 GGIFOGL-BENEFIT              PIC X(4).            01202000
                   10 GGIFOGL-REINS                PIC X(2).            01203000
                   10 GGIFOGL-GRP-TYPE             PIC X.               01204000
                   10 GGIFOGL-AGT-TYPE             PIC X.               01205000
                   10 GGIFOGL-ADMIN-AREA           PIC X.               01206000
                   10 GGIFOGL-LIVES-RANGE          PIC X.               01207000
               05  GGIFUND-TYPE OCCURS 6 TIMES     PIC X.               01211000
               05  GGIBILLING-OVERIDE.                                  01211100
                   10 GGIBILLING-OVERIDE-IND       PIC X.               01211200
                   10 GGIBILLING-OR-TELE-NUM       PIC X(11).           01211300
               05  GGIPH-EOB                       PIC X.               01212001
               05  GGIADMIN-AREA                   PIC X(1).            01213001
               05  GGILIVES-RANGE                  PIC X(1).            01213001
               05  GGIMAIL-INSTRUCTION             PIC X(1).            01213001
               05  GGIUNDEFINED                    PIC X(14).           01213001
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 005                               *         
      *****************************************************************         
      *                                                               *         
           03  GCOGCOVERAGE-RECORD-PTR.                                 01220000
               05  GCOCOV-REC-PTR OCCURS 10 TIMES.                      01230000
                   07  GCOTRIDENT                  PIC XXX.             01240000
                   07  GCOLENGTH    COMP-3         PIC S9(5).           01250000
                   07  GCOCOV-RECORD-TYPE          PIC XXX.             01260000
                   07  GCOHISTORY-IND              PIC X.               01270000
                   07  GCOUNDEFINED                PIC X(20).           01280000
                   07  GCOCOV-SEGMENT OCCURS 10 TIMES.                  01290000
                       09  GCOEFFECTIVE-DATE  COMP-3.                   01300000
                           11  GCOEFFECTIVE-YEAR   PIC S9(3).           01310000
                           11  GCOEFFECTIVE-DAY    PIC S9(3).           01320000
                       09  GCOEND-DATE   COMP-3.                        01330000
                           11  GCOEND-YEAR         PIC S9(3).           01340000
                           11  GCOEND-DAY          PIC S9(3).           01350000
                       09  GCOERROR-IND            PIC XXX.             01360000
                       09  GCOCONSOLIDATION-IND    PIC X.               01370000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 010                               *         
      *****************************************************************         
      *                                                               *         
             03  GSTSTATISTICS.                                         01380000
               05  GSTTRIDENT                      PIC XXX.             01390000
               05  GSTLENGTH           COMP-3      PIC S9(5).           01400000
               05  GSTPAID-IND                     PIC X.               01410000
               05  GSTOUTSTANDING      COMP-3.                          01420000
                   07  GSTOUT-LIFE                 PIC S9(7)V99.        01430000
                   07  GSTOUT-LTD                  PIC S9(7)V99.        01440000
                   07  GSTOUT-A-S                  PIC S9(7)V99.        01450000
                   07  GSTOUT-ADMIN-FEE            PIC S9(7)V99.        01460000
               05  GSTPREMIUM-IND                  PIC X.               01470000
               05  GSTADVANCE          COMP-3      PIC S9(7)V99.        01480000
               05  GSTCALENDAR-YEAR-TO-DATE                             01490000
                                       COMP-3.                          01500000
                   07  GSTCAL-YTD-LIFE.                                 01510000
                       09  GSTCAL-YTD-LI-BILLED    PIC S9(7)V99.        01520000
                       09  GSTCAL-YTD-LI-PAID      PIC S9(7)V99.        01530000
                   07  GSTCAL-YTD-A-S.                                  01540000
                       09  GSTCAL-YTD-AS-BILLED    PIC S9(7)V99.        01550000
                       09  GSTCAL-YTD-AS-PAID      PIC S9(7)V99.        01560000
                   07  GSTCAL-YTD-LTD.                                  01570000
                       09  GSTCAL-YTD-LTD-BILLED   PIC S9(7)V99.        01580000
                       09  GSTCAL-YTD-LTD-PAID     PIC S9(7)V99.        01590000
                   07  GSTCAL-YTD-ADMIN-FEE.                            01600000
                       09  GSTCAL-YTD-ADMIN-FEE-BILLED  PIC S9(7)V99.   01610000
                       09  GSTCAL-YTD-ADMIN-FEE-PAID    PIC S9(7)V99.   01620000
               05  GSTPREMIUMS         COMP-3.                          01630000
                   07  GSTPREM-LIFE                PIC S9(7)V99.        01640000
                   07  GSTPREM-LTD                 PIC S9(7)V99.        01650000
                   07  GSTPREM-AD-D                PIC S9(7)V99.        01660000
                   07  GSTPREM-WI                  PIC S9(7)V99.        01670000
                   07  GSTPREM-EHC.                                     01680000
                       09  GSTPREM-EHC-SINGLE      PIC S9(7)V99.        01690000
                       09  GSTPREM-EHC-FAMILY1     PIC S9(7)V99.        01700000
                       09  GSTPREM-EHC-FAMILY2     PIC S9(7)V99.        01710000
                   07  GSTPREM-DEN.                                     01720000
                       09  GSTPREM-DEN-SINGLE      PIC S9(7)V99.        01730000
                       09  GSTPREM-DEN-FAMILY1     PIC S9(7)V99.        01740000
                       09  GSTPREM-DEN-FAMILY2     PIC S9(7)V99.        01750000
               05  GSTCURR-MONTH-PREM-ADJ                               01760000
                                       COMP-3.                          01770000
                   07  GSTPREM-ADJ-LIFE            PIC S9(5)V99.        01780000
                   07  GSTPREM-ADJ-LTD             PIC S9(5)V99.        01790000
                   07  GSTPREM-ADJ-AD-D            PIC S9(5)V99.        01800000
                   07  GSTPREM-ADJ-WI              PIC S9(5)V99.        01810000
                   07  GSTPREM-ADJ-EHC.                                 01820000
                       09  GSTPREM-ADJ-EHC-SINGLE  PIC S9(5)V99.        01830000
                       09  GSTPREM-ADJ-EHC-FAMILY1 PIC S9(5)V99.        01840000
                       09  GSTPREM-ADJ-EHC-FAMILY2 PIC S9(5)V99.        01850000
                   07  GSTPREM-ADJ-DEN.                                 01860000
                       09  GSTPREM-ADJ-DEN-SINGLE  PIC S9(5)V99.        01870000
                       09  GSTPREM-ADJ-DEN-FAMILY1 PIC S9(5)V99.        01880000
                       09  GSTPREM-ADJ-DEN-FAMILY2 PIC S9(5)V99.        01890000
               05  GSTPROJECTED-LOSS-RATIO COMP-3.                      01900000
                   07  GSTPROJ-LR-PREV-POLYR       PIC S9(4)V9.         01910000
                   07  GSTPROJ-LR-CURR-POLYR       PIC S9(4)V9.         01920000
               05  GSTACCEPT-LOSS-PREV     COMP-3  PIC S9(4)V9.         01930000
               05  GSTPART                 COMP-3  PIC S9(7).           01940000
               05  GSTNON-PART             COMP-3  PIC S9(5).           01950000
               05  GSTUNDEFINED                    PIC X(33).           01960000
               05  GSTNUM-COV-CODES        COMP-3  PIC S9(3).           01970000
               05  GSTSTATS-BY-COV OCCURS 15 TIMES COMP-3.              01980000
                   07  GSTSTAT-COV-CODE            PIC S9(3).           01990000
                   07  GSTSTAT-VOL                 PIC S9(9).           02000000
                   07  GSTSTAT-LIVES               PIC S9(7).           02010000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 030                               *         
      *****************************************************************         
      *                                                               *         
             03  GBNBILLING-NAME-ADDRESS.                               02020000
               05  GBNTRIDENT                      PIC XXX.             02030000
               05  GBNLENGTH           COMP-3      PIC S9(5).           02040000
               05  GBNPOST-CODE                    PIC X(6).            02050000
               05  GBN-NAAD-PROC-DATE.                                  02060000
                   10  GBN-NAAD-PROC-YEAR          PIC S9(3)   COMP-3.          
                   10  GBN-NAAD-PROC-DAY           PIC S9(3)   COMP-3.          
               05  GBNUNDEFINED                    PIC X(16).           02060000
               05  GBNBILL-NAME-ADDR.                                           
                   10  GBNBILL-NAME-ADDR-LINES     OCCURS 6 TIMES.              
                       15  GBNBILL-NAME-ADDR-LINE  PIC X(30).                   
                       15  GBNBILL-NAME-ADDR-DELIM PIC X.                       
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 040                               *         
      *****************************************************************         
      *                                                               *         
           03  GCNCLAIMS-NAME-ADDRESS.                                  02080000
               05  GCNTRIDENT                      PIC XXX.             02090000
               05  GCNLENGTH           COMP-3      PIC S9(5).           02100000
               05  GCNPOST-CODE                    PIC X(6).            02110000
               05  GCNUNDEFINED                    PIC X(20).           02120000
               05  GCNCL-NAME-ADDR.                                             
                   10  GCNCL-NAME-ADDR-LINES       OCCURS 6 TIMES.              
                       15  GCNCL-NAME-ADDR-LINE    PIC X(30).                   
                       15  GCNCL-NAME-ADDR-DELIM   PIC X.                       
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 045                               *         
      *****************************************************************         
      *                                                               *         
           03  GONOTHER-NAME-ADDRESS.                                   02140000
               05  GONTRIDENT                      PIC XXX.             02150000
               05  GONLENGTH              COMP-3   PIC S9(5).           02160000
               05  GONPOST-CODE                    PIC X(6).            02170000
               05  GONIDENT                        PIC XX.              02180000
               05  GONUNDEFINED                    PIC X(18).           02190000
               05  GONOTHER-NAME-ADDR.                                          
                   10  GONOTHER-NAME-ADDR-LINES    OCCURS 6 TIMES.              
                       15  GONOTHER-NAME-ADDR-LINE  PIC X(30).                  
                       15  GONOTHER-NAME-ADDR-DELIM PIC X.                      
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 050                               *         
      *****************************************************************         
      *                                                               *         
           03  GRIREINSURANCE.                                          02210000
            04 GRIREIN          OCCURS 2 TIMES.                         02220000
               05  GRITRIDENT                      PIC XXX.             02230000
               05  GRILENGTH           COMP-3      PIC S9(5).           02240000
               05  GRIEFFECTIVE-DATE   COMP-3.                          02250000
                   07  GRIEFFECTIVE-YEAR           PIC S9(3).           02260000
                   07  GRIEFFECTIVE-DAY            PIC S9(3).           02270000
               05  GRITYPE                         PIC X.               02280000
               05  GRICOMPANY                      PIC X.               02290000
               05  GRIBENEFIT                      PIC X.               02300000
               05  GRISHARE-QUOTA-PER  COMP-3      PIC S9(3)V99.        02310000
               05  GRIRETENTION-MAX    COMP-3      PIC S9(7).           02320000
               05  GRICO-INSURANCE-PER COMP-3      PIC S9(3)V99.        02330000
               05  GRIREINSURANCE-RATE COMP-3.                          02340000
                   07  GRIRATE-SINGLE              PIC S9(3)V99.        02350000
                   07  GRIRATE-FAMILY              PIC S9(3)V99.        02360000
               05  GRISELF-BILL-PER    COMP-3      PIC S9(3)V99.        02370000
               05  GRIMAJ-MED-MAX      COMP-3      PIC S9(9).           02380000
               05  GRIEXEMPT-CLASSES               PIC X(10).           02390000
               05  GRIUNDEFINED                    PIC X(20).           02400000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 080                               *         
      *****************************************************************         
      *                                                               *         
           03  GBIBILLING.                                              02410000
               05  GBITRIDENT                      PIC XXX.             02420000
               05  GBILENGTH           COMP-3      PIC S9(5).           02430000
               05  GBIMOST-OUTSTANDING COMP-3.                          02440000
                   07  GBIDUE-DATE.                                     02450000
                       09  GBIDUE-YEAR             PIC S9(3).           02460000
                       09  GBIDUE-DAY              PIC S9(3).           02470000
                   07  GBIBILL.                                         02480000
                       09  GBILIFE                 PIC S9(7)V99.        02490000
                       09  GBILTD                  PIC S9(7)V99.        02500000
                       09  GBIA-S                  PIC S9(7)V99.        02510000
                       09  GBIADMIN-FEE            PIC S9(7)V99.        02520000
                   07  GBIPOSITION                 PIC S9(3).           02530000
               05  GBINO-HIST-SEG          COMP-3  PIC S9(3).           02540000
               05  GBIUNDEFINED1                   PIC X(23).           02550000
               05  GBIHISTORY-INFO OCCURS 25 TIMES.                     02560000
                   07  GBIHIST-DUE-DATE COMP-3.                         02570000
                       09  GBIHIST-DUE-YEAR        PIC S9(3).           02580000
                       09  GBIHIST-DUE-DAY         PIC S9(3).           02590000
                   07  GBIHIST-PREPARED-DATE COMP-3.                    02600000
                       09  GBIHIST-PREPARED-YEAR   PIC S9(3).           02610000
                       09  GBIHIST-PREPARED-DAY    PIC S9(3).           02620000
                   07  GBI-PREM-DUE        COMP-3  PIC S9(7)V99.        02630000
                   07  GBICERT-HOLDERS     COMP-3  PIC S9(7).           02640000
                   07  GBIHIST-ADMIN-FEE   COMP-3  PIC S9(7)V99.        02650000
                   07  GBINO-COV-LINES     COMP-3  PIC S9(3).           02660000
                   07  GBIUNDEFINED2               PIC X.               02670000
                   07  GBISTATS-BY-COVERAGE OCCURS 10 TIMES COMP-3.     02680000
                       09  GBICOV-CODE             PIC S9(3).           02690000
                       09  GBIAMT-BILLED           PIC S9(7)V99.        02700000
                       09  GBINO-LIVES             PIC S9(5).           02710000
                       09  GBIVOL                  PIC S9(9).           02720000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 090                               *         
      *****************************************************************         
      *                                                               *         
           03  GPAPAYMENT.                                              02730000
               05  GPATRIDENT                      PIC XXX.             02740000
               05  GPALENGTH           COMP-3      PIC S9(5).           02750000
               05  GPAUNDEFINED                    PIC X(20).           02760000
               05  GPAPAYMENT-INFO OCCURS 60 TIMES.                     02770000
                   07  GPAPAYMENT-DATE COMP-3.                          02780000
                       09  GPAPAY-YEAR             PIC S9(3).           02790000
                       09  GPAPAY-DAY              PIC S9(3).           02800000
                   07  GPAPAY-AMOUNT   COMP-3      PIC S9(7)V99.        02810000
                   07  GPAPAY-TYPE                 PIC X.               02820000
                   07  GPAPAY-APPLIED-DATE COMP-3.                      02830000
                       09  GPAPAY-APP-YEAR         PIC S9(3).           02840000
                       09  GPAPAY-APP-DAY          PIC S9(3).           02850000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 100                               *         
      *****************************************************************         
      *                                                               *         
           03  GPSPREMIUM-SPLIT.                                        02860000
               05  GPSTRIDENT                      PIC XXX.             02870000
               05  GPSLENGTH           COMP-3      PIC S9(5).           02880000
               05  GPSUNDEFINED1                   PIC X(20).           02890000
               05  GPSPREM-SPLIT-INFO.                                  02900000
                 06  GPSPREM-INFO OCCURS 10 TIMES.                      02910000
                   07  GPSCODE                     PIC X.               02920000
                   07  GPSTART-DATE    COMP-3.                          02930000
                       09  GPSSTART-YEAR           PIC S9(3).           02940000
                       09  GPSSTART-DAY            PIC S9(3).           02950000
                   07  GPSSTOP-DATE    COMP-3.                          02960000
                       09  GPSSTOP-YEAR            PIC S9(3).           02970000
                       09  GPSSTOP-DAY             PIC S9(3).           02980000
                   07  GPSFIRST-YEAR   COMP-3.                          02990000
                       09  GPSFRST-LIFE            PIC S9(5)V99.        03000000
                       09  GPSFRST-LTD             PIC S9(5)V99.        03010000
                       09  GPSFRST-A-S             PIC S9(5)V99.        03020000
                   07  GPSUNDEFINED2               PIC X(20).           03030000
               05  GPSFLAT-AMOUNT REDEFINES GPSPREM-SPLIT-INFO.         03040000
               06  GPSFLAT-AMNT   OCCURS 10 TIMES.                      03050000
                   07  GPSUNDEFINED3               PIC X(9).            03060000
                   07  GPSFLAT-AMT.                                     03070000
                       09  GPSORIGINAL COMP-3      PIC S9(5)V99.        03080000
                       09  GPSREMAINING COMP-3     PIC S9(5)V99.        03090000
                   07  GPSUNDEFINED4               PIC X(24).           03100000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 110                               *         
      *****************************************************************         
      *                                                               *         
           03  GAGAGENTS-INFORMATION.                                   03110000
               05  GAGAGENTS-INFO OCCURS 4 TIMES.                       03120000
                   07  GAGTRIDENT                  PIC XXX.             03130000
                   07  GAGLENGTH       COMP-3      PIC S9(5).           03140000
                   07  GAGAGENT-CODE               PIC 9(6).            03150000
                   07  GAGACCOUNT                  PIC X.               03160000
                   07  GAGAGENT-BRANCH             PIC X(5).            03170000
                   07  GAGRENEW-COMM-PD   COMP-3   PIC S9(7)V99.        03171000
                   07  GAGRENEW-COMM-EARN COMP-3   PIC S9(7)V99.        03172000
                   07  GAGSALES-CRED-PD   COMP-3   PIC S9(7)V99.        03173000
                   07  GAGSALES-CRED-EARN COMP-3   PIC S9(7)V99.        03174000
                   07  GAGCASE-CNT-PD     COMP-3   PIC S9(7)V99.        03175000
                   07  GAGCASE-CNT-EARN   COMP-3   PIC S9(7)V99.        03176000
                   07  GAGNAP-PAID1       COMP-3   PIC S9(7)V99.        03177000
                   07  GAGNAP-EARNED1     COMP-3   PIC S9(7)V99.        03178000
                   07  GAGNAP-PAID2       COMP-3   PIC S9(7)V99.        03179000
                   07  GAGNAP-EARNED2     COMP-3   PIC S9(7)V99.        03179100
                   07  GAGPAYMENT-PLAN.                                 03180000
                     09  GAGPAYMENT-TYPE           PIC X.               03190000
                     09  GAGCANN-COUNTER           PIC X.               03200000
                     09  GAGPERCENT-DEFER          PIC S9(5)V99  COMP-3.03210000
                     09  GAGANNUALIZED-PAID        PIC S9(7)V99  COMP-3.03220000
                     09  GAGANNUALIZED-EARN        PIC S9(7)V99  COMP-3.03230000
                   07  GAGUNDEFINED1               PIC X(60).           03240000
                   07  GAGAGENTS-INF   OCCURS 5 TIMES.                  03250000
                     09  GAGCOMM-IND               PIC X.               03260000
                     09  GAGSTART-DATE COMP-3.                          03270000
                         11 GAGSTART-YEAR          PIC S9(3).           03280000
                         11 GAGSTART-DAY           PIC S9(3).           03290000
                     09  GAGSTOP-DATE  COMP-3.                          03300000
                         11 GAGSTOP-YEAR           PIC S9(3).           03310000
                         11 GAGSTOP-DAY            PIC S9(3).           03320000
                     09  GAGFIRST-YEAR COMP-3.                          03330000
                         11  GAGFRST-LIFE          PIC S9(3)V99.        03340000
                         11  GAGFRST-LTD           PIC S9(3)V99.        03350000
                         11  GAGFRST-A-S           PIC S9(3)V99.        03360000
                         11  GAGFRST-ADMIN-FEE     PIC S9(3)V99.        03370000
                     09  GAGUNDEFINED2             PIC X(12).           03380000
                     09  GAGRENEWAL    COMP-3.                          03390000
                         11  GAGREN-LIFE           PIC S9(3)V99.        03400000
                         11  GAGREN-LTD            PIC S9(3)V99.        03410000
                         11  GAGREN-A-S            PIC S9(3)V99.        03420000
                         11  GAGREN-ADMIN-FEE      PIC S9(3)V99.        03430000
                     09  GAGUNDEFINED3             PIC X(12).           03440000
                     09  GAGSCALE                  PIC X(3).            03451000
                     09  GAGLTD-SCALE              PIC X(3).            03461000
                     09  GAGFLAT-RATE      COMP-3  PIC S999V99.         03470000
                     09  GAGLTD-FLAT-RATE  COMP-3  PIC S999V99.         03480000
                     09  GAGPOINT                  PIC X.               03490000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 120                               *         
      *****************************************************************         
      *                                                               *         
           03  GSNSPECIAL-NOTES.                                        03500000
               05  GSNSPEC-NOTES OCCURS 10 TIMES.                       03510000
                   07  GSNTRIDENT                  PIC XXX.             03520000
                   07  GSNLENGTH       COMP-3      PIC S9(5).           03530000
                   07  GSNCODE                     PIC XX.              03540000
                   07  GSNSPEC-NOTES-INFO.                              03550000
                       09  GSNSTART-DATE   COMP-3.                      03560000
                         11  GSNSTART-YEAR         PIC S9(3).           03570000
                         11  GSNSTART-DAY          PIC S9(3).           03580000
                       09  GSNSTOP-DATE    COMP-3.                      03590000
                         11  GSNSTOP-YEAR          PIC S9(3).           03600000
                         11  GSNSTOP-DAY           PIC S9(3).           03610000
                       09  GSNINFO                 PIC X(25).           03620000
                   07  GSNUNDEFINED                PIC X(20).           03630000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 125                               *         
      *****************************************************************         
      *                                                               *         
           03  GAMAMENDMENT.                                            03640000
               05  GAMTRIDENT                      PIC XXX.             03650000
               05  GAMLENGTH               COMP-3  PIC S9(5).           03660000
               05  GAMUNDEFINED1                   PIC X(10).           03670000
               05  GAMAMENDMENT-HISTORY OCCURS 15 TIMES.                03680000
                   07  GAMEFFECTIVE-DATE   COMP-3.                      03690000
                       09  GAMEFFECTIVE-YEAR       PIC S9(3).           03700000
                       09  GAMEFFECTIVE-DAY        PIC S9(3).           03710000
                   07  GAMPROCESSED-DATE   COMP-3.                      03720000
                       09  GAMPROCESSED-YEAR       PIC S9(3).           03730000
                       09  GAMPROCESSED-DAY        PIC S9(3).           03740000
                   07  GAMREASON                   PIC X(20).           03750000
                   07  GAMUNDEFINED2               PIC X(4).            03760000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 130                               *         
      *****************************************************************         
      *                                                               *         
           03  GTMTERMINATION.                                          03770000
               05  GTMTRIDENT                      PIC XXX.             03780000
               05  GTMLENGTH       COMP-3          PIC S9(5).           03790000
               05  GTMUNDEFINED                    PIC X(10).           03800000
               05  GTMTERMINATION-HISTORY OCCURS 5 TIMES.               03810000
                   07  GTMEFFECTIVE-DATE   COMP-3.                      03820000
                       09  GTMEFFECTIVE-YEAR       PIC S9(3).           03830000
                       09  GTMEFFECTIVE-DAY        PIC S9(3).           03840000
                   07  GTMPROCESSED-DATE  COMP-3.                       03850000
                       09  GTMPROCESSED-YEAR       PIC S9(3).           03860000
                       09  GTMPROCESSED-DAY        PIC S9(3).           03870000
                   07  GTMPURGE-DATE       COMP-3.                      03880000
                       09  GTMPURGE-YEAR           PIC S9(3).           03890000
                       09  GTMPURGE-DAY            PIC S9(3).           03900000
                   07  GTMCODE                     PIC XX.              03910000
                   07  GTMREINSTATEMENT-INFO.                           03920000
                       09  GTMREIN-DATE    COMP-3.                      03930000
                           11 GTMREIN-YEAR         PIC S9(3).           03940000
                           11 GTMREIN-DAY          PIC S9(3).           03950000
                       09  GTMREIN-CODE            PIC X(2).            03960000
           03  GICIDENTIFICATION-CERTIFICATE.                           03970000
               05  GICTRIDENT                      PIC XXX.             03980000
               05  GICLENGTH               COMP-3  PIC S9(5).           03990000
               05  GICUNDEFINED1                   PIC X(20).           04000000
               05  GICCOVERAGE-INFO OCCURS 10 TIMES.                    04010000
                   10  GICCLASSES.                                      04020000
                       15  GICCLASS OCCURS 5 TIMES PIC XX.              04030000
                   10  GICCOVERAGE-CODES.                               04040000
                       15  GICCOVERAGE-CODE        PIC XX               04050000
                                    OCCURS 10 TIMES.                    04060000
                   10  GICUNDEFINED2               PIC X(9).            04070000
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 150                               *         
      *                                                                         
      *  NOTE: THIS TRAILER IS BASICALLY AN EXTENSION OF THE BILLING            
      *        TRAILER. USE THE BILLING TRLR FOR DUE-DATE AND POSITION.         
      *  NOTE: GTX-HIST-DUE-DATE SHOULD ALWAYS BE EXACTLY THE SAME              
      *        VALUE AS GBIHIST-DUE-DATE. IT IS STORED ON THE TAX               
      *        TRLR ONLY TO ASSIST IN RECOVERY SHOULD THE TAX AND               
      *        BILLING TRLRS GET OUT OF SYNC.                                   
      *****************************************************************         
      *                                                               *         
           03  GTX-TAXES.                                               04080000
               05  GTX-TRIDENT                   PIC X(3).                      
               05  GTX-LENGTH                    PIC S9(5)      COMP-3.         
               05  GTX-OUTSTANDING.                                             
                   10  GTX-OUTST-CNT             PIC S9(3)      COMP-3.         
                   10  GTX-OUTST-ENTRY OCCURS 13 TIMES.                         
                       15  GTX-OUTST-TAX-CODE    PIC X(2).                      
                       15  GTX-OUTST-TAX-AMT     PIC S9(7)V9(2) COMP-3.         
               05  GTX-FILLER1                   PIC X(51).                     
               05  GTX-HIST-CNT                  PIC S9(3)      COMP-3.         
               05  GTX-HISTORY.                                                 
                   10  GTX-HIST-ENTRY OCCURS 25 TIMES.                          
                       15  GTX-HIST-DUE-DATE.                                   
                           20  GTX-HIST-DUE-YEAR PIC S9(3)      COMP-3.         
                           20  GTX-HIST-DUE-DAY  PIC S9(3)      COMP-3.         
                       15  GTX-HIST-TAX-CNT      PIC S9(3)      COMP-3.         
                       15  GTX-FILLER3           PIC X(1).                      
                       15  GTX-HIST-ENTRY2 OCCURS 13 TIMES.                     
                           20  GTX-HIST-TAX-CODE PIC X(2).                      
                           20  GTX-HIST-TAX-AMT  PIC S9(7)V9(2) COMP-3.         
      *                                                               *         
      *****************************************************************         
      *         TRAILER NUMBER ===> 160                               *         
      *****************************************************************         
      *                                                               *         
           03  GF1FILLER1.                                              04270000
               05  GF1TRIDENT                      PIC XXX.             04280000
               05  GF1LENGTH              COMP-3   PIC S9(5).           04290000
               05  GF1DEFINED                      PIC X(497).          04310001
