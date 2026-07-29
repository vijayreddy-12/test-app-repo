       CBL FLAG(I)                                                      00010000
       IDENTIFICATION DIVISION.                                         00060000
       PROGRAM-ID.    GCCPFRMM.                                         00070000
                                                                        00080000
      ****************************************************************  00090000
      *  GROUP BENEFITS E-BUSINESS MASS CHANGE REPORT                   00100000
      *                                                                 00110000
      * PROGRAM DESCRIPTION:                                            00120000
      *     THIS PROGRAM READS A FLAT FILE CREATED BY GCCPFRMF THAT     00130000
      *  CONTAINS HEADER AND DETAIL RECORDS NECESSARY TO PRODUCE THE    00140000
      *  MASS CHANGE REPORT FOR USE BY CERT ADMIN.                      00150000
      *                                                                 00160000
      * CALLING MODULES                                                 00170000
      *     NOT APPLICABLE.                                             00180000
      *                                                                 00190000
      * CALLED MODULES                                                  00200000
      *     GAEDATSR - DATA SERVER                                      00210000
      *                                                                 00220000
      * COPYBOOKS                                                       00230000
      *     GARDSVRB - VERBS FOR GAEDATSR                               00240000
      *     CRPTMASS - MASS CHANGE FILE LAYOUT                          00250000
      *                                                                 00260000
      * INPUT  -  FLAT FILE OF MASS CHANGE HEADER AND DETAIL RECORDS    00270000
      * OUTPUT -  MASS CHANGE REPORT                                    00280000
      *                                                                 00290000
      ****************************************************************  00300000
      * DATE       NAME        DESCRIPTION                              00310000
      * ---------  ----------  ---------------------------------------  00320000
      * 30JAN2001  J.ELKINS    CREATION                                 00330000
      * 27JUN2001  J.ELKINS    ADD BANNER AND TRAILER PAGES FOR         00331000
      *                        EACH BUS SEGMENT AND TAT                 00332000
      *                        ADD NEW RECORD TYPE (T=TRAILER RECORD)   00333000
      *                        THE TRAILER RECORDS CREATED IN GCCPFRMF  00334000
      *                        ARE PASSED TO THIS PROGRAM AND TO        00335000
      *                        GCCPFRMR FOR THE DISTRIBUTION REPORT     00336000
      *                        RDS REPORT GLHPBR001                     00337000
      * 08MAY2002  VANHEMMEN   RELEASE 4.2 - ADD WEB TIMESTAMP          00338000
      *                        TO HEADER 3                              00339000
WB    * 26APR2004  BASHAWE     GBSS TASK 30069 - CHANGE DELIVERY LOC    00339100
WB    * 19JUL2004  BASHAWE     GBSS TASK 32308 - CHANGE DEL LOC 2 PICKUP00339200
      * 26AUG2008  IBM GR      UPGRADED IN ECU PROJECT                  00339200
      * 18MAY2010  ANDREW DING GLMASS REPORT RE-SORT.                   00339200
      *                        RE-ORDER IS DONE AT WEB dataserver               
      *                        MassChangeEntityImpl.java.                       
      *                        The order conditions are:                        
      *                        A. SALARY CHANGES (REASON = SPACE)               
      *                                           NEW SALARY > 0                
      *                                           NOT DIVISION CHANGES          
      *                        B. TERMINATION ALL NON SPACE REASON CODE         
      *                                           EXCEPT 'RE'                   
      *                        C. REINSTATEMENT   REASON = 'RE'                 
      *                        D. ALL OTHER CHANGES.                            
      * 05AUG2010  ANDREW DING ADD NEW FIELD TO GLMASS.                 00339200
      * MAY2013 - GROUP NUMBER EXPANSION PROJECT - 7-DIGIT COMPLIANT            
      *                                                                         
      ****************************************************************  00340000
                                                                        00350000
       ENVIRONMENT DIVISION.                                            00360000
       CONFIGURATION SECTION.                                           00370000
       SOURCE-COMPUTER.  IBM-370-165.                                   00380000
       OBJECT-COMPUTER.  IBM-370-165.                                   00390000
                                                                        00400000
       INPUT-OUTPUT SECTION.                                            00410000
                                                                        00420000
       FILE-CONTROL.                                                    00430000
                                                                        00440000
       DATA DIVISION.                                                   00441000
                                                                        00442000
       FILE SECTION.                                                    00443000
                                                                        00444000
       WORKING-STORAGE SECTION.                                         00450000
                                                                        00460000
       01  WS-CONSTANTS.                                                00470000
           05  FILLER                   PIC X(32) VALUE                 00480000
               '*** GCCPFRMM WORKING STORAGE ***'.                      00490000
                                                                        00500000
       01  WS-CALLING-VARIABLES.                                        00510000
           05  WS-GAEDATSR-VERB         PIC X(16).                      00520000
           05  WS-GAEDATSR              PIC X(8)  VALUE 'GAEDATSR'.     00530000
                                                                        00540000
       01  WS-COUNTS.                                                   00550000
           05  WS-LINE-COUNT            PIC S9(6) COMP-3 VALUE ZERO.    00560000
           05  WS-PAGE-COUNT            PIC S9(6) COMP-3 VALUE ZERO.    00570000
           05  WS-MAX-LINE-COUNT        PIC S9(6) COMP-3 VALUE +65.     00580000
                                                                        00590000
       01  WS-VARIABLES.                                                00600000
           05  WS-INPUT-LR              PIC X(16) VALUE                 00670000
                                            'CARD-DATA-010   '.         00680000
           05  WS-PRINT-LR              PIC X(16) VALUE                 00690000
                                            'PRINT-DATA-022  '.         00700000
           05  WS-PREV-REC-TYPE         PIC X VALUE SPACE.              00690000
           05  WS-CURR-REC-TYPE         PIC X VALUE SPACE.              00690000
           05  WS-PREV-TERM-TYPE        PIC X VALUE SPACE.              00690000
           05  WS-CURR-TERM-TYPE        PIC X VALUE SPACE.              00690000
               88 SALARY-CHANGE         VALUE 'S'.                              
               88 TERMINATION           VALUE 'T'.                              
               88 REINSTATEMENT         VALUE 'R'.                              
               88 OTHER-CHANGE          VALUE 'O'.                              
                                                                        00711000
       01  GLMASS-RECORD.                                               00720000
           COPY CRPTMASS.                                               00739000
                                                                        00740000
       01  PRT-LINE.                                                    00750000
           05  PRT-CTL                  PIC X(1).                       00760000
           05  PRT-DATA                 PIC X(169).                     00770000
                                                                        00780000
       01  PRT-INPUT-FILE-EMPTY.                                        00781000
           05  FILLER                   PIC X     VALUE '1'.            00782000
           05  FILLER                   PIC X(47) VALUE                 00783000
           'NO EMPLOYMENT/SALARY CHANGE FORMS/TRANSACTIONS'.            00784000
           05  FILLER                   PIC X(20) VALUE                 00784100
           'RECEIVED OR PRINTED'.                                       00784200
                                                                        00785000
       01  PRT-ENG-HDR-1.                                               00790000
           05  FILLER                   PIC X     VALUE '1'.            00800000
           05  FILLER                   PIC X(31) VALUE                 00810000
                                'EMPLOYMENT/SALARY CHANGE REPORT'.      00820000
           05  FILLER                   PIC X(69) VALUE SPACES.         00830000
           05  FILLER                   PIC X(14) VALUE                 00840000
                                        'CURRENT DATE: '.               00850000
           05  PRT-ENG-TODAY            PIC X(12).                      00860000
           05  FILLER                   PIC X(23) VALUE SPACES.         00870000
           05  FILLER                   PIC X(8)  VALUE                 00880000
                                        'GCCPFRMM'.                     00890000
           05  FILLER                   PIC X(12) VALUE SPACES.         00900000
                                                                        00901000
       01  PRT-FR-HDR-1.                                                00902000
           05  FILLER                   PIC X     VALUE '1'.            00903000
           05  FILLER                   PIC X(30) VALUE                 00904000
                               'RELEVE DE MODIFICATION GLOBALE'.        00905000
           05  FILLER                   PIC X(70) VALUE SPACES.         00906000
           05  FILLER                   PIC X(14) VALUE                 00907000
                                        'DATE DU JOUR: '.               00908000
           05  PRT-FR-TODAY             PIC X(12).                      00909000
           05  FILLER                   PIC X(23) VALUE SPACES.         00909100
           05  FILLER                   PIC X(8)  VALUE                 00909200
                                        'GCCPFRMM'.                     00909300
           05  FILLER                   PIC X(12) VALUE SPACES.         00909400
                                                                        00909500
       01  PRT-ENG-HDR-2.                                               00910000
           05  FILLER                   PIC X     VALUE '0'.            00920000
           05  FILLER                   PIC X(14) VALUE                 00930000
                                        'PLAN SPONSOR: '.               00940000
           05  PRT-ENG-SPONSOR          PIC X(60) VALUE SPACES.         00950000
           05  FILLER                   PIC X(26) VALUE SPACES.         00960000
           05  FILLER                   PIC X(21) VALUE                 00970000
                                        'CONFIRMATION NUMBER: '.        00980000
           05  PRT-ENG-CONFIRMATION     PIC X(11) VALUE SPACES.         00990000
           05  FILLER                   PIC X(14) VALUE SPACES.         01000000
           05  FILLER                   PIC X(5)  VALUE                 01010000
                                        'PAGE '.                        01020000
           05  PRT-ENG-PAGE-NO          PIC ZZZZZ9.                     01030000
           05  FILLER                   PIC X(12) VALUE SPACES.         01040000
                                                                        01050000
       01  PRT-FR-HDR-2.                                                01190000
           05  FILLER                   PIC X     VALUE '0'.            01200000
           05  FILLER                   PIC X(21) VALUE                 01210000
                                        'PROMOTEUR DE REGIME: '.        01220000
           05  PRT-FR-SPONSOR           PIC X(60) VALUE SPACES.         01230000
           05  FILLER                   PIC X(19) VALUE SPACES.         01240000
           05  FILLER                   PIC X(21) VALUE                 01250000
                                        'NUMERO DE DOSSIER:   '.        01260000
           05  PRT-FR-CONFIRMATION      PIC X(11) VALUE SPACES.         01270000
           05  FILLER                   PIC X(14) VALUE SPACES.         01280000
           05  FILLER                   PIC X(5)  VALUE                 01290000
                                        'PAGE '.                        01300000
           05  PRT-FR-PAGE-NO           PIC ZZZZZ9.                     01310000
           05  FILLER                   PIC X(12) VALUE SPACES.         01320000
                                                                        01330000
       01  PRT-ENG-HDR-3.                                               01331000
           05  FILLER                   PIC X     VALUE ' '.            01332000
           05  FILLER                   PIC X(7)  VALUE 'DATE : '.      01333000
           05  PRT-ENG-WEB-DATE         PIC X(10) VALUE SPACES.         01334000
           05  FILLER                   PIC X(10) VALUE '   TIME : '.   01335000
           05  PRT-ENG-WEB-TIME         PIC X(5)  VALUE SPACES.         01336000
           05  FILLER                   PIC X(68) VALUE SPACES.         01336100
           05  FILLER                   PIC X(21) VALUE                 01337000
                                        'SEQUENCE NUMBER:     '.        01338000
           05  PRT-ENG-SEQUENCE         PIC X(5) VALUE SPACES.          01339000
           05  FILLER                   PIC X(20) VALUE SPACES.         01339100
                                                                        01339600
       01  PRT-FR-HDR-3.                                                01339700
           05  FILLER                   PIC X     VALUE ' '.            01339800
           05  FILLER                   PIC X(7)  VALUE 'DATE : '.      01339900
           05  PRT-FR-WEB-DATE          PIC X(10) VALUE SPACES.         01340000
           05  FILLER                   PIC X(10) VALUE '  HEURE : '.   01340100
           05  PRT-FR-WEB-TIME          PIC X(5)  VALUE SPACES.         01340200
           05  FILLER                   PIC X(68) VALUE SPACES.         01340300
           05  FILLER                   PIC X(21) VALUE                 01340400
                                        'NUMERO DE SEQUENCE:  '.        01340500
           05  PRT-FR-SEQUENCE          PIC X(5) VALUE SPACES.          01340600
           05  FILLER                   PIC X(20) VALUE SPACES.         01340700
                                                                        01341100
       01  PRT-ENG-DET-1.                                               01342000
           05  FILLER                   PIC X     VALUE '0'.            01350000
           05  FILLER                   PIC X(42) VALUE                 01360000
           ' Plan   Acct/Div   Cert Num    Plan Member'.                01370000
           05  FILLER                   PIC X(127) VALUE SPACES.        01380000
                                                                        01390000
       01  PRT-FR-DET-1.                                                01391000
           05  FILLER                   PIC X     VALUE '0'.            01392000
           05  FILLER                   PIC X(42) VALUE                 01393000
           'Contrat Compte/div Certificat  Participant'.                01394000
           05  FILLER                   PIC X(127) VALUE SPACES.        01395000
                                                                        01396000
       01  PRT-DET-2.                                                   01400000
           05  FILLER                   PIC X     VALUE ' '.            01410000
           05  PRT-PLAN                 PIC X(7).                       01420000
           05  FILLER                   PIC X(5)  VALUE SPACES.         01430000
           05  PRT-ACCOUNT              PIC X(3).                       01440000
           05  FILLER                   PIC X(4)  VALUE SPACES.         01450000
           05  PRT-CERT                 PIC X(11).                      01460000
           05  FILLER                   PIC X(1)  VALUE SPACES.         01470000
           05  PRT-MBR-NAME             PIC X(75).                      01480000
           05  FILLER                   PIC X(63) VALUE SPACES.         01490000
                                                                        01500000
       01  PRT-ENG-DET-3.                                               01751000
           05  FILLER                   PIC X     VALUE '0'.            01752000
           05  FILLER                   PIC X(18) VALUE SPACES.         01753000
           05  FILLER                   PIC X(8)  VALUE                 01754000
                                        'Eff Date'.                     01755000
           05  FILLER                   PIC X(8)  VALUE SPACES.         01756000
           05  FILLER                   PIC X(4)  VALUE                 01757000
                                        'Term'.                         01758000
           05  FILLER                   PIC X(8)  VALUE SPACES.         01759000
           05  FILLER                   PIC X(9)  VALUE                 01759100
                                        'Return To'.                    01759200
           05  FILLER                   PIC X(10) VALUE SPACES.         01759300
           05  FILLER                   PIC X(3)  VALUE                 01759400
                                        'Sal'.                          01759500
           05  FILLER                   PIC X(4)  VALUE SPACES.         01759600
           05  FILLER                   PIC X(3)  VALUE                 01759700
                                        'Sal'.                          01759800
           05  FILLER                   PIC X(5)  VALUE SPACES.         01759900
           05  FILLER                   PIC X(11) VALUE                         
                                        ' Weekly    '.                          
           05  FILLER                   PIC X(10) VALUE                 01760000
                                        'Occ Change'.                   01760100
           05  FILLER                   PIC X(26) VALUE SPACES.         01760200
           05  FILLER                   PIC X(41) VALUE                 01760300
               '-Class-    Acct/Div    Bill Div   --EoI--'.             01760400
           05  FILLER                   PIC X(01) VALUE SPACES.         01760500
                                                                        01760600
       01  PRT-FR-DET-3.                                                01760700
           05  FILLER                   PIC X     VALUE '0'.            01760800
           05  FILLER                   PIC X(17) VALUE SPACES.         01760900
           05  FILLER                   PIC X(12) VALUE                 01761000
                                        'Date d''effet'.                01761100
           05  FILLER                   PIC X(4)  VALUE SPACES.         01761200
           05  FILLER                   PIC X(6)  VALUE                 01761300
                                        'Raison'.                       01761400
           05  FILLER                   PIC X(7)  VALUE SPACES.         01761500
           05  FILLER                   PIC X(12) VALUE                 01761600
                                        'Date Retour'.                  01761700
           05  FILLER                   PIC X(3)  VALUE SPACES.         01761800
           05  FILLER                   PIC X(7)  VALUE                 01761900
                                        'Nouveau'.                      01762000
           05  FILLER                   PIC X(3)  VALUE SPACES.         01762100
           05  FILLER                   PIC X(6)  VALUE                 01762200
                                        'Period'.                       01762300
           05  FILLER                   PIC X(11) VALUE                 01763000
                                        ' Heures    '.                          
           05  FILLER                   PIC X(2)  VALUE SPACES.         01762400
           05  FILLER                   PIC X(13) VALUE                 01762500
                                        'Nouvel Emploi'.                01762600
           05  FILLER                   PIC X(23) VALUE SPACES.         01762700
           05  FILLER                   PIC X(42) VALUE                 01762800
              'Categorie   Nouveau    Div Factur -Pr Ass-'.             01762900
                                                                        01763100
       01  PRT-ENG-DET-4.                                               01764000
           05  FILLER                   PIC X     VALUE ' '.            01770000
           05  FILLER                   PIC X(35) VALUE SPACES.         01780000
           05  FILLER                   PIC X(41) VALUE                 01790000
               'Rsn        Work Date          Chg    Freq'.             01800000
           05  FILLER                   PIC X(11) VALUE                         
               '     hours '.                                                   
           05  FILLER                   PIC X(40) VALUE SPACES.         01810000
           05  FILLER                   PIC X(42) VALUE                 01820000
               'Old New      Chg       Old  New   Rqd Mld'.             01830000
           05  FILLER                   PIC X(01) VALUE SPACES.         01840000
                                                                        01850000
       01  PRT-FR-DET-4.                                                01851000
           05  FILLER                   PIC X     VALUE ' '.            01852000
           05  FILLER                   PIC X(33) VALUE SPACES.         01853000
           05  FILLER                   PIC X(44) VALUE                 01854000
               'Cess         Au Travail         Sal    Sal  '.          01855000
           05  FILLER                   PIC X(11) VALUE                 01856000
               'Par semaine'.                                                   
           05  FILLER                   PIC X(39) VALUE SPACES.         01856000
           05  FILLER                   PIC X(42) VALUE                 01857000
               'Anc Nouv  Compte/div   Anc  Nouv Exig Env'.             01858000
                                                                        01859100
       01  PRT-DET-5.                                                   01860000
           05  FILLER                   PIC X     VALUE ' '.            01870000
           05  FILLER                   PIC X(17) VALUE SPACES.         01880000
           05  PRT-CHDT-DATE            PIC X(12).                      01890000
           05  FILLER                   PIC X(6)  VALUE SPACES.         01891000
           05  PRT-TERM-REASON          PIC XX.                         01892000
           05  FILLER                   PIC X(8)  VALUE SPACES.         01893000
           05  PRT-RETURN-DATE          PIC X(12).                      01894000
           05  FILLER                   PIC X     VALUE SPACES.         01900000
           05  PRT-SALARY-CHG           PIC ZZ,ZZZ,ZZ9.99.              01910000
           05  FILLER                   PIC X(2)  VALUE SPACES.         01920000
           05  PRT-SALARY-FREQ          PIC X(1).                       01930000
           05  FILLER                   PIC X(8)  VALUE SPACES.         01940000
           05  PRT-WORKING-HOURS        PIC Z9.9.                       01930000
           05  FILLER                   PIC X(5)  VALUE SPACES.         01940000
           05  PRT-OCCUPATION-CHG       PIC X(29).                      01950000
           05  FILLER                   PIC X(7)  VALUE SPACES.         01960000
           05  PRT-CLASS-OLD            PIC X(3).                       01970000
           05  FILLER                   PIC X     VALUE SPACES.         01980000
           05  PRT-CLASS-NEW            PIC X(3).                       01990000
           05  FILLER                   PIC X(6)  VALUE SPACES.         02000000
           05  PRT-ACCT-CHG             PIC X(3).                       02010000
           05  FILLER                   PIC X(7)  VALUE SPACES.         02020000
           05  PRT-DIV-OLD              PIC X(3).                       02030000
           05  FILLER                   PIC XX    VALUE SPACES.         02040000
           05  PRT-DIV-NEW              PIC X(3).                       02050000
           05  FILLER                   PIC X(4)  VALUE SPACES.         02060000
           05  PRT-EVIDENCE-REQD        PIC X(1).                       02070000
           05  FILLER                   PIC X(3)  VALUE SPACES.         02080000
           05  PRT-MAILED               PIC X(1).                       02090000
           05  FILLER                   PIC X(03) VALUE SPACES.         02100000
                                                                        02110000
       01  PRT-UNDERLINE.                                               02110100
           05  FILLER                   PIC X     VALUE '0'.            02110200
           05  FILLER                   PIC X(57) VALUE                 02110300
           '========================================================='. 02110400
           05  FILLER                   PIC X(57) VALUE                 02110500
           '========================================================='. 02110600
           05  FILLER                   PIC X(55) VALUE                 02110700
           '======================================================='.   02110800
                                                                        02111100
       01  PRT-BLANKLINE.                                               02111200
           05  FILLER                   PIC X     VALUE ' '.            02111300
           05  FILLER                   PIC X(169) VALUE SPACES.        02111400
                                                                        02111500
       01  PRT-ENG-LAST.                                                02111600
           05  FILLER                   PIC X     VALUE ' '.            02111700
           05  FILLER                   PIC X(13) VALUE                 02111800
                                        'END OF REPORT'.                02111900
           05  FILLER                   PIC X(156) VALUE SPACES.        02112000
                                                                        02112100
       01  PRT-FR-LAST.                                                 02112200
           05  FILLER                   PIC X     VALUE ' '.            02112300
           05  FILLER                   PIC X(13) VALUE                 02112400
                                        'FIN DU RELEVE'.                02112500
           05  FILLER                   PIC X(156) VALUE SPACES.        02112600
                                                                        02112700
       01  PRT-BANNER-1.                                                02112800
           05  FILLER                   PIC X     VALUE '1'.            02112900
           05  FILLER                   PIC X(169) VALUE SPACES.        02113000
                                                                        02114000
       01  PRT-BANNER-2.                                                02115000
           05  FILLER                   PIC X     VALUE '-'.            02116000
           05  FILLER                   PIC X(20) VALUE                 02117000
                                        'DISTRIBUTE TO:'.               02118000
           05  FILLER                   PIC X(149) VALUE SPACES.        02119000
                                                                        02119100
       01  PRT-BANNER-3.                                                02119200
           05  FILLER                   PIC X     VALUE '-'.            02119300
           05  FILLER                   PIC X(21) VALUE                 02119400
                                        'PLAN MEMBER ADMIN. - '.        02119500
           05  PRT-BANNER-3-DEST        PIC X(4).                       02119600
           05  FILLER                   PIC X(144) VALUE SPACES.        02119700
                                                                        02119800
       01  PRT-BANNER-4.                                                02119900
           05  FILLER                   PIC X     VALUE '-'.            02120000
           05  FILLER                   PIC X(20) VALUE                 02120100
                                        'TURNAROUND TIME:  '.           02120200
           05  PRT-BANNER-4-TAT         PIC XX.                         02120300
           05  FILLER                   PIC X(147) VALUE SPACES.        02120400
                                                                        02120500
       01  PRT-BANNER-5.                                                02120600
           05  FILLER                   PIC X     VALUE '-'.            02120700
           05  FILLER                   PIC X(22) VALUE                 02120800
WB                                      'DEL. STATION: PICKUP'.         02120900
WB    *                                 'DEL. STATION: 500-G-C'.        02121000
      *                                 'DEL. STATION:  GB-C'.          02121100
           05  FILLER                   PIC X(147) VALUE SPACES.        02121200
                                                                        02121300
       01  PRT-TRAILER-1.                                               02121400
           05  FILLER                   PIC X     VALUE '0'.            02121500
           05  FILLER                   PIC X(20) VALUE                 02121600
                                        'TRANSACTION COUNT: '.          02121700
           05  PRT-TRAILER-1-TRANS-COUNT PIC ZZZZZZ9.                   02121800
           05  FILLER                   PIC X(142) VALUE SPACES.        02121900
                                                                        02122000
       01  PRT-TRAILER-2.                                               02122100
           05  FILLER                   PIC X     VALUE '0'.            02122200
           05  PRT-TRAILER-2-GLMASS     PIC X(70).                      02122300
           05  FILLER                   PIC X(99).                      02122400
                                                                        02122500
       01  PRT-TRAILER-3.                                               02122600
           05  FILLER                   PIC X     VALUE '0'.            02122700
           05  FILLER                   PIC X(20) VALUE                 02122800
           'LAST PAGE OF'.                                              02122900
                                                                        02123000
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                  02124000
                                                                        02130000
       01  ICBM.                        COPY ICBM.                      02140000
                                                                        02150000
       01  WS-SWITCHES.                                                 02160000
           05  WS-EOF-SWITCH            PIC X.                          02170000
               88  WS-EOF               VALUE '1'.                      02180000
                                                                        02190000
           05  WS-ERROR-SWITCH          PIC S9(4).                      02200000
               88  WS-RUN-SUCCESSFUL    VALUE +0.                       02201000
               88  WS-INPUT-FILE-EMPTY  VALUE +4.                       02202000
               88  WS-FATAL-ERROR       VALUE +16.                      02210000
                                                                        02220000
           05  WS-LANG-SWITCH           PIC X.                          02221000
               88  WS-ENGLISH           VALUE 'E'.                      02222000
               88  WS-FRENCH            VALUE 'F'.                      02223000
                                                                        02224000
           05  WS-BANNER-CONTROL        PIC X(3).                       02225000
           05  WS-TRANS-COUNT           PIC 9(7) COMP-3 VALUE 0.        02225100
                                                                        02226000
       PROCEDURE DIVISION.                                              02230000
                                                                        02240000
       0000-MAINLINE.                                                   02250000
                                                                        02260000
           PERFORM 1000-INITIALIZATION   THRU 1000-EXIT.                02270000
                                                                        02280000
           PERFORM 2000-PROCESS-INPUT    THRU 2000-EXIT                 02290000
               UNTIL WS-EOF                                             02300000
               OR    WS-FATAL-ERROR.                                    02310000
                                                                        02320000
           PERFORM 3000-FINISH           THRU 3000-EXIT.                02330000
                                                                        02331000
           IF NOT WS-RUN-SUCCESSFUL                                     02332000
               MOVE WS-ERROR-SWITCH      TO RETURN-CODE                 02333000
           END-IF.                                                      02334000
                                                                        02340000
           GOBACK.                                                      02350000
                                                                        02360000
       0000-EXIT.                                                       02370000
           EXIT.                                                        02380000
                                                                        02390000
       1000-INITIALIZATION.                                             02400000
      ***************************************************************** 02410000
      *  - INITIALIZE VARIABLES                                         02420000
      *  - READ FIRST RECORD                                            02430000
      *  - PRINT BANNER PAGE                                            02440000
      ***************************************************************** 02450000
                                                                        02460000
           MOVE 'GCCPFRMM'          TO   ICBM-PROGRAM-NAME.             02461000
           MOVE LOW-VALUES          TO   LINKAGE-CONTROL.               02462000
                                                                        02463000
           MOVE '0'                 TO   WS-EOF-SWITCH.                 02470000
           MOVE +0                  TO   WS-ERROR-SWITCH.               02480000
           MOVE SPACE               TO   WS-PREV-REC-TYPE                       
                                         WS-CURR-REC-TYPE                       
                                         WS-PREV-TERM-TYPE                      
                                         WS-CURR-TERM-TYPE.                     
                                                                        02490000
           MOVE OBTAIN-FIRST        TO   WS-GAEDATSR-VERB.              02500000
           PERFORM 9000-READ-INPUT       THRU 9000-EXIT.                02510000
                                                                        02520000
           IF WS-FATAL-ERROR                                            02521000
               GO TO 1000-EXIT.                                         02522000
                                                                        02523000
           IF WS-EOF                                                    02530000
               MOVE PRT-INPUT-FILE-EMPTY  TO PRT-LINE                   02530100
               PERFORM 9100-PRINT-LINE                                  02530200
                  THRU 9100-EXIT                                        02530300
               SET WS-INPUT-FILE-EMPTY   TO  TRUE                       02550000
               GO TO 1000-EXIT.                                         02560000
                                                                        02570000
       1000-EXIT.                                                       02650000
           EXIT.                                                        02660000
                                                                        02670000
       2000-PROCESS-INPUT.                                              02680000
      ***************************************************************** 02690000
      *  - FOR HEADER RECORDS, PRINT PAGE HEADINGS                      02700000
      *  - FOR DETAIL RECORDS, PRINT DETAIL LINES                       02710000
      *  - FOR TRAILER RECORDS PRINT TRAILER PAGES                      02720000
      ***************************************************************** 02730000
                                                                        02740000
           EVALUATE TRUE                                                02741000
               WHEN GLMASS-REC-TYPE  =  'H'                             02750000
                   IF GLMASS-BANNER-CONTROL NOT = WS-BANNER-CONTROL     02751000
                       PERFORM 5300-PRT-BANNER-PAGE                     02752000
                          THRU 5300-EXIT                                02753000
                   END-IF                                               02754000
                   MOVE GLMASS-LANG              TO WS-LANG-SWITCH      02760000
                   IF WS-FRENCH                                         02770000
                       MOVE GLMASS-SPONSOR       TO PRT-FR-SPONSOR      02771000
                       MOVE GLMASS-CONFIRMATION  TO PRT-FR-CONFIRMATION 02772000
                       MOVE GLMASS-SEQ-NO        TO PRT-FR-SEQUENCE     02773000
                       MOVE GLMASS-FRENCH-TODAY  TO PRT-FR-TODAY        02774000
                       MOVE GLMASS-DATE-TIME(9:10)                      02774100
                                                 TO PRT-FR-WEB-DATE     02774200
                       MOVE GLMASS-DATE-TIME(32:5)                      02774300
                                                 TO PRT-FR-WEB-TIME     02774400
                       PERFORM 5100-PRT-FR-HEADINGS                     02775000
                          THRU 5100-EXIT                                02776000
                   ELSE                                                 02820000
                       MOVE GLMASS-SPONSOR       TO PRT-ENG-SPONSOR     02830000
                       MOVE GLMASS-CONFIRMATION  TO PRT-ENG-CONFIRMATION02840000
                       MOVE GLMASS-SEQ-NO        TO PRT-ENG-SEQUENCE    02850000
                       MOVE GLMASS-ENGLISH-TODAY TO PRT-ENG-TODAY       02860000
                       MOVE GLMASS-DATE-TIME(9:10)                      02860100
                                                 TO PRT-ENG-WEB-DATE    02860200
                       MOVE GLMASS-DATE-TIME(32:5)                      02860300
                                                 TO PRT-ENG-WEB-TIME    02860400
                       PERFORM 5000-PRT-ENG-HEADINGS                    02861000
                          THRU 5000-EXIT                                02862000
                   END-IF                                               02870000
                   IF WS-FATAL-ERROR                                    02871000
                       GO TO 2000-EXIT                                  02872000
                   END-IF                                               02873000
                   MOVE OBTAIN-NEXT             TO WS-GAEDATSR-VERB     02874000
                   PERFORM 9000-READ-INPUT                              02875000
                      THRU 9000-EXIT                                    02876000
               WHEN GLMASS-REC-TYPE  =  'D'                             02880000
                   PERFORM 5200-PRT-DETAIL-LINES                        02900000
                      THRU 5200-EXIT                                    02901000
                   IF WS-FATAL-ERROR                                    02902000
                       GO TO 2000-EXIT                                  02903000
                   END-IF                                               02904000
                   MOVE OBTAIN-NEXT             TO WS-GAEDATSR-VERB     02905000
                   PERFORM 9000-READ-INPUT                              02906000
                      THRU 9000-EXIT                                    02907000
               WHEN GLMASS-REC-TYPE  =  'T'                             02908000
                   PERFORM 5400-PRT-TRAILER-PAGE                        02909000
                      THRU 5400-EXIT                                    02909100
                      UNTIL GLMASS-REC-TYPE  NOT =  'T'                 02909200
                      OR    WS-FATAL-ERROR                              02909300
                      OR    WS-EOF                                      02909400
               WHEN OTHER                                               02910000
                   DISPLAY '************************************'       02911000
                   DISPLAY '*INVALID RECORD TYPE ON INPUT FILE *'       02912000
                   DISPLAY '************************************'       02913000
                   SET WS-FATAL-ERROR           TO TRUE                 02914000
           END-EVALUATE.                                                02915000
                                                                        02920000
       2000-EXIT.                                                       02990000
           EXIT.                                                        03000000
                                                                        03010000
       3000-FINISH.                                                     03020000
      ***************************************************************** 03030000
      *  - PRINT FINAL LINE ON MASS CHANGE REPORT                       03040000
      *  - CLOSE FILES                                                  03050000
      ***************************************************************** 03060000
                                                                        03070000
           IF WS-FATAL-ERROR                                            03080000
               GO TO 3000-EXIT.                                         03090000
                                                                        03100000
           IF WS-RUN-SUCCESSFUL                                         03101000
               IF WS-FRENCH                                             03110000
                   MOVE PRT-FR-LAST          TO  PRT-LINE               03120000
               ELSE                                                     03130000
                   MOVE PRT-ENG-LAST           TO  PRT-LINE             03140000
               END-IF                                                   03150000
                                                                        03160000
               PERFORM 9100-PRINT-LINE            THRU  9100-EXIT       03170000
               IF WS-FATAL-ERROR                                        03180000
                   GO TO 3000-EXIT                                      03190000
               END-IF                                                   03191000
           END-IF.                                                      03192000
                                                                        03200000
           MOVE FINISH-LR                     TO  WS-GAEDATSR-VERB.     03210000
                                                                        03220000
           CALL WS-GAEDATSR   USING WS-GAEDATSR-VERB                    03230000
                                    LOGICAL-RECORD-NAME                 03240000
                                    ICBM.                               03250000
                                                                        03260000
       3000-EXIT.                                                       03270000
           EXIT.                                                        03280000
                                                                        03290000
       5000-PRT-ENG-HEADINGS.                                           03300000
      ***************************************************************** 03310000
      *  - INCREMENT PAGE COUNTER                                       03320000
      *  - PRINT ENGLISH HEADINGS                                       03330000
      *  - SET LINE COUNTER TO 4                                        03340000
      ***************************************************************** 03350000
                                                                        03360000
           ADD 1 TO WS-PAGE-COUNT.                                      03370000
           MOVE WS-PAGE-COUNT                 TO  PRT-ENG-PAGE-NO.      03380000
           MOVE PRT-ENG-HDR-1                 TO  PRT-LINE.             03390000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      03400000
                                                                        03410000
           IF WS-FATAL-ERROR                                            03420000
               GO TO 5000-EXIT.                                         03430000
                                                                        03440000
           MOVE PRT-ENG-HDR-2                 TO  PRT-LINE.             03450000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      03460000
                                                                        03470000
           IF WS-FATAL-ERROR                                            03480000
               GO TO 5000-EXIT.                                         03490000
                                                                        03500000
           MOVE PRT-ENG-HDR-3                 TO  PRT-LINE.             03501000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      03502000
                                                                        03503000
           IF WS-FATAL-ERROR                                            03504000
               GO TO 5000-EXIT.                                         03505000
                                                                        03506000
           MOVE +6                            TO  WS-LINE-COUNT.        03510000
                                                                        03520000
       5000-EXIT.                                                       03530000
           EXIT.                                                        03540000
                                                                        03550000
       5100-PRT-FR-HEADINGS.                                            03560000
      ***************************************************************** 03570000
      *  - INCREMENT PAGE COUNTER                                       03580000
      *  - PRINT FRENCH HEADINGS                                        03590000
      *  - SET LINE COUNTER TO 5                                        03600000
      ***************************************************************** 03610000
                                                                        03620000
           ADD 1 TO WS-PAGE-COUNT.                                      03630000
           MOVE WS-PAGE-COUNT                 TO  PRT-FR-PAGE-NO.       03640000
           MOVE PRT-FR-HDR-1                  TO  PRT-LINE.             03650000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      03660000
                                                                        03670000
           IF WS-FATAL-ERROR                                            03680000
               GO TO 5100-EXIT.                                         03690000
                                                                        03700000
           MOVE PRT-FR-HDR-2                  TO  PRT-LINE.             03710000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      03720000
                                                                        03730000
           IF WS-FATAL-ERROR                                            03740000
               GO TO 5100-EXIT.                                         03750000
                                                                        03760000
           MOVE PRT-FR-HDR-3                  TO  PRT-LINE.             03761000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      03762000
                                                                        03763000
           IF WS-FATAL-ERROR                                            03764000
               GO TO 5100-EXIT.                                         03765000
                                                                        03766000
           MOVE +6                            TO  WS-LINE-COUNT.        03770000
                                                                        03780000
       5100-EXIT.                                                       03790000
           EXIT.                                                        03800000
                                                                        03810000
       5200-PRT-DETAIL-LINES.                                           03820000
      ***************************************************************** 03830000
      *  - PRINT ENGLISH OR FRENCH DETAIL LINES                         03840000
      *  - MOVE INPUT TO PRINT LINES                                    03850000
      *  - ON PAGE OVERFLOW PRINT HEADINGS                              03860000
      ***************************************************************** 03870000
                                                                        03880000
           IF WS-LINE-COUNT  >  WS-MAX-LINE-COUNT OR                    03890000
            ((WS-PREV-TERM-TYPE NOT = WS-CURR-TERM-TYPE) AND                    
             (WS-PREV-REC-TYPE = 'D'  AND WS-CURR-REC-TYPE = 'D'))              
               IF WS-FRENCH                                             03900000
                   PERFORM 5100-PRT-FR-HEADINGS  THRU  5100-EXIT        03910000
               ELSE                                                     03920000
                   PERFORM 5000-PRT-ENG-HEADINGS   THRU  5000-EXIT      03930000
               END-IF                                                   03940000
               MOVE WS-CURR-TERM-TYPE   TO WS-PREV-TERM-TYPE                    
           END-IF.                                                      03950000
                                                                        03960000
           IF WS-FATAL-ERROR                                            03970000
               GO TO 5200-EXIT.                                         03980000
                                                                        03990000
           IF WS-FRENCH                                                 04000000
               MOVE PRT-FR-DET-1             TO  PRT-LINE               04010000
           ELSE                                                         04020000
               MOVE PRT-ENG-DET-1              TO  PRT-LINE             04030000
           END-IF.                                                      04040000
                                                                        04050000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04060000
                                                                        04070000
           IF WS-FATAL-ERROR                                            04080000
               GO TO 5200-EXIT.                                         04090000
                                                                        04106000
           MOVE GLMASS-PLAN                   TO  PRT-PLAN.             04110000
           MOVE GLMASS-ACCOUNT                TO  PRT-ACCOUNT.          04120000
           MOVE GLMASS-CERT                   TO  PRT-CERT.             04130000
           MOVE GLMASS-MBR-NAME               TO  PRT-MBR-NAME.         04140000
                                                                        04150000
           MOVE PRT-DET-2                     TO  PRT-LINE.             04160000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04170000
                                                                        04180000
           IF WS-FATAL-ERROR                                            04190000
               GO TO 5200-EXIT.                                         04200000
                                                                        04210000
           IF WS-FRENCH                                                 04220000
               MOVE PRT-FR-DET-3             TO  PRT-LINE               04230000
           ELSE                                                         04240000
               MOVE PRT-ENG-DET-3              TO  PRT-LINE             04250000
           END-IF.                                                      04260000
                                                                        04270000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04280000
                                                                        04290000
           IF WS-FATAL-ERROR                                            04300000
               GO TO 5200-EXIT.                                         04310000
                                                                        04320000
           IF WS-FRENCH                                                 04340000
               MOVE PRT-FR-DET-4             TO  PRT-LINE               04350000
           ELSE                                                         04360000
               MOVE PRT-ENG-DET-4              TO  PRT-LINE             04370000
           END-IF.                                                      04380000
                                                                        04390000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04400000
                                                                        04410000
           IF WS-FATAL-ERROR                                            04420000
               GO TO 5200-EXIT.                                         04430000
                                                                        04440000
           MOVE GLMASS-CHDT-DATE              TO  PRT-CHDT-DATE.        04450000
           MOVE GLMASS-TERM-REASON            TO  PRT-TERM-REASON.      04460000
           MOVE GLMASS-RETURN-DATE            TO  PRT-RETURN-DATE.      04470000
           MOVE GLMASS-SALARY-CHG             TO  PRT-SALARY-CHG.       04480000
           MOVE GLMASS-SALARY-FREQ            TO  PRT-SALARY-FREQ.      04490000
           MOVE GLMASS-WORKING-HOURS          TO  PRT-WORKING-HOURS.    04490000
           MOVE GLMASS-OCCUPATION-CHG         TO  PRT-OCCUPATION-CHG.   04500000
           MOVE GLMASS-CLASS-OLD              TO  PRT-CLASS-OLD.        04510000
           MOVE GLMASS-CLASS-NEW              TO  PRT-CLASS-NEW.        04520000
           MOVE GLMASS-ACCT-CHG               TO  PRT-ACCT-CHG.         04530000
           MOVE GLMASS-DIV-OLD                TO  PRT-DIV-OLD.          04540000
           MOVE GLMASS-DIV-NEW                TO  PRT-DIV-NEW.          04550000
           MOVE GLMASS-EVIDENCE-REQD          TO  PRT-EVIDENCE-REQD.    04560000
           MOVE GLMASS-MAILED                 TO  PRT-MAILED.           04570000
                                                                        04580000
           MOVE PRT-DET-5                     TO  PRT-LINE.             04590000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04600000
                                                                        04610000
           IF WS-FATAL-ERROR                                            04620000
               GO TO 5200-EXIT.                                         04630000
                                                                        04640000
           MOVE PRT-UNDERLINE                 TO  PRT-LINE.             04641000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04642000
                                                                        04643000
           IF WS-FATAL-ERROR                                            04644000
               GO TO 5200-EXIT.                                         04645000
                                                                        04646000
           ADD 1  TO WS-TRANS-COUNT.                                    04647000
           ADD 10 TO WS-LINE-COUNT.                                     04650000
                                                                        04660000
       5200-EXIT.                                                       04670000
           EXIT.                                                        04680000
                                                                        04690000
       5300-PRT-BANNER-PAGE.                                            04700000
      ***************************************************************** 04710000
      *  - PRINT BANNER PAGE                                            04720000
      ***************************************************************** 04730000
                                                                        04740000
           MOVE GLMASS-BANNER-CONTROL         TO  WS-BANNER-CONTROL.    04741000
                                                                        04742000
           MOVE PRT-BANNER-1                  TO  PRT-LINE.             04750000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04760000
                                                                        04770000
           IF WS-FATAL-ERROR                                            04780000
               GO TO 5300-EXIT.                                         04790000
                                                                        04800000
           MOVE PRT-BANNER-2                  TO  PRT-LINE.             04810000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04820000
                                                                        04830000
           IF WS-FATAL-ERROR                                            04840000
               GO TO 5300-EXIT.                                         04850000
                                                                        04860000
           EVALUATE TRUE                                                04861000
              WHEN GLMASS-BUS-SEG  =  'E'                               04862000
                  MOVE 'EAST'                 TO  PRT-BANNER-3-DEST     04863000
              WHEN GLMASS-BUS-SEG  =  'W'                               04864000
                  MOVE 'WEST'                 TO  PRT-BANNER-3-DEST     04865000
              WHEN GLMASS-BUS-SEG  =  'G'                               04866000
                  MOVE 'GFM'                  TO  PRT-BANNER-3-DEST     04867000
              WHEN OTHER                                                04868000
                  MOVE SPACES                 TO  PRT-BANNER-3-DEST     04869000
           END-EVALUATE.                                                04869100
                                                                        04869200
           MOVE PRT-BANNER-3                  TO  PRT-LINE.             04870000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04880000
                                                                        04890000
           IF WS-FATAL-ERROR                                            04900000
               GO TO 5300-EXIT.                                         04910000
                                                                        04920000
           MOVE GLMASS-TAT                    TO  PRT-BANNER-4-TAT.     04920100
           MOVE PRT-BANNER-4                  TO  PRT-LINE.             04921000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04922000
                                                                        04923000
           IF WS-FATAL-ERROR                                            04924000
               GO TO 5300-EXIT.                                         04925000
                                                                        04926000
           MOVE PRT-BANNER-5                  TO  PRT-LINE.             04927000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04928000
                                                                        04929000
           IF WS-FATAL-ERROR                                            04929100
               GO TO 5300-EXIT.                                         04929200
                                                                        04929300
       5300-EXIT.                                                       04930000
           EXIT.                                                        04940000
                                                                        04941000
       5400-PRT-TRAILER-PAGE.                                           04942000
      ***************************************************************** 04943000
      *  - PRINT TRAILER PAGE                                           04944000
      ***************************************************************** 04945000
                                                                        04946000
           MOVE PRT-BANNER-1                  TO  PRT-LINE.             04946100
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04946200
                                                                        04946300
           IF WS-FATAL-ERROR                                            04946400
               GO TO 5400-EXIT.                                         04946500
                                                                        04946600
           MOVE WS-TRANS-COUNT        TO  PRT-TRAILER-1-TRANS-COUNT.    04947000
           MOVE 0                     TO  WS-TRANS-COUNT.               04948000
           MOVE PRT-TRAILER-1                 TO  PRT-LINE.             04949000
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04949100
                                                                        04949200
           IF WS-FATAL-ERROR                                            04949300
               GO TO 5400-EXIT.                                         04949400
                                                                        04949500
           MOVE PRT-TRAILER-3                 TO  PRT-LINE.             04949600
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04949700
                                                                        04949800
           IF WS-FATAL-ERROR                                            04949900
               GO TO 5400-EXIT.                                         04950000
                                                                        04951000
           MOVE PRT-BANNER-3                  TO  PRT-LINE.             04951300
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04951400
                                                                        04951500
           IF WS-FATAL-ERROR                                            04951600
               GO TO 5400-EXIT.                                         04951700
                                                                        04951800
           MOVE PRT-BANNER-4                  TO  PRT-LINE.             04952600
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04952700
                                                                        04952800
           IF WS-FATAL-ERROR                                            04952900
               GO TO 5400-EXIT.                                         04953000
                                                                        04953100
           MOVE PRT-BLANKLINE                 TO  PRT-LINE.             04953200
           PERFORM 9100-PRINT-LINE                THRU  9100-EXIT.      04953300
                                                                        04953400
           IF WS-FATAL-ERROR                                            04953500
               GO TO 5400-EXIT.                                         04953600
                                                                        04953700
           PERFORM 5410-PRT-TRAILER-RECORDS                             04953800
              THRU 5410-EXIT                                            04953900
              UNTIL WS-EOF                                              04954000
              OR    GLMASS-REC-TYPE  NOT =  'T'                         04954100
              OR    WS-FATAL-ERROR.                                     04954200
                                                                        04954300
       5400-EXIT.                                                       04954400
           EXIT.                                                        04954500
                                                                        04954600
       5410-PRT-TRAILER-RECORDS.                                        04955000
      ***************************************************************** 04956000
      *  - PRINT THE TOTAL LINES CREATED IN GCCPFRMF                    04957000
      ***************************************************************** 04958000
                                                                        04959000
           MOVE GLMASS-TRAILER      TO  PRT-TRAILER-2-GLMASS.           04959100
           MOVE PRT-TRAILER-2       TO  PRT-LINE.                       04959200
           PERFORM 9100-PRINT-LINE                                      04959300
              THRU 9100-EXIT.                                           04959400
                                                                        04959500
           IF WS-FATAL-ERROR                                            04959600
               GO TO 5410-EXIT.                                         04959700
                                                                        04959800
           MOVE OBTAIN-NEXT         TO  WS-GAEDATSR-VERB.               04959900
           PERFORM 9000-READ-INPUT                                      04960000
              THRU 9000-EXIT.                                           04960100
                                                                        04960200
       5410-EXIT.                                                       04960300
           EXIT.                                                        04960400
                                                                        04960500
       9000-READ-INPUT.                                                 04961000
      ***************************************************************** 04970000
      *  - CALL DATA SERVER TO READ FLAT FILE OF MASS CHANGES           04980000
      ***************************************************************** 04990000
                                                                        05000000
           MOVE WS-INPUT-LR         TO  LOGICAL-RECORD-NAME.            05010000
                                                                        05020000
           CALL WS-GAEDATSR         USING WS-GAEDATSR-VERB              05030000
                                          GLMASS-RECORD                 05040000
                                          ICBM.                         05050000
                                                                        05060000
           EVALUATE TRUE                                                05070000
               WHEN LR-NOT-FOUND                                        05080000
                    SET WS-EOF      TO  TRUE                            05090000
               WHEN LR-STATUS-OK                                        05100000
                    CONTINUE                                            05110000
               WHEN OTHER                                               05120000
                    DISPLAY '*****************************'             05121000
                    DISPLAY '*ERROR IN READING INPUT FILE*'             05130000
                    DISPLAY '*****************************'             05131000
                    DISPLAY LINKAGE-STATUS                              05132000
                    SET WS-FATAL-ERROR  TO  TRUE                        05140000
           END-EVALUATE.                                                05150000
                                                                                
           MOVE WS-CURR-REC-TYPE        TO WS-PREV-REC-TYPE.                    
           MOVE GLMASS-REC-TYPE         TO WS-CURR-REC-TYPE.                    
           IF GLMASS-REC-TYPE = 'D'                                             
              MOVE WS-CURR-TERM-TYPE    TO WS-PREV-TERM-TYPE                    
              EVALUATE TRUE                                                     
              WHEN (GLMASS-TERM-REASON = '  ')    AND                           
                   (GLMASS-SALARY-FREQ NOT = ' ') AND                           
                   (GLMASS-ACCT-CHG = '   ')                                    
                  SET SALARY-CHANGE      TO TRUE                                
              WHEN (GLMASS-TERM-REASON NOT = '  ') AND                          
                   (GLMASS-TERM-REASON NOT = 'RE')                              
                  SET TERMINATION        TO TRUE                                
              WHEN GLMASS-TERM-REASON = 'RE'                                    
                  SET REINSTATEMENT      TO TRUE                                
              WHEN OTHER                                                        
                  SET OTHER-CHANGE       TO TRUE                                
              END-EVALUATE                                                      
           END-IF.                                                              
                                                                        05160000
       9000-EXIT.                                                       05170000
           EXIT.                                                        05180000
                                                                        05190000
       9100-PRINT-LINE.                                                 05200000
      ***************************************************************** 05210000
      *  - CALL DATA SERVER TO PRINT THE MASS CHANGE REPORT             05220000
      ***************************************************************** 05230000
                                                                        05240000
           MOVE WS-PRINT-LR         TO  LOGICAL-RECORD-NAME.            05250000
           MOVE STORE-LR            TO  WS-GAEDATSR-VERB.               05260000
                                                                        05270000
           CALL WS-GAEDATSR         USING WS-GAEDATSR-VERB              05280000
                                          PRT-LINE                      05290000
                                          ICBM.                         05300000
                                                                        05310000
           IF NOT LR-STATUS-OK                                          05320000
               DISPLAY '*********************************'              05330000
               DISPLAY '*ERROR IN WRITING TO OUTPUT FILE*'              05340000
               DISPLAY '*********************************'              05350000
               DISPLAY LINKAGE-STATUS                                   05351000
               SET WS-FATAL-ERROR   TO  TRUE                            05360000
           END-IF.                                                      05370000
                                                                        05380000
       9100-EXIT.                                                       05390000
           EXIT.                                                        05400000
