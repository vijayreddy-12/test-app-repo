       CBL FLAG(I),DATA(24)                                                     
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRMN.                                                 
      *AUTHOR.        J. ELKINS.                                                
      *DATE-WRITTEN.  MAR 14, 2002.                                             
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS - E-COMMERCE                                          
      *   GCCPFRMN - PRINT THE LIST OF ACTIVE GROUP/DIVISIONS WITH              
      *              GB INTERNET ACCESS                                         
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *                                                                         
      *   THIS PROGRAM PRINTS A REPORT OF SITE AUTHORITIES AND NUMBER           
      *   OF LIVES FOR GROUP/DIVISION BY                                        
      *   BUSINESS SEGMENT AND TURNAROUND TIME.                                 
      *                                                                         
      *                                                                         
      *   INPUT FILES:  NONE                                                    
      *                                                                         
      *   OUTPUT FILES: LIST OF ACTIVE GROUP/DIVISIONS WITH GB INTERNET         
      *                 ACCESS REPORT                                           
      *                                                                         
      *   DB2 TABLES                                                            
      *   ACCESSED:     TGD     - GROUP DIVISION        - SELECT                
      *                 TCTBS   - BUSINESS SEGMENT CODE - SELECT                
      *                 TCTTAT  - TURNAROUND TIME       - SELECT                
      *                                                                         
      *   CALLS:        GAEDATSR - FILE I/O                                     
      *                 GCCPFRMO - MODULE TO CALL                               
      *                            GIPSY I-O ROUTINE FOR DETAIL RECORD          
      *                                                                         
      *   INCLUDE CODE: SQLCA    - SQL COMMUNICATION AREA                       
      *                                                                         
      *                 TGDD     - TGD     TABLE DECLARATION                    
      *                 TCTBSD   - TCTBS   TABLE DECLARATION                    
      *                 TCTTATD  - TCTTAT  TABLE DECLARATION                    
      *                                                                         
      *                 TGD      - TGD     HOST VARIABLES                       
      *                 TCTBS    - TCTBS   HOST VARIABLES                       
      *                 TCTTAT   - TCTTAT  HOST VARIABLES                       
      *                                                                         
      *                                                                         
      *                                                                         
      *                 ICBM     - INTERFACE CONTROL BLOCK (DATA SERVER)        
      *                                                                         
      *****************************************************************         
      *****************************************************************         
      *   MODIFICATION LOG                                                      
      ******************************************************************        
      * PROGRAMMER   ‡  DATE  ‡             CHANGE                              
      *    NAME      ‡DD/MM/YY‡           DESCRIPTION                           
      *--------------+--------+-----------------------------------------        
      * J. ELKINS    ‡14/03/02‡ ORIGINAL CODE                                   
      * R. KNAUD     ‡MAY 2003‡ HUB: CHANGE IGRP READ TO CALL XC4SORIG;         
      *              ‡        ‡      ADDITIONAL VO REQUIREMENTS MAY             
      *              ‡        ‡      TURN UP LATER.                             
      * IBM GR       ‡26/08/08‡ UPGRADED IN ECU PROJECT                         
      *--------------+--------+-----------------------------------------        
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-370-165.                                            
       OBJECT-COMPUTER. IBM-370-165.                                            
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *****************************************************************         
      *** DB2 INCLUDES                                                          
      *****************************************************************         
                                                                                
      *** SQL COMMUNICATION AREA                                                
                                                                                
           EXEC SQL INCLUDE SQLCA                                               
                                    END-EXEC.                                   
                                                                                
      *** DB2 TABLE DECLARATIONS                                                
                                                                                
           EXEC SQL INCLUDE TGDD                                                
                                    END-EXEC.                                   
           EXEC SQL INCLUDE TCTBSD                                              
                                    END-EXEC.                                   
           EXEC SQL INCLUDE TCTTATD                                             
                                    END-EXEC.                                   
                                                                                
      *** DB2 HOST & INDICATOR VARIABLES                                        
                                                                                
      *01  DCLTGC.                                                              
           EXEC SQL INCLUDE TGD                                                 
                                    END-EXEC.                                   
                                                                                
      *01  DCLTCTTAT.                                                           
           EXEC SQL INCLUDE TCTTAT                                              
                                    END-EXEC.                                   
                                                                                
      *01  DCLTCTBS.                                                            
           EXEC SQL INCLUDE TCTBS                                               
                                    END-EXEC.                                   
                                                                                
       01  GAEDATSR-PARMS.                                                      
                                        COPY GARDSVRB.                          
                                                                                
      *****************************************************************         
      *** TURNAROUND TIME (TAT) CODES AND DESCRIPTIONS                          
      *****************************************************************         
       01  WS-TAT-CODE-TABLE.                                                   
           05  WS-TAT-CD-MAX                  PIC S9(4) COMP  VALUE +10.        
           05  WS-TAT-CD-COUNT                PIC S9(4) COMP  VALUE   0.        
           05  FILLER                         OCCURS 10 TIMES.                  
               10  WS-TAT-ID                  PIC X(2).                         
               10  WS-TAT-DESC                PIC X(30).                        
                                                                                
      *****************************************************************         
      *** BUSINESS SEGMENT CODES AND DESCRIPTIONS                               
      *****************************************************************         
       01  WS-BS-CODE-TABLE.                                                    
           05  WS-BS-CD-MAX                   PIC S9(4) COMP  VALUE +10.        
           05  WS-BS-CD-COUNT                 PIC S9(4) COMP  VALUE   0.        
           05  FILLER                         OCCURS 10 TIMES.                  
               10  WS-BS-CD                   PIC X(1).                         
               10  WS-BS-DESC                 PIC X(30).                        
                                                                                
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-VARIABLES.                                                        
                                                                                
           05  SUB1                           PIC S9(4) COMP   VALUE 0.         
           05  SUB2                           PIC S9(4) COMP   VALUE 0.         
                                                                                
           05  WS-GROUP-ID                    PIC X(7).                         
           05  WS-GROUP-ID-NUM.                                                 
GNE****        10  FILLER                     PIC X.                            
GNE            10  WS-GROUP-NUMBER            PIC 9(7).                         
           05  FILLER                         REDEFINES                         
                                              WS-GROUP-ID-NUM.                  
GNE****        10  FILLER                     PIC X(2).                         
GNE            10  WS-GROUP-GIPSY             PIC 9(7).                         
                                                                                
           05  WS-TAT                         PIC X(2).                         
           05  WS-BUS-SEG-CD                  PIC X(1).                         
           05  WS-PREV-GROUP-ID               PIC X(7)  VALUE SPACES.           
                                                                                
           05  WS-TEST-MODE              PIC  X(02).                            
               88  WS-GIPSY-ACTIVE       VALUE '11' '12' '13' '14'              
                                               '15' '16' '17' '18'              
                                               '19' '20' '28'                   
                                               '23' '24' '25'.                  
               88  WS-GIPSY-TERMED       VALUE '21' '22' '26' '27'              
                                               '29' '30'.                       
           05  WS-NO-OF-LIVES                 PIC S9(7) COMP-3.                 
                                                                                
           05  WS-TS                          PIC X(26).                        
                                                                                
           05  WS-TIMESTAMP.                                                    
               10  WS-YEAR                    PIC X(4).                         
               10  FILLER                     PIC X(1).                         
               10  WS-MONTH                   PIC X(2).                         
               10  FILLER                     PIC X(1).                         
               10  WS-DAY                     PIC X(2).                         
               10  FILLER                     PIC X(16).                        
                                                                                
           05  WS-MONTH-NAMES                           VALUE                   
           'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'.                              
               10  WS-MONTH-NAME              PIC X(3)  OCCURS 12 TIMES.        
                                                                                
      * SWITCHES                                                                
                                                                                
           05  WS-EOF-SW                      PIC X.                            
               88  WS-EOF                     VALUE     'Y'.                    
               88  WS-EOF-NO                  VALUE     'N'.                    
                                                                                
           05  WS-GIPSY-GROUP-SW              PIC X.                            
               88  WS-GIPSY-GROUP             VALUE     'Y'.                    
               88  WS-GIPSY-GROUP-NO          VALUE     'N'.                    
                                                                                
      *****************************************************************         
      *** ACCUMULATORS                                                          
      *****************************************************************         
                                                                                
       01  WS-ACCUMULATORS.                                                     
           05  WS-ACCM-BS.                                                      
               10  WS-ACCM-BS-GROUP-CNT       PIC S9(7) COMP-3.                 
               10  WS-ACCM-BS-DIV-CNT         PIC S9(7) COMP-3.                 
               10  WS-ACCM-BS-AUTH-PMSS-CNT   PIC S9(7) COMP-3.                 
               10  WS-ACCM-BS-AUTH-PA-CNT     PIC S9(7) COMP-3.                 
               10  WS-ACCM-BS-AUTH-PM-CNT     PIC S9(7) COMP-3.                 
               10  WS-ACCM-BS-NO-LIVES        PIC S9(7) COMP-3.                 
           05  WS-ACCM-TAT.                                                     
               10  WS-ACCM-TAT-GROUP-CNT      PIC S9(7) COMP-3.                 
               10  WS-ACCM-TAT-DIV-CNT        PIC S9(7) COMP-3.                 
               10  WS-ACCM-TAT-AUTH-PMSS-CNT  PIC S9(7) COMP-3.                 
               10  WS-ACCM-TAT-AUTH-PA-CNT    PIC S9(7) COMP-3.                 
               10  WS-ACCM-TAT-AUTH-PM-CNT    PIC S9(7) COMP-3.                 
               10  WS-ACCM-TAT-NO-LIVES       PIC S9(7) COMP-3.                 
                                                                                
      *****************************************************************         
      *** REPORT LINES                                                          
      *****************************************************************         
      *                                                                         
       01  PRINT-RECORD.                                                        
           05  PRINT-CTL                      PIC X(1).                         
           05  PRINT-DATA                     PIC X(132).                       
                                                                                
       01  P-BLK-LINE.                                                          
           05  FILLER                         PIC X(1)  VALUE ' '.              
           05  FILLER                         PIC X(132) VALUE SPACES.          
       01  P-HDR-1.                                                             
           05  FILLER                         PIC X(1)  VALUE '1'.              
           05  FILLER                         PIC X(38) VALUE SPACES.           
           05  FILLER                         PIC X(54) VALUE                   
           'List of Active Group/Divisions with GB Internet Access'.            
           05  FILLER                         PIC X(20) VALUE SPACES.           
           05  FILLER                         PIC X(20) VALUE                   
           'Program:  GCCPFRMN'.                                                
                                                                                
       01  P-HDR-2.                                                             
           05  FILLER                         PIC X(1)  VALUE ' '.              
           05  FILLER                         PIC X(60) VALUE SPACES.           
           05  P-HDR-2-MONTH                  PIC X(3).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-HDR-2-DAY                    PIC X(02).                        
           05  FILLER                         PIC X(02) VALUE ', '.             
           05  P-HDR-2-YEAR                   PIC X(04).                        
           05  FILLER                         PIC X(60) VALUE SPACES.           
                                                                                
       01  P-HDR-3.                                                             
           05  FILLER                         PIC X     VALUE '-'.              
           05  FILLER                         PIC X(20) VALUE                   
           'Turnaround Time:'.                                                  
           05  P-HDR-3-TAT                    PIC X(30).                        
           05  FILLER                         PIC X(82) VALUE SPACES.           
                                                                                
       01  P-HDR-4.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(20) VALUE                   
           'Business Segment:'.                                                 
           05  P-HDR-4-BS                     PIC X(30).                        
           05  FILLER                         PIC X(82) VALUE SPACES.           
                                                                                
       01  P-HDR-5.                                                             
           05  FILLER                         PIC X     VALUE '-'.              
           05  FILLER                         PIC X(7)  VALUE                   
           'Group'.                                                             
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(8)  VALUE                   
           'Division'.                                                          
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(60) VALUE                   
           'Company'.                                                           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(37) VALUE                   
           '           Site Authority'.                                         
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           '# OF Lives-'.                                                       
           05  FILLER                         PIC X(1)  VALUE SPACES.           
                                                                                
       01  P-HDR-6.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(7)  VALUE                   
           'Number'.                                                            
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(8)  VALUE                   
           'Number'.                                                            
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(60) VALUE                   
           'Name'.                                                              
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(37) VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           '   GIPSY HO'.                                                       
           05  FILLER                         PIC X(1)  VALUE SPACES.           
                                                                                
       01  P-HDR-7.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(7)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(8)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(60) VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(37) VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           '       only'.                                                       
           05  FILLER                         PIC X(1)  VALUE SPACES.           
                                                                                
       01  P-HDR-8.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(7)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(8)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(60) VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(14) VALUE                   
           ' Plan Member'.                                                      
           05  FILLER                         PIC X(02) VALUE SPACES.           
           05  FILLER                         PIC X(06) VALUE                   
           ' Plan'.                                                             
           05  FILLER                         PIC X(02) VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE                   
           '    Plan'.                                                          
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
                                                                                
       01  P-HDR-9.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(7)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(8)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(60) VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(14) VALUE                   
           'Claim/Coverage'.                                                    
           05  FILLER                         PIC X(02) VALUE SPACES.           
           05  FILLER                         PIC X(06) VALUE                   
           'Member'.                                                            
           05  FILLER                         PIC X(02) VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE                   
           'Administrator'.                                                     
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
                                                                                
       01  P-HDR-10.                                                            
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(7)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(8)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(60) VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(14) VALUE SPACES.           
           05  FILLER                         PIC X(02) VALUE SPACES.           
           05  FILLER                         PIC X(06) VALUE                   
           'Enroll'.                                                            
           05  FILLER                         PIC X(02) VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
                                                                                
       01  P-HDR-11.                                                            
           05  FILLER                         PIC X     VALUE '0'.              
           05  FILLER                         PIC X(16) VALUE                   
           'Number of Groups'.                                                  
           05  FILLER                         PIC X(08) VALUE SPACES.           
           05  FILLER                         PIC X(19) VALUE                   
           'Number of Divisions'.                                               
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(48) VALUE                   
           '                 Site Authority'.                                   
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           '# OF Lives-'.                                                       
                                                                                
       01  P-HDR-12.                                                            
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(16) VALUE SPACES.           
           05  FILLER                         PIC X(10) VALUE SPACES.           
           05  FILLER                         PIC X(17) VALUE SPACES.           
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(48) VALUE SPACES.           
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           '   GIPSY HO'.                                                       
                                                                                
       01  P-HDR-13.                                                            
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(16) VALUE SPACES.           
           05  FILLER                         PIC X(10) VALUE SPACES.           
           05  FILLER                         PIC X(17) VALUE SPACES.           
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(48) VALUE SPACES.           
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           '       only'.                                                       
       01  P-HDR-14.                                                            
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(16) VALUE SPACES.           
           05  FILLER                         PIC X(10) VALUE SPACES.           
           05  FILLER                         PIC X(17) VALUE SPACES.           
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(14) VALUE                   
           '   Plan Member'.                                                    
           05  FILLER                         PIC X(05) VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           'Plan Member'.                                                       
           05  FILLER                         PIC X(05) VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE                   
           '         Plan'.                                                     
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE SPACES.           
                                                                                
       01  P-HDR-15.                                                            
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(16) VALUE SPACES.           
           05  FILLER                         PIC X(10) VALUE SPACES.           
           05  FILLER                         PIC X(17) VALUE SPACES.           
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(14) VALUE                   
           'Claim/Coverage'.                                                    
           05  FILLER                         PIC X(05) VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           '     Enroll'.                                                       
           05  FILLER                         PIC X(05) VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE                   
           'Administrator'.                                                     
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE SPACES.           
                                                                                
       01  P-HDR-16.                                                            
           05  FILLER                         PIC X     VALUE '1'.              
           05  FILLER                         PIC X(50) VALUE                   
           'Count Totals by Turnaround Period'.                                 
           05  FILLER                         PIC X(82) VALUE SPACES.           
                                                                                
       01  P-DETL.                                                              
           05  FILLER                         PIC X     VALUE ' '.              
           05  P-DETL-GROUP-ID                PIC X(7).                         
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(5)  VALUE SPACES.           
           05  P-DETL-DIV-ID                  PIC X(3).                         
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  P-DETL-SPONSOR-NAME            PIC X(60).                        
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(6)  VALUE SPACES.           
           05  P-DETL-AUTH-PMSS               PIC X(3).                         
           05  FILLER                         PIC X(5)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  P-DETL-AUTH-PA                 PIC X(3).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(5)  VALUE SPACES.           
           05  P-DETL-AUTH-PM                 PIC X(3).                         
           05  FILLER                         PIC X(5)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  P-DETL-LIVES                   PIC Z,ZZZ,ZZ9.                    
           05  P-DETL-LIVES-X                                                   
               REDEFINES P-DETL-LIVES         PIC X(9).                         
           05  FILLER                         PIC X     VALUE SPACES.           
                                                                                
       01  P-TOTAL-1.                                                           
           05  FILLER                         PIC X     VALUE '-'.              
           05  P-TOTAL-1-GROUP-CNT            PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-1-DIV-CNT              PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(60) VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(4)  VALUE SPACES.           
           05  P-TOTAL-1-AUTH-PMSS-CNT        PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(3)  VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-1-AUTH-PA-CNT          PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(3)  VALUE SPACES.           
           05  P-TOTAL-1-AUTH-PM-CNT          PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(3)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  P-TOTAL-1-LIVES                PIC Z,ZZZ,ZZ9.                    
           05  FILLER                         PIC X     VALUE SPACES.           
                                                                                
       01  P-TOTAL-2.                                                           
           05  FILLER                         PIC X     VALUE '-'.              
           05  FILLER                         PIC X(07) VALUE SPACES.           
           05  P-TOTAL-2-GROUP-CNT            PIC Z,ZZZ,ZZ9.                    
           05  FILLER                         PIC X(10) VALUE SPACES.           
           05  FILLER                         PIC X(08) VALUE SPACES.           
           05  P-TOTAL-2-DIV-CNT              PIC Z,ZZZ,ZZ9.                    
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  FILLER                         PIC X(05) VALUE SPACES.           
           05  P-TOTAL-2-AUTH-PMSS-CNT        PIC Z,ZZZ,ZZ9.                    
           05  FILLER                         PIC X(05) VALUE SPACES.           
           05  FILLER                         PIC X(02) VALUE SPACES.           
           05  P-TOTAL-2-AUTH-PA-CNT          PIC Z,ZZZ,ZZ9.                    
           05  FILLER                         PIC X(05) VALUE SPACES.           
           05  FILLER                         PIC X(04) VALUE SPACES.           
           05  P-TOTAL-2-AUTH-PM-CNT          PIC Z,ZZZ,ZZ9.                    
           05  FILLER                         PIC X(15) VALUE SPACES.           
           05  P-TOTAL-2-LIVES                PIC ZZZ,ZZZ,ZZ9.                  
                                                                                
       01  WS-LINE-COUNT                      PIC S9(4) COMP VALUE +35.         
       01  WS-LINE-MAX                        PIC S9(4) COMP VALUE +35.         
                                                                                
      *****************************************************************         
      *** CONSTANTS                                                             
      *****************************************************************         
      *                                                                         
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GAEDATSR                 PIC X(08)                            
                                          VALUE 'GAEDATSR'.                     
           05  WS-GACCDDET                 PIC X(08)                            
                                          VALUE 'GACCDDET'.                     
           05  WS-GCCPFRMO                 PIC X(08)                            
                                          VALUE 'GCCPFRMO'.                     
           05  WS-GAEDATSR-VERB            PIC X(16).                           
           05  WS-PRINT-LR                 PIC X(16)                            
                                          VALUE 'PRINT-DATA-022  '.             
                                                                                
      *****************************************************************         
      *** GIPSY DETAIL VSAM ACCESS PARAMETERS                                   
      *****************************************************************         
       01  GACCDDET-PARMS.                                                      
           05  GACCDDET-FUNCTION COMP    PIC S9(4).                             
           05  GACCDDET-KEY.                                                    
GNE            10  GACCDDET-GROU COMP-3  PIC S9(7) VALUE +0.                    
               10  GACCDDET-ACCT         PIC X(3)  VALUE LOW-VALUES.            
               10  GACCDDET-CERT COMP-3  PIC S9(9) VALUE +0.                    
           05  GACCDDET-WORK             PIC X(9)  VALUE SPACES.                
      *                                                                         
                                                                                
       01  GACCDDET-ERROR-CODE           PIC X(3).                              
           88  GACCDDET-OK                         VALUE SPACES.                
           88  GACCDDET-EOF                        VALUE 'R01'.                 
           88  GACCDDET-NOT-FOUND                  VALUE 'R01'.                 
           88  GACCDDET-DISK-ERROR                 VALUE 'R03'.                 
           88  GACCDDET-DUP-RECORD                 VALUE 'R08'.                 
           88  GACCDDET-FUNC-INVALID               VALUE 'R09'.                 
                                                                                
      *****************************************************************         
      *** GIPSY DETAIL VSAM RECORD.                                             
      *****************************************************************         
                                                                                
       01  FILLER                        PIC X(20) VALUE                        
GNE        'GDETEXP7  STARTS*'.                                                 
      *01  GIPSY-DETAIL-RECORD.                                                 
GNE        COPY GDETEXP7.                                                       
                                                                                
      ******************************************************************        
      *    CL2 INTERFACE CONTRACT RECORD                                        
      ******************************************************************        
                                                                                
       01  IGRP-RECORD.                                                         
         03  IGRP-AREA.                                                         
             05  FILLER                  PIC X(04).                             
         03  IGRP-REC.                                                          
             COPY XC4CFIGP.                                                     
                                                                                
       01  WS-PGM-XC4SORIG               PIC X(8) VALUE 'XC4SORIG'.             
       01  XC4CORIG.                                                            
           COPY XC4CORIG.                                                       
                                                                                
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME              PIC X(60) VALUE                      
                'GCCPFRMN - PRINT TRANSACTION SUBMISSION TRACKING'.             
           10  AB-PARAGRAPH-NAME  OCCURS 25 TIMES                               
                                         PIC X(60).                             
           10  AB-MESSAGE.                                                      
               15  AB-MSG1               PIC X(70).                             
               15  AB-MSG2               PIC X(70).                             
           10  AB-SQLCODE                PIC ----9.                             
           10  LVL                       PIC S9(4) COMP.                        
           10  CNT                       PIC S9(4) COMP.                        
                                                                                
       01  LOGICAL-RECORD-NAMES.                                                
                                         COPY HCSLRNAM.                         
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
      ****************************************************************          
      *** DB2 CURSOR DECLARES                                                   
      ****************************************************************          
                                                                                
           EXEC SQL                                                             
             DECLARE TAT_NAMES CURSOR FOR                                       
             SELECT CODE_VALUE                                                  
                   ,CODE_ENG_DESC                                               
             FROM   TCTTAT                                                      
             ORDER  BY CODE_VALUE                                               
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             DECLARE BS_NAMES CURSOR FOR                                        
             SELECT CODE_VALUE                                                  
                   ,CODE_ENG_DESC                                               
             FROM   TCTBS                                                       
             ORDER  BY CODE_VALUE                                               
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             DECLARE CURR_DATE CURSOR FOR                                       
             SELECT  CURRENT TIMESTAMP                                          
             FROM    TGD                                                        
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             DECLARE GROUP_LOOP CURSOR FOR                                      
             SELECT  A.GROUP_ID                                                 
                    ,A.DIV_ID                                                   
                    ,A.SPONSOR_NAME                                             
                    ,A.AUTH_PMSS_IND                                            
                    ,A.AUTH_PA_IND                                              
                    ,A.AUTH_PM_ENROL_IND                                        
             FROM    TGD                 A                                      
             WHERE   A.TAT               = :WS-TAT                              
             AND     A.BUS_SEG_CD        = :WS-BUS-SEG-CD                       
             ORDER   BY A.GROUP_ID, A.DIV_ID                                    
           END-EXEC.                                                            
                                                                                
      ****************************************************************          
      *********   P R O C E D U R E   D I V I S I O N   **************          
      ****************************************************************          
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           MOVE 1 TO LVL.                                                       
           MOVE '0000-MAINLINE' TO AB-PARAGRAPH-NAME (LVL).                     
                                                                                
           PERFORM 1000-INITIALIZATION                                          
              THRU 1000-EXIT.                                                   
                                                                                
           PERFORM 2000-PROCESS-REPORT-BY-TAT                                   
              THRU 2000-EXIT                                                    
           VARYING SUB1 FROM 1 BY 1                                             
             UNTIL SUB1 > WS-TAT-CD-COUNT.                                      
                                                                                
           PERFORM 3000-FINISH                                                  
              THRU 3000-EXIT.                                                   
                                                                                
           GOBACK.                                                              
                                                                                
       1000-INITIALIZATION.                                                     
      *****************************************************************         
      *  - INITIALIZE DATA SERVER AREA                                          
      *  - BUILD TAT TABLE                                                      
      *  - BUILD BUSINESS SEGMENT TABLE                                         
      *  - GET DATE FOR REPORT HEADING                                          
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE 'GCCPFRMN'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
           SET ORIG-OPEN TO TRUE.                                               
           CALL WS-PGM-XC4SORIG USING XC4CORIG.                                 
                                                                                
      * GET TAT CODES AND DESCRIPTIONS FROM THE TCTTAT TABLE                    
                                                                                
           PERFORM 1010-GET-TAT-CODES                                           
              THRU 1010-EXIT                                                    
                                                                                
      * GET BUSINESS SEGMENT CODES AND DESCRIPTIONS FORM THE                    
      * TCTBS TABLE                                                             
                                                                                
           PERFORM 1020-GET-BS-CODES                                            
              THRU 1020-EXIT.                                                   
                                                                                
      * GET TODAY'S DATE FOR REPORT HEADING                                     
                                                                                
           EXEC SQL                                                             
             OPEN CURR_DATE                                                     
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             FETCH CURR_DATE                                                    
             INTO  :WS-TS                                                       
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH DATE ENTRIES'     TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           MOVE WS-TS                    TO WS-TIMESTAMP.                       
                                                                                
           EXEC SQL                                                             
             CLOSE CURR_DATE                                                    
           END-EXEC.                                                            
                                                                                
      * MOVE CURRENT DATE TO HEADING                                            
                                                                                
           MOVE WS-MONTH                 TO SUB1.                               
           MOVE WS-MONTH-NAME (SUB1)     TO P-HDR-2-MONTH.                      
           MOVE WS-DAY                   TO P-HDR-2-DAY.                        
           MOVE WS-YEAR                  TO P-HDR-2-YEAR.                       
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1010-GET-TAT-CODES.                                                      
      *****************************************************************         
      * SELECT TAT CODES AND DESCRIPTIONS FROM THE TCTTAT TABLE                 
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1010-GET-TAT-CODES'     TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE 0                        TO SUB1.                               
                                                                                
           EXEC SQL                                                             
             OPEN TAT_NAMES                                                     
           END-EXEC.                                                            
                                                                                
           PERFORM                                                              
               WITH TEST BEFORE                                                 
               UNTIL SQLCODE = +100                                             
               OR    SUB1    = WS-TAT-CD-MAX                                    
                                                                                
               EXEC SQL                                                         
                 FETCH TAT_NAMES                                                
                 INTO  :DCLTCTTAT.CODE-VALUE                                    
                      ,:DCLTCTTAT.CODE-ENG-DESC                                 
               END-EXEC                                                         
                                                                                
               MOVE 'FETCH TAT NAMES'    TO AB-MESSAGE                          
               PERFORM 8900-CHECK-SQL-CODE                                      
                  THRU 8900-EXIT                                                
                                                                                
               IF SQLCODE = 0                                                   
                   ADD 1                 TO SUB1                                
                   MOVE CODE-VALUE       OF DCLTCTTAT                           
                                         TO WS-TAT-ID (SUB1)                    
                   MOVE CODE-ENG-DESC    OF DCLTCTTAT                           
                                         TO WS-TAT-DESC (SUB1)                  
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
             CLOSE TAT_NAMES                                                    
           END-EXEC                                                             
                                                                                
           MOVE SUB1                     TO WS-TAT-CD-COUNT.                    
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1010-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1020-GET-BS-CODES.                                                       
      *****************************************************************         
      * SELECT THE BUSINESS SEGMENT CODES AND DESCRIPTIONS FROM THE             
      * TCTBS TABLE                                                             
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1020-GET-BS-CODES'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE 0                        TO SUB1.                               
                                                                                
           EXEC SQL                                                             
             OPEN BS_NAMES                                                      
           END-EXEC.                                                            
                                                                                
           PERFORM                                                              
               WITH TEST BEFORE                                                 
               UNTIL SQLCODE = +100                                             
               OR    SUB1    = WS-BS-CD-MAX                                     
                                                                                
               EXEC SQL                                                         
                 FETCH BS_NAMES                                                 
                  INTO  :DCLTCTBS.CODE-VALUE                                    
                       ,:DCLTCTBS.CODE-ENG-DESC                                 
               END-EXEC                                                         
                                                                                
               MOVE 'FETCH BS NAMES'     TO AB-MESSAGE                          
               PERFORM 8900-CHECK-SQL-CODE                                      
                  THRU 8900-EXIT                                                
                                                                                
               IF SQLCODE = 0                                                   
                   ADD 1                 TO SUB1                                
                   MOVE CODE-VALUE       OF DCLTCTBS                            
                                         TO WS-BS-CD (SUB1)                     
                   MOVE CODE-ENG-DESC    OF DCLTCTBS                            
                                         TO WS-BS-DESC (SUB1)                   
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
             CLOSE BS_NAMES                                                     
           END-EXEC.                                                            
                                                                                
           MOVE SUB1                     TO WS-BS-CD-COUNT.                     
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1020-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-PROCESS-REPORT-BY-TAT.                                              
      *****************************************************************         
      *  - PROCESS REPORT FOR EACH TAT                                          
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS-REPORT-BY-TAT'                                    
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-TAT-ID (SUB1)         TO WS-TAT.                             
           MOVE WS-TAT-DESC (SUB1)       TO P-HDR-3-TAT.                        
                                                                                
           MOVE 0                        TO WS-ACCM-TAT-GROUP-CNT               
                                            WS-ACCM-TAT-DIV-CNT                 
                                            WS-ACCM-TAT-AUTH-PMSS-CNT           
                                            WS-ACCM-TAT-AUTH-PA-CNT             
                                            WS-ACCM-TAT-AUTH-PM-CNT             
                                            WS-ACCM-TAT-NO-LIVES.               
                                                                                
           PERFORM 2010-PROCESS-REPORT-BY-BS                                    
              THRU 2010-EXIT                                                    
           VARYING SUB2 FROM 1 BY 1                                             
             UNTIL SUB2 > WS-BS-CD-COUNT.                                       
                                                                                
           PERFORM 5200-PRT-TAT-TOTALS                                          
              THRU 5200-EXIT.                                                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2010-PROCESS-REPORT-BY-BS.                                               
      *****************************************************************         
      *  - PROCESS REPORT FOR EACH BS                                           
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2010-PROCESS-REPORT-BY-BS'                                     
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-BS-CD (SUB2)          TO WS-BUS-SEG-CD.                      
           MOVE WS-BS-DESC (SUB2)        TO P-HDR-4-BS.                         
                                                                                
           MOVE 0                        TO WS-ACCM-BS-GROUP-CNT                
                                            WS-ACCM-BS-DIV-CNT                  
                                            WS-ACCM-BS-AUTH-PMSS-CNT            
                                            WS-ACCM-BS-AUTH-PA-CNT              
                                            WS-ACCM-BS-AUTH-PM-CNT              
                                            WS-ACCM-BS-NO-LIVES.                
                                                                                
           EXEC SQL                                                             
             OPEN GROUP_LOOP                                                    
           END-EXEC.                                                            
                                                                                
           SET WS-EOF-NO                 TO TRUE.                               
                                                                                
           PERFORM 2020-PROCESS-REPORT-BY-GROUP                                 
              THRU 2020-EXIT                                                    
             UNTIL WS-EOF.                                                      
                                                                                
           EXEC SQL                                                             
             CLOSE GROUP_LOOP                                                   
           END-EXEC.                                                            
                                                                                
           PERFORM 5100-PRT-BS-TOTALS                                           
              THRU 5100-EXIT.                                                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2010-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2020-PROCESS-REPORT-BY-GROUP.                                            
      *****************************************************************         
      *  - PROCESS ONE LINE OF REPORT FOR EACH GROUP/DIV                        
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2020-PROCESS-INFORCE-BY-GROUP'                                 
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             FETCH GROUP_LOOP                                                   
             INTO  :DCLTGD.GROUP-ID                                             
                  ,:DCLTGD.DIV-ID                                               
                  ,:DCLTGD.SPONSOR-NAME                                         
                  ,:DCLTGD.AUTH-PMSS-IND                                        
                  ,:DCLTGD.AUTH-PA-IND                                          
                  ,:DCLTGD.AUTH-PM-ENROL-IND                                    
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH GROUP LOOP'       TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           IF SQLCODE = +100                                                    
               SET WS-EOF                TO TRUE                                
           ELSE                                                                 
               PERFORM 2030-PROCESS-ENTRY                                       
                  THRU 2030-EXIT                                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2020-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2030-PROCESS-ENTRY.                                                      
      *****************************************************************         
      * MOVE ENTRY TO PRINT LINE                                                
      * FOR GIPSY GROUP FIND NO. OF LIVES                                       
      * ACCUMULATE BUSINESS SEGMENT TOTALS                                      
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2030-PROCESS-ENTRY'    TO AB-PARAGRAPH-NAME (LVL).             
                                                                                
           IF GROUP-ID       OF DCLTGD   =  WS-PREV-GROUP-ID                    
               MOVE SPACES               TO P-DETL-GROUP-ID                     
           ELSE                                                                 
               MOVE GROUP-ID OF DCLTGD   TO P-DETL-GROUP-ID                     
               MOVE GROUP-ID OF DCLTGD   TO WS-PREV-GROUP-ID                    
               ADD 1                     TO WS-ACCM-BS-GROUP-CNT                
           END-IF.                                                              
                                                                                
           MOVE DIV-ID       OF DCLTGD   TO P-DETL-DIV-ID.                      
           ADD 1                         TO WS-ACCM-BS-DIV-CNT.                 
           MOVE SPONSOR-NAME OF DCLTGD   TO P-DETL-SPONSOR-NAME.                
                                                                                
           IF AUTH-PMSS-IND  OF DCLTGD   =  'Y'                                 
               ADD 1                     TO WS-ACCM-BS-AUTH-PMSS-CNT            
               MOVE 'Yes'                TO P-DETL-AUTH-PMSS                    
           ELSE                                                                 
               MOVE 'No'                 TO P-DETL-AUTH-PMSS                    
           END-IF.                                                              
                                                                                
           IF AUTH-PA-IND    OF DCLTGD   =  'Y'                                 
               ADD 1                     TO WS-ACCM-BS-AUTH-PA-CNT              
               MOVE 'Yes'                TO P-DETL-AUTH-PA                      
           ELSE                                                                 
               MOVE 'No'                 TO P-DETL-AUTH-PA                      
           END-IF.                                                              
                                                                                
           IF AUTH-PM-ENROL-IND                                                 
                             OF DCLTGD   =  'Y'                                 
               ADD 1                     TO WS-ACCM-BS-AUTH-PM-CNT              
               MOVE 'Yes'                TO P-DETL-AUTH-PM                      
           ELSE                                                                 
               MOVE 'No'                 TO P-DETL-AUTH-PM                      
           END-IF.                                                              
                                                                                
      * DETERMINE IS GROUP IS GIPSY OF GFM                                      
                                                                                
           PERFORM 7100-READ-INTERFACE                                          
              THRU 7100-EXIT.                                                   
                                                                                
           IF WS-GIPSY-GROUP                                                    
               MOVE WS-NO-OF-LIVES       TO P-DETL-LIVES                        
               ADD WS-NO-OF-LIVES        TO WS-ACCM-BS-NO-LIVES                 
           ELSE                                                                 
               MOVE SPACES               TO P-DETL-LIVES-X                      
           END-IF.                                                              
                                                                                
           IF WS-LINE-COUNT              =  WS-LINE-MAX                         
               PERFORM 5000-PRT-HEADINGS                                        
                  THRU 5000-EXIT                                                
               MOVE GROUP-ID OF DCLTGD   TO P-DETL-GROUP-ID                     
           END-IF.                                                              
                                                                                
           MOVE P-DETL                   TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
           ADD 1                         TO WS-LINE-COUNT.                      
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2030-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3000-FINISH.                                                             
      *****************************************************************         
      *  - CLOSE FILES                                                          
      *****************************************************************         
                                                                                
           SET ORIG-close TO TRUE.                                              
           CALL WS-PGM-XC4SORIG USING XC4CORIG.                                 
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '3000-FINISH'           TO AB-PARAGRAPH-NAME (LVL).             
                                                                                
      * CLOSE GIPSY DETAIL                                                      
                                                                                
           MOVE +1                      TO GACCDDET-FUNCTION.                   
                                                                                
           CALL WS-GCCPFRMO       USING GACCDDET-PARMS                          
                                        GACCDDET-ERROR-CODE                     
GNE                                     GDETEXP7                                
           END-CALL.                                                            
                                                                                
           MOVE FINISH-LR               TO WS-GAEDATSR-VERB.                    
                                                                                
           CALL WS-GAEDATSR             USING WS-GAEDATSR-VERB                  
                                              LOGICAL-RECORD-NAME               
                                              ICBM.                             
                                                                                
           IF NOT LR-STATUS-OK                                                  
               DISPLAY 'ERROR IN CLOSING FILES'                                 
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       5000-PRT-HEADINGS.                                                       
      *****************************************************************         
      * PRINT PAGE HEADINGS                                                     
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '5000-PRT-HEADINGS'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE P-HDR-1                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-2                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-3                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-4                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-5                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-6                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-7                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-8                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-9                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-10                 TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-BLK-LINE               TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE 0                        TO WS-LINE-COUNT.                      
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       5000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       5100-PRT-BS-TOTALS.                                                      
      *****************************************************************         
      *  - PRINT TOTALS AT THE END OF EACH BUSINESS SEGMENT                     
      *  - ACCUMULATE TAT TOTALS                                                
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '5100-PRT-BS-TOTALS' TO AB-PARAGRAPH-NAME (LVL).                
                                                                                
           MOVE WS-ACCM-BS-GROUP-CNT     TO P-TOTAL-1-GROUP-CNT.                
           MOVE WS-ACCM-BS-DIV-CNT       TO P-TOTAL-1-DIV-CNT.                  
           MOVE WS-ACCM-BS-AUTH-PMSS-CNT TO P-TOTAL-1-AUTH-PMSS-CNT.            
           MOVE WS-ACCM-BS-AUTH-PA-CNT   TO P-TOTAL-1-AUTH-PA-CNT.              
           MOVE WS-ACCM-BS-AUTH-PM-CNT   TO P-TOTAL-1-AUTH-PM-CNT.              
           MOVE WS-ACCM-BS-NO-LIVES      TO P-TOTAL-1-LIVES.                    
                                                                                
           MOVE P-TOTAL-1                TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE WS-LINE-MAX              TO WS-LINE-COUNT.                      
                                                                                
           ADD WS-ACCM-BS-GROUP-CNT      TO WS-ACCM-TAT-GROUP-CNT.              
           ADD WS-ACCM-BS-DIV-CNT        TO WS-ACCM-TAT-DIV-CNT.                
           ADD WS-ACCM-BS-AUTH-PMSS-CNT  TO WS-ACCM-TAT-AUTH-PMSS-CNT.          
           ADD WS-ACCM-BS-AUTH-PA-CNT    TO WS-ACCM-TAT-AUTH-PA-CNT.            
           ADD WS-ACCM-BS-AUTH-PM-CNT    TO WS-ACCM-TAT-AUTH-PM-CNT.            
           ADD WS-ACCM-BS-NO-LIVES       TO WS-ACCM-TAT-NO-LIVES.               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       5100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       5200-PRT-TAT-TOTALS.                                                     
      *****************************************************************         
      *  - PRINT TOTALS AT THE END OF EACH TAT                                  
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '5200-PRT-TAT-TOTALS' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE P-HDR-16                 TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-3                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-11                 TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-12                 TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-13                 TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-14                 TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE P-HDR-15                 TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE WS-ACCM-TAT-GROUP-CNT    TO P-TOTAL-2-GROUP-CNT.                
           MOVE WS-ACCM-TAT-DIV-CNT      TO P-TOTAL-2-DIV-CNT.                  
           MOVE WS-ACCM-TAT-AUTH-PMSS-CNT TO P-TOTAL-2-AUTH-PMSS-CNT.           
           MOVE WS-ACCM-TAT-AUTH-PA-CNT  TO P-TOTAL-2-AUTH-PA-CNT.              
           MOVE WS-ACCM-TAT-AUTH-PM-CNT  TO P-TOTAL-2-AUTH-PM-CNT.              
           MOVE WS-ACCM-TAT-NO-LIVES     TO P-TOTAL-2-LIVES.                    
                                                                                
           MOVE P-TOTAL-2                TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE WS-LINE-MAX              TO WS-LINE-COUNT.                      
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       5200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       7000-PRINT-OUTPUT.                                                       
      *****************************************************************         
      * CALL DATA SERVER TO PRINT REPORT                                        
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7000-PRINT-OUTPUT'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-PRINT-LR              TO LOGICAL-RECORD-NAME.                
           MOVE STORE-LR                 TO WS-GAEDATSR-VERB.                   
                                                                                
           CALL WS-GAEDATSR              USING WS-GAEDATSR-VERB                 
                                               PRINT-RECORD                     
                                               ICBM.                            
                                                                                
           IF NOT LR-STATUS-OK                                                  
               DISPLAY 'ERROR WRITING TO PRINT FILE'                            
               DISPLAY PRINT-RECORD                                             
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       7000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       7100-READ-INTERFACE.                                             25700000
      *****************************************************************         
      * IS THE CONTRACT GIPSY OR GFM                                            
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7100-READ-INTERFACE'    TO AB-PARAGRAPH-NAME(LVL).             
                                                                                
           MOVE GROUP-ID OF DCLTGD       TO WS-GROUP-ID.                        
                                                                                
           INSPECT WS-GROUP-ID REPLACING ALL ' ' BY '0'.                        
                                                                                
           IF WS-GROUP-ID                NOT NUMERIC                            
               SET WS-GIPSY-GROUP-NO     TO TRUE                                
           ELSE                                                                 
               SET ORIG-READ-KEYED       TO TRUE                                
               MOVE WS-GROUP-ID          TO WS-GROUP-ID-NUM                     
               MOVE WS-GROUP-NUMBER      TO ORIG-INPUT-CONTRACT                 
               MOVE SPACES               TO ORIG-OUTPUT-FIELDS                  
               CALL WS-PGM-XC4SORIG   USING XC4CORIG                            
               IF ORIG-SUCCESSFUL                                               
                  IF ORIG-ADMIN-GPY                                             
                     MOVE 0                     TO WS-NO-OF-LIVES               
                     SET WS-GIPSY-GROUP         TO TRUE                         
                     MOVE WS-GROUP-GIPSY        TO GACCDDET-GROU                
                     MOVE DIV-ID OF DCLTGD      TO GACCDDET-ACCT                
                     MOVE ZEROES                TO GACCDDET-CERT                
                     MOVE LOW-VALUES            TO GACCDDET-WORK                
                     MOVE +0004                 TO GACCDDET-FUNCTION            
                     MOVE SPACES                TO GACCDDET-ERROR-CODE          
                                                                                
                     PERFORM 7200-READ-GISPY-DETAIL                             
                        THRU 7200-EXIT                                          
                       UNTIL GACCDDET-ERROR-CODE NOT = SPACES                   
                  ELSE                                                          
                     SET WS-GIPSY-GROUP-NO      TO TRUE                         
                  END-IF                                                        
               ELSE                                                             
                  IF ORIG-NOT-FOUND                                             
                     SET WS-GIPSY-GROUP-NO      TO TRUE                         
                  ELSE                                                          
                     DISPLAY 'ERROR IN CALLING PGM XC4SORIG'                    
                     DISPLAY ORIG-RETURN-CODE                                   
                     PERFORM 9999-ABEND                                         
                        THRU 9999-EXIT                                          
                  END-IF                                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       7100-EXIT.                                                       25980000
           EXIT.                                                        25990000
                                                                                
       7200-READ-GISPY-DETAIL.                                                  
      *****************************************************************         
      * BROWSE THE DETAIL FILE AND LOOK FOR ANY RECORD FOR THIS                 
      * GROUP/DIV                                                               
      *****************************************************************         
                                                                                
GNE        INITIALIZE                    GDETEXP7.                              
           MOVE SPACES                   TO GACCDDET-ERROR-CODE.                
                                                                                
           CALL WS-GCCPFRMO       USING  GACCDDET-PARMS                         
                                         GACCDDET-ERROR-CODE                    
GNE                                      GDETEXP7                               
           END-CALL.                                                            
                                                                                
           IF GACCDDET-OK                                                       
      * VALIDATE TRAILER NO                                                     
               IF GDET-NO-TRLR >  0 AND                                         
                  GDET-NO-TRLR <= 30                                            
                   MOVE GDET-MODE (GDET-NO-TRLR)                                
                                         TO WS-TEST-MODE                        
                   IF WS-GIPSY-ACTIVE                                           
                       ADD 1             TO WS-NO-OF-LIVES                      
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
                                                                                
       7200-EXIT.                                                               
           EXIT.                                                                
                                                               EJECT            
       8900-CHECK-SQL-CODE.                                                     
      *****************************************************************         
      * THIS PARAGRAPH CHECKS THE SQL CODE AFTER A DB2 CALL AND HANDLES         
      * ANY ERRORS DETECTED.                                                    
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '8900-CHECK-SQL-CODE'    TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EVALUATE SQLCODE                                                     
                                                                                
              WHEN ZERO                                                         
                  CONTINUE                                                      
              WHEN +100                                                         
                  CONTINUE                                                      
              WHEN OTHER                                                        
                  MOVE SQLCODE           TO AB-SQLCODE                          
                  MOVE SQLERRMC          TO AB-MSG2                             
                  INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                       
                  PERFORM 9999-ABEND                                            
                                                                                
           END-EVALUATE.                                                        
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       8900-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9999-ABEND.                                                              
      *****************************************************************         
      * THIS PARAGRAPH IS CALLED IF AN EXCEPTIONAL CONDITION, WHICH             
      * CANNOT ALLOW THE PROGRAM TO CONTINUE NORMALLY, IS FOUND.                
      * MESSAGES GIVING DETAILS OF THE ABEND ARE DISPLAYEDAND THE               
      * AND THE PROGRAM WILL TERMINATE WITH A RETURN CODE OF 16.                
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '9999-ABEND' TO AB-PARAGRAPH-NAME (LVL).                        
                                                                                
      ***  DISPLAY ABEND MESSAGE                                                
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '*************************************************'.         
           DISPLAY '***** P R O G R A M   T E R M I N A T E D   *****'.         
           DISPLAY '*****          A B N O R M A L L Y          *****'.         
           DISPLAY '*************************************************'.         
           DISPLAY ' '.                                                         
           DISPLAY 'MODULE NAME    : ' AB-MODULE-NAME.                          
           DISPLAY 'PARAGRAPH NAME : ' AB-PARAGRAPH-NAME (LVL).                 
                                                                                
           IF AB-SQLCODE NOT = ZERO                                             
              DISPLAY ' '                                                       
              DISPLAY 'SQLCODE:  ' AB-SQLCODE                                   
           END-IF.                                                              
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY AB-MSG1.                                                     
           DISPLAY AB-MSG2.                                                     
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '********     P R O G R A M   F L O W     ********'.         
           DISPLAY ' '.                                                         
           PERFORM                                                              
              WITH TEST BEFORE                                                  
              VARYING CNT FROM 1 BY 1                                           
              UNTIL CNT > LVL                                                   
                                                                                
              DISPLAY AB-PARAGRAPH-NAME (CNT)                                   
                                                                                
           END-PERFORM.                                                         
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                        12370000
           MOVE +16 TO RETURN-CODE.                                     12380000
                                                                        12370000
           GOBACK.                                                              
                                                                                
       9999-EXIT.                                                               
           EXIT.                                                                
                                                                                
MAN07 *9999-DUMP-ROUTINE.                                                       
MAN07 ******************************************************************        
MAN07 *    SERIOUS ERROR OCCURRED.                                              
MAN07 *    DISPLAY SOME CONSOLE MESSAGES, TAKE A DUMP THEN ABEND.               
MAN07 *    COPYBOOK GWRDUMP IS REQUIRED                                         
MAN07 ******************************************************************        
MAN07 *                                                                         
MAN07 *    MOVE WS-PROGRAM-ID TO GWRDUMP-PROGRAM-NAME.                          
MAN07 *    CALL GWCDUMP    USING ICBM                                           
MAN07 *                          GWRDUMP.                                       
MAN07 *                                                                         
MAN07 *9999-EXIT.                                                               
MAN07 *    EXIT.                                                                
