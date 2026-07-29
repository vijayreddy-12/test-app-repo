       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRMK.                                                 
      *AUTHOR.        J. ELKINS.                                                
      *DATE-WRITTEN.  AUG 01, 2001.                                             
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS - E-COMMERCE                                          
      *   GCCPFRMK - PRINT THE PLAN MEMBER REGISTRATION/LOGIN ACTIVITY          
      *              REPORT                                                     
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *                                                                         
      *   THIS PROGRAM PRINTS A REPORT OF THE NUMBER OF REGISTRATION,           
      *   SUCCESSFUL AND UNSUCCESSFUL LOGINS FOR GROUPS FOR EACH                
      *   BUSINESS SEGMENT AND TURNAROUND TIME.                                 
      *                                                                         
      *                                                                         
      *   INPUT FILES:  NONE                                                    
      *                                                                         
      *   OUTPUT FILES: PLAN MEMBER REGISTRATION/LOGIN ACTIVITY                 
      *                 REPORT                                                  
      *                                                                         
      *   DB2 TABLES                                                            
      *   ACCESSED:     TGCT    - GROUP CERTIFICATE     - SELECT                
      *                 TGD     - GROUP DIVISION        - SELECT                
      *                 TEH     - EVENT HISTORY         - SELECT                
      *                 TLH     - LOGIN HISTORY         - SELECT                
      *                 TCTBS   - BUSINESS SEGMENT CODE - SELECT                
      *                 TCTTAT  - TURNAROUND TIME       - SELECT                
      *                                                                         
      *   CALLS:        GAEDATSR- FILE I/O                                      
      *                                                                         
      *   INCLUDE CODE: SQLCA    - SQL COMMUNICATION AREA                       
      *                                                                         
      *                 TGCTD    - TGCT    TABLE DECLARATION                    
      *                 TGDD     - TGD     TABLE DECLARATION                    
      *                 TEHD     - TEH     TABLE DECLARATION                    
      *                 TLHD     - TLH     TABLE DECLARATION                    
      *                 TCTBSD   - TCTBS   TABLE DECLARATION                    
      *                 TCTTATD  - TCTTAT  TABLE DECLARATION                    
      *                                                                         
      *                 TGCT     - TGCT    HOST VARIABLES                       
      *                 TGD      - TGD     HOST VARIABLES                       
      *                 TEH      - TEH     HOST VARIABLES                       
      *                 TLH      - TLH     HOST VARIABLES                       
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
      * PROGRAMMER   º  DATE  º             CHANGE                              
      *    NAME      ºDD/MM/YYº           DESCRIPTION                           
      *--------------+--------+-----------------------------------------        
      * J. ELKINS    º01/08/01º ORIGINAL CODE                                   
+R45WR* W. KURZATZ   º04/03/03º CHANGED 2000-GET-REG-COUNT TO USE PMSSRG        
+R45WR*              º        º INSTEAD OF PMSSPM                               
CC    * C. CHURCHILL º29/12/03º CHANGED 2000-GET-REG-COUNT TO USE JUST          
CC    *              |        | ONE OF THE TGCT RECS FOR EACH TCUST             
      * IBM GR       º26/08/08º UPGRADED IN ECU PROJECT                         
      *--------------+--------+-----------------------------------------        
                                                                                
      /                                                                         
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
                                                                                
           EXEC SQL INCLUDE SQLCA   END-EXEC.                                   
                                                                                
      *** DB2 TABLE DECLARATIONS                                                
                                                                                
           EXEC SQL INCLUDE TGCTD   END-EXEC.                                   
           EXEC SQL INCLUDE TGDD    END-EXEC.                                   
           EXEC SQL INCLUDE TEHD    END-EXEC.                                   
           EXEC SQL INCLUDE TLHD    END-EXEC.                                   
           EXEC SQL INCLUDE TCTBSD  END-EXEC.                                   
           EXEC SQL INCLUDE TCTTATD END-EXEC.                                   
                                                                                
      *** DB2 HOST & INDICATOR VARIABLES                                        
                                                                                
      *01  DCLTGCT.                                                             
           EXEC SQL INCLUDE TGCT    END-EXEC.                                   
                                                                                
      *01  DCLTGC.                                                              
           EXEC SQL INCLUDE TGD     END-EXEC.                                   
                                                                                
      *01  DCLTEH.                                                              
           EXEC SQL INCLUDE TEH     END-EXEC.                                   
                                                                                
      *01  DCLTLH.                                                              
           EXEC SQL INCLUDE TLH     END-EXEC.                                   
                                                                                
      *01  DCLTCTTAT.                                                           
           EXEC SQL INCLUDE TCTTAT  END-EXEC.                                   
                                                                                
      *01  DCLTCTBS.                                                            
           EXEC SQL INCLUDE TCTBS   END-EXEC.                                   
                                                                                
                                                                                
                                                                                
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                          
                                                                                
                                                                                
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
      * CONTROL CARD                                                            
      *****************************************************************         
       01  GCCCCRDK-RECORD.                                                     
           COPY GCCCCRDK.                                                       
                                                                                
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-VARIABLES.                                                        
                                                                                
           05  SUB1                           PIC S9(4) COMP   VALUE 0.         
           05  SUB2                           PIC S9(4) COMP   VALUE 0.         
                                                                                
           05  WS-WORK-YEAR                   PIC S9(5) COMP-3.                 
           05  WS-WORK-REMAINDER              PIC S9(5) COMP-3.                 
           05  WS-WORK-FIELD                  PIC S9(5) COMP-3.                 
           05  WS-WORK-DAY                    PIC X(2).                         
                                                                                
           05  WS-FROM-TS                     PIC X(26).                        
           05  WS-TO-TS                       PIC X(26).                        
                                                                                
           05  WS-MTH1-FROM-TS.                                                 
               10  WS-MTH1-FROM-YEAR          PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH1-FROM-MONTH         PIC X(2).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH1-FROM-DAY           PIC X(2)  VALUE '01'.             
               10  FILLER                     PIC X(16) VALUE                   
           '-00.00.00.000000'.                                                  
                                                                                
           05  WS-MTH1-TO-TS.                                                   
               10  WS-MTH1-TO-YEAR            PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH1-TO-MONTH           PIC X(2).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH1-TO-DAY             PIC X(2).                         
               10  FILLER                     PIC X(16) VALUE                   
           '-23.59.59.999999'.                                                  
                                                                                
           05  WS-MTH2-FROM-TS.                                                 
               10  WS-MTH2-FROM-YEAR          PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH2-FROM-MONTH         PIC X(2).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH2-FROM-DAY           PIC X(2)  VALUE '01'.             
               10  FILLER                     PIC X(16) VALUE                   
           '-00.00.00.000000'.                                                  
                                                                                
           05  WS-MTH2-TO-TS.                                                   
               10  WS-MTH2-TO-YEAR            PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH2-TO-MONTH           PIC X(2).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH2-TO-DAY             PIC X(2).                         
               10  FILLER                     PIC X(16) VALUE                   
           '-23.59.59.999999'.                                                  
                                                                                
           05  WS-MTH3-FROM-TS.                                                 
               10  WS-MTH3-FROM-YEAR          PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH3-FROM-MONTH         PIC X(2).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH3-FROM-DAY           PIC X(2)  VALUE '01'.             
               10  FILLER                     PIC X(16) VALUE                   
           '-00.00.00.000000'.                                                  
                                                                                
           05  WS-MTH3-TO-TS.                                                   
               10  WS-MTH3-TO-YEAR            PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH3-TO-MONTH           PIC X(2).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-MTH3-TO-DAY             PIC X(2).                         
               10  FILLER                     PIC X(16) VALUE                   
           '-23.59.59.999999'.                                                  
                                                                                
                                                                                
           05  WS-1-MONTH                     PIC S9(4)V COMP-3 VALUE 1.        
           05  WS-2-MONTH                     PIC S9(4)V COMP-3 VALUE 2.        
           05  WS-3-MONTH                     PIC S9(4)V COMP-3 VALUE 3.        
                                                                                
           05  WS-TS                          PIC X(26).                        
           05  WS-TS-MTH1                     PIC X(26).                        
           05  WS-TS-MTH2                     PIC X(26).                        
           05  WS-TS-MTH3                     PIC X(26).                        
                                                                                
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
                                                                                
           05  WS-COUNT                       PIC S9(9) COMP.                   
                                                                                
           05  WS-GROUP-ID                    PIC X(7).                         
           05  WS-TAT                         PIC X(2).                         
           05  WS-BUS-SEG-CD                  PIC X(1).                         
           05  WS-LOGIN-SUCC-IND              PIC X(1).                         
           05  WS-PREV-GROUP-ID               PIC X(7)  VALUE SPACES.           
                                                                                
      * SWITCHES                                                                
                                                                                
           05  WS-EOF-SW                      PIC X.                            
               88  WS-EOF                     VALUE     'Y'.                    
               88  WS-EOF-NO                  VALUE     'N'.                    
                                                                                
           05  WS-GROUP-PROCESSED-SW          PIC X.                            
               88  WS-GROUP-PROCESSED         VALUE     'Y'.                    
               88  WS-GROUP-PROCESSED-NO      VALUE     'N'.                    
      *****************************************************************         
      *** ACCUMULATORS                                                          
      *****************************************************************         
      * FOR PAGE TOTALS                                                         
           05  WS-ACCM-TOT-REG-MTH1           PIC S9(7) COMP-3.                 
           05  WS-ACCM-TOT-REG-MTH2           PIC S9(7) COMP-3.                 
           05  WS-ACCM-TOT-REG-MTH3           PIC S9(7) COMP-3.                 
           05  WS-ACCM-TOT-LOGIN-Y-MTH1       PIC S9(7) COMP-3.                 
           05  WS-ACCM-TOT-LOGIN-Y-MTH2       PIC S9(7) COMP-3.                 
           05  WS-ACCM-TOT-LOGIN-Y-MTH3       PIC S9(7) COMP-3.                 
           05  WS-ACCM-TOT-LOGIN-N-MTH1       PIC S9(7) COMP-3.                 
           05  WS-ACCM-TOT-LOGIN-N-MTH2       PIC S9(7) COMP-3.                 
           05  WS-ACCM-TOT-LOGIN-N-MTH3       PIC S9(7) COMP-3.                 
      * FOR LINE TOTALS                                                         
           05  WS-3-MONTH-TOT-REG             PIC S9(7) COMP-3.                 
           05  WS-3-MONTH-TOT-Y-LOGIN         PIC S9(7) COMP-3.                 
           05  WS-3-MONTH-TOT-N-LOGIN         PIC S9(7) COMP-3.                 
                                                                                
      *****************************************************************         
      *** REPORT LINES                                                          
      *****************************************************************         
      *                                                                         
       01  PRINT-RECORD.                                                        
           05  PRINT-CTL                      PIC X(1).                         
           05  PRINT-DATA                     PIC X(160).                       
                                                                                
       01  P-HDR-1.                                                             
           05  FILLER                         PIC X(1)  VALUE '1'.              
           05  FILLER                         PIC X(59) VALUE SPACES.           
           05  FILLER                         PIC X(48) VALUE                   
           'PLAN MEMBER REGISTRATION / LOGIN ACTIVITY REPORT'.                  
           05  FILLER                         PIC X(31) VALUE SPACES.           
           05  FILLER                         PIC X(18) VALUE                   
           'PROGRAM: GCCPFRMK'.                                                 
                                                                                
       01  P-HDR-2.                                                             
           05  FILLER                         PIC X(1)  VALUE ' '.              
           05  FILLER                         PIC X(63) VALUE SPACES.           
           05  FILLER                         PIC X(35) VALUE                   
           '(PLAN MEMBER CLAIM / COVERAGE SITE)'.                               
           05  FILLER                         PIC X(40) VALUE SPACES.           
           05  FILLER                         PIC X(6)  VALUE                   
           'DATE: '.                                                            
           05  P-HDR-2-MONTH                  PIC X(3).                         
           05  FILLER                         PIC X     VALUE SPACES.           
           05  P-HDR-2-DAY                    PIC X(2).                         
           05  FILLER                         PIC X(2)  VALUE ', '.             
           05  P-HDR-2-YEAR                   PIC X(4).                         
                                                                                
       01  P-HDR-3.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(63) VALUE SPACES.           
           05  FILLER                         PIC X(16) VALUE                   
           'FOR THE PERIOD:'.                                                   
           05  P-HDR-3-FROM-MONTH             PIC X(3).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-HDR-3-FROM-YEAR              PIC X(4).                         
           05  FILLER                         PIC X(4)  VALUE                   
           ' TO '.                                                              
           05  P-HDR-3-TO-MONTH               PIC X(3).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-HDR-3-TO-YEAR                PIC X(4).                         
                                                                                
       01  P-HDR-4.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(18) VALUE                   
           'TURNAROUND TIME:  '.                                                
           05  P-HDR-4-TAT                    PIC X(30).                        
                                                                                
       01  P-HDR-5.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(18) VALUE                   
           'BUSINESS SEGMENT: '.                                                
           05  P-HDR-5-BS                     PIC X(30).                        
                                                                                
       01  P-HDR-6.                                                             
           05  FILLER                         PIC X     VALUE '-'.              
           05  FILLER                         PIC X(59) VALUE SPACES.           
           05  FILLER                         PIC X(24) VALUE                   
           '   # OF REGISTRATIONS'.                                             
           05  FILLER                         PIC X(12) VALUE                   
            ' TOTAL # OF'.                                                      
           05  FILLER                         PIC X(48) VALUE                   
           '                  # OF LOGINS'.                                     
           05  FILLER                         PIC X(15)  VALUE                  
           '    TOTAL #'.                                                       
                                                                                
       01  P-HDR-7.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(83) VALUE SPACES.           
           05  FILLER                         PIC X(12) VALUE                   
           'REGISTRATION'.                                                      
           05  FILLER                         PIC X(48) VALUE SPACES.           
           05  FILLER                         PIC X(15) VALUE                   
           '      OF'.                                                          
                                                                                
       01  P-HDR-8.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(143) VALUE SPACES.          
           05  FILLER                         PIC X(15) VALUE                   
           '    LOGINS'.                                                        
                                                                                
       01  P-HDR-9.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(8)  VALUE                   
           'GROUP'.                                                             
           05  FILLER                         PIC X(50) VALUE                   
           'COMPANY'.                                                           
           05  FILLER                         PIC X(8)  VALUE SPACES.           
           05  FILLER                         PIC X(8)  VALUE SPACES.           
           05  FILLER                         PIC X(8)  VALUE SPACES.           
           05  FILLER                         PIC X(12) VALUE                   
           ' TOTAL OF  '.                                                       
           05  FILLER                         PIC X(16) VALUE SPACES.           
           05  FILLER                         PIC X(16) VALUE SPACES.           
           05  FILLER                         PIC X(16) VALUE SPACES.           
           05  FILLER                         PIC X(15) VALUE                   
           '     TOTAL OF'.                                                     
                                                                                
       01  P-HDR-10.                                                            
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(7)  VALUE                   
           'NUMBER'.                                                            
           05  FILLER                         PIC X(56) VALUE                   
           ' NAME'.                                                             
           05  P-HDR-10-R-MONTH-1             PIC X(8).                         
           05  P-HDR-10-R-MONTH-2             PIC X(8).                         
           05  P-HDR-10-R-MONTH-3             PIC X(3).                         
           05  FILLER                         PIC X(19) VALUE                   
           ' 3 MONTHS'.                                                         
           05  P-HDR-10-L-MONTH-1             PIC X(16).                        
           05  P-HDR-10-L-MONTH-2             PIC X(16).                        
           05  P-HDR-10-L-MONTH-3             PIC X(14).                        
           05  FILLER                         PIC X(15) VALUE                   
           '3 MONTHS'.                                                          
                                                                                
       01  P-HDR-11.                                                            
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(59) VALUE SPACES.           
           05  FILLER                         PIC X(24) VALUE SPACES.           
           05  FILLER                         PIC X(12) VALUE SPACES.           
           05  FILLER                         PIC X(16) VALUE                   
           '    SUC/UNS'.                                                       
           05  FILLER                         PIC X(16) VALUE                   
           '    SUC/UNS'.                                                       
           05  FILLER                         PIC X(16) VALUE                   
           '    SUC/UNS'.                                                       
           05  FILLER                         PIC X(15) VALUE                   
           '    SUC/UNS'.                                                       
                                                                                
       01  P-DETL.                                                              
           05  FILLER                         PIC X     VALUE ' '.              
           05  P-DETL-GROUP-ID                PIC X(7).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-SPONSOR-NAME            PIC X(50).                        
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-R-MONTH-1               PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-R-MONTH-2               PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-R-MONTH-3               PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-R-TOTAL                 PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  P-DETL-L-Y-MONTH-1             PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE '/'.              
           05  P-DETL-L-N-MONTH-1             PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-L-Y-MONTH-2             PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE '/'.              
           05  P-DETL-L-N-MONTH-2             PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-L-Y-MONTH-3             PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE '/'.              
           05  P-DETL-L-N-MONTH-3             PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-L-Y-TOTAL               PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE '/'.              
           05  P-DETL-L-N-TOTAL               PIC ZZZZZZ9.                      
                                                                                
       01  P-TOTAL.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(59) VALUE                   
           'TOTALS'.                                                            
           05  P-TOTAL-R-MONTH-1              PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-R-MONTH-2              PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-R-MONTH-3              PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-R-TOTAL                PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(2)  VALUE SPACES.           
           05  P-TOTAL-L-Y-MONTH-1            PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE '/'.              
           05  P-TOTAL-L-N-MONTH-1            PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-L-Y-MONTH-2            PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE '/'.              
           05  P-TOTAL-L-N-MONTH-2            PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-L-Y-MONTH-3            PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE '/'.              
           05  P-TOTAL-L-N-MONTH-3            PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-L-Y-TOTAL              PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(1)  VALUE '/'.              
           05  P-TOTAL-L-N-TOTAL              PIC ZZZZZZ9.                      
                                                                                
       01  WS-LINE-COUNT                      PIC S9(4) COMP VALUE +35.         
       01  WS-LINE-MAX                        PIC S9(4) COMP VALUE +35.         
                                                                                
      *****************************************************************         
      *** CONSTANTS                                                             
      *****************************************************************         
      *                                                                         
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GAEDATSR                 PIC X(08)                            
                                          VALUE 'GAEDATSR'.                     
           05  WS-GAEDATSR-VERB            PIC X(16).                           
           05  WS-INPUT-LR                 PIC X(16)                            
                                          VALUE 'CARD-DATA-010   '.             
           05  WS-PRINT-LR                 PIC X(16)                            
                                          VALUE 'PRINT-DATA-022  '.             
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME              PIC X(60) VALUE                      
                'GCCPFRMK - PRINT TRANSACTION SUBMISSION TRACKING'.             
           10  AB-PARAGRAPH-NAME  OCCURS 25 TIMES                               
                                           PIC X(60).                           
           10  AB-MESSAGE.                                                      
               15  AB-MSG1                 PIC X(70).                           
               15  AB-MSG2                 PIC X(70).                           
           10  AB-SQLCODE                  PIC ----9.                           
           10  LVL                         PIC S9(4) COMP.                      
           10  CNT                         PIC S9(4) COMP.                      
                                                                                
                                                                                
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
             DECLARE DATE_ENTRIES CURSOR FOR                                    
             SELECT  CURRENT TIMESTAMP                                          
                    ,CURRENT TIMESTAMP - :WS-1-MONTH MONTHS                     
                    ,CURRENT TIMESTAMP - :WS-2-MONTH MONTHS                     
                    ,CURRENT TIMESTAMP - :WS-3-MONTH MONTHS                     
             FROM    TGD                                                        
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             DECLARE CURR_DATE CURSOR FOR                                       
             SELECT  CURRENT TIMESTAMP                                          
             FROM    TGD                                                        
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             DECLARE GROUP_LOOP CURSOR FOR                                      
             SELECT  A.GROUP_ID                                                 
                    ,A.SPONSOR_NAME                                             
             FROM    TGD                 A                                      
             WHERE   A.TAT               = :WS-TAT                              
             AND     A.BUS_SEG_CD        = :WS-BUS-SEG-CD                       
             ORDER   BY A.GROUP_ID                                              
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             DECLARE FIRST_GROUP_DIV CURSOR FOR                                 
             SELECT  BUS_SEG_CD                                                 
                    ,TAT                                                        
             FROM    TGD                                                        
             WHERE   GROUP_ID            = :WS-GROUP-ID                         
             ORDER   BY GROUP_ID                                                
                     ,  DIV_ID                                                  
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
      *  - GET DATE RANGE TO BE USED IN THE REPORT                              
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE 'GCCPFRMK'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
      * GET TAT CODES AND DESCRIPTIONS FROM THE TCTTAT TABLE                    
                                                                                
           PERFORM 1010-GET-TAT-CODES                                           
              THRU 1010-EXIT                                                    
                                                                                
      * GET BUSINESS SEGMENT CODES AND DESCRIPTIONS FORM THE                    
      * TCTBS TABLE                                                             
                                                                                
           PERFORM 1020-GET-BS-CODES                                            
              THRU 1020-EXIT.                                                   
                                                                                
      * GET DATE RANGE FOR REPORTS                                              
                                                                                
           PERFORM 1030-GET-DATE-RANGE                                          
              THRU 1030-EXIT.                                                   
                                                                                
                                                                                
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
                                                                                
                                                                                
                                                                                
       1030-GET-DATE-RANGE.                                                     
      *****************************************************************         
      *  - READ CONTROL CARD                                                    
      *  - IF NOT COMMENTED OUT USE TO BUILD TO AND FROM                        
      *    TIMESTAMPS FOR THE THREE MONTHS OF THE REPORT                        
      *  - IF COMMENTED OUT GET TIMESTAMP FROM DATA BASE                        
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1030-GET-DATE-RANGE'    TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
      * READ CONTROL CARD                                                       
                                                                                
           MOVE OBTAIN-FIRST             TO WS-GAEDATSR-VERB.                   
                                                                                
           PERFORM 6000-READ-INPUT                                              
              THRU 6000-EXIT.                                                   
                                                                                
      * IF CONTROL CARD PRESENT USE INFORMATION FROM CONTROL CARD               
      * TO CALCULATE THE TO AND FROM TIMESTAMPS.  IF NOT                        
      * GET THE DATES FROM THE DATA BASE                                        
                                                                                
           IF GCCCCRDK-CTL-CARD-PRESENT                                         
               MOVE GCCCCRDK-MONTH3      TO WS-MTH1-FROM-MONTH                  
               MOVE GCCCCRDK-MONTH3      TO WS-MTH1-TO-MONTH                    
               MOVE GCCCCRDK-MONTH3      TO WS-MONTH                            
               PERFORM 1045-GET-LAST-DAY-OF-MTH                                 
                  THRU 1045-EXIT                                                
               MOVE WS-WORK-DAY          TO WS-MTH1-TO-DAY                      
               MOVE GCCCCRDK-YEAR3       TO WS-MTH1-FROM-YEAR                   
               MOVE GCCCCRDK-YEAR3       TO WS-MTH1-TO-YEAR                     
               MOVE GCCCCRDK-MONTH2      TO WS-MTH2-FROM-MONTH                  
               MOVE GCCCCRDK-MONTH2      TO WS-MTH2-TO-MONTH                    
               MOVE GCCCCRDK-MONTH2      TO WS-MONTH                            
               PERFORM 1045-GET-LAST-DAY-OF-MTH                                 
                  THRU 1045-EXIT                                                
               MOVE WS-WORK-DAY          TO WS-MTH2-TO-DAY                      
               MOVE GCCCCRDK-YEAR2       TO WS-MTH2-FROM-YEAR                   
               MOVE GCCCCRDK-YEAR2       TO WS-MTH2-TO-YEAR                     
               MOVE GCCCCRDK-MONTH1      TO WS-MTH3-FROM-MONTH                  
               MOVE GCCCCRDK-MONTH1      TO WS-MTH3-TO-MONTH                    
               MOVE GCCCCRDK-MONTH1      TO WS-MONTH                            
               PERFORM 1045-GET-LAST-DAY-OF-MTH                                 
                  THRU 1045-EXIT                                                
               MOVE WS-WORK-DAY          TO WS-MTH3-TO-DAY                      
               MOVE GCCCCRDK-YEAR1       TO WS-MTH3-FROM-YEAR                   
               MOVE GCCCCRDK-YEAR1       TO WS-MTH3-TO-YEAR                     
               PERFORM 1050-GET-CURRENT-DATE                                    
                  THRU 1050-EXIT                                                
           ELSE                                                                 
               PERFORM 1040-GET-DATES                                           
                  THRU 1040-EXIT                                                
           END-IF.                                                              
                                                                                
      * MOVE TO RANGE TO HEADING                                                
                                                                                
           MOVE WS-MTH1-TO-MONTH         TO SUB1.                               
           MOVE WS-MONTH-NAME (SUB1)     TO P-HDR-3-TO-MONTH                    
                                            P-HDR-10-R-MONTH-1                  
                                            P-HDR-10-L-MONTH-1.                 
           MOVE WS-MTH1-TO-YEAR          TO P-HDR-3-TO-YEAR.                    
           MOVE WS-MTH2-TO-MONTH         TO SUB1.                               
           MOVE WS-MONTH-NAME (SUB1)     TO P-HDR-10-R-MONTH-2                  
                                            P-HDR-10-L-MONTH-2.                 
           MOVE WS-MTH3-TO-MONTH         TO SUB1.                               
           MOVE WS-MONTH-NAME (SUB1)     TO P-HDR-3-FROM-MONTH                  
                                            P-HDR-10-R-MONTH-3                  
                                            P-HDR-10-L-MONTH-3.                 
           MOVE WS-MTH3-TO-YEAR          TO P-HDR-3-FROM-YEAR.                  
                                                                                
      * MOVE CURRENT DATE TO HEADING                                            
                                                                                
           MOVE WS-MONTH                 TO SUB1.                               
           MOVE WS-MONTH-NAME (SUB1)     TO P-HDR-2-MONTH.                      
           MOVE WS-DAY                   TO P-HDR-2-DAY.                        
           MOVE WS-YEAR                  TO P-HDR-2-YEAR.                       
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1030-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1040-GET-DATES.                                                          
      *****************************************************************         
      * THIS PARAGRAPH IS USED IF THE DATES ARE NOT PASSED THROUGH              
      * CONTROL CARDS.  THIS JOB WILL RUN MONTHLY AFTER MIDNIGHT                
      * ON THE LAST DAY OF THE MONTH.  THE COUNTS WILL REFLECT THE              
      * PREVIOUS 3 MONTHS.                                                      
      * CURRENT DATE WILL BE OBTAINED FROM DATA BASE                            
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1040-GET-DATES'         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             OPEN DATE_ENTRIES                                                  
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             FETCH DATE_ENTRIES                                                 
             INTO  :WS-TS                                                       
                  ,:WS-TS-MTH1                                                  
                  ,:WS-TS-MTH2                                                  
                  ,:WS-TS-MTH3                                                  
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH DATE ENTRIES'     TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
      * BUILD TO AND FROM TIMESTAMPS FOR EACH MONTH                             
                                                                                
           MOVE WS-TS-MTH1               TO WS-TIMESTAMP.                       
           MOVE WS-YEAR                  TO WS-MTH1-FROM-YEAR.                  
           MOVE WS-YEAR                  TO WS-MTH1-TO-YEAR.                    
           MOVE WS-MONTH                 TO WS-MTH1-FROM-MONTH.                 
           MOVE WS-MONTH                 TO WS-MTH1-TO-MONTH.                   
           PERFORM 1045-GET-LAST-DAY-OF-MTH                                     
              THRU 1045-EXIT.                                                   
           MOVE WS-WORK-DAY              TO WS-MTH1-TO-DAY.                     
                                                                                
           MOVE WS-TS-MTH2               TO WS-TIMESTAMP.                       
           MOVE WS-YEAR                  TO WS-MTH2-FROM-YEAR.                  
           MOVE WS-YEAR                  TO WS-MTH2-TO-YEAR.                    
           MOVE WS-MONTH                 TO WS-MTH2-FROM-MONTH.                 
           MOVE WS-MONTH                 TO WS-MTH2-TO-MONTH.                   
           PERFORM 1045-GET-LAST-DAY-OF-MTH                                     
              THRU 1045-EXIT.                                                   
           MOVE WS-WORK-DAY              TO WS-MTH2-TO-DAY.                     
                                                                                
           MOVE WS-TS-MTH3               TO WS-TIMESTAMP.                       
           MOVE WS-YEAR                  TO WS-MTH3-FROM-YEAR.                  
           MOVE WS-YEAR                  TO WS-MTH3-TO-YEAR.                    
           MOVE WS-MONTH                 TO WS-MTH3-FROM-MONTH.                 
           MOVE WS-MONTH                 TO WS-MTH3-TO-MONTH.                   
           PERFORM 1045-GET-LAST-DAY-OF-MTH                                     
              THRU 1045-EXIT.                                                   
           MOVE WS-WORK-DAY              TO WS-MTH3-TO-DAY.                     
                                                                                
           MOVE WS-TS                    TO WS-TIMESTAMP.                       
                                                                                
           EXEC SQL                                                             
             CLOSE DATE_ENTRIES                                                 
           END-EXEC.                                                            
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1040-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1045-GET-LAST-DAY-OF-MTH.                                                
      *****************************************************************         
      * THIS PARAGRAPH FINDS THE LAST DAY OF THE MONTH BASED ON THE             
      * MONTH CODE                                                              
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1045-GET-LAST-DAY-OF-MTH'                                      
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EVALUATE TRUE                                                        
           WHEN WS-MONTH = '04' OR '06' OR '09' OR '11'                         
               MOVE '30'                 TO WS-WORK-DAY                         
           WHEN WS-MONTH = '01' OR '03' OR '05' OR '07' OR '08'                 
                        OR '10' OR '12'                                         
               MOVE '31'                 TO WS-WORK-DAY                         
           WHEN WS-MONTH = '02'                                                 
               MOVE WS-YEAR              TO WS-WORK-YEAR                        
               DIVIDE WS-WORK-YEAR       BY 4                                   
                      GIVING                WS-WORK-FIELD                       
                      REMAINDER             WS-WORK-REMAINDER                   
               IF WS-WORK-REMAINDER      =  0                                   
                   MOVE '29'             TO WS-WORK-DAY                         
               ELSE                                                             
                   MOVE '28'             TO WS-WORK-DAY                         
               END-IF                                                           
           END-EVALUATE.                                                        
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1045-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1050-GET-CURRENT-DATE.                                                   
      *****************************************************************         
      * IF CONTROL CARD USED FOR DATE RANGE ACCESS THE CURRENT DATE             
      * FOR USE IN REPORT HEADING                                               
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1050-PROCESS-INFORCE'   TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             OPEN CURR_DATE                                                     
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             FETCH CURR_DATE                                                    
             INTO  :WS-TS                                                       
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH CURRENT DATE'     TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           MOVE WS-TS                    TO WS-TIMESTAMP.                       
                                                                                
           EXEC SQL                                                             
             CLOSE CURR_DATE                                                    
           END-EXEC.                                                            
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1050-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-PROCESS-REPORT-BY-TAT.                                              
      *****************************************************************         
      *  - PROCESS REPORT FOR EACH TAT                                          
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS-REPORT-BY-TAT'                                    
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-TAT-ID (SUB1)         TO WS-TAT.                             
           MOVE WS-TAT-DESC (SUB1)       TO P-HDR-4-TAT.                        
                                                                                
           PERFORM 2010-PROCESS-REPORT-BY-BS                                    
              THRU 2010-EXIT                                                    
           VARYING SUB2 FROM 1 BY 1                                             
             UNTIL SUB2 > WS-BS-CD-COUNT.                                       
                                                                                
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
           MOVE WS-BS-DESC (SUB2)        TO P-HDR-5-BS.                         
                                                                                
           MOVE 0                        TO WS-ACCM-TOT-REG-MTH1                
                                            WS-ACCM-TOT-REG-MTH2                
                                            WS-ACCM-TOT-REG-MTH3                
                                            WS-ACCM-TOT-LOGIN-Y-MTH1            
                                            WS-ACCM-TOT-LOGIN-Y-MTH2            
                                            WS-ACCM-TOT-LOGIN-Y-MTH3            
                                            WS-ACCM-TOT-LOGIN-N-MTH1            
                                            WS-ACCM-TOT-LOGIN-N-MTH2            
                                            WS-ACCM-TOT-LOGIN-N-MTH3.           
                                                                                
           EXEC SQL                                                             
             OPEN GROUP_LOOP                                                    
           END-EXEC.                                                            
                                                                                
           SET WS-EOF-NO                 TO TRUE.                               
                                                                                
           PERFORM 2020-PROCESS-REPORT-BY-GROUP                                 
              THRU 2020-EXIT                                                    
             UNTIL WS-EOF.                                                      
                                                                                
           IF WS-ACCM-TOT-REG-MTH1       = 0 AND                                
              WS-ACCM-TOT-REG-MTH2       = 0 AND                                
              WS-ACCM-TOT-REG-MTH3       = 0 AND                                
              WS-ACCM-TOT-LOGIN-N-MTH1   = 0 AND                                
              WS-ACCM-TOT-LOGIN-Y-MTH1   = 0 AND                                
              WS-ACCM-TOT-LOGIN-N-MTH2   = 0 AND                                
              WS-ACCM-TOT-LOGIN-Y-MTH2   = 0 AND                                
              WS-ACCM-TOT-LOGIN-N-MTH3   = 0 AND                                
              WS-ACCM-TOT-LOGIN-Y-MTH3   = 0                                    
               NEXT SENTENCE                                                    
           ELSE                                                                 
               PERFORM 5100-PRT-REPORT-TOTALS                                   
                  THRU 5100-EXIT                                                
           END-IF.                                                              
                                                                                
           EXEC SQL                                                             
             CLOSE GROUP_LOOP                                                   
           END-EXEC.                                                            
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2010-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2020-PROCESS-REPORT-BY-GROUP.                                            
      *****************************************************************         
      *  - PROCESS ONE LINE OF REPORT FOR EACH GROUP                            
      *  - COUNTS MUST BE > 0 TO PRINT LINE                                     
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2020-PROCESS-INFORCE-BY-GROUP'                                 
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             FETCH GROUP_LOOP                                                   
             INTO  :DCLTGD.GROUP-ID                                             
                  ,:DCLTGD.SPONSOR-NAME                                         
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH GROUP LOOP'       TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           IF SQLCODE = 0 AND                                                   
              GROUP-ID OF DCLTGD      NOT = WS-PREV-GROUP-ID                    
               MOVE GROUP-ID OF DCLTGD   TO WS-PREV-GROUP-ID                    
                                            WS-GROUP-ID                         
               MOVE 0                    TO WS-3-MONTH-TOT-REG                  
                                            WS-3-MONTH-TOT-Y-LOGIN              
                                            WS-3-MONTH-TOT-N-LOGIN              
               PERFORM 2040-CHECK-GROUP                                         
                  THRU 2040-EXIT                                                
               IF WS-GROUP-PROCESSED-NO                                         
                   PERFORM 2030-PROCESS-REPORT-LINE                             
                      THRU 2030-EXIT                                            
               END-IF                                                           
           END-IF.                                                              
                                                                                
           IF SQLCODE = +100                                                    
               SET WS-EOF                TO TRUE                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2020-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2030-PROCESS-REPORT-LINE.                                                
      *****************************************************************         
      * PRINT LINE FOR EACH GROUP WHERE COUNTS ARE > 0                          
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2030-PROCESS-REPORT-LINE'                                      
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE GROUP-ID OF DCLTGD       TO P-DETL-GROUP-ID.                    
           MOVE SPONSOR-NAME OF DCLTGD   TO P-DETL-SPONSOR-NAME.                
                                                                                
      * REGISTRATION FIRST MONTH                                                
                                                                                
           MOVE WS-MTH1-FROM-TS          TO WS-FROM-TS.                         
           MOVE WS-MTH1-TO-TS            TO WS-TO-TS.                           
           PERFORM 2100-GET-REG-COUNT                                           
              THRU 2100-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-R-MONTH-1.                   
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-REG.                 
           ADD  WS-COUNT                 TO WS-ACCM-TOT-REG-MTH1.               
                                                                                
      * SUCCESSFUL LOGIN FIRST MONTH                                            
                                                                                
           MOVE 'Y'                      TO WS-LOGIN-SUCC-IND.                  
           PERFORM 2200-GET-LOGIN-COUNT                                         
              THRU 2200-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-L-Y-MONTH-1.                 
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-Y-LOGIN.             
           ADD  WS-COUNT                 TO WS-ACCM-TOT-LOGIN-Y-MTH1.           
                                                                                
      * UNSUCCESSFUL LOGIN FIRST MONTH                                          
                                                                                
           MOVE 'N'                      TO WS-LOGIN-SUCC-IND.                  
           PERFORM 2200-GET-LOGIN-COUNT                                         
              THRU 2200-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-L-N-MONTH-1.                 
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-N-LOGIN.             
           ADD  WS-COUNT                 TO WS-ACCM-TOT-LOGIN-N-MTH1.           
                                                                                
      * REGISTRATION SECOND MONTH                                               
                                                                                
           MOVE WS-MTH2-FROM-TS          TO WS-FROM-TS.                         
           MOVE WS-MTH2-TO-TS            TO WS-TO-TS.                           
           PERFORM 2100-GET-REG-COUNT                                           
              THRU 2100-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-R-MONTH-2.                   
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-REG.                 
           ADD  WS-COUNT                 TO WS-ACCM-TOT-REG-MTH2.               
                                                                                
      * SUCCESSFUL LOGIN SECOND MONTH                                           
                                                                                
           MOVE 'Y'                      TO WS-LOGIN-SUCC-IND.                  
           PERFORM 2200-GET-LOGIN-COUNT                                         
              THRU 2200-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-L-Y-MONTH-2.                 
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-Y-LOGIN.             
           ADD  WS-COUNT                 TO WS-ACCM-TOT-LOGIN-Y-MTH2.           
                                                                                
      * UNSUCCESSFUL LOGIN SECOND MONTH                                         
                                                                                
           MOVE 'N'                      TO WS-LOGIN-SUCC-IND.                  
           PERFORM 2200-GET-LOGIN-COUNT                                         
              THRU 2200-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-L-N-MONTH-2.                 
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-N-LOGIN.             
           ADD  WS-COUNT                 TO WS-ACCM-TOT-LOGIN-N-MTH2.           
                                                                                
                                                                                
      * REGISTRATION THIRD MONTH                                                
                                                                                
           MOVE WS-MTH3-FROM-TS          TO WS-FROM-TS.                         
           MOVE WS-MTH3-TO-TS            TO WS-TO-TS.                           
           PERFORM 2100-GET-REG-COUNT                                           
              THRU 2100-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-R-MONTH-3.                   
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-REG.                 
           ADD  WS-COUNT                 TO WS-ACCM-TOT-REG-MTH3.               
                                                                                
      * SUCCESSFUL LOGIN THIRD MONTH                                            
                                                                                
           MOVE 'Y'                      TO WS-LOGIN-SUCC-IND.                  
           PERFORM 2200-GET-LOGIN-COUNT                                         
              THRU 2200-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-L-Y-MONTH-3.                 
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-Y-LOGIN.             
           ADD  WS-COUNT                 TO WS-ACCM-TOT-LOGIN-Y-MTH3.           
                                                                                
      * UNSUCCESSFUL LOGIN THIRD MONTH                                          
                                                                                
           MOVE 'N'                      TO WS-LOGIN-SUCC-IND.                  
           PERFORM 2200-GET-LOGIN-COUNT                                         
              THRU 2200-EXIT.                                                   
           MOVE WS-COUNT                 TO P-DETL-L-N-MONTH-3.                 
           ADD  WS-COUNT                 TO WS-3-MONTH-TOT-N-LOGIN.             
           ADD  WS-COUNT                 TO WS-ACCM-TOT-LOGIN-N-MTH3.           
                                                                                
      * PRINT LINE IF COUNTS ARE > 0                                            
                                                                                
           IF WS-3-MONTH-TOT-REG         =  0 AND                               
              WS-3-MONTH-TOT-Y-LOGIN     =  0 AND                               
              WS-3-MONTH-TOT-N-LOGIN     =  0                                   
               NEXT SENTENCE                                                    
           ELSE                                                                 
               MOVE WS-3-MONTH-TOT-REG   TO P-DETL-R-TOTAL                      
               MOVE WS-3-MONTH-TOT-Y-LOGIN                                      
                                         TO P-DETL-L-Y-TOTAL                    
               MOVE WS-3-MONTH-TOT-N-LOGIN                                      
                                         TO P-DETL-L-N-TOTAL                    
               IF WS-LINE-COUNT          =  WS-LINE-MAX                         
                   PERFORM 5000-PRT-HEADINGS                                    
                      THRU 5000-EXIT                                            
               END-IF                                                           
               ADD 1                     TO WS-LINE-COUNT                       
               MOVE P-DETL               TO PRINT-RECORD                        
               PERFORM 7000-PRINT-OUTPUT                                        
                  THRU 7000-EXIT                                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2030-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2040-CHECK-GROUP.                                                        
      *****************************************************************         
      * IF THE DIVISIONS WITHIN A GROUP HAVE MORE THAN 1 BUSINESS               
      * SEGMENT OR TAT, DISPLAY THE GROUPS STATISTICS UNDER THE                 
      * TAT AND BUSINESS SEGMENT CODE OF THE FIRST DIVISION                     
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2040-CHECK-GROUP'       TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             OPEN FIRST_GROUP_DIV                                               
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             FETCH FIRST_GROUP_DIV                                              
             INTO  :DCLTGD.BUS-SEG-CD                                           
                  ,:DCLTGD.TAT                                                  
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH FIRST GROUP DIV'  TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           IF BUS-SEG-CD OF DCLTGD       =  WS-BUS-SEG-CD AND                   
              TAT        OF DCLTGD       =  WS-TAT                              
               SET WS-GROUP-PROCESSED-NO TO TRUE                                
           ELSE                                                                 
               SET WS-GROUP-PROCESSED    TO TRUE                                
           END-IF.                                                              
                                                                                
           EXEC SQL                                                             
             CLOSE FIRST_GROUP_DIV                                              
           END-EXEC.                                                            
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2040-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2100-GET-REG-COUNT.                                                      
      *****************************************************************         
      * FIND THE NUMBER OF REGISTRATION FOR THIS GROUP WITHIN                   
      * TIMEFRAME                                                               
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2100-GET-REG-COUNT'     TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             SELECT COUNT(*)                                                    
             INTO   :WS-COUNT                                                   
             FROM   TGCT       A                                                
                   ,TEH        B                                                
             WHERE  A.GROUP_ID           =  :WS-GROUP-ID                        
             AND    A.CUST_ID            =   B.CUST_ID                          
CC           AND    A.CHN_TS             =                                      
CC                  (SELECT MIN(C.CHN_TS)                                       
CC                     FROM TGCT C                                              
CC                    WHERE B.CUST_ID = C.CUST_ID)                              
+R45WR       AND    B.EVENT_TYP_CD       =  'PMSSRG'                            
             AND    B.EVENT_TS           >= :WS-FROM-TS                         
             AND    B.EVENT_TS           <= :WS-TO-TS                           
           END-EXEC.                                                            
                                                                                
           MOVE 'COUNT REGISTRATIONS'    TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2200-GET-LOGIN-COUNT.                                                    
      *****************************************************************         
      * FIND THE NUMBER OF LOGINS FOR THIS GROUP WITHIN                         
      * TIMEFRAME                                                               
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2200-GET-LOGIN-COUNT'   TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             SELECT COUNT(*)                                                    
             INTO   :WS-COUNT                                                   
             FROM   TGCT       A                                                
                   ,TLH        B                                                
             WHERE  A.GROUP_ID           =  :WS-GROUP-ID                        
             AND    A.CUST_ID            =   B.CUST_ID                          
             AND    B.LOGIN_SUCC_IND     =  :WS-LOGIN-SUCC-IND                  
             AND    B.LOGIN_TS           >= :WS-FROM-TS                         
             AND    B.LOGIN_TS           <= :WS-TO-TS                           
           END-EXEC.                                                            
                                                                                
           MOVE 'COUNT LOGINS'           TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3000-FINISH.                                                             
      *****************************************************************         
      *  - CLOSE FILES                                                          
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '3000-FINISH'           TO AB-PARAGRAPH-NAME (LVL).             
                                                                                
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
                                                                                
           MOVE P-HDR-11                 TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           MOVE 0                        TO WS-LINE-COUNT.                      
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       5000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       5100-PRT-REPORT-TOTALS.                                                  
      *****************************************************************         
      *  - PRINT TOTALS AT THE END OF EACH REPORT                               
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '5100-PRT-REPORT-TOTALS' TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-ACCM-TOT-REG-MTH1     TO P-TOTAL-R-MONTH-1.                  
           MOVE WS-ACCM-TOT-REG-MTH2     TO P-TOTAL-R-MONTH-2.                  
           MOVE WS-ACCM-TOT-REG-MTH3     TO P-TOTAL-R-MONTH-3.                  
           MOVE WS-ACCM-TOT-LOGIN-Y-MTH1 TO P-TOTAL-L-Y-MONTH-1.                
           MOVE WS-ACCM-TOT-LOGIN-Y-MTH2 TO P-TOTAL-L-Y-MONTH-2.                
           MOVE WS-ACCM-TOT-LOGIN-Y-MTH3 TO P-TOTAL-L-Y-MONTH-3.                
           MOVE WS-ACCM-TOT-LOGIN-N-MTH1 TO P-TOTAL-L-N-MONTH-1.                
           MOVE WS-ACCM-TOT-LOGIN-N-MTH2 TO P-TOTAL-L-N-MONTH-2.                
           MOVE WS-ACCM-TOT-LOGIN-N-MTH3 TO P-TOTAL-L-N-MONTH-3.                
                                                                                
           COMPUTE WS-3-MONTH-TOT-REG    =  WS-ACCM-TOT-REG-MTH1                
                                         +  WS-ACCM-TOT-REG-MTH2                
                                         +  WS-ACCM-TOT-REG-MTH3.               
                                                                                
           MOVE WS-3-MONTH-TOT-REG       TO P-TOTAL-R-TOTAL.                    
                                                                                
           COMPUTE WS-3-MONTH-TOT-Y-LOGIN                                       
                                         =  WS-ACCM-TOT-LOGIN-Y-MTH1            
                                         +  WS-ACCM-TOT-LOGIN-Y-MTH2            
                                         +  WS-ACCM-TOT-LOGIN-Y-MTH3.           
                                                                                
           MOVE WS-3-MONTH-TOT-Y-LOGIN   TO P-TOTAL-L-Y-TOTAL.                  
                                                                                
           COMPUTE WS-3-MONTH-TOT-N-LOGIN                                       
                                         =  WS-ACCM-TOT-LOGIN-N-MTH1            
                                         +  WS-ACCM-TOT-LOGIN-N-MTH2            
                                         +  WS-ACCM-TOT-LOGIN-N-MTH3.           
                                                                                
           MOVE WS-3-MONTH-TOT-N-LOGIN   TO P-TOTAL-L-N-TOTAL.                  
                                                                                
           MOVE P-TOTAL                  TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
      * FORCE HEADINGS TO PRINT FOR NEW REPORT                                  
                                                                                
           MOVE WS-LINE-MAX              TO WS-LINE-COUNT.                      
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       5100-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       6000-READ-INPUT.                                                         
      *****************************************************************         
      *  - READ 1 CONTROL CARD                                                  
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6000-READ-INPUT'        TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-INPUT-LR              TO LOGICAL-RECORD-NAME.                
           INITIALIZE GCCCCRDK-RECORD.                                          
                                                                                
           CALL WS-GAEDATSR              USING WS-GAEDATSR-VERB                 
                                               GCCCCRDK-RECORD                  
                                               ICBM.                            
                                                                                
           IF LR-STATUS-OK                                                      
               CONTINUE                                                         
           ELSE                                                                 
               MOVE 'ERROR READING CONTROL CARD INPUT' TO AB-MSG1               
               MOVE '6000-READ-INPUT'                  TO AB-MSG2               
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       6000-EXIT.                                                               
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
