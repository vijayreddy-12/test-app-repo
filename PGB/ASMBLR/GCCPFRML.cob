       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRML.                                                 
      *AUTHOR.        J. ELKINS.                                                
      *DATE-WRITTEN.  NOV 01, 2001.                                             
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS - E-COMMERCE                                          
      *   GCCPFRML - PRINT THE PLAN ADMINISTRATOR LOGIN ACTIVITY                
      *              REPORT                                                     
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *                                                                         
      *   THIS PROGRAM PRINTS A REPORT OF THE NUMBER OF                         
      *   SUCCESSFUL AND UNSUCCESSFUL LOGINS FOR A USERID (PA) AND              
      *   THE DATE OF THE LAST LOG IN.                                          
      *   THE REPORT IS BY BUSINESS SEGMENT.                                    
      *                                                                         
      *                                                                         
      *   INPUT FILES:  NONE                                                    
      *                                                                         
      *   OUTPUT FILES: PLAN ADMINISTRATOR LOGIN ACTIVITY                       
      *                 REPORT                                                  
      *                                                                         
      *   DB2 TABLES                                                            
      *   ACCESSED:     TUT     - PLAN ADMINISTRATOR    - SELECT                
      *                 TCUST   - CUSTOMER              - SELECT                
      *                 TPAA    - CUSTOMER ACCESS       - SELECT                
      *                 TGD     - GROUP DIVISION        - SELECT                
      *                 TLH     - LOGIN HISTORY         - SELECT                
      *                 TCTBS   - BUSINESS SEGMENT CODE - SELECT                
      *                                                                         
      *   CALLS:        GAEDATSR - FILE I/O                                     
      *                 GC2TODAY - GET SYSTEM DATE                              
      *                                                                         
      *   INCLUDE CODE: SQLCA    - SQL COMMUNICATION AREA                       
      *                                                                         
      *                 TUTD     - TUT     TABLE DECLARATION                    
      *                 TCUSTD   - TCUSTD  TABLE DECLARATION                    
      *                 TPAAD    - TPAA    TABLE DECLARATION                    
      *                 TGDD     - TGD     TABLE DECLARATION                    
      *                 TLHD     - TLH     TABLE DECLARATION                    
      *                 TCTBSD   - TCTBS   TABLE DECLARATION                    
      *                                                                         
      *                 TUT      - TUT     HOST VARIABLES                       
      *                 TCUST    - TCUST   HOST VARIABLES                       
      *                 TPAA     - TPAA    HOST VARIABLES                       
      *                 TGD      - TGD     HOST VARIABLES                       
      *                 TLH      - TLH     HOST VARIABLES                       
      *                 TCTBS    - TCTBS   HOST VARIABLES                       
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
      * J. ELKINS    º01/11/01º ORIGINAL CODE                                   
      *--------------+--------+-----------------------------------------        
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
                                                                                
           EXEC SQL INCLUDE TUTD    END-EXEC.                                   
           EXEC SQL INCLUDE TCUSTD  END-EXEC.                                   
           EXEC SQL INCLUDE TPAAD   END-EXEC.                                   
           EXEC SQL INCLUDE TGDD    END-EXEC.                                   
           EXEC SQL INCLUDE TLHD    END-EXEC.                                   
           EXEC SQL INCLUDE TCTBSD  END-EXEC.                                   
                                                                                
      *** DB2 HOST & INDICATOR VARIABLES                                        
                                                                                
      *01  DCLTUT.                                                              
           EXEC SQL INCLUDE TUT     END-EXEC.                                   
                                                                                
      *01  DCLTCUST.                                                            
           EXEC SQL INCLUDE TCUST   END-EXEC.                                   
                                                                                
      *01  DCLTPAA.                                                             
           EXEC SQL INCLUDE TPAA    END-EXEC.                                   
                                                                                
      *01  DCLTGD.                                                              
           EXEC SQL INCLUDE TGD     END-EXEC.                                   
                                                                                
      *01  DCLTLH.                                                              
           EXEC SQL INCLUDE TLH     END-EXEC.                                   
                                                                                
      *01  DCLTCTBS.                                                            
           EXEC SQL INCLUDE TCTBS   END-EXEC.                                   
                                                                                
                                                                                
                                                                                
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                          
                                                                                
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
       01  GCCCCRDL-RECORD.                                                     
           COPY GCCCCRDL.                                                       
                                                                                
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
                                                                                
           05  WS-BUILD-FROM-TS.                                                
               10  WS-FROM-YEAR               PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-FROM-MONTH              PIC X(2).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-FROM-DAY                PIC X(2)  VALUE '01'.             
               10  FILLER                     PIC X(16) VALUE                   
           '-00.00.00.000000'.                                                  
                                                                                
           05  WS-BUILD-TO-TS.                                                  
               10  WS-TO-YEAR                 PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-TO-MONTH                PIC X(2).                         
               10  FILLER                     PIC X(1)  VALUE '-'.              
               10  WS-TO-DAY                  PIC X(2).                         
               10  FILLER                     PIC X(16) VALUE                   
           '-23.59.59.999999'.                                                  
                                                                                
                                                                                
           05  WS-MONTH-OFF                   PIC S9(4)V COMP-3 VALUE 1.        
                                                                                
           05  WS-TS                          PIC X(26).                        
           05  WS-TS-MTH                      PIC X(26).                        
                                                                                
           05  WS-TIMESTAMP.                                                    
               10  WS-YEAR                    PIC X(4).                         
               10  FILLER                     PIC X(1).                         
               10  WS-MONTH                   PIC X(2).                         
               10  FILLER                     PIC X(1).                         
               10  WS-DAY                     PIC X(2).                         
               10  FILLER                     PIC X(16).                        
                                                                                
           05  WS-NO                          PIC X(3)  VALUE 'NO '.            
           05  WS-YES                         PIC X(3)  VALUE 'YES'.            
                                                                                
           05  WS-COUNT                       PIC S9(9) COMP.                   
                                                                                
           05  WS-BUS-SEG-CD                  PIC X(1).                         
           05  WS-LOGIN-SUCC-IND              PIC X(1)  VALUE 'Y'.              
           05  WS-LOGIN-FAIL-IND              PIC X(1)  VALUE 'N'.              
           05  WS-PREV-USER-ID                PIC X(20).                        
                                                                                
           05  WS-VDATE-EXT-DATE.                                               
               10  WS-VDATE-EXT-DD            PIC X(2).                         
               10  WS-VDATE-EXT-MMM           PIC X(3).                         
               10  WS-VDATE-EXT-YYYY          PIC X(4).                         
                                                                                
           05  WS-YYYYMMDD.                                                     
               10  WS-YYYY                    PIC X(4).                         
               10  WS-MM                      PIC X(2).                         
               10  WS-DD                      PIC X(2).                         
                                                                                
           05  WS-NAME.                                                         
               10  WS-NAME-CHAR               PIC X OCCURS 30 TIMES.            
                                                                                
           05  WS-FIRST-NAME                  PIC X(30).                        
           05  WS-LAST-NAME                   PIC X(30).                        
           05  WS-CUST-NAME                   PIC X(61).                        
                                                                                
           05  WS-DELIMITER                   PIC X      VALUE '&'.             
                                                                                
      * SWITCHES                                                                
                                                                                
           05  WS-EOF-SW                      PIC X.                            
               88  WS-EOF                     VALUE     'Y'.                    
               88  WS-EOF-NO                  VALUE     'N'.                    
                                                                                
           05  WS-LOGIN-SW                    PIC X.                            
               88  WS-LOGIN-YES               VALUE     'Y'.                    
               88  WS-LOGIN-NO                VALUE     'N'.                    
                                                                                
           05  WS-PRT-IN-RPT-SW               PIC X.                            
               88  WS-PRT-IN-RPT-YES          VALUE     'Y'.                    
               88  WS-PRT-IN-RPT-NO           VALUE     'N'.                    
      *****************************************************************         
      *** ACCUMULATORS                                                          
      *****************************************************************         
      *                                                                         
       01  WS-ACCM-USER-COUNT                 PIC S9(7) COMP-3.                 
       01  WS-ACCM-FAILED-LOGINS              PIC S9(7) COMP-3.                 
       01  WS-ACCM-SUCC-LOGINS                PIC S9(7) COMP-3.                 
                                                                                
      *****************************************************************         
      *** REPORT LINES                                                          
      *****************************************************************         
      *                                                                         
       01  PRINT-RECORD.                                                        
           05  PRINT-CTL                      PIC X(1).                         
           05  PRINT-DATA                     PIC X(160).                       
                                                                                
       01  P-HDR-1.                                                             
           05  FILLER                         PIC X(1)  VALUE '1'.              
           05  FILLER                         PIC X(63) VALUE SPACES.           
           05  FILLER                         PIC X(33) VALUE                   
           'Plan Administrator Login Activity'.                                 
           05  FILLER                         PIC X(44) VALUE SPACES.           
           05  FILLER                         PIC X(20) VALUE                   
           'Program: GCCPFRML'.                                                 
                                                                                
       01  P-HDR-2.                                                             
           05  FILLER                         PIC X(1)  VALUE ' '.              
           05  FILLER                         PIC X(67) VALUE SPACES.           
           05  FILLER                         PIC X(25) VALUE                   
           '(Plan Administrator Site)'.                                         
           05  FILLER                         PIC X(48) VALUE SPACES.           
           05  FILLER                         PIC X(6)  VALUE                   
           'Date: '.                                                            
           05  P-HDR-2-MONTH                  PIC X(3).                         
           05  FILLER                         PIC X     VALUE SPACES.           
           05  P-HDR-2-DAY                    PIC X(2).                         
           05  FILLER                         PIC X(2)  VALUE ', '.             
           05  P-HDR-2-YEAR                   PIC X(4).                         
                                                                                
       01  P-HDR-3.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(63) VALUE SPACES.           
           05  FILLER                         PIC X(16) VALUE                   
           'For the period:'.                                                   
           05  P-HDR-3-FROM-DAY               PIC X(2).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-HDR-3-FROM-MONTH             PIC X(3).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-HDR-3-FROM-YEAR              PIC X(4).                         
           05  FILLER                         PIC X(4)  VALUE                   
           ' TO '.                                                              
           05  P-HDR-3-TO-DAY                 PIC X(2).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-HDR-3-TO-MONTH               PIC X(3).                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-HDR-3-TO-YEAR                PIC X(4).                         
                                                                                
       01  P-HDR-4.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(18) VALUE                   
           'Business Segment: '.                                                
           05  P-HDR-4-BS                     PIC X(30).                        
                                                                                
       01  P-HDR-5.                                                             
           05  FILLER                         PIC X     VALUE '-'.              
           05  FILLER                         PIC X(20) VALUE                   
               'User ID'.                                                       
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(43) VALUE                   
               'Plan'.                                                          
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(40) VALUE                   
           'Company'.                                                           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE                   
           'Plan'.                                                              
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           'Date Last'.                                                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(25) VALUE                   
           '# of Logins in Last Month'.                                         
                                                                                
       01  P-HDR-6.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(20) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(43) VALUE                   
               'Administrator'.                                                 
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(40) VALUE                   
           'Name'.                                                              
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE                   
           'Administrator'.                                                     
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           'Logged In'.                                                         
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(7)  VALUE                   
           'Failed'.                                                            
           05  FILLER                         PIC X(8)  VALUE SPACES.           
           05  FILLER                         PIC X(10) VALUE                   
           'Successful'.                                                        
                                                                                
       01  P-HDR-7.                                                             
           05  FILLER                         PIC X     VALUE ' '.              
           05  FILLER                         PIC X(20) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(43) VALUE                   
           'Name'.                                                              
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(40) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE                   
           'Login'.                                                             
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE                   
           '(ddmmmyyyy)'.                                                       
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(25) VALUE SPACES.           
                                                                                
       01  P-BLK-LINE.                                                          
           05  FILLER                         PIC X(1)  VALUE '0'.              
           05  FILLER                         PIC X(160) VALUE SPACES.          
                                                                                
       01  P-DETL.                                                              
           05  FILLER                         PIC X     VALUE ' '.              
           05  P-DETL-USER-ID                 PIC X(20).                        
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-CUST-NAME               PIC X(43).                        
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-SPONSOR-NAME            PIC X(40).                        
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-PA-LOGIN                PIC X(13).                        
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-DATE-LAST-LOGIN.                                          
               10  FILLER                     PIC X(1)  VALUE SPACES.           
               10  P-DETL-DD                  PIC X(2).                         
               10  P-DETL-MMM                 PIC X(3).                         
               10  P-DETL-YYYY                PIC X(4).                         
               10  FILLER                     PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-DETL-FAILED                  PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(11) VALUE SPACES.           
           05  P-DETL-SUCCESSFUL              PIC ZZZZZZ9.                      
                                                                                
       01  P-TOTAL.                                                             
           05  FILLER                         PIC X     VALUE '-'.              
           05  FILLER                         PIC X(20) VALUE                   
           'Totals'.                                                            
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-USERS                  PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(36) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(40) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(13) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  FILLER                         PIC X(11) VALUE SPACES.           
           05  FILLER                         PIC X(1)  VALUE SPACES.           
           05  P-TOTAL-FAILED                 PIC ZZZZZZ9.                      
           05  FILLER                         PIC X(11) VALUE SPACES.           
           05  P-TOTAL-SUCCESSFUL             PIC ZZZZZZ9.                      
                                                                                
       01  P-NO-USERS.                                                          
           05  FILLER                         PIC X     VALUE '-'.              
           05  FILLER                         PIC X(50) VALUE                   
           'No users found for this business segment'.                          
                                                                                
       01  WS-LINE-COUNT                      PIC S9(4) COMP VALUE +50.         
       01  WS-LINE-MAX                        PIC S9(4) COMP VALUE +50.         
                                                                                
      *****************************************************************         
      *** CONSTANTS                                                             
      *****************************************************************         
      *                                                                         
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GC2DATE                PIC X(08) VALUE 'GC2DATE'.             
           05  WS-GAEDATSR               PIC X(08)                              
                                         VALUE 'GAEDATSR'.                      
           05  WS-GAEDATSR-VERB          PIC X(16).                             
           05  WS-INPUT-LR               PIC X(16)                              
                                         VALUE 'CARD-DATA-010   '.              
           05  WS-PRINT-LR               PIC X(16)                              
                                         VALUE 'PRINT-DATA-022  '.              
       01  GAC-DATE-PARAMETERS.          COPY GARDATEP.                         
                                                                                
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME              PIC X(60) VALUE                      
                'GCCPFRML - PRINT TRANSACTION SUBMISSION TRACKING'.             
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
             DECLARE ACTLOG CURSOR FOR                                          
             SELECT A.USER_ID                                                   
                   ,A.CUST_ID                                                   
                   ,B.CUST_FIRST_NAME                                           
                   ,B.CUST_LAST_NAME                                            
                   ,B.SPONSOR_NAME                                              
                   ,B.CUST_STAT_CD                                              
                   ,B.CUST_STAT_REAS_CD                                         
                   ,C.GROUP_ID                                                  
                   ,C.DIV_ID                                                    
             FROM   TUT                  A                                      
                   ,TCUST                B                                      
                   ,TPAA                 C                                      
             WHERE A.CUST_ID             =  B.CUST_ID                           
               AND B.ROLE               ^=  'T'                                 
               AND A.CUST_ID             =  C.CUST_ID                           
             ORDER  BY A.USER_ID                                                
                      ,C.GROUP_ID                                               
                      ,C.DIV_ID                                                 
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
             SELECT  CURRENT TIMESTAMP - :WS-MONTH-OFF MONTHS                   
             FROM    TGD                                                        
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             DECLARE LSTLOG CURSOR FOR                                          
             SELECT  LOGIN_TS                                                   
             FROM    TLH                                                        
             WHERE   CUST_ID             = :DCLTUT.CUST-ID                      
             ORDER   BY LOGIN_TS DESC                                           
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
                                                                                
           PERFORM 2000-PROCESS-REPORT-BY-BS                                    
              THRU 2000-EXIT                                                    
           VARYING SUB1 FROM 1 BY 1                                             
             UNTIL SUB1 > WS-BS-CD-COUNT.                                       
                                                                                
           PERFORM 3000-FINISH                                                  
              THRU 3000-EXIT.                                                   
                                                                                
                                                                                
           GOBACK.                                                              
                                                                                
                                                                                
       1000-INITIALIZATION.                                                     
      *****************************************************************         
      *  - INITIALIZE DATA SERVER AREA                                          
      *  - BUILD BUSINESS SEGMENT TABLE                                         
      *  - GET DATE RANGE TO BE USED IN THE REPORT                              
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE 'GCCPFRML'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
      * GET BUSINESS SEGMENT CODES AND DESCRIPTIONS FORM THE                    
      * TCTBS TABLE                                                             
                                                                                
           PERFORM 1020-GET-BS-CODES                                            
              THRU 1020-EXIT.                                                   
                                                                                
      * GET DATE RANGE FOR REPORTS                                              
                                                                                
           PERFORM 1030-GET-DATE-RANGE                                          
              THRU 1030-EXIT.                                                   
                                                                                
      * GET TODAY'S DATE                                                        
                                                                                
           MOVE LOW-VALUES               TO GAC-DATE-PARAMETERS.                
           MOVE 'E'                      TO VDATE-REQ-SERVICE.                  
           MOVE 'A'                      TO VDATE-REQ-BASIS.                    
           MOVE 'E'                      TO VDATE-REQ-LANGUAGE.                 
           MOVE 1                        TO VDATE-REQ-DETAIL.                   
                                                                                
           CALL WS-GC2DATE               USING GAC-DATE-PARAMETERS.             
                                                                                
           MOVE VDATE-EXT-YEAR           TO P-HDR-2-YEAR.                       
           MOVE VDATE-EXT-MONTH          TO P-HDR-2-MONTH.                      
           MOVE VDATE-EXT-DAY            TO P-HDR-2-DAY.                        
                                                                                
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1000-EXIT.                                                               
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
                                                                                
           IF GCCCCRDL-CTL-CARD-PRESENT                                         
               MOVE GCCCCRDL-MONTH       TO WS-FROM-MONTH                       
               MOVE GCCCCRDL-MONTH       TO WS-TO-MONTH                         
               MOVE GCCCCRDL-MONTH       TO WS-MONTH                            
               PERFORM 1045-GET-LAST-DAY-OF-MTH                                 
                  THRU 1045-EXIT                                                
               MOVE WS-WORK-DAY          TO WS-TO-DAY                           
                                            WS-DAY                              
               MOVE GCCCCRDL-YEAR        TO WS-FROM-YEAR                        
                                            WS-TO-YEAR                          
                                            WS-YEAR                             
               MOVE WS-BUILD-FROM-TS     TO WS-FROM-TS                          
               MOVE WS-BUILD-TO-TS       TO WS-TO-TS                            
           ELSE                                                                 
               PERFORM 1040-GET-DATES                                           
                  THRU 1040-EXIT                                                
           END-IF.                                                              
                                                                                
      * MOVE TO RANGE TO HEADING                                                
      * CALL DATE ROUTINE TO CONVERT MONTH FROM MM TO MMM                       
                                                                                
           MOVE 'B'                      TO VDATE-REQ-SERVICE.                  
           MOVE 'B'                      TO VDATE-REQ-BASIS.                    
           MOVE '1'                      TO VDATE-REQ-DETAIL.                   
           MOVE 'E'                      TO VDATE-REQ-LANGUAGE.                 
                                                                                
           MOVE WS-YEAR                  TO WS-YYYY.                            
           MOVE WS-MONTH                 TO WS-MM.                              
           MOVE WS-DAY                   TO WS-DD.                              
           MOVE WS-YYYYMMDD              TO VDATE1-YYYYMMDD.                    
                                                                                
           CALL WS-GC2DATE               USING GAC-DATE-PARAMETERS.             
                                                                                
           MOVE VDATE-EXT-DATE           TO WS-VDATE-EXT-DATE.                  
           MOVE WS-VDATE-EXT-MMM         TO P-HDR-3-TO-MONTH                    
                                            P-HDR-3-FROM-MONTH.                 
                                                                                
           MOVE WS-TO-YEAR               TO P-HDR-3-TO-YEAR.                    
           MOVE WS-FROM-YEAR             TO P-HDR-3-FROM-YEAR.                  
                                                                                
           MOVE WS-TO-DAY                TO P-HDR-3-TO-DAY.                     
           MOVE WS-FROM-DAY              TO P-HDR-3-FROM-DAY.                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1030-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1040-GET-DATES.                                                          
      *****************************************************************         
      * THIS PARAGRAPH IS USED IF THE DATES ARE NOT PASSED THROUGH              
      * CONTROL CARDS.  THIS JOB WILL RUN MONTHLY AFTER MIDNIGHT                
      * ON THE LAST DAY OF THE MONTH.  THE COUNTS WILL REFLECT THE              
      * PREVIOUS MONTH.                                                         
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1040-GET-DATES'         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             OPEN DATE_ENTRIES                                                  
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             FETCH DATE_ENTRIES                                                 
             INTO  :WS-TS-MTH                                                   
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH DATE ENTRIES'     TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
      * BUILD TO AND FROM TIMESTAMPS FOR MONTH                                  
                                                                                
           MOVE WS-TS-MTH                TO WS-TIMESTAMP.                       
           MOVE WS-YEAR                  TO WS-FROM-YEAR.                       
           MOVE WS-YEAR                  TO WS-TO-YEAR.                         
           MOVE WS-MONTH                 TO WS-FROM-MONTH.                      
           MOVE WS-MONTH                 TO WS-TO-MONTH.                        
           PERFORM 1045-GET-LAST-DAY-OF-MTH                                     
              THRU 1045-EXIT.                                                   
           MOVE WS-WORK-DAY              TO WS-TO-DAY.                          
                                                                                
           MOVE WS-BUILD-FROM-TS         TO WS-FROM-TS.                         
           MOVE WS-BUILD-TO-TS           TO WS-TO-TS.                           
                                                                                
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
                                                                                
       2000-PROCESS-REPORT-BY-BS.                                               
      *****************************************************************         
      *  - PROCESS REPORT FOR EACH BUSINESS SEGMENT                             
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS-REPORT-BY-BS'                                     
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-BS-CD (SUB1)          TO WS-BUS-SEG-CD.                      
           MOVE WS-BS-DESC (SUB1)        TO P-HDR-4-BS.                         
                                                                                
           MOVE 0                        TO WS-ACCM-USER-COUNT                  
                                            WS-ACCM-FAILED-LOGINS               
                                            WS-ACCM-SUCC-LOGINS.                
                                                                                
           MOVE SPACES                   TO WS-PREV-USER-ID.                    
                                                                                
           PERFORM 5000-PRT-HEADINGS                                            
              THRU 5000-EXIT.                                                   
                                                                                
           SET WS-EOF-NO                 TO TRUE.                               
                                                                                
           EXEC SQL                                                             
             OPEN ACTLOG                                                        
           END-EXEC.                                                            
                                                                                
           PERFORM 2100-PROCESS-USER                                            
              THRU 2100-EXIT                                                    
             UNTIL WS-EOF.                                                      
                                                                                
           EXEC SQL                                                             
             CLOSE ACTLOG                                                       
           END-EXEC.                                                            
                                                                                
           PERFORM 5100-PRT-REPORT-TOTALS                                       
              THRU 5100-EXIT.                                                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2000-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       2100-PROCESS-USER.                                                       
      *****************************************************************         
      *  - FETCH NEXT ACTIVE USER                                               
      *  - MOVE TO DETAIL LINE                                                  
      *  - FIND THE LAST SUCCESSFUL  LOGIN DATE                                 
      *  - COUNT FAILED LOGINS                                                  
      *  - COUNT SUCCESSFUL LOGINS                                              
      *  - PRINT LINE                                                           
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2100-PROCESS-USER'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             FETCH ACTLOG                                                       
             INTO  :DCLTUT.USER-ID                                              
                  ,:DCLTUT.CUST-ID                                              
                  ,:DCLTCUST.CUST-FIRST-NAME                                    
                  ,:DCLTCUST.CUST-LAST-NAME                                     
                  ,:DCLTCUST.SPONSOR-NAME                                       
                  ,:DCLTCUST.CUST-STAT-CD                                       
                  ,:DCLTCUST.CUST-STAT-REAS-CD                                  
                  ,:DCLTPAA.GROUP-ID                                            
                  ,:DCLTPAA.DIV-ID                                              
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH ACTLOG'          TO AB-MESSAGE.                          
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
           IF SQLCODE                   = +100                                  
               SET WS-EOF               TO TRUE                                 
           ELSE                                                                 
               PERFORM 2500-TEST-FOR-RPT                                        
                  THRU 2500-EXIT                                                
               IF WS-PRT-IN-RPT-YES                                             
                   ADD 1                 TO WS-ACCM-USER-COUNT                  
                   MOVE USER-ID OF DCLTUT                                       
                                         TO P-DETL-USER-ID                      
                   MOVE SPONSOR-NAME OF DCLTCUST                                
                                         TO P-DETL-SPONSOR-NAME                 
                   PERFORM 2200-FORMAT-NAME                                     
                      THRU 2200-EXIT                                            
                   PERFORM 2300-GET-LAST-LOGIN                                  
                      THRU 2300-EXIT                                            
                   PERFORM 2400-COUNT-LOGINS                                    
                      THRU 2400-EXIT                                            
                   PERFORM 5200-PRT-REPORT-LINE                                 
                      THRU 5200-EXIT                                            
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2200-FORMAT-NAME.                                                        
      *****************************************************************         
      * - REMOVE EXCESS SPACES FROM FIRST AND LAST NAME BEFORE                  
      *   PRINTING                                                              
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2200-FORMAT-NAME'       TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE CUST-FIRST-NAME          TO WS-NAME.                            
           PERFORM 2210-FIND-END                                                
              THRU 2210-EXIT                                                    
           VARYING SUB2 FROM 30 BY -1                                           
                   UNTIL SUB2 = 1 OR                                            
                         WS-NAME-CHAR (SUB2) NOT = SPACE.                       
                                                                                
           ADD 1                         TO SUB2.                               
           MOVE WS-DELIMITER             TO WS-NAME-CHAR (SUB2).                
           MOVE WS-NAME                  TO WS-FIRST-NAME.                      
                                                                                
           MOVE CUST-LAST-NAME           TO WS-LAST-NAME.                       
                                                                                
           MOVE SPACES                   TO WS-CUST-NAME.                       
                                                                                
           STRING WS-FIRST-NAME DELIMITED BY WS-DELIMITER                       
                  ' ' DELIMITED BY SIZE                                         
                  WS-LAST-NAME DELIMITED BY SIZE                                
                  INTO WS-CUST-NAME.                                            
                                                                                
           MOVE WS-CUST-NAME             TO P-DETL-CUST-NAME.                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2210-FIND-END.                                                           
      *****************************************************************         
      * - USED TO LOOK FOR THE END OF THE NAME                                  
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2210-FIND-END'          TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
        2210-EXIT.                                                              
            EXIT.                                                               
                                                                                
       2300-GET-LAST-LOGIN.                                                     
      *****************************************************************         
      * - FIND THE DATE OF THE LAST LOGIN FOR THIS USER                         
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2300-GET-LAST-LOGIN'    TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EXEC SQL                                                             
             OPEN LSTLOG                                                        
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
             FETCH LSTLOG                                                       
             INTO  :DCLTLH.LOGIN-TS                                             
           END-EXEC.                                                            
                                                                                
           MOVE 'FETCH FIRST LOGIN'      TO AB-MESSAGE.                         
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           IF SQLCODE                    =  +100                                
               MOVE WS-NO                TO P-DETL-PA-LOGIN                     
               SET WS-LOGIN-NO           TO TRUE                                
               MOVE SPACES               TO P-DETL-DATE-LAST-LOGIN              
           ELSE                                                                 
               MOVE WS-YES               TO P-DETL-PA-LOGIN                     
               SET WS-LOGIN-YES          TO TRUE                                
                                                                                
      * CALL DATE ROUTINE TO FORMAT DATE                                        
                                                                                
               MOVE 'B'                  TO VDATE-REQ-SERVICE                   
               MOVE 'B'                  TO VDATE-REQ-BASIS                     
               MOVE '1'                  TO VDATE-REQ-DETAIL                    
               MOVE 'E'                  TO VDATE-REQ-LANGUAGE                  
                                                                                
               MOVE LOGIN-TS OF DCLTLH   TO WS-TIMESTAMP                        
                                                                                
               MOVE WS-YEAR              TO WS-YYYY                             
               MOVE WS-MONTH             TO WS-MM                               
               MOVE WS-DAY               TO WS-DD                               
               MOVE WS-YYYYMMDD          TO VDATE1-YYYYMMDD                     
                                                                                
               CALL WS-GC2DATE           USING GAC-DATE-PARAMETERS              
                                                                                
               MOVE VDATE-EXT-DATE       TO WS-VDATE-EXT-DATE                   
               MOVE WS-VDATE-EXT-YYYY    TO P-DETL-YYYY                         
               MOVE WS-VDATE-EXT-MMM     TO P-DETL-MMM                          
               MOVE WS-VDATE-EXT-DD      TO P-DETL-DD                           
           END-IF.                                                              
                                                                                
           EXEC SQL                                                             
             CLOSE LSTLOG                                                       
           END-EXEC.                                                            
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2300-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2400-COUNT-LOGINS.                                                       
      *****************************************************************         
      * - COUNT THE SUCCESSFUL AND FAILED LOGINS FOR THIS USER                  
      *   DURNG THE TIME SPAN                                                   
      * - IF EXECUTION OF FIRST LOGIN FOUND NO LOGINS MOVE ZERO TO              
      *   PRINT LINE                                                            
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2400-COUNT-LOGINS'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           IF WS-LOGIN-YES                                                      
               EXEC SQL                                                         
                 SELECT COUNT (*)                                               
                 INTO   :WS-COUNT                                               
                 FROM   TLH                                                     
                 WHERE  CUST_ID          =  :DCLTUT.CUST-ID                     
                 AND    LOGIN_SUCC_IND   =  :WS-LOGIN-SUCC-IND                  
                 AND    LOGIN_TS         >= :WS-FROM-TS                         
                 AND    LOGIN_TS         <= :WS-TO-TS                           
               END-EXEC                                                         
                                                                                
               MOVE 'SELECT SUCC LOGIN'  TO AB-MESSAGE                          
               PERFORM 8900-CHECK-SQL-CODE                                      
                  THRU 8900-EXIT                                                
                                                                                
               IF SQLCODE = 0                                                   
                   MOVE WS-COUNT         TO P-DETL-SUCCESSFUL                   
                   ADD  WS-COUNT         TO WS-ACCM-SUCC-LOGINS                 
               ELSE                                                             
                   MOVE 0                TO P-DETL-SUCCESSFUL                   
               END-IF                                                           
                                                                                
               EXEC SQL                                                         
                 SELECT COUNT (*)                                               
                 INTO   :WS-COUNT                                               
                 FROM   TLH                                                     
                 WHERE  CUST_ID          =  :DCLTUT.CUST-ID                     
                 AND    LOGIN_SUCC_IND   =  :WS-LOGIN-FAIL-IND                  
                 AND    LOGIN_TS         >= :WS-FROM-TS                         
                 AND    LOGIN_TS         <= :WS-TO-TS                           
               END-EXEC                                                         
                                                                                
               MOVE 'SELECT FAIL LOGIN'  TO AB-MESSAGE                          
               PERFORM 8900-CHECK-SQL-CODE                                      
                  THRU 8900-EXIT                                                
                                                                                
               IF SQLCODE = 0                                                   
                   MOVE WS-COUNT         TO P-DETL-FAILED                       
                   ADD  WS-COUNT         TO WS-ACCM-FAILED-LOGINS               
               ELSE                                                             
                   MOVE 0                TO P-DETL-FAILED                       
               END-IF                                                           
           ELSE                                                                 
               MOVE 0                    TO P-DETL-FAILED                       
                                            P-DETL-SUCCESSFUL                   
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2400-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2500-TEST-FOR-RPT.                                                       
      *****************************************************************         
      * - DON'T PROCESS FOR TERMINATED CUSTOMERS (PA'S)                         
      * - USE THE BUSINESS SEGMENT OF THE FIRST GROUP/DIVISION                  
      *   A USER ID HAS ACCESS TO, TO DECIDE WHICH REPORT THE                   
      *   USER IF WILL BE REPORTED IN                                           
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2500-TEST-FOR-RPT'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           SET WS-PRT-IN-RPT-NO          TO TRUE                                
                                                                                
      * CHECK FOR TERMINATED CUSTOMERS                                          
                                                                                
           IF CUST-STAT-CD OF DCLTCUST        =  'S' AND                        
              CUST-STAT-REAS-CD OF DCLTCUST   =  'T'                            
               SET WS-PRT-IN-RPT-NO      TO TRUE                                
           ELSE                                                                 
                                                                                
      * CHECK WHETHER THIS IS A NEW USER ID                                     
                                                                                
               IF WS-PREV-USER-ID    NOT = USER-ID OF DCLTUT                    
                   MOVE USER-ID OF DCLTUT                                       
                                         TO WS-PREV-USER-ID                     
                   MOVE SPACES           TO BUS-SEG-CD OF DCLTGD                
                                                                                
                   EXEC SQL                                                     
                     SELECT BUS_SEG_CD                                          
                     INTO   :DCLTGD.BUS-SEG-CD                                  
                     FROM   TGD                                                 
                     WHERE  GROUP_ID     =  :DCLTPAA.GROUP-ID                   
                     AND    DIV_ID       =  :DCLTPAA.DIV-ID                     
                   END-EXEC                                                     
                                                                                
                   PERFORM 8900-CHECK-SQL-CODE                                  
                      THRU 8900-EXIT                                            
                                                                                
      * IS THE FIRST GROUP/DIV FOR THIS USER ID IN THE BUSINESS SEGMENT         
      * CURRENTLY BEING PROCESSED                                               
                                                                                
                   IF BUS-SEG-CD OF DCLTGD    =  WS-BUS-SEG-CD                  
                       SET WS-PRT-IN-RPT-YES  TO TRUE                           
                   ELSE                                                         
                       SET WS-PRT-IN-RPT-NO   TO TRUE                           
                   END-IF                                                       
               ELSE                                                             
                   SET WS-PRT-IN-RPT-NO       TO TRUE                           
               END-IF                                                           
           END-IF.                                                              
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2500-EXIT.                                                               
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
                                                                                
                                                                                
           MOVE P-BLK-LINE               TO PRINT-RECORD.                       
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
                                                                                
           IF WS-ACCM-USER-COUNT         >  0                                   
               MOVE WS-ACCM-USER-COUNT   TO P-TOTAL-USERS                       
               MOVE WS-ACCM-FAILED-LOGINS                                       
                                         TO P-TOTAL-FAILED                      
               MOVE WS-ACCM-SUCC-LOGINS  TO P-TOTAL-SUCCESSFUL                  
               MOVE P-TOTAL              TO PRINT-RECORD                        
               PERFORM 7000-PRINT-OUTPUT                                        
                  THRU 7000-EXIT                                                
           ELSE                                                                 
               MOVE P-NO-USERS           TO PRINT-RECORD                        
               PERFORM 7000-PRINT-OUTPUT                                        
                  THRU 7000-EXIT                                                
           END-IF.                                                              
                                                                                
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       5100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       5200-PRT-REPORT-LINE.                                                    
      *****************************************************************         
      *  - PRINT LINE OF REPORT                                                 
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '5200-PRT-REPORT-LINE'   TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           IF WS-LINE-COUNT              =  WS-LINE-MAX                         
               PERFORM 5000-PRT-HEADINGS                                        
                  THRU 5000-EXIT                                                
           END-IF.                                                              
                                                                                
           ADD 1                         TO WS-LINE-COUNT.                      
           MOVE P-DETL                   TO PRINT-RECORD.                       
           PERFORM 7000-PRINT-OUTPUT                                            
              THRU 7000-EXIT.                                                   
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       5200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6000-READ-INPUT.                                                         
      *****************************************************************         
      *  - READ 1 CONTROL CARD                                                  
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6000-READ-INPUT'        TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-INPUT-LR              TO LOGICAL-RECORD-NAME.                
           INITIALIZE GCCCCRDL-RECORD.                                          
                                                                                
           CALL WS-GAEDATSR              USING WS-GAEDATSR-VERB                 
                                               GCCCCRDL-RECORD                  
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
