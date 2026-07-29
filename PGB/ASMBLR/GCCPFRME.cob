       CBL FLAG(I),DATA(24)                                                     
      *                                                                         
GNE                                                                             
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPFRME.                                                 
      *AUTHOR.        KLYN.                                                     
      *DATE-WRITTEN.  NOV 6, 2000.                                              
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS)                                                      
      *   GCCPFRME - EXTRACT OF E-FORMS FOR PRINT ROUTINES                      
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *                                                                         
      *   THIS PROGRAM WILL SELECT FORMS BASED ON THE RE-PRINT CONTROL          
      *   CARD OR IT WILL SELECT ALL FORMS                                      
      *   WHICH HAVE NOT YET BEEN PRINTED.                                      
      *                                                                         
      *   THE PROGRAM WILL READ A SELECTION PARAMETER AND GET                   
      *   CURRENT TIMESTAMP.                                                    
      *   OPEN A DB2 CURSOR WHICH MATCHES THE PARAMETER SELECTIONS              
      *   ALONG WITH THE SUBMISSION DATA TABLE, FORM TABLE AND                  
      *   GROUP DIVISION TABLE.                                                 
      *   THIS REQUIRED INFORMATION IS OUTPUT TO A VARIABLE LENGTH              
      *   FLAT EXTRACT FILE.                                                    
      *   ONCE THIS FILE RECORD HAS BEEN SUCCESSFULLY CREATED THE               
      *   SENT_TO_PRINT FIELD ON THE SUBMISSION DATA TABLE IS UPDATED           
      *   WITH THE CURRENT TIMESSTAMP.                                          
      *   IF THE PARAMETER IS NOT PRESENT, ALL VALID CURSOR MATCHES ARE         
      *   SELECTED FOR EXTRACT WHEN RECORD HAS BEEN SUCCESSFULLY CREATED        
      *   THE SENT_TO_PRINT FIELD ON THE SUBMISSION DATA TABLE IS               
      *   UPDATED  WITH THE CURRENT TIMESSTAMP.                                 
      *   NOTE THE SENT_TO_PRINT FIELD IS NOT UPDATED DURING A RERUN OR         
      *   REPRINT                                                               
      *                                                                         
      *   INPUT FILES:  SELECTION CRITERIA  (PARM)                              
      *                 TGD     - GROUP DIVISION TABLE                          
      *                 TFORM   - FORMS TABLE                                   
      *                 TSD     - SUBMISSION TABLE                              
      *                                                                         
      *   OUTPUT FILES: AFP FORM EXTRACT                                        
      *                                                                         
      *   DB2 TABLES                                                            
      *   ACCESSED:     TGD     - GROUP DIVISION TABLE - FETCH                  
      *                 TFORM   - FORMS TABLE          - FETCH                  
      *                 TSD     - SUBMISSION TABLE     - FETCH/UPDATE           
      *                                                                         
      *   CALLS:        GAEDATSR- FILE I/O                                      
      *                                                                         
      *   INCLUDE CODE: SQLCA    - SQL COMMUNICATION AREA                       
      *                                                                         
      *                 TGDD     - TGD     TABLE DECLARATION                    
      *                 TFORMD   - TFORM   TABLE DECLARATION                    
      *                 TSDD     - TSD     TABLE DECLARATION                    
      *                                                                         
      *                 TGD      - TGD     HOST VARIABLES                       
      *                 TFORM    - TFORM   HOST VARIABLES                       
      *                 TSD      - TSD     HOST VARIABLES                       
      *                                                                         
      *                 TGDI     - TGD     INDICATOR VARIABLES                  
      *                 TSDI     - TSD     INDICATOR VARIABLES                  
      *                                                                         
      *                                                                         
      *                 GCCCRPRM - REPRINT PARM RECORD LAYOUT                   
      *                 GCCCFEXT - EXTRACT FILE LAYOUT                          
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
      * KLYN         ‡06/11/00‡ ORIGINAL CODE                                   
      *              ‡        ‡                                                 
      * KLYN         ‡17/01/01‡ ADD RGO 15750 PROCESS FOR MONTREAL              
      *              ‡        ‡ AND OTHER V3.1 CHANGES                          
      * JUDY ELKINS  |27/04/01| ALLOW FOR CONT-STATUS OF 'T'. CONTRACT          
      *              |        | STATUS ORIGINATES FROM CONTRACT_STAT_CD         
      *              |        | ON THE TGD TABLE                                
      * JUDY ELKINS  |31/05/01| INCLUDE TURNAROUND TIME WITH DATE AND           
      *              |        | TIME STAMP AT THE TOP OF EACH FORM              
      * JUDY ELKINS  |13/06/01| INCLUDE TURNAROUND TIME IN SORT FIELDS          
      * JOHN/JACK    |07/10/02| ADD NEW CURSOR EXTR_FORMS_RERUN_A               
      *              |        | FOR ENTIRE BATCH RE-PRINTS EFFICIENCY.          
      * JBROSSARD    |07/25/02| ALLOW 31ST DAY TO BE PROCESSED                  
      * J WODNICKI   |03/18/03| RECOMPILE FOR GCCCRPRM CHANGES. T26127          
      *--------------+--------+-----------------------------------------        
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
           EXEC SQL INCLUDE TFORMD                                              
                                    END-EXEC.                                   
           EXEC SQL INCLUDE TSDD                                                
                                    END-EXEC.                                   
                                                                                
      *** DB2 HOST & INDICATOR VARIABLES                                        
                                                                                
      *01  DCLTGD.                                                              
           EXEC SQL INCLUDE TMXKEY                                              
                                    END-EXEC.                                   
                                                                                
      *01  DCLFORM.                                                             
           EXEC SQL INCLUDE TFORM                                               
                                    END-EXEC.                                   
                                                                                
      *01  DCLTSD.                                                              
           EXEC SQL INCLUDE TSD                                                 
                                    END-EXEC.                                   
           EXEC SQL INCLUDE TSDI                                                
                                    END-EXEC.                                   
                                                                                
      *01 DUMMY-DATE-TIME.                                                      
           EXEC SQL INCLUDE TCTFRMD                                             
                                    END-EXEC.                                   
           EXEC SQL INCLUDE TCTFRM                                              
                                    END-EXEC.                                   
                                                                                
      *----------------------------------------------------------------*        
      *    REPRINT PARM FILE LAYOUT                                             
      *----------------------------------------------------------------*        
       01  GCCCRPRM-RECORD.                                                     
                                        COPY  GCCCRPRM.                         
                                                                                
      *----------------------------------------------------------------*        
      *    TEMPORARY PARM FILE LAYOUT                                           
      *----------------------------------------------------------------*        
      *                                                                         
       01 WS-TEMP-PARM-RECORD.                                                  
           05  WS-SQL-PARM-SENT-FROM               PIC X(26).                   
           05  WS-SQL-PARM-SENT-TO                 PIC X(26).                   
           05  WS-TEMP-PARM-SENT-FROM              PIC X(26)  VALUE             
               '1999-01-01-00.01.01.000001'.                                    
           05  FILLER REDEFINES WS-TEMP-PARM-SENT-FROM.                         
               10  WS-TEMP-PARM-FROM-YYYY          PIC 9(4).                    
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-FROM-MM            PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-FROM-DD            PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-FROM-HH            PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-FROM-MIN           PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-FROM-SEC           PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-FROM-NN            PIC 9(6).                    
           05  WS-TEMP-PARM-SENT-TO                PIC X(26)  VALUE             
               '9999-01-01-00.01.01.000001'.                                    
           05  FILLER REDEFINES WS-TEMP-PARM-SENT-TO.                           
               10  WS-TEMP-PARM-TO-YYYY            PIC 9(4).                    
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-TO-MM              PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-TO-DD              PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-TO-HH              PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-TO-MIN             PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-TO-SEC             PIC 99.                      
               10  FILLER                          PIC X.                       
               10  WS-TEMP-PARM-TO-NN              PIC 9(6).                    
           05  WS-TEMP-PARM-FORM-NAME-FROM         PIC X(8).                    
           05  WS-TEMP-PARM-FORM-NAME-TO           PIC X(8).                    
           05  WS-TEMP-PARM-GROUP-FROM             PIC X(7).                    
           05  WS-TEMP-PARM-GROUP-TO               PIC X(7).                    
           05  WS-TEMP-PARM-DIV-FROM               PIC X(3).                    
           05  WS-TEMP-PARM-DIV-TO                 PIC X(3).                    
           05  WS-TEMP-PARM-CERT-FROM              PIC X(10).                   
           05  WS-TEMP-PARM-CERT-TO                PIC X(10).                   
           05  WS-TEMP-PARM-CONF-FROM      PIC S9(11) COMP-3.                   
           05  WS-TEMP-PARM-CONF-TO        PIC S9(11) COMP-3.                   
                                                                                
       01 WS-TEMP-TIMESTAMP.                                                    
           05  WS-TEMP-WEB-SENT-TIMESTAMP  PIC X(26).                           
           05  FILLER   REDEFINES WS-TEMP-WEB-SENT-TIMESTAMP.                   
               10  WS-TEMP-WST-DATE        PIC X(10).                           
               10  FILLER                  PIC X.                               
               10  WS-TEMP-WST-TIME        PIC X(5).                            
               10  FILLER                  PIC X(10).                           
           05  WS-TEMP-TAT                 PIC X(2) VALUE SPACES.               
           05  WS-REFORM-WS-TIMESTAMP.                                          
               10  FILLER                  PIC X(8)   VALUE 'DATE:'.            
               10  WS-REFORM-WST-DATE      PIC X(15).                           
               10  FILLER                  PIC X(8)   VALUE 'TIME:'.            
               10  WS-REFORM-WST-TIME      PIC X(5).                            
               10  FILLER                  PIC X(6)  VALUE                      
                   ' TAT:'.                                                     
               10  WS-REFORM-TURN-TIME     PIC X(6).                            
       01 WS-TEMP-EXTR-RECORD.                                                  
           05  WS-TEMP-EXT-HEADER.                                              
               10  WS-TEMP-EXT-LANG                PIC X.                       
               10  WS-TEMP-EXT-CONT-STATUS         PIC X.                       
               10  WS-TEMP-EXT-SORT-CAT0           PIC XX.                      
               10  WS-TEMP-EXT-SORT-CAT1           PIC X.                       
               10  WS-TEMP-EXT-SORT-CAT2           PIC X.                       
               10  WS-TEMP-EXT-REGION              PIC X.                       
               10  WS-TEMP-EXT-BUS-SEG             PIC X.                       
               10  WS-TEMP-EXT-GROUP               PIC X(7).                    
               10  WS-TEMP-EXT-DIV                 PIC X(3).                    
               10  WS-TEMP-EXT-FORM-NBR            PIC X(8).                    
               10  WS-TEMP-EXT-CERT-ID             PIC X(10).                   
               10  WS-TEMP-EXT-CONF-NBR            PIC S9(11) COMP-3.           
               10  WS-TEMP-EXT-FORM-DATA.                                       
                   15  WS-TEMP-EXT-FORM-LENGTH     PIC S9(4)  COMP.             
                   15  WS-TEMP-EXT-FORM-DETL       PIC X(3959).                 
      *----------------------------------------------------------------*        
      *    EXTRACT OUTPUT FILE LAYOUT                                           
      *----------------------------------------------------------------*        
       01 GCCCFEXT-RECORD.                                                      
          03  GCCCFEXT-RECL                PIC S9(4)  COMP.                     
          03  FILLER                       PIC XX     VALUE  SPACES.            
          03 GCCCFEXT-DETAIL.                                                   
                                        COPY GCCCFEXT.                          
                                                                                
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-VARIABLES.                                                        
           05  WS-TEMP-RGO                 PIC X(5).                            
           05  WS-MATCH-SEQ-NO             PIC S9(5) COMP-3  VALUE 1.           
           05  WS-TIMESTAMP                PIC X(26)    VALUE SPACES.           
           05  WS-INPUT-EOF-MKR            PIC X        VALUE 'N'.              
               88  WS-INPUT-EOF                         VALUE 'Y'.              
           05  WS-INPUT-OPEN-MKR           PIC X        VALUE 'N'.              
               88  WS-INPUT-OPEN                        VALUE 'Y'.              
           05  WS-OUTPUT-OPEN-MKR           PIC X        VALUE 'N'.             
               88  WS-OUTPUT-OPEN                        VALUE 'Y'.             
           05  WS-RERUN-MKR                 PIC X        VALUE 'N'.             
               88  WS-RERUN                              VALUE 'Y'.             
           05  WS-OBTAIN-FIRST             PIC X(16)   VALUE                    
                                               'OBTAIN  FIRST   '.              
           05  WS-OBTAIN-NEXT              PIC X(16)   VALUE                    
                                               'OBTAIN  NEXT    '.              
           05  WS-STORE-LR                 PIC X(16) VALUE                      
                                               'STORE           '.              
           05  WS-INPUT-LR                 PIC X(16) VALUE                      
                                               'CARD-DATA-010   '.              
           05  WS-OUTPUT-LR                PIC X(16) VALUE                      
                                               'PRINT-DATA-070  '.              
           05  WS-DELIMITER                PIC X     VALUE '%'.                 
           05  WS-UPD-FORM-ID              PIC S9(11)V USAGE COMP-3.            
           05  WS-UPD-FORM-SEQ-NUM         PIC S9(3)V USAGE COMP-3.             
           05  WS-TSD-SEQ-NUM              PIC S9(5)V USAGE COMP-3.             
           05  WS-MAS-SEQ-NUM              PIC S9(5)V USAGE COMP-3.             
                                                                                
      *                                                                         
      *****************************************************************         
      *** CONSTANTS                                                             
      *****************************************************************         
      *                                                                         
       01  WS-CALLING-VARIABLES.                                                
           05  WS-GAEDATSR                 PIC X(08)                            
                                          VALUE 'GAEDATSR'.                     
           05  WS-GAEDATSR-VERB            PIC X(16).                           
      *----------------------------------------------------------------*        
      *    ACTION VERBS USED TO CALL GAEDATSR                                   
      *----------------------------------------------------------------*        
       01  DATA-SERVER-VERBS.                                                   
           COPY GARDSVRB.                                                       
                                                                                
      *****************************************************************         
      *** VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME              PIC X(60) VALUE                      
                'GCCPFRME - FORMS EXTRACT FOR PRINT'.                           
           10  AB-PARAGRAPH-NAME  OCCURS 25 TIMES                               
                                           PIC X(60).                           
           10  AB-MESSAGE.                                                      
               15  AB-MSG1                 PIC X(70).                           
               15  AB-MSG2                 PIC X(70).                           
           10  AB-SQLCODE                  PIC ----9.                           
           10  LVL                         PIC S9(4) COMP.                      
           10  CNT                         PIC S9(4) COMP.                      
                                                                                
      *****************************************************************         
      *** DB2 CURSOR DECLARE                                                    
      *****************************************************************         
                                                                                
      * RETRIEVE ALL OUTSTANDING EXTRACT RECORDS NOT                            
      * YET SENT TO PRINT WHEN NO PARAMETER INPUT TO PROGRAM                    
                                                                                
           EXEC SQL                                                             
             DECLARE EXTR_FORMS_WHOLE CURSOR FOR                                
             SELECT  A.LANG_CD                                                  
                    ,A.FORM_CD                                                  
                    ,C.CONTRACT_STAT_CD                                         
                    ,C.REG_CD                                                   
                    ,C.BUS_SEG_CD                                               
                    ,C.REG_GROUP_OFFICE                                         
                    ,A.CERT_ID                                                  
                    ,A.CONFIRM_ID                                               
                    ,A.SEQ_NUM                                                  
                    ,B.FORM_SEQ_NUM                                             
                    ,A.GROUP_ID                                                 
                    ,A.DIV_ID                                                   
                    ,A.FORM_ID                                                  
                    ,A.FORM_SEQ_NUM                                             
                    ,A.WEB_SENT_TS                                              
                    ,C.TAT                                                      
                    ,B.FORM_DATA_STRING                                         
              FROM TSD            A                                             
                  ,TFORM          B                                             
                  ,TGD            C                                             
              WHERE A.GROUP_ID       =  C.GROUP_ID                              
                AND A.DIV_ID         =  C.DIV_ID                                
                AND A.FORM_ID        =  B.FORM_ID                               
                AND A.PURGE_TS      IS NULL                                     
                AND A.SENT_TO_PRINT_TS IS NULL                                  
              ORDER BY B.FORM_ID, B.FORM_SEQ_NUM                                
           END-EXEC.                                                            
                                                                                
      *         AND B.FORM_SEQ_NUM   = :WS-MATCH-SEQ-NO                         
                                                                                
      *****************************************************************         
      *** DB2 CURSOR DECLARE                                                    
      *****************************************************************         
                                                                                
      * RETRIEVE ALL OUTSTANDING EXTRACT RECORDS WHICH MEET THE                 
      * REPRINT PARAMETER CRITERIA                                              
                                                                                
           EXEC SQL                                                             
             DECLARE EXTR_FORMS_RERUN CURSOR FOR                                
             SELECT  A.LANG_CD                                                  
                    ,A.FORM_CD                                                  
                    ,C.CONTRACT_STAT_CD                                         
                    ,C.REG_CD                                                   
                    ,C.BUS_SEG_CD                                               
                    ,C.REG_GROUP_OFFICE                                         
                    ,A.CERT_ID                                                  
                    ,A.CONFIRM_ID                                               
                    ,A.SEQ_NUM                                                  
                    ,B.FORM_SEQ_NUM                                             
                    ,A.GROUP_ID                                                 
                    ,A.DIV_ID                                                   
                    ,A.FORM_ID                                                  
                    ,A.FORM_SEQ_NUM                                             
                    ,A.WEB_SENT_TS                                              
                    ,C.TAT                                                      
                    ,B.FORM_DATA_STRING                                         
              FROM TSD            A                                             
                  ,TFORM          B                                             
                  ,TGD            C                                             
              WHERE A.GROUP_ID       =  C.GROUP_ID                              
                AND A.DIV_ID         =  C.DIV_ID                                
                AND A.FORM_ID        =  B.FORM_ID                               
                AND A.PURGE_TS      IS NULL                                     
                AND A.SENT_TO_PRINT_TS >=                                       
                                 :WS-SQL-PARM-SENT-FROM                         
                AND A.SENT_TO_PRINT_TS <=                                       
                                 :WS-SQL-PARM-SENT-TO                           
                AND A.FORM_CD          >= :WS-TEMP-PARM-FORM-NAME-FROM          
                AND A.FORM_CD          <= :WS-TEMP-PARM-FORM-NAME-TO            
                AND A.GROUP_ID         >= :WS-TEMP-PARM-GROUP-FROM              
                AND A.GROUP_ID         <= :WS-TEMP-PARM-GROUP-TO                
                AND A.DIV_ID           >= :WS-TEMP-PARM-DIV-FROM                
                AND A.DIV_ID           <= :WS-TEMP-PARM-DIV-TO                  
                AND A.CERT_ID          >= :WS-TEMP-PARM-CERT-FROM               
                AND A.CERT_ID          <= :WS-TEMP-PARM-CERT-TO                 
                AND A.CONFIRM_ID       >= :WS-TEMP-PARM-CONF-FROM               
                AND A.CONFIRM_ID       <= :WS-TEMP-PARM-CONF-TO                 
              ORDER BY B.FORM_ID, B.FORM_SEQ_NUM                                
           END-EXEC.                                                            
                                                                                
      *         AND B.FORM_SEQ_NUM   = :WS-MATCH-SEQ-NO                         
                                                                                
      *****************************************************************         
      *** DB2 CURSOR DECLARE                                                    
      *****************************************************************         
                                                                                
      * RETRIEVE ALL OUTSTANDING EXTRACT RECORDS FOR THE GIVEN DATE             
      * RANGE                                                                   
                                                                                
           EXEC SQL                                                             
             DECLARE EXTR_FORMS_RERUN_A CURSOR FOR                              
             SELECT  A.LANG_CD                                                  
                    ,A.FORM_CD                                                  
                    ,C.CONTRACT_STAT_CD                                         
                    ,C.REG_CD                                                   
                    ,C.BUS_SEG_CD                                               
                    ,C.REG_GROUP_OFFICE                                         
                    ,A.CERT_ID                                                  
                    ,A.CONFIRM_ID                                               
                    ,A.SEQ_NUM                                                  
                    ,B.FORM_SEQ_NUM                                             
                    ,A.GROUP_ID                                                 
                    ,A.DIV_ID                                                   
                    ,A.FORM_ID                                                  
                    ,A.FORM_SEQ_NUM                                             
                    ,A.WEB_SENT_TS                                              
                    ,C.TAT                                                      
                    ,B.FORM_DATA_STRING                                         
              FROM TSD            A                                             
                  ,TFORM          B                                             
                  ,TGD            C                                             
              WHERE A.GROUP_ID       =  C.GROUP_ID                              
                AND A.DIV_ID         =  C.DIV_ID                                
                AND A.FORM_ID        =  B.FORM_ID                               
                AND A.PURGE_TS      IS NULL                                     
                AND A.SENT_TO_PRINT_TS >=                                       
                                 :WS-SQL-PARM-SENT-FROM                         
                AND A.SENT_TO_PRINT_TS <=                                       
                                 :WS-SQL-PARM-SENT-TO                           
              ORDER BY B.FORM_ID, B.FORM_SEQ_NUM                                
           END-EXEC.                                                            
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
      ****************************************************************          
      *********   P R O C E D U R E   D I V I S I O N   **************          
      ****************************************************************          
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           MOVE 1 TO LVL.                                                       
           MOVE '0000-MAINLINE' TO AB-PARAGRAPH-NAME (LVL).                     
                                                                                
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
                                                                                
           PERFORM 2000-PROCESS         THRU 2000-EXIT.                         
                                                                                
           PERFORM 3000-COMPLETION      THRU 3000-EXIT.                         
                                                                                
           GOBACK.                                                              
                                                                                
       1000-INITIALIZATION.                                                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE 'GCCPFRME'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
                                                                                
      ******************************************************************        
      * HANDLE CONTROL CARD                                                     
      ******************************************************************        
                                                                                
           MOVE WS-OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                       
                                                                                
           PERFORM 6100-READ-INPUT THRU                                         
                   6100-READ-INPUT-EXIT.                                        
                                                                                
           PERFORM 1100-VALIDATE-PARM THRU 1100-EXIT.                           
                                                                                
           PERFORM 6300-CLOSE-INPUT THRU 6300-EXIT.                             
                                                                                
           EXEC SQL                                                             
                 SELECT                                                         
                     CURRENT TIMESTAMP                                          
                   INTO                                                         
                      :WS-TIMESTAMP                                             
                   FROM                                                         
                      TCTFRM                                                    
                   WHERE                                                        
                      CODE_VALUE   = 'GL0003  '                                 
           END-EXEC.                                                            
                                                                                
           MOVE 'GET TIMESTAMP' TO AB-MESSAGE.                                  
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
      ******************************************************************        
      * VALIDATE/INITIALIZE PARM DATA                                           
      ******************************************************************        
                                                                                
       1100-VALIDATE-PARM.                                                      
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1100-VALIDATE-PARM' TO AB-PARAGRAPH-NAME (LVL).                
                                                                                
                MOVE     LOW-VALUES  TO  WS-TEMP-PARM-FORM-NAME-FROM            
                                         WS-TEMP-PARM-GROUP-FROM                
                                         WS-TEMP-PARM-DIV-FROM                  
                                         WS-TEMP-PARM-CERT-FROM.                
                                                                                
                MOVE     HIGH-VALUES TO  WS-TEMP-PARM-FORM-NAME-TO              
                                         WS-TEMP-PARM-DIV-TO                    
                                         WS-TEMP-PARM-GROUP-TO                  
                                         WS-TEMP-PARM-CERT-TO.                  
                                                                                
           MOVE  0                   TO  WS-TEMP-PARM-CONF-FROM.                
           MOVE  99999999999         TO  WS-TEMP-PARM-CONF-TO.                  
                                                                                
           IF  WS-INPUT-EOF                                                     
               NEXT SENTENCE                                                    
             ELSE                                                               
           IF  GCCCRPRM-ASTERIX  =  ' '                                         
               PERFORM  1150-LOAD-PARM THRU 1150-EXIT.                          
                                                                                
           MOVE  WS-TEMP-PARM-SENT-FROM  TO  WS-SQL-PARM-SENT-FROM.             
           MOVE  WS-TEMP-PARM-SENT-TO    TO  WS-SQL-PARM-SENT-TO.               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1150-LOAD-PARM.                                                          
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1150-LOAD-PARM' TO AB-PARAGRAPH-NAME (LVL).                    
                                                                                
           MOVE 'Y'   TO WS-RERUN-MKR.                                          
                                                                                
           IF  GCCCRPRM-SENT-TO-PRINT-TS-FROM  =  SPACES                        
               NEXT SENTENCE                                                    
             ELSE                                                               
               PERFORM 1160-LOAD-FROM-DATE THRU 1160-EXIT.                      
                                                                                
           IF  GCCCRPRM-SENT-TO-PRINT-TS-TO    =  SPACES                        
               NEXT SENTENCE                                                    
             ELSE                                                               
               PERFORM 1170-LOAD-TO-DATE THRU 1170-EXIT.                        
                                                                                
           IF  GCCCRPRM-FORM-NAME  NOT  =  SPACES                               
           MOVE  GCCCRPRM-FORM-NAME  TO  WS-TEMP-PARM-FORM-NAME-TO              
                                         WS-TEMP-PARM-FORM-NAME-FROM.           
                                                                                
           IF  GCCCRPRM-GROUP      NOT  =  SPACES                               
           MOVE  GCCCRPRM-GROUP      TO  WS-TEMP-PARM-GROUP-TO                  
                                         WS-TEMP-PARM-GROUP-FROM.               
                                                                                
           IF  GCCCRPRM-DIV        NOT  =  SPACES                               
           MOVE  GCCCRPRM-DIV        TO  WS-TEMP-PARM-DIV-TO                    
                                         WS-TEMP-PARM-DIV-FROM.                 
                                                                                
           IF  GCCCRPRM-CERT       NOT  =  SPACES                               
           MOVE  GCCCRPRM-CERT       TO  WS-TEMP-PARM-CERT-TO                   
                                         WS-TEMP-PARM-CERT-FROM.                
                                                                                
           IF  GCCCRPRM-CONF       NOT  =  SPACES                               
           MOVE  GCCCRPRM-CONF       TO  WS-TEMP-PARM-CONF-TO                   
                                         WS-TEMP-PARM-CONF-FROM.                
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1150-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1160-LOAD-FROM-DATE.                                                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE ' 1160-LOAD-FROM-DATE' TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
           IF  GCCCRPRM-STP-TS-FROM-YYYY   >   1999                             
               MOVE GCCCRPRM-STP-TS-FROM-YYYY TO WS-TEMP-PARM-FROM-YYYY.        
                                                                                
           IF  GCCCRPRM-STP-TS-FROM-MM     >   0  AND  <  13                    
               MOVE GCCCRPRM-STP-TS-FROM-MM   TO WS-TEMP-PARM-FROM-MM.          
                                                                                
           IF  GCCCRPRM-STP-TS-FROM-DD     >   0  AND  <  32                    
               MOVE GCCCRPRM-STP-TS-FROM-DD   TO WS-TEMP-PARM-FROM-DD.          
                                                                                
           IF  GCCCRPRM-STP-TS-FROM-HH     <  25                                
               MOVE GCCCRPRM-STP-TS-FROM-HH   TO WS-TEMP-PARM-FROM-HH.          
                                                                                
           IF  GCCCRPRM-STP-TS-FROM-MIN    <  60                                
               MOVE GCCCRPRM-STP-TS-FROM-MIN  TO WS-TEMP-PARM-FROM-MIN.         
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1160-EXIT.                                                               
           EXIT.                                                                
                                                                                
       1170-LOAD-TO-DATE.                                                       
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE ' 1170-LOAD-TO-DATE' TO AB-PARAGRAPH-NAME (LVL).                
                                                                                
           IF  GCCCRPRM-STP-TS-TO-YYYY     >   1999                             
               MOVE GCCCRPRM-STP-TS-TO-YYYY TO WS-TEMP-PARM-TO-YYYY.            
                                                                                
           IF  GCCCRPRM-STP-TS-TO-MM       >   0  AND  <  13                    
               MOVE GCCCRPRM-STP-TS-TO-MM     TO WS-TEMP-PARM-TO-MM.            
                                                                                
           IF  GCCCRPRM-STP-TS-TO-DD       >   0  AND  <  32                    
               MOVE GCCCRPRM-STP-TS-TO-DD     TO WS-TEMP-PARM-TO-DD.            
                                                                                
           IF  GCCCRPRM-STP-TS-TO-HH       <  25                                
               MOVE GCCCRPRM-STP-TS-TO-HH     TO WS-TEMP-PARM-TO-HH.            
                                                                                
           IF  GCCCRPRM-STP-TS-TO-MIN      <  60                                
               MOVE GCCCRPRM-STP-TS-TO-MIN    TO WS-TEMP-PARM-TO-MIN.           
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       1170-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-PROCESS.                                                            
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS' TO AB-PARAGRAPH-NAME (LVL).                      
                                                                                
           IF  WS-RERUN                                                         
               IF  GCCCRPRM-FORM-NAME  =  SPACES AND                            
                   GCCCRPRM-GROUP      =  SPACES AND                            
                   GCCCRPRM-DIV        =  SPACES AND                            
                   GCCCRPRM-CERT       =  SPACES AND                            
                   GCCCRPRM-CONF       =  SPACES                                
                   PERFORM 2060-PROCESS-RERUN-A THRU 2060-EXIT                  
               ELSE                                                             
                   PERFORM 2050-PROCESS-RERUN THRU 2050-EXIT                    
             ELSE                                                               
               PERFORM 2075-PROCESS-WHOLE THRU 2075-EXIT.                       
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2000-EXIT.                                                               
            EXIT.                                                               
                                                                                
       2050-PROCESS-RERUN.                                                      
      *                                                                         
      *    NOTE. THE SENT TO PRINT FIELD IS NOT UPDATED                         
      *          DURING A RERUN/REPRINT                                         
      *                                                                         
           ADD 1 TO LVL.                                                        
           MOVE '2050-PROCESS-RERUN' TO AB-PARAGRAPH-NAME (LVL).                
                                                                                
           EXEC SQL                                                             
             OPEN EXTR_FORMS_RERUN                                              
           END-EXEC.                                                            
                                                                                
           MOVE 'OPEN EXTR_FORMS_RERUN CURSOR' TO AB-MESSAGE.                   
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
                                                                                
           PERFORM                                                              
              WITH TEST BEFORE                                                  
              UNTIL SQLCODE = +100                                              
                                                                                
              EXEC SQL                                                          
                FETCH  EXTR_FORMS_RERUN                                         
                 INTO  :WS-TEMP-EXT-LANG                                        
                      ,:WS-TEMP-EXT-FORM-NBR                                    
                      ,:WS-TEMP-EXT-CONT-STATUS                                 
                      ,:WS-TEMP-EXT-REGION                                      
                      ,:WS-TEMP-EXT-BUS-SEG                                     
                      ,:WS-TEMP-RGO                                             
                      ,:WS-TEMP-EXT-CERT-ID                                     
                      ,:WS-TEMP-EXT-CONF-NBR                                    
                      ,:WS-TSD-SEQ-NUM                                          
                      ,:WS-MAS-SEQ-NUM                                          
                      ,:WS-TEMP-EXT-GROUP                                       
                      ,:WS-TEMP-EXT-DIV                                         
                      ,:WS-UPD-FORM-ID                                          
                      ,:WS-UPD-FORM-SEQ-NUM                                     
                      ,:WS-TEMP-WEB-SENT-TIMESTAMP                              
                      ,:WS-TEMP-TAT                                             
                      ,:DCLTFORM.FORM-DATA-STRING                               
              END-EXEC                                                          
                                                                                
              MOVE 'FETCH EXTRACT RERUN CURSOR' TO AB-MESSAGE                   
              PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT                        
                                                                                
              IF SQLCODE = ZERO                                                 
                 MOVE FORM-DATA-STRING  OF  DCLTFORM                            
                               TO WS-TEMP-EXT-FORM-DATA                         
                 PERFORM 2100-PRODUCE-OUTPUT  THRU  2100-EXIT                   
              END-IF                                                            
                                                                                
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
             CLOSE EXTR_FORMS_RERUN                                             
           END-EXEC.                                                            
                                                                                
           MOVE 'CLOSE EXTR_FORMS_RERUN CURSOR' TO AB-MESSAGE.                  
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2050-EXIT.                                                               
            EXIT.                                                               
                                                                                
       2060-PROCESS-RERUN-A.                                                    
      *                                                                         
      *    NOTE. THE SENT TO PRINT FIELD IS NOT UPDATED                         
      *          DURING A RERUN/REPRINT                                         
      *                                                                         
           ADD 1 TO LVL.                                                        
           MOVE '2060-PROCESS-RERUN-A' TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
           EXEC SQL                                                             
             OPEN EXTR_FORMS_RERUN_A                                            
           END-EXEC.                                                            
                                                                                
           MOVE 'OPEN EXTR_FORMS_RERUN_A CURSOR' TO AB-MESSAGE.                 
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
                                                                                
           PERFORM                                                              
              WITH TEST BEFORE                                                  
              UNTIL SQLCODE = +100                                              
                                                                                
              EXEC SQL                                                          
                FETCH  EXTR_FORMS_RERUN_A                                       
                 INTO  :WS-TEMP-EXT-LANG                                        
                      ,:WS-TEMP-EXT-FORM-NBR                                    
                      ,:WS-TEMP-EXT-CONT-STATUS                                 
                      ,:WS-TEMP-EXT-REGION                                      
                      ,:WS-TEMP-EXT-BUS-SEG                                     
                      ,:WS-TEMP-RGO                                             
                      ,:WS-TEMP-EXT-CERT-ID                                     
                      ,:WS-TEMP-EXT-CONF-NBR                                    
                      ,:WS-TSD-SEQ-NUM                                          
                      ,:WS-MAS-SEQ-NUM                                          
                      ,:WS-TEMP-EXT-GROUP                                       
                      ,:WS-TEMP-EXT-DIV                                         
                      ,:WS-UPD-FORM-ID                                          
                      ,:WS-UPD-FORM-SEQ-NUM                                     
                      ,:WS-TEMP-WEB-SENT-TIMESTAMP                              
                      ,:WS-TEMP-TAT                                             
                      ,:DCLTFORM.FORM-DATA-STRING                               
              END-EXEC                                                          
                                                                                
              MOVE 'FETCH EXTRACT RERUN_A CURSOR' TO AB-MESSAGE                 
              PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT                        
                                                                                
              IF SQLCODE = ZERO                                                 
                 MOVE FORM-DATA-STRING  OF  DCLTFORM                            
                               TO WS-TEMP-EXT-FORM-DATA                         
                 PERFORM 2100-PRODUCE-OUTPUT  THRU  2100-EXIT                   
              END-IF                                                            
                                                                                
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
             CLOSE EXTR_FORMS_RERUN_A                                           
           END-EXEC.                                                            
                                                                                
           MOVE 'CLOSE EXTR_FORMS_RERUN_A CURSOR' TO AB-MESSAGE.                
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2060-EXIT.                                                               
            EXIT.                                                               
                                                                                
       2075-PROCESS-WHOLE.                                                      
           ADD 1 TO LVL.                                                        
           MOVE '2075-PROCESS-WHOLE' TO AB-PARAGRAPH-NAME (LVL).                
                                                                                
           EXEC SQL                                                             
             OPEN EXTR_FORMS_WHOLE                                              
           END-EXEC.                                                            
                                                                                
           MOVE 'OPEN EXTR_FORMS_WHOLE CURSOR' TO AB-MESSAGE.                   
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
                                                                                
           PERFORM                                                              
              WITH TEST BEFORE                                                  
              UNTIL SQLCODE = +100                                              
                                                                                
              EXEC SQL                                                          
                FETCH  EXTR_FORMS_WHOLE                                         
                 INTO  :WS-TEMP-EXT-LANG                                        
                      ,:WS-TEMP-EXT-FORM-NBR                                    
                      ,:WS-TEMP-EXT-CONT-STATUS                                 
                      ,:WS-TEMP-EXT-REGION                                      
                      ,:WS-TEMP-EXT-BUS-SEG                                     
                      ,:WS-TEMP-RGO                                             
                      ,:WS-TEMP-EXT-CERT-ID                                     
                      ,:WS-TEMP-EXT-CONF-NBR                                    
                      ,:WS-TSD-SEQ-NUM                                          
                      ,:WS-MAS-SEQ-NUM                                          
                      ,:WS-TEMP-EXT-GROUP                                       
                      ,:WS-TEMP-EXT-DIV                                         
                      ,:WS-UPD-FORM-ID                                          
                      ,:WS-UPD-FORM-SEQ-NUM                                     
                      ,:WS-TEMP-WEB-SENT-TIMESTAMP                              
                      ,:WS-TEMP-TAT                                             
                      ,:DCLTFORM.FORM-DATA-STRING                               
              END-EXEC                                                          
                                                                                
              MOVE 'FETCH WHOLE EXTRACT RECORD CURSOR' TO AB-MESSAGE            
              PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT                        
                                                                                
              IF SQLCODE = ZERO                                                 
                 MOVE FORM-DATA-STRING  OF  DCLTFORM                            
                               TO WS-TEMP-EXT-FORM-DATA                         
                 PERFORM 2100-PRODUCE-OUTPUT  THRU  2100-EXIT                   
                 PERFORM 2200-UPDATE-TS       THRU  2200-EXIT                   
              END-IF                                                            
                                                                                
           END-PERFORM.                                                         
                                                                                
           EXEC SQL                                                             
             CLOSE EXTR_FORMS_WHOLE                                             
           END-EXEC.                                                            
                                                                                
           MOVE 'CLOSE EXTR_FORMS_WHOLE CURSOR' TO AB-MESSAGE.                  
           PERFORM 8900-CHECK-SQL-CODE THRU 8900-EXIT.                          
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2075-EXIT.                                                               
            EXIT.                                                               
                                                                                
       2100-PRODUCE-OUTPUT.                                                     
                                                                                
      ******************************************************************        
      *    PRODUCE OUTPUT RECORD                                                
      ******************************************************************        
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2100-PRODUCE-OUTPUT' TO AB-PARAGRAPH-NAME (LVL).               
                                                                                
           MOVE  WS-TEMP-EXT-HEADER      TO  GCCCFEXT-HEADER.                   
                                                                                
           MOVE  ' '                     TO  GCCCFEXT-SORT-CAT1.                
                                                                                
           IF  GCCCFEXT-CONT-STATUS  =  'I' OR 'T'                              
               MOVE  GCCCFEXT-REGION     TO  GCCCFEXT-SORT-CAT2                 
               MOVE  WS-TEMP-TAT         TO  GCCCFEXT-SORT-CAT0                 
            ELSE                                                                
               MOVE  GCCCFEXT-BUS-SEG    TO  GCCCFEXT-SORT-CAT2                 
               MOVE  SPACES              TO  GCCCFEXT-SORT-CAT0                 
               IF  WS-TEMP-RGO     =   '15750'                                  
                   MOVE 'M'              TO  GCCCFEXT-SORT-CAT1.                
                                                                                
           MOVE  WS-TSD-SEQ-NUM          TO  GCCCFEXT-SEQ-NBR.                  
      *    MOVE  WS-UPD-FORM-SEQ-NUM     TO  GCCCFEXT-SEQ-NBR.                  
                                                                                
           MOVE  WS-TEMP-EXT-FORM-DETL   TO  GCCCFEXT-FORM-DATA.                
                                                                                
           MOVE  WS-TEMP-WST-DATE        TO  WS-REFORM-WST-DATE.                
           MOVE  WS-TEMP-WST-TIME        TO  WS-REFORM-WST-TIME.                
           MOVE  WS-TEMP-TAT             TO  WS-REFORM-TURN-TIME.               
           MOVE  WS-REFORM-WS-TIMESTAMP  TO  GCCCFEXT-TIMESTAMP.                
                                                                                
           MOVE  SPACES                  TO  WS-REFORM-WST-DATE                 
                                             WS-REFORM-WST-TIME.                
                                                                                
           COMPUTE  GCCCFEXT-RECL     =   WS-TEMP-EXT-FORM-LENGTH               
                                      +   66.                                   
                                                                                
           MOVE WS-OUTPUT-LR           TO LOGICAL-RECORD-NAME.                  
           MOVE WS-STORE-LR            TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR        USING  WS-GAEDATSR-VERB                      
                                         GCCCFEXT-RECORD                        
                                         ICBM.                                  
           INITIALIZE GCCCFEXT-RECORD                                           
           IF LR-STATUS-OK                                                      
               MOVE  'Y'               TO  WS-OUTPUT-OPEN-MKR                   
             ELSE                                                               
               MOVE 'ERROR WRITING TO FORM EXTRACT' TO AB-MSG1                  
               MOVE GCCCFEXT-RECORD                 TO AB-MSG2                  
               PERFORM 9999-ABEND THRU 9999-EXIT.                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2200-UPDATE-TS.                                                          
      *****************************************************                     
      *    UPDATE TIMESTAMP FOR PRINT ON SUBMISSION TABLE                       
      *    TSD                                                                  
      *****************************************************                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2200-UPDATE-TS' TO AB-PARAGRAPH-NAME (LVL).                    
                                                                                
           EXEC SQL                                                             
               UPDATE TSD                                                       
                 SET                                                            
                  SENT_TO_PRINT_TS   = :WS-TIMESTAMP                            
               WHERE                                                            
                   FORM_ID           = :WS-UPD-FORM-ID                          
               AND FORM_SEQ_NUM      = :WS-UPD-FORM-SEQ-NUM                     
            END-EXEC                                                            
                                                                                
           MOVE 'UPDATE SUB TABLE TIMESTAMP' TO AB-MESSAGE                      
           PERFORM 8900-CHECK-SQL-CODE  THRU 8900-EXIT.                         
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       2200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       3000-COMPLETION.                                                         
      *****************************************************************         
      *  THIS PARAGRAPH...                                                      
      *    - CLOSES OUTPUT FILE                                                 
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '3000-COMPLETION'      TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
           MOVE 'N'                    TO WS-OUTPUT-OPEN-MKR.                   
                                                                                
           MOVE WS-OUTPUT-LR           TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
           IF LR-STATUS-OK                                                      
               NEXT SENTENCE                                                    
             ELSE                                                               
               MOVE 'ERROR CLOSING FORM EXTRACT' TO AB-MSG1                     
               MOVE '3000-COMPLETION'               TO AB-MSG2                  
               PERFORM 9999-ABEND THRU 9999-EXIT.                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6100-READ-INPUT.                                                         
      ******************************************************************        
      *    READ NEXT INPUT RECORD.                                     *        
      ******************************************************************        
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6100-READ-INPUT'      TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
           MOVE WS-INPUT-LR            TO LOGICAL-RECORD-NAME.                  
           INITIALIZE GCCCRPRM-RECORD.                                          
                                                                                
           CALL WS-GAEDATSR          USING  WS-GAEDATSR-VERB                    
                                         GCCCRPRM-RECORD                        
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
               MOVE  'Y'               TO  WS-INPUT-OPEN-MKR                    
             ELSE                                                               
               MOVE 'Y'                TO  WS-INPUT-EOF-MKR                     
               MOVE 'ERROR READING PARM INPUT' TO AB-MSG1                       
               MOVE ' 6100-READ-INPUT'         TO AB-MSG2                       
               PERFORM 9999-ABEND THRU 9999-EXIT.                               
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       6100-READ-INPUT-EXIT.                                                    
           EXIT.                                                                
                                                                                
       6300-CLOSE-INPUT.                                                        
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '6300-CLOSE-INPUT'     TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
           MOVE WS-INPUT-LR            TO LOGICAL-RECORD-NAME.                  
           MOVE FINISH-LR              TO WS-GAEDATSR-VERB.                     
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
           MOVE  'N'                 TO  WS-INPUT-OPEN-MKR.                     
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       6300-EXIT.                                                               
           EXIT.                                                                
                                                                                
      *****************************************************************         
      * THIS PARAGRAPH CHECKS THE SQL CODE AFTER A DB2 CALL AND HANDLES         
      * ANY ERRORS DETECTED.                                                    
      *****************************************************************         
                                                                                
       8900-CHECK-SQL-CODE.                                                     
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '8900-CHECK-SQL-CODE'  TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
           EVALUATE SQLCODE                                                     
                                                                                
              WHEN ZERO                                                         
                 CONTINUE                                                       
              WHEN +100                                                         
                 CONTINUE                                                       
              WHEN OTHER                                                        
                 MOVE SQLCODE      TO AB-SQLCODE                                
                 MOVE SQLERRMC     TO AB-MSG2                                   
                 INSPECT AB-MSG2 CONVERTING X'FF' TO '-'                        
                 PERFORM 9999-ABEND THRU 9999-EXIT                              
                                                                                
           END-EVALUATE.                                                        
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                                
       8900-EXIT.                                                               
           EXIT.                                                                
                                                                                
      *****************************************************************         
      * THIS PARAGRAPH IS CALLED IF AN EXCEPTIONAL CONDITION, WHICH             
      * CANNOT ALLOW THE PROGRAM TO CONTINUE NORMALLY, IS FOUND.                
      * MESSAGES GIVING DETAILS OF THE ABEND ARE DISPLAYEDAND THE               
      * AND THE PROGRAM WILL TERMINATE WITH A RETURN CODE OF 16.                
      *****************************************************************         
                                                                                
       9999-ABEND.                                                              
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '9999-ABEND'           TO AB-PARAGRAPH-NAME (LVL).              
                                                                                
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
                                                                                
           IF  WS-INPUT-OPEN                                                    
               PERFORM  6300-CLOSE-INPUT   THRU  6300-EXIT.                     
                                                                                
           IF  WS-OUTPUT-OPEN                                                   
               PERFORM  3000-COMPLETION    THRU  3000-EXIT.                     
                                                                                
           SUBTRACT 1 FROM LVL.                                                 
                                                                        12330000
           EXEC SQL                                                     12340000
             ROLLBACK                                                   12350000
           END-EXEC.                                                    12360000
                                                                        12370000
           MOVE +16 TO RETURN-CODE.                                     12380000
                                                                                
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
