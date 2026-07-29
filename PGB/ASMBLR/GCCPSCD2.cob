       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPSCD2.                                                 
      *AUTHOR.        J ELKINS.                                                 
      *DATE-WRITTEN.  MAR 03, 2004.                                             
      *DATE-COMPILED.                                                           
                                                                                
      *****************************************************************         
      *   (GROUP BENEFITS)                                                      
      *   GCCPSCD2 - SEND OUT EMAIL NOTIFICATIONS FOR REAL TIME                 
      *              REPORTING SCHEDULER                                        
      *                                                                         
      *                                                                         
      *   PROGRAM DESCRIPTION:                                                  
      *   THIS PROGRAM READS A FILE CREATED BY GCCPSCD1.  THIS                  
      *   FILE CONTAINS THE USER-ID AND REQ-DS FOR EMAILS THAT                  
      *   ARE REQUIRED TO NOTIFY USERS THAT THEIR REQESTS ARE READY.            
      *   THE EMAIL ADDRESS WILL BE FOUND ON THE CPD TABLES.                    
      *   THE MESSAGE SENT (IN FRENCH OF ENGLISH) IS READ FROM THE              
      *   CONTROL CARDS.                                                        
      *                                                                         
      *   INPUT TABLES                                                          
      *        1.  TUT                                                          
      *        2.  TCUST                                                        
      *                                                                         
      *                                                                         
      *   OUTPUT FILES:                                                         
      *        1.  EMAIL NOTIFICATION FILE                                      
      *                                                                         
      *   CALLS:        DATA SERVER                                             
      *                                                                         
      *****************************************************************         
      *****************************************************************         
      *   MODIFICATION LOG                                                      
      ******************************************************************        
      * PROGRAMMER   º  DATE  º             CHANGE                              
      *    NAME      ºDD/MM/YYº           DESCRIPTION                           
      *--------------+--------+-----------------------------------------        
      * J ELKINS     º03/03/04º ORIGINAL CODE                                   
      * D SIMMONS    º21/10/05º HANDLING OF FRENCH CHARACTERS                   
      * IBM GR       º26/08/08º UPGRADED IN ECU PROJECT                         
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
      *    CALLED MODULES                                                       
      *****************************************************************         
                                                                                
       01  WS-CALLED-MODULES.                                                   
           05  WS-GC2DATE                PIC X(08)  VALUE 'GC2DATE'.            
           05  WS-GAEDATSR               PIC X(08)                              
                                         VALUE 'GAEDATSR'.                      
                                                                                
      ******************************************************************        
      *   DATE ROUTINE VARIABLES                                                
      ******************************************************************        
       01  GAC-DATE-PARAMETERS.          COPY GARDATEP.                         
                                                                                
                                                                                
      ****************************************************************          
      *   DATA SERVER VARIABLES                                                 
      ****************************************************************          
       01  WS-CALLING-VARIABLE.                                                 
           05  WS-GAEDATSR-VERB          PIC X(16).                             
           05  WS-EMAIL-OUT-LR           PIC X(16)                              
                                         VALUE 'PRINT-DATA-020  '.              
           05  WS-CC-LR                  PIC X(16)                              
                                         VALUE 'CARD-DATA-010   '.              
           05  WS-EMAIL-IN-LR            PIC X(16)                              
                                         VALUE 'CARD-DATA-011   '.              
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
       01  DATA-SERVER-VERBS.                                                   
           COPY GARDSVRB.                                                       
                                                                                
      *****************************************************************         
      *   CONTROL CARD                                                          
      *****************************************************************         
       01  WS-CC-RECORD.                                                        
           05  WS-CC-RECORD-TYPE         PIC X(4).                              
               88  WS-CC-COMMENT                   VALUE '0000'                 
                                                         '    '.                
               88  WS-CC-ENGLISH                   VALUE '1000'.                
               88  WS-CC-FRENCH                    VALUE '2000'.                
           05  WS-CC-TEXT                PIC X(76).                             
                                                                                
      *****************************************************************         
      *   FILE LAYOUT OF  EMAIL NOTIFICATIONS                                   
      *****************************************************************         
       01  GCCCSCD1-RECORD.                                                     
           COPY GCCCSCD1.                                                       
                                                                                
      *****************************************************************         
      *   STORAGE FOR EMAIL NOTIFICATION TEXT                                   
      *****************************************************************         
       01  WS-EMAIL-ENG.                                                        
           05  WS-EMAIL-ENG-COUNT        PIC S9(4) COMP.                        
           05  FILLER.                                                          
               10  WS-EMAIL-ENG-TEXT     PIC X(76) OCCURS 100 TIMES.            
                                                                                
       01  WS-EMAIL-FR.                                                         
           05  WS-EMAIL-FR-COUNT         PIC S9(4) COMP.                        
           05  FILLER.                                                          
               10  WS-EMAIL-FR-TEXT      PIC X(130) OCCURS 100 TIMES.           
                                                                                
      *****************************************************************         
      *   DISPLAY FIELDS FOR END OF RUN                                         
      *****************************************************************         
                                                                                
       01  WS-DISPLAY-FIELDS.                                                   
           05  WS-DISPLAY1.                                                     
               10  FILLER                  PIC X(55)  VALUE                     
               'CA REPORTING REAL TIME SCHEDULER EMAIL NOTIFICATION'.           
               10  FILLER                  PIC X(20)  VALUE                     
               'PROGRAM:  GCCPSCD2'.                                            
           05  WS-DISPLAY2.                                                     
               10  FILLER                  PIC X(55)  VALUE SPACES.             
               10  FILLER                  PIC X(10)  VALUE                     
               'DATE:     '.                                                    
               10  WS-DISPLAY2-MON         PIC X(4).                            
               10  WS-DISPLAY2-DAY         PIC 99.                              
               10  FILLER                  PIC X(2)   VALUE ', '.               
               10  WS-DISPLAY2-YEAR        PIC 9999.                            
           05  WS-DISPLAY3.                                                     
               10  FILLER                  PIC X(63)  VALUE                     
               'NUMBER OF REQUESTS READ                            ='.          
               10  WS-DISPLAY3-REC-CNTR    PIC ZZZZZZZZZZ9.                     
           05  WS-DISPLAY4.                                                     
               10  FILLER                  PIC X(63)  VALUE                     
               'NUMBER OF EMAILS SENT                              ='.          
               10  WS-DISPLAY4-REC-CNTR    PIC ZZZZZZZZZZ9.                     
                                                                                
       01  WS-SWITCHES-COUNTERS.                                                
                                                                                
           05  WS-EOF-SW                 PIC X     VALUE 'N'.                   
               88  WS-EOF                          VALUE 'Y'.                   
               88  WS-EOF-NO                       VALUE 'N'.                   
                                                                                
           05  WS-EMAIL-ADDR-SW          PIC X     VALUE 'N'.                   
               88  WS-EMAIL-ADDR-FOUND             VALUE 'Y'.                   
               88  WS-EMAIL-ADDR-FOUND-NO          VALUE 'N'.                   
                                                                                
           05  WS-CC-EOF-WS              PIC X     VALUE 'N'.                   
               88  WS-CC-EOF                       VALUE 'Y'.                   
               88  WS-CC-EOF-NO                    VALUE 'N'.                   
                                                                                
           05  WS-FIRST-TIME-SW          PIC X     VALUE 'Y'.                   
               88  WS-FIRST-TIME                   VALUE 'Y'.                   
               88  WS-FIRST-TIME-NO                VALUE 'N'.                   
                                                                                
           05  WS-READ-CNTR              PIC S9(11) COMP-3                      
                                                    VALUE 0.                    
           05  WS-EMAIL-CNTR             PIC S9(11) COMP-3                      
                                                    VALUE 0.                    
                                                                                
           05  SUB1                      PIC S9(4)  COMP.                       
                                                                                
           05  WS-PREVIOUS-USER-ID       PIC X(20)                              
               VALUE SPACES.                                                    
                                                                                
           05  WS-ENG-SUBJ               PIC X(70)  VALUE                       
               'Your Real-time Report Request(s) are available!'.               
                                                                                
           05  WS-FR-SUBJ                PIC X(70)  VALUE                       
               'VOTRE RELEVE PERIODIQUE EST PRET A PRODUIRE!'.                  
      *****************************************************************         
      *   EMAIL OUTPUT LINES                                                    
      *****************************************************************         
       01  WS-OUT-LINE                   PIC X(130).                            
                                                                                
       01  WS-OUT-HEAD1.                                                        
           05  FILLER                    PIC X(102)                             
               VALUE 'HELO ACDNIBU'.                                            
                                                                                
       01  WS-OUT-HEAD2.                                                        
           05  FILLER                    PIC X(102)                             
               VALUE 'MIME-VERSION: 1.0 '.                                      
                                                                                
       01  WS-OUT-TRAIL1.                                                       
           05  FILLER                    PIC X(102)                             
               VALUE SPACES.                                                    
                                                                                
       01  WS-OUT-TRAIL2.                                                       
           05  FILLER                    PIC X(102)                             
               VALUE '--==SPLUNG'.                                              
                                                                                
       01  WS-OUT-TRAIL3.                                                       
           05  FILLER                    PIC X(102)                             
               VALUE '.'.                                                       
                                                                                
       01  WS-OUT-ENDING.                                                       
           05  FILLER                    PIC X(102)                             
               VALUE 'QUIT'.                                                    
                                                                                
       01  WS-OUT-DETAIL-LINES.                                                 
           05  WS-OUT-DETAIL1.                                                  
               10  FILLER                PIC X(102)                             
                   VALUE 'RSET'.                                                
                                                                                
           05  WS-OUT-DETAIL2.                                                  
               10  FILLER                PIC X(102)                             
                   VALUE 'MAIL FROM: <BATCH@ACDNIBU> '.                         
                                                                                
           05  WS-OUT-DETAIL3.                                                  
               10  FILLER                PIC X(10)                              
                   VALUE 'RCPT TO: <'.                                          
               10  WS-OUT-DETAIL3-EMAIL-ADDR                                    
                                         PIC X(60).                             
               10  FILLER                PIC X(32)                              
                   VALUE '>'.                                                   
                                                                                
           05  WS-OUT-DETAIL4.                                                  
               10  FILLER                PIC X(102)                             
                   VALUE 'DATA'.                                                
                                                                                
           05  WS-OUT-DETAIL5.                                                  
               10  FILLER                PIC X(102)                             
                   VALUE 'FROM: GBADMIN_PA_WEBSIT@MANULIFE.COM'.                
                                                                                
           05  WS-OUT-DETAIL6.                                                  
               10  FILLER                PIC X(04)                              
                   VALUE 'TO:'.                                                 
               10  WS-OUT-DETAIL6-TO     PIC X(98)                              
                   VALUE 'PLAN ADVISOR'.                                        
                                                                                
           05  WS-OUT-DETAIL7.                                                  
               10  FILLER                PIC X(09)                              
                   VALUE 'SUBJECT:'.                                            
               10  WS-OUT-DETAIL7-SUBJ   PIC X(93).                             
                                                                                
           05  WS-OUT-DETAIL8.                                                  
               10  FILLER                PIC X(30)                              
                   VALUE 'CONTENT-TYPE: MULTIPART/MIXED;'.                      
               10  FILLER                PIC X(72)                              
                   VALUE ' BOUNDARY="==SPLUNG"'.                                
                                                                                
           05  WS-OUT-DETAIL9.                                                  
               10  FILLER                PIC X(102)                             
                   VALUE SPACES.                                                
                                                                                
           05  WS-OUT-DETAIL10.                                                 
               10  FILLER                PIC X(102)                             
                   VALUE '--==SPLUNG '.                                         
                                                                                
           05  WS-OUT-DETAIL11.                                                 
               10  FILLER                PIC X(26)                              
                   VALUE 'CONTENT-TYPE: TEXT/PLAIN;'.                           
               10  FILLER PIC X(76)                                             
                   VALUE 'CHARSET= "US-ASCII"'.                                 
                                                                                
           05  WS-OUT-DETAIL12.                                                 
               10  FILLER                PIC X(27)                              
                   VALUE 'CONTENT-TRANSFER-ENCODING:'.                          
               10  FILLER PIC X(75)                                             
                   VALUE 'QUOTED-PRINTABLE'.                                    
                                                                                
           05  WS-OUT-DETAIL13.                                                 
               10  FILLER                PIC X(102)                             
                   VALUE SPACES.                                                
                                                                                
       01  FILLER                        REDEFINES                              
                                         WS-OUT-DETAIL-LINES.                   
           10  WS-OUT-DTL-LINE           PIC X(102)                             
                                         OCCURS 13 TIMES.                       
                                                                                
       01  WS-OUT-REQ-DS-LINE.                                                  
           05  FILLER                    PIC X(2) VALUE '- '.                   
           05  WS-OUT-REQ-DS             PIC X(100).                            
                                                                                
      *****************************************************************         
      *   FRENCH CHARACTER WORKING AREA.                                        
      *****************************************************************         
       01  WS-FRENCH-WORKING-AREA.                                              
           05  WS-FR-INPUT-LINE.                                                
               10  WS-FR-INPUT-CHAR      PIC X(1)  OCCURS 102 TIMES.            
           05  WS-FR-OUTPUT-LINE.                                               
               10  WS-FR-OUTPUT-CHAR     PIC X(1)  OCCURS 130 TIMES.            
           05  WS-FR-INPUT-LINE-SUB      PIC S9(4) VALUE ZERO COMP.             
           05  WS-FR-INPUT-LINE-CURR-MAX PIC S9(4) VALUE +102 COMP.             
           05  WS-FR-INPUT-LINE-MAX      PIC S9(4) VALUE +102 COMP.             
           05  WS-FR-OUTPUT-LINE-SUB     PIC S9(4) VALUE ZERO COMP.             
           05  WS-FR-OUTPUT-LINE-MAX     PIC S9(4) VALUE +130 COMP.             
           05  WS-CHAR                   PIC X(1).                              
               88  WS-CHAR-FRENCH-ACCENT VALUES                                 
                   'š', 'Ý', 'ß', 'Þ', 'Ü', '˜', '',                           
                   '±', 'ˆ', '”', '°',                                          
                   'û', '²', 'ü', 'Ö',                                          
                   'é', 'ä', '…', 'Ñ', 'Ž',                                     
                   'ú', '§', '÷', 'ð', '[',                                     
                   'ë', '', 'á', 'Ÿ', '€', 'â', '«',                           
                   'Û', '©', 'ª', 'œ',                                          
                   '¨', '¥', '™', 'ã',                                          
                   'è', 'ì', 'Ê', 'í', '¾',                                     
                   'ô', '£', '­', 'õ', '¯'.                                     
           05  WS-ENCODED-CHARS.                                                
               10  WS-ENCODED-CHAR       PIC X(1)  OCCURS 3 TIMES.              
           05  WS-ENCODED-CHAR-SUB       PIC S9(4) VALUE ZERO COMP.             
           05  WS-ENCODED-CHAR-MAX       PIC S9(4) VALUE +3   COMP.             
           05  WS-FAT-SUB                PIC S9(4) VALUE ZERO COMP.             
           05  WS-FAT-MAX                PIC S9(4) VALUE +50  COMP.             
           05  WS-FRENCH-ACCENTS.                                               
               10  FILLER                PIC X(4) VALUE 'š=C0'.                 
               10  FILLER                PIC X(4) VALUE 'Ý=C1'.                 
               10  FILLER                PIC X(4) VALUE 'ß=C2'.                 
               10  FILLER                PIC X(4) VALUE 'Þ=C3'.                 
               10  FILLER                PIC X(4) VALUE 'Ü=C4'.                 
               10  FILLER                PIC X(4) VALUE '˜=C5'.                 
               10  FILLER                PIC X(4) VALUE '=C7'.                 
               10  FILLER                PIC X(4) VALUE '±=C8'.                 
               10  FILLER                PIC X(4) VALUE 'ˆ=C9'.                 
               10  FILLER                PIC X(4) VALUE '”=CA'.                 
               10  FILLER                PIC X(4) VALUE '°=CB'.                 
               10  FILLER                PIC X(4) VALUE 'û=CC'.                 
               10  FILLER                PIC X(4) VALUE '²=CD'.                 
               10  FILLER                PIC X(4) VALUE 'ü=CE'.                 
               10  FILLER                PIC X(4) VALUE 'Ö=CF'.                 
               10  FILLER                PIC X(4) VALUE 'é=D2'.                 
               10  FILLER                PIC X(4) VALUE 'ä=D3'.                 
               10  FILLER                PIC X(4) VALUE '…=D4'.                 
               10  FILLER                PIC X(4) VALUE 'Ñ=D5'.                 
               10  FILLER                PIC X(4) VALUE 'Ž=D6'.                 
               10  FILLER                PIC X(4) VALUE 'ú=D9'.                 
               10  FILLER                PIC X(4) VALUE '§=DA'.                 
               10  FILLER                PIC X(4) VALUE '÷=DB'.                 
               10  FILLER                PIC X(4) VALUE 'ð=DC'.                 
               10  FILLER                PIC X(4) VALUE '[=DD'.                 
               10  FILLER                PIC X(4) VALUE 'ë=E0'.                 
               10  FILLER                PIC X(4) VALUE '=E1'.                 
               10  FILLER                PIC X(4) VALUE 'á=E2'.                 
               10  FILLER                PIC X(4) VALUE 'Ÿ=E3'.                 
               10  FILLER                PIC X(4) VALUE '€=E4'.                 
               10  FILLER                PIC X(4) VALUE 'â=E5'.                 
               10  FILLER                PIC X(4) VALUE '«=E7'.                 
               10  FILLER                PIC X(4) VALUE 'Û=E8'.                 
               10  FILLER                PIC X(4) VALUE '©=E9'.                 
               10  FILLER                PIC X(4) VALUE 'ª=EA'.                 
               10  FILLER                PIC X(4) VALUE 'œ=EB'.                 
               10  FILLER                PIC X(4) VALUE '¨=EC'.                 
               10  FILLER                PIC X(4) VALUE '¥=ED'.                 
               10  FILLER                PIC X(4) VALUE '™=EE'.                 
               10  FILLER                PIC X(4) VALUE 'ã=EF'.                 
               10  FILLER                PIC X(4) VALUE 'è=F2'.                 
               10  FILLER                PIC X(4) VALUE 'ì=F3'.                 
               10  FILLER                PIC X(4) VALUE 'Ê=F4'.                 
               10  FILLER                PIC X(4) VALUE 'í=F5'.                 
               10  FILLER                PIC X(4) VALUE '¾=F6'.                 
               10  FILLER                PIC X(4) VALUE 'ô=F9'.                 
               10  FILLER                PIC X(4) VALUE '£=FA'.                 
               10  FILLER                PIC X(4) VALUE '­=FB'.                 
               10  FILLER                PIC X(4) VALUE 'õ=FC'.                 
               10  FILLER                PIC X(4) VALUE '¯=FD'.                 
           05  WS-FRENCH-ACCENTS-TABLE   REDEFINES WS-FRENCH-ACCENTS.           
               10  WS-FAT-ENTRY          OCCURS 50 TIMES.                       
                   15  WS-FAT-FRENCH-CHAR          PIC X(1).                    
                   15  WS-FAT-ENCODED-CHAR         PIC X(3).                    
                                                                                
      *****************************************************************         
      *   DB2 INCLUDES                                                          
      *****************************************************************         
           EXEC SQL INCLUDE SQLCA        END-EXEC.                              
           EXEC SQL INCLUDE TUTD         END-EXEC.                              
           EXEC SQL INCLUDE TCUSTD       END-EXEC.                              
                                                                                
      *01  DCLTUT.                                                              
           EXEC SQL INCLUDE TUT          END-EXEC.                              
      *01  DCLTCUST.                                                            
           EXEC SQL INCLUDE TCUST        END-EXEC.                              
                                                                                
      *****************************************************************         
      *   VARIABLES                                                             
      *****************************************************************         
       01  WS-ABEND-INFO.                                                       
           10  AB-MODULE-NAME            PIC X(60) VALUE                        
                'GCCPSCD2 - REAL TIME SCHEDULER       '.                        
           10  AB-PARAGRAPH-NAME  OCCURS 25 TIMES                               
                                         PIC X(60).                             
           10  AB-MESSAGE.                                                      
               15  AB-MSG1               PIC X(70).                             
               15  AB-MSG2               PIC X(70).                             
           10  AB-SQLCODE                PIC ----9.                             
           10  LVL                       PIC S9(4) COMP.                        
           10  CNT                       PIC S9(4) COMP.                        
                                                                                
                                                                                
      ****************************************************************          
      *********   P R O C E D U R E   D I V I S I O N   **************          
      ****************************************************************          
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           MOVE 1 TO LVL.                                                       
           MOVE '0000-MAINLINE'         TO   AB-PARAGRAPH-NAME (LVL).           
                                                                                
           PERFORM 1000-INITIALIZATION                                          
              THRU 1000-EXIT.                                                   
                                                                                
           PERFORM 2000-PROCESS                                                 
              THRU 2000-EXIT                                                    
             UNTIL WS-EOF.                                                      
                                                                                
           PERFORM 3000-COMPLETION                                              
              THRU 3000-EXIT.                                                   
                                                                                
           GOBACK.                                                              
                                                                                
                                                                                
       1000-INITIALIZATION.                                                     
      *****************************************************************         
      * - GET TODAY'S DATE                                                      
      * - OPEN CURSOR                                                           
      * - INITIALIZE OUTPUT                                                     
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '1000-INITIALIZATION'    TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE 'GCCPSCD2'               TO ICBM-PROGRAM-NAME.                  
           MOVE LOW-VALUES               TO LINKAGE-CONTROL.                    
                                                                                
                                                                                
      *    USE GACDATE TO GET CURRENT DATE                             *        
                                                                                
           MOVE 'E'                      TO VDATE-REQ-SERVICE.                  
           MOVE 'A'                      TO VDATE-REQ-BASIS.                    
           MOVE '1'                      TO VDATE-REQ-DETAIL.                   
           MOVE 'E'                      TO VDATE-REQ-LANGUAGE.                 
                                                                                
           CALL WS-GC2DATE               USING GAC-DATE-PARAMETERS.             
                                                                                
           MOVE VDATE-EXT-YEAR           TO WS-DISPLAY2-YEAR.                   
           MOVE VDATE-EXT-MONTH          TO WS-DISPLAY2-MON.                    
           MOVE VDATE-EXT-DAY            TO WS-DISPLAY2-DAY.                    
                                                                                
      * START SYSOUT DISPLAY                                                    
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY WS-DISPLAY1.                                                 
           DISPLAY WS-DISPLAY2.                                                 
           DISPLAY ' '.                                                         
                                                                                
      ******************************************************************        
      *    CONTROL CARD FILE CONTAINS THE TEXT OF THE EMAIL                     
      *    TO BE SENT IN BOTH FRENCH AND ENGLISH                                
      ******************************************************************        
                                                                                
           SET WS-CC-EOF-NO              TO TRUE.                               
           MOVE OBTAIN-FIRST             TO WS-GAEDATSR-VERB.                   
           MOVE WS-CC-LR                 TO LOGICAL-RECORD-NAME.                
                                                                                
           PERFORM 9200-READ-CONTROL-CARD                                       
              THRU 9200-EXIT                                                    
             UNTIL WS-CC-EOF.                                                   
                                                                                
           MOVE OBTAIN-FIRST             TO WS-GAEDATSR-VERB.                   
           PERFORM 9000-READ-REQUEST                                            
              THRU 9000-EXIT.                                                   
                                                                                
      * ARE THERE ANY REQUESTS IN THE FILE                                      
                                                                                
           IF WS-EOF                                                            
               GO TO 1000-EXIT                                                  
           END-IF.                                                              
                                                                                
      * START THE EMAIL FILE                                                    
                                                                                
           MOVE WS-OUT-HEAD1             TO WS-OUT-LINE.                        
           PERFORM 9100-WRITE-EMAIL-FILE                                        
              THRU 9100-EXIT.                                                   
                                                                                
           MOVE WS-OUT-HEAD2             TO WS-OUT-LINE.                        
           PERFORM 9100-WRITE-EMAIL-FILE                                        
              THRU 9100-EXIT.                                                   
                                                                                
       1000-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
                                                                                
       2000-PROCESS.                                                            
      *****************************************************************         
      *    PROCESS THE REQUESTS UNTIL END OF FILE                               
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2000-PROCESS'           TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           IF GCCCSCD1-USER-ID           =  WS-PREVIOUS-USER-ID                 
               PERFORM 2200-PROCESS-REQ-DS                                      
                  THRU 2200-EXIT                                                
           ELSE                                                                 
               PERFORM 2100-PROCESS-NEW-EMAIL                                   
                  THRU 2100-EXIT                                                
           END-IF.                                                              
                                                                                
           MOVE OBTAIN-NEXT              TO WS-GAEDATSR-VERB.                   
           PERFORM 9000-READ-REQUEST                                            
              THRU 9000-EXIT.                                                   
                                                                                
       2000-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       2100-PROCESS-NEW-EMAIL.                                                  
      *****************************************************************         
      *  - CLOSE OUT PREVIOUS EMAIL (IF NOT FIRST TIME)                         
      *  - FIND EMAIL ADDRESS                                                   
      *  - START NEW EMAIL                                                      
      *  - DEPENDING ON LANGUAGE SEND TEXT OF MESSAGE                           
      *  - WRITE REPORT DESCRIPTION                                             
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2100-PROCESS-NEW-EMAIL' TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           IF WS-FIRST-TIME                                                     
               SET WS-FIRST-TIME-NO      TO TRUE                                
           ELSE                                                                 
               IF WS-EMAIL-ADDR-FOUND                                           
                   PERFORM 7500-CLOSE-EMAIL                                     
                      THRU 7500-EXIT                                            
               END-IF                                                           
           END-IF.                                                              
                                                                                
           MOVE GCCCSCD1-USER-ID         TO WS-PREVIOUS-USER-ID.                
                                                                                
           PERFORM 7100-FIND-EMAIL-ADDR                                         
              THRU 7100-EXIT.                                                   
                                                                                
           IF WS-EMAIL-ADDR-FOUND                                               
               ADD 1                     TO WS-EMAIL-CNTR                       
           ELSE                                                                 
               GO TO 2100-EXIT                                                  
           END-IF.                                                              
                                                                                
           MOVE EMAIL-ADDR OF DCLTCUST   TO WS-OUT-DETAIL3-EMAIL-ADDR.          
                                                                                
           IF LANG-CD OF DCLTCUST        =  'F'                                 
               MOVE WS-FR-SUBJ           TO WS-OUT-DETAIL7-SUBJ                 
           ELSE                                                                 
               MOVE WS-ENG-SUBJ          TO WS-OUT-DETAIL7-SUBJ                 
           END-IF.                                                              
                                                                                
           PERFORM 7200-START-EMAIL                                             
              THRU 7200-EXIT                                                    
           VARYING SUB1 FROM 1 BY 1                                             
             UNTIL SUB1 > 13.                                                   
                                                                                
           IF LANG-CD OF DCLTCUST        =  'F'                                 
               PERFORM 7400-FR-EMAIL                                            
                  THRU 7400-EXIT                                                
               VARYING SUB1 FROM 1 BY 1                                         
                 UNTIL SUB1 > WS-EMAIL-FR-COUNT                                 
           ELSE                                                                 
               PERFORM 7300-ENG-EMAIL                                           
                  THRU 7300-EXIT                                                
               VARYING SUB1 FROM 1 BY 1                                         
                 UNTIL SUB1 > WS-EMAIL-ENG-COUNT                                
           END-IF.                                                              
           PERFORM 2200-PROCESS-REQ-DS                                          
              THRU 2200-EXIT.                                                   
                                                                                
       2100-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       2200-PROCESS-REQ-DS.                                                     
      *****************************************************************         
      *    WRITE THE REPORT DESCRIPTION OF THE REPORT THAT IS                   
      *    READY IF THE EMAIL ADDRESS WAS FOUND FOR THIS USER-ID                
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '2200-PROCESS-REQ-DS'                                           
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           IF WS-EMAIL-ADDR-FOUND                                               
               MOVE GCCCSCD1-REQ-DS      TO WS-OUT-REQ-DS                       
               MOVE WS-OUT-REQ-DS-LINE   TO WS-FR-INPUT-LINE                    
               PERFORM 8100-CHECK-FOR-ACCENTS                                   
                  THRU 8100-EXIT                                                
               MOVE WS-FR-OUTPUT-LINE    TO WS-OUT-LINE                         
               PERFORM 9100-WRITE-EMAIL-FILE                                    
                  THRU 9100-EXIT                                                
           END-IF.                                                              
                                                                                
       2200-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       3000-COMPLETION.                                                         
      *****************************************************************         
      * CLOSE FILES                                                             
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '3000-COMPLETION'        TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
      * CLOSE EMAIL                                                             
                                                                                
           IF WS-FIRST-TIME-NO                                                  
               PERFORM 7500-CLOSE-EMAIL                                         
                  THRU 7500-EXIT                                                
               MOVE WS-OUT-ENDING        TO WS-OUT-LINE                         
               PERFORM 9100-WRITE-EMAIL-FILE                                    
                  THRU 9100-EXIT                                                
           END-IF.                                                              
                                                                                
      * CLOSE DATA SERVER                                                       
                                                                                
           MOVE WS-EMAIL-OUT-LR           TO LOGICAL-RECORD-NAME.               
           MOVE FINISH-LR                 TO WS-GAEDATSR-VERB.                  
                                                                                
           CALL WS-GAEDATSR USING WS-GAEDATSR-VERB                              
                                  LOGICAL-RECORD-NAME                           
                                  ICBM.                                         
                                                                                
                                                                                
      * DISPLAY JOB STATISTICS                                                  
                                                                                
           MOVE WS-READ-CNTR             TO WS-DISPLAY3-REC-CNTR.               
           DISPLAY WS-DISPLAY3.                                                 
           MOVE WS-EMAIL-CNTR            TO WS-DISPLAY4-REC-CNTR.               
           DISPLAY WS-DISPLAY4.                                                 
                                                                                
                                                                                
       3000-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       7000-PROCESS-CC.                                                         
      *****************************************************************         
      *   BUILD EMAIL MESSAGES FROM CONTROL CARD INPUT                          
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7000-PROCESS-CC'        TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           EVALUATE TRUE                                                        
             WHEN WS-CC-COMMENT                                                 
               CONTINUE                                                         
             WHEN WS-CC-ENGLISH                                                 
               ADD 1                     TO WS-EMAIL-ENG-COUNT                  
               IF WS-EMAIL-ENG-COUNT     >  100                                 
                   DISPLAY 'NUMBER OF EMAIL LINES EXCEEDS THE '                 
                           'TABLE CAPASITY'                                     
                   PERFORM 9999-ABEND                                           
                      THRU 9999-EXIT                                            
               ELSE                                                             
                   MOVE WS-CC-TEXT       TO WS-EMAIL-ENG-TEXT                   
                                           (WS-EMAIL-ENG-COUNT)                 
               END-IF                                                           
             WHEN WS-CC-FRENCH                                                  
               ADD 1                     TO WS-EMAIL-FR-COUNT                   
               IF WS-EMAIL-FR-COUNT      >  100                                 
                   DISPLAY 'NUMBER OF EMAIL LINES EXCEEDS THE '                 
                           'TABLE CAPASITY'                                     
                   PERFORM 9999-ABEND                                           
                      THRU 9999-EXIT                                            
               ELSE                                                             
                   MOVE WS-CC-TEXT       TO WS-FR-INPUT-LINE                    
                   PERFORM 8100-CHECK-FOR-ACCENTS                               
                      THRU 8100-EXIT                                            
                   MOVE WS-FR-OUTPUT-LINE                                       
                                         TO WS-EMAIL-FR-TEXT                    
                                           (WS-EMAIL-FR-COUNT)                  
               END-IF                                                           
             WHEN OTHER                                                         
               DISPLAY 'INVALID CONTROL CARD'                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-EVALUATE.                                                        
                                                                                
       7000-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       7100-FIND-EMAIL-ADDR.                                                    
      *****************************************************************         
      *    GET THE USERS EMAIL ADDRESS                                          
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7100-FIND-EMAIL-ADDR'   TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE GCCCSCD1-USER-ID         TO USER-ID OF DCLTUT.                  
                                                                                
           EXEC SQL                                                             
             SELECT                                                             
               B.EMAIL_ADDR                                                     
              ,B.LANG_CD                                                        
             INTO                                                               
              :DCLTCUST.EMAIL-ADDR                                              
             ,:DCLTCUST.LANG-CD                                                 
             FROM                                                               
               TUT                       A                                      
              ,TCUST                     B                                      
             WHERE                                                              
               A.USER_ID                  = :DCLTUT.USER-ID                     
             AND                                                                
               A.CUST_ID                 = B.CUST_ID                            
           END-EXEC.                                                            
                                                                                
           PERFORM 8900-CHECK-SQL-CODE                                          
              THRU 8900-EXIT.                                                   
                                                                                
           IF SQLCODE                    = +100                                 
               SET WS-EMAIL-ADDR-FOUND-NO                                       
                                         TO TRUE                                
           ELSE                                                                 
               IF EMAIL-ADDR OF DCLTCUST = SPACES                               
                   SET WS-EMAIL-ADDR-FOUND-NO                                   
                                         TO TRUE                                
               ELSE                                                             
                   SET WS-EMAIL-ADDR-FOUND                                      
                                         TO TRUE                                
               END-IF                                                           
           END-IF.                                                              
                                                                                
       7100-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       7200-START-EMAIL.                                                        
      *****************************************************************         
      *    WRITE THE BEGINNING OF THE EMAIL                                     
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7200-START-EMAIL'       TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-OUT-DTL-LINE (SUB1)   TO WS-OUT-LINE.                        
                                                                                
           PERFORM 9100-WRITE-EMAIL-FILE                                        
              THRU 9100-EXIT.                                                   
                                                                                
       7200-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       7300-ENG-EMAIL.                                                          
      *****************************************************************         
      *    WRITE THE ENGLISH BODY OF THE EMAIL                                  
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7300-ENG-EMAIL'         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-EMAIL-ENG-TEXT (SUB1) TO WS-OUT-LINE.                        
                                                                                
           PERFORM 9100-WRITE-EMAIL-FILE                                        
              THRU 9100-EXIT.                                                   
                                                                                
       7300-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       7400-FR-EMAIL.                                                           
      *****************************************************************         
      *    WRITE THE FRENCH BODY OF THE EMAIL                                   
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7400-FR-EMAIL'          TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-EMAIL-FR-TEXT (SUB1)  TO WS-OUT-LINE.                        
                                                                                
           PERFORM 9100-WRITE-EMAIL-FILE                                        
              THRU 9100-EXIT.                                                   
                                                                                
       7400-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       7500-CLOSE-EMAIL.                                                        
      *****************************************************************         
      *    WRITE THE CLOSING LINES OF THE EMAIL                                 
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '7500-CLOSE-EMAIL'       TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-OUT-TRAIL1            TO WS-OUT-LINE.                        
           PERFORM 9100-WRITE-EMAIL-FILE                                        
              THRU 9100-EXIT.                                                   
                                                                                
           MOVE WS-OUT-TRAIL2            TO WS-OUT-LINE.                        
           PERFORM 9100-WRITE-EMAIL-FILE                                        
              THRU 9100-EXIT.                                                   
                                                                                
           MOVE WS-OUT-TRAIL3            TO WS-OUT-LINE.                        
           PERFORM 9100-WRITE-EMAIL-FILE                                        
              THRU 9100-EXIT.                                                   
                                                                                
       7500-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       8100-CHECK-FOR-ACCENTS.                                                  
      *****************************************************************         
      * THIS PARAGRAPH CONTROLS THE PROCESSING FOR CONVERTING FRENCH            
      * ACCENT CHARACTERS. PARAMETERS ARE:                                      
      *     WS-FR-INPUT-LINE  : INPUT  LINES                                    
      *     WS-FR-OUTPUT-LINE : OUTPUT LINES                                    
      *****************************************************************         
           ADD 1 TO LVL.                                                        
           MOVE '8100-CHECK-FOR-ACCENTS' TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE SPACES                   TO WS-FR-OUTPUT-LINE.                  
           MOVE ZERO                     TO WS-FR-OUTPUT-LINE-SUB.              
      *****************************************************************         
      * SET THE CURRENT INPUT LINE MAXIMUM BASED ON THE LAST                    
      * NON SPACE CHARACTER IN THE LINE.                                        
      *****************************************************************         
           PERFORM                                                              
                   VARYING WS-FR-INPUT-LINE-CURR-MAX                            
                      FROM WS-FR-INPUT-LINE-MAX BY -1                           
                     UNTIL WS-FR-INPUT-LINE-CURR-MAX < 1                        
                        OR WS-FR-INPUT-CHAR (WS-FR-INPUT-LINE-CURR-MAX)         
                           NOT = SPACES                                         
           END-PERFORM.                                                         
           PERFORM 8200-CHECK-CHARS                                             
              THRU 8200-EXIT                                                    
                   VARYING WS-FR-INPUT-LINE-SUB FROM 1 BY 1                     
                     UNTIL WS-FR-INPUT-LINE-SUB                                 
                         > WS-FR-INPUT-LINE-CURR-MAX.                           
       8100-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       8200-CHECK-CHARS.                                                        
      *****************************************************************         
      * THIS PARAGRAPH WILL CHECK EACH CHARACTER IN THE INPUT LINE              
      * TO SEE IF IT IS A FRENCH ACCENT. IF NOT, IT IS MOVED DIRECTLY           
      * TO THE OUTPUT LINE.                                                     
      *                                                                         
      * IF IT IS A FRENCH CHARACTER, THE ENCODING IS OBTAINED AND               
      * MOVED TO THE OUTPUT LINE.                                               
      *****************************************************************         
           ADD 1 TO LVL.                                                        
           MOVE '8200-CHECK-CHARS'       TO AB-PARAGRAPH-NAME (LVL).            
      *****************************************************************         
      * CHECK THAT WE HAVE NOT EXCEEDED THE OUTPUT LINE LENGTH.                 
      *****************************************************************         
           IF (WS-FR-OUTPUT-LINE-SUB + 1) > WS-FR-OUTPUT-LINE-MAX               
               DISPLAY 'THE INPUT LINE IS TOO LARGE AFTER CONVERTING '          
                       'THE FRENCH ACCENTS (1)'                                 
               DISPLAY WS-FR-INPUT-LINE                                         
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT.                                               
      *****************************************************************         
      * IF IT'S NOT A FRENCH ACCENT, MOVE IT TO THE OUTPUT LINE & EXIT          
      *****************************************************************         
           MOVE WS-FR-INPUT-CHAR (WS-FR-INPUT-LINE-SUB) TO WS-CHAR.             
           IF  NOT WS-CHAR-FRENCH-ACCENT                                        
               ADD 1                     TO WS-FR-OUTPUT-LINE-SUB               
               MOVE WS-FR-INPUT-CHAR  (WS-FR-INPUT-LINE-SUB) TO                 
                    WS-FR-OUTPUT-CHAR (WS-FR-OUTPUT-LINE-SUB)                   
               GO TO 8200-EXIT.                                                 
      *****************************************************************         
      * CHECK TO SEE WHICH FRENCH ACCENT IT IS.                                 
      *****************************************************************         
           PERFORM                                                              
               VARYING WS-FAT-SUB FROM 1 BY 1                                   
                 UNTIL WS-FAT-SUB  >   WS-FAT-MAX                               
                    OR WS-CHAR     =   WS-FAT-FRENCH-CHAR (WS-FAT-SUB)          
           END-PERFORM.                                                         
      *****************************************************************         
      * WE SHOULD NEVER PASS THIS TEST, BUT JUST IN CASE.                       
      *****************************************************************         
           IF  WS-FAT-SUB > WS-FAT-MAX                                          
               DISPLAY 'THE FRENCH ACCENT WAS NOT FOUND IN THE TABLE '          
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT.                                               
           MOVE WS-FAT-ENCODED-CHAR (WS-FAT-SUB) TO WS-ENCODED-CHARS.           
      *****************************************************************         
      * CHECK THAT WE HAVE NOT EXCEEDED THE OUTPUT LINE LENGTH.                 
      *****************************************************************         
           IF (WS-ENCODED-CHAR-MAX + WS-FR-OUTPUT-LINE-SUB) >                   
               WS-FR-OUTPUT-LINE-MAX                                            
               DISPLAY 'THE INPUT LINE IS TOO LARGE AFTER CONVERTING '          
                       'THE FRENCH ACCENTS (2)'                                 
               DISPLAY WS-FR-INPUT-LINE                                         
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT.                                               
      *****************************************************************         
      * MOVE THE ENCODING TO THE OUTPUT LINE                                    
      *****************************************************************         
           PERFORM                                                              
               VARYING WS-ENCODED-CHAR-SUB  FROM 1 BY 1                         
                 UNTIL WS-ENCODED-CHAR-SUB   >   WS-ENCODED-CHAR-MAX            
               ADD 1                         TO  WS-FR-OUTPUT-LINE-SUB          
               MOVE WS-ENCODED-CHAR   (WS-ENCODED-CHAR-SUB)                     
                 TO WS-FR-OUTPUT-CHAR (WS-FR-OUTPUT-LINE-SUB)                   
           END-PERFORM.                                                         
       8200-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
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
                     THRU 9999-EXIT                                             
                                                                                
           END-EVALUATE.                                                        
                                                                                
       8900-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       9000-READ-REQUEST.                                                       
      *****************************************************************         
      * READ THE NEXT EMAIL REQUEST                                             
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '9000-READ-REQUEST'      TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-EMAIL-IN-LR           TO LOGICAL-RECORD-NAME.                
                                                                                
           CALL WS-GAEDATSR              USING WS-GAEDATSR-VERB                 
                                               GCCCSCD1-RECORD                  
                                               ICBM.                            
                                                                                
           IF LR-NOT-FOUND                                                      
               SET WS-EOF                TO TRUE                                
               GO TO 9000-EXIT                                                  
           END-IF.                                                              
                                                                                
           IF NOT LR-STATUS-OK                                                  
               DISPLAY 'ERROR READING EMAIL REQUEST FILE '                      
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
           ADD 1 TO                      WS-READ-CNTR.                          
                                                                                
       9000-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
       9100-WRITE-EMAIL-FILE.                                                   
      *****************************************************************         
      * CALL DATA SERVER TO WRITE EMAIL LINES                                   
      *****************************************************************         
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '9100-WRITE-EMAIL-FILE'                                         
                                         TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           MOVE WS-EMAIL-OUT-LR          TO LOGICAL-RECORD-NAME.                
           MOVE STORE-LR                 TO WS-GAEDATSR-VERB.                   
                                                                                
           CALL WS-GAEDATSR              USING WS-GAEDATSR-VERB                 
                                               WS-OUT-LINE                      
                                               ICBM.                            
                                                                                
           IF NOT LR-STATUS-OK                                                  
               DISPLAY 'ERROR WRITING TO EMAIL   FILE'                          
               DISPLAY WS-OUT-LINE                                              
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-IF.                                                              
                                                                                
       9100-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
                                                                                
       9200-READ-CONTROL-CARD.                                                  
      ******************************************************************        
      * READ THE CONTROL CARD                                                   
      ******************************************************************        
                                                                                
           ADD 1 TO LVL.                                                        
           MOVE '9200-READ-CONTROL-CARD' TO AB-PARAGRAPH-NAME (LVL).            
                                                                                
           CALL WS-GAEDATSR              USING WS-GAEDATSR-VERB                 
                                               WS-CC-RECORD                     
                                               ICBM.                            
                                                                                
           EVALUATE TRUE                                                        
             WHEN LR-NOT-FOUND                                                  
               SET WS-CC-EOF             TO TRUE                                
             WHEN LR-STATUS-OK                                                  
               PERFORM 7000-PROCESS-CC                                          
                  THRU 7000-EXIT                                                
             WHEN OTHER                                                         
               DISPLAY 'ERROR READING CONTROL CARD'                             
               DISPLAY PROGRAM-LINKAGE-STATUS                                   
               PERFORM 9999-ABEND                                               
                  THRU 9999-EXIT                                                
           END-EVALUATE.                                                        
                                                                                
           MOVE OBTAIN-NEXT              TO WS-GAEDATSR-VERB.                   
                                                                                
       9200-EXIT.                                                               
           SUBTRACT 1 FROM LVL.                                                 
           EXIT.                                                                
                                                                                
      *****************************************************************         
      * THIS PARAGRAPH IS CALLED IF AN EXCEPTIONAL CONDITION, WHICH             
      * CANNOT ALLOW THE PROGRAM TO CONTINUE NORMALLY, IS FOUND.                
      * MESSAGES GIVING DETAILS OF THE ABEND ARE DISPLAYEDAND THE               
      * AND THE PROGRAM WILL TERMINATE WITH A RETURN CODE OF 16.                
      *****************************************************************         
                                                                                
       9999-ABEND.                                                              
                                                                                
      ***  DISPLAY ABEND MESSAGE                                                
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '*************************************************'.         
           DISPLAY '***** P R O G R A M   T E R M I N A T E D   *****'.         
           DISPLAY '*****          A B N O R M A L L Y          *****'.         
           DISPLAY '*************************************************'.         
           DISPLAY ' '.                                                         
           DISPLAY 'MODULE NAME    : ' AB-MODULE-NAME.                          
           DISPLAY 'PARAGRAPH NAME : ' AB-PARAGRAPH-NAME (LVL).                 
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY 'SQLCODE ', AB-SQLCODE.                                      
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
                                                                        12370000
           MOVE +16 TO RETURN-CODE.                                     12380000
                                                                                
       9999-EXIT.                                                               
           GOBACK.                                                              
