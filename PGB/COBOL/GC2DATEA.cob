       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.       GC2DATEA.                                              
      *-------------------------------------------------------------*           
      * THIS PROGRAM IS EQUIVALENT TO SEVERAL OTHER PROGRAMS                    
      * ANY CHANGES MADE TO THIS PROGRAM SHOULD ALSO BE MADE TO:                
      *                                                                         
      *     GVSDATEA - GLHNEW ENDEVOR                                           
      *     GC2DATEA - GLHSYS LAN - STDSERV                                     
      *     GACDATEA - GLHSYS LAN - STDSERV                                     
      *                                                                         
      * May 1998, I.Bucknell, minor fixes for leap year calculation     May98IB 
      *           and QDATE call compatibility.                         May98IB 
      * Oct 1999, I.Bucknell, patch service "D" bug, and day-of-month   Oct99IB 
      *           edit weaknesses.                                      Oct99IB 
      * Nov 1999, I.Bucknell, okay, it was worse than that.  2-digit    Nov99IB 
      *           Julian calls do not work reliably and as a result     Nov99IB 
      *           have now been shut off.                               Nov99IB 
      * Nov 1999, I.Bucknell, whoops,... and the day of week didn't     Nov99IB 
      *           seem to work quite right either.  Doesn't really      Nov99IB 
      *           surprise me at this point but it does kinda seem      Nov99IB 
      *           unfair that my name is the only one up there really   Nov99IB 
      *           I didn't write it does it seem fair to you like       Nov99IB 
      *           I've been fixing this dang thing for maybe three      Nov99IB 
      *           weeks now will *I* get to party down like everybody   Nov99IB 
      *           else nooooooooo good thing the real Millennium is     Nov99IB 
      *           2001 cause that's about how long it'll be before I    Nov99IB 
      *           get walkin' papers from the rubber bubble no-no       Nov99IB 
      *           that's okay I'm fine really don't mind me just a      Nov99IB 
      *           bit tired that's all chainsaw what chainsaw who       Nov99IB 
      *           could have put that there?                            Nov99IB 
      * Sep 2008, IBM, Upgraded in ECU project                          Nov99IB 
      *-------------------------------------------------------------*           
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                                 
       01  MAINLINE-WORKING-STORAGE.                                            
           05  FILLER                                  PIC X(27)                
                   VALUE '* GC2DATEA - WS STARTS HERE'.                         
       01  LOW-DATE2.                                                           
           03 LD-ALIS-YY2                       COMP-3 PIC S999 VALUE 0.        
           03 LD-ALIS-DD2                       COMP-3 PIC S999 VALUE 0.        
                                                                                
      *   LOW-DATE2 LOOKS AFTER HISTORICAL COMPATABILITY PROBLEMS               
      *   BETWEEN GACDATE (OLD) AND GACDATEA (OLD & NEW).                       
                                                                                
       01  CII-HIGH-DATE VALUE +9999999         COMP-3 PIC S9(7).               
       01  CII-LOW-DATE  VALUE +0000000         COMP-3 PIC S9(7).               
       01  JULIAN-HIGH-DATE VALUE  9999999             PIC 9(7).                
       01  JULIAN-LOW-DATE  VALUE  ZERO                PIC 9(7).                
       01  DEFAULT-WINDOW.                                                      
           03 DW-YEARS                                 PIC S99 VALUE 15.        
           03 DW-MONTHS                                PIC S99 VALUE 0.         
           03 DW-DAYS                                  PIC S99 VALUE 0.         
       01  WORKING-DATE.                                                        
           03 WRK-YEAR                          COMP-3 PIC S9(5).               
           03 WRK-MTH                           COMP-3 PIC S9(5).               
           03 WRK-DAY                           COMP-3 PIC S9(5).               
       01  CHECK-DATE.                                                          
           03 CHD-YEAR                                 PIC 9(4).                
           03 CHD-MTH                                  PIC 99.                  
           03 CHD-DAY                                  PIC 99.                  
       01  CHD REDEFINES CHECK-DATE                    PIC 9(8).                
       01  CHD-XX REDEFINES CHECK-DATE.                                         
           03 CHD-CC                                   PIC 99.                  
           03 CHD-YY                                   PIC 99.                  
      /                                                                         
                                                                                
       01  VDATE-WORK-AREA.                                                     
           05  WORK-REQUEST-AREA.                                               
               10  WORK-REQ-SERVICE                        PIC X.               
                   88  VALID-FUNCTION             VALUES '1' THRU '9'           
                                                         'A' THRU 'E'.          
               10  WORK-REQ-BASIS                          PIC X.               
                   88  VALID-BASIS        VALUES 'A' 'B' 'C' 'D' 'E'.           
                   88  LEAPYEAR-BASIS             VALUE  'A'.                   
                   88  NORMAL-YEAR-BASIS          VALUE  'B'.                   
                   88  INSURANCE-YEAR-BASIS       VALUE  'C'.                   
                   88  LEAPYEAR-FORWARD-BASIS     VALUE  'D'.                   
                   88  LEAPYEAR-BACKWARD-BASIS    VALUE  'E'.                   
                   88  VALID-FUNCT1-BASIS VALUES 'A' 'B' 'C' 'D' 'E'.           
                   88  VALID-FUNCT2-BASIS VALUES 'A' 'B' 'C' 'D' 'E'.           
                   88  VALID-FUNCT3-BASIS VALUES 'A' 'B'.                       
                   88  VALID-FUNCT4-BASIS VALUES 'A' 'B'.                       
                   88  VALID-FUNCT5-BASIS VALUES 'A' 'B'.                       
                   88  VALID-FUNCT6-BASIS VALUE      'B'.                       
                   88  VALID-FUNCT7-BASIS VALUE      'B'.                       
                   88  VALID-FUNCT8-BASIS VALUES 'A' 'B'     'D' 'E'.           
                   88  VALID-FUNCT9-BASIS VALUES 'A' 'B'     'D' 'E'.           
                   88  VALID-FUNCTA-BASIS VALUES 'A' 'B'.                       
                   88  VALID-FUNCTB-BASIS VALUES 'A' 'B' 'C' 'D' 'E'.           
                   88  VALID-FUNCTD-BASIS VALUES 'A' 'B'     'D' 'E'.    Nov99IB
                                                                                
               10  WORK-REQ-DETAIL                         PIC X.               
                   88  VALID-FUNCT1-DETAIL        VALUES '1' '2' '3'.           
                   88  VALID-FUNCT2-DETAIL        VALUES '1' '2'.               
                   88  VALID-FUNCT3-DETAIL        VALUES '1' '2' '3'.           
                   88  VALID-FUNCT7-DETAIL        VALUES '1' '2' '3'.           
                   88  VALID-FUNCTB-DETAIL        VALUES '1' '2' '3'.           
                   88  VALID-FUNCTD-DETAIL        VALUE  '1'.            Nov99IB
                                                                                
               10  WORK-REQ-LANGUAGE                       PIC X.               
                   88  VALID-LANGUAGE             VALUES 'E' 'F'.               
                   88  ENGLISH                    VALUE  'E'.                   
                   88  FRENCH                     VALUE  'F'.                   
               10  FILLER                               PIC XXX.                
      /                                                                         
           05  WORK-EXT-DATE.                                                   
               10  WORK-EXT-DAY                            PIC 99.              
               10  WORK-EXT-MONTH                          PIC XXX.             
               10  WORK-EXT-YEAR-X.                                             
                   15  WORK-EXT-YEAR                       PIC 9999.            
           05  WORK-DDMMYY           REDEFINES WORK-EXT-DATE.                   
               10  WORK-DD                                 PIC 99.              
               10  WORK-MM                                 PIC 99.              
               10  WORK-YY                                 PIC 99.              
           05  WORK-ALIS-DATE1                    COMP-3.                       
               10  WORK-ALIS-YEAR1                         PIC S9(5).           
               10  WORK-ALIS-DAY1                          PIC S9(5).           
           05  WORK-ALIS-DATE2                    COMP-3.                       
               10  WORK-ALIS-YEAR2                         PIC S9(5).           
               10  WORK-ALIS-DAY2                          PIC S9(5).           
           05  WORK-ADJUST-AREA                   COMP-3.                       
               10  WORK-ADJUST-DAYS                        PIC S9(7).           
               10  WORK-DAY-OF-WEEK  REDEFINES WORK-ADJUST-DAYS                 
                                                           PIC S9(7).           
               10  WORK-ADJUST-MONTHS                      PIC S9(5).           
               10  WORK-ADJUST-YEARS                       PIC S9(5).           
           05  WORK-USE-TIME.                                                   
               10  WORK-USE                                PIC XX.              
                   88  VALID-USE        VALUES '0+' '0-' '1+' '2+' '2-'.        
                   88  PLUS-USE                   VALUES '0+' '1+' '2+'.        
                   88  MINUS-USE                  VALUES '0-' '2-'.             
                   88  OFFSET-USE                 VALUES '0+' '0-'.             
                   88  SAME-MONTH-USE             VALUE  '1+'.                  
                   88  DIFF-MONTH-USE             VALUES '2+' '2-'.             
               10  WORK-TIME                      COMP-3   PIC S9(2).           
           05  FILLER                                      PIC X(10).           
       01  WORK-AREA-SPLIT.                                                     
           05  WORK-FULL-YEAR.                                                  
               10  WORK-TEMP-CENTURY       VALUE '19'      PIC XX.              
               10  WORK-TEMP-YEAR                          PIC XX.              
           05  WORK-TEMP-RESULT                   COMP-3   PIC S9(7).           
           05  WORK-REMAINDER                     COMP-3   PIC S9(3).           
           05  SAVE-TAB-ENTRY                     COMP-3   PIC S9(3).           
           05  SAVE-ADJUST-CONSTANT               COMP-3   PIC S9(3).           
      /                                                                         
       01  MONTH-TABLE-WORK-AREA.                                               
           05  TS-RETURN-CODE                              PIC X.               
               88  TS-SEARCH-IN-PROGRESS                   VALUE '0'.           
               88  TS-ENTRY-FOUND                          VALUE '1'.           
               88  TS-ENTRY-NOT-FOUND                      VALUE '2'.           
           05  TS-TAB-ENTRY                       COMP-3   PIC S9(5).           
           05  TS-ALIS-DAY                        COMP-3   PIC S9(5).           
           05  TS-EXT-MONTH                                PIC X(3).            
           05  TS-TABLE-VALUES.                                                 
               10  FILLER        VALUE 'JANJAN31'          PIC X(8).            
               10  FILLER        VALUE +000       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'FEBFEV28'          PIC X(8).            
               10  FILLER        VALUE +031       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'MARMAR31'          PIC X(8).            
               10  FILLER        VALUE +059       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'APRAVR30'          PIC X(8).            
               10  FILLER        VALUE +090       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'MAYMAI31'          PIC X(8).            
               10  FILLER        VALUE +120       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'JUNJUN30'          PIC X(8).            
               10  FILLER        VALUE +151       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'JULJUL31'          PIC X(8).            
               10  FILLER        VALUE +181       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'AUGAOU31'          PIC X(8).            
               10  FILLER        VALUE +212       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'SEPSEP30'          PIC X(8).            
               10  FILLER        VALUE +243       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'OCTOCT31'          PIC X(8).            
               10  FILLER        VALUE +273       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'NOVNOV30'          PIC X(8).            
               10  FILLER        VALUE +304       COMP-3   PIC S9(3).           
               10  FILLER        VALUE 'DECDEC31'          PIC X(8).            
               10  FILLER        VALUE +334       COMP-3   PIC S9(3).           
           05  TS-MONTH-TABLE    REDEFINES TS-TABLE-VALUES                      
                                 OCCURS 12 TIMES.                               
               10  TS-ENGLISH-MONTH                        PIC X(3).            
               10  TS-FRENCH-MONTH                         PIC X(3).            
               10  TS-MAX-DAY                              PIC 9(2).            
               10  TS-ACCUM-DAYS                  COMP-3   PIC S9(3).           
      /                                                                         
       01  LEAP-YEAR-ROUTINE-PARAMETERS.                                        
           05  LP-LEAPYEAR-FLAG                            PIC X.               
               88  LP-NOT-LEAPYEAR                VALUE  '0'.                   
               88  LP-LEAPYEAR                    VALUES '1' '2' '3'.           
               88  LP-LEAPYEAR-AND-MAR            VALUE  '2' '3'.               
               88  LP-LEAPYEAR-NOT-FEB29          VALUE  '2'.                   
               88  LP-LEAPYEAR-FEB29              VALUE  '3'.                   
           05  LP-MAX-DAYS                        COMP-3   PIC S9(3).           
           05  LP-WORK-YEAR                       COMP-3   PIC S9(5).           
           05  LP-TEMP-RESULT                     COMP-3   PIC S9(5).           
           05  LP-REMAINDER                       COMP-3   PIC S9(5).           
           05  LP-ALIS-DATE                       COMP-3.                       
               10  LP-ALIS-YEAR                            PIC S9(5).           
               10  LP-ALIS-DAY                             PIC S9(5).           
                                                                                
       01  VAPS-FIGURATIVE-CONSTANTS.                                           
           COPY GARDATEC.                                                       
           05  ADDITIONAL-CONSTANTS.                                            
               10  CHAR-28         VALUE '28'          PIC XX.                  
               10  CHAR-29         VALUE '29'          PIC XX.                  
               10  PACK-12         VALUE +12           PIC S9(3).               
               10  PACK-13         VALUE +13           PIC S9(3).               
               10  PACK-15         VALUE +15           PIC S9(3).               
               10  PACK-010        VALUE +10           PIC S9(3).               
               10  PACK-020        VALUE +20           PIC S9(3).               
               10  PACK-030        VALUE +30           PIC S9(3).               
               10  PACK-040        VALUE +40           PIC S9(3).               
               10  PACK-050        VALUE +50           PIC S9(3).               
               10  PACK-060        VALUE +60           PIC S9(3).               
               10  PACK-070        VALUE +70           PIC S9(3).               
               10  PACK-080        VALUE +80           PIC S9(3).               
               10  PACK-090        VALUE +90           PIC S9(3).               
               10  PACK-100        VALUE +100          PIC S9(3).               
               10  PACK-101        VALUE +101          PIC S9(3).               
               10  PACK-110        VALUE +110          PIC S9(3).               
               10  PACK-200        VALUE +200          PIC S9(3).               
               10  PACK-365        VALUE +365          PIC S9(3).               
               10  PACK-366        VALUE +366          PIC S9(3).               
               10  PACK-400        VALUE +400          PIC S9(3).               
               10  PACK-999        VALUE +999          PIC S9(3).               
               10  PACK-1800       VALUE +1800         PIC S9(5).               
                                                                                
       01  END-OF-WORKING-STORAGE                      PIC X(27)                
               VALUE '* GC2DATEA - WS  ENDS  HERE'.                             
      /                                                                         
       LINKAGE SECTION.                                                         
       01  VAPS-VDATE-PARAMETERS.                                               
           COPY GARDATEP.                                                       
       01  REAL-SYSTEM-DATE.                                                    
           03 RSD.                                                              
               05 RSD-YEAR                             PIC 9(4) VALUE 0.        
               05 RSD-MONTH                            PIC 99   VALUE 0.        
               05 RSD-DAY                              PIC 99   VALUE 0.        
           03  RSD-CICS REDEFINES RSD.                                          
               05 RSD-CICS-CC                          PIC 99.                  
               05 RSD-CICS-YY                          PIC 99.                  
               05 RSD-CICS-DDD                         PIC S999.                
               05 RSD-CICS-BLANK                       PIC X.                   
      /                                                                         
       PROCEDURE DIVISION USING VAPS-VDATE-PARAMETERS REAL-SYSTEM-DATE.         
       0000-MAINLINE.                                                           
                                                                                
      *    CONVERT CICS JULIAN DATES TO SYSTEM-DATE, OTHERWISE ASSUME           
      *    SYSTEM DATE HAS BEEN PASSED.                                         
                                                                                
           IF RSD-CICS-CC NUMERIC                                               
               IF RSD-CICS-CC = ZERO OR 1                                       
                   ADD 19 TO RSD-CICS-CC.                                       
           IF RSD-CICS-BLANK = SPACE                                            
               MOVE VAPS-VDATE-PARAMETERS TO VDATE-WORK-AREA                    
               MOVE RSD-CICS TO VDATE-JULIAN-DATE                               
               MOVE '1' TO VDATE-REQ-DETAIL                                     
               MOVE 'A' TO VDATE-REQ-BASIS                                      
               PERFORM X300-JULIAN-TO-ALIS   THRU X399X                         
               PERFORM X100-ALIS-TO-YYYYMMDD THRU X199X                         
               MOVE VDATE1-YYYYMMDD TO REAL-SYSTEM-DATE                         
               MOVE VDATE-WORK-AREA TO VAPS-VDATE-PARAMETERS.                   
      ******************************************************************        
      *    NOTE : ALL FUNCTIONS ASSUME THAT AN ERROR IN THE INPUT DATA *        
      *    WILL OCCUR UNTIL EACH IS READY TO ACTUALLY RETURN THE       *        
      *    REQUESTED OUTPUT.                                           *        
      ******************************************************************        
           MOVE CHAR-1 TO VDATE-RET-IND.                                        
           MOVE ZERO   TO VDATE-RET-CODE.                                       
                                                                                
           PERFORM 0100-FIELD-INIT THRU 0100-FIELD-INIT-EXIT.                   
           IF VDATE-RET-CODE NOT EQUAL ZERO  THEN                               
               GO TO 0000-MAINLINE-RETURN.                                      
                                                                                
           IF WORK-REQ-SERVICE EQUAL CHAR-1         THEN                        
               PERFORM 1000-EXT-TO-INT THRU                                     
                       1000-EXT-TO-INT-EXIT                                     
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-2         THEN                        
               PERFORM 2000-INT-TO-EXT THRU                                     
                       2000-INT-TO-EXT-EXIT                                     
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-3         THEN                        
               PERFORM 3000-CALC-DATE-INTERVAL THRU                             
                       3000-CALC-DATE-INTERVAL-EXIT                             
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-4         THEN                        
               PERFORM 4000-ADJUST-INTERNAL-DATE THRU                           
                       4000-ADJUST-INTERNAL-DATE-EXIT                           
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-5         THEN                        
               PERFORM 5000-ADD-FREQUENCY THRU                                  
                       5000-ADD-FREQUENCY-EXIT                                  
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-6         THEN                        
               PERFORM 6000-ADJUST-USE-TIME THRU                                
                       6000-ADJUST-USE-TIME-EXIT                                
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-7         THEN                        
               PERFORM 7000-CALC-AGE THRU                                       
                       7000-CALC-AGE-EXIT                                       
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-8         THEN                        
               PERFORM 8000-CALC-DAY-OF-WEEK THRU                               
                       8000-CALC-DAY-OF-WEEK-EXIT                               
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-9         THEN                        
               PERFORM 9000-INT-TO-DDMMYY    THRU                               
                       9000-INT-TO-DDMMYY-EXIT                                  
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-A         THEN                        
               PERFORM A000-FIND-CLOSEST-DATE THRU                              
                       A000-FIND-CLOSEST-DATE-EXIT                              
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-B         THEN                        
               PERFORM B000-YYYYMMDD THRU B999X                                 
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-C         THEN                        
               PERFORM C000-CLIENTS-II THRU C999X                               
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-D         THEN                        
               PERFORM D000-JULIAN     THRU D999X                               
           ELSE                                                                 
           IF WORK-REQ-SERVICE EQUAL CHAR-E         THEN                        
               PERFORM E000-SYSTEM-DATE THRU E999X.                             
       0000-MAINLINE-RETURN.                                                    
           GOBACK.                                                              
      /                                                                         
       0100-FIELD-INIT.                                                         
      ******************************************************************        
      *   THIS ROUTINE EDITS GLOBAL FIELDS FOR ERRORS AND CAUSES AN    *        
      *   IMMEDIATE RETURN TO THE CALLING PROGRAM. ANY FIELDS THAT     *        
      *   ARE NOT USED BY EVERY ROUTINE ARE EDITTED AND STORED IN THE  *        
      *   WORK AREA IF VALID. AN INVALID FIELD CAUSES SPACES TO BE     *        
      *   STORED IN ITS CORRESPONDING FIELD IN THE WORK AREA.          *        
      ******************************************************************        
                                                                                
           MOVE VDATE-REQUEST-AREA TO WORK-REQUEST-AREA.                        
                                                                                
           IF NOT VALID-FUNCTION  THEN                                          
               MOVE PACK-1 TO VDATE-RET-CODE                                    
               GO TO 0100-FIELD-INIT-EXIT.                                      
                                                                                
           IF NOT VALID-BASIS  THEN                                             
               MOVE PACK-2 TO VDATE-RET-CODE                                    
               GO TO 0100-FIELD-INIT-EXIT.                                      
           MOVE VDATE-EXT-DATE TO WORK-EXT-DATE.                                
                                                                                
           IF NOT VALID-LANGUAGE                                        May98IB 
               MOVE 'E' TO WORK-REQ-LANGUAGE.                           May98IB 
                                                                        May98IB 
           IF VDATE-REQ-SERVICE = 'E' AND VDATE-REQ-BASIS = 'B'         May98IB 
               MOVE 'E' TO VDATE-REQ-BASIS.                             May98IB 
                                                                        May98IB 
           IF VDATE-ALIS-DATE1 NOT = LOW-DATE AND                               
              VDATE-ALIS-DATE1 NOT = LOW-DATE2                                  
               IF VDATE-ALIS-YEAR1 NOT NUMERIC OR                               
                  VDATE-ALIS-DAY1  NOT NUMERIC OR                               
                  VDATE-ALIS-YEAR1 LESS THAN ZERO OR                            
                  VDATE-ALIS-DAY1  LESS THAN PACK-1 THEN                        
                   MOVE SPACES TO WORK-ALIS-DATE1                               
                 ELSE                                                           
                   MOVE VDATE-ALIS-YEAR1 TO WORK-ALIS-YEAR1                     
                   MOVE VDATE-ALIS-DAY1  TO WORK-ALIS-DAY1                      
                   MOVE WORK-ALIS-DATE1   TO LP-ALIS-DATE                       
                   PERFORM Z400-LEAPYEAR-RTN THRU Z499X                         
                   IF WORK-ALIS-DAY1 GREATER THAN LP-MAX-DAYS                   
                       MOVE SPACES TO WORK-ALIS-DATE1.                          
                                                                                
           IF VDATE-ALIS-YEAR2 NOT NUMERIC OR                                   
              VDATE-ALIS-DAY2  NOT NUMERIC OR                                   
              VDATE-ALIS-YEAR2 LESS THAN ZERO OR                                
              VDATE-ALIS-DAY2  LESS THAN PACK-1 THEN                            
               MOVE SPACES TO WORK-ALIS-DATE2                                   
           ELSE                                                                 
               MOVE VDATE-ALIS-YEAR2 TO WORK-ALIS-YEAR2                         
               MOVE VDATE-ALIS-DAY2  TO WORK-ALIS-DAY2                          
               MOVE WORK-ALIS-DATE2   TO LP-ALIS-DATE                           
               PERFORM Z400-LEAPYEAR-RTN THRU Z499X                             
               IF WORK-ALIS-DAY2 GREATER THAN LP-MAX-DAYS                       
                   MOVE SPACES TO WORK-ALIS-DATE2.                              
      /                                                                         
           IF VDATE-ADJUST-YEARS  NOT NUMERIC OR                                
              VDATE-ADJUST-MONTHS NOT NUMERIC OR                                
              VDATE-ADJUST-DAYS   NOT NUMERIC  THEN                             
               MOVE SPACES TO WORK-ADJUST-AREA                                  
           ELSE                                                                 
               MOVE VDATE-ADJUST-YEARS  TO WORK-ADJUST-YEARS                    
               MOVE VDATE-ADJUST-MONTHS TO WORK-ADJUST-MONTHS                   
               MOVE VDATE-ADJUST-DAYS   TO WORK-ADJUST-DAYS.                    
                                                                                
           MOVE VDATE-USE-TIME TO WORK-USE-TIME.                                
       0100-FIELD-INIT-EXIT.                                                    
           EXIT.                                                                
      /                                                                         
       1000-EXT-TO-INT.                                                         
      ******************************************************************        
      *    THIS ROUTINE CONVERTS A DATE FROM DDMMMYY(YY) FORMAT TO     *        
      *    ALIS DATE FORMAT. IF THE DATE IS INVALID, ZEROS ARE RETURNED*        
      *    FEB/1996: FOR BASIS 'D' AND 'E', 29FEB IS VALID, BUT THE    *        
      *    CONVERSION CONSIDER AS NON-LEAP-YEAR                        *        
      *    IE. 28 FEB IS 59TH DAY,                                     *        
      *        29 FEB IS 59TH DAY FOR 'E' BASIS                        *        
      *        29 FEB IS 60TH DAY FOR 'D' BASIS                        *        
      *        01 MAR IS 60TH DAY FOR BOTH 'D' AND 'E' BASIS           *        
      ******************************************************************        
                                                                                
           MOVE PACK-010 TO VDATE-RET-CODE.                                     
           MOVE LOW-DATE TO VDATE-ALIS-DATE1.                                   
           IF NOT VALID-FUNCT1-DETAIL  THEN                                     
               ADD PACK-3 TO VDATE-RET-CODE                                     
               GO TO 1000-EXT-TO-INT-EXIT.                                      
           IF NOT VALID-LANGUAGE THEN                                           
               ADD PACK-4 TO VDATE-RET-CODE                                     
               GO TO 1000-EXT-TO-INT-EXIT.                                      
                                                                                
           PERFORM Y200-EDIT-EXTERNAL THRU Y299X.                               
           IF VDATE-RET-CODE NOT = PACK-010                                     
               GO TO 1000-EXT-TO-INT-EXIT.                                      
                                                                                
           PERFORM X500-EXTERNAL-TO-YYYYMMDD THRU X599X.                        
           IF WORK-REQ-DETAIL EQUAL CHAR-2 OR CHAR-3                            
               PERFORM Z800-ADJUST-WINDOW    THRU Z899X.                        
                                                                                
           PERFORM Y000-EDIT-YYYYMMDD THRU Y099X.                               
           IF VDATE-RET-CODE NOT = PACK-010                                     
               GO TO 1000-EXT-TO-INT-EXIT.                                      
                                                                                
           PERFORM X000-YYYYMMDD-TO-ALIS THRU X099X.                            
           PERFORM X600-YYYYMMDD-TO-CII  THRU X699X.                            
                                                                                
           MOVE ZERO            TO VDATE-RET-IND.                               
           MOVE ZERO            TO VDATE-RET-CODE.                              
       1000-EXT-TO-INT-EXIT.                                                    
           EXIT.                                                                
      /                                                                         
       2000-INT-TO-EXT.                                                         
      ******************************************************************        
      *    THIS FUNCTION CONVERTS AN ALIS DATE TO EITHER DDMMMYYYY OR  *        
      *    DDMMMYY FORMAT. AN INVALID ALIS DATE CAUSES ALL '*' TO BE   *        
      *    RETURNED.                                                   *        
      ******************************************************************        
                                                                                
           MOVE PACK-020 TO VDATE-RET-CODE.                                     
           MOVE ALL '*'  TO VDATE-EXT-DATE.                                     
           IF NOT VALID-FUNCT2-DETAIL  THEN                                     
               ADD PACK-3 TO VDATE-RET-CODE                                     
               GO TO 2000-INT-TO-EXT-EXIT.                                      
           IF NOT VALID-LANGUAGE THEN                                           
               ADD PACK-4 TO VDATE-RET-CODE                                     
               GO TO 2000-INT-TO-EXT-EXIT.                                      
           IF VDATE-ALIS-DATE1 EQUAL LOW-DATE OR LOW-DATE2                      
               MOVE '01JAN0000' TO VDATE-EXT-DATE                               
               MOVE '00000101' TO VDATE1-YYYYMMDD                               
               MOVE CII-LOW-DATE TO VDATE-CII-DATE                              
               MOVE ZERO TO VDATE-RET-IND                                       
                            VDATE-RET-CODE                                      
               GO TO 2000-INT-TO-EXT-EXIT.                                      
           IF WORK-ALIS-DATE1 EQUAL SPACES  THEN                                
               ADD PACK-5 TO VDATE-RET-CODE                                     
               GO TO 2000-INT-TO-EXT-EXIT.                                      
           IF VDATE-ALIS-DATE1 EQUAL HIGH-DATE                                  
               MOVE '31DEC9999' TO VDATE-EXT-DATE                               
               MOVE '99991231' TO VDATE1-YYYYMMDD                               
               MOVE CII-HIGH-DATE TO VDATE-CII-DATE                             
               MOVE ZERO TO VDATE-RET-IND                                       
                            VDATE-RET-CODE                                      
               GO TO 2000-INT-TO-EXT-EXIT.                                      
                                                                                
           PERFORM X100-ALIS-TO-YYYYMMDD     THRU X199X.                        
           PERFORM X400-YYYYMMDD-TO-EXTERNAL THRU X499X.                        
           PERFORM X600-YYYYMMDD-TO-CII THRU X699X.                             
                                                                                
           MOVE ZERO          TO VDATE-RET-IND.                                 
           MOVE ZERO          TO VDATE-RET-CODE.                                
       2000-INT-TO-EXT-EXIT.                                                    
           EXIT.                                                                
      /                                                                         
       3000-CALC-DATE-INTERVAL.                                                 
      ******************************************************************        
      *    THIS ROUTINE CALCULATES THE NUMBER OF DAYS, NUMBER OF DAYS  *        
      *    AND MONTHS, OR NUMBER OF DAYS, MONTHS AND YEARS BETWEEN TWO *        
      *    DATES. THE LOW DATE IS ASSUMED TO BE IN ALIS DATE1.         *        
      ******************************************************************        
                                                                                
           MOVE PACK-030 TO VDATE-RET-CODE.                                     
           IF NOT VALID-FUNCT3-BASIS  THEN                                      
               ADD PACK-2 TO VDATE-RET-CODE                                     
               GO TO 3000-CALC-DATE-INTERVAL-EXIT.                              
           IF NOT VALID-FUNCT3-DETAIL  THEN                                     
               ADD PACK-3 TO VDATE-RET-CODE                                     
               GO TO 3000-CALC-DATE-INTERVAL-EXIT.                              
           IF WORK-ALIS-DATE1 EQUAL SPACES OR                                   
              WORK-ALIS-DATE2 EQUAL SPACES   THEN                               
               ADD PACK-5 TO VDATE-RET-CODE                                     
               GO TO 3000-CALC-DATE-INTERVAL-EXIT.                              
           IF WORK-ALIS-DATE2 LESS THAN WORK-ALIS-DATE1  THEN                   
               ADD PACK-6 TO VDATE-RET-CODE                                     
               GO TO 3000-CALC-DATE-INTERVAL-EXIT.                              
                                                                                
           COMPUTE WORK-ADJUST-YEARS = WORK-ALIS-YEAR2 -                        
                                       WORK-ALIS-YEAR1.                         
           IF WORK-REQ-DETAIL EQUAL CHAR-3  THEN                                
               PERFORM 3100-CALC-DAYS THRU 3100-CALC-DAYS-EXIT                  
               GO TO 3000-CALC-DATE-INTERVAL-EXIT.                              
                                                                                
           MOVE WORK-ALIS-DATE1 TO LP-ALIS-DATE.                                
           PERFORM Z400-LEAPYEAR-RTN THRU Z499X                                 
           MOVE WORK-ALIS-DAY1 TO TS-ALIS-DAY.                                  
           IF LP-LEAPYEAR-AND-MAR  THEN                                         
               SUBTRACT PACK-1 FROM TS-ALIS-DAY.                                
           IF LP-LEAPYEAR-NOT-FEB29  THEN                                       
               SUBTRACT PACK-1 FROM WORK-ALIS-DAY1.                             
           PERFORM Z200-ACCUM-DAYS-TABSRCH THRU                                 
                   Z200-ACCUM-DAYS-TABSRCH-EXIT.                                
           COMPUTE WORK-ADJUST-MONTHS = ZERO - TS-TAB-ENTRY.                    
           COMPUTE WORK-ADJUST-DAYS = TS-ACCUM-DAYS (TS-TAB-ENTRY) -            
                                      WORK-ALIS-DAY1.                           
           MOVE WORK-ALIS-DATE2 TO LP-ALIS-DATE.                                
           PERFORM Z400-LEAPYEAR-RTN THRU Z499X                                 
           MOVE WORK-ALIS-DAY2 TO TS-ALIS-DAY.                                  
           IF LP-LEAPYEAR-AND-MAR  THEN                                         
               SUBTRACT PACK-1 FROM TS-ALIS-DAY.                                
           IF LP-LEAPYEAR-NOT-FEB29  THEN                                       
               SUBTRACT PACK-1 FROM WORK-ALIS-DAY2.                             
           PERFORM Z200-ACCUM-DAYS-TABSRCH THRU                                 
                   Z200-ACCUM-DAYS-TABSRCH-EXIT.                                
           ADD TS-TAB-ENTRY TO WORK-ADJUST-MONTHS.                              
           COMPUTE WORK-ADJUST-DAYS = WORK-ADJUST-DAYS +                        
                                      WORK-ALIS-DAY2 -                          
                                      TS-ACCUM-DAYS (TS-TAB-ENTRY).             
      /                                                                         
           SUBTRACT PACK-1 FROM TS-TAB-ENTRY.                                   
           IF TS-TAB-ENTRY EQUAL ZERO  THEN                                     
               MOVE PACK-12 TO TS-TAB-ENTRY.                                    
                                                                                
           IF WORK-ADJUST-DAYS LESS THAN ZERO  THEN                             
               SUBTRACT PACK-1 FROM WORK-ADJUST-MONTHS                          
               ADD TS-MAX-DAY (TS-TAB-ENTRY) TO WORK-ADJUST-DAYS                
               IF LP-LEAPYEAR AND TS-TAB-ENTRY EQUAL PACK-2  THEN               
                   ADD PACK-1 TO WORK-ADJUST-DAYS.                              
           IF WORK-ADJUST-MONTHS LESS THAN ZERO  THEN                           
               SUBTRACT PACK-1 FROM WORK-ADJUST-YEARS                           
               ADD PACK-12 TO WORK-ADJUST-MONTHS.                               
                                                                                
           IF WORK-REQ-DETAIL EQUAL CHAR-2                                      
               COMPUTE WORK-ADJUST-MONTHS = WORK-ADJUST-MONTHS +                
                                            WORK-ADJUST-YEARS * PACK-12         
               MOVE ZERO TO WORK-ADJUST-YEARS.                                  
                                                                                
           IF WORK-ADJUST-MONTHS GREATER THAN PACK-999  THEN                    
               ADD PACK-7 TO VDATE-RET-CODE                                     
               GO TO 3000-CALC-DATE-INTERVAL-EXIT.                              
                                                                                
           MOVE WORK-ADJUST-DAYS   TO VDATE-ADJUST-DAYS.                        
           MOVE WORK-ADJUST-MONTHS TO VDATE-ADJUST-MONTHS.                      
           MOVE WORK-ADJUST-YEARS  TO VDATE-ADJUST-YEARS.                       
           MOVE ZERO               TO VDATE-RET-IND.                            
           MOVE ZERO               TO VDATE-RET-CODE.                           
                                                                                
       3000-CALC-DATE-INTERVAL-EXIT.                                            
           EXIT.                                                                
      /                                                                         
       3100-CALC-DAYS.                                                          
      ******************************************************************        
      *    THIS PARAGRAPH IS CALLED WHEN THE NUMBER OF DAYS (ONLY)     *        
      *    IS REQUIRED.                                                *        
      ******************************************************************        
           COMPUTE WORK-ADJUST-DAYS = WORK-ADJUST-YEARS * PACK-365 +            
                                      WORK-ALIS-DAY2 - WORK-ALIS-DAY1.          
           MOVE WORK-ALIS-DATE1 TO LP-ALIS-DATE.                                
           PERFORM 3200-ADD-IN-FEB29 THRU 3200-ADD-IN-FEB29-EXIT                
               UNTIL LP-ALIS-YEAR EQUAL TO WORK-ALIS-YEAR2.                     
                                                                                
           IF WORK-ADJUST-DAYS GREATER THAN PACK-999  THEN                      
               ADD PACK-8 TO VDATE-RET-CODE                                     
               GO TO 3100-CALC-DAYS-EXIT.                                       
                                                                                
           MOVE ZERO TO VDATE-ADJUST-YEARS.                                     
           MOVE ZERO TO VDATE-ADJUST-MONTHS.                                    
           MOVE WORK-ADJUST-DAYS TO VDATE-ADJUST-DAYS.                          
           MOVE ZERO TO VDATE-RET-IND.                                          
           MOVE ZERO TO VDATE-RET-CODE.                                         
       3100-CALC-DAYS-EXIT.                                                     
           EXIT.                                                                
                                                                                
       3200-ADD-IN-FEB29.                                                       
      ******************************************************************        
      *    FOR EACH LEAPYEAR BETWEEN YEAR1 (INCLUSIVE) AND YEAR2       *        
      *    (EXCLUSIVE), ONE DAY IS ADDED TO THE TOTAL NUMBER OF DAYS.  *        
      ******************************************************************        
                                                                                
           PERFORM Z400-LEAPYEAR-RTN THRU Z499X                                 
           IF LP-LEAPYEAR  THEN                                                 
               ADD PACK-1 TO WORK-ADJUST-DAYS.                                  
           ADD PACK-1 TO LP-ALIS-YEAR.                                          
       3200-ADD-IN-FEB29-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
       4000-ADJUST-INTERNAL-DATE.                                               
      ******************************************************************        
      *   THIS ROUTINE ADDS DAYS AND/OR MONTHS AND/OR YEARS TO ALIS    *        
      *   ALIS DATE1, RETURNING THE RESULT IN ALIS DATE2. THE DAYS,    *        
      *   MONTHS, AND YEARS MAY BE POSITIVE, NEGATIVE, OR ZERO.        *        
      ******************************************************************        
                                                                                
           MOVE PACK-040 TO VDATE-RET-CODE.                                     
           IF NOT VALID-FUNCT4-BASIS  THEN                                      
               ADD PACK-2 TO VDATE-RET-CODE                                     
               GO TO 4000-ADJUST-INTERNAL-DATE-EXIT.                            
           IF WORK-ALIS-DATE1  EQUAL SPACES OR                                  
              WORK-ADJUST-AREA EQUAL SPACES   THEN                              
               ADD PACK-5 TO VDATE-RET-CODE                                     
               GO TO 4000-ADJUST-INTERNAL-DATE-EXIT.                            
                                                                                
           COMPUTE WORK-ALIS-YEAR2 = WORK-ALIS-YEAR1 +                          
                                     WORK-ADJUST-YEARS.                         
           MOVE WORK-ALIS-DATE1 TO LP-ALIS-DATE.                                
           PERFORM Z400-LEAPYEAR-RTN THRU Z499X                                 
           IF LP-LEAPYEAR-AND-MAR THEN                                          
               SUBTRACT PACK-1 FROM WORK-ALIS-DAY1.                             
           MOVE WORK-ALIS-DAY1 TO TS-ALIS-DAY.                                  
           PERFORM Z200-ACCUM-DAYS-TABSRCH THRU                                 
                   Z200-ACCUM-DAYS-TABSRCH-EXIT.                                
           COMPUTE WORK-ALIS-DAY2 = WORK-ALIS-DAY1 -                            
                                    TS-ACCUM-DAYS (TS-TAB-ENTRY).               
           ADD WORK-ADJUST-MONTHS TO TS-TAB-ENTRY.                              
           PERFORM 4200-ADJUST-MONTHS-RTN THRU                                  
                   4200-ADJUST-MONTHS-RTN-EXIT                                  
               UNTIL TS-TAB-ENTRY GREATER THAN ZERO AND                         
                     TS-TAB-ENTRY LESS THAN PACK-13.                            
           ADD TS-ACCUM-DAYS (TS-TAB-ENTRY) TO WORK-ALIS-DAY2.                  
                                                                                
           MOVE WORK-ALIS-DATE2 TO LP-ALIS-DATE.                                
           IF LP-LEAPYEAR-FEB29 THEN                                            
               ADD PACK-1 TO WORK-ALIS-DAY2.                                    
           PERFORM Z400-LEAPYEAR-RTN THRU Z499X                                 
           IF LP-LEAPYEAR-AND-MAR THEN                                          
               ADD PACK-1 TO WORK-ALIS-DAY2.                                    
                                                                                
           ADD WORK-ADJUST-DAYS TO WORK-ALIS-DAY2.                              
           PERFORM 4400-ADJUST-DAYS-RTN THRU                                    
                   4400-ADJUST-DAYS-RTN-EXIT                                    
               UNTIL WORK-ALIS-DAY2 GREATER THAN ZERO AND                       
                     WORK-ALIS-DAY2 NOT GREATER THAN LP-MAX-DAYS.               
           IF WORK-ALIS-YEAR2 GREATER THAN PACK-999 OR                          
              WORK-ALIS-YEAR2   LESS  THAN   ZERO     THEN                      
               ADD PACK-6 TO VDATE-RET-CODE                                     
               GO TO 4000-ADJUST-INTERNAL-DATE-EXIT.                            
      /                                                                         
           MOVE WORK-ALIS-YEAR2 TO VDATE-ALIS-YEAR2.                            
           MOVE WORK-ALIS-DAY2  TO VDATE-ALIS-DAY2.                             
           MOVE ZERO            TO VDATE-RET-IND.                               
           MOVE ZERO            TO VDATE-RET-CODE.                              
       4000-ADJUST-INTERNAL-DATE-EXIT.                                          
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       4200-ADJUST-MONTHS-RTN.                                                  
      ******************************************************************        
      *    THIS ROUTINE INSURES THAT THE CURRENT MONTH IS BETWEEN 1 AND*        
      *    12.                                                         *        
      ******************************************************************        
           IF TS-TAB-ENTRY GREATER THAN PACK-12  THEN                           
               ADD PACK-1        TO  WORK-ALIS-YEAR2                            
               SUBTRACT PACK-12 FROM TS-TAB-ENTRY.                              
           IF TS-TAB-ENTRY LESS THAN PACK-1  THEN                               
               SUBTRACT PACK-1  FROM WORK-ALIS-YEAR2                            
               ADD PACK-12       TO  TS-TAB-ENTRY.                              
       4200-ADJUST-MONTHS-RTN-EXIT.                                             
           EXIT.                                                                
                                                                                
       4400-ADJUST-DAYS-RTN.                                                    
      ******************************************************************        
      *    THIS ROUTINE ENSURES THAT THE NUMBER OF DAYS IS BETWEEN 1   *        
      *    AND 365 (OR 366 IF THE YEAR2 IS A LEAPYEAR).                *        
      ******************************************************************        
                                                                                
           IF WORK-ALIS-DAY2 GREATER THAN LP-MAX-DAYS  THEN                     
               ADD PACK-1 TO  WORK-ALIS-YEAR2                                   
               SUBTRACT LP-MAX-DAYS FROM WORK-ALIS-DAY2.                        
                                                                                
           IF WORK-ALIS-DAY2 LESS THAN PACK-1  THEN                             
               SUBTRACT PACK-1 FROM WORK-ALIS-YEAR2                             
               MOVE WORK-ALIS-DATE2 TO LP-ALIS-DATE                             
               PERFORM Z400-LEAPYEAR-RTN THRU Z499X                             
               ADD LP-MAX-DAYS TO WORK-ALIS-DAY2.                               
                                                                                
           MOVE WORK-ALIS-DATE2 TO LP-ALIS-DATE.                                
           PERFORM Z400-LEAPYEAR-RTN THRU Z499X.                                
       4400-ADJUST-DAYS-RTN-EXIT.                                               
           EXIT.                                                                
      /                                                                         
       5000-ADD-FREQUENCY.                                                      
      ******************************************************************        
      *    THIS ROUTINE USES THE ADJUST DATE ROUTINE TO ADD ON A       *        
      *    FREQUENCY TO ALIS DATE1. THE RESULT IS RETURNED IN ALIS     *        
      *    DATE2.                                                      *        
      ******************************************************************        
                                                                                
           MOVE PACK-050 TO VDATE-RET-CODE.                                     
           IF VDATE-ADJUST-MONTHS NOT NUMERIC  THEN                             
               ADD PACK-5 TO VDATE-RET-CODE                                     
               GO TO 5000-ADD-FREQUENCY-EXIT.                                   
                                                                                
           MOVE ZEROS TO WORK-ADJUST-YEARS.                                     
           MOVE ZEROS TO WORK-ADJUST-DAYS.                                      
           MOVE VDATE-ADJUST-MONTHS TO WORK-ADJUST-MONTHS.                      
           PERFORM 4000-ADJUST-INTERNAL-DATE THRU                               
                   4000-ADJUST-INTERNAL-DATE-EXIT.                              
           IF VDATE-RET-CODE NOT EQUAL ZERO  THEN                               
               COMPUTE VDATE-RET-CODE = VDATE-RET-CODE - PACK-040               
                                                       + PACK-050.              
       5000-ADD-FREQUENCY-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
       6000-ADJUST-USE-TIME.                                                    
      ******************************************************************        
      *    THIS ROUTINE DECIDES HOW TO ADJUST ALIS DATE1 ACCORDING TO  *        
      *    THE USE CODE AND CALLS ADJUST DATE ROUTINE TO DO THE ACTUAL *        
      *    ADJUSTING. THE RESULT IS STORED IN ALIS DATE2.              *        
      ******************************************************************        
                                                                                
           MOVE PACK-060 TO VDATE-RET-CODE.                                     
           IF NOT VALID-FUNCT6-BASIS  THEN                                      
               ADD PACK-2 TO VDATE-RET-CODE                                     
               GO TO 6000-ADJUST-USE-TIME-EXIT.                                 
           IF NOT VALID-USE  THEN                                               
               ADD PACK-3 TO VDATE-RET-CODE                                     
               GO TO 6000-ADJUST-USE-TIME-EXIT.                                 
           IF WORK-ALIS-DATE1 EQUAL SPACES OR                                   
              WORK-TIME        NOT NUMERIC   THEN                               
               ADD PACK-5 TO VDATE-RET-CODE                                     
               GO TO 6000-ADJUST-USE-TIME-EXIT.                                 
                                                                                
           MOVE ZEROS TO WORK-ADJUST-YEARS.                                     
           MOVE ZEROS TO WORK-ADJUST-MONTHS.                                    
           MOVE ZEROS TO WORK-ADJUST-DAYS.                                      
                                                                                
           IF MINUS-USE AND OFFSET-USE  THEN                                    
               SUBTRACT WORK-TIME FROM WORK-ADJUST-DAYS                         
           ELSE                                                                 
               MOVE WORK-TIME TO WORK-ADJUST-DAYS.                              
                                                                                
           IF NOT OFFSET-USE  THEN                                              
               MOVE WORK-ALIS-DAY1 TO TS-ALIS-DAY                               
               PERFORM Z200-ACCUM-DAYS-TABSRCH THRU                             
                       Z200-ACCUM-DAYS-TABSRCH-EXIT                             
               MOVE ZERO TO WORK-ALIS-DAY1                                      
               COMPUTE WORK-ADJUST-MONTHS = TS-TAB-ENTRY - PACK-1.              
                                                                                
           IF DIFF-MONTH-USE  THEN                                              
               IF PLUS-USE  THEN                                                
                   ADD PACK-1 TO WORK-ADJUST-MONTHS                             
               ELSE                                                             
                   SUBTRACT PACK-1 FROM WORK-ADJUST-MONTHS.                     
                                                                                
           PERFORM 4000-ADJUST-INTERNAL-DATE THRU                               
                   4000-ADJUST-INTERNAL-DATE-EXIT.                              
           IF VDATE-RET-CODE NOT EQUAL ZERO  THEN                               
               COMPUTE VDATE-RET-CODE = VDATE-RET-CODE - PACK-040               
                                                       + PACK-060.              
       6000-ADJUST-USE-TIME-EXIT.                                               
           EXIT.                                                                
      /                                                                         
       7000-CALC-AGE.                                                           
      ******************************************************************        
      *   THIS ROUTINE DETERMINES THE AGE AS TO NEXT LAST OR NEAREST   *        
      *   BIRTHDAY. THE CALC DATE INTERVAL ROUTINE IS USED TO GET THE  *        
      *   EXACT AGE AND THIS PARAGRAPH RETURNS ONE OF THE ABOVE 3      *        
      *   AGES (IN ADJUST YEARS) ACCORDING TO THE VALUE OF THE DETAIL  *        
      *   FIELD.                                                       *        
      ******************************************************************        
                                                                                
           MOVE PACK-070 TO VDATE-RET-CODE.                                     
           IF NOT VALID-FUNCT7-BASIS  THEN                                      
               ADD PACK-2 TO VDATE-RET-CODE                                     
               GO TO 7000-CALC-AGE-EXIT.                                        
           IF NOT VALID-FUNCT7-DETAIL THEN                                      
               ADD PACK-3 TO VDATE-RET-CODE                                     
               GO TO 7000-CALC-AGE-EXIT.                                        
                                                                                
           MOVE CHAR-1 TO WORK-REQ-DETAIL.                                      
           PERFORM 3000-CALC-DATE-INTERVAL THRU                                 
                   3000-CALC-DATE-INTERVAL-EXIT.                                
           MOVE VDATE-REQ-DETAIL TO WORK-REQ-DETAIL.                            
           IF VDATE-RET-CODE NOT EQUAL ZERO  THEN                               
               COMPUTE VDATE-RET-CODE = VDATE-RET-CODE - PACK-030               
                                                       + PACK-070               
               GO TO 7000-CALC-AGE-EXIT.                                        
                                                                                
           MOVE WORK-ADJUST-YEARS TO VDATE-ADJUST-YEARS.                        
           MOVE ZERO              TO VDATE-RET-IND.                             
           MOVE ZERO              TO VDATE-RET-CODE.                            
                                                                                
           IF WORK-REQ-DETAIL EQUAL CHAR-3  THEN                                
               GO TO 7000-CALC-AGE-EXIT.                                        
           IF WORK-REQ-DETAIL EQUAL CHAR-2  THEN                                
               ADD PACK-1 TO VDATE-ADJUST-YEARS                                 
               GO TO 7000-CALC-AGE-EXIT.                                        
           IF WORK-ADJUST-MONTHS GREATER THAN PACK-6 OR                         
              WORK-ADJUST-MONTHS      EQUAL   PACK-6 AND                        
              WORK-ADJUST-DAYS   GREATER THAN PACK-15  THEN                     
               ADD PACK-1 TO VDATE-ADJUST-YEARS.                                
       7000-CALC-AGE-EXIT.                                                      
           EXIT.                                                                
      /                                                                         
       8000-CALC-DAY-OF-WEEK.                                                   
      ******************************************************************        
      *    DAY OF THE WEEK CALCULATION. (VALID FOR 1900 - 2199 ONLY)   *        
      *       DOW. = NUMBER OF YEARS SINCE 1900 (EACH YEAR HAS 52 FULL *        
      *                    WEEKS AND ONE DAY LEFT OVER)                *        
      *              ADD NUMBER OF DAYS IN CURRENT DATE                *        
      *              DIVIDE NUMBER OF YEARS SINCE 1900 (EXCLUDING      *        
      *                    CURRENT YEAR) BY FOUR AND ADD TO DOW. TO    *        
      *                    ACCOUNT FOR ALL FEB 29TH'S                  *        
      *              DIVIDE BY 7 TO REMOVE FULL WEEKS                  *        
      *              CONVERT SUNDAY'S VALUE FROM 0 TO 7                *        
      *                                                                *        
      *    IF BASIS B AND DATE IS ACTUALLY IN A LEAPYEAR (AND AFTER    *        
      *    FEB 29TH), ADD ON ONE DAY TO GET THE PROPER DAY OF THE WEEK.*        
      ******************************************************************        
                                                                                
           MOVE PACK-080 TO VDATE-RET-CODE.                                     
           IF NOT VALID-FUNCT8-BASIS THEN                                       
               ADD PACK-2 TO VDATE-RET-CODE                                     
               GO TO 8000-CALC-DAY-OF-WEEK-EXIT.                                
           IF WORK-ALIS-DATE1 = SPACES                                          
              OR WORK-ALIS-YEAR1 LESS THAN PACK-100                             
                ADD PACK-5 TO VDATE-RET-CODE                                    
                GO TO 8000-CALC-DAY-OF-WEEK-EXIT.                               
                                                                                
           MOVE WORK-ALIS-DATE1 TO LP-ALIS-DATE.                                
           MOVE CHAR-A          TO WORK-REQ-BASIS.                              
           PERFORM Z400-LEAPYEAR-RTN THRU Z499X.                                
           MOVE VDATE-REQ-BASIS TO WORK-REQ-BASIS.                              
           IF LP-LEAPYEAR-AND-MAR AND NORMAL-YEAR-BASIS THEN                    
               ADD PACK-1 TO WORK-ALIS-DAY1.                                    
                                                                                
           COMPUTE WORK-DAY-OF-WEEK = WORK-ALIS-YEAR1 + WORK-ALIS-DAY1          
                                    - PACK-100.                                 
                                                                                
           COMPUTE WORK-DAY-OF-WEEK = WORK-DAY-OF-WEEK +                        
                   (WORK-ALIS-YEAR1 - PACK-101) / PACK-4 +                      
                   (WORK-ALIS-YEAR1 + 200) / 500.                       Nov99IB 
      * That last bit was to push forward by 1 day starting in 2100     Nov99IB 
      * because 2100 will not be a leap year.  If you're reading this   Nov99IB 
      * in 2199 you better modify it 'cause I'm long since dead and     Nov99IB 
      * finally done with this thing really you JUST CAN'T REACH ME ok? Nov99IB 
                                                                                
           IF LP-LEAPYEAR-FEB29 AND LEAPYEAR-FORWARD-BASIS THEN                 
               ADD PACK-1 TO WORK-DAY-OF-WEEK.                                  
           IF LP-LEAPYEAR-FEB29 AND LEAPYEAR-BACKWARD-BASIS THEN                
               SUBTRACT PACK-1 FROM WORK-DAY-OF-WEEK.                           
                                                                                
           DIVIDE WORK-DAY-OF-WEEK BY PACK-7 GIVING LP-TEMP-RESULT              
                                          REMAINDER WORK-DAY-OF-WEEK.           
                                                                                
           IF WORK-DAY-OF-WEEK EQUAL ZERO THEN                                  
               MOVE PACK-7 TO WORK-DAY-OF-WEEK.                                 
                                                                                
           MOVE WORK-DAY-OF-WEEK TO VDATE-DAY-OF-WEEK.                          
           MOVE ZERO             TO VDATE-RET-IND.                              
           MOVE ZERO             TO VDATE-RET-CODE.                             
       8000-CALC-DAY-OF-WEEK-EXIT.                                              
           EXIT.                                                                
      /                                                                         
       9000-INT-TO-DDMMYY.                                                      
      ******************************************************************        
      *    THIS FUNCTION CONVERTS AN ALIS DATE TO DDMMYY FORMAT.       *        
      *    AN INVALID ALIS DATE CAUSES ALL '*' TO BE RETURNED.         *        
      ******************************************************************        
                                                                                
           MOVE PACK-090 TO VDATE-RET-CODE.                                     
           MOVE SPACES   TO VDATE-EXT-DATE.                                     
           MOVE ALL '*'  TO VDATE-DDMMYY.                                       
                                                                                
           IF NOT VALID-FUNCT9-BASIS  THEN                                      
               ADD PACK-2 TO VDATE-RET-CODE                                     
               GO TO 9000-INT-TO-DDMMYY-EXIT.                                   
           IF WORK-ALIS-DATE1 EQUAL SPACES  THEN                                
               ADD PACK-5 TO VDATE-RET-CODE                                     
               GO TO 9000-INT-TO-DDMMYY-EXIT.                                   
                                                                                
           PERFORM X100-ALIS-TO-YYYYMMDD   THRU X199X.                          
           PERFORM X200-YYYYMMDD-TO-DDMMYY THRU X299X.                          
           PERFORM X600-YYYYMMDD-TO-CII THRU X699X.                             
                                                                                
           MOVE ZERO        TO VDATE-RET-IND.                                   
           MOVE ZERO        TO VDATE-RET-CODE.                                  
       9000-INT-TO-DDMMYY-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
       A000-FIND-CLOSEST-DATE.                                                  
      ******************************************************************        
      *   THIS ROUTINE DETERMINES THE 1ST DATE THAT IS GREATER THAN OR *        
      *   EQUAL TO THE ALIS DATE2 THAT WOULD RESULT BY CONTINUOUSLY    *        
      *   ADDING THE ADJUST CONSTANT (IN ADJUST DAYS) TO ALIS DATE1    *        
      *   THE DATE IS CALCULATED BY DETERMINING THE NUMBER OF DAYS     *        
      *   BETWEEN THE TWO DATES, DIVIDING BY THE CONSTANT, AND ADDING  *        
      *   THE CONSTANT MINUS THE DIVISION DEMANDER TO THE ALIS DATE2.  *        
      ******************************************************************        
                                                                                
           MOVE PACK-110 TO VDATE-RET-CODE.                                     
           IF NOT VALID-FUNCTA-BASIS  THEN                                      
               ADD PACK-2 TO VDATE-RET-CODE                                     
               GO TO A000-FIND-CLOSEST-DATE-EXIT.                               
           IF WORK-ALIS-DATE1 EQUAL SPACES OR                                   
              WORK-ALIS-DATE2 EQUAL SPACES OR                                   
              VDATE-ADJUST-DAYS NOT NUMERIC OR                                  
              VDATE-ADJUST-DAYS EQUAL ZERO  THEN                                
               ADD PACK-5 TO VDATE-RET-CODE                                     
               GO TO A000-FIND-CLOSEST-DATE-EXIT.                               
           IF WORK-ALIS-DATE2 LESS THAN WORK-ALIS-DATE1  THEN                   
               ADD PACK-6 TO VDATE-RET-CODE                                     
               GO TO A000-FIND-CLOSEST-DATE-EXIT.                               
                                                                                
           MOVE VDATE-ADJUST-DAYS TO SAVE-ADJUST-CONSTANT.                      
           MOVE CHAR-3 TO WORK-REQ-DETAIL.                                      
           PERFORM 3000-CALC-DATE-INTERVAL THRU                                 
                   3000-CALC-DATE-INTERVAL-EXIT.                                
                                                                                
           DIVIDE WORK-ADJUST-DAYS BY SAVE-ADJUST-CONSTANT                      
                               GIVING WORK-TEMP-RESULT                          
                            REMAINDER WORK-REMAINDER.                           
           IF WORK-REMAINDER EQUAL ZERO THEN                                    
               MOVE SAVE-ADJUST-CONSTANT TO WORK-REMAINDER.                     
           COMPUTE WORK-ADJUST-DAYS = SAVE-ADJUST-CONSTANT -                    
                                      WORK-REMAINDER.                           
           MOVE ZERO            TO WORK-ADJUST-MONTHS                           
                                   WORK-ADJUST-YEARS.                           
           MOVE WORK-ALIS-DATE2 TO WORK-ALIS-DATE1.                             
           PERFORM 4000-ADJUST-INTERNAL-DATE THRU                               
                   4000-ADJUST-INTERNAL-DATE-EXIT.                              
           IF VDATE-RET-CODE NOT EQUAL ZERO  THEN                               
               COMPUTE VDATE-RET-CODE = VDATE-RET-CODE - PACK-040               
                                                       + PACK-110               
               GO TO A000-FIND-CLOSEST-DATE-EXIT.                               
           MOVE WORK-ALIS-YEAR2      TO VDATE-ALIS-YEAR2.                       
           MOVE WORK-ALIS-DAY2       TO VDATE-ALIS-DAY2.                        
           MOVE SAVE-ADJUST-CONSTANT TO VDATE-ADJUST-DAYS.                      
           MOVE ZERO                 TO VDATE-RET-IND.                          
           MOVE ZERO                 TO VDATE-RET-CODE.                         
       A000-FIND-CLOSEST-DATE-EXIT.                                             
           EXIT.                                                                
      /                                                                         
                                                                                
       B000-YYYYMMDD.                                                           
                                                                                
           MOVE 120 TO VDATE-RET-CODE.                                          
                                                                                
           IF VDATE-REQ-DETAIL > '1'                                            
               MOVE VDATE1-YYYY TO CHD-XX                                       
               IF CHD-YY NOT NUMERIC                                            
                   ADD PACK-6 TO VDATE-RET-CODE                                 
                   GO TO B999X                                          Oct99IB 
               ELSE                                                             
                   MOVE ZERO        TO CHD-CC                                   
                   MOVE CHD-YEAR    TO VDATE1-YYYY.                             
                                                                                
           IF VDATE1-YYYYMMDD EQUAL '99991231'                                  
               MOVE '31DEC9999' TO VDATE-EXT-DATE                               
               MOVE HIGH-DATE TO VDATE-ALIS-DATE1                               
               MOVE CII-HIGH-DATE TO VDATE-CII-DATE                             
               MOVE ZERO TO VDATE-RET-IND                                       
                            VDATE-RET-CODE                                      
               GO TO B999X.                                                     
           IF VDATE1-YYYYMMDD EQUAL '00000101'                                  
              AND VDATE-REQ-DETAIL EQUAL '1'                            May98IB 
               MOVE '01JAN0000' TO VDATE-EXT-DATE                               
               MOVE LOW-DATE TO VDATE-ALIS-DATE1                                
               MOVE CII-LOW-DATE TO VDATE-CII-DATE                              
               MOVE ZERO TO VDATE-RET-IND                                       
                            VDATE-RET-CODE                                      
               GO TO B999X.                                                     
                                                                                
           IF NOT VALID-FUNCTB-BASIS                                            
               ADD PACK-2 TO VDATE-RET-CODE                                     
               GO TO B999X.                                                     
           IF NOT VALID-FUNCTB-DETAIL                                           
               ADD PACK-3 TO VDATE-RET-CODE                                     
               GO TO B999X.                                                     
           IF NOT VALID-LANGUAGE                                                
               ADD PACK-4 TO VDATE-RET-CODE                                     
               GO TO B999X.                                                     
                                                                                
           PERFORM Y000-EDIT-YYYYMMDD THRU Y099X.                               
           IF VDATE-RET-CODE NOT = 120                                          
               GO TO B999X.                                                     
                                                                                
           IF VDATE-REQ-DETAIL EQUAL '1'                                        
               MOVE VDATE1-YYYY TO VDATE-EXT-YEAR                               
           ELSE                                                                 
               PERFORM Z800-ADJUST-WINDOW THRU Z899X                            
               MOVE VDATE1-YYYY TO VDATE-EXT-YEAR-SHORT.                        
                                                                                
           MOVE VDATE1-DD TO VDATE-EXT-DAY.                                     
           MOVE SPACES TO VDATE-EXT-MONTH.                                      
           IF ENGLISH                                                           
               MOVE TS-ENGLISH-MONTH (VDATE1-MM) TO VDATE-EXT-MONTH             
             ELSE                                                               
               MOVE TS-FRENCH-MONTH (VDATE1-MM) TO VDATE-EXT-MONTH.             
                                                                                
           PERFORM X000-YYYYMMDD-TO-ALIS THRU X099X.                            
           PERFORM X600-YYYYMMDD-TO-CII THRU X699X.                             
                                                                                
           MOVE ZERO                 TO VDATE-RET-IND.                          
           MOVE ZERO                 TO VDATE-RET-CODE.                         
                                                                                
       B999X.                                                                   
                                                                                
           EXIT.                                                                
      /                                                                         
                                                                                
       C000-CLIENTS-II.                                                         
                                                                                
           MOVE 130 TO VDATE-RET-CODE.                                          
           IF VDATE-CII-DATE NOT NUMERIC                                        
               ADD PACK-5 TO VDATE-RET-CODE                                     
               GO TO C999X.                                                     
                                                                                
           IF VDATE-CII-DATE EQUAL CII-LOW-DATE                                 
               MOVE '01JAN0000' TO VDATE-EXT-DATE                               
               MOVE '00000101' TO VDATE1-YYYYMMDD                               
               MOVE LOW-DATE TO VDATE-ALIS-DATE1                                
               MOVE ZERO TO VDATE-RET-CODE                                      
                            VDATE-RET-IND                                       
               GO TO C999X.                                                     
           IF VDATE-CII-DATE EQUAL CII-HIGH-DATE                                
               MOVE '31DEC9999' TO VDATE-EXT-DATE                               
               MOVE '99991231' TO VDATE1-YYYYMMDD                               
               MOVE HIGH-DATE TO VDATE-ALIS-DATE1                               
               MOVE ZERO TO VDATE-RET-CODE                                      
                            VDATE-RET-IND                                       
               GO TO C999X.                                                     
                                                                                
           PERFORM X700-CII-TO-YYYYMMDD      THRU X799X.                        
                                                                                
           PERFORM Y000-EDIT-YYYYMMDD        THRU Y099X.                        
           IF VDATE-RET-CODE NOT = 130                                          
               GO TO C999X.                                                     
                                                                                
           PERFORM X000-YYYYMMDD-TO-ALIS     THRU X099X.                        
           PERFORM X400-YYYYMMDD-TO-EXTERNAL THRU X499X.                        
                                                                                
           MOVE ZERO                 TO VDATE-RET-IND.                          
           MOVE ZERO                 TO VDATE-RET-CODE.                         
                                                                                
       C999X.                                                                   
                                                                                
           EXIT.                                                                
      /                                                                         
                                                                                
       D000-JULIAN.                                                             
                                                                                
           MOVE 140 TO VDATE-RET-CODE.                                          
           IF NOT VALID-FUNCTD-BASIS  THEN                               Nov99IB
               ADD PACK-2 TO VDATE-RET-CODE                              Nov99IB
               GO TO D999X.                                              Nov99IB
           IF NOT VALID-FUNCTD-DETAIL  THEN                              Nov99IB
               ADD PACK-3 TO VDATE-RET-CODE                              Nov99IB
               GO TO D999X.                                              Nov99IB
           IF VDATE-JULIAN-YY NOT NUMERIC                                       
               ADD PACK-6 TO VDATE-RET-CODE                                     
               GO TO D999X.                                                     
           IF VDATE-REQ-DETAIL = '1'                                            
               IF VDATE-JULIAN-CC NOT NUMERIC                                   
                   ADD PACK-6 TO VDATE-RET-CODE                                 
                   GO TO D999X.                                                 
           IF VDATE-JULIAN-DDD NOT NUMERIC                                      
               ADD PACK-8 TO VDATE-RET-CODE                                     
               GO TO D999X.                                                     
                                                                                
           IF VDATE-JULIAN-DATE EQUAL JULIAN-LOW-DATE                           
               MOVE '00000101' TO VDATE1-YYYYMMDD                               
               MOVE LOW-DATE TO VDATE-ALIS-DATE1                                
               MOVE CII-LOW-DATE TO VDATE-CII-DATE                              
               MOVE ZERO TO VDATE-RET-CODE                                      
                            VDATE-RET-IND                                       
               GO TO D999X.                                                     
           IF VDATE-JULIAN-DATE EQUAL JULIAN-HIGH-DATE                          
               MOVE '99991231' TO VDATE1-YYYYMMDD                               
               MOVE HIGH-DATE TO VDATE-ALIS-DATE1                               
               MOVE CII-HIGH-DATE TO VDATE-CII-DATE                             
               MOVE ZERO                 TO VDATE-RET-IND                       
               MOVE ZERO                 TO VDATE-RET-CODE                      
               GO TO D999X.                                                     
                                                                                
           IF VDATE-JULIAN-DDD < 1 OR VDATE-JULIAN-DDD > 366                    
               ADD PACK-8 TO VDATE-RET-CODE                                     
               GO TO D999X.                                                     
                                                                                
           PERFORM X300-JULIAN-TO-ALIS THRU X399X.                              
           PERFORM X100-ALIS-TO-YYYYMMDD     THRU X199X.                        
                                                                                
      *    THIS CODE WAS ADDED TO REJECT DAY 366 FOR                            
      *    NON LEAP YEARS                                                       
                                                                                
           IF VDATE-JULIAN-DDD = 366 AND LP-NOT-LEAPYEAR                        
               ADD PACK-8 TO VDATE-RET-CODE                                     
               GO TO D999X.                                                     
           IF VDATE-REQ-DETAIL > '1'                                            
               MOVE ZERO TO VDATE1-YYYY                                         
               ADD  VDATE-JULIAN-YY TO VDATE1-YYYY                              
               PERFORM Z800-ADJUST-WINDOW THRU Z899X.                           
           PERFORM X000-YYYYMMDD-TO-ALIS THRU X099X.                            
           PERFORM X600-YYYYMMDD-TO-CII THRU X699X.                             
           MOVE ZERO                 TO VDATE-RET-IND.                          
           MOVE ZERO                 TO VDATE-RET-CODE.                         
                                                                                
       D999X.                                                                   
                                                                                
           EXIT.                                                                
      /                                                                         
                                                                                
       E000-SYSTEM-DATE.                                                        
                                                                                
           MOVE REAL-SYSTEM-DATE TO VDATE1-YYYYMMDD.                            
           PERFORM B000-YYYYMMDD THRU B999X.                                    
                                                                                
       E999X.                                                                   
                                                                                
           EXIT.                                                                
      /                                                                         
       X000-YYYYMMDD-TO-ALIS.                                                   
                                                                                
           COMPUTE VDATE-ALIS-YEAR1 = VDATE1-YYYY - PACK-1800.                  
                                                                                
           IF VDATE-ALIS-YEAR1 LESS THAN ZERO  THEN                             
               ADD PACK-6 TO VDATE-RET-CODE                                     
               GO TO X099X.                                                     
                                                                                
           COMPUTE VDATE-ALIS-DAY1 = TS-ACCUM-DAYS (VDATE1-MM)                  
                                    + VDATE1-DD.                                
           IF LP-LEAPYEAR-FLAG = CHAR-2 AND LEAPYEAR-BASIS                      
               ADD 1 TO VDATE-ALIS-DAY1.                                        
                                                                                
       X099X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       X100-ALIS-TO-YYYYMMDD.                                                   
                                                                                
           COMPUTE VDATE1-YYYY = VDATE-ALIS-YEAR1 + PACK-1800.                  
                                                                                
           MOVE VDATE-ALIS-DAY1 TO TS-ALIS-DAY LP-ALIS-DAY.                     
           MOVE VDATE-ALIS-YEAR1 TO LP-ALIS-YEAR.                               
           PERFORM Z400-LEAPYEAR-RTN THRU Z499X.                                
           IF LP-LEAPYEAR                                                       
               IF VDATE-REQ-BASIS = 'A' OR 'D' OR 'E'                           
                   IF TS-ALIS-DAY > 59                                          
                       SUBTRACT 1 FROM TS-ALIS-DAY                              
                       IF VDATE-REQ-BASIS = 'D' AND TS-ALIS-DAY = 59            
                           MOVE 60 TO TS-ALIS-DAY.                              
                                                                                
           PERFORM Z200-ACCUM-DAYS-TABSRCH                                      
                                 THRU Z200-ACCUM-DAYS-TABSRCH-EXIT.             
           IF TS-ENTRY-NOT-FOUND                                                
               ADD PACK-8 TO VDATE-RET-CODE                                     
               GO TO X199X.                                                     
                                                                                
           COMPUTE VDATE1-DD = TS-ALIS-DAY                                      
                                  - TS-ACCUM-DAYS (TS-TAB-ENTRY)                
           IF VDATE-REQ-BASIS = 'A'                                             
               IF VDATE-ALIS-DAY1 = 60 AND LP-LEAPYEAR                          
                   MOVE 29 TO VDATE1-DD.                                        
                                                                                
           MOVE TS-TAB-ENTRY TO VDATE1-MM.                                      
                                                                                
       X199X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       X200-YYYYMMDD-TO-DDMMYY.                                                 
                                                                                
           MOVE ZERO        TO VDATE-YY.                                        
           MOVE VDATE1-YYYY TO VDATE-YY.                                        
           MOVE VDATE1-MM   TO VDATE-MM.                                        
           MOVE VDATE1-DD   TO VDATE-DD.                                        
                                                                                
       X299X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       X300-JULIAN-TO-ALIS.                                                     
                                                                                
                                                                                
           COMPUTE VDATE-ALIS-YEAR1 = VDATE-JULIAN-YY.                          
      *      YEAH, YEAH, I KNOW!                                                
      *    FOR 2 DIGIT YEARS, THE ALIS DATE IS WRONG AT THIS POINT.             
      *    BUT IT'S JUST A GUESS. THE DATE WILL BE WINDOWED AND                 
      *    THE ALIS YEAR CORRECTED IN SUCCEEDING ROUTINES.                      
                                                                                
      *****************************************************************  Oct99IB
      *    IF VDATE-REQ-DETAIL > '1' AND VDATE-ALIS-YEAR1 = ZERO         Oct99IB
      *       MOVE 200 TO VDATE-ALIS-YEAR1.                              Oct99IB
      *    See original comments above re: 2-digit years.  Day values    Oct99IB
      *    were being calcualted 1 day off for '00' (year 2000), since   Oct99IB
      *    the 'guess' came out to 1800, which was NOT a leap year.      Oct99IB
      *****************************************************************  Nov99IB
      *    NOTE: following the discovery of further problems with        Nov99IB
      *          2-digit Julian calls, that option has been shut off     Nov99IB
      *          at D000-JULIAN.                                         Nov99IB
      *****************************************************************  Nov99IB
                                                                                
           IF VDATE-REQ-DETAIL = '1'                                            
               COMPUTE VDATE-ALIS-YEAR1 =                                       
                  VDATE-ALIS-YEAR1 + (VDATE-JULIAN-CC - 18) * 100.              
           COMPUTE VDATE-ALIS-DAY1 = VDATE-JULIAN-DDD.                          
                                                                                
       X399X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       X400-YYYYMMDD-TO-EXTERNAL.                                               
                                                                                
           MOVE ZERO TO VDATE-EXT-YEAR.                                         
           IF WORK-REQ-DETAIL > '1'                                             
               MOVE VDATE1-YYYY  TO VDATE-EXT-YEAR-SHORT                        
             ELSE                                                               
               MOVE VDATE1-YYYY TO VDATE-EXT-YEAR.                              
           MOVE VDATE1-DD TO VDATE-EXT-DAY.                                     
           IF ENGLISH                                                           
               MOVE TS-ENGLISH-MONTH (VDATE1-MM) TO VDATE-EXT-MONTH             
             ELSE                                                               
                                                                                
               MOVE TS-FRENCH-MONTH  (VDATE1-MM) TO VDATE-EXT-MONTH.            
                                                                                
       X499X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       X500-EXTERNAL-TO-YYYYMMDD.                                               
                                                                                
           MOVE ZERO TO VDATE1-YYYY.                                            
           IF WORK-REQ-DETAIL > '1'                                             
               MOVE VDATE-EXT-YEAR-SHORT TO VDATE1-YYYY                         
             ELSE                                                               
               MOVE VDATE-EXT-YEAR      TO VDATE1-YYYY.                         
           MOVE VDATE-EXT-DAY TO VDATE1-DD.                                     
           MOVE VDATE-EXT-MONTH TO TS-EXT-MONTH.                                
           PERFORM Z000-MONTH-TABSRCH THRU Z000-MONTH-TABSRCH-EXIT.             
           MOVE TS-TAB-ENTRY TO VDATE1-MM.                                      
           IF VDATE1-MM = 2 AND VDATE1-DD = 29                                  
               IF VDATE-REQ-BASIS = 'D'                                         
                   MOVE 03 TO VDATE1-MM                                         
                   MOVE 01 TO VDATE1-DD                                         
                 ELSE                                                           
                   IF VDATE-REQ-BASIS = 'E'                                     
                       MOVE 28 TO VDATE1-DD.                                    
                                                                                
       X599X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       X600-YYYYMMDD-TO-CII.                                                    
                                                                                
           MOVE VDATE1-YYYYMMDD TO CHECK-DATE.                                  
           SUBTRACT 1700 FROM CHD-YEAR.                                         
           COMPUTE VDATE-CII-DATE = CHD.                                        
                                                                                
       X699X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       X700-CII-TO-YYYYMMDD.                                                    
                                                                                
           COMPUTE CHD =  VDATE-CII-DATE.                                       
           ADD 1700 TO CHD-YEAR.                                                
           MOVE CHECK-DATE TO VDATE1-YYYYMMDD.                                  
                                                                                
       X799X.                                                                   
                                                                                
           EXIT.                                                                
      /                                                                         
       Y000-EDIT-YYYYMMDD.                                                      
                                                                                
      *        ALIS DATES SUPPORT JANUARY 1 1800 UPWARDS ONLY.                  
                                                                                
                                                                                
           IF VDATE1-YYYY NOT NUMERIC                                           
               ADD PACK-6 TO VDATE-RET-CODE                                     
               GO TO Y099X.                                                     
           IF VDATE1-YYYY < 1800 AND VDATE-REQ-DETAIL = '1'                     
               ADD PACK-6 TO VDATE-RET-CODE                                     
               GO TO Y099X.                                                     
                                                                                
           IF VDATE1-MM NOT NUMERIC OR                                          
              VDATE1-MM < 1 OR  VDATE1-MM > 12                                  
               ADD PACK-7 TO VDATE-RET-CODE                                     
               GO TO Y099X.                                                     
                                                                                
           IF VDATE1-DD NOT NUMERIC                                             
               ADD PACK-8 TO VDATE-RET-CODE                                     
               GO TO Y099X.                                                     
                                                                                
           IF VDATE1-DD = ZERO                                          Oct99IB 
               ADD PACK-8 TO VDATE-RET-CODE                             Oct99IB 
               GO TO Y099X.                                             Oct99IB 
                                                                        Oct99IB 
           PERFORM Y100-YYYYMMDD-LEAP-CHECK THRU Y199X.                         
                                                                                
           IF VDATE1-DD > 1 AND VDATE1-DD < 29                                  
               GO TO Y099X.                                                     
                                                                                
           IF VDATE-REQ-BASIS = 'C'                                             
               IF VDATE1-DD > 28                                                
                   ADD PACK-2 TO VDATE-RET-CODE                                 
                   GO TO Y099X.                                                 
                                                                                
           IF VDATE1-DD NOT > TS-MAX-DAY (VDATE1-MM)                            
               GO TO Y099X.                                                     
                                                                                
      *       FEBRUARY 29TH IS THE LAST THING TO CHECK                          
                                                                                
           IF LP-LEAPYEAR-FLAG = 0 OR VDATE-REQ-BASIS = 'B'                     
               ADD PACK-8 TO VDATE-RET-CODE                                     
               GO TO Y099X.                                                     
           IF LP-LEAPYEAR-FEB29                                                 
               IF VDATE-REQ-BASIS = 'D'                                         
                   MOVE 3 TO VDATE1-MM                                          
                   MOVE 1 TO VDATE1-DD                                          
                   GO TO Y099X                                          Oct99IB 
                 ELSE                                                           
                   IF VDATE-REQ-BASIS = 'E'                                     
                       MOVE 28 TO VDATE1-DD                                     
                       GO TO Y099X                                      Oct99IB 
                   ELSE                                                 Oct99IB 
                       GO TO Y099X.                                     Oct99IB 
                                                                                
      *    Fall thru at this point means day exceeded max for that      Oct99IB 
      *    month but was not legitimized by being Feb 29 of a leap.     Oct99IB 
           ADD PACK-8 TO VDATE-RET-CODE.                                Oct99IB 
                                                                                
       Y099X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       Y100-YYYYMMDD-LEAP-CHECK.                                                
                                                                                
           MOVE PACK-365 TO LP-MAX-DAYS.                                        
           MOVE ZERO TO LP-LEAPYEAR-FLAG.                                       
                                                                                
      ******************************************************************May98IB 
      * Rules Hierarchy:                                                May98IB 
      * ----------------                                                May98IB 
      *  If it's divisible by 4 it IS a leap year,... unless it's ALSO  May98IB 
      *  divisible by 100, in which case it's NOT a leap year, UNLESS   May98IB 
      *  it's ALSO divisible by 400, in which case it stays a leap year.May98IB 
      *  Thus 1900 was not a leap year, and neither will 2100 be, but   May98IB 
      *  2000 IS a leap year, demonstrating EACH of the above rules.    May98IB 
      *  (There are further exceptions starting at the year 4000, but   May98IB 
      *  if we had to suffer then so do you, Flash Gordon.)             May98IB 
      ******************************************************************May98IB 
           DIVIDE VDATE1-YYYY BY PACK-400 GIVING WORK-TEMP-RESULT               
                       REMAINDER WORK-REMAINDER.                                
           IF WORK-REMAINDER = 0                                                
               GO TO Y100-LEAP-YEAR-YES.                                May98IB 
                                                                                
           MOVE VDATE1-YYYY TO WORK-FULL-YEAR.                                  
           DIVIDE VDATE1-YYYY BY PACK-4 GIVING WORK-TEMP-RESULT                 
                       REMAINDER WORK-REMAINDER.                                
           IF WORK-TEMP-YEAR = '00' OR  WORK-REMAINDER NOT = ZERO               
               GO TO Y199X.                                                     
                                                                                
       Y100-LEAP-YEAR-YES.                                              May98IB 
           MOVE 1 TO WORK-REMAINDER, LP-LEAPYEAR-FLAG.                          
           MOVE PACK-366 TO LP-MAX-DAYS.                                        
           IF VDATE1-MM > 2                                                     
               MOVE CHAR-2 TO LP-LEAPYEAR-FLAG                                  
             ELSE                                                               
               IF VDATE1-MM = 2 AND VDATE1-DD = 29                              
                   MOVE CHAR-3 TO LP-LEAPYEAR-FLAG.                             
                                                                                
       Y199X.                                                                   
                                                                                
           EXIT.                                                                
                                                                                
       Y200-EDIT-EXTERNAL.                                                      
                                                                                
      *    CHECK FOR NUMERIC DATA. REMAINING EDITS WILL BE AFTER                
      *    CONVERSION TO YYYYMMDD FORMAT.                                       
                                                                                
           IF VDATE-EXT-DAY NOT NUMERIC                                         
               ADD PACK-8 TO VDATE-RET-CODE                                     
               GO TO Y299X.                                                     
           IF VDATE-REQ-DETAIL > '1'                                            
               IF VDATE-EXT-YEAR-SHORT NOT NUMERIC                              
                   ADD PACK-6 TO VDATE-RET-CODE                                 
                   GO TO Y299X.                                                 
           IF VDATE-REQ-DETAIL = '1'                                            
               IF VDATE-EXT-YEAR NOT NUMERIC                                    
                   ADD PACK-6 TO VDATE-RET-CODE                                 
                   GO TO Y299X.                                                 
                                                                                
       Y299X.                                                                   
      /                                                                         
       Z000-MONTH-TABSRCH.                                                      
      ******************************************************************        
      *    THIS ROUTINE AND TS-MONTH-RTN ARE USED TO FIND THE          *        
      *    APPROPRIATE ENTRY IN THE MONTH TABLE ACCORDING TO THE ALPHA *        
      *    MONTH PASSED TO IT.                                         *        
      ******************************************************************        
                                                                                
           MOVE ZERO TO TS-RETURN-CODE.                                         
           MOVE PACK-1 TO TS-TAB-ENTRY.                                         
           PERFORM Z010-TS-MONTH-RTN THRU Z010-TS-MONTH-RTN-EXIT                
               UNTIL NOT TS-SEARCH-IN-PROGRESS.                                 
       Z000-MONTH-TABSRCH-EXIT.                                                 
           EXIT.                                                                
                                                                                
                                                                                
       Z010-TS-MONTH-RTN.                                                       
           IF TS-EXT-MONTH EQUAL TS-ENGLISH-MONTH (TS-TAB-ENTRY) OR             
              TS-EXT-MONTH EQUAL TS-FRENCH-MONTH (TS-TAB-ENTRY) THEN            
                MOVE CHAR-1 TO TS-RETURN-CODE                                   
           ELSE                                                                 
                ADD PACK-1  TO TS-TAB-ENTRY.                                    
           IF TS-TAB-ENTRY GREATER THAN PACK-12  THEN                           
               MOVE CHAR-2 TO TS-RETURN-CODE.                                   
       Z010-TS-MONTH-RTN-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
       Z200-ACCUM-DAYS-TABSRCH.                                                 
      ******************************************************************        
      *   THIS ROUTINE RETURNS THE APPROPRIATE MONTH TABLE ENTRY A     *        
      *   ACCORDING TO THE ALIS DAY VALUE PASSED TO IT. IT USES THE    *        
      *   NEXT ROUTINE (TS-ACCUM-DAYS-RTN) TO DO THIS                  *        
      ******************************************************************        
                                                                                
           MOVE ZERO TO TS-RETURN-CODE.                                         
           MOVE PACK-12 TO TS-TAB-ENTRY.                                        
           PERFORM Z210-TS-ACCUM-DAYS-RTN THRU                                  
                   Z210-TS-ACCUM-DAYS-RTN-EXIT                                  
               UNTIL NOT TS-SEARCH-IN-PROGRESS.                                 
       Z200-ACCUM-DAYS-TABSRCH-EXIT.                                            
           EXIT.                                                                
                                                                                
       Z210-TS-ACCUM-DAYS-RTN.                                                  
           IF TS-ALIS-DAY GREATER THAN TS-ACCUM-DAYS (TS-TAB-ENTRY)             
               MOVE CHAR-1 TO TS-RETURN-CODE                                    
           ELSE                                                                 
               SUBTRACT PACK-1 FROM TS-TAB-ENTRY.                               
           IF TS-TAB-ENTRY EQUAL ZERO  THEN                                     
               MOVE PACK-1 TO TS-TAB-ENTRY                                      
               MOVE CHAR-2 TO TS-RETURN-CODE.                                   
       Z210-TS-ACCUM-DAYS-RTN-EXIT.                                             
           EXIT.                                                                
      /                                                                         
       Z400-LEAPYEAR-RTN.                                                       
      ******************************************************************        
      *   THIS ROUTINE DETERMINES IF THE DATE PASSED TO IT IS A        *        
      *   LEAPYEAR. IF IT IS, THE ROUTINE ALSO DECIDES WHETHER IT IS   *        
      *   FEB. 29TH, OR AFTER.                                         *        
      ******************************************************************        
      * Rules Hierarchy:                                                May98IB 
      * ----------------                                                May98IB 
      *  If it's divisible by 4 it IS a leap year,...  unless it's ALSO May98IB 
      *  divisible by 100, in which case it's NOT a leap year, UNLESS   May98IB 
      *  it's ALSO divisible by 400, in which case it stays a leap year.May98IB 
      *  Thus 1900 was not a leap year, and neither will 2100 be, but   May98IB 
      *  2000 IS a leap year, demonstrating EACH of the above rules.    May98IB 
      *  (There are further exceptions starting at the year 4000, but   May98IB 
      *  if we had to suffer then so do you, Flash Gordon.)             May98IB 
      ******************************************************************May98IB 
                                                                                
           MOVE ZERO TO LP-LEAPYEAR-FLAG.                                       
           MOVE PACK-365 TO LP-MAX-DAYS.                                        
           IF NOT (LEAPYEAR-BASIS OR                                            
                   LEAPYEAR-FORWARD-BASIS OR                                    
                   LEAPYEAR-BACKWARD-BASIS)      THEN                           
               GO TO Z499X.                                                     
           COMPUTE LP-WORK-YEAR = LP-ALIS-YEAR + PACK-1800.                     
           DIVIDE  LP-WORK-YEAR BY PACK-400 GIVING LP-TEMP-RESULT               
                                         REMAINDER LP-REMAINDER.                
           IF LP-REMAINDER NOT EQUAL ZERO  THEN                                 
               DIVIDE LP-WORK-YEAR BY PACK-100 GIVING LP-TEMP-RESULT            
                                            REMAINDER LP-REMAINDER              
               IF LP-REMAINDER EQUAL ZERO                                       
                   GO TO Z499X.                                                 
           DIVIDE LP-WORK-YEAR BY PACK-4 GIVING LP-TEMP-RESULT                  
                                      REMAINDER LP-REMAINDER.                   
           IF LP-REMAINDER NOT EQUAL ZERO  THEN                                 
               GO TO Z499X.                                                     
                                                                                
           MOVE CHAR-1 TO LP-LEAPYEAR-FLAG.                                     
           MOVE PACK-366 TO LP-MAX-DAYS.                                        
           IF LP-ALIS-DAY GREATER THAN TS-ACCUM-DAYS (BIN-3) + PACK-1           
               MOVE CHAR-2 TO LP-LEAPYEAR-FLAG.                                 
           IF LP-ALIS-DAY EQUAL TS-ACCUM-DAYS (BIN-3) + PACK-1                  
               MOVE CHAR-3 TO LP-LEAPYEAR-FLAG.                                 
                                                                                
       Z499X.                                                                   
                                                                                
           EXIT.                                                                
      /                                                                         
       Z800-ADJUST-WINDOW.                                                      
                                                                                
           MOVE ZERO TO WRK-YEAR                                                
                        WRK-MTH                                                 
                        WRK-DAY.                                                
           IF VDATE-REQ-DETAIL = '1'                                            
               GO TO Z899X.                                                     
           IF VDATE-REQ-DETAIL = '2'                                            
               MOVE DW-YEARS  TO WRK-YEAR                                       
               MOVE DW-MONTHS TO WRK-MTH                                        
               MOVE DW-DAYS   TO WRK-DAY                                        
             ELSE                                                               
               IF VDATE-REQ-DETAIL = '3'                                        
                   MOVE VDATE-ADJUST-DAYS    TO WRK-DAY                         
                   MOVE VDATE-ADJUST-YEARS   TO WRK-YEAR                        
                   MOVE VDATE-ADJUST-MONTHS  TO WRK-MTH.                        
           ADD RSD-YEAR  TO WRK-YEAR.                                           
           ADD RSD-MONTH TO WRK-MTH.                                            
           ADD RSD-DAY   TO WRK-DAY.                                            
                                                                                
       Z810-MAX-DATE.                                                           
                                                                                
           IF WRK-MTH > 12                                                      
               ADD 1 TO WRK-YEAR                                                
               SUBTRACT 12 FROM WRK-MTH                                         
               GO TO Z810-MAX-DATE                                              
             ELSE                                                               
               IF WRK-MTH < 1                                                   
                   SUBTRACT 1 FROM WRK-YEAR                                     
                   ADD 12 TO WRK-MTH                                            
                   GO TO Z810-MAX-DATE.                                         
           IF WRK-DAY > TS-MAX-DAY (WRK-MTH)                                    
               SUBTRACT TS-MAX-DAY (WRK-MTH) FROM WRK-DAY                       
               ADD 1 TO WRK-MTH                                                 
               GO TO Z810-MAX-DATE                                              
             ELSE                                                               
               IF WRK-DAY < 1                                                   
                   SUBTRACT 1 FROM WRK-MTH                                      
                   ADD TS-MAX-DAY (WRK-MTH) TO WRK-DAY                          
                   GO TO Z810-MAX-DATE.                                         
                                                                                
       Z820-CALC-YEAR.                                                          
                                                                                
           SUBTRACT VDATE1-YYYY FROM WRK-YEAR                                   
                               GIVING VDATE-EXT-YEAR-SHORT.                     
           SUBTRACT VDATE-EXT-YEAR-SHORT FROM WRK-YEAR                          
                               GIVING VDATE1-YYYY.                              
           COMPUTE VDATE-EXT-YEAR-SHORT = VDATE1-YYYY.                          
                                                                                
       Z830-FINAL-CHECK.                                                        
                                                                                
           MOVE WRK-YEAR TO CHD-YEAR.                                           
           MOVE WRK-MTH  TO CHD-MTH.                                            
           MOVE WRK-DAY  TO CHD-DAY.                                            
           IF VDATE1-YYYYMMDD > CHECK-DATE                                      
               SUBTRACT 100 FROM VDATE1-YYYY.                                   
                                                                                
       Z899X.                                                                   
                                                                                
           EXIT.                                                                
