       CBL FLAG(I)                                                              
      *                                                                         
      * THE ABOVE COBOL COMPILER DIRECTIVE IS REQUIRED BECAUSE                  
      * THE DATA SERVER MODULE GAEDATSR IS CALLED BY THIS ROUTINE.              
      *                                                                         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GCCPRGNF.                                                 
      *AUTHOR.        FRANK MARUCCI.                                            
      ******************************************************************        
      *         << GROUP BENEFITS INTERNET REGISTRATION >>                      
      *         << CONFIRMATION LETTER AFP FORMATTER >>                         
      *                                                                         
      * PROGRAM DESCRIPTION:                                                    
      *   THIS PROGRAM READS A FLAT FILE CREATED BY GCCPRGNA THAT               
      *   CONTAINS THE REGISTRATION LETTER REQUESTS WHICH INCLUDES              
      *   APPROPRIATE EMPLOYEE ADDRESS.  THESE RECORDS CONTAIN A                
      *   MAILING CODE THAT HAS BEEN SET ACCORDINGLY.                           
      *                                                                         
      *   C - SEND BY COURIER   TO PLAN ADMINISTRATOR                           
      *   B - SEND BY BULK MAIL TO PLAN ADMINISTRATOR                           
      *   D - SEND TO INDIVIDUAL EMPLOYEES BY DIRECT MAIL                       
      *                                                                         
      *   A BANNER PAGE WILL BE PRINTED FOR EACH MAILING CATEGORY               
      *   OF REGISTRATION LETTER - WHICH WILL INDICATE HOW THE                  
      *   ATTACHED LETTERS WILL BE HANDLED.                                     
      *                                                                         
      *   REGISTRATION LETTER AFP TYPE CODES:  ENGLISH    FRENCH                
      *                                                                         
      *   REGISTRATION LETTER                     1         2                   
      *   ALL BANNER PAGES                        3                             
      *                                                                         
      * CALLING MODULES                                                         
      *     THIS IS THE MAINLINE PROGRAM                                        
      *                                                                         
      * CALLED MODULES                                                          
      *     GAEDATSR - DATA SERVER                                              
      *                                                                         
      * COPYBOOKS                                                               
      *     GCCCCEXT - REGISTRATION LETTER REQUESTS (WITH ADDRESSES)            
      *     GCCCREGL - REGISTRATION LETTER AFP FILE FORMAT                      
      *     GCCCREGB - REGISTRATION LETTER AFP FILE FORMAT (BACKPAGE)           
      *                                                                         
      * INPUT  - FLAT FILE OF REGISTRATION LETTER REQUESTS                      
      *                                                                         
      * OUTPUT - FLAT FILE OF REGISTRATION AFP FORMAT REQUESTS                  
      *                                                                         
      ******************************************************************        
      * DATE       NAME      DESCRIPTION                                        
      * ---------  --------  -------------------------------------------        
      * 13OCT2001  F.MARUCCI CREATION.                                          
      *                                                                         
      * 09SEP2002  J.FARROW  CHANGED THE COST CENTER FROM 567 TO 213            
      *                                                                         
      * 05NOV2002  KLYN      ADDED ACTIVATION KEY TO INPUT/OUTPUT               
      *                                                                         
      * 09DEC2004  WODNIJA   ADDED GROUP AND CERT TO NEW BACKPAGE,              
      *                      GBSS TASK 28002. COPYBOOK GCCCREGB.                
      *                                                                         
WB    * 24FEB2004  SPAULHO   ADDED CSC PHONE AND URL TO LETTER AS DATA          
      *                                                                         
WB2   * 01JUN2005  BASHAWE   ADDED HP TYPE FOR HEALTHPRO MEMBERS                
      *                                                                         
WB3   * 21DEC2005  BASHAWE   ADDED HP-VO TYPE                                   
      *                                                                         
      * 30MAY2006  COOPEMI   ADDED SUPPORT FOR NEW BUNDLES AND NEW              
      *                      LETTER COUNT SHEET AT END OF EACH BUNDLE           
      *                                                                         
      * 12SEP2006  PRANGMA   TEMPLATE ID IS NOW 3 DIGITS.                       
      *                                                                         
      * 26AUG2008  IBM GR    UPGRADED IN ECU PROJECT                            
      *                                                                         
      * 23JUN2009  PAIKPRA   ADD NEW MKI BANNER PAGE HANDLING                   
      *                      (REF TL 108280 & TL 120562)                        
      *                      BANNER PAGE CHANGED FROM 213 TO 413                
      *                      (REF TL 119699)                                    
      * 24MAR2010  DINGAND   BANNER PAGE FOR OTIP ACTIVATION LETTERS            
      *                      (REF TL134005)                                     
      *   NOV2009  LEEKRAN   ADD NEW OTIP BANNER PAGE HANDLING                  
      *                                                                         
      *   MAR2010  P PAIK    MKI LETTER ENHANCEMENT                             
      *                                                                         
      * 09SEP2010  HASSELFELT TL 139845 ADD MAIL INSTRUCTION OF I               
      *                       THIS WILL PRODUCE MESSAGE SEND TO: PIA            
      *                                                                         
      ******************************************************************        
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-370-165.                                            
       OBJECT-COMPUTER. IBM-370-165.                                            
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
                                                                                
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-CONSTANTS.                                                        
           05  FILLER                      PIC X(32) VALUE                      
               '*** GCCPRGNF WORKING STORAGE ***'.                              
                                                                                
           05  WS-GAEDATSR-VERB            PIC X(16).                           
           05  GAEDATSR                    PIC X(8)  VALUE 'GAEDATSR'.          
           05  WS-GC2DATE                  PIC X(07) VALUE 'GC2DATE'.           
                                                                                
           05  WS-NO-INP-RECORDS           PIC 9(4)  VALUE ZERO.                
           05  WS-NO-OUTPUT-RECS           PIC 9(4)  VALUE ZERO.                
                                                                                
           05  WS-OBTAIN-FIRST    PIC X(16)   VALUE 'OBTAIN  FIRST   '.         
           05  WS-OBTAIN-NEXT     PIC X(16)   VALUE 'OBTAIN  NEXT    '.         
                                                                                
           05  WS-INPUT-LR        PIC X(16)   VALUE 'CARD-DATA-010   '.         
           05  WS-PRINT-LR        PIC X(16)   VALUE 'PRINT-DATA-020  '.         
                                                                                
                                                                                
       01  WS-CURRENT-DATE.                                                     
           05 WS-CURR-DAY         PIC 9(2).                                     
           05 WS-CURR-MTH         PIC X(3).                                     
           05 WS-CURR-YEAR        PIC 9(4).                                     
                                                                                
       01  WS-REGN-DATE.                                                        
           05 WS-REGN-DAY         PIC 9(2).                                     
           05 WS-REGN-MTH         PIC X(3).                                     
           05 WS-REGN-YEAR        PIC 9(4).                                     
                                                                                
       01  WS-DISPLAY-DATE.                                                     
           05 WS-DISPLAY-DAY      PIC 9(2).                                     
           05 WS-DISPLAY-SP1      PIC X     VALUE SPACES.                       
           05 WS-DISPLAY-MTH      PIC X(3).                                     
           05 WS-DISPLAY-SP2      PIC X     VALUE SPACES.                       
           05 WS-DISPLAY-YEAR     PIC 9(4).                                     
                                                                                
       01  WS-F-CURRENT-DATE      PIC X(11).                                    
       01  WS-E-CURRENT-DATE      PIC X(11).                                    
                                                                                
       01  WS-PREV-MAIL-INSTR    PIC X       VALUE SPACES.                      
                                                                                
       01  WS-LETTER-COUNT       PIC 9(5)    VALUE ZEROS.                       
       01  WS-LETTER-OUT-REC.                                                   
           05  WS-LETTER-OUT-HDR  PIC X(3)   VALUE '** '.                       
           05  WS-LETTER-OUT-CNT  PIC X(5)   VALUE '   '.                       
           05  WS-LETTER-OUT-EMP  PIC X(40)  VALUE SPACES.                      
           05  WS-LETTER-OUT-TRL  PIC X(3)   VALUE ' **'.                       
                                                                                
WB     01  WS-SPACE-URL-VARIABLES.                                              
WB         05  WS-URL-CNT               PIC 9(3).                               
WB         05  WS-SPACE-CNT             PIC 9(3).                               
WB         05  WS-SPACES-LEFT           PIC 9(2).                               
WB                                                                              
WB         05 WS-URL.                                                           
WB             10  WS-URL-CHAR-READ      PIC X(1)  OCCURS 100 TIMES.            
                                                                                
                                                                                
       01  GAEDATSR-PARMS.              COPY GARDSVRB.                          
                                                                                
       01  GAC-DATE-PARAMETERS.         COPY GARDATEP.                          
                                                                                
       01  LOGICAL-RECORD-NAMES.        COPY HCSLRNAM.                          
                                                                                
       01  INP-RECORD.                                                          
             COPY GCCCCEXT.                                                     
                                                                                
       01  LTR-OUT-RECORD.                                                      
             COPY GCCCREGL.                                                     
                                                                                
WB3   *01  LTR-OUT-RECORD-BP.                                                   
WB3   *      COPY GCCCREGB.                                                     
                                                                                
       01  WS-OUT-AFP-RECORD.                                                   
           02 FILLER              PIC X(2000).                                  
                                                                                
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
                                                                                
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INITIALIZATION  THRU 1000-EXIT.                         
                                                                                
      *   GENERATE REGISTRATION LETTERS/BANNERS UNTIL                           
      *   END-OF-FILE IS ENCOUNTERED FOR THE INPUT FILE:                        
                                                                                
           PERFORM 2000-GEN-LTR                                                 
           THRU    2000-GEN-LTR-EXIT                                            
                   UNTIL NOT LR-STATUS-OK.                                      
                                                                                
      *   PRINT TRAILING RECORD FOR LAST BUNDLE                                 
                                                                                
           PERFORM 2400-PRINT-BANNER-PAGE THRU                                  
                   2400-PRINT-BANNER-PAGE-EXIT.                                 
                                                                                
           PERFORM 9000-FINISH          THRU 9000-EXIT.                         
                                                                                
       0000-EXIT.                                                               
           GOBACK.                                                              
                                                                                
       1000-INITIALIZATION.                                                     
                                                                                
           MOVE 'GCCPRGNF'           TO ICBM-PROGRAM-NAME.                      
           MOVE LOW-VALUES           TO LINKAGE-CONTROL.                        
           MOVE ZEROS                TO WS-LETTER-COUNT.                        
                                                                                
      ******************************************************************        
      * GET CURRENT DATE FROM DATE ROUTINE                                      
      ******************************************************************        
                                                                                
           PERFORM 2300-TODAYS-DATE THRU                                        
                   2300-TODAYS-DATE-EXIT.                                       
                                                                                
      ******************************************************************        
      * READ FIRST INPUT RECORD                                                 
      ******************************************************************        
                                                                                
           MOVE WS-OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                       
           PERFORM 3000-READ-INPUT THRU                                         
                   3000-READ-INPUT-EXIT.                                        
                                                                                
       1000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       2000-GEN-LTR.                                                            
      ******************************************************************        
      * GENERATE REGISTRATION LETTERS AND BANNER PAGES                          
      ******************************************************************        
                                                                                
           IF NOT LR-STATUS-OK                                                  
               GO TO 2000-GEN-LTR-EXIT.                                         
                                                                                
           INITIALIZE LTR-OUT-RECORD.                                           
WB3   *               LTR-OUT-RECORD-BP.                                        
                                                                                
           IF WS-PREV-MAIL-INSTR NOT =                                          
              GCCCCEXT-MAIL-INSTRUCTION                                         
               PERFORM 2400-PRINT-BANNER-PAGE THRU                              
                       2400-PRINT-BANNER-PAGE-EXIT.                             
      *----------------------------------------------------------               
      *  CHECK CUSTOMER REPLY LANGUAGE, AND SET LETTER TYPE                     
      *----------------------------------------------------------               
                                                                                
           ADD 1 TO WS-LETTER-COUNT.                                            
                                                                                
           PERFORM 2500-BUILD-LTR THRU                                          
                   2500-BUILD-LTR-EXIT.                                         
                                                                                
           PERFORM 2200-GET-NEXT-INP-RECORD THRU                                
                   2200-GET-NEXT-INP-RECORD-EXIT.                               
                                                                                
       2000-GEN-LTR-EXIT.                                                       
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
       2200-GET-NEXT-INP-RECORD.                                                
                                                                                
           MOVE WS-OBTAIN-NEXT       TO WS-GAEDATSR-VERB.                       
           PERFORM 3000-READ-INPUT THRU                                         
                   3000-READ-INPUT-EXIT.                                        
                                                                                
       2200-GET-NEXT-INP-RECORD-EXIT.                                           
           EXIT.                                                                
                                                                                
                                                                                
       2300-TODAYS-DATE.                                                        
      ******************************************************************        
      *    USE GACDATE TO GET CURRENT DATE FOR LETTER (ENG AND FR)     *        
      ******************************************************************        
                                                                                
           MOVE 'E'             TO    VDATE-REQ-SERVICE.                        
           MOVE 'B'             TO    VDATE-REQ-BASIS.                          
           MOVE '1'             TO    VDATE-REQ-DETAIL.                         
           MOVE 'E'             TO    VDATE-REQ-LANGUAGE.                       
                                                                                
           CALL WS-GC2DATE      USING GAC-DATE-PARAMETERS.                      
                                                                                
           MOVE VDATE-EXT-DATE  TO    WS-CURRENT-DATE.                          
           MOVE WS-CURR-DAY     TO    WS-DISPLAY-DAY.                           
           MOVE WS-CURR-MTH     TO    WS-DISPLAY-MTH.                           
           MOVE WS-CURR-YEAR    TO    WS-DISPLAY-YEAR.                          
           MOVE WS-DISPLAY-DATE TO    WS-E-CURRENT-DATE.                        
                                                                                
           MOVE 'E'             TO    VDATE-REQ-SERVICE.                        
           MOVE 'B'             TO    VDATE-REQ-BASIS.                          
           MOVE '1'             TO    VDATE-REQ-DETAIL.                         
           MOVE 'F'             TO    VDATE-REQ-LANGUAGE.                       
                                                                                
           CALL WS-GC2DATE      USING GAC-DATE-PARAMETERS.                      
                                                                                
           MOVE VDATE-EXT-DATE  TO    WS-CURRENT-DATE.                          
           MOVE WS-CURR-DAY     TO    WS-DISPLAY-DAY.                           
           MOVE WS-CURR-MTH     TO    WS-DISPLAY-MTH.                           
           MOVE WS-CURR-YEAR    TO    WS-DISPLAY-YEAR.                          
           MOVE WS-DISPLAY-DATE TO    WS-F-CURRENT-DATE.                        
                                                                                
       2300-TODAYS-DATE-EXIT.                                                   
           EXIT.                                                                
                                                                                
       2400-PRINT-BANNER-PAGE.                                                  
                                                                                
      * PRINT TRAILING BANNER IF THIS IS NOT THE FIRST SECTION.                 
                                                                                
           IF WS-LETTER-COUNT > 0                                               
                                                                                
              MOVE SPACES TO LTR-OUT-RECORD                                     
WB3   *                   LTR-OUT-RECORD-BP.                                    
                                                                                
              MOVE WS-LETTER-COUNT                                              
                TO WS-LETTER-OUT-CNT                                            
                                                                                
              MOVE '** LETTER COUNT FOR SECTION                      **'        
                TO GCCCREGL-OUT-BANNER-TEXT(1)                                  
              MOVE WS-LETTER-OUT-REC                                            
                TO GCCCREGL-OUT-BANNER-TEXT(2)                                  
              MOVE '**                                               **'        
                TO GCCCREGL-OUT-BANNER-TEXT(3)                                  
              MOVE '**                                               **'        
                TO GCCCREGL-OUT-BANNER-TEXT(4)                                  
                                                                                
              MOVE  3  TO GCCCREGL-OUT-BANNER-TYPE                              
              MOVE '1' TO GCCCREGL-OUT-BANNER-CC                                
                                                                                
              MOVE GCCCREGL-OUT-BANNER TO WS-OUT-AFP-RECORD                     
              PERFORM 2600-WRITE-LTR THRU 2600-EXIT                             
                                                                                
              IF LR-STATUS-OK                                                   
                    ADD +1 TO WS-NO-OUTPUT-RECS                                 
                 ELSE                                                           
                    DISPLAY 'ERROR ADDING TO LETTER FILE'                       
                    DISPLAY PROGRAM-LINKAGE-STATUS                              
                    PERFORM 9999-ABEND THRU 9999-ABEND-EXIT                     
              END-IF.                                                           
                                                                                
      *    RESET LETTER COUNT                                                   
                                                                                
           MOVE ZEROS TO WS-LETTER-COUNT.                                       
                                                                                
      *    PRINT LEADING BANNER                                                 
                                                                                
           MOVE SPACES TO LTR-OUT-RECORD.                                       
WB3   *                   LTR-OUT-RECORD-BP.                                    
                                                                                
           IF GCCCCEXT-MAIL-INSTRUCTION = 'A'                                   
           OR GCCCCEXT-MAIL-INSTRUCTION = 'G'                                   
             MOVE '** PGB1200  GB-E                                 **'         
               TO GCCCREGL-OUT-BANNER-TEXT(1)                                   
             MOVE '** SEND TO:                                      **'         
               TO GCCCREGL-OUT-BANNER-TEXT(2)                                   
             MOVE '** GB PIA FLEXLINK                               **'         
               TO GCCCREGL-OUT-BANNER-TEXT(3)                                   
             MOVE '**                                               **'         
               TO GCCCREGL-OUT-BANNER-TEXT(4).                                  
                                                                                
139845     IF GCCCCEXT-MAIL-INSTRUCTION = 'I'                                   
139845       MOVE '** PGB1200  GB-E                                 **'         
139845         TO GCCCREGL-OUT-BANNER-TEXT(1)                                   
139845       MOVE '** SEND TO:                                      **'         
139845         TO GCCCREGL-OUT-BANNER-TEXT(2)                                   
139845       MOVE '** GB PIA                                        **'         
139845         TO GCCCREGL-OUT-BANNER-TEXT(3)                                   
139845       MOVE '**                                               **'         
139845         TO GCCCREGL-OUT-BANNER-TEXT(4).                                  
                                                                                
           IF GCCCCEXT-MAIL-INSTRUCTION = 'S'                                   
             MOVE '** PGB1200  GB-E                                 **'         
               TO GCCCREGL-OUT-BANNER-TEXT(1)                                   
             MOVE '** INTERNAL MAIL                                 **'         
               TO GCCCREGL-OUT-BANNER-TEXT(2)                                   
             MOVE '** SEND TO MAIL ROOM                             **'         
               TO GCCCREGL-OUT-BANNER-TEXT(3)                                   
             MOVE '**                                               **'         
               TO GCCCREGL-OUT-BANNER-TEXT(4).                                  
                                                                                
           IF GCCCCEXT-MAIL-INSTRUCTION = 'C'                                   
             MOVE '** PGB1200  GB-E                                 **'         
               TO GCCCREGL-OUT-BANNER-TEXT(1)                                   
             MOVE '** INSERT IN INDIVIDUAL ENVELOPES & THEN BULK    **'         
               TO GCCCREGL-OUT-BANNER-TEXT(2)                                   
             MOVE '**        TO COMPANY VIA COURIER                 **'         
               TO GCCCREGL-OUT-BANNER-TEXT(3)                                   
             MOVE '** COST CENTER = W/A 413                         **'         
               TO GCCCREGL-OUT-BANNER-TEXT(4).                                  
                                                                                
           IF GCCCCEXT-MAIL-INSTRUCTION = 'B'                                   
            MOVE '** PGB1200  GB-E                                   **'        
               TO GCCCREGL-OUT-BANNER-TEXT(1)                                   
            MOVE '** INSERT IN INDIVIDUAL ENVELOPES & THEN BULK      **'        
               TO GCCCREGL-OUT-BANNER-TEXT(2)                                   
            MOVE '**         TO COMPANY VIA CANADA POST              **'        
               TO GCCCREGL-OUT-BANNER-TEXT(3)                                   
            MOVE '** COST CENTER = W/A 413                           **'        
               TO GCCCREGL-OUT-BANNER-TEXT(4).                                  
                                                                                
           IF GCCCCEXT-MAIL-INSTRUCTION = 'D'                                   
             MOVE '** PGB1200  M14A  INSERTING                      **'         
               TO GCCCREGL-OUT-BANNER-TEXT(1)                                   
             MOVE '** INSERT IN INDIVIDUAL ENVELOPES &              **'         
               TO GCCCREGL-OUT-BANNER-TEXT(2)                                   
             MOVE '**        DIRECT MAIL VIA CANADA POST            **'         
               TO GCCCREGL-OUT-BANNER-TEXT(3)                                   
             MOVE '** COST CENTER = W/A 413                         **'         
               TO GCCCREGL-OUT-BANNER-TEXT(4).                                  
                                                                                
           IF GCCCCEXT-MAIL-INSTRUCTION = 'P'                                   
             MOVE                                                               
             '** PGB1200  M14A  INSERTING - OTIP                     **'        
               TO GCCCREGL-OUT-BANNER-TEXT(1)                                   
             MOVE                                                               
             '** INSERT IN INDIVIDUAL OTIP BRANDED ENVELOPES         **'        
               TO GCCCREGL-OUT-BANNER-TEXT(2)                                   
             MOVE                                                               
             '** DIRECT MAIL VIA CANADA POST (OTIP) ENVELOPE(GL8191B)**'        
               TO GCCCREGL-OUT-BANNER-TEXT(3)                                   
             MOVE '** COST CENTER = W/A 0413                         **'        
               TO GCCCREGL-OUT-BANNER-TEXT(4).                                  
                                                                                
           IF GCCCCEXT-MAIL-INSTRUCTION = 'R'                                   
             MOVE '** PGB1270  GB-E                                 **'         
               TO GCCCREGL-OUT-BANNER-TEXT(1)                                   
             MOVE '** SEND TO:                                      **'         
               TO GCCCREGL-OUT-BANNER-TEXT(2)                                   
             MOVE '** RELIZON VIA MAILROOM                          **'         
               TO GCCCREGL-OUT-BANNER-TEXT(3)                                   
             MOVE '**                                               **'         
               TO GCCCREGL-OUT-BANNER-TEXT(4).                                  
                                                                                
                                                                                
      * ONLY PRINT NEW BANNER IF THIS IS NOT THE END OF THE FILE                
                                                                                
           IF GCCCCEXT-MAIL-INSTRUCTION NOT = ' '                               
                                                                                
              MOVE  3  TO GCCCREGL-OUT-BANNER-TYPE                              
              MOVE '1' TO GCCCREGL-OUT-BANNER-CC                                
                                                                                
              MOVE GCCCREGL-OUT-BANNER TO WS-OUT-AFP-RECORD                     
              PERFORM 2600-WRITE-LTR THRU 2600-EXIT                             
                                                                                
              IF LR-STATUS-OK                                                   
                    ADD +1 TO WS-NO-OUTPUT-RECS                                 
                 ELSE                                                           
                    DISPLAY 'ERROR ADDING TO LETTER FILE'                       
                    DISPLAY PROGRAM-LINKAGE-STATUS                              
                    PERFORM 9999-ABEND THRU 9999-ABEND-EXIT                     
              END-IF.                                                           
                                                                                
                                                                                
           MOVE GCCCCEXT-MAIL-INSTRUCTION TO WS-PREV-MAIL-INSTR.                
                                                                                
       2400-PRINT-BANNER-PAGE-EXIT.                                             
           EXIT.                                                                
                                                                                
       2500-BUILD-LTR.                                                          
           MOVE GCCCCEXT-TEMPLATE-ID      TO GCCCREGL-OUT-TYPE.                 
           MOVE GCCCCEXT-CUST-GROUP       TO GCCCREGL-OUT-GROUP.                
WB3   *                                      GCCCREGB-OUT-GROUP.                
           MOVE GCCCCEXT-CUST-DIVISION    TO GCCCREGL-OUT-DIV.                  
           MOVE GCCCCEXT-CUST-CERT        TO GCCCREGL-OUT-CERT.                 
WB3   *                                      GCCCREGB-OUT-CERT.                 
BC         MOVE GCCCCEXT-SPONSOR-NAME     TO GCCCREGL-SPONSOR-NAME.             
           MOVE GCCCCEXT-CUST-NAME        TO GCCCREGL-OUT-NAME.                 
           MOVE GCCCCEXT-CUST-ADDR1       TO GCCCREGL-OUT-ADDR1.                
           MOVE GCCCCEXT-CUST-ADDR2       TO GCCCREGL-OUT-ADDR2.                
           MOVE GCCCCEXT-CUST-ADDR3       TO GCCCREGL-OUT-ADDR3.                
           MOVE GCCCCEXT-CUST-ADDR4       TO GCCCREGL-OUT-ADDR4.                
                                                                                
HNS        MOVE GCCCCEXT-CSC-PHONE        TO GCCCREGL-OUT-PHONE.                
HNS        MOVE GCCCCEXT-SITE-URL         TO GCCCREGL-OUT-URL.                  
WB                                                                              
WB3   * NO MORE BACKPAGE VARIABLES                                              
WB    * PUT PRETTY SPACE PADDING IN FRONT OF URL'S THAT ARE SMALLER             
WB    * THAN 99 CHARS TO CREATE THE LOOK OF CENTER ALIGNMENT FOR THE            
WB    * BACK PAGE URL                                                           
WB    *    MOVE GCCCCEXT-SITE-URL TO WS-URL.                                    
WB    *    PERFORM VARYING WS-URL-CNT FROM 1 BY 1                               
WB    *       UNTIL WS-URL-CNT > 100                                            
WB    *         OR WS-URL-CHAR-READ (WS-URL-CNT) = SPACE                        
WB    *    END-PERFORM.                                                         
WB    *                                                                         
WB    *    IF WS-URL-CNT < 99 THEN                                              
WB    *       SUBTRACT WS-URL-CNT    FROM 100 GIVING WS-SPACES-LEFT             
WB    *       DIVIDE   WS-SPACES-LEFT BY   2  GIVING WS-SPACE-CNT               
WB   *                                                                          
WB    *       MOVE GCCCCEXT-SITE-URL                                            
WB    *         TO GCCCREGB-OUT-URL(WS-SPACE-CNT:WS-URL-CNT)                    
WB    *    ELSE                                                                 
WB    *       MOVE GCCCCEXT-SITE-URL TO GCCCREGB-OUT-URL                        
WB    *    END-IF.                                                              
WB                                                                              
WB                                                                              
                                                                                
      * CONVERT REGIST-DATE TO OUTPUT FORMAT YYYYMMMDD                          
                                                                                
           MOVE 'B'                       TO VDATE-REQ-SERVICE.                 
           MOVE 'B'                       TO VDATE-REQ-BASIS.                   
           MOVE '1'                       TO VDATE-REQ-DETAIL.                  
           IF GCCCCEXT-CUST-REPLY-LANG    =  'F'                                
               MOVE 'F'                   TO VDATE-REQ-LANGUAGE                 
           ELSE                                                                 
               MOVE 'E'                   TO VDATE-REQ-LANGUAGE                 
           END-IF.                                                              
           MOVE GCCCCEXT-REGIST-DATE      TO VDATE1-YYYYMMDD.                   
                                                                                
           CALL WS-GC2DATE                USING GAC-DATE-PARAMETERS.            
                                                                                
           MOVE VDATE-EXT-DATE            TO WS-REGN-DATE.                      
           MOVE WS-REGN-DAY               TO WS-DISPLAY-DAY.                    
           MOVE WS-REGN-MTH               TO WS-DISPLAY-MTH.                    
           MOVE WS-REGN-YEAR              TO WS-DISPLAY-YEAR.                   
           MOVE WS-DISPLAY-DATE           TO GCCCREGL-OUT-REGN-DATE.            
                                                                                
                                                                                
           IF GCCCCEXT-CUST-REPLY-LANG = 'F'                                    
              MOVE WS-F-CURRENT-DATE      TO GCCCREGL-OUT-CURR-DATE             
           ELSE                                                                 
              MOVE WS-E-CURRENT-DATE      TO GCCCREGL-OUT-CURR-DATE.            
                                                                                
           MOVE GCCCCEXT-CO-NAME1         TO GCCCREGL-OUT-CO-NAME1.             
           MOVE GCCCCEXT-CO-NAME2         TO GCCCREGL-OUT-CO-NAME2.             
                                                                                
JAK        MOVE GCCCCEXT-ACTN-KEY         TO GCCCREGL-ACTN-KEY.                 
                                                                                
           MOVE GCCCCEXT-ENROL-DATE       TO GCCCREGL-ENROL-DATE.               
                                                                                
           MOVE '1'                       TO GCCCREGL-OUT-CC.                   
                                                                                
           MOVE LTR-OUT-RECORD TO WS-OUT-AFP-RECORD.                            
           PERFORM 2600-WRITE-LTR THRU 2600-EXIT.                               
                                                                                
WB3   *    MOVE LTR-OUT-RECORD-BP TO WS-OUT-AFP-RECORD.                         
WB3   *    PERFORM 2600-WRITE-LTR THRU 2600-EXIT.                               
                                                                                
           IF LR-STATUS-OK                                                      
                 ADD +1 TO WS-NO-OUTPUT-RECS                                    
              ELSE                                                              
                 DISPLAY 'ERROR ADDING TO LETTER FILE'                          
                 DISPLAY PROGRAM-LINKAGE-STATUS                                 
                 PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                       
                                                                                
                                                                                
       2500-BUILD-LTR-EXIT.                                                     
           EXIT.                                                                
                                                                                
       2600-WRITE-LTR.                                                          
                                                                                
           MOVE WS-PRINT-LR           TO LOGICAL-RECORD-NAME.                   
                                                                                
           MOVE STORE-LR          TO     WS-GAEDATSR-VERB.                      
           CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
                                         WS-OUT-AFP-RECORD                      
                                         ICBM.                                  
                                                                                
       2600-EXIT.                                                               
           EXIT.                                                                
                                                                                
                                                                                
       3000-READ-INPUT.                                                         
      ******************************************************************        
      *    READ INPUT RECORD.                                          *        
      ******************************************************************        
           MOVE WS-INPUT-LR          TO LOGICAL-RECORD-NAME.                    
           INITIALIZE INP-RECORD.                                               
                                                                                
           CALL GAEDATSR          USING  WS-GAEDATSR-VERB                       
                                         INP-RECORD                             
                                         ICBM.                                  
                                                                                
           IF LR-STATUS-OK                                                      
                 ADD +1 TO WS-NO-INP-RECORDS.                                   
                                                                                
       3000-READ-INPUT-EXIT.                                                    
           EXIT.                                                                
                                                                                
                                                                                
       9000-FINISH.                                                             
                                                                                
      ******************************************************************        
      * CLOSE FILES THAT ARE OPEN...                                            
      ******************************************************************        
                                                                                
           MOVE WS-INPUT-LR TO LOGICAL-RECORD-NAME.                             
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB.                                
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
           IF LR-STATUS-OK                                                      
                 NEXT SENTENCE                                                  
              ELSE                                                              
                 DISPLAY 'ERROR CLOSING INPUT LETTER FILE'                      
                 DISPLAY PROGRAM-LINKAGE-STATUS                                 
                 PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                       
                                                                                
                                                                                
           MOVE WS-PRINT-LR TO LOGICAL-RECORD-NAME.                             
           MOVE FINISH-LR   TO WS-GAEDATSR-VERB.                                
           CALL GAEDATSR USING WS-GAEDATSR-VERB                                 
                               LOGICAL-RECORD-NAME                              
                               ICBM.                                            
                                                                                
           IF LR-STATUS-OK                                                      
                 NEXT SENTENCE                                                  
              ELSE                                                              
                 DISPLAY 'ERROR CLOSING OUTPUT AFP FILE'                        
                 DISPLAY PROGRAM-LINKAGE-STATUS                                 
                 PERFORM 9999-ABEND THRU 9999-ABEND-EXIT.                       
                                                                                
                                                                                
           DISPLAY 'REGISTRATION LETTER REQUESTS: ' WS-NO-INP-RECORDS.          
           DISPLAY 'LETTER AFP RECORDS WRITTEN: ' WS-NO-OUTPUT-RECS.    ECS.    
                                                                                
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9999-ABEND.                                                              
      ******************************************************************        
      *    ABEND AND HALT PROGRAM                                               
      ******************************************************************        
                                                                                
           MOVE +16 TO RETURN-CODE.                                             
           GOBACK.                                                              
                                                                                
       9999-ABEND-EXIT.                                                         
           EXIT.                                                                
