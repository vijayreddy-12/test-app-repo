       CBL FLAG(I),DATA(24)                                                     
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GACBRFMT.                                                 
      *AUTHOR.        FRED MUELLER.                                             
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.  OCTOBER 5, 1998.                                          
      *DATE-COMPILED.                                                           
      *----------------------------------------------------------------*        
      *                                                                *        
      *  SYSTEM    : GROUP BENEFITS                                    *        
      *                           - GENERIC PARSE UTILITY              *        
      *  LANGUAGE  : COBOL II                                          *        
      *                                                                *        
      *  TYPE      : BATCH                                             *        
      *                                                                *        
      *  COPYBOOKS : GARDSVRB - DATA SERVER VERBS                      *        
      *            : ICBM     - DATA SERVER INTERFACE CONTROL BLOCK    *        
      *            : MLX2PRSI - V2 PARSER CONTROL AREA                 *        
      *            : MLX2PRSO - V2 PARSER RETURN AREA                  *        
      *            : MLMSGHDR - V1 MLI FIXED MESSAGE HEADER DEFINITION *        
      *            : MLMV2HDR - V2 MLI FIXED MESSAGE HEADER DEFINITION *        
      *                                                                *        
      *  CALLS     : GAEDATSR - DATA SERVER                            *        
      *            : MLX2CLR  - PARSER CLEAR MESSAGE                   *        
      *            : MLX2CTOT - PARSER COPYBOOK TO TAGGED STREAM       *        
      *            : MLX2TTOC - PARSER TAGGED STREAM TO COPYBOOK       *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *  PARMS  1. 'NO PARSER ERRORS  ' NO DISPLAY FOR PARSER ERRORS   *        
      *            'NO PARSER WARNINGS' NO DISPLAY FOR PARSER WARNINGS *        
      *            'NO PARSER SUMMARY ' NO PARSER ERRORS OR WARNINGS   *        
      *            'NO PARSER DETAIL  ' NO DISPLAY OF OCCURS 10 DATA   *        
      *         2. 'NO EXIT ERRORS    ' NO DISPLAYS OF EXIT RETURNS    *        
      * NOTE: FOR PARM  2, PARM 1 MUST BE USED, OR AT LEAST 20 SPACES  *        
      *       SET TO HOLD POSITION                                     *        
      ******************************************************************        
      *                                                                *        
      *  INPUT     : LEAD CARD REQUEST FILE                            *        
      *            : TRANSACTION FILE                                  *        
      *            : TARGET FORMAT OF TRANSACTION FILE (MSG OR REC)    *        
      *            : VERSION OF MESSAGING (REQUIRED IF FORMAT=MSG)     *        
      *            : COPYBOOK DEFINITION FOR USE BY PARSER             *        
      *            : PREPROCESSOR EXIT-PROGRAM  (OPTIONAL)             *        
      *            : POSTPROCESSOR EXIT-PROGRAM (OPTIONAL)             *        
      *            : CONDCODE  (OPTIONAL)                              *        
      *                                                                *        
      *  OUTPUT    : REFORMATTED TRANSACTION FILE                      *        
      *            : RETURN CODE                                       *        
      *                                                                *        
      *--HISTORY LOG---------------------------------------------------*        
      *  SEQ  DATE       DESIGNER   DESCRIPTION                        *        
      *  001  OCT 1998   F MUELLER  CREATED                            *        
BM002 *  002  JUN 1999   B MELANSON MASK CONDITION CODE, AND DISPLAYS  *        
      *  003  JUL 2008   IBM GR     UPGRADED IN ECU PROJECT            *        
      *----------------------------------------------------------------*        
       ENVIRONMENT DIVISION.                                                    
                                                                                
       CONFIGURATION SECTION.                                                   
                                                                                
       SOURCE-COMPUTER. IBM-370.                                                
       OBJECT-COMPUTER. IBM-370.                                                
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
      /                                                                         
       WORKING-STORAGE SECTION.                                                 
       01  FILLER                      PIC X(40) VALUE                          
               '**  GACBRFMT WORKING STORAGE BEGINS  **'.                       
                                                                                
       01  WS-INDICATORS.                                                       
           05  ERROR-IND               PIC X(01).                               
               88  NO-ERROR                      VALUE 'N'.                     
               88  YES-ERROR                     VALUE 'Y'.                     
               88  FATAL-ERROR                   VALUE 'F'.                     
           05  PARSE-ERROR-IND         PIC X(01).                               
               88  PARSE-NO-ERRORS               VALUE ' '.                     
               88  PARSE-WARNINGS                VALUE 'W'.                     
               88  PARSE-ERRORS                  VALUE 'E'.                     
           05  INPUT-EOF-IND           PIC X(01).                               
               88  INPUT-NOT-EOF                 VALUE 'N'.                     
               88  INPUT-EOF                     VALUE 'Y'.                     
                                                                                
       01  WS-CALLED-MODULES.                                                   
           05  WS-GAEDATSR             PIC X(08) VALUE 'GAEDATSR'.              
           05  WS-PARSER               PIC X(08).                               
               88  PARSE-CLEAR-CALL              VALUE 'MLX2CLR '.              
               88  PARSE-MSG-TO-REC-CALL         VALUE 'MLX2TTOC'.              
               88  PARSE-REC-TO-MSG-CALL         VALUE 'MLX2CTOT'.              
       01  SUB                         PIC 9(02).                               
       01  READ-COUNT                  PIC 9(08) VALUE 0.                       
       01  READ-COUNT-DISPLAY          REDEFINES READ-COUNT                     
                                       PIC ZZZZZZZ9.                            
       01  WS-VARIABLES.                                                        
BM002      05  CNT-PRE-WARNINGS  VALUE 0   PIC 9(06).                           
BM002      05  CNT-PRE-ERRORS    VALUE 0   PIC 9(06).                           
BM002      05  CNT-PRE-FATALS    VALUE 0   PIC 9(06).                           
BM002      05  CNT-POST-WARNINGS VALUE 0   PIC 9(06).                           
BM002      05  CNT-POST-ERRORS   VALUE 0   PIC 9(06).                           
BM002      05  CNT-POST-FATALS   VALUE 0   PIC 9(06).                           
BM002      05  CNT-NOT-FOUNDS    VALUE 0   PIC 9(06).                           
BM002      05  CNT-NON-CRITICALS VALUE 0   PIC 9(06).                           
BM002      05  CNT-CRITICALS     VALUE 0   PIC 9(06).                           
BM002      05  PRT-CNT-1                   PIC ZZZ,ZZ9.                         
BM002      05  PRT-CNT-2                   PIC ZZZ,ZZ9.                         
BM002      05  PRT-CNT-3                   PIC ZZZ,ZZ9.                         
BM002      05  PRT-PARM-LENGTH         PIC ZZZ9-.                               
           05  WS-COMMENT-CHAR         PIC X(01).                               
               88 VALID-COMMENT        VALUE '*' ';'.                           
           05  WS-RETURN-CODE          PIC 9(02).                               
               88 PROCESS-SUCCESSFUL   VALUE 00.                                
               88 PROCESS-FAILED       VALUE 16.                                
           05  WS-FINAL-RETURN-CODE    PIC 9(02).                               
           05  WS-EXIT-RETURN-CODE     PIC 9(02).                               
               88  EXIT-RET-OK                   VALUE 00.                      
               88  EXIT-RET-WARNING              VALUE 01 THRU 07.              
               88  EXIT-RET-ERROR                VALUE 08.                      
               88  EXIT-RET-FATAL-ERROR          VALUE 09 THRU 99.              
           05  WS-GAEDATSR-VERB        PIC X(16).                               
           05  WS-TARGET-FORMAT        PIC X(03).                               
               88  TARGET-MESSAGE-FORMAT         VALUE 'MSG'.                   
               88  TARGET-RECORD-FORMAT          VALUE 'REC'.                   
           05  WS-ERROR-ACTION         PIC X(04) VALUE SPACES.                  
               88  ERROR-ACTION-SKIP             VALUE 'SKIP'.                  
               88  ERROR-ACTION-SAVE             VALUE 'SAVE'.                  
               88  ERROR-ACTION-STOP             VALUE 'STOP'.                  
           05  WS-PARSE-ERR-PGM        PIC X(08) VALUE SPACES.                  
           05  WS-PRE-EXIT-PGM         PIC X(08) VALUE SPACES.                  
           05  WS-POST-EXIT-PGM        PIC X(08) VALUE SPACES.                  
BM002      05  WS-ALLOWED-CONDITION    PIC 9(04) VALUE 0.                       
BM002          88  ALLOWED-CONDITION VALUE 0001 THRU 9999.                      
           05  WS-HEADER-VERSION-X     PIC X(07).                               
           05  WS-HEADER-VERSION-FIELDS.                                        
               10  WS-VERSION-NUM-1    PIC X(04).                               
               10  WS-VERSION-LEN-1    PIC 9.                                   
               10  WS-VERSION-NUM-2    PIC X(02).                               
               10  WS-VERSION-LEN-2    PIC 9.                                   
               10  WS-VERSION-999999.                                           
                   15  WS-VERSION-9999 PIC 9(04).                               
                   15  WS-VERSION-99   PIC 9(02).                               
               10  WS-HEADER-VERSION   REDEFINES WS-VERSION-999999              
                                       PIC 9(04)V99.                            
                   88  MSG-VERSION-2             VALUE 2.                       
                   88  MSG-VERSION-1             VALUE 1.                       
           05  WS-QALIAS-NAME          PIC X(48).                               
           05  WS-COPYBOOK-NAME        PIC X(08).                               
           05  WS-CHAR-OPTION          PIC X(01).                               
           05  WS-RUN-OPTION           PIC X(01).                               
           05  WS-OCCURS-OPTION        PIC X(01).                               
           05  WS-SECTION              PIC X(01).                               
           05  WS-SECTION-OCCUR        PIC X(04).                               
           05  WS-SECTION-OCCUR-LEN    PIC S9(04) COMP.                         
           05  WS-SECTION-OCCUR-NUM    PIC S9(04) COMP.                         
                                                                                
       01  WS-LEADCARD-TABLE.                                                   
           05  WS-LEADCARD-TOKEN       PIC X(30).                               
           05  WS-LEADCARD-DETAIL.                                              
               10 WS-LEADCARD-SIZE     PIC S9(4) COMP.                          
               10 WS-LEADCARD-DATA     PIC X(48).                               
                                                                                
       01  MLI-MSG-HEADER.                                                      
           05  FILLER                  PIC X(200).                              
                                                                                
      *-----------------------------------------------------------------        
      *    MLI MESSAGE HEADER VERSION 1.00                                      
      *-----------------------------------------------------------------        
       01  MLI-MSG-HEADER-V1.                                                   
           COPY MLMSGHDR.                                                       
      /                                                                         
                                                                                
      *-----------------------------------------------------------------        
      *    MLI MESSAGE HEADER VERSION 2.00                                      
      *-----------------------------------------------------------------        
       01  MLI-MSG-HEADER-V2.                                                   
           COPY MLMV2HDR.                                                       
      /                                                                         
                                                                                
      *-----------------------------------------------------------------        
      *    PARSER CONTROL AREA                                                  
      *-----------------------------------------------------------------        
       01  PARSER-CONTROL-AREA.                                                 
           COPY MLX2PRSI.                                                       
      /                                                                         
                                                                                
      *-----------------------------------------------------------------        
      *    PARSER RETURN AREA                                                   
      *-----------------------------------------------------------------        
       01  PARSER-RETURN-AREA.                                                  
           COPY MLX2PRSO.                                                       
      /                                                                         
                                                                                
      *-----------------------------------------------------------------        
      *    I/O BUFFER AREA                                                      
      *-----------------------------------------------------------------        
       01  LEADCARD-RECORD             PIC X(80).                               
       01  RECORD-AREA.                                                         
           05  RECORD-DATA-LENGTH      PIC S9(04) COMP.                         
           05  FILLER                  PIC X(02).                               
           05  RECORD-BUFFER           PIC X(16000).                            
       01  MESSAGE-AREA.                                                        
           05  MESSAGE-DATA-LENGTH     PIC S9(04) COMP.                         
           05  FILLER                  PIC X(02).                               
           05  MESSAGE-BUFFER          PIC X(16000).                            
      /                                                                         
                                                                                
      *----------------------------------------------------------------*        
      *    ACTION VERBS USED TO CALL GAEDATSR                                   
      *----------------------------------------------------------------*        
       01  DATA-SERVER-VERBS.                                                   
           COPY GARDSVRB.                                                       
      /                                                                         
                                                                                
      *----------------------------------------------------------------*        
      *    LOGICAL RECORD NAMES                                                 
      *----------------------------------------------------------------*        
       01  LEADCARD-LR                   PIC X(16)                              
                               VALUE 'CARD-DATA-010   '.                        
       01  INPUT-LR                      PIC X(16)                              
                               VALUE 'CARD-DATA-070   '.                        
       01  OUTPUT-LR                     PIC X(16)                              
                               VALUE 'PRINT-DATA-071  '.                        
       01  ERROR-LR                      PIC X(16)                              
                               VALUE 'PRINT-DATA-072  '.                        
      /                                                                         
                                                                                
       01  ICBM-AREA.                                                           
           COPY ICBM.                                                           
      /                                                                         
                                                                                
       01  FILLER                      PIC X(40) VALUE                          
               '*** GACBRFMT WORKING STORAGE ENDS   ***'.                       
                                                                                
      /                                                                         
BM002  LINKAGE SECTION.                                                         
BM002  01  INPUT-PARM.                                                          
BM002      05 PARM-LENGTH    PIC S9(04) COMP.                                   
BM002      05 PARM-DATA.                                                        
BM002         10  PARM-PARSER               PIC X(20).                          
BM002             88 NO-PARSER-MASK      VALUE                                  
BM002                                    '                    '.                
BM002             88 MASK-PARSER-ERRORS  VALUE                                  
BM002                                    'NO PARSER SUMMARY   '                 
BM002                                    'NO PARSER ERRORS    '.                
BM002             88 MASK-PARSER-WARNS   VALUE                                  
BM002                                    'NO PARSER SUMMARY   '                 
BM002                                    'NO PARSER WARNINGS  '.                
BM002             88 MASK-PARSER-DETAIL  VALUE                                  
BM002                                    'NO PARSER SUMMARY   '                 
BM002                                    'NO PARSER ERRORS    '                 
BM002                                    'NO PARSER WARNINGS  '                 
BM002                                    'NO PARSER DETAIL    '.                
BM002         10  PARM-EXIT                 PIC X(20).                          
BM002             88 NO-EXIT-MASK        VALUE                                  
BM002                                    '                    '.                
BM002             88 MASK-EXITS  VALUE   'NO EXIT ERRORS      '.                
                                                                                
       PROCEDURE DIVISION USING INPUT-PARM.                                     
      *-----------------------------------                                      
                                                                                
       0000-MAINLINE.                                                           
                                                                                
           PERFORM 1000-INITIALIZATION THRU                                     
                   1000-INITIALIZATION-EXIT.                                    
                                                                                
           PERFORM 1500-PROCESS-LEADCARD THRU                                   
                   1500-PROCESS-LEADCARD-EXIT                                   
                                                                                
           IF NO-ERROR                                                          
              PERFORM 2000-MAIN-PROCESS THRU                                    
                      2000-MAIN-PROCESS-EXIT                                    
           END-IF.                                                              
                                                                                
           PERFORM 3000-FINALIZATION THRU                                       
                   3000-FINALIZATION-EXIT.                                      
                                                                                
       0000-MAINLINE-EXIT.                                                      
           STOP RUN.                                                            
      /                                                                         
                                                                                
      ******************************************************************        
      * INITIALIZATION                                                          
      ******************************************************************        
       1000-INITIALIZATION.                                                     
BM002      EVALUATE TRUE                                                        
BM002      WHEN PARM-LENGTH = 0                                                 
BM002          CONTINUE                                                         
BM002      WHEN PARM-LENGTH = 40                                                
BM002          DISPLAY '========================================='              
BM002          EVALUATE TRUE                                                    
BM002            WHEN MASK-PARSER-DETAIL                                        
BM002              DISPLAY '      PARSER ERROR MASK: ' PARM-PARSER              
BM002            WHEN NO-PARSER-MASK                                            
BM002              CONTINUE                                                     
BM002            WHEN OTHER                                                     
BM002              DISPLAY '    INVALID PARSER MASK: ' PARM-PARSER              
BM002              MOVE 97 TO RETURN-CODE                                       
BM002              STOP RUN                                                     
BM002          END-EVALUATE                                                     
BM002          EVALUATE TRUE                                                    
BM002            WHEN MASK-EXITS                                                
BM002              DISPLAY '           EXIT MASK ON: ' PARM-EXIT                
BM002            WHEN NO-EXIT-MASK                                              
BM002              CONTINUE                                                     
BM002            WHEN OTHER                                                     
BM002              DISPLAY '      INVALID EXIT MASK: ' PARM-EXIT                
BM002              MOVE 98 TO RETURN-CODE                                       
BM002              STOP RUN                                                     
BM002          END-EVALUATE                                                     
BM002          DISPLAY '========================================='              
BM002      WHEN OTHER                                                           
BM002          MOVE PARM-LENGTH TO PRT-PARM-LENGTH                              
BM002          DISPLAY 'INVALID PARM LENGTH: ' PRT-PARM-LENGTH                  
BM002          DISPLAY '       INVALID PARM: ' PARM-DATA                        
BM002          MOVE 99 TO RETURN-CODE                                           
BM002          STOP RUN                                                         
BM002      END-EVALUATE.                                                        
                                                                                
                                                                                
           SET NO-ERROR                   TO TRUE.                              
           SET ERROR-ACTION-SKIP          TO TRUE.                              
           MOVE 'GACBRFMT'                TO ICBM-PROGRAM-NAME.                 
           MOVE LOW-VALUES                TO LINKAGE-CONTROL.                   
                                                                                
      *                                                                         
      * SET UP THE PARSER CONTROL DEFAULT VALUES                                
      *                                                                         
      * SECTION TO UPDATE IS THE MESSAGE BODY                                   
      *                                                                         
           MOVE 'B'                       TO WS-SECTION.                        
           MOVE '1'                       TO WS-SECTION-OCCUR.                  
           MOVE +1                        TO WS-SECTION-OCCUR-LEN.              
           MOVE WS-SECTION-OCCUR (1:WS-SECTION-OCCUR-LEN)                       
                                          TO WS-SECTION-OCCUR-NUM.              
      *                                                                         
      * RUN OPTION IS NORMAL  (I.E. PARSER DEFN DEFAULTING APPLIES)             
      *                                                                         
           MOVE '0'                       TO WS-RUN-OPTION.                     
      *                                                                         
      * CHAR OPTION IS NONE   (I.E. NO CHARACTER TRANSLATION APPLIES)           
      *                                                                         
           MOVE 'N'                       TO WS-CHAR-OPTION.                    
      *                                                                         
      * OCCURS OPTION IS NONE (I.E. COLLAPSE EMPTY OCCURS DEPENDING)            
      *                                                                         
           MOVE ' '                       TO WS-OCCURS-OPTION.                  
                                                                                
                                                                                
           PERFORM 1100-DEFAULT-MSG-HEADER THRU                                 
                   1100-DEFAULT-MSG-HEADER-EXIT.                                
                                                                                
           SET PROCESS-SUCCESSFUL         TO TRUE.                              
           PERFORM 9000-SET-RETURN-CODE THRU                                    
                   9000-SET-RETURN-CODE-EXIT.                                   
                                                                                
       1000-INITIALIZATION-EXIT.                                                
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * SET UP DEFAULTS FOR MLI MESSAGE HEADER.                                 
      ******************************************************************        
       1100-DEFAULT-MSG-HEADER.                                                 
                                                                                
           INITIALIZE MLI-MSG-HEADER-V1                                         
                      MLI-MSG-HEADER-V2.                                        
                                                                                
           MOVE 2.00                                                            
             TO HEADER-VERSION-NUMBER       OF MLI-MSG-HEADER-V2.               
           MOVE 1.00                                                            
             TO MSGHDR-VERSION              OF MLI-MSG-HEADER-V1.               
           MOVE '000100'                                                        
             TO MESSAGE-TYPE-VERSION-NUMBER OF MLI-MSG-HEADER-V2.               
           MOVE 'MVS'                                                           
             TO ACCESS-CHANNEL              OF MLI-MSG-HEADER-V2.               
           MOVE 'S0'                                                            
             TO SECURITY-LEVEL              OF MLI-MSG-HEADER-V2.               
           MOVE 'S1'                                                            
             TO ENTRY-USERROLE              OF MLI-MSG-HEADER-V2.               
           MOVE 'ENG'                                                           
             TO REPLY-LANGUAGE              OF MLI-MSG-HEADER-V2.               
           MOVE '+0000'                                                         
             TO OFFSET-FROM-GMT             OF MLI-MSG-HEADER-V2.               
                                                                                
       1100-DEFAULT-MSG-HEADER-EXIT.                                            
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * OBTAIN LEADCARD REQUEST INFO FROM DLSI10 AND STORE THEM.                
      ******************************************************************        
       1500-PROCESS-LEADCARD.                                                   
                                                                                
           DISPLAY '**************************************************'.        
           DISPLAY '*           INPUT CARD DISPLAY BEGINS            *'.        
           DISPLAY '**************************************************'.        
                                                                                
      *                                                                         
      * OPEN LEADCARD FILE, READ FIRST RECORD                                   
      *                                                                         
           MOVE LEADCARD-LR       TO LOGICAL-RECORD-NAME.                       
           MOVE OBTAIN-FIRST      TO WS-GAEDATSR-VERB.                          
           PERFORM 1510-LEADCARD-DATASRVR THRU                                  
                   1510-LEADCARD-DATASRVR-EXIT.                                 
                                                                                
      *                                                                         
      * WHILE NOT EOF                                                           
      *   PARSE EACH RECORD INTO WORKING STORAGE                                
      *   READ NEXT RECORD                                                      
      *                                                                         
           PERFORM 1505-PARSE-LEADCARD THRU                                     
                   1505-PARSE-LEADCARD-EXIT                                     
                   UNTIL LR-NOT-FOUND OR FATAL-ERROR.                           
                                                                                
      *                                                                         
      * CLOSE LEADCARD FILE                                                     
      *                                                                         
           MOVE FINISH-LR      TO WS-GAEDATSR-VERB                              
           PERFORM 1510-LEADCARD-DATASRVR THRU                                  
                   1510-LEADCARD-DATASRVR-EXIT.                                 
                                                                                
           DISPLAY '**************************************************'.        
           DISPLAY '*            INPUT CARD DISPLAY ENDS             *'.        
           DISPLAY '**************************************************'.        
      *                                                                         
      *    DISPLAY PARAMETERS TO BE USED IN THE PROGRAM                         
      *                                                                         
           IF NO-ERROR                                                          
              PERFORM 1520-DISPLAY-LEADCARD THRU                                
                      1520-DISPLAY-LEADCARD-EXIT                                
           END-IF.                                                              
                                                                                
       1500-PROCESS-LEADCARD-EXIT.                                              
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * UNSTRING INFORMATION FOUND IN EACH LEADCARD RECORD                      
      ******************************************************************        
       1505-PARSE-LEADCARD.                                                     
                                                                                
           DISPLAY LEADCARD-RECORD.                                             
                                                                                
           INITIALIZE WS-LEADCARD-TABLE.                                        
           UNSTRING LEADCARD-RECORD                                             
           DELIMITED BY '=(' OR '=' OR ')' OR ALL SPACES                        
                INTO WS-LEADCARD-TOKEN                                          
                     WS-LEADCARD-DATA COUNT IN WS-LEADCARD-SIZE.                
                                                                                
           EVALUATE WS-LEADCARD-TOKEN                                           
            WHEN 'TARGET'                                                       
               MOVE WS-LEADCARD-DATA          TO WS-TARGET-FORMAT               
            WHEN 'COPYBOOK'                                                     
               MOVE WS-LEADCARD-DATA          TO WS-COPYBOOK-NAME               
            WHEN 'SECTION'                                                      
               UNSTRING WS-LEADCARD-DATA                                        
                 DELIMITED BY ',' OR ALL SPACES                                 
                 INTO WS-SECTION                                                
                      WS-SECTION-OCCUR  COUNT IN WS-SECTION-OCCUR-LEN           
                 MOVE WS-SECTION-OCCUR (1:WS-SECTION-OCCUR-LEN)                 
                                              TO WS-SECTION-OCCUR-NUM           
            WHEN 'OPTIONS'                                                      
               UNSTRING WS-LEADCARD-DATA                                        
                 DELIMITED BY ',' OR ALL SPACES                                 
                 INTO WS-RUN-OPTION                                             
                      WS-CHAR-OPTION                                            
                      WS-OCCURS-OPTION                                          
            WHEN 'PREPROC'                                                      
               MOVE WS-LEADCARD-DATA          TO WS-PRE-EXIT-PGM                
            WHEN 'POSTPROC'                                                     
               MOVE WS-LEADCARD-DATA          TO WS-POST-EXIT-PGM               
BM002       WHEN 'CONDCODE'                                                     
BM002          MOVE WS-LEADCARD-DATA(1:4)     TO WS-ALLOWED-CONDITION           
            WHEN 'ERRORACTION'                                                  
               MOVE WS-LEADCARD-DATA          TO WS-ERROR-ACTION                
            WHEN 'MESSAGEVERSION'                                               
               MOVE WS-LEADCARD-DATA (1:WS-LEADCARD-SIZE)                       
                                              TO WS-HEADER-VERSION-X            
               UNSTRING WS-HEADER-VERSION-X DELIMITED BY '.'                    
               INTO WS-VERSION-NUM-1 COUNT IN WS-VERSION-LEN-1                  
                    WS-VERSION-NUM-2 COUNT IN WS-VERSION-LEN-2                  
               MOVE WS-VERSION-NUM-1 (1:WS-VERSION-LEN-1)                       
                 TO WS-VERSION-9999                                             
               MOVE WS-VERSION-NUM-2 (1:WS-VERSION-LEN-2)                       
                 TO WS-VERSION-99                                               
               MOVE WS-HEADER-VERSION                                           
                 TO HEADER-VERSION-NUMBER     OF MLI-MSG-HEADER-V2              
                    MSGHDR-VERSION            OF MLI-MSG-HEADER-V1              
            WHEN 'QALIAS'                                                       
               MOVE WS-LEADCARD-DATA          TO WS-QALIAS-NAME                 
               UNSTRING WS-QALIAS-NAME DELIMITED BY '.'                         
               INTO ACCESS-CHANNEL            OF MLI-MSG-HEADER-V2              
                    MESSAGE-TYPE-QUALIFIER    OF MLI-MSG-HEADER-V2              
               MOVE MESSAGE-TYPE-QUALIFIER    OF MLI-MSG-HEADER-V2              
                 TO MSGHDR-TXN-TYPE           OF MLI-MSG-HEADER-V1              
            WHEN 'MESSAGETYPEIDMAJOR'                                           
               MOVE WS-LEADCARD-DATA                                            
                 TO TYPEID-MAJOR              OF MLI-MSG-HEADER-V2              
                    MSGHDR-TXN-SUB-TYPE-MAJOR OF MLI-MSG-HEADER-V1              
            WHEN 'MESSAGETYPEIDMINOR'                                           
               MOVE WS-LEADCARD-DATA                                            
                 TO TYPEID-MINOR              OF MLI-MSG-HEADER-V2              
                    MSGHDR-TXN-SUB-TYPE-MINOR OF MLI-MSG-HEADER-V1              
            WHEN 'MESSAGETYPEVERSION'                                           
               MOVE WS-LEADCARD-DATA                                            
                 TO MESSAGE-TYPE-VERSION-NUMBER OF MLI-MSG-HEADER-V2            
            WHEN 'ENTRYUSERID'                                                  
               MOVE WS-LEADCARD-DATA                                            
                 TO ENTRY-USERID              OF MLI-MSG-HEADER-V2              
            WHEN 'ENTRYUSERROLE'                                                
               MOVE WS-LEADCARD-DATA                                            
                 TO ENTRY-USERROLE            OF MLI-MSG-HEADER-V2              
            WHEN 'SECURITYLEVEL'                                                
               MOVE WS-LEADCARD-DATA                                            
                 TO SECURITY-LEVEL            OF MLI-MSG-HEADER-V2              
            WHEN 'REPLYLANGUAGE'                                                
               MOVE WS-LEADCARD-DATA                                            
                 TO REPLY-LANGUAGE            OF MLI-MSG-HEADER-V2              
            WHEN OTHER                                                          
               MOVE WS-LEADCARD-TOKEN (1:1)   TO WS-COMMENT-CHAR                
               IF VALID-COMMENT                                                 
                  CONTINUE                                                      
               ELSE                                                             
                  DISPLAY '** CARD DATA IGNORED --> ' LEADCARD-RECORD           
               END-IF                                                           
           END-EVALUATE.                                                        
                                                                                
           MOVE OBTAIN-NEXT    TO WS-GAEDATSR-VERB.                             
           PERFORM 1510-LEADCARD-DATASRVR THRU                                  
                   1510-LEADCARD-DATASRVR-EXIT.                                 
                                                                                
       1505-PARSE-LEADCARD-EXIT.                                                
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * HANDLE LEADCARD DATA SERVER CALLS                                       
      ******************************************************************        
       1510-LEADCARD-DATASRVR.                                                  
                                                                                
           CALL WS-GAEDATSR       USING WS-GAEDATSR-VERB                        
                                        LEADCARD-RECORD                         
                                        ICBM-AREA.                              
           IF LR-NOT-FOUND                                                      
           OR KEYED-LR-NOT-FOUND                                                
              IF WS-GAEDATSR-VERB = OBTAIN-FIRST                                
                 DISPLAY '**LEADCARD FILE EMPTY: '                              
                            PROGRAM-LINKAGE-STATUS                              
                 PERFORM 8000-FATAL-ERROR THRU                                  
                         8000-FATAL-ERROR-EXIT                                  
              END-IF                                                            
           ELSE                                                                 
              IF NOT LR-STATUS-OK                                               
                 IF WS-GAEDATSR-VERB = FINISH-LR                                
                    DISPLAY '**LEADCARD FILE CLOSE ERROR: '                     
                            PROGRAM-LINKAGE-STATUS                              
                 ELSE                                                           
                    DISPLAY '**LEADCARD FILE READ ERROR: '                      
                 END-IF                                                         
                 PERFORM 8000-FATAL-ERROR THRU                                  
                         8000-FATAL-ERROR-EXIT                                  
              END-IF                                                            
           END-IF.                                                              
                                                                                
       1510-LEADCARD-DATASRVR-EXIT.                                             
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * DISPLAY LEADCARD PROCESSING RESULTS                                     
      ******************************************************************        
       1520-DISPLAY-LEADCARD.                                                   
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '=================================================='.        
           DISPLAY '=         REFORMAT CONTROL DISPLAY BEGINS        ='.        
           DISPLAY '=================================================='.        
           DISPLAY '= PARSER CONTROLS:   '.                                     
           DISPLAY '=   COPYBOOK DEFN  - ', WS-COPYBOOK-NAME.                   
           DISPLAY '=   SECTION        - ', WS-SECTION.                         
           DISPLAY '=   SECTION OCCURS - ', WS-SECTION-OCCUR.                   
           DISPLAY '=   RUN OPTION     - ', WS-RUN-OPTION.                      
           DISPLAY '=   CHAR OPTION    - ', WS-CHAR-OPTION.                     
           DISPLAY '=   OCCURS OPTION  - ', WS-OCCURS-OPTION.                   
           DISPLAY '= EXIT CONTROLS:     '.                                     
           DISPLAY '=   PRE PROCESSOR  - ', WS-PRE-EXIT-PGM.                    
           DISPLAY '=   POST PROCESSOR - ', WS-POST-EXIT-PGM.                   
           DISPLAY '= ERROR CONTROLS:    '.                                     
           DISPLAY '=   ACTION         - ', WS-ERROR-ACTION.                    
           DISPLAY '= COND CODE LIMIT  - ', WS-ALLOWED-CONDITION.               
           DISPLAY '=                    '.                                     
           DISPLAY '= TARGET FORMAT    - ', WS-TARGET-FORMAT.                   
                                                                                
           EVALUATE TRUE                                                        
            WHEN TARGET-RECORD-FORMAT                                           
              DISPLAY '=                   (MESSAGES --> RECORDS)'              
            WHEN TARGET-MESSAGE-FORMAT                                          
              DISPLAY '=                   (RECORDS --> MESSAGES)'              
              DISPLAY '= QALIAS           - ', WS-QALIAS-NAME                   
              DISPLAY '= '                                                      
              DISPLAY '= <<<< MLI MESSAGE HEADER INFORMATION >>>>'              
              DISPLAY '= '                                                      
              DISPLAY '=  MESSAGE HEADER VERSION: ', WS-HEADER-VERSION-X        
                                                                                
              EVALUATE TRUE                                                     
               WHEN MSG-VERSION-1                                               
                 DISPLAY '=  MESSAGE TYPE QUALIFIER: ',                         
                          MSGHDR-TXN-TYPE           OF MLI-MSG-HEADER-V1        
                 DISPLAY '=   MESSAGE TYPE ID MAJOR: ',                         
                          MSGHDR-TXN-SUB-TYPE-MAJOR OF MLI-MSG-HEADER-V1        
                 DISPLAY '=   MESSAGE TYPE ID MINOR: ',                         
                          MSGHDR-TXN-SUB-TYPE-MINOR OF MLI-MSG-HEADER-V1        
                                                                                
                                                                                
               WHEN OTHER                                                       
                 DISPLAY '=  MESSAGE TYPE QUALIFIER: ',                         
                          MESSAGE-TYPE-QUALIFIER    OF MLI-MSG-HEADER-V2        
                 DISPLAY '=   MESSAGE TYPE ID MAJOR: ',                         
                          TYPEID-MAJOR              OF MLI-MSG-HEADER-V2        
                 DISPLAY '=   MESSAGE TYPE ID MINOR: ',                         
                          TYPEID-MINOR              OF MLI-MSG-HEADER-V2        
                 DISPLAY '=    MESSAGE TYPE VERSION: ',                         
                          MESSAGE-TYPE-VERSION-NUMBER                           
                                                    OF MLI-MSG-HEADER-V2        
                 DISPLAY '=           ENTRY USER ID: ',                         
                          ENTRY-USERID              OF MLI-MSG-HEADER-V2        
                 DISPLAY '=         ENTRY USER ROLE: ',                         
                          ENTRY-USERROLE            OF MLI-MSG-HEADER-V2        
                 DISPLAY '=          ACCESS CHANNEL: ',                         
                          ACCESS-CHANNEL            OF MLI-MSG-HEADER-V2        
                 DISPLAY '=          SECURITY LEVEL: ',                         
                          SECURITY-LEVEL            OF MLI-MSG-HEADER-V2        
                 DISPLAY '=          REPLY LANGUAGE: ',                         
                          REPLY-LANGUAGE            OF MLI-MSG-HEADER-V2        
              END-EVALUATE                                                      
                                                                                
            WHEN OTHER                                                          
              DISPLAY '=                      (UNRECOGNIZED)'                   
              PERFORM 8000-FATAL-ERROR THRU                                     
                      8000-FATAL-ERROR-EXIT                                     
           END-EVALUATE.                                                        
                                                                                
           DISPLAY '=================================================='.        
           DISPLAY '=          REFORMAT CONTROL DISPLAY ENDS         ='.        
           DISPLAY '=================================================='.        
           DISPLAY ' '.                                                         
                                                                                
       1520-DISPLAY-LEADCARD-EXIT.                                              
           EXIT.                                                                
      /                                                                         
                                                                                
                                                                                
       2000-MAIN-PROCESS.                                                       
                                                                                
      *                                                                         
      *  READ THE FIRST RECORD AND PROCESS TO THE EOF                           
      *  STOP IF FATAL ERRORS ARE ENCOUNTERED.                                  
      *                                                                         
           DISPLAY '##################################################'.        
           DISPLAY '#         REFORMAT PROCESS DISPLAY BEGINS        #'.        
           DISPLAY '##################################################'.        
           DISPLAY ' '.                                                         
                                                                                
           SET  INPUT-NOT-EOF           TO TRUE.                                
           MOVE OBTAIN-FIRST            TO WS-GAEDATSR-VERB.                    
           PERFORM 2100-READ-INPUT-FILE THRU                                    
                   2100-READ-INPUT-FILE-EXIT.                                   
                                                                                
           PERFORM 2105-REFORMAT-INPUT THRU                                     
                   2105-REFORMAT-INPUT-EXIT                                     
                     UNTIL INPUT-EOF OR FATAL-ERROR.                            
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '##################################################'.        
           DISPLAY '#          REFORMAT PROCESS DISPLAY ENDS         #'.        
           DISPLAY '##################################################'.        
                                                                                
       2000-MAIN-PROCESS-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * REFORMAT EACH INPUT RECORD                                              
      ******************************************************************        
       2105-REFORMAT-INPUT.                                                     
                                                                                
           INITIALIZE PARSER-CONTROL-AREA.                                      
           INITIALIZE PARSER-RETURN-AREA.                                       
           SET NO-ERROR        TO TRUE.                                         
                                                                                
           EVALUATE TRUE                                                        
            WHEN TARGET-MESSAGE-FORMAT                                          
                                                                                
               INITIALIZE MESSAGE-AREA                                          
                                                                                
               PERFORM 2110-CLEAR-MESSAGE THRU                                  
                       2110-CLEAR-MESSAGE-EXIT                                  
                                                                                
               IF NO-ERROR                                                      
                  MOVE PRSO-DATA-LENGTH      TO MESSAGE-DATA-LENGTH             
                  PERFORM 2120-INIT-PARSER-CNTL THRU                            
                          2120-INIT-PARSER-CNTL-EXIT                            
                                                                                
                  PERFORM 2200-CALL-PRE-EXIT-PGM THRU                           
                          2200-CALL-PRE-EXIT-PGM-EXIT                           
               END-IF                                                           
                                                                                
               IF NO-ERROR                                                      
                  SET  PARSE-REC-TO-MSG-CALL TO TRUE                            
                  PERFORM 2300-CALL-PARSER THRU                                 
                          2300-CALL-PARSER-EXIT                                 
               END-IF                                                           
                                                                                
               IF NO-ERROR                                                      
                  MOVE PRSO-DATA-LENGTH      TO MESSAGE-DATA-LENGTH             
                  PERFORM 2400-CALL-POST-EXIT-PGM THRU                          
                          2400-CALL-POST-EXIT-PGM-EXIT                          
               END-IF                                                           
                                                                                
            WHEN TARGET-RECORD-FORMAT                                           
                                                                                
               INITIALIZE RECORD-AREA                                           
                                                                                
               PERFORM 2120-INIT-PARSER-CNTL THRU                               
                       2120-INIT-PARSER-CNTL-EXIT                               
                                                                                
               PERFORM 2200-CALL-PRE-EXIT-PGM THRU                              
                       2200-CALL-PRE-EXIT-PGM-EXIT                              
                                                                                
               IF NO-ERROR                                                      
                  SET  PARSE-MSG-TO-REC-CALL TO TRUE                            
                  PERFORM 2300-CALL-PARSER THRU                                 
                          2300-CALL-PARSER-EXIT                                 
               END-IF                                                           
                                                                                
               IF NO-ERROR                                                      
                  MOVE PRSO-DATA-LENGTH      TO RECORD-DATA-LENGTH              
                  PERFORM 2400-CALL-POST-EXIT-PGM THRU                          
                          2400-CALL-POST-EXIT-PGM-EXIT                          
               END-IF                                                           
                                                                                
           END-EVALUATE.                                                        
                                                                                
      *                                                                         
      *  IF ALL IS WELL, WRITE TO THE OUTPUT FILE                               
      *  IF INPUT CAUSED BYPASS ERROR, TAKE APPROPRIATE ERROR ACTION            
      *  IF INPUT CAUSED FATAL ERROR, STOP PROCESSING                           
      *                                                                         
           EVALUATE TRUE                                                        
            WHEN NO-ERROR                                                       
               PERFORM 2500-WRITE-OUTPUT-FILE THRU                              
                       2500-WRITE-OUTPUT-FILE-EXIT                              
                                                                                
            WHEN YES-ERROR                                                      
              EVALUATE TRUE                                                     
               WHEN ERROR-ACTION-SKIP                                           
                  DISPLAY 'ERROR ACTION TAKEN: INPUT SKIPPED'                   
                                                                                
               WHEN ERROR-ACTION-SAVE                                           
                  DISPLAY 'ERROR ACTION TAKEN: INPUT SAVED'                     
                  PERFORM 2600-WRITE-ERROR-FILE THRU                            
                          2600-WRITE-ERROR-FILE-EXIT                            
                                                                                
               WHEN ERROR-ACTION-STOP                                           
                  DISPLAY 'ERROR ACTION TAKEN: STOP PROCESSING'                 
                  PERFORM 8000-FATAL-ERROR THRU                                 
                          8000-FATAL-ERROR-EXIT                                 
                                                                                
              END-EVALUATE                                                      
                                                                                
           END-EVALUATE.                                                        
                                                                                
      *                                                                         
      *  IF NOTHING FATAL HAS HAPPENED, GET THE NEXT RECORD                     
      *                                                                         
           IF FATAL-ERROR                                                       
              CONTINUE                                                          
           ELSE                                                                 
              MOVE OBTAIN-NEXT       TO WS-GAEDATSR-VERB                        
              PERFORM 2100-READ-INPUT-FILE THRU                                 
                      2100-READ-INPUT-FILE-EXIT                                 
           END-IF.                                                              
                                                                                
                                                                                
       2105-REFORMAT-INPUT-EXIT.                                                
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * READ INPUT FILE INTO BUFFER                                             
      ******************************************************************        
       2100-READ-INPUT-FILE.                                                    
                                                                                
           MOVE INPUT-LR              TO LOGICAL-RECORD-NAME.                   
                                                                                
           EVALUATE TRUE                                                        
           WHEN TARGET-MESSAGE-FORMAT                                           
              INITIALIZE RECORD-AREA                                            
              CALL WS-GAEDATSR     USING WS-GAEDATSR-VERB                       
                                         RECORD-AREA                            
                                         ICBM-AREA                              
           WHEN TARGET-RECORD-FORMAT                                            
              INITIALIZE MESSAGE-AREA                                           
              CALL WS-GAEDATSR     USING WS-GAEDATSR-VERB                       
                                         MESSAGE-AREA                           
                                         ICBM-AREA                              
           END-EVALUATE.                                                        
                                                                                
           IF LR-NOT-FOUND                                                      
           OR KEYED-LR-NOT-FOUND                                                
              SET INPUT-EOF           TO TRUE                                   
           ELSE                                                                 
              IF NOT LR-STATUS-OK                                               
                 DISPLAY '**INPUT FILE READ ERROR: '                            
                                         PROGRAM-LINKAGE-STATUS                 
                 PERFORM 8000-FATAL-ERROR THRU                                  
                         8000-FATAL-ERROR-EXIT                                  
              ELSE                                                              
                 ADD 1                TO READ-COUNT                             
              END-IF                                                            
           END-IF.                                                              
                                                                                
                                                                                
       2100-READ-INPUT-FILE-EXIT.                                               
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * CREATE A CLEAR MESSAGE BASED ON VERSION REQUIRED.                       
      ******************************************************************        
       2110-CLEAR-MESSAGE.                                                      
                                                                                
           SET  PARSE-CLEAR-CALL         TO TRUE.                               
           SET  PRSI-SECTION-ALL         TO TRUE.                               
           SET  PRSI-NORMAL              TO TRUE.                               
           SET  PRSI-NO-OPTIONS          TO TRUE.                               
           MOVE LENGTH OF MLI-MSG-HEADER TO PRSI-MAX-LENGTH-AREA1.              
           MOVE LENGTH OF MESSAGE-BUFFER TO PRSI-MAX-LENGTH-AREA3.              
                                                                                
           EVALUATE TRUE                                                        
           WHEN MSG-VERSION-1                                                   
              MOVE MLI-MSG-HEADER-V1     TO MLI-MSG-HEADER                      
           WHEN OTHER                                                           
              MOVE MLI-MSG-HEADER-V2     TO MLI-MSG-HEADER                      
           END-EVALUATE.                                                        
                                                                                
           CALL WS-PARSER             USING PARSER-CONTROL-AREA                 
                                            MLI-MSG-HEADER                      
                                            MESSAGE-BUFFER                      
                                            PARSER-RETURN-AREA.                 
                                                                                
           PERFORM 2350-CHECK-PARSER-CALL THRU                                  
                   2350-CHECK-PARSER-CALL-EXIT.                                 
                                                                                
       2110-CLEAR-MESSAGE-EXIT.                                                 
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * DEFAULT/INITIALIZE PARSER CONTROL AREA VALUES                           
      ******************************************************************        
       2120-INIT-PARSER-CNTL.                                                   
                                                                                
           MOVE WS-COPYBOOK-NAME        TO PRSI-STRUCTURE-NAME1.                
           MOVE WS-SECTION              TO PRSI-SECTION.                        
           MOVE WS-SECTION-OCCUR-NUM    TO PRSI-SECTION-OCCUR.                  
           MOVE WS-RUN-OPTION           TO PRSI-RUN-OPTION.                     
           MOVE WS-CHAR-OPTION          TO PRSI-CHAR-OPTIONS.                   
           MOVE WS-OCCURS-OPTION        TO PRSI-OCCURS-OPTION.                  
                                                                                
       2120-INIT-PARSER-CNTL-EXIT.                                              
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * CALL THE PRE-PROCESS EXIT PGM (IF ONE IS PROVIDED)                      
      ******************************************************************        
       2200-CALL-PRE-EXIT-PGM.                                                  
                                                                                
           IF WS-PRE-EXIT-PGM = LOW-VALUES OR SPACES                            
              CONTINUE                                                          
           ELSE                                                                 
              CALL WS-PRE-EXIT-PGM  USING MESSAGE-AREA                          
                                          RECORD-AREA                           
                                          PARSER-CONTROL-AREA                   
                                          PARSER-RETURN-AREA                    
                                          WS-EXIT-RETURN-CODE                   
                                                                                
              EVALUATE TRUE                                                     
               WHEN EXIT-RET-OK                                                 
                 CONTINUE                                                       
BM002          WHEN EXIT-RET-WARNING AND MASK-EXITS                             
BM002            ADD 1 TO CNT-PRE-WARNINGS                                      
               WHEN EXIT-RET-WARNING                                            
                 DISPLAY '?? WARNINGS ENCOUNTERED IN PREPROCESSOR: '            
                          WS-PRE-EXIT-PGM                                       
BM002            DISPLAY '??  SEQUENCE ID: ' READ-COUNT-DISPLAY                 
BM002                     '       RETCODE: ' WS-EXIT-RETURN-CODE                
BM002          WHEN EXIT-RET-ERROR AND MASK-EXITS                               
BM002            ADD 1 TO CNT-PRE-ERRORS                                        
BM002            SET YES-ERROR              TO TRUE                             
               WHEN EXIT-RET-ERROR                                              
                 DISPLAY '## ERRORS   ENCOUNTERED IN PREPROCESSOR: '            
                          WS-PRE-EXIT-PGM                                       
BM002            DISPLAY ' ## SEQUENCE ID: ' READ-COUNT-DISPLAY                 
BM002                     '       RETCODE: ' WS-EXIT-RETURN-CODE                
                 SET YES-ERROR              TO TRUE                             
BM002          WHEN MASK-EXITS                                                  
BM002            SET FATAL-ERROR            TO TRUE                             
BM002            ADD 1 TO CNT-PRE-FATALS                                        
               WHEN OTHER                                                       
                 DISPLAY '## FATAL ERROR DETECTED IN PREPROCESSOR: '            
                          WS-PRE-EXIT-PGM                                       
BM002            DISPLAY ' ## SEQUENCE ID: ' READ-COUNT-DISPLAY                 
BM002                     '       RETCODE: ' WS-EXIT-RETURN-CODE                
                 SET FATAL-ERROR            TO TRUE                             
              END-EVALUATE                                                      
                                                                                
              MOVE WS-EXIT-RETURN-CODE      TO WS-RETURN-CODE                   
              PERFORM 9000-SET-RETURN-CODE THRU                                 
                      9000-SET-RETURN-CODE-EXIT                                 
                                                                                
              MOVE WS-PRE-EXIT-PGM          TO WS-PARSE-ERR-PGM                 
              PERFORM 2360-DISPLAY-PARSE-ERROR THRU                             
                      2360-DISPLAY-PARSE-ERROR-EXIT                             
           END-IF.                                                              
                                                                                
       2200-CALL-PRE-EXIT-PGM-EXIT.                                             
           EXIT.                                                                
      /                                                                         
                                                                        00109100
      ******************************************************************        
      * CALL THE PARSER TO UPDATE THE MESSAGE FROM A RECORD STRUCTURE.          
      ******************************************************************        
       2300-CALL-PARSER.                                                        
                                                                                
           MOVE LENGTH OF MESSAGE-BUFFER    TO PRSI-MAX-LENGTH-AREA1.           
           MOVE LENGTH OF RECORD-BUFFER     TO PRSI-MAX-LENGTH-AREA3.           
                                                                                
           CALL WS-PARSER                USING PARSER-CONTROL-AREA              
                                               MESSAGE-BUFFER                   
                                               RECORD-BUFFER                    
                                               PARSER-RETURN-AREA.              
                                                                                
           PERFORM 2350-CHECK-PARSER-CALL THRU                                  
                   2350-CHECK-PARSER-CALL-EXIT.                                 
                                                                                
                                                                                
       2300-CALL-PARSER-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * CHECK RESULT OF PARSER CALL                                             
      ******************************************************************        
       2350-CHECK-PARSER-CALL.                                                  
                                                                                
           IF PRSO-CRITICAL-ERROR                                               
              SET YES-ERROR                 TO TRUE                             
           END-IF.                                                              
                                                                                
           MOVE PRSO-ERROR-IND              TO WS-RETURN-CODE.                  
           PERFORM 9000-SET-RETURN-CODE THRU                                    
                   9000-SET-RETURN-CODE-EXIT.                                   
                                                                                
           MOVE WS-PARSER                   TO WS-PARSE-ERR-PGM.                
           PERFORM 2360-DISPLAY-PARSE-ERROR THRU                                
                   2360-DISPLAY-PARSE-ERROR-EXIT.                               
                                                                                
           MOVE PRSO-INDEX-HANDLE           TO PRSI-INDEX-HANDLE.               
                                                                                
       2350-CHECK-PARSER-CALL-EXIT.                                             
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * DISPLAY PARSER ERROR TABLE INFO                                         
      ******************************************************************        
       2360-DISPLAY-PARSE-ERROR.                                                
                                                                                
           EVALUATE TRUE                                                        
            WHEN PRSO-NO-ERROR                                                  
              SET PARSE-NO-ERRORS TO TRUE                                       
BM002       WHEN PRSO-NOT-FOUND          AND MASK-PARSER-WARNS                  
BM002         SET PARSE-WARNINGS TO TRUE                                        
BM002         ADD 1 TO CNT-NOT-FOUNDS                                           
BM002       WHEN PRSO-NON-CRITICAL-ERROR AND MASK-PARSER-WARNS                  
BM002         SET PARSE-WARNINGS TO TRUE                                        
BM002         ADD 1 TO CNT-NON-CRITICALS                                        
            WHEN PRSO-NOT-FOUND                                                 
            WHEN PRSO-NON-CRITICAL-ERROR                                        
              SET PARSE-WARNINGS TO TRUE                                        
              DISPLAY '-----------'                                             
                      '------------------------------------------------'        
              DISPLAY '-  PARSER WARNINGS: ' WS-PARSE-ERR-PGM                   
                      '       SEQUENCE ID: ' READ-COUNT-DISPLAY                 
BM002       WHEN PRSO-CRITICAL-ERROR    AND MASK-PARSER-ERRORS                  
BM002         SET PARSE-ERRORS   TO TRUE                                        
BM002         ADD 1 TO CNT-CRITICALS                                            
            WHEN PRSO-CRITICAL-ERROR                                            
              SET PARSE-ERRORS   TO TRUE                                        
              DISPLAY '-----------'                                             
                      '------------------------------------------------'        
              DISPLAY '-    PARSER ERRORS: ' WS-PARSE-ERR-PGM                   
                      '       SEQUENCE ID: ' READ-COUNT-DISPLAY                 
           END-EVALUATE.                                                        
                                                                                
BM002      IF PRSO-ERROR-COUNT = ZERO OR MASK-PARSER-DETAIL                     
              CONTINUE                                                          
           ELSE                                                                 
              DISPLAY '-----------'                                             
                      '------------------------------------------------'        
              DISPLAY '          '                                              
                      'GRP-LVL-1    GRP-LVL-2    GRP-LVL-3  PACKET  ERR'        
              DISPLAY '          '                                              
                      ' (ID,OCC)     (ID,OCC)     (ID,OCC)    ID   CODE'        
              DISPLAY '-----------'                                             
                      '------------------------------------------------'        
              PERFORM  VARYING SUB FROM 1 BY 1                                  
                       UNTIL   SUB > PRSO-ERROR-COUNT                           
                 DISPLAY '  '  PARSE-ERROR-IND '(' SUB ')'                      
                         '  (' PRSO-LVL1-ID (SUB) ','                           
                                PRSO-LVL1-OCCUR (SUB) ')'                       
                         '  (' PRSO-LVL2-ID (SUB) ','                           
                                PRSO-LVL2-OCCUR (SUB) ')'                       
                         '  (' PRSO-LVL3-ID (SUB) ','                           
                                PRSO-LVL3-OCCUR (SUB) ')'                       
                         '  '  PRSO-ERROR-PACKET (SUB) '  '                     
                         '  '  PRSO-ERROR-CODE (SUB)                            
                 IF PRSO-MESSAGE-VERSION < 2                                    
                    DISPLAY '        ' PRSO-V1-MESSAGE (SUB)                    
                 END-IF                                                         
              END-PERFORM                                                       
              DISPLAY '-----------'                                             
                      '------------------------------------------------'        
           END-IF.                                                              
                                                                                
       2360-DISPLAY-PARSE-ERROR-EXIT.                                           
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * CALL THE POSTPROCESSOR EXIT PGM (IF ONE IS PROVIDED) TO UPDATE          
      * THE CURRENT MESSAGE FOUND IN INPUT-AREA.                                
      ******************************************************************        
       2400-CALL-POST-EXIT-PGM.                                                 
                                                                                
           IF WS-POST-EXIT-PGM = LOW-VALUES OR SPACES                           
              CONTINUE                                                          
           ELSE                                                                 
              CALL WS-POST-EXIT-PGM USING MESSAGE-AREA                          
                                          RECORD-AREA                           
                                          PARSER-CONTROL-AREA                   
                                          PARSER-RETURN-AREA                    
                                          WS-EXIT-RETURN-CODE                   
                                                                                
              EVALUATE TRUE                                                     
               WHEN EXIT-RET-OK                                                 
                 CONTINUE                                                       
BM002          WHEN EXIT-RET-WARNING AND MASK-EXITS                             
BM002            ADD 1 TO CNT-POST-WARNINGS                                     
               WHEN EXIT-RET-WARNING                                            
                 DISPLAY '?? WARNINGS ENCOUNTERED IN POSTPROCESSOR: '           
                          WS-POST-EXIT-PGM                                      
BM002            DISPLAY ' ?? SEQUENCE ID: ' READ-COUNT-DISPLAY                 
BM002                     '       RETCODE: ' WS-EXIT-RETURN-CODE                
BM002          WHEN EXIT-RET-ERROR AND MASK-EXITS                               
BM002            SET YES-ERROR              TO TRUE                             
BM002            ADD 1 TO CNT-POST-ERRORS                                       
               WHEN EXIT-RET-ERROR                                              
                 DISPLAY '## ERRORS   ENCOUNTERED IN PREPROCESSOR: '            
                          WS-POST-EXIT-PGM                                      
BM002            DISPLAY ' ## SEQUENCE ID: ' READ-COUNT-DISPLAY                 
BM002                     '       RETCODE: ' WS-EXIT-RETURN-CODE                
                 SET YES-ERROR              TO TRUE                             
BM002          WHEN MASK-EXITS                                                  
BM002            SET FATAL-ERROR            TO TRUE                             
BM002            ADD 1 TO CNT-POST-FATALS                                       
               WHEN OTHER                                                       
                 DISPLAY '## FATAL ERROR DETECTED IN POSTPROCESSOR: '           
                          WS-POST-EXIT-PGM                                      
BM002            DISPLAY ' ## SEQUENCE ID: ' READ-COUNT-DISPLAY                 
BM002                     '       RETCODE: ' WS-EXIT-RETURN-CODE                
                 SET FATAL-ERROR            TO TRUE                             
              END-EVALUATE                                                      
                                                                                
              MOVE WS-EXIT-RETURN-CODE      TO WS-RETURN-CODE                   
              PERFORM 9000-SET-RETURN-CODE THRU                                 
                      9000-SET-RETURN-CODE-EXIT                                 
                                                                                
              MOVE WS-POST-EXIT-PGM         TO WS-PARSE-ERR-PGM                 
              PERFORM 2360-DISPLAY-PARSE-ERROR THRU                             
                      2360-DISPLAY-PARSE-ERROR-EXIT                             
           END-IF.                                                              
                                                                                
       2400-CALL-POST-EXIT-PGM-EXIT.                                            
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * WRITE BUFFER TO OUTPUT FILE                                             
      ******************************************************************        
       2500-WRITE-OUTPUT-FILE.                                                  
                                                                                
           MOVE STORE-LR          TO WS-GAEDATSR-VERB.                          
           MOVE OUTPUT-LR         TO LOGICAL-RECORD-NAME.                       
                                                                                
           EVALUATE TRUE                                                        
           WHEN TARGET-MESSAGE-FORMAT                                           
                                                                                
              ADD  4                  TO MESSAGE-DATA-LENGTH                    
              CALL WS-GAEDATSR     USING WS-GAEDATSR-VERB                       
                                         MESSAGE-AREA                           
                                         ICBM-AREA                              
                                                                                
           WHEN TARGET-RECORD-FORMAT                                            
                                                                                
              ADD  4                  TO RECORD-DATA-LENGTH                     
              CALL WS-GAEDATSR     USING WS-GAEDATSR-VERB                       
                                         RECORD-AREA                            
                                         ICBM-AREA                              
           END-EVALUATE.                                                        
                                                                                
           IF NOT LR-STATUS-OK                                                  
              DISPLAY '**OUTPUT FILE WRITE ERROR: '                             
                                     PROGRAM-LINKAGE-STATUS                     
              PERFORM 8000-FATAL-ERROR THRU                                     
                      8000-FATAL-ERROR-EXIT                                     
           END-IF.                                                              
                                                                                
       2500-WRITE-OUTPUT-FILE-EXIT.                                             
           EXIT.                                                        00110800
      /                                                                 00110900
                                                                                
      ******************************************************************        
      * WRITE INPUT BUFFER TO ERROR FILE                                        
      ******************************************************************        
       2600-WRITE-ERROR-FILE.                                                   
                                                                                
           MOVE STORE-LR          TO WS-GAEDATSR-VERB.                          
           MOVE ERROR-LR          TO LOGICAL-RECORD-NAME.                       
                                                                                
           EVALUATE TRUE                                                        
           WHEN TARGET-MESSAGE-FORMAT                                           
                                                                                
              CALL WS-GAEDATSR     USING WS-GAEDATSR-VERB                       
                                         RECORD-AREA                            
                                         ICBM-AREA                              
                                                                                
           WHEN TARGET-RECORD-FORMAT                                            
                                                                                
              CALL WS-GAEDATSR     USING WS-GAEDATSR-VERB                       
                                         MESSAGE-AREA                           
                                         ICBM-AREA                              
           END-EVALUATE.                                                        
                                                                                
           IF NOT LR-STATUS-OK                                                  
              DISPLAY '**ERROR FILE WRITE ERROR: '                              
                                     PROGRAM-LINKAGE-STATUS                     
              PERFORM 8000-FATAL-ERROR THRU                                     
                      8000-FATAL-ERROR-EXIT                                     
           END-IF.                                                              
                                                                                
       2600-WRITE-ERROR-FILE-EXIT.                                              
           EXIT.                                                        00110800
      /                                                                 00110900
                                                                                
      ***************************************************************** 00109200
      *  THIS PARAGRAPH...                                              00109300
      *    - CLOSES ALL FILES                                           00109400
      *    - SETS FINAL RETURN CODE                                     00109400
      ***************************************************************** 00109500
       3000-FINALIZATION.                                               00109700
                                                                        00110200
           MOVE FINISH-LR         TO WS-GAEDATSR-VERB.                          
           MOVE SPACES            TO LOGICAL-RECORD-NAME.                       
           CALL WS-GAEDATSR    USING WS-GAEDATSR-VERB                           
                                     MESSAGE-AREA                               
                                     ICBM-AREA.                                 
           IF NOT LR-STATUS-OK                                                  
              DISPLAY '** FILE CLOSE ERROR: '                                   
                                     PROGRAM-LINKAGE-STATUS                     
              PERFORM 8000-FATAL-ERROR THRU                                     
                      8000-FATAL-ERROR-EXIT                                     
           END-IF.                                                              
                                                                                
           IF MASK-EXITS                                                        
BM002          MOVE  CNT-PRE-WARNINGS TO PRT-CNT-1                              
BM002          MOVE  CNT-PRE-ERRORS   TO PRT-CNT-2                              
BM002          MOVE  CNT-PRE-FATALS   TO PRT-CNT-3                              
BM002          DISPLAY ' PRE EXITS MASKED: '                                    
BM002                         '  WARNINGS:' PRT-CNT-1                           
BM002                         '    ERRORS:' PRT-CNT-2                           
BM002                         '     FATAL:' PRT-CNT-3                           
BM002          MOVE  CNT-POST-WARNINGS TO PRT-CNT-1                             
BM002          MOVE  CNT-POST-ERRORS   TO PRT-CNT-2                             
BM002          MOVE  CNT-POST-FATALS   TO PRT-CNT-3                             
BM002          DISPLAY 'POST EXITS MASKED: '                                    
BM002                         '  WARNINGS:' PRT-CNT-1                           
BM002                         '    ERRORS:' PRT-CNT-2                           
BM002                         '     FATAL:' PRT-CNT-3                           
BM002      END-IF.                                                              
           IF   CNT-NOT-FOUNDS    > 0                                           
             OR CNT-NON-CRITICALS > 0                                           
             OR CNT-CRITICALS     > 0                                           
           THEN                                                                 
BM002          MOVE CNT-NOT-FOUNDS     TO PRT-CNT-1                             
BM002          MOVE CNT-NON-CRITICALS  TO PRT-CNT-2                             
BM002          MOVE CNT-CRITICALS      TO PRT-CNT-3                             
BM002          DISPLAY 'PARSER ERRORS MASKED: '                                 
BM002                         'NOT FOUND:' PRT-CNT-1                            
BM002                    '  NON CRITICAL:' PRT-CNT-2                            
BM002                      '    CRITICAL:' PRT-CNT-3                            
           END-IF.                                                              
BM002      IF ALLOWED-CONDITION                                                 
BM002         DISPLAY 'HIGHEST ALLOWED CODE: ' WS-ALLOWED-CONDITION             
BM002         IF WS-FINAL-RETURN-CODE NOT >    WS-ALLOWED-CONDITION             
BM002            DISPLAY 'INITIAL CONDITION CODE: ' WS-FINAL-RETURN-CODE        
BM002            MOVE ZERO TO WS-FINAL-RETURN-CODE                              
BM002         END-IF                                                            
BM002      END-IF.                                                      00110500
BM002      DISPLAY 'FINAL CONDITION CODE: ' WS-FINAL-RETURN-CODE                
           MOVE WS-FINAL-RETURN-CODE   TO RETURN-CODE.                          
       3000-FINALIZATION-EXIT.                                          00109700
           EXIT.                                                        00110800
      /                                                                 00110900
                                                                                
      ******************************************************************        
      * FATAL ERROR HANDLING                                                    
      ******************************************************************        
       8000-FATAL-ERROR.                                                        
                                                                                
           SET FATAL-ERROR          TO TRUE.                                    
           SET PROCESS-FAILED       TO TRUE.                                    
                                                                                
           PERFORM 9000-SET-RETURN-CODE THRU                                    
                   9000-SET-RETURN-CODE-EXIT.                                   
                                                                                
       8000-FATAL-ERROR-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
                                                                                
      ******************************************************************        
      * SET FINAL COMPLETION CODE                                               
      ******************************************************************        
       9000-SET-RETURN-CODE.                                                    
                                                                                
           IF WS-FINAL-RETURN-CODE < WS-RETURN-CODE                             
              MOVE WS-RETURN-CODE TO WS-FINAL-RETURN-CODE                       
           END-IF.                                                              
                                                                                
       9000-SET-RETURN-CODE-EXIT.                                               
           EXIT.                                                                
      /                                                                         
