       ID DIVISION.                                                             
       PROGRAM-ID.    BRSI0100.                                                 
      ******************************************************************        
      *  COBOL/OS390                                                   *        
      *                                                                *        
      *  PROGRAM NAME: BRSI0100                                        *        
      *  AUTHOR      : TARIQ NOOR                                      *        
      *  DATE WRITTEN: AUGUST 26, 2004.                                *        
      *  TYPE        : BATCH PROGRAM                                   *        
      *                                                                *        
      *  PROJECT     : INTEGRATION (BRS).                              *        
      *                                                                *        
      *  DESCRIPTION : THIS PROGRAM PRINTS DETAIL AND SUMMARY CONTROL  *        
      *                REPORTS FOR ISSUED CHEQUES DATA FOR BRS FROM:   *        
      *                LH CLAIMS-GROUP                                 *        
      *                LH CLAIMS-AMF                                   *        
      *                MARINER                                         *        
      *                NAVIGATOR                                       *        
      *                EXCLAIMS                                        *        
      *                HEALTHPRO                                       *        
      *                FACETS                                          *        
      *                IDEAS TPA                                       *        
      *                EXCLAIMS TPA                                    *        
      *                                                                *        
      ******************************************************************        
      *  MODIFICATION LOG                                              *        
      ******************************************************************        
      *                                                                *        
      * USERID    DATE      REL/LOG   DESCRIPTION                      *        
      * --------  --------  --------  -------------------------------- *        
      * NOORTAR   20040826            INITIAL VERSION.                 *        
      *                                                                *        
      * -------   20081006            COMPILER UPGRADE PROJECT         *        
      *                                                                *        
      ******************************************************************        
      *  ENVIRONMENT DIVISION                                          *        
      *                                                                *        
      ******************************************************************        
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-3090 WITH DEBUGGING MODE.                           
      *                                                                         
      ******************************************************************        
      *                                                                *        
      *  INPUT OUTPUT SECTION                                          *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
                                                                                
      * INPUT BRS ISSUE FILE                                                    
           SELECT BRS-ISSUE-FILE                                                
                   ASSIGN              TO BRSISSU.                              
                                                                                
      * INPUT DATE FILE                                                         
           SELECT DATE-CARD-FILE                                                
                   ASSIGN              TO DATECRD.                              
                                                                                
      * OUTPUT DETAIL CONTROL REPORT                                            
           SELECT BRS-DETAIL-REPORT                                             
                   ASSIGN              TO BRSDRPT.                              
                                                                                
      * OUTPUT SUMMARY CONTROL REPORT                                           
           SELECT BRS-SUMMARY-REPORT                                            
                   ASSIGN              TO BRSSRPT.                              
                                                                                
      ******************************************************************        
      *  END OF INPUT OUTPUT SECTION                                   *        
      ******************************************************************        
      *                                                                         
       DATA DIVISION.                                                           
      *                                                                         
      ******************************************************************        
      *  FILE SECTION                                                  *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
       FILE SECTION.                                                            
      *                                                                         
       FD  BRS-ISSUE-FILE                                                       
              RECORD    CONTAINS 160 CHARACTERS                                 
              BLOCK     CONTAINS 0   CHARACTERS                                 
              RECORDING MODE     IS  F.                                         
                                                                                
       01  BRS-ISSUE-REC                    PIC  X(160).                        
                                                                                
       FD  DATE-CARD-FILE                                                       
              RECORD    CONTAINS 200 CHARACTERS                                 
              BLOCK     CONTAINS 0   CHARACTERS                                 
              RECORDING MODE     IS  F.                                         
                                                                                
       01  DATE-CARD-REC                    PIC  X(200).                        
                                                                                
       FD  BRS-DETAIL-REPORT                                                    
              RECORD    CONTAINS 132 CHARACTERS                                 
              BLOCK     CONTAINS 0   CHARACTERS                                 
              RECORDING MODE     IS  F.                                         
                                                                                
       01  BRS-DTL-REC                      PIC  X(132).                        
                                                                                
       FD  BRS-SUMMARY-REPORT                                                   
              RECORD    CONTAINS 132 CHARACTERS                                 
              BLOCK     CONTAINS 0   CHARACTERS                                 
              RECORDING MODE     IS  F.                                         
                                                                                
       01  BRS-SUM-REC                      PIC  X(132).                        
                                                                                
      ******************************************************************        
      *  END OF FILE SECTION                                           *        
      ******************************************************************        
                                                                                
      ******************************************************************        
      *                                                                *        
      *  WORKING STORAGE SECTION                                       *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
       WORKING-STORAGE SECTION.                                                 
      *                                                                         
       01  WC-START-WORKING-STORAGE.                                            
           05  FILLER                       PIC X(80)  VALUE                    
           '### BRSI0100 WORKING STORAGE STARTS HERE ###'.                      
                                                                                
       01  WC-CONSTANTS.                                                        
           05  WC-THIS-PGM                 PIC  X(08) VALUE 'BRSI0100'.         
           05  WC-DRAFT-MAX                PIC S9(02) VALUE +8.                 
           05  WC-CURRENT                  PIC  X(08) VALUE 'CURRENT '.         
           05  WC-RC-CODE008               PIC  9(03) VALUE 008.                
           05  WC-RC-CODE016               PIC  9(03) VALUE 016.                
                                                                                
       01  WV-WORKING-VARIABLES.                                                
           05  WV-DRAFT-NUM-DIG2           PIC  X.                              
           05  WV-DRAFT-NUM.                                                    
               10  WV-DRAFT-NUM8           PIC  X     OCCURS 8 TIMES.           
           05  WV-MICR-FIRST               PIC  X(10) VALUE SPACES.             
           05  WV-MICR-LAST                PIC  X(10) VALUE SPACES.             
           05  WV-STOR-SRC-CD              PIC  X(03) VALUE SPACES.             
           05  WV-STOR-BNK-ACT             PIC  X(05) VALUE SPACES.             
                                                                                
           05  WV-DATE-X                   PIC  9(7).                           
           05  WV-DATE-Y  REDEFINES  WV-DATE-X.                                 
               10  WV-DATE-Y-C             PIC  9.                              
               10  WV-DATE-Y-YYMMDD        PIC  9(6).                           
           05  WV-DATE-Z.                                                       
               10  WV-DATE-Z-CC            PIC  99   VALUE 20.                  
               10  WV-DATE-Z-YYMMDD        PIC  9(6).                           
           05  WV-RUN-DATE.                                                     
               10  WV-RD-YY                PIC  XX.                             
               10  WV-RD-MM                PIC  XX.                             
               10  WV-RD-DD                PIC  XX.                             
           05  WV-RUN-TIME.                                                     
               10  WV-RT-H                 PIC  99.                             
               10  WV-RT-M                 PIC  99.                             
               10  WV-RT-S                 PIC  99.                             
                                                                                
       01  WA-ACCUMULATORS.                                                     
           05  WA-D-PAGE                   PIC  99   VALUE ZEROS.               
           05  WA-S-PAGE                   PIC  99   VALUE ZEROS.               
           05  WA-DLINE-CNT                PIC  99   VALUE 99.                  
           05  WA-SLINE-CNT                PIC  99   VALUE 99.                  
           05  WA-DTOT-CHEQUES             PIC  9(6) VALUE ZEROS.               
           05  WA-DTOT-CHEQ-AMT            PIC  9(10)V99 VALUE ZEROS.           
           05  WA-DRTOT-CHEQUES            PIC  9(7) VALUE ZEROS.               
           05  WA-DRTOT-CHEQ-AMT           PIC  9(11)V99 VALUE ZEROS.           
           05  WA-STOT-CHEQUES             PIC  9(5) VALUE ZEROS.               
           05  WA-STOT-CHEQ-AMT            PIC  9(09)V99 VALUE ZEROS.           
           05  WA-TTOT-CHEQUES             PIC  9(6) VALUE ZEROS.               
           05  WA-TTOT-CHEQ-AMT            PIC  9(12)V99 VALUE ZEROS.           
                                                                                
       01  WS-SWITCHES.                                                         
           05  WS-ISSUE-EOF                PIC  X      VALUE 'N'.               
               88  WS-ISSU-EOF-YES         VALUE 'Y'.                           
                                                                                
       01  WX-INDEXES.                                                          
      *    05  WX-CNT                  PIC S9(02) COMP.                         
           05  WX-BRS-APPLX.                                                    
               10  FILLER         PIC  X(36)                                    
      ***********************DETAIL***TOTAL*********************                
                   VALUE 'LG PGLPR003 PGLPR004 LH CLAIMS-GROUP'.                
               10  FILLER         PIC  X(36)                                    
                   VALUE 'LA PGLPR005 PGLPR006 LH CLAIMS-AMF  '.                
               10  FILLER         PIC  X(36)                                    
                   VALUE 'MR PGLPR007 PGLPR008 MARINER        '.                
               10  FILLER         PIC  X(36)                                    
                   VALUE 'NV PGLPR010 PGLPR011 NAVIGATOR      '.                
               10  FILLER         PIC  X(36)                                    
                   VALUE 'XC PGLPR013 PGLPR014 EXCLAIMS       '.                
               10  FILLER         PIC  X(36)                                    
                   VALUE 'HP PGLPR019 PGLPR020 HEALTHPRO      '.                
               10  FILLER         PIC  X(36)                                    
                   VALUE 'FA PGLPR025 PGLPR026 FACETS         '.                
               10  FILLER         PIC  X(36)                                    
                   VALUE 'TI PGLPR027 PGLPR028 IDEAS TPA      '.                
               10  FILLER         PIC  X(36)                                    
                   VALUE 'TX PGLPR017 PGLPR018 EXCLAIMS TPA   '.                
           05  WX-BRS-APPL  REDEFINES  WX-BRS-APPLX  OCCURS 9 TIMES             
                                      INDEXED BY WX-IND.                        
               10  WX-APLCD       PIC  XX.                                      
               10  FILLER         PIC  X.                                       
               10  WX-RDS1        PIC  X(08).                                   
               10  FILLER         PIC  X.                                       
               10  WX-RDS2        PIC  X(08).                                   
               10  FILLER         PIC  X.                                       
               10  WX-BRSAPL      PIC  X(15).                                   
                                                                                
      ******************************************************************        
      *  PRINTING - DETAIL CONTROL REPORT                              *        
      ******************************************************************        
       01  DTL-HEAD-1.                                                          
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  DTL-HD1-RDS                 PIC  X(08).                          
           05  FILLER                      PIC  X     VALUE '/'.                
           05  DTL-HD1-PGM                 PIC  X(08).                          
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  DTL-HD1-DATE.                                                    
               10  FILLER                  PIC  XX    VALUE '20'.               
               10  DTL-HD1-YY              PIC  XX.                             
               10  FILLER                  PIC  X     VALUE '-'.                
               10  DTL-HD1-MM              PIC  XX.                             
               10  FILLER                  PIC  X     VALUE '-'.                
               10  DTL-HD1-DD              PIC  XX.                             
           05  FILLER                      PIC  X(02) VALUE SPACES.             
           05  DTL-HD1-TIME.                                                    
               10  DTL-HD1-H                   PIC  99.                         
               10  FILLER                  PIC  X     VALUE ':'.                
               10  DTL-HD1-M                   PIC  99.                         
               10  FILLER                  PIC  X     VALUE ':'.                
               10  DTL-HD1-S               PIC  99.                             
           05  FILLER                      PIC  X(81) VALUE SPACES.             
           05  FILLER                      PIC  X(06) VALUE 'PAGE: '.           
           05  DTL-HD1-PAGE                PIC  99.                             
       01  DTL-HEAD-2.                                                          
           05  FILLER                      PIC  X(57) VALUE SPACES.             
           05  FILLER                      PIC  X(18)                           
                                           VALUE 'MANULIFE FINANCIAL'.          
           05  FILLER                      PIC  X(57) VALUE SPACES.             
       01  DTL-HEAD-3.                                                          
           05  FILLER                      PIC  X(55) VALUE SPACES.             
           05  FILLER                      PIC  X(23)                           
                                      VALUE 'BRS FEED ISSUED CHEQUES'.          
           05  FILLER                      PIC  X(54) VALUE SPACES.             
       01  DTL-HEAD-4.                                                          
           05  FILLER                      PIC  X(46) VALUE SPACES.             
           05  FILLER                      PIC  X(27)                           
                                    VALUE 'CONTROL DETAIL REPORT FROM '.        
           05  DTL-HD4-NAME                PIC  X(15).                          
           05  FILLER                      PIC  X(41) VALUE SPACES.             
       01  DTL-HEAD-5.                                                          
           05  FILLER                      PIC  X(50) VALUE SPACES.             
           05  FILLER                      PIC  X(13)                           
                                           VALUE 'SOURCE CODE: '.               
           05  DTL-HD5-SRC                 PIC  X(03).                          
           05  FILLER                      PIC  X(08) VALUE '  DATE: '.         
           05  DTL-HD5-DATE.                                                    
               10  DTL-HD5-CCYY            PIC  X(04).                          
               10  DTL-HD5-MM              PIC  XX.                             
               10  DTL-HD5-DD              PIC  XX.                             
           05  FILLER                      PIC  X(50) VALUE SPACES.             
       01  DTLD-HEAD-1.                                                         
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(20)                           
                                           VALUE 'BNK ACCOUNT NUMBER: '.        
           05  DDH1-BNK-ACCT               PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(106) VALUE SPACES.            
       01  DTLD-HEAD-2.                                                         
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(10)                           
                                                VALUE '  CHEQUE  '.             
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(08) VALUE ' CHEQUE '.         
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(14)                           
                                                VALUE '    CHEQUE    '.         
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(13)                           
                                                VALUE '   CHEQUE    '.          
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(12)                           
                                                VALUE ' POLICY     '.           
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(10)                           
                                                VALUE '   CERT   '.             
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(10)                           
                                                VALUE '   DIV.   '.             
           05  FILLER                      PIC  X(24) VALUE SPACES.             
       01  DTLD-HEAD-3.                                                         
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(10)                           
                                                VALUE '  NUMBER  '.             
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(08) VALUE '  DATE  '.         
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(14)                           
                                                VALUE '    AMOUNT    '.         
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(13)                           
                                                VALUE '    REF#     '.          
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(12)                           
                                                VALUE ' NUMBER     '.           
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(10)                           
                                                VALUE '  NUMBER  '.             
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(10)                           
                                                VALUE '  NUMBER  '.             
           05  FILLER                      PIC  X(24) VALUE SPACES.             
       01  DTLD-LINE.                                                           
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  DTLD-CHQ-NUM                PIC  X(10).                          
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  DTLD-CHQ-DATE               PIC  X(08).                          
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  DTLD-CHEQ-AMOUNT            PIC  ZZZ,ZZZ,ZZ9.99.                 
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  DTLD-CHEQ-REF               PIC  X(13).                          
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  DTLD-POLICY                 PIC  X(12).                          
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  DTLD-CERT-NUM               PIC  X(10).                          
           05  FILLER                      PIC  X(08) VALUE SPACES.             
           05  DTLD-DIV-NUM                PIC  X(03).                          
           05  FILLER                      PIC  X(28) VALUE SPACES.             
       01  DTLDASH-LINE.                                                        
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(26) VALUE SPACES.             
           05  FILLER                      PIC  X(16) VALUE ALL '-'.            
           05  FILLER                      PIC  X(89) VALUE SPACES.             
       01  DTL-TOTAL-LINE.                                                      
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(26) VALUE SPACES.             
           05  DTL-TOT-CHEQ-AMOUNT         PIC  Z,ZZZ,ZZZ,ZZ9.99.               
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  FILLER                      PIC  X(32)                           
                           VALUE 'TOTAL NUMBER OF ISSUED CHEQUES: '.            
           05  DTL-TOT-CHEQ-ISSU           PIC  ZZZ,ZZ9.                        
           05  FILLER                      PIC  X(45) VALUE SPACES.             
       01  DTL-RPT-TOTAL-LINE.                                                  
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(11) VALUE SPACES.             
           05  FILLER                      PIC  X(14)                           
                                           VALUE 'REPORT TOTAL: '.              
           05  DTL-RPT-TOT-AMOUNT          PIC  ZZ,ZZZ,ZZZ,ZZ9.99.              
           05  FILLER                      PIC  X(20) VALUE SPACES.             
           05  FILLER                      PIC  X(15)                           
                                           VALUE 'REPORT TOTALS: '.             
           05  DTL-RPT-TOT-CHEQ            PIC  Z,ZZZ,ZZ9.                      
           05  FILLER                      PIC  X(45) VALUE SPACES.             
                                                                                
      ******************************************************************        
      *  PRINTING - SUMMARY CONTROL REPORT                             *        
      ******************************************************************        
       01  SUM-HEAD-1.                                                          
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  HD1-RDS                     PIC  X(08).                          
           05  FILLER                      PIC  X     VALUE '/'.                
           05  HD1-PGM                     PIC  X(08).                          
           05  FILLER                      PIC  X(05) VALUE SPACES.             
           05  HD1-DATE.                                                        
               10  HD1-CC                  PIC  XX    VALUE '20'.               
               10  HD1-YY                  PIC  XX.                             
               10  FILLER                  PIC  X     VALUE '-'.                
               10  HD1-MM                  PIC  XX.                             
               10  FILLER                  PIC  X     VALUE '-'.                
               10  HD1-DD                  PIC  XX.                             
           05  FILLER                      PIC  X(02) VALUE SPACES.             
           05  HD1-TIME.                                                        
               10  HD1-H                   PIC  99.                             
               10  FILLER                  PIC  X     VALUE ':'.                
               10  HD1-M                   PIC  99.                             
               10  FILLER                  PIC  X     VALUE ':'.                
               10  HD1-S                   PIC  99.                             
           05  FILLER                      PIC  X(29) VALUE SPACES.             
           05  FILLER                      PIC  X(06) VALUE 'PAGE: '.           
           05  HD1-PAGE                    PIC  99.                             
       01  SUM-HEAD-2.                                                          
           05  FILLER                      PIC  X(31) VALUE SPACES.             
           05  FILLER                      PIC  X(18)                           
                                           VALUE 'MANULIFE FINANCIAL'.          
           05  FILLER                      PIC  X(31) VALUE SPACES.             
       01  SUM-HEAD-3.                                                          
           05  FILLER                      PIC  X(29) VALUE SPACES.             
           05  FILLER                      PIC  X(23)                           
                                      VALUE 'BRS FEED ISSUED CHEQUES'.          
           05  FILLER                      PIC  X(28) VALUE SPACES.             
       01  SUM-HEAD-4.                                                          
           05  FILLER                      PIC  X(25) VALUE SPACES.             
           05  FILLER                      PIC  X(26)                           
                                    VALUE 'CONTROL TOTAL REPORT FROM '.         
           05  HD4-NAME                    PIC  X(15).                          
           05  FILLER                      PIC  X(14) VALUE SPACES.             
       01  SUM-HEAD-5.                                                          
           05  FILLER                      PIC  X(32) VALUE SPACES.             
           05  FILLER                      PIC  X(06) VALUE 'DATE: '.           
           05  HD5-DATE.                                                        
               10  HD5-CCYY                PIC  X(04).                          
               10  HD5-MM                  PIC  XX.                             
               10  HD5-DD                  PIC  XX.                             
           05  FILLER                      PIC  X(32) VALUE SPACES.             
       01  SUMD-HEAD-1.                                                         
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(13)                           
                                           VALUE 'SOURCE CODE: '.               
           05  SDH1-SRC-CODE                PIC  X(03).                         
           05  FILLER                      PIC  X(63) VALUE SPACES.             
       01  SUMD-HEAD-2.                                                         
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(16)                           
                                           VALUE 'BNK ACCOUNT     '.            
           05  FILLER                      PIC  X(20)                           
                                           VALUE '    NUMBER OF       '.        
           05  FILLER                      PIC  X(22) VALUE SPACES.             
           05  FILLER                      PIC  X(09)                           
                                           VALUE 'AMOUNT OF'.                   
           05  FILLER                      PIC  X(12) VALUE SPACES.             
       01  SUMD-HEAD-3.                                                         
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(16)                           
                                           VALUE 'NUMBER          '.            
           05  FILLER                      PIC  X(20)                           
                                           VALUE '    CHEQUES         '.        
           05  FILLER                      PIC  X(22) VALUE SPACES.             
           05  FILLER                      PIC  X(09)                           
                                           VALUE 'CHEQUES  '.                   
           05  FILLER                      PIC  X(12) VALUE SPACES.             
       01  SUMD-LINE.                                                           
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  SUMD-BNK-ACC-NUM            PIC  X(05).                          
           05  FILLER                      PIC  X(18) VALUE SPACES.             
           05  SUMD-NUM-OF-CHEQ            PIC  ZZ,ZZZ.                         
           05  FILLER                      PIC  X(25) VALUE SPACES.             
           05  SUMD-CHEQ-AMOUNT            PIC  $$$,$$$,$$9.99.                 
           05  FILLER                      PIC  X(34) VALUE SPACES.             
       01  SUMDASH-LINE.                                                        
           05  FILLER                      PIC  X(24) VALUE SPACES.             
           05  FILLER                      PIC  X(06) VALUE ALL '-'.            
           05  FILLER                      PIC  X(25) VALUE SPACES.             
           05  FILLER                      PIC  X(14) VALUE ALL '-'.            
           05  FILLER                      PIC  X(34) VALUE SPACES.             
       01  SUM-TOTAL-LINE.                                                      
           05  FILLER                      PIC  X(15) VALUE SPACES.             
           05  FILLER                      PIC  X(08) VALUE 'TOTALS: '.         
           05  SUM-TOT-NUM-OF-CHEQ         PIC  ZZZ,ZZZ.                        
           05  FILLER                      PIC  X(21) VALUE SPACES.             
           05  SUM-TOT-CHEQ-AMOUNT         PIC  $$$,$$$,$$$,$$9.99.             
           05  FILLER                      PIC  X(34) VALUE SPACES.             
       01  MICR-LINE-1.                                                         
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(20)                           
                                           VALUE 'MICR NUMBER: FIRST: '.        
           05  ML1-FIRST                   PIC  X(10).                          
           05  FILLER                      PIC  X(49) VALUE SPACES.             
       01  MICR-LINE-2.                                                         
           05  FILLER                      PIC  X     VALUE SPACES.             
           05  FILLER                      PIC  X(20)                           
                                           VALUE 'MICR NUMBER:  LAST: '.        
           05  ML1-LAST                    PIC  X(10).                          
           05  FILLER                      PIC  X(49) VALUE SPACES.             
                                                                                
      ******************************************************************        
      *  COPYBOOKS                                                     *        
      ******************************************************************        
      *    DATE CARD FILE COPYBOOK                                              
       01  GARDTCRD.                                                            
           COPY  GARDTCRD.                                                      
      *                                                                         
           COPY  BRSISSU.                                                       
                                                                                
      ******************************************************************        
      *  DB2 COPYBOOKS                                                 *        
      ******************************************************************        
                                                                                
      *--* DB2 SQL COMMUNICATIONS AREA                                          
      *    EXEC SQL                                                             
      *       INCLUDE SQLCA                                                     
      *    END-EXEC.                                                            
                                                                                
       01  WC-END-WORKING-STORAGE.                                              
           05  FILLER                       PIC X(80) VALUE                     
           '### TESTEST WORKING STORAGE ENDS HERE ###'.                         
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  PARM-DATA.                                                           
           03  PARM-LEN                     PIC S9(4)      COMP.                
           03  PARM-IND                     PIC XX.                             
           03  FILLER                       PIC X(58).                          
                                                                                
      ******************************************************************        
      *  END OF WORKING STORAGE SECTION                                *        
      ******************************************************************        
                                                                                
      ******************************************************************        
      *                                                                *        
      *  PROCEDURE DIVISION                                            *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
       PROCEDURE DIVISION  USING PARM-DATA.                                     
                                                                                
       00000-MAINLINE.                                                          
      ******************************************************************        
      *  MAINLINE                                                      *        
      ******************************************************************        
                                                                                
           PERFORM A0500-HOUSEKEEPING THRU A0500-EXIT                           
           PERFORM A1000-PROCESSING THRU A1000-EXIT                             
             UNTIL WS-ISSU-EOF-YES                                              
           PERFORM A2000-CONCLUSION THRU A2000-EXIT                             
           PERFORM A8000-WRAPUP THRU A8000-EXIT                                 
           STOP RUN                                                             
           .                                                                    
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. INITIALIZE PROGRAM WORKING STORAGE VARIABLES             *        
      *    2. INITIALIZE BATCH CONTROL TOTAL VARIABLES                 *        
      *    3. INITIALIZE THE OUTPUT RECORD AREA                        *        
      *    4. OPEN INPUT/OUTPUT FILES FOR PROCESSING                   *        
      *    5. GET REPORTING DATES                                      *        
      *                                                                *        
      ******************************************************************        
       A0500-HOUSEKEEPING.                                                      
      *                                                                         
           DISPLAY ' ***** PROGRAM NAME  = '  WC-THIS-PGM                       
                                                                                
           ACCEPT  WV-RUN-DATE      FROM  DATE                                  
           MOVE  WV-RD-YY           TO  DTL-HD1-YY                              
                                        HD1-YY                                  
           MOVE  WV-RD-MM           TO  DTL-HD1-MM                              
                                        HD1-MM                                  
           MOVE  WV-RD-DD           TO  DTL-HD1-DD                              
                                        HD1-DD                                  
                                                                                
           ACCEPT  WV-RUN-TIME      FROM  TIME                                  
           MOVE  WV-RT-H            TO  DTL-HD1-H                               
                                        HD1-H                                   
           MOVE  WV-RT-M            TO  DTL-HD1-M                               
                                        HD1-M                                   
           MOVE  WV-RT-S            TO  DTL-HD1-S                               
                                        HD1-S                                   
                                                                                
           DISPLAY ' ***** RUN DATE      = '  DTL-HD1-DATE                      
           DISPLAY ' ***** RUN TIME      = '  DTL-HD1-TIME                      
           DISPLAY '  '                                                         
           DISPLAY ' ***** ISSUED CHEQUES DATA *****'                           
                                                                                
           MOVE  WC-THIS-PGM        TO   DTL-HD1-PGM                            
                                         HD1-PGM                                
                                                                                
           SET  WX-IND        TO  +1                                            
           SEARCH  WX-BRS-APPL                                                  
                   VARYING WX-IND                                               
               AT  END                                                          
                 DISPLAY 'ERROR IN PARM-CARD, INVALID VALUE = ' PARM-IND        
                 DISPLAY '*** REQUIRED VALUE FOR LH CLAIMS-GRP = LG '           
                 DISPLAY '*** OR                               '                
                 DISPLAY '*** REQUIRED VALUE FOR LH CLAIMS-AMF = LA '           
                 DISPLAY '*** OR                               '                
                 DISPLAY '*** REQUIRED VALUE FOR MARINER       = MR '           
                 DISPLAY '*** OR                               '                
                 DISPLAY '*** REQUIRED VALUE FOR NAVIGATOR     = NV '           
                 DISPLAY '*** OR                               '                
                 DISPLAY '*** REQUIRED VALUE FOR EXCLAIMS      = XC '           
                 DISPLAY '*** OR                               '                
                 DISPLAY '*** REQUIRED VALUE FOR HEALTHPRO     = HP '           
                 DISPLAY '*** OR                               '                
                 DISPLAY '*** REQUIRED VALUE FOR FACETS        = FA '           
                 DISPLAY '*** OR                               '                
                 DISPLAY '*** REQUIRED VALUE FOR IDEAS TPA     = TI '           
                 DISPLAY '*** OR                               '                
                 DISPLAY '*** REQUIRED VALUE FOR EXCLAIMS TPA  = TX '           
                 MOVE  WC-RC-CODE016         TO  RETURN-CODE                    
                 STOP RUN                                                       
                                                                                
             WHEN  WX-APLCD (WX-IND)  =  PARM-IND                               
                   MOVE  WX-BRSAPL (WX-IND)      TO  HD4-NAME                   
                                                     DTL-HD4-NAME               
                   MOVE  WX-RDS1 (WX-IND)        TO  DTL-HD1-RDS                
                   MOVE  WX-RDS2 (WX-IND)        TO  HD1-RDS                    
               DISPLAY ' ***** DETAIL AND SUMMARY CONTROL REPORT FOR * '        
                       HD4-NAME                                                 
           END-SEARCH                                                           
      *                                                                         
           DISPLAY '  '                                                         
      *                                                                         
           OPEN  INPUT                                                          
                         BRS-ISSUE-FILE                                         
                         DATE-CARD-FILE                                         
           OPEN  OUTPUT                                                         
                         BRS-SUMMARY-REPORT                                     
                         BRS-DETAIL-REPORT                                      
                                                                                
           INITIALIZE WA-ACCUMULATORS                                           
      *    INITIALIZE WS-SWITCHES                                               
                                                                                
           PERFORM  A7000-READ-DATE  THRU  A7000-EXIT                           
             UNTIL                                                              
                    GARDTCRD-IDENTIFIER  =  WC-CURRENT                          
                                                                                
           MOVE  GARDTCRD-Y4M2D2             TO  DTL-HD5-DATE                   
                                                 HD5-DATE                       
                                                                                
           READ  BRS-ISSUE-FILE  INTO  BRS-ISSUE-DETAIL-RECORD                  
              AT END                                                            
                 DISPLAY '***** '                                               
                 DISPLAY '***** ERROR/NO RECORDS ON ISSUE FILE '                
                 DISPLAY '***** '                                               
                 MOVE  WC-RC-CODE016         TO  RETURN-CODE                    
                 STOP RUN                                                       
           END-READ                                                             
                                                                                
           IF  DTL-INTR-FLE-TYP                                                 
               MOVE  DTL-SOURCE-CODE          TO  WV-STOR-SRC-CD                
                                                  SDH1-SRC-CODE                 
                                                  DTL-HD5-SRC                   
               MOVE  DTL-ACCOUNT-NUM          TO  WV-STOR-BNK-ACT               
                                                  DDH1-BNK-ACCT                 
               MOVE  DTL-MICR-CHEQUE-NUM      TO  WV-MICR-FIRST                 
                                                                                
               PERFORM  A5000-SUM-REPORT-HEADING                                
                  THRU  A5000-EXIT                                              
               PERFORM  A5100-DTL-REPORT-HEADING                                
                  THRU  A5100-EXIT                                              
           ELSE                                                                 
               DISPLAY '***** '                                                 
               DISPLAY '***** NO ISSUE RECORDS ON FILE '                        
               DISPLAY '***** '                                                 
               MOVE  BRS-ISSUE-DETAIL-RECORD                                    
                 TO  BRS-ISSUE-TRAILER-RECORD                                   
               MOVE  TRL-SOURCE-CODE          TO  WV-STOR-SRC-CD                
                                                  SDH1-SRC-CODE                 
                                                  DTL-HD5-SRC                   
               PERFORM  A5000-SUM-REPORT-HEADING                                
                  THRU  A5000-EXIT                                              
               PERFORM  A5100-DTL-REPORT-HEADING                                
                  THRU  A5100-EXIT                                              
               SET  WS-ISSU-EOF-YES  TO  TRUE                                   
           END-IF                                                               
           .                                                                    
                                                                                
       A0500-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. READ PAYMENT FILE                                        *        
      *    2. PROCESS RECORDS UNTIL END OF FILE                        *        
      *                                                                *        
      ******************************************************************        
       A1000-PROCESSING.                                                        
      *                                                                         
           IF  DTL-SOURCE-CODE  NOT =  WV-STOR-SRC-CD                           
               PERFORM  A1010-PROCESS-SRC-ACT-BREAK                             
                  THRU  A1010-EXIT                                              
               PERFORM  A1015-AFTER-SRC-BREAK                                   
                  THRU  A1015-EXIT                                              
           ELSE                                                                 
               IF  DTL-ACCOUNT-NUM  NOT =  WV-STOR-BNK-ACT                      
                   PERFORM  A1010-PROCESS-SRC-ACT-BREAK                         
                      THRU  A1010-EXIT                                          
                   PERFORM  A1020-PROCESS-ACT-BREAK                             
                      THRU  A1020-EXIT                                          
               END-IF                                                           
           END-IF                                                               
                                                                                
           PERFORM  A1100-PROCESS-BRS-REPORT                                    
              THRU  A1100-EXIT                                                  
                                                                                
           READ  BRS-ISSUE-FILE  INTO  BRS-ISSUE-DETAIL-RECORD                  
                 AT END                                                         
                 SET  WS-ISSU-EOF-YES  TO  TRUE                                 
           END-READ                                                             
                                                                                
           IF  NOT  DTL-INTR-FLE-TYP                                            
               MOVE  BRS-ISSUE-DETAIL-RECORD                                    
                 TO  BRS-ISSUE-TRAILER-RECORD                                   
               SET  WS-ISSU-EOF-YES  TO  TRUE                                   
                                                                                
           END-IF                                                               
           .                                                                    
                                                                                
       A1000-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. WRITE REPORT LINES AT BREAK OF SOURCE CODE & ACCOUNT #   *        
      *                                                                *        
      ******************************************************************        
       A1010-PROCESS-SRC-ACT-BREAK.                                             
      *                                                                         
           MOVE  WV-STOR-BNK-ACT           TO  SUMD-BNK-ACC-NUM                 
           MOVE  WA-DTOT-CHEQ-AMT          TO  DTL-TOT-CHEQ-AMOUNT              
                                               SUMD-CHEQ-AMOUNT                 
           ADD   WA-DTOT-CHEQ-AMT          TO  WA-DRTOT-CHEQ-AMT                
                                               WA-STOT-CHEQ-AMT                 
           MOVE  WA-DTOT-CHEQUES           TO  DTL-TOT-CHEQ-ISSU                
                                               SUMD-NUM-OF-CHEQ                 
           ADD   WA-DTOT-CHEQUES           TO  WA-DRTOT-CHEQUES                 
                                               WA-STOT-CHEQUES                  
                                                                                
           WRITE  BRS-DTL-REC  FROM  DTLDASH-LINE    AFTER  1                   
           WRITE  BRS-DTL-REC  FROM  DTL-TOTAL-LINE  AFTER  1                   
           MOVE  SPACES                    TO  BRS-DTL-REC                      
           WRITE  BRS-DTL-REC                        AFTER  1                   
           ADD   3                         TO  WA-DLINE-CNT                     
                                                                                
           IF  WA-SLINE-CNT  >  60                                              
               PERFORM  A5000-SUM-REPORT-HEADING                                
                  THRU  A5000-EXIT                                              
           END-IF                                                               
                                                                                
           WRITE  BRS-SUM-REC  FROM  SUMD-LINE    AFTER  1                      
           ADD   1                         TO  WA-SLINE-CNT                     
                                                                                
           ADD   WA-STOT-CHEQUES           TO  WA-TTOT-CHEQUES                  
           ADD   WA-STOT-CHEQ-AMT          TO  WA-TTOT-CHEQ-AMT                 
                                                                                
           MOVE  ZEROS                     TO  WA-DTOT-CHEQ-AMT                 
                                               WA-DTOT-CHEQUES                  
                                               WA-STOT-CHEQ-AMT                 
                                               WA-STOT-CHEQUES                  
           .                                                                    
                                                                                
       A1010-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. WRITE REPORT LINES AFTER BREAK AND CONTINUE WITH NEW     *        
      *       SOURCE CODE                                              *        
      *                                                                *        
      ******************************************************************        
       A1015-AFTER-SRC-BREAK.                                                   
      *                                                                         
           MOVE  WA-TTOT-CHEQUES           TO  SUM-TOT-NUM-OF-CHEQ              
           MOVE  WA-TTOT-CHEQ-AMT          TO  SUM-TOT-CHEQ-AMOUNT              
                                                                                
           WRITE  BRS-SUM-REC  FROM  SUMDASH-LINE    AFTER  1                   
           WRITE  BRS-SUM-REC  FROM  SUM-TOTAL-LINE  AFTER  1                   
           MOVE  SPACES                    TO  BRS-SUM-REC                      
           WRITE  BRS-SUM-REC                        AFTER  1                   
           ADD   3                         TO  WA-SLINE-CNT                     
                                                                                
           MOVE  ZEROS                     TO  WA-TTOT-CHEQ-AMT                 
                                               WA-TTOT-CHEQUES                  
                                                                                
           MOVE  DTL-SOURCE-CODE           TO  WV-STOR-SRC-CD                   
                                               SDH1-SRC-CODE                    
                                               DTL-HD5-SRC                      
           MOVE  DTL-ACCOUNT-NUM           TO  WV-STOR-BNK-ACT                  
                                               DDH1-BNK-ACCT                    
                                                                                
           PERFORM  A5100-DTL-REPORT-HEADING                                    
              THRU  A5100-EXIT                                                  
                                                                                
           WRITE  BRS-SUM-REC  FROM        SUMD-HEAD-1  AFTER 1                 
           MOVE  SPACES                    TO  BRS-SUM-REC                      
           WRITE  BRS-SUM-REC                        AFTER  1                   
           WRITE  BRS-SUM-REC  FROM        SUMD-HEAD-2  AFTER 1                 
           WRITE  BRS-SUM-REC  FROM        SUMD-HEAD-3  AFTER 1                 
           MOVE  SPACES                    TO  BRS-SUM-REC                      
           WRITE  BRS-SUM-REC                        AFTER  1                   
           ADD   5                         TO  WA-SLINE-CNT                     
           .                                                                    
                                                                                
       A1015-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. WRITE REPORT LINES AT BREAK OF ACCOUNT NUMBER            *        
      *                                                                *        
      ******************************************************************        
       A1020-PROCESS-ACT-BREAK.                                                 
      *                                                                         
           MOVE  DTL-ACCOUNT-NUM           TO  WV-STOR-BNK-ACT                  
                                               DDH1-BNK-ACCT                    
                                                                                
           WRITE  BRS-DTL-REC  FROM  DTLD-HEAD-1  AFTER 1                       
           MOVE   SPACE        TO    BRS-DTL-REC                                
           WRITE  BRS-DTL-REC                     AFTER 1                       
           WRITE  BRS-DTL-REC  FROM  DTLD-HEAD-2  AFTER 1                       
           WRITE  BRS-DTL-REC  FROM  DTLD-HEAD-3  AFTER 1                       
           MOVE   SPACE        TO    BRS-DTL-REC                                
           WRITE  BRS-DTL-REC                     AFTER 1                       
           ADD    5            TO    WA-DLINE-CNT                               
           .                                                                    
                                                                                
       A1020-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. CONSTRUCT DETAIL CONTROL REPORT                          *        
      *    2. WRITE DETAIL CONTROL REPORT LINE                         *        
      *                                                                *        
      ******************************************************************        
       A1100-PROCESS-BRS-REPORT.                                                
                                                                                
           MOVE  DTL-MICR-CHEQUE-NUM       TO  WV-MICR-LAST                     
                                                                                
           MOVE  DTL-MICR-CHEQUE-NUM       TO  DTLD-CHQ-NUM                     
           MOVE  DTL-CHEQUE-DATE           TO  DTLD-CHQ-DATE                    
           MOVE  DTL-CHEQUE-AMOUNT         TO  DTLD-CHEQ-AMOUNT         00080002
           ADD   DTL-CHEQUE-AMOUNT         TO  WA-DTOT-CHEQ-AMT         00250002
           MOVE  DTL-REFERENCE-NUM         TO  DTLD-CHEQ-REF            00090002
           MOVE  DTL-POLICY-NUM            TO  DTLD-POLICY              00100002
           MOVE  DTL-CERTIFICATE           TO  DTLD-CERT-NUM            00110002
           MOVE  DTL-DIVISION-NUM          TO  DTLD-DIV-NUM                     
                                                                        00130002
           IF  WA-DLINE-CNT  >  60                                              
               PERFORM  A5100-DTL-REPORT-HEADING                                
                  THRU  A5100-EXIT                                              
           END-IF                                                               
                                                                                
           WRITE  BRS-DTL-REC  FROM        DTLD-LINE  AFTER 1                   
                                                                        00130002
           ADD   1                         TO  WA-DTOT-CHEQUES                  
                                               WA-DLINE-CNT                     
           .                                                                    
                                                                                
       A1100-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. WRITE TRAILER RECORD                                     *        
      *    2. PRINT CONTROL REPORT                                     *        
      *                                                                *        
      ******************************************************************        
       A2000-CONCLUSION.                                                        
                                                                                
           PERFORM  A1010-PROCESS-SRC-ACT-BREAK                                 
              THRU  A1010-EXIT                                                  
                                                                                
           MOVE  WA-DRTOT-CHEQ-AMT         TO  DTL-RPT-TOT-AMOUNT               
           MOVE  WA-DRTOT-CHEQUES          TO  DTL-RPT-TOT-CHEQ                 
           WRITE  BRS-DTL-REC  FROM        DTL-RPT-TOTAL-LINE AFTER 1           
                                                                                
           MOVE  WA-TTOT-CHEQUES           TO  SUM-TOT-NUM-OF-CHEQ              
           MOVE  WA-TTOT-CHEQ-AMT          TO  SUM-TOT-CHEQ-AMOUNT              
                                                                                
           WRITE  BRS-SUM-REC  FROM  SUMDASH-LINE    AFTER  1                   
           WRITE  BRS-SUM-REC  FROM  SUM-TOTAL-LINE  AFTER  1                   
           MOVE  SPACES                    TO  BRS-SUM-REC                      
           WRITE  BRS-SUM-REC                        AFTER  1                   
           ADD   3                         TO  WA-SLINE-CNT                     
                                                                                
           MOVE  WV-MICR-FIRST         TO  ML1-FIRST                            
           WRITE  BRS-SUM-REC  FROM        MICR-LINE-1 AFTER 3                  
           MOVE  WV-MICR-LAST          TO  ML1-LAST                             
           WRITE  BRS-SUM-REC  FROM        MICR-LINE-2 AFTER 1                  
                                                                                
           IF  WA-DRTOT-CHEQ-AMT  =  TRL-TOTAL-DOLLAR-AMT                       
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY '  '                                                     
               DISPLAY '***** TOTALS MISMATCH FOR DOLLAR AMOUNT *****'          
               DISPLAY '***** TOTAL AMOUNT CALCULATED        = '                
                        WA-DRTOT-CHEQ-AMT                                       
               DISPLAY '***** TOTAL AMOUNT ON TRAILER RECORD = '                
                        TRL-TOTAL-DOLLAR-AMT                                    
               MOVE  WC-RC-CODE008         TO  RETURN-CODE                      
           END-IF                                                               
                                                                                
           IF  WA-DRTOT-CHEQUES   =  TRL-TOTAL-REC-CNT                          
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY '  '                                                     
               DISPLAY '***** TOTALS MISMATCH FOR RECORD COUNT *****'           
               DISPLAY '***** TOTAL CALCULATED RECORD COUNT = '                 
                        WA-DRTOT-CHEQUES                                        
               DISPLAY '***** TOTAL TRAILER RECORD COUNT    = '                 
                        TRL-TOTAL-REC-CNT                                       
               MOVE  WC-RC-CODE008         TO  RETURN-CODE                      
           END-IF                                                               
           .                                                                    
                                                                                
       A2000-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. WRITE SUMMARY CONTROL REPORT HEADINGS                    *        
      *                                                                *        
      ******************************************************************        
       A5000-SUM-REPORT-HEADING.                                                
                                                                                
           ADD   1             TO    WA-S-PAGE                                  
           MOVE  WA-S-PAGE     TO    HD1-PAGE                                   
           WRITE  BRS-SUM-REC  FROM  SUM-HEAD-1   AFTER PAGE                    
           WRITE  BRS-SUM-REC  FROM  SUM-HEAD-2   AFTER 1                       
           WRITE  BRS-SUM-REC  FROM  SUM-HEAD-3   AFTER 1                       
           WRITE  BRS-SUM-REC  FROM  SUM-HEAD-4   AFTER 1                       
           WRITE  BRS-SUM-REC  FROM  SUM-HEAD-5   AFTER 1                       
           MOVE   SPACE        TO    BRS-SUM-REC                                
           WRITE  BRS-SUM-REC                     AFTER 1                       
           WRITE  BRS-SUM-REC  FROM  SUMD-HEAD-1  AFTER 1                       
           MOVE   SPACE        TO    BRS-SUM-REC                                
           WRITE  BRS-SUM-REC                     AFTER 1                       
           WRITE  BRS-SUM-REC  FROM  SUMD-HEAD-2  AFTER 1                       
           WRITE  BRS-SUM-REC  FROM  SUMD-HEAD-3  AFTER 1                       
           MOVE   SPACE        TO    BRS-SUM-REC                                
           WRITE  BRS-SUM-REC                     AFTER 1                       
           MOVE   11           TO    WA-SLINE-CNT                               
           .                                                                    
                                                                                
       A5000-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. WRITE DETAIL CONTROL REPORT HEADINGS                     *        
      *                                                                *        
      ******************************************************************        
       A5100-DTL-REPORT-HEADING.                                                
                                                                                
           ADD   1             TO    WA-D-PAGE                                  
           MOVE  WA-D-PAGE     TO    DTL-HD1-PAGE                               
                                                                                
           WRITE  BRS-DTL-REC  FROM  DTL-HEAD-1   AFTER PAGE                    
           WRITE  BRS-DTL-REC  FROM  DTL-HEAD-2   AFTER 1                       
           WRITE  BRS-DTL-REC  FROM  DTL-HEAD-3   AFTER 1                       
           WRITE  BRS-DTL-REC  FROM  DTL-HEAD-4   AFTER 1                       
           WRITE  BRS-DTL-REC  FROM  DTL-HEAD-5   AFTER 1                       
           MOVE   SPACE        TO    BRS-DTL-REC                                
           WRITE  BRS-DTL-REC                     AFTER 1                       
           WRITE  BRS-DTL-REC  FROM  DTLD-HEAD-1  AFTER 1                       
           MOVE   SPACE        TO    BRS-DTL-REC                                
           WRITE  BRS-DTL-REC                     AFTER 1                       
           WRITE  BRS-DTL-REC  FROM  DTLD-HEAD-2  AFTER 1                       
           WRITE  BRS-DTL-REC  FROM  DTLD-HEAD-3  AFTER 1                       
           MOVE   SPACE        TO    BRS-DTL-REC                                
           WRITE  BRS-DTL-REC                     AFTER 1                       
           MOVE   11           TO    WA-DLINE-CNT                               
           .                                                                    
                                                                                
       A5100-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. READ DATE CARD FILE TO GET THE BUSINESS DATE             *        
      *                                                                *        
      ******************************************************************        
       A7000-READ-DATE.                                                         
                                                                                
           READ  DATE-CARD-FILE  INTO  GARDTCRD                                 
                 AT END                                                         
                   DISPLAY '***** ERROR IN READING DATE FILE *****'             
                   DISPLAY '***** OR DATE RECORD NOT FOUND   *****'             
                   MOVE  WC-RC-CODE016         TO  RETURN-CODE                  
                   STOP RUN                                                     
           END-READ                                                             
           .                                                                    
                                                                                
       A7000-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  THIS PARAGRAPH WILL:                                          *        
      *    1. CLOSE OUTPUT FILE                                        *        
      *                                                                *        
      ******************************************************************        
       A8000-WRAPUP.                                                            
                                                                                
           CLOSE  BRS-ISSUE-FILE                                                
                  DATE-CARD-FILE                                                
                  BRS-DETAIL-REPORT                                             
                  BRS-SUMMARY-REPORT                                            
           .                                                                    
                                                                                
       A8000-EXIT.                                                              
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *  END OF PROGRAM BRSI0100                                       *        
      ******************************************************************        
