       01  GDETEXP7.                                                            
      ******************************************************************        
      *    SEP/91 M. PRANGE.                                                    
      *         - STOLE BENEFICIARY NAME (32 CHARS) TO KEEP NEW FIELDS          
      *           FOR USE BY THIRD-PARTY-ADMIN (ECLIPSE) PROCESSING.            
      *                                                                 00060000
      *    JUN/93 B. WILKE.                                             00070000
      *         - STOLE ONE CHAR FROM FILLER AT END OF TPA DATA         00080000
      *           (FORMERLY BENE. NAME) FOR USE AS GDET-TAX-EXEMPT      00090000
      *           THIS FIELD WILL EITHER BE 'Y' OR SPACES (FOR NO)      00100001
      *                                                                 00060000
      *    MAR/96 M. PRANGE                                             00070000
      *         - ADDED FIELD GDET-COB-IND.                             00080000
      *                                                                         
      *    OCT/10 C. MUNCAN                                                     
      *         - IDMS TO DB2 PROJECT 7-DIGIT GROUP NUMBER EXPANSION            
      ******************************************************************        
           05  GDET-ROOTTRLR.                                                   
               10  GDET-RECLENGTH                  PIC S9(4)    COMP.           
               10  FILLER                          PIC X(2).                    
               10  GDET-IDENT.                                                  
                   15  GDET-GROUP                  PIC S9(7)    COMP-3.         
                   15  GDET-ACCT                   PIC XXX.                     
                   15  GDET-ID                     PIC X.                       
                   15  GDET-CERT                   PIC S9(9)    COMP-3.         
               10  GDET-ALTKEYGROUP                PIC S9(7)    COMP-3.         
               10  GDET-SIN                        PIC S9(9)    COMP-3.         
               10  GDET-BIRTHDATE                               COMP-3.         
                   15  GDET-BIRTHYR                PIC S9(3).                   
                   15  GDET-BIRTHDAY               PIC S9(3).                   
               10  GDET-SEX                        PIC X.                       
               10  GDET-RES                        PIC X(2).                    
               10  GDET-LANG                       PIC X.                       
               10  GDET-CLMFLDR                    PIC X.                       
               10  GDET-EE-HIRED-DATE                           COMP-3.         
                   15  GDET-EE-HIRED-YEAR          PIC S9(3).                   
                   15  GDET-EE-HIRED-DAY           PIC S9(3).                   
               10  GDET-SP-BIRTHDATE                            COMP-3.         
                   15  GDET-SP-BIRTHYR             PIC S9(3).                   
                   15  GDET-SP-BIRTHDAY            PIC S9(3).                   
               10  GDET-FUTURE-EFF-DATE                         COMP-3.         
                   15  GDET-FUTURE-EFF-YEAR        PIC S9(3).                   
                   15  GDET-FUTURE-EFF-DAY         PIC S9(3).                   
               10  GDET-DEC-REF-ACC-CODE           PIC XX.                      
               10  GDET-WAIVER-WAITING-PER         PIC X.                       
               10  GDET-EXCESS-IND                 PIC X.                       
               10  GDET-EVID-OF-INS-DATE                        COMP-3.         
                   15  GDET-EVID-OF-INS-YEAR       PIC S9(3).                   
                   15  GDET-EVID-OF-INS-DAY        PIC S9(3).                   
               10  GDET-PREV-ACCT                  PIC X(3).                    
               10  GDET-NEXT-ACCT                  PIC X(3).                    
               10  GDET-DIVISION                   PIC S9(3)    COMP-3.         
               10  GDET-EVID-OF-INS-LIFE           PIC XX.                      
               10  GDET-EVID-OF-INS-LTD            PIC XX.                      
               10  GDETROOT-FILLER                 PIC X(66).                   
           05  GDET-NAMETRLR.                                                   
               10  GDET-NMTRLR-ID                  PIC X(3).                    
               10  GDET-NMTRLR-LGTH                PIC S9(5)    COMP-3.         
               10  GDET-NAMES.                                                  
                   15  GDET-CURRNAME               PIC X(32).                   
                   15  GDET-PREVNAME               PIC X(32).                   
                   15  GDET-TPA-INFO.                                           
                       20  GDET-TPA-CARDNAME       PIC X(24).                   
                       20  GDET-OVERAGE-DATE.                                   
                           25  GDET-OVERAGE-YEAR   PIC S9(3)    COMP-3.         
                           25  GDET-OVERAGE-DAY    PIC S9(3)    COMP-3.         
                       20  GDET-TPA-COV-CODE       PIC X(2).                    
                   15  GDET-TAX-EXEMPT             PIC X(1).            00620000
                       88  GDET-TAX-EXEMPT-YES            VALUE 'Y'.    00621002
                       88  GDET-TAX-EXEMPT-NO             VALUE ' '.    00622002
                   15  GDET-COB-IND                PIC X(1).            00630000
                       88  GDET-COB-NA                    VALUE ' ' 'N'.00621002
                       88  GDET-COB-PRIMARY               VALUE 'P'.    00621002
                       88  GDET-COB-SECONDARY             VALUE 'S'.    00621002
               10  GDET-NAMES-FILLER               PIC X(32).                   
           05  GDET-HISTTRLR.                                                   
               10  GDET-HITRLR-ID                  PIC X(3).                    
               10  GDET-HITRLR-LGTH                PIC S9(5)    COMP-3.         
               10  GDET-NO-TRLR                    PIC S9(4)  COMP SYNC.        
               10  GDET-HISTORY-SEGMENTS.                                       
                   15  GDET-HIST-TRLR OCCURS 30 TIMES.                          
                       20  GDET-EFFDATE                         COMP-3.         
                           25  GDET-EFFYR          PIC S9(3).                   
                           25  GDET-EFFDAY         PIC S9(3).                   
                       20  GDET-PROCDATE                        COMP-3.         
                           25  GDET-PROCYR         PIC S9(3).                   
                           25  GDET-PROCDAY        PIC S9(3).                   
                       20  GDET-CLASS              PIC XX.                      
                       20  GDET-OCC-LTD            PIC X.                       
                       20  GDET-MODE               PIC XX.                      
                       20  GDET-SALARY.                                         
                           25  GDET-SALMODE        PIC X.                       
                           25  GDET-SALAMT         PIC S9(5)V99 COMP-3.         
                       20  GDET-HIST-FILLER        PIC X(20).                   
                       20  GDET-HIST-COVERAGE.                                  
                           25  GDET-NO-CODES       PIC S9(3)    COMP-3.         
                           25  GDET-COV-DATA OCCURS 20 TIMES.                   
                               30  GDET-CD-CODE    PIC S9(3)    COMP-3.         
                               30  GDET-CD-VOL     PIC S9(7)    COMP-3.         
                               30  GDET-CD-SEL-IND PIC XX.                      
                               30  GDET-CD-PREM    PIC S9(5)V99 COMP-3.         
