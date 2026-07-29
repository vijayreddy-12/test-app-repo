           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L 3 1 8 7 PAGE 2                      *        
      *                                                                *        
      *   1. AFP PRINT, APPLICATION FOR CHANGE                         *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 3 1 8 7 PAGE 2                      *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *  01 - 180901 - JE    RELEASE 3.2 - ADD 3 NEW FIELDS            *        
      *                      1 FIELD NO LONGER USED                    *        
      *                      IN TESTING FOR 3.2, WE FOUND THAT         *        
      *                      GL3187-COMMENTS WAS NOT BEING USED AND    *        
      *                      AFP WAS NOT EXPECTING THIS FIELD.  IT HAS *        
      *                      BEEN REMOVED                              *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL3187-RECORD.                                                       
           05  GL3187-CC-P2                            PIC X.                   
           05  GL3187-FORM-TYPE-P2                     PIC XX.                  
      *                                                                         
           05  GL3187-PAGE-2.                                                   
               10  GL3187-FAMILY-INFO.                                          
                   15  GL3187-SPOUSE-CHG-CODE          PIC X.                   
                   15  GL3187-SPOUSE-CHG-EFF-DATE.                              
                       20  GL3187-SPOUSE-CHG-EFF-E.                             
                           25 GL3187-SPOUSE-CHG-EFF-EDD  PIC XX.                
                           25 GL3187-SPOUSE-CHG-EFF-ESL1 PIC X.                 
                           25 GL3187-SPOUSE-CHG-EFF-EMMM PIC X(3).              
                           25 GL3187-SPOUSE-CHG-EFF-ESL2 PIC X.                 
                           25 GL3187-SPOUSE-CHG-EFF-EYYYY PIC X(4).             
                           25 FILLER                     PIC X.                 
                       20  GL3187-SPOUSE-CHG-EFF-F                              
                                   REDEFINES GL3187-SPOUSE-CHG-EFF-E.           
                           25 GL3187-SPOUSE-CHG-EFF-FDD  PIC XX.                
                           25 GL3187-SPOUSE-CHG-EFF-FSL1 PIC X.                 
                           25 GL3187-SPOUSE-CHG-EFF-FMMMM PIC X(4).             
                           25 GL3187-SPOUSE-CHG-EFF-FSL2 PIC X.                 
                           25 GL3187-SPOUSE-CHG-EFF-FYYYY PIC X(4).             
      *                                                                         
                   15  GL3187-SPOUSE-NAME              PIC X(28).               
                   15  GL3187-FAMILY-SPOUSE-DOB.                                
                       20  GL3187-FAM-SPOUSE-DOB-E.                             
                           25 GL3187-FAM-SPOUSE-DOB-EDD  PIC XX.                
                           25 GL3187-FAM-SPOUSE-DOB-ESL1 PIC X.                 
                           25 GL3187-FAM-SPOUSE-DOB-EMMM PIC X(3).              
                           25 GL3187-FAM-SPOUSE-DOB-ESL2 PIC X.                 
                           25 GL3187-FAM-SPOUSE-DOB-EYYYY PIC X(4).             
                           25 FILLER                     PIC X.                 
                       20  GL3187-FAM-SPOUSE-DOB-F                              
                                   REDEFINES GL3187-FAM-SPOUSE-DOB-E.           
                           25 GL3187-FAM-SPOUSE-DOB-FDD  PIC XX.                
                           25 GL3187-FAM-SPOUSE-DOB-FSL1 PIC X.                 
                           25 GL3187-FAM-SPOUSE-DOB-FMMMM PIC X(4).             
                           25 GL3187-FAM-SPOUSE-DOB-FSL2 PIC X.                 
                           25 GL3187-FAM-SPOUSE-DOB-FYYYY PIC X(4).             
      *                                                                         
                   15  GL3187-FAM-SPOUSE-GENDER.                                
                       20  GL3187-FAM-SPOUSE-MALE      PIC X.                   
                       20  GL3187-FAM-SPOUSE-FEMALE    PIC X.                   
                   15  GL3187-FAM-SPOUSE-RELSHP        PIC X.                   
                   15  GL3187-CHILD-SPOUSE-IND         PIC X(6).                
                   15  GL3187-CHG-CODE-CHILD     OCCURS 8                       
                                                       PIC X.                   
      *                                                                         
                   15  GL3187-EFF-CHILD          OCCURS 8.                      
                       20  GL3187-CHILD-EFF-E.                                  
                           25  GL3187-CHILD-EFF-EDD    PIC XX.                  
                           25  GL3187-CHILD-EFF-ESL1   PIC X.                   
                           25  GL3187-CHILD-EFF-EMMM   PIC X(3).                
                           25  GL3187-CHILD-EFF-ESL2   PIC X.                   
                           25  GL3187-CHILD-EFF-EYYYY  PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL3187-CHILD-EFF-F                                   
                                   REDEFINES  GL3187-CHILD-EFF-E.               
                           25  GL3187-CHILD-EFF-FDD    PIC XX.                  
                           25  GL3187-CHILD-EFF-FSL1   PIC X.                   
                           25  GL3187-CHILD-EFF-FMMMM  PIC X(4).                
                           25  GL3187-CHILD-EFF-FSL2   PIC X.                   
                           25  GL3187-CHILD-EFF-FYYYY  PIC X(4).                
      *                                                                         
      *                                                                         
                   15  GL3187-NAME-CHILD      OCCURS 8                          
                                                       PIC X(28).               
      *                                                                         
                   15  GL3187-DOB-CHILD       OCCURS 8.                         
                       20  GL3187-CHILD-DOB-E.                                  
                           25  GL3187-CHILD-DOB-EDD    PIC XX.                  
                           25  GL3187-CHILD-DOB-ESL1   PIC X.                   
                           25  GL3187-CHILD-DOB-EMMM   PIC X(3).                
                           25  GL3187-CHILD-DOB-ESL2   PIC X.                   
                           25  GL3187-CHILD-DOB-EYYYY  PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL3187-CHILD-DOB-F                                   
                                   REDEFINES  GL3187-CHILD-DOB-E.               
                           25  GL3187-CHILD-DOB-FDD    PIC XX.                  
                           25  GL3187-CHILD-DOB-FSL1   PIC X.                   
                           25  GL3187-CHILD-DOB-FMMMM  PIC X(4).                
                           25  GL3187-CHILD-DOB-FSL2   PIC X.                   
                           25  GL3187-CHILD-DOB-FYYYY  PIC X(4).                
      *                                                                         
                   15  GL3187-CHILD-GENDER       OCCURS 8.                      
                       20  GL3187-CHILD-MALE           PIC X.                   
                       20  GL3187-CHILD-FEMALE         PIC X.                   
      *                                                                         
                   15  GL3187-CHILD-RELSHP       OCCURS 8                       
                                                       PIC X.                   
      *                                                                         
                   15  GL3187-CHILD-STUDENT      OCCURS 8.                      
                       20  GL3187-CHILD-STUDENT-YES    PIC X.                   
                       20  GL3187-CHILD-STUDENT-NO     PIC X.                   
      *                                                                         
                   15  GL3187-CHILD-DISABLED     OCCURS 8.                      
                       20  GL3187-CHILD-DISABLED-YES   PIC X.                   
                       20  GL3187-CHILD-DISABLED-NO    PIC X.                   
      *                                                                         
               10  GL3187-TERMINATE-DEP-COV.                                    
      * RELEASE 3.2 GL3187-TERMINATE-ALL-COV NO LONGER USED                     
                   15  GL3187-TERMINATE-ALL-COV        PIC X.                   
                   15  GL3187-TERMINATE-ALL-DATE.                               
                       20 GL3187-TERM-ALL-EDATE.                                
                           25 GL3187-TERM-ALL-EDD      PIC XX.                  
                           25 GL3187-TERM-ALL-ESL1     PIC X.                   
                           25 GL3187-TERM-ALL-EMMM     PIC X(3).                
                           25 GL3187-TERM-ALL-ESL2     PIC X.                   
                           25 GL3187-TERM-ALL-EYYYY    PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3187-TERM-ALL-FDATE                                 
                                  REDEFINES GL3187-TERM-ALL-EDATE.              
                           25 GL3187-TERM-ALL-FDD      PIC XX.                  
                           25 GL3187-TERM-ALL-FSL1     PIC X.                   
                           25 GL3187-TERM-ALL-FMMMM    PIC X(4).                
                           25 GL3187-TERM-ALL-FSL2     PIC X.                   
                           25 GL3187-TERM-ALL-FYYYY    PIC X(4).                
      *                                                                         
                   15  GL3187-TERMINATE-ALL-REASON     PIC X(75).               
               10  GL3187-BENFIT-REFUSAL.                                       
                   15  GL3187-REFUSE-EHC.                                       
                       20  GL3187-REFUSE-EHC-BOX1      PIC X.                   
                       20  GL3187-REFUSE-EHC-BOX2      PIC X.                   
                   15  GL3187-REFUSE-EHC-DATE-E.                                
                       20  GL3187-REF-EHC-EDD          PIC XX.                  
                       20  GL3187-REF-EHC-ESL1         PIC X.                   
                       20  GL3187-REF-EHC-EMMM         PIC X(3).                
                       20  GL3187-REF-EHC-ESL2         PIC X.                   
                       20  GL3187-REF-EHC-EYYYY        PIC X(4).                
                       20  FILLER                      PIC X.                   
                   15  GL3187-REFUSE-EHC-DATE-F                                 
                               REDEFINES  GL3187-REFUSE-EHC-DATE-E.             
                       20  GL3187-REF-EHC-FDD          PIC XX.                  
                       20  GL3187-REF-EHC-FSL1         PIC X.                   
                       20  GL3187-REF-EHC-FMMMM        PIC X(4).                
                       20  GL3187-REF-EHC-FSL2         PIC X.                   
                       20  GL3187-REF-EHC-FYYYY        PIC X(4).                
      *                                                                         
                   15  GL3187-REFUSE-DENT.                                      
                       20  GL3187-REFUSE-DENT-BOX1     PIC X.                   
                       20  GL3187-REFUSE-DENT-BOX2     PIC X.                   
                   15  GL3187-REFUSE-DENT-DATE-E.                               
                       20  GL3187-REF-DENT-EDD         PIC XX.                  
                       20  GL3187-REF-DENT-ESL1        PIC X.                   
                       20  GL3187-REF-DENT-EMMM        PIC X(3).                
                       20  GL3187-REF-DENT-ESL2        PIC X.                   
                       20  GL3187-REF-DENT-EYYYY       PIC X(4).                
                       20  FILLER                      PIC X.                   
                   15  GL3187-REFUSE-DENT-DATE-F                                
                               REDEFINES  GL3187-REFUSE-DENT-DATE-E.            
                       20  GL3187-REF-DENT-FDD         PIC XX.                  
                       20  GL3187-REF-DENT-FSL1        PIC X.                   
                       20  GL3187-REF-DENT-FMMMM       PIC X(4).                
                       20  GL3187-REF-DENT-FSL2        PIC X.                   
                       20  GL3187-REF-DENT-FYYYY       PIC X(4).                
      *                                                                         
                   15  GL3187-REFUSE-LIFE              PIC X.                   
                   15  GL3187-REFUSE-LIFE-DATE-E.                               
                       20  GL3187-REF-LIFE-EDD         PIC XX.                  
                       20  GL3187-REF-LIFE-ESL1        PIC X.                   
                       20  GL3187-REF-LIFE-EMMM        PIC X(3).                
                       20  GL3187-REF-LIFE-ESL2        PIC X.                   
                       20  GL3187-REF-LIFE-EYYYY       PIC X(4).                
                       20  FILLER                      PIC X.                   
                   15  GL3187-REFUSE-LIFE-DATE-F                                
                               REDEFINES  GL3187-REFUSE-LIFE-DATE-E.            
                       20  GL3187-REF-LIFE-FDD         PIC XX.                  
                       20  GL3187-REF-LIFE-FSL1        PIC X.                   
                       20  GL3187-REF-LIFE-FMMMM       PIC X(4).                
                       20  GL3187-REF-LIFE-FSL2        PIC X.                   
                       20  GL3187-REF-LIFE-FYYYY       PIC X(4).                
      *                                                                         
                   15  GL3187-QUEBEC-AGE.                                       
                       20  GL3187-QUEBEC-AGE-BOX1      PIC X.                   
                       20  GL3187-QUEBEC-AGE-BOX2      PIC X.                   
      *                                                                         
               10  GL3187-DIRECT-DEPOSIT-DETAILS.                               
                   15 GL3187-BANK-NAME           PIC X(60).                     
                   15 GL3187-BANK-TRANSIT        PIC X(5).                      
                   15 GL3187-BANK-INSTITUTION    PIC X(3).                      
                   15 GL3187-BANK-ACCOUNT        PIC X(12).                     
               10  GL3187-EMAIL.                                                
                   15 GL3187-EMAIL-ADRS-WORK  PIC X(60).                        
                   15 GL3187-EMAIL-ADRS-HOME  PIC X(60).                        
      *                                                                         
               10  GL3187-AUTHORIZATION.                                        
                   15  GL3187-ENGLISH-TODAY.                                    
                       20  GL3187-ETODAY-DD            PIC XX.                  
                       20  GL3187-ETODAY-SL1           PIC X.                   
                       20  GL3187-ETODAY-MMM           PIC X(3).                
                       20  GL3187-ETODAY-SL2           PIC X.                   
                       20  GL3187-ETODAY-YYYY          PIC X(4).                
                       20  FILLER                      PIC X.                   
                   15  GL3187-FRENCH-TODAY                                      
                                REDEFINES  GL3187-ENGLISH-TODAY.                
                       20  GL3187-FTODAY-DD            PIC XX.                  
                       20  GL3187-FTODAY-SL1           PIC X.                   
                       20  GL3187-FTODAY-MMMM          PIC X(4).                
                       20  GL3187-FTODAY-SL2           PIC X.                   
                       20  GL3187-FTODAY-YYYY          PIC X(4).                
                   15  GL3187-REGION-P2                PIC X(5).                
      *                                                                         
      * RELEASE 3.2 - NEW FIELDS                                                
               10  GL3187-SEC6-CHECKBOX                PIC X.                   
               10  GL3187-SEC7-CHECKBOX                PIC X.                   
               10  GL3187-SEC8-CHECKBOX                PIC X.                   
      *                                                                         
