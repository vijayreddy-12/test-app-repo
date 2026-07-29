           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L 3 1 8 7 PAGE 1                      *        
      *                                                                *        
      *   1. AFP PRINT, APPLICATION FOR CHANGE                         *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 3 1 8 7 PAGE1                       *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *  01 - 180901 - JE    RELEASE 3.2 - 4 FIELDS NO LONGER USED     *        
      *                      4 FIELDS ADDED TO THE BOTTOM              *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL3187-RECORD.                                                       
           05  GL3187-CC-P1                            PIC X.                   
           05  GL3187-FORM-TYPE-P1                     PIC XX.                  
           05  GL3187-PAGE-1.                                                   
               10  GL3187-DATE-TIME                    PIC X(60).               
               10  GL3187-SECT1.                                                
                   15  GL3187-PLAN       OCCURS 5      PIC X(7).                
                   15  GL3187-ACCOUNT    OCCURS 5      PIC X(3).                
                   15  GL3187-DIV        OCCURS 5      PIC XXX.                 
                   15  GL3187-CERT                     PIC X(11).               
                   15  GL3187-SPONSOR                  PIC X(60).               
                   15  GL3187-MBR-NAME.                                         
                       20  GL3187-MBR-SURNAME          PIC X(40).               
                       20  GL3187-MBR-FSTNAME          PIC X(30).               
                       20  GL3187-MBR-INITS            PIC X(5).                
                   15  GL3187-MBR-NEWNAME.                                      
                       20  GL3187-MBR-NEW-SURNAME      PIC X(40).               
                       20  GL3187-MBR-NEW-FSTNAME      PIC X(30).               
                       20  GL3187-MBR-NEW-INITS        PIC X(5).                
      *                                                                         
               10  GL3187-MBR-ADDRESS.                                          
                   15  GL3187-MBR-STREET               PIC X(50).               
                   15  GL3187-MBR-CITY                 PIC X(30).               
                   15  GL3187-MBR-PROV                 PIC XX.                  
                   15  GL3187-MBR-POSTCODE             PIC X(10).               
      *                                                                         
               10  GL3187-BENEFIT-ADDITION.                                     
                   15  GL3187-EHC.                                              
                       20  GL3187-EHC-BOX1             PIC X.                   
                       20  GL3187-EHC-BOX2             PIC X.                   
                       20  GL3187-EHC-BOX3             PIC X.                   
                       20  GL3187-EHC-BOX4             PIC X.                   
                   15  GL3187-DENT.                                             
                       20  GL3187-DENT-BOX1            PIC X.                   
                       20  GL3187-DENT-BOX2            PIC X.                   
                       20  GL3187-DENT-BOX3            PIC X.                   
                       20  GL3187-DENT-BOX4            PIC X.                   
                   15  GL3187-DEP-LIFE                 PIC X.                   
                   15  GL3187-ADD-REASON.                                       
                       20  GL3187-ADD-REASON1          PIC X.                   
                       20  GL3187-ADD-REASON2          PIC X.                   
                       20  GL3187-ADD-REASON3          PIC X.                   
                       20  GL3187-ADD-REASON4          PIC X.                   
                   15  GL3187-MARRIAGE-DATE.                                    
                       20 GL3187-MARRIAGE-EDATE.                                
                           25 GL3187-MAR-DTE-EDD       PIC XX.                  
                           25 GL3187-MAR-DTE-ESL1      PIC X.                   
                           25 GL3187-MAR-DTE-EMMM      PIC X(3).                
                           25 GL3187-MAR-DTE-ESL2      PIC X.                   
                           25 GL3187-MAR-DTE-EYYYY     PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3187-MARRIAGE-FDATE                                 
                                  REDEFINES GL3187-MARRIAGE-EDATE.              
                           25 GL3187-MAR-DTE-FDD       PIC XX.                  
                           25 GL3187-MAR-DTE-FSL1      PIC X.                   
                           25 GL3187-MAR-DTE-FMMMM     PIC X(4).                
                           25 GL3187-MAR-DTE-FSL2      PIC X.                   
                           25 GL3187-MAR-DTE-FYYYY     PIC X(4).                
                   15  GL3187-COMMON-LAW-EFF-DATE.                              
                       20 GL3187-COM-EFF-EDATE.                                 
                           25 GL3187-COM-EFF-EDD       PIC XX.                  
                           25 GL3187-COM-EFF-ESL1      PIC X.                   
                           25 GL3187-COM-EFF-EMMM      PIC X(3).                
                           25 GL3187-COM-EFF-ESL2      PIC X.                   
                           25 GL3187-COM-EFF-EYYYY     PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3187-COM-EFF-FDATE                                  
                                  REDEFINES GL3187-COM-EFF-EDATE.               
                           25 GL3187-COM-EFF-FDD       PIC XX.                  
                           25 GL3187-COM-EFF-FSL1      PIC X.                   
                           25 GL3187-COM-EFF-FMMMM     PIC X(4).                
                           25 GL3187-COM-EFF-FSL2      PIC X.                   
                           25 GL3187-COM-EFF-FYYYY     PIC X(4).                
                   15  GL3187-TERMINATE-DATE.                                   
                       20 GL3187-TERM-EDATE.                                    
                           25 GL3187-TERM-EDD          PIC XX.                  
                           25 GL3187-TERM-ESL1         PIC X.                   
                           25 GL3187-TERM-EMMM         PIC X(3).                
                           25 GL3187-TERM-ESL2         PIC X.                   
                           25 GL3187-TERM-EYYYY        PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3187-TERM-FDATE                                     
                                  REDEFINES GL3187-TERM-EDATE.                  
                           25 GL3187-TERM-FDD          PIC XX.                  
                           25 GL3187-TERM-FSL1         PIC X.                   
                           25 GL3187-TERM-FMMMM        PIC X(4).                
                           25 GL3187-TERM-FSL2         PIC X.                   
                           25 GL3187-TERM-FYYYY        PIC X(4).                
      *                                                                         
                   15  GL3187-EFFECTIVE-DATE.                                   
                       20 GL3187-EFF-EDATE.                                     
                           25 GL3187-EFF-EDD           PIC XX.                  
                           25 GL3187-EFF-ESL1          PIC X.                   
                           25 GL3187-EFF-EMMM          PIC X(3).                
                           25 GL3187-EFF-ESL2          PIC X.                   
                           25 GL3187-EFF-EYYYY         PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3187-EFF-FDATE                                      
                                  REDEFINES GL3187-EFF-EDATE.                   
                           25 GL3187-EFF-FDD           PIC XX.                  
                           25 GL3187-EFF-FSL1          PIC X.                   
                           25 GL3187-EFF-FMMMM         PIC X(4).                
                           25 GL3187-EFF-FSL2          PIC X.                   
                           25 GL3187-EFF-FYYYY         PIC X(4).                
                   15  GL3187-OTHER-DETAILS            PIC X(110).              
                   15  GL3187-EVIDENCE-REQD.                                    
                       20  GL3187-EVIDENCE-REQD-YES    PIC X.                   
                       20  GL3187-EVIDENCE-REQD-NO     PIC X.                   
      * RELEASE 3.2 GL3187-MAILED NO LONGER USED                                
                   15  GL3187-MAILED.                                           
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X.                   
      *                                                                         
               10  GL3187-SPOUSAL-INFO.                                         
      * RELEASE 3.2 GL3187-SPOUSE-GENDER NO LONGER USED                         
                   15  GL3187-SPOUSE-GENDER.                                    
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X.                   
                   15  GL3187-SPOUSE-DOB.                                       
                       20 GL3187-SPOUSE-DOB-E.                                  
                           25 GL3187-SPOUSE-DOB-EDD    PIC XX.                  
                           25 GL3187-SPOUSE-DOB-ESL1   PIC X.                   
                           25 GL3187-SPOUSE-DOB-EMMM   PIC X(3).                
                           25 GL3187-SPOUSE-DOB-ESL2   PIC X.                   
                           25 GL3187-SPOUSE-DOB-EYYYY  PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3187-SPOUSE-DOB-F                                   
                                      REDEFINES GL3187-SPOUSE-DOB-E.            
                           25 GL3187-SPOUSE-DOB-FDD    PIC XX.                  
                           25 GL3187-SPOUSE-DOB-FSL1   PIC X.                   
                           25 GL3187-SPOUSE-DOB-FMMMM  PIC X(4).                
                           25 GL3187-SPOUSE-DOB-FSL2   PIC X.                   
                           25 GL3187-SPOUSE-DOB-FYYYY  PIC X(4).                
      *                                                                         
      * RELEASE 3.2 GL3187-MARITAL-STATUS NO LONGER USED                        
                   15  GL3187-MARITAL-STATUS.                                   
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X.                   
      * RELEASE 3.2 GL3187-RELATIONSHIP-START-DATE NO LONGER USED               
                   15  GL3187-RELATIONSHIP-START-DATE.                          
                       20  FILLER                      PIC XX.                  
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X(3).                
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X(4).                
                       20  FILLER                      PIC X.                   
      *                                                                         
                   15  GL3187-SPOUSE-HEALTH-COV.                                
                       20  GL3187-SPOUSE-HLTH-COV-Y    PIC X.                   
                       20  GL3187-SPOUSE-HLTH-COV-N    PIC X.                   
      *                                                                         
                   15  GL3187-SPOUSE-HC-EFF-DATE.                               
                       20 GL3187-SPOUSE-HC-EFF-EDATE.                           
                           25 GL3187-SPOUSE-HC-EFF-EDD  PIC XX.                 
                           25 GL3187-SPOUSE-HC-EFF-ESL1 PIC X.                  
                           25 GL3187-SPOUSE-HC-EFF-EMMM PIC X(3).               
                           25 GL3187-SPOUSE-HC-EFF-ESL2 PIC X.                  
                           25 GL3187-SPOUSE-HC-EFF-EYYYY PIC X(4).              
                           25 FILLER                    PIC X.                  
                       20 GL3187-SPOUSE-HC-EFF-FDATE                            
                                REDEFINES GL3187-SPOUSE-HC-EFF-EDATE.           
                           25 GL3187-SPOUSE-HC-EFF-FDD  PIC XX.                 
                           25 GL3187-SPOUSE-HC-EFF-FSL1 PIC X.                  
                           25 GL3187-SPOUSE-HC-EFF-FMMMM PIC X(4).              
                           25 GL3187-SPOUSE-HC-EFF-FSL2 PIC X.                  
                           25 GL3187-SPOUSE-HC-EFF-FYYYY PIC X(4).              
                   15  GL3187-SPOUSE-HC-SCOPE.                                  
                       20  GL3187-SPOUSE-HC-SCOPE-1    PIC X.                   
                       20  GL3187-SPOUSE-HC-SCOPE-2    PIC X.                   
                       20  GL3187-SPOUSE-HC-SCOPE-3    PIC X.                   
                       20  GL3187-SPOUSE-HC-SCOPE-4    PIC X.                   
                   15  GL3187-SPOUSE-DENT-COV.                                  
                       20  GL3187-SPOUSE-DENT-COV-Y    PIC X.                   
                       20  GL3187-SPOUSE-DENT-COV-N    PIC X.                   
                   15  GL3187-SPOUSE-DC-EFF-DATE.                               
                       20 GL3187-SPOUSE-DC-EFF-EDATE.                           
                           25 GL3187-SPOUSE-DC-EFF-EDD  PIC XX.                 
                           25 GL3187-SPOUSE-DC-EFF-ESL1 PIC X.                  
                           25 GL3187-SPOUSE-DC-EFF-EMMM PIC X(3).               
                           25 GL3187-SPOUSE-DC-EFF-ESL2 PIC X.                  
                           25 GL3187-SPOUSE-DC-EFF-EYYYY PIC X(4).              
                           25 FILLER                    PIC X.                  
                       20 GL3187-SPOUSE-DC-EFF-FDATE                            
                                REDEFINES GL3187-SPOUSE-DC-EFF-EDATE.           
                           25 GL3187-SPOUSE-DC-EFF-FDD  PIC XX.                 
                           25 GL3187-SPOUSE-DC-EFF-FSL1 PIC X.                  
                           25 GL3187-SPOUSE-DC-EFF-FMMMM PIC X(4).              
                           25 GL3187-SPOUSE-DC-EFF-FSL2 PIC X.                  
                           25 GL3187-SPOUSE-DC-EFF-FYYYY PIC X(4).              
                   15  GL3187-SPOUSE-DC-SCOPE.                                  
                       20  GL3187-SPOUSE-DC-SCOPE-1    PIC X.                   
                       20  GL3187-SPOUSE-DC-SCOPE-2    PIC X.                   
                       20  GL3187-SPOUSE-DC-SCOPE-3    PIC X.                   
                       20  GL3187-SPOUSE-DC-SCOPE-4    PIC X.                   
                   15  GL3187-REGION-P1                PIC X(5).                
      * NEW FIELDS ADDED WITH RELEASE 3.2                                       
                   15  GL3187-SEC2-CHECKBOX            PIC X.                   
                   15  GL3187-SEC3-CHECKBOX            PIC X.                   
                   15  GL3187-SEC4-CHECKBOX            PIC X.                   
                   15  GL3187-SEC5-CHECKBOX            PIC X.                   
