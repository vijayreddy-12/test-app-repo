           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C 3 1 8 7                         *        
      *                                                                *        
      *   1. AFP PRINT, APPLICATION FOR CHANGE                         *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C 3 1 8 7                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      *  01 - 010122 - JAK   CHANGES V3.1                              *        
      *                                                                *        
      *  02 - 180901 - JE    RELEASE 3.2 - 5 FIELDS NO LONGER USED     *        
      *                      7 FIELDS ADDED                            *        
      *  03 - 20150515 - IBM   TL 236472 ADD BANK DEAILS AND EMAIL IDS *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCC3187-RECORD.                                                     
           05  GCCC3187-HEADER.                                                 
               10  GCCC3187-CONF-NBR               PIC 9(11).                   
               10  GCCC3187-SEQ-NBR                PIC 9(5).                    
               10  GCCC3187-FORM-NBR               PIC X(8).                    
               10  GCCC3187-LANG                   PIC X.                       
               10  GCCC3187-CUST-GROUP-NBR         PIC X(7).                    
               10  GCCC3187-CUST-DIV               PIC X(3).                    
               10  GCCC3187-CUST-CERT-NBR          PIC X(10).                   
               10  GCCC3187-WEB-TIMESTAMP.                                      
                   15  GCCC3187-WEB-YYYY           PIC X(4).                    
                   15  GCCC3187-WEB-MM             PIC XX.                      
                   15  GCCC3187-WEB-DD             PIC XX.                      
                   15  GCCC3187-WEB-HH             PIC XX.                      
                   15  GCCC3187-WEB-MIN            PIC XX.                      
                   15  GCCC3187-WEB-SS             PIC XX.                      
                   15  GCCC3187-WEB-NN             PIC XX.                      
           05  GCCC3187-DETAIL.                                                 
               10  GCCC3187-DATE-TIME              PIC X(60).                   
      *        10  GCCC3187-SECT1.                                              
               10  GCCC3187-GEN-DATA   OCCURS 5.                                
                   15  GCCC3187-PLAN               PIC X(7).                    
                   15  GCCC3187-ACCOUNT            PIC X(3).                    
                   15  GCCC3187-DIV                PIC X(3).                    
               10  GCCC3187-CERT                   PIC X(11).                   
               10  GCCC3187-SPONSOR                PIC X(60).                   
               10  GCCC3187-MBR-NAME.                                           
                   15  GCCC3187-MBR-SURNAME        PIC X(40).                   
                   15  GCCC3187-MBR-FSTNAME        PIC X(30).                   
                   15  GCCC3187-MBR-INITS          PIC X(5).                    
               10  GCCC3187-NEWNAME.                                            
                   15  GCCC3187-NEW-SURNAME        PIC X(40).                   
                   15  GCCC3187-NEW-FSTNAME        PIC X(30).                   
                   15  GCCC3187-NEW-INITS          PIC X(5).                    
      *                                                                         
               10  GCCC3187-MBR-ADDRESS.                                        
                   15  GCCC3187-MBR-STREET         PIC X(50).                   
                   15  GCCC3187-MBR-CITY           PIC X(30).                   
                   15  GCCC3187-MBR-PROV           PIC XX.                      
                   15  GCCC3187-MBR-POSTCODE       PIC X(10).                   
      *                                                                         
               10  GCCC3187-SPOUSE-ADD-REASON      PIC X.                       
               10  GCCC3187-MARRIED-DATE.                                       
                   15  GCCC3187-MAR-DATE-YYYY      PIC X(4).                    
                   15  GCCC3187-MAR-DATE-MM        PIC XX.                      
                   15  GCCC3187-MAR-DATE-DD        PIC XX.                      
               10  GCCC3187-COM-DATE.                                           
                   15  GCCC3187-COM-DATE-YYYY      PIC X(4).                    
                   15  GCCC3187-COM-DATE-MM        PIC XX.                      
                   15  GCCC3187-COM-DATE-DD        PIC XX.                      
               10  GCCC3187-COV-TERM-DATE.                                      
                   15  GCCC3187-COV-TERM-DTE-YYYY  PIC X(4).                    
                   15  GCCC3187-COV-TERM-DTE-MM    PIC XX.                      
                   15  GCCC3187-COV-TERM-DTE-DD    PIC XX.                      
               10  GCCC3187-COV-EFF-DATE.                                       
                   15  GCCC3187-COV-EFF-DATE-YYYY  PIC X(4).                    
                   15  GCCC3187-COV-EFF-DATE-MM    PIC XX.                      
                   15  GCCC3187-COV-EFF-DATE-DD    PIC XX.                      
               10  GCCC3187-OTHER-DETAILS          PIC X(110).                  
               10  GCCC3187-EVIDENCE               PIC X.                       
      * RELEASE 3.2 GCCC3187-MAILED NO LONGER USED                              
               10  FILLER                          PIC X.                       
      * RELEASE 3.2 GCCC3187-SPOUSE-GENDER NO LONGER USED                       
               10  FILLER                          PIC X.                       
               10  GCCC3187-SPOUSE-DOB.                                         
                   15  GCCC3187-SPS-DOB-YYYY       PIC X(4).                    
                   15  GCCC3187-SPS-DOB-MM         PIC XX.                      
                   15  GCCC3187-SPS-DOB-DD         PIC XX.                      
      * RELEASE 3.2 GCCC3187-MARITAL-STATUS NO LONGER USED                      
               10  FILLER                          PIC X.                       
      * RELEASE 3.2 GCCC3187-REL-DATE NO LONGER USED                            
               10  FILLER                          PIC X(4).                    
               10  FILLER                          PIC XX.                      
               10  FILLER                          PIC XX.                      
               10  GCCC3187-COVERAGE-INFO      OCCURS 6.                        
                   15  GCCC3187-COV-BENEFIT        PIC X(4).                    
                   15  GCCC3187-COV-SEL-IND        PIC X.                       
                   15  GCCC3187-COV-SPOUS-IND      PIC X.                       
                   15  GCCC3187-COV-SPOUS-EFF-DATE.                             
                       20  GCCC3187-COV-SPOUS-EFDT-YYYY                         
                                                   PIC X(4).                    
                       20  GCCC3187-COV-SPOUS-EFDT-MM                           
                                                   PIC XX.                      
                       20  GCCC3187-COV-SPOUS-EFDT-DD                           
                                                   PIC XX.                      
                   15  GCCC3187-COV-SPOUS-PLAN     PIC X.                       
                   15  GCCC3187-COV-REFUSAL-IND    PIC X.                       
                   15  GCCC3187-COV-REFUS-EFF-DATE.                             
                       20  GCCC3187-COV-REFUS-EFDT-YYYY                         
                                                   PIC X(4).                    
                       20  GCCC3187-COV-REFUS-EFDT-MM                           
                                                   PIC XX.                      
                       20  GCCC3187-COV-REFUS-EFDT-DD                           
                                                   PIC XX.                      
               10  GCCC3187-REGION                 PIC X.                       
      *                                                                         
               10  GCCC3187-SPOUSE-CHG-CODE        PIC X.                       
               10  GCCC3187-SPOUSE-CHG-EFF-DATE.                                
                   15  GCCC3187-SCH-EFF-YYYY       PIC X(4).                    
                   15  GCCC3187-SCH-EFF-MM         PIC XX.                      
                   15  GCCC3187-SCH-EFF-DD         PIC XX.                      
               10  GCCC3187-SPOUSE-NAME            PIC X(28).                   
               10  GCCC3187-FAM-SPOUSE-DOB.                                     
                   15  GCCC3187-FSPS-DOB-YYYY      PIC X(4).                    
                   15  GCCC3187-FSPS-DOB-MM        PIC XX.                      
                   15  GCCC3187-FSPS-DOB-DD        PIC XX.                      
               10  GCCC3187-FAM-SPOUSE-GENDER      PIC X.                       
               10  GCCC3187-SPOUSE-RELSHP          PIC X.                       
               10  GCCC3187-CHILD-SPOUSE-IND       PIC X.                       
               10  GCCC3187-CHILD-INFO       OCCURS 8.                          
                   15  GCCC3187-CHILD-CHG-CODE     PIC X.                       
                   15  GCCC3187-CHILD-EFF-DATE.                                 
                       20  GCCC3187-CHEFF-YYYY     PIC X(4).                    
                       20  GCCC3187-CHEFF-MM       PIC XX.                      
                       20  GCCC3187-CHEFF-DD       PIC XX.                      
                   15  GCCC3187-CHILD-NAME         PIC X(28).                   
                   15  GCCC3187-CHILD-DOB.                                      
                       20  GCCC3187-CHDOB-YYYY     PIC X(4).                    
                       20  GCCC3187-CHDOB-MM       PIC XX.                      
                       20  GCCC3187-CHDOB-DD       PIC XX.                      
                   15  GCCC3187-CHILD-GENDER       PIC X.                       
                   15  GCCC3187-CHILD-RELSHP       PIC X.                       
                   15  GCCC3187-CHILD-STUDENT      PIC X.                       
                   15  GCCC3187-CHILD-DISAB        PIC X.                       
      * RELEASE 3.2 GCCC3187-TERM-ALL NO LONGER USED                            
               10  FILLER                          PIC X.                       
               10  GCCC3187-TERM-ALL-DATE.                                      
                   15  GCCC3187-TERM-ALL-DTE-YYYY  PIC X(4).                    
                   15  GCCC3187-TERM-ALL-DTE-MM    PIC XX.                      
                   15  GCCC3187-TERM-ALL-DTE-DD    PIC XX.                      
               10  GCCC3187-REASONFOR              PIC X(75).                   
               10  GCCC3187-QUEBEC                 PIC X.                       
      *                                                                         
               10  GCCC3187-TODAY.                                              
                   15  GCCC3187-TODAY-YYYY         PIC X(4).                    
                   15  GCCC3187-TODAY-MM           PIC XX.                      
                   15  GCCC3187-TODAY-DD           PIC XX.                      
      *                                                                         
      * RELEASE 3.2 ADDED 7 FIELDS                                              
               10  GCCC3187-SEC2-CHECKBOX          PIC X.                       
               10  GCCC3187-SEC3-CHECKBOX          PIC X.                       
               10  GCCC3187-SEC4-CHECKBOX          PIC X.                       
               10  GCCC3187-SEC5-CHECKBOX          PIC X.                       
               10  GCCC3187-SEC6-CHECKBOX          PIC X.                       
               10  GCCC3187-SEC7-CHECKBOX          PIC X.                       
               10  GCCC3187-SEC8-CHECKBOX          PIC X.                       
               10  GCCC3187-BANK-NAME              PIC X(60).                   
               10  GCCC3187-BANK-TRANSIT           PIC X(5).                    
               10  GCCC3187-BANK-INSTITUTION       PIC X(3).                    
               10  GCCC3187-BANK-ACCOUNT           PIC X(12).                   
               10  GCCC3187-EMAIL-ADRS-WORK        PIC X(60).                   
               10  GCCC3187-EMAIL-ADRS-HOME        PIC X(60).                   
