           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L 0 0 0 3                             *        
      *                                                                *        
      *   1. AFP PRINT, APPLICATION FOR OPTIONAL LIFE INSURANCE        *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 0 0 0 5                             *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 010103 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL0005-RECORD.                                                       
           05  GL0005-CC-P2                        PIC X.                       
           05  GL0005-FORM-TYPE-P2                 PIC XX.                      
      *                                                                         
           05  GL0005-PAGE-2.                                                   
               10  GL0005-GENERAL.                                              
                   15  GL0005-SPOUSE-NAME.                                      
                       20  GL0005-SPOUSE-SURNAME   PIC X(40).                   
                       20  GL0005-SPOUSE-FSTNAME   PIC X(30).                   
                       20  GL0005-SPOUSE-INITS     PIC X(5).                    
                   15  GL0005-SPOUSE-GENDER.                                    
                       20  GL0005-SPOUSE-MALE         PIC X.                    
                       20  GL0005-SPOUSE-FEMALE       PIC X.                    
                   15  GL0005-SPOUSE-DOB.                                       
                       20  GL0005-SPOUSE-DOB-E.                                 
                           25 GL0005-SPOUSE-DOB-EDD   PIC XX.                   
                           25 GL0005-SPOUSE-DOB-ESL1  PIC X.                    
                           25 GL0005-SPOUSE-DOB-EMMM  PIC X(3).                 
                           25 GL0005-SPOUSE-DOB-ESL2  PIC X.                    
                           25 GL0005-SPOUSE-DOB-EYYYY PIC X(4).                 
                           25 FILLER                  PIC X.                    
                       20 GL0005-SPOUSE-DOB-F                                   
                                  REDEFINES GL0005-SPOUSE-DOB-E.                
                           25 GL0005-SPOUSE-DOB-FDD   PIC XX.                   
                           25 GL0005-SPOUSE-DOB-FSL1  PIC X.                    
                           25 GL0005-SPOUSE-DOB-FMMMM PIC X(4).                 
                           25 GL0005-SPOUSE-DOB-FSL2  PIC X.                    
                           25 GL0005-SPOUSE-DOB-FYYYY PIC X(4).                 
                   15  GL0005-SPOUSE-SMOKER.                                    
                       20  GL0005-SPOUSE-SMOKE-YES    PIC X.                    
                       20  GL0005-SPOUSE-SMOKE-NO     PIC X.                    
      *                                                                         
                   15  GL0005-SPS-OL-AMOUNT.                                    
                       20  GL0005-SPS-OL-CURR-AMT    PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-SPS-OL-XSAL-AMT    PIC 9.                     
                       20  GL0005-SPS-OL-TOTAL-AMT                              
                                                     PIC ZZ,ZZZ,ZZ9.99.         
                   15  GL0005-SPS-ADTL-AMOUNT.                                  
                       20  GL0005-SPS-ADTL-CURR-AMT                             
                                                     PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-SPS-ADTL-XSAL-AMT  PIC 9.                     
                       20  GL0005-SPS-ADTL-TOTAL-AMT                            
                                                     PIC ZZ,ZZZ,ZZ9.99.         
                   15  GL0005-SPS-TOT-AMOUNT.                                   
                       20  GL0005-SPS-TOT-CURR-AMT                              
                                                     PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-SPS-TOT-XSAL-AMT   PIC 9.                     
                       20  GL0005-SPS-TOT-TOTAL-AMT                             
                                                     PIC ZZ,ZZZ,ZZ9.99.         
                   15  GL0005-SPS-ADD-AMOUNT.                                   
                       20  GL0005-SPS-ADD-CURR-AMT                              
                                                     PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-SPS-ADD-ADTL-AMT   PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-SPS-ADD-TOTAL-AMT                             
                                                     PIC ZZ,ZZZ,ZZ9.99.         
      *                                                                         
                   15  GL0005-DEP-NAME     OCCURS 5.                            
                       20  GL0005-DEP-SURNAME         PIC X(40).                
                       20  GL0005-DEP-FSTNAME         PIC X(30).                
                       20  GL0005-DEP-INITS           PIC X(5).                 
                   15  GL0005-DEP-GENDER   OCCURS 5.                            
                       20  GL0005-DEP-MALE            PIC X.                    
                       20  GL0005-DEP-FEMALE          PIC X.                    
                   15  GL0005-DEP-DOB      OCCURS 5.                            
                       20  GL0005-BEN-DOB-E.                                    
                           25 GL0005-BEN-DOB-EDD      PIC XX.                   
                           25 GL0005-BEN-DOB-ESL1     PIC X.                    
                           25 GL0005-BEN-DOB-EMMM     PIC X(3).                 
                           25 GL0005-BEN-DOB-ESL2     PIC X.                    
                           25 GL0005-BEN-DOB-EYYYY    PIC X(4).                 
                           25 FILLER                  PIC X.                    
                       20 GL0005-BEN-DOB-F                                      
                                  REDEFINES GL0005-BEN-DOB-E.                   
                           25 GL0005-BEN-DOB-FDD      PIC XX.                   
                           25 GL0005-BEN-DOB-FSL1     PIC X.                    
                           25 GL0005-BEN-DOB-FMMMM    PIC X(4).                 
                           25 GL0005-BEN-DOB-FSL2     PIC X.                    
                           25 GL0005-BEN-DOB-FYYYY    PIC X(4).                 
                   15  GL0005-DEP-RELSHP   OCCURS 5   PIC X(15).                
                   15  GL0005-DEP-STUDENT  OCCURS 5.                            
                       20  GL0005-DEP-STUD-YES        PIC X.                    
                       20  GL0005-DEP-STUD-NO         PIC X.                    
      *                                                                         
                   15  GL0005-DEP-OL-AMOUNT.                                    
                       20  GL0005-DEP-OL-CURR-AMT    PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-DEP-OL-ADTL-AMT    PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-DEP-OL-TOTAL-AMT   PIC ZZ,ZZZ,ZZ9.99.         
                   15  GL0005-DEP-ADD-AMOUNT.                                   
                       20  GL0005-DEP-ADD-CURR-AMT   PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-DEP-ADD-ADTL-AMT   PIC ZZ,ZZZ,ZZ9.99.         
                       20  GL0005-DEP-ADD-TOTAL-AMT  PIC ZZ,ZZZ,ZZ9.99.         
      *                                                                         
               10  GL0005-AUTH.                                                 
                   15  GL0005-ENGLISH-TODAY.                                    
                       20  GL0005-ETODAY-DD        PIC XX.                      
                       20  GL0005-ETODAY-SL1       PIC X.                       
                       20  GL0005-ETODAY-MMM       PIC X(3).                    
                       20  GL0005-ETODAY-SL2       PIC X.                       
                       20  GL0005-ETODAY-YYYY      PIC X(4).                    
                       20  FILLER                  PIC X.                       
                   15  GL0005-FRENCH-TODAY                                      
                               REDEFINES  GL0005-ENGLISH-TODAY.                 
                       20  GL0005-FTODAY-DD        PIC XX.                      
                       20  GL0005-FTODAY-SL1       PIC X.                       
                       20  GL0005-FTODAY-MMMM      PIC X(4).                    
                       20  GL0005-FTODAY-SL2       PIC X.                       
                       20  GL0005-FTODAY-YYYY      PIC X(4).                    
                   15  GL0005-REGION-P2            PIC X(5).                    
