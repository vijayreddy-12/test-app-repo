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
           05  GL0005-CC-P1                        PIC X.                       
           05  GL0005-FORM-TYPE-P1                 PIC XX.                      
      *                                                                         
           05  GL0005-PAGE-1.                                                   
               10  GL0005-DATE-TIME                PIC X(60).                   
               10  GL0005-GEN-INFO.                                             
                   15  GL0005-COVERAGE.                                         
                       20  GL0005-COV-MMBR         PIC X.                       
                       20  GL0005-COV-MMBR-SPS     PIC X.                       
                       20  GL0005-COV-MMBR-DEP     PIC X.                       
                       20  GL0005-COV-MBR-SPS-DEP  PIC X.                       
                   15  GL0005-PLAN     OCCURS 3    PIC X(7).                    
                   15  GL0005-ACCOUNT  OCCURS 3    PIC XXX.                     
                   15  GL0005-DIVISION OCCURS 3    PIC XXX.                     
                   15  GL0005-CLASS    OCCURS 3    PIC XXX.                     
                   15  GL0005-CERT                 PIC X(11).                   
                   15  GL0005-EARNINGS     PIC ZZ,ZZZ,ZZ9.99.                   
                   15  GL0005-SPONSOR              PIC X(60).                   
                   15  GL0005-ELIG.                                             
                       20  GL0005-ELIG-E.                                       
                           25 GL0005-ELIG-EDD      PIC XX.                      
                           25 GL0005-ELIG-ESL1     PIC X.                       
                           25 GL0005-ELIG-EMMM     PIC X(3).                    
                           25 GL0005-ELIG-ESL2     PIC X.                       
                           25 GL0005-ELIG-EYYYY    PIC X(4).                    
                           25 FILLER               PIC X.                       
                       20 GL0005-ELIG-F                                         
                                  REDEFINES GL0005-ELIG-E.                      
                           25 GL0005-ELIG-FDD      PIC XX.                      
                           25 GL0005-ELIG-FSL1     PIC X.                       
                           25 GL0005-ELIG-FMMMM    PIC X(4).                    
                           25 GL0005-ELIG-FSL2     PIC X.                       
                           25 GL0005-ELIG-FYYYY    PIC X(4).                    
      *            15  GL0005-EVIDENCE-REQD.                                    
      *                20  GL0005-EVIDENCE-REQD-YES PIC X.                      
      *                20  GL0005-EVIDENCE-REQD-NO  PIC X.                      
                   15  GL0005-MAILED.                                           
                       20  GL0005-MAILED-YES        PIC X.                      
                       20  GL0005-MAILED-NO         PIC X.                      
      *                                                                         
                   15  GL0005-MBR-NAME.                                         
                       20  GL0005-MBR-SURNAME      PIC X(40).                   
                       20  GL0005-MBR-FSTNAME      PIC X(30).                   
                       20  GL0005-MBR-INITS        PIC X(5).                    
                   15  GL0005-MBR-DOB.                                          
                       20  GL0005-MBR-DOB-E.                                    
                           25 GL0005-MBR-DOB-EDD   PIC XX.                      
                           25 GL0005-MBR-DOB-ESL1  PIC X.                       
                           25 GL0005-MBR-DOB-EMMM  PIC X(3).                    
                           25 GL0005-MBR-DOB-ESL2  PIC X.                       
                           25 GL0005-MBR-DOB-EYYYY PIC X(4).                    
                           25 FILLER               PIC X.                       
                       20 GL0005-MBR-DOB-F                                      
                                  REDEFINES GL0005-MBR-DOB-E.                   
                           25 GL0005-MBR-DOB-FDD   PIC XX.                      
                           25 GL0005-MBR-DOB-FSL1  PIC X.                       
                           25 GL0005-MBR-DOB-FMMMM PIC X(4).                    
                           25 GL0005-MBR-DOB-FSL2  PIC X.                       
                           25 GL0005-MBR-DOB-FYYYY PIC X(4).                    
                   15  GL0005-LANG.                                             
                       20  GL0005-LANG-ENG         PIC X.                       
                       20  GL0005-LANG-FRC         PIC X.                       
                   15  GL0005-GENDER.                                           
                       20  GL0005-MALE             PIC X.                       
                       20  GL0005-FEMALE           PIC X.                       
                   15  GL0005-PROV                 PIC XX.                      
                   15  GL0005-SMOKER.                                           
                       20  GL0005-SMOKE-YES        PIC X.                       
                       20  GL0005-SMOKE-NO         PIC X.                       
      *                                                                         
                   15  GL0005-OL-AMOUNT.                                        
                       20  GL0005-OL-CURR-AMT     PIC ZZ,ZZZ,ZZ9.99.            
                       20  GL0005-OL-XSAL-AMT     PIC 9.                        
                       20  GL0005-OL-TOTAL-AMT    PIC ZZ,ZZZ,ZZ9.99.            
                   15  GL0005-ADTL-AMOUNT.                                      
                       20  GL0005-ADTL-CURR-AMT   PIC ZZ,ZZZ,ZZ9.99.            
                       20  GL0005-ADTL-XSAL-AMT   PIC 9.                        
                       20  GL0005-ADTL-TOTAL-AMT  PIC ZZ,ZZZ,ZZ9.99.            
                   15  GL0005-TOT-AMOUNT.                                       
                       20  GL0005-TOT-CURR-AMT    PIC ZZ,ZZZ,ZZ9.99.            
                       20  GL0005-TOT-XSAL-AMT    PIC 9.                        
                       20  GL0005-TOT-TOTAL-AMT   PIC ZZ,ZZZ,ZZ9.99.            
                   15  GL0005-ADD-AMOUNT.                                       
                       20  GL0005-ADD-CURR-AMT    PIC ZZ,ZZZ,ZZ9.99.            
                       20  GL0005-ADD-XSAL-AMT    PIC ZZ,ZZZ,ZZ9.99.            
                       20  GL0005-ADD-TOTAL-AMT   PIC ZZ,ZZZ,ZZ9.99.            
      *                                                                         
                   15  GL0005-BENEFICIARY          PIC X.                       
                   15  GL0005-REGION-P1            PIC X(5).                    
