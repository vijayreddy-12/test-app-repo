           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C 0 0 0 5                         *        
      *                                                                *        
      *   1. MQ MESSAGE, APPLICATION FOR OPTIONAL LIFE INSURANCE       *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C 0 0 0 5                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 010102 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCC0005-RECORD.                                                     
           05  GCCC0005-HEADER.                                                 
               10  GCCC0005-CONF-NBR               PIC 9(11).                   
               10  GCCC0005-SEQ-NBR                PIC 9(5).                    
               10  GCCC0005-FORM-NBR               PIC X(8).                    
               10  GCCC0005-LANG                   PIC X.                       
               10  GCCC0005-CUST-GROUP-NBR         PIC X(7).                    
               10  GCCC0005-CUST-DIV               PIC X(3).                    
               10  GCCC0005-CUST-CERT-NBR          PIC X(10).                   
               10  GCCC0005-WEB-TIMESTAMP.                                      
                   15  GCCC0005-WEB-YYYY           PIC X(4).                    
                   15  GCCC0005-WEB-MM             PIC XX.                      
                   15  GCCC0005-WEB-DD             PIC XX.                      
                   15  GCCC0005-WEB-HH             PIC XX.                      
                   15  GCCC0005-WEB-MIN            PIC XX.                      
                   15  GCCC0005-WEB-SS             PIC XX.                      
                   15  GCCC0005-WEB-NN             PIC XX.                      
      *                                                                         
           05 GCCC0005-DETAIL.                                                  
               10  GCCC0005-PAGE1.                                              
                   15  GCCC0005-TIMESTAMP          PIC X(60).                   
      *                                                                         
               10  GCCC0005-GEN-INFO.                                           
                   15  GCCC0005-COVERAGE           PIC X.                       
                   15  GCCC0005-MBR-INFO    OCCURS 3.                           
                       20  GCCC0005-PLAN-NBR       PIC X(7).                    
                       20  GCCC0005-ACCOUNT-NBR    PIC X(3).                    
                       20  GCCC0005-DIV            PIC XXX.                     
                       20  GCCC0005-CLASS          PIC XXX.                     
                   15  GCCC0005-CERT-NBR           PIC X(11).                   
                   15  GCCC0005-EARNINGS           PIC 9(11)V99.                
                   15  GCCC0005-EMPLOYER           PIC X(60).                   
                   15  GCCC0005-ELIG-DATE.                                      
                       20  GCCC0005-ELIG-YYYY      PIC X(4).                    
                       20  GCCC0005-ELIG-MM        PIC XX.                      
                       20  GCCC0005-ELIG-DD        PIC XX.                      
                   15  GCCC0005-MAILED             PIC X.                       
                   15  GCCC0005-MBR-NAME.                                       
                       20  GCCC0005-MBR-SURNAME    PIC X(40).                   
                       20  GCCC0005-MBR-FSTNAME    PIC X(30).                   
                       20  GCCC0005-MBR-INITS      PIC X(5).                    
                   15  GCCC0005-MBR-DOB.                                        
                       20  GCCC0005-MBR-DOB-YYYY   PIC X(4).                    
                       20  GCCC0005-MBR-DOB-MM     PIC XX.                      
                       20  GCCC0005-MBR-DOB-DD     PIC XX.                      
                   15  GCCC0005-MBR-LANG           PIC X.                       
                   15  GCCC0005-MBR-GENDER         PIC X.                       
                   15  GCCC0005-MBR-PROV           PIC XX.                      
                   15  GCCC0005-MBR-SMOKER         PIC X.                       
      *                                                                         
                   15  GCCC0005-OL-CURR-AMT        PIC 9(11)V99.                
                   15  GCCC0005-OL-XSAL-AMT        PIC 9.                       
                   15  GCCC0005-OL-TOTAL-AMT       PIC 9(11)V99.                
      *                                                                         
                   15  GCCC0005-ADTL-CURR-AMT      PIC 9(11)V99.                
                   15  GCCC0005-ADTL-XSAL-AMT      PIC 9.                       
                   15  GCCC0005-ADTL-TOTAL-AMT     PIC 9(11)V99.                
      *                                                                         
                   15  GCCC0005-TOT-CURR-AMT       PIC 9(11)V99.                
                   15  GCCC0005-TOT-XSAL-AMT       PIC 9.                       
                   15  GCCC0005-TOT-TOTAL-AMT      PIC 9(11)V99.                
      *                                                                         
                   15  GCCC0005-ADD-CURR-AMT       PIC 9(11)V99.                
                   15  GCCC0005-ADD-XSAL-AMT       PIC 9(11)V99.                
                   15  GCCC0005-ADD-TOTAL-AMT      PIC 9(11)V99.                
      *                                                                         
                   15  GCCC0005-BENEFICIARY        PIC X.                       
               10  GCCC0005-SPOUSAL-INFO.                                       
                   15  GCCC0005-SPS-NAME.                                       
                       20  GCCC0005-SPS-SURNAME      PIC X(40).                 
                       20  GCCC0005-SPS-FSTNAME      PIC X(30).                 
                       20  GCCC0005-SPS-INITS        PIC X(5).                  
                   15  GCCC0005-SPS-GENDER         PIC X.                       
                   15  GCCC0005-SPOUSE-DOB.                                     
                       20  GCCC0005-SPS-DOB-YYYY   PIC X(4).                    
                       20  GCCC0005-SPS-DOB-MM     PIC XX.                      
                       20  GCCC0005-SPS-DOB-DD     PIC XX.                      
                   15  GCCC0005-SPS-SMOKER         PIC X.                       
      *                                                                         
                   15  GCCC0005-SPS-OL-CURR-AMT    PIC 9(11)V99.                
                   15  GCCC0005-SPS-OL-XSAL-AMT    PIC 9.                       
                   15  GCCC0005-SPS-OL-TOTAL-AMT   PIC 9(11)V99.                
      *                                                                         
                   15  GCCC0005-SPS-ADTL-CURR-AMT  PIC 9(11)V99.                
                   15  GCCC0005-SPS-ADTL-XSAL-AMT  PIC 9.                       
                   15  GCCC0005-SPS-ADTL-TOTAL-AMT PIC 9(11)V99.                
      *                                                                         
                   15  GCCC0005-SPS-TOT-CURR-AMT   PIC 9(11)V99.                
                   15  GCCC0005-SPS-TOT-XSAL-AMT   PIC 9.                       
                   15  GCCC0005-SPS-TOT-TOTAL-AMT  PIC 9(11)V99.                
      *                                                                         
                   15  GCCC0005-SPS-ADD-CURR-AMT   PIC 9(11)V99.                
                   15  GCCC0005-SPS-ADD-ADTL-AMT   PIC 9(11)V99.                
                   15  GCCC0005-SPS-ADD-TOTAL-AMT  PIC 9(11)V99.                
      *                                                                         
               10  GCCC0005-DEPENDENT-INFO.                                     
                   15  GCCC0005-DEP-DATA   OCCURS 5.                            
                       20  GCCC0005-DEP-NAME.                                   
                           25  GCCC0005-DEP-SURNAME  PIC X(40).                 
                           25  GCCC0005-DEP-FSTNAME  PIC X(30).                 
                           25  GCCC0005-DEP-INITS    PIC X(5).                  
                       20  GCCC0005-DEP-GENDER       PIC X.                     
                       20  GCCC0005-DEP-DOB.                                    
                           25  GCCC0005-DEP-DOB-YYYY PIC X(4).                  
                           25  GCCC0005-DEP-DOB-MM   PIC XX.                    
                           25  GCCC0005-DEP-DOB-DD   PIC XX.                    
                       20  GCCC0005-DEP-RELSHP       PIC X(15).                 
                       20  GCCC0005-DEP-STUDENT      PIC X.                     
                   15  GCCC0005-DEP-OL-CURR-AMT     PIC 9(11)V99.               
                   15  GCCC0005-DEP-OL-OL-ADTL-AMT  PIC 9(11)V99.               
                   15  GCCC0005-DEP-OL-OL-TOTAL-AMT PIC 9(11)V99.               
      *                                                                         
                   15  GCCC0005-DEP-ADD-CURR-AMT    PIC 9(11)V99.               
                   15  GCCC0005-DEP-ADD-ADTL-AMT    PIC 9(11)V99.               
                   15  GCCC0005-DEP-ADD-TOTAL-AMT   PIC 9(11)V99.               
      *                                                                         
               10  GCCC0005-CERT-AUTH.                                          
                   15  GCCC0005-SUBMIT-DATE.                                    
                       20  GCCC0005-SUBMIT-YYYY    PIC X(4).                    
                       20  GCCC0005-SUBMIT-MM      PIC XX.                      
                       20  GCCC0005-SUBMIT-DD      PIC XX.                      
                   15  GCCC0005-REGION             PIC X.                       
      *                                                                         
