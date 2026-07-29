           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C 0 0 0 3                         *        
      *                                                                *        
      *   1. MQ MESSAGE, REFUSAL OF ALL COVERAGE                       *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C 0 0 0 3                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      *  01 - 010122 - JAK   CHANGES V3.1                              *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCC0003-RECORD.                                                     
           05  GCCC0003-HEADER.                                                 
               10  GCCC0003-CONF-NBR               PIC 9(11).                   
               10  GCCC0003-SEQ-NBR                PIC 9(5).                    
               10  GCCC0003-FORM-NBR               PIC X(8).                    
               10  GCCC0003-LANG                   PIC X.                       
               10  GCCC0003-CUST-GROUP-NBR         PIC X(7).                    
               10  GCCC0003-CUST-DIV               PIC X(3).                    
               10  GCCC0003-CUST-CERT-NBR          PIC X(10).                   
               10  GCCC0003-WEB-TIMESTAMP.                                      
                   15  GCCC0003-WEB-YYYY           PIC X(4).                    
                   15  GCCC0003-WEB-MM             PIC XX.                      
                   15  GCCC0003-WEB-DD             PIC XX.                      
                   15  GCCC0003-WEB-HH             PIC XX.                      
                   15  GCCC0003-WEB-MIN            PIC XX.                      
                   15  GCCC0003-WEB-SS             PIC XX.                      
                   15  GCCC0003-WEB-NN             PIC XX.                      
      *                                                                         
           05 GCCC0003-DETAIL.                                                  
               10  GCCC0003-PAGE1.                                              
                   15  GCCC0003-TIMESTAMP          PIC X(60).                   
      *                                                                         
               10  GCCC0003-GEN-INFO.                                           
                   15  GCCC0003-GEN-DATA    OCCURS 5.                           
                       20  GCCC0003-PLAN-NBR       PIC X(7).                    
                       20  GCCC0003-ACCOUNT-NBR    PIC X(3).                    
                       20  GCCC0003-DIVISION-NBR   PIC X(3).                    
                   15  GCCC0003-CERT-NBR           PIC X(11).                   
                   15  GCCC0003-PLAN-SPONSOR-NAME  PIC X(60).                   
                   15  GCCC0003-EMPLOYER           PIC X(80).                   
                   15  GCCC0003-NAME-1.                                         
                       20  GCCC0003-SURNAME        PIC X(40).                   
                       20  GCCC0003-FSTNAME        PIC X(30).                   
                       20  GCCC0003-INITS          PIC X(5).                    
                   15  GCCC0003-COMMENTS           PIC X(360).                  
      *                                                                         
               10  GCCC0003-CERT-AUTH.                                          
                   15  GCCC0003-SUBMIT-DATE.                                    
                       20  GCCC0003-SUBMIT-YYYY    PIC X(4).                    
                       20  GCCC0003-SUBMIT-MM      PIC XX.                      
                       20  GCCC0003-SUBMIT-DD      PIC XX.                      
                   15  GCCC0003-REGION             PIC X.                       
      *                                                                         
