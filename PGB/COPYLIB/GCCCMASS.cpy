           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C M A S S                         *        
      *                                                                *        
      *   1. MQ MESSAGE, MASS CHANGE SCREEN                            *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C M A S S                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 010102 - JAK   NEW COPYBOOK                              *        
      *  01 - 100805 - IBM   ADD NEW FIELD TO GLMASS                  *         
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCCMASS-RECORD.                                                     
           05  GCCCMASS-HEADER.                                                 
               10  GCCCMASS-CONF-NBR               PIC 9(11).                   
               10  GCCCMASS-SEQ-NBR                PIC 9(5).                    
               10  GCCCMASS-FORM-NBR               PIC X(8).                    
               10  GCCCMASS-LANG                   PIC X.                       
               10  GCCCMASS-GROUP                  PIC X(7).                    
               10  GCCCMASS-DIV                    PIC X(3).                    
               10  GCCCMASS-PAGE-ID                PIC 9(7).                    
               10  FILLER                          PIC X(3).                    
               10  GCCCMASS-WEB-TIMESTAMP.                                      
                   15  GCCCMASS-WEB-YYYY           PIC X(4).                    
                   15  GCCCMASS-WEB-MM             PIC XX.                      
                   15  GCCCMASS-WEB-DD             PIC XX.                      
                   15  GCCCMASS-WEB-HH             PIC XX.                      
                   15  GCCCMASS-WEB-MIN            PIC XX.                      
                   15  GCCCMASS-WEB-SS             PIC XX.                      
                   15  GCCCMASS-WEB-NN             PIC XX.                      
      *                                                                         
           05 GCCCMASS-DETAIL.                                                  
               10  GCCCMASS-PAGE1.                                              
                   15  GCCCMASS-TIMESTAMP          PIC X(60).                   
      *                                                                         
               10  GCCCMASS-SUB-HEADER-INFO.                                    
                   15  GCCCMASS-CLIENT-NAME        PIC X(60).                   
      *                                                                         
               10  GCCCMASS-GEN-INFO    OCCURS  15.                             
                   15  GCCCMASS-PLAN               PIC X(7).                    
                   15  GCCCMASS-ACCOUNT-NBR        PIC X(3).                    
                   15  GCCCMASS-CERT-NBR           PIC X(11).                   
                   15  GCCCMASS-EMPL-LAST-NME      PIC X(40).                   
                   15  GCCCMASS-EMPL-FIRST-NME     PIC X(30).                   
                   15  GCCCMASS-EMPL-MID-INIT      PIC X(5).                    
                   15  GCCCMASS-EFF-DATE.                                       
                       20  GCCCMASS-EFF-DTE-YYYY   PIC X(4).                    
                       20  GCCCMASS-EFF-DTE-MM     PIC XX.                      
                       20  GCCCMASS-EFF-DTE-DD     PIC XX.                      
                   15  GCCCMASS-TERM-REASON        PIC XX.                      
                   15  GCCCMASS-RETURN-DATE.                                    
                       20  GCCCMASS-RET-YYYY       PIC X(4).                    
                       20  GCCCMASS-RET-MM         PIC XX.                      
                       20  GCCCMASS-RET-DD         PIC XX.                      
                   15  GCCCMASS-SALARY-AMT         PIC 9(11)V99.                
                   15  GCCCMASS-SALARY-FREQ        PIC X.                       
                   15  GCCCMASS-OCC                PIC X(29).                   
                   15  GCCCMASS-CLASS.                                          
                       20  GCCCMASS-CLASS-NEW      PIC X(3).                    
                       20  GCCCMASS-CLASS-OLD      PIC X(3).                    
                   15  GCCCMASS-ACCT-CHG           PIC X(3).                    
                   15  GCCCMASS-DIV.                                            
                       20  GCCCMASS-DIV-NEW        PIC X(3).                    
                       20  GCCCMASS-DIV-OLD        PIC X(3).                    
                   15  GCCCMASS-EVIDENCE           PIC X.                       
                   15  GCCCMASS-MAILED             PIC X.                       
                   15  GCCCMASS-WORKING-HOURS      PIC 9(11)V99.                
      *                                                                         
               10  GCCCMASS-CERT-AUTH.                                          
                   15  GCCCMASS-SUBMIT-DATE.                                    
                       20  GCCCMASS-SUBMIT-YYYY    PIC X(4).                    
                       20  GCCCMASS-SUBMIT-MM      PIC XX.                      
                       20  GCCCMASS-SUBMIT-DD      PIC XX.                      
                   15  GCCCMASS-REGION             PIC X.                       
      *                                                                         
