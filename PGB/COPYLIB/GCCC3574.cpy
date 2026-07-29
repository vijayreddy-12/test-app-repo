           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C 3 5 7 4                         *        
      *                                                                *        
      *   1. AFP PRINT, ENROLMENT/RE-ENROLMENT APPLICATION WITH HCSA   *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C 3 5 7 4                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE       PGR   DESCRIPTION                             *        
      *  --   ------     ---   --------------------------------------  *        
      *                                                                *        
      *  00 - 20020528 - JVH   ECOMM 4.2                               *        
      *                        NEW COPYBOOK - CLONED FROM GCCC2971     *        
      *                           (THE SAME EXCEPT FOR HCSA FIELDS     *        
      *                            AND NUMBER OF DEPENDENTS ALLOWED)   *        
      *  01 - 20020718 - JE    HCSA DATE NOT BEING RECEIVED FROM WEB   *        
      *                        IN DATE FORMAT                          *        
      *                        ALLOWED 3 BYTES OF FILLER TO ALLOW FOR  *        
      *                        POSSIBLE DIFFERENCES IN FORMAT          *        
      *  02 - 20150515 - IBM   TL 236472 ADD BANK DEAILS AND EMAIL IDS *        
      *                        REMOVE  WAITING-PERIOD-COND AND         *        
      *                        HCSA-ALLOC-TYPE                         *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCC3574-RECORD.                                                     
           05  GCCC3574-HEADER.                                                 
               10  GCCC3574-CONF-NBR               PIC 9(11).                   
               10  GCCC3574-SEQ-NBR                PIC 9(5).                    
               10  GCCC3574-FORM-NBR               PIC X(8).                    
               10  GCCC3574-LANG                   PIC X.                       
               10  GCCC3574-CUST-GROUP-NBR         PIC X(7).                    
               10  GCCC3574-CUST-DIV               PIC X(3).                    
               10  GCCC3574-CUST-CERT-NBR          PIC X(10).                   
               10  GCCC3574-WEB-TIMESTAMP.                                      
                   15  GCCC3574-WEB-YYYY           PIC X(4).                    
                   15  GCCC3574-WEB-MM             PIC XX.                      
                   15  GCCC3574-WEB-DD             PIC XX.                      
                   15  GCCC3574-WEB-HH             PIC XX.                      
                   15  GCCC3574-WEB-MIN            PIC XX.                      
                   15  GCCC3574-WEB-SS             PIC XX.                      
                   15  GCCC3574-WEB-NN             PIC XX.                      
           05  GCCC3574-DETAIL.                                                 
               10  GCCC3574-DATE-TIME              PIC X(60).                   
               10  GCCC3574-GEN-DATA   OCCURS 5.                                
                   15  GCCC3574-PLAN               PIC X(7).                    
                   15  GCCC3574-ACCOUNT            PIC X(3).                    
                   15  GCCC3574-DIV                PIC XXX.                     
                   15  GCCC3574-CLASS              PIC XXX.                     
               10  GCCC3574-CERT                   PIC X(11).                   
               10  GCCC3574-SPONSOR                PIC X(60).                   
               10  GCCC3574-HIRE-DATE.                                          
                   15  GCCC3574-HIRE-DT-YYYY       PIC X(4).                    
                   15  GCCC3574-HIRE-DT-MM         PIC XX.                      
                   15  GCCC3574-HIRE-DT-DD         PIC XX.                      
               10  GCCC3574-PREV-EMPL-DATE.                                     
                   15  GCCC3574-PE-DT-YYYY         PIC X(4).                    
                   15  GCCC3574-PE-DT-MM           PIC XX.                      
                   15  GCCC3574-PE-DT-DD           PIC XX.                      
               10  GCCC3574-REHIRE-DATE.                                        
                   15  GCCC3574-REHIRE-DT-YYYY     PIC X(4).                    
                   15  GCCC3574-REHIRE-DT-MM       PIC XX.                      
                   15  GCCC3574-REHIRE-DT-DD       PIC XX.                      
               10  GCCC3574-OCC                    PIC X(29).                   
               10  GCCC3574-HOURS-X.                                            
                   15  GCCC3574-HOURS              PIC 9(4)V99.                 
               10  GCCC3574-EARNINGS-X.                                         
                   15  GCCC3574-EARNINGS           PIC 9(11)V99.                
               10  GCCC3574-EVIDENCE               PIC X.                       
               10  FILLER                          PIC X.                       
               10  GCCC3574-EMP-NAME.                                           
                   15  GCCC3574-EMP-SURNAME        PIC X(40).                   
                   15  GCCC3574-EMP-FSTNAME        PIC X(30).                   
                   15  GCCC3574-EMP-INITS          PIC X(5).                    
               10  GCCC3574-EMP-DOB.                                            
                   15  GCCC3574-EMP-DOB-YYYY       PIC X(4).                    
                   15  GCCC3574-EMP-DOB-MM         PIC XX.                      
                   15  GCCC3574-EMP-DOB-DD         PIC XX.                      
               10  GCCC3574-EMP-GENDER             PIC X.                       
               10  GCCC3574-EMP-PROV               PIC XX.                      
               10  GCCC3574-EMP-LANG               PIC X.                       
               10  GCCC3574-EMP-ADDRESS.                                        
                   15  GCCC3574-EMP-STREET         PIC X(50).                   
                   15  GCCC3574-EMP-CITY           PIC X(30).                   
                   15  GCCC3574-EMP-ADDR-PROV      PIC XX.                      
                   15  GCCC3574-EMP-POSTCODE       PIC X(10).                   
               10  GCCC3574-COVERAGE-INFO      OCCURS 6 TIMES.                  
                   15  GCCC3574-COV-BENEFIT        PIC X(4).                    
                   15  GCCC3574-COV-SEL-IND        PIC X.                       
                   15  GCCC3574-COV-SPOUS-IND      PIC X.                       
                   15  GCCC3574-COV-SPOUS-EFF-DATE.                             
                       20  GCCC3574-COV-SPOUS-EFDT-YYYY                         
                                                   PIC X(4).                    
                       20  GCCC3574-COV-SPOUS-EFDT-MM                           
                                                   PIC XX.                      
                       20  GCCC3574-COV-SPOUS-EFDT-DD                           
                                                   PIC XX.                      
                   15  GCCC3574-COV-SPOUS-PLAN     PIC X.                       
               10  GCCC3574-QUEBEC-AGE             PIC X.                       
               10  FILLER                          PIC X.                       
               10  GCCC3574-SPOUSE-DOB.                                         
                   15  GCCC3574-SPOUS-DOB-YYYY     PIC X(4).                    
                   15  GCCC3574-SPOUS-DOB-MM       PIC XX.                      
                   15  GCCC3574-SPOUS-DOB-DD       PIC XX.                      
               10  FILLER                          PIC X.                       
               10  GCCC3574-COMMON-LAW-DATE.                                    
                   15  GCCC3574-CL-DATE-YYYY       PIC X(4).                    
                   15  GCCC3574-CL-DATE-MM         PIC XX.                      
                   15  GCCC3574-CL-DATE-DD         PIC XX.                      
               10  GCCC3574-REGION                 PIC X.                       
               10  GCCC3574-SPOUSE-NAME            PIC X(28).                   
               10  GCCC3574-FAM-SPOUSE-DOB.                                     
                   15  GCCC3574-FAM-SPOUS-DOB-YYYY PIC X(4).                    
                   15  GCCC3574-FAM-SPOUS-DOB-MM   PIC XX.                      
                   15  GCCC3574-FAM-SPOUS-DOB-DD   PIC XX.                      
               10  GCCC3574-FAM-SPOUSE-GENDER      PIC X.                       
               10  GCCC3574-FAM-SPOUSE-RELSHP      PIC X.                       
               10  GCCC3574-CHILD-INFO       OCCURS 5.                          
                   15  GCCC3574-CHILD-NAME         PIC X(28).                   
                   15  GCCC3574-CHILD-DOB.                                      
                       20  GCCC3574-CHDOB-YYYY     PIC X(4).                    
                       20  GCCC3574-CHDOB-MM       PIC XX.                      
                       20  GCCC3574-CHDOB-DD       PIC XX.                      
                   15  GCCC3574-CHILD-GENDER       PIC X.                       
                   15  GCCC3574-CHILD-RELSHP       PIC X.                       
                   15  GCCC3574-CHILD-STUDENT      PIC X.                       
                   15  GCCC3574-CHILD-DISAB        PIC X.                       
               10  GCCC3574-BEN                    PIC X.                       
               10  GCCC3574-TODAY.                                              
                   15  GCCC3574-TODAY-YYYY         PIC X(4).                    
                   15  GCCC3574-TODAY-MM           PIC XX.                      
                   15  GCCC3574-TODAY-DD           PIC XX.                      
               10  GCCC3574-SECT10.                                             
                   15  GCCC3574-COMMENTS           PIC X(75).                   
               10  GCCC3574-SPOUSE-IND             PIC X.                       
               10  GCCC3574-COMN-LAW-IND           PIC X.                       
               10  GCCC3574-WAITING-PERIOD-IND     PIC X.                       
      * GCCC3574-WAITING-PERIOD-COND - REPLACED WITH FILLER                     
               10  FILLER                          PIC X.                       
               10  GCCC3574-HCSA-IND               PIC X.                       
               10  GCCC3574-HCSA-PLAN-NUMBER       PIC X(7).                    
               10  GCCC3574-HCSA-EFF-DATE.                                      
                   15  GCCC3574-HCSA-EFF-DATE-DD   PIC X(2).                    
                   15  GCCC3574-HCSA-EFF-DATE-MMM  PIC X(3).                    
                   15  GCCC3574-HCSA-EFF-DATE-YYYY PIC X(4).                    
                   15  FILLER                      PIC X(3).                    
               10  GCCC3574-HCSA-ALLOC-AMT         PIC 9(9)V99.                 
      * GCCC3574-HCSA-ALLOC-TYPE - REPLACED WITH FILLER                         
               10  FILLER                          PIC X.                       
               10  GCCC3574-BANK-NAME              PIC X(60).                   
               10  GCCC3574-BANK-TRANSIT           PIC X(5).                    
               10  GCCC3574-BANK-INSTITUTION       PIC X(3).                    
               10  GCCC3574-BANK-ACCOUNT           PIC X(12).                   
               10  GCCC3574-EMAIL-ADRS-WORK     PIC X(60).                      
               10  GCCC3574-EMAIL-ADRS-HOME     PIC X(60).                      
      *                                                                         
