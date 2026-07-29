           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C 2 9 7 1                         *        
      *                                                                *        
      *   1. AFP PRINT, ENROLMENT/RE-ENROLMENT APPLICATION             *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C 2 9 7 1                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE       PGR   DESCRIPTION                             *        
      *  --   ------     ---   --------------------------------------  *        
      *                                                                *        
      *  00 - 001031 -   JAK   NEW COPYBOOK                            *        
      *                                                                *        
      *  01 - 010122 -   JAK   CHANGES V3.1                            *        
      *                                                                *        
      *  02 - 170901 -   JE    CHANGES 3.2 - 2 FIELDS NO LONGER USED   *        
      *                        CHANGED TO FILLER                       *        
      *                        2 FIELDS ADDED TO END OF COPYBOOK       *        
      *                                                                *        
      *  03 - 20020528 - JVH   ECOMM 4.2                               *        
      *                        2 FIELDS ADDED TO END OF COPYBOOK       *        
      *                                                                *        
      *  04 - 20031022 - JW    GBSS TASK 26184 - CHANGES TO GL2971     *        
      *  05 - 20150515 - IBM   TL 236472 ADD BANK DEAILS AND EMAIL IDS *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCC2971-RECORD.                                                     
           05  GCCC2971-HEADER.                                                 
               10  GCCC2971-CONF-NBR               PIC 9(11).                   
               10  GCCC2971-SEQ-NBR                PIC 9(5).                    
               10  GCCC2971-FORM-NBR               PIC X(8).                    
               10  GCCC2971-LANG                   PIC X.                       
               10  GCCC2971-CUST-GROUP-NBR         PIC X(7).                    
               10  GCCC2971-CUST-DIV               PIC X(3).                    
               10  GCCC2971-CUST-CERT-NBR          PIC X(10).                   
               10  GCCC2971-WEB-TIMESTAMP.                                      
                   15  GCCC2971-WEB-YYYY           PIC X(4).                    
                   15  GCCC2971-WEB-MM             PIC XX.                      
                   15  GCCC2971-WEB-DD             PIC XX.                      
                   15  GCCC2971-WEB-HH             PIC XX.                      
                   15  GCCC2971-WEB-MIN            PIC XX.                      
                   15  GCCC2971-WEB-SS             PIC XX.                      
                   15  GCCC2971-WEB-NN             PIC XX.                      
           05  GCCC2971-DETAIL.                                                 
               10  GCCC2971-DATE-TIME              PIC X(60).                   
               10  GCCC2971-GEN-DATA   OCCURS 5.                                
                   15  GCCC2971-PLAN               PIC X(7).                    
                   15  GCCC2971-ACCOUNT            PIC X(3).                    
                   15  GCCC2971-DIV                PIC XXX.                     
                   15  GCCC2971-CLASS              PIC XXX.                     
               10  GCCC2971-CERT                   PIC X(11).                   
               10  GCCC2971-SPONSOR                PIC X(60).                   
               10  GCCC2971-HIRE-DATE.                                          
                   15  GCCC2971-HIRE-DT-YYYY       PIC X(4).                    
                   15  GCCC2971-HIRE-DT-MM         PIC XX.                      
                   15  GCCC2971-HIRE-DT-DD         PIC XX.                      
               10  GCCC2971-PREV-EMPL-DATE.                                     
                   15  GCCC2971-PE-DT-YYYY         PIC X(4).                    
                   15  GCCC2971-PE-DT-MM           PIC XX.                      
                   15  GCCC2971-PE-DT-DD           PIC XX.                      
               10  GCCC2971-REHIRE-DATE.                                        
                   15  GCCC2971-REHIRE-DT-YYYY     PIC X(4).                    
                   15  GCCC2971-REHIRE-DT-MM       PIC XX.                      
                   15  GCCC2971-REHIRE-DT-DD       PIC XX.                      
               10  GCCC2971-OCC                    PIC X(29).                   
               10  GCCC2971-HOURS-X.                                            
                   15  GCCC2971-HOURS              PIC 9(4)V99.                 
               10  GCCC2971-EARNINGS-X.                                         
                   15  GCCC2971-EARNINGS           PIC 9(11)V99.                
               10  GCCC2971-EVIDENCE               PIC X.                       
      * GCCC2971-MAILED NO LONGER USED - REPLACED WITH FILLER                   
               10  FILLER                          PIC X.                       
               10  GCCC2971-EMP-NAME.                                           
                   15  GCCC2971-EMP-SURNAME        PIC X(40).                   
                   15  GCCC2971-EMP-FSTNAME        PIC X(30).                   
                   15  GCCC2971-EMP-INITS          PIC X(5).                    
               10  GCCC2971-EMP-DOB.                                            
                   15  GCCC2971-EMP-DOB-YYYY       PIC X(4).                    
                   15  GCCC2971-EMP-DOB-MM         PIC XX.                      
                   15  GCCC2971-EMP-DOB-DD         PIC XX.                      
               10  GCCC2971-EMP-GENDER             PIC X.                       
               10  GCCC2971-EMP-PROV               PIC XX.                      
               10  GCCC2971-EMP-LANG               PIC X.                       
               10  GCCC2971-EMP-ADDRESS.                                        
                   15  GCCC2971-EMP-STREET         PIC X(50).                   
                   15  GCCC2971-EMP-CITY           PIC X(30).                   
                   15  GCCC2971-EMP-ADDR-PROV      PIC XX.                      
                   15  GCCC2971-EMP-POSTCODE       PIC X(10).                   
               10  GCCC2971-COVERAGE-INFO      OCCURS 6 TIMES.                  
                   15  GCCC2971-COV-BENEFIT        PIC X(4).                    
                   15  GCCC2971-COV-SEL-IND        PIC X.                       
                   15  GCCC2971-COV-SPOUS-IND      PIC X.                       
                   15  GCCC2971-COV-SPOUS-EFF-DATE.                             
                       20  GCCC2971-COV-SPOUS-EFDT-YYYY                         
                                                   PIC X(4).                    
                       20  GCCC2971-COV-SPOUS-EFDT-MM                           
                                                   PIC XX.                      
                       20  GCCC2971-COV-SPOUS-EFDT-DD                           
                                                   PIC XX.                      
                   15  GCCC2971-COV-SPOUS-PLAN     PIC X.                       
               10  GCCC2971-QUEBEC-AGE             PIC X.                       
      * GCCC2971-SPOUSE-GENDER NO LONGER USED - REPLACED BY FILLER              
               10  FILLER                          PIC X.                       
               10  GCCC2971-SPOUSE-DOB.                                         
                   15  GCCC2971-SPOUS-DOB-YYYY     PIC X(4).                    
                   15  GCCC2971-SPOUS-DOB-MM       PIC XX.                      
                   15  GCCC2971-SPOUS-DOB-DD       PIC XX.                      
      * GCCC2971-SPOUSE-STATUS NO LONGER USED -REPLACED BY FILLER               
               10  FILLER                          PIC X.                       
               10  GCCC2971-COMMON-LAW-DATE.                                    
                   15  GCCC2971-CL-DATE-YYYY       PIC X(4).                    
                   15  GCCC2971-CL-DATE-MM         PIC XX.                      
                   15  GCCC2971-CL-DATE-DD         PIC XX.                      
               10  GCCC2971-REGION                 PIC X.                       
               10  GCCC2971-SPOUSE-NAME            PIC X(28).                   
               10  GCCC2971-FAM-SPOUSE-DOB.                                     
                   15  GCCC2971-FAM-SPOUS-DOB-YYYY PIC X(4).                    
                   15  GCCC2971-FAM-SPOUS-DOB-MM   PIC XX.                      
                   15  GCCC2971-FAM-SPOUS-DOB-DD   PIC XX.                      
               10  GCCC2971-FAM-SPOUSE-GENDER      PIC X.                       
               10  GCCC2971-FAM-SPOUSE-RELSHP      PIC X.                       
               10  GCCC2971-CHILD-INFO       OCCURS 8.                          
                   15  GCCC2971-CHILD-NAME         PIC X(28).                   
                   15  GCCC2971-CHILD-DOB.                                      
                       20  GCCC2971-CHDOB-YYYY     PIC X(4).                    
                       20  GCCC2971-CHDOB-MM       PIC XX.                      
                       20  GCCC2971-CHDOB-DD       PIC XX.                      
                   15  GCCC2971-CHILD-GENDER       PIC X.                       
                   15  GCCC2971-CHILD-RELSHP       PIC X.                       
                   15  GCCC2971-CHILD-STUDENT      PIC X.                       
                   15  GCCC2971-CHILD-DISAB        PIC X.                       
      * GCCC2971-BEN NO LONGER USED - REPLACED WITH FILLER                      
               10  FILLER                          PIC X.                       
               10  GCCC2971-TODAY.                                              
                   15  GCCC2971-TODAY-YYYY         PIC X(4).                    
                   15  GCCC2971-TODAY-MM           PIC XX.                      
                   15  GCCC2971-TODAY-DD           PIC XX.                      
               10  GCCC2971-SECT10.                                             
                   15  GCCC2971-COMMENTS           PIC X(75).                   
      * 2 FIELDS ADDED FOR RELEASE 3.2                                          
               10  GCCC2971-SPOUSE-IND             PIC X.                       
               10  GCCC2971-COMN-LAW-IND           PIC X.                       
      * 2 FIELDS ADDED FOR RELEASE 4.2                                          
               10  GCCC2971-WAITING-PERIOD-IND     PIC X.                       
      * GCCC2971-WAITING-PERIOD-COND - REPLACED WITH FILLER                     
               10  FILLER                          PIC X.                       
               10  GCCC2971-BANK-NAME              PIC X(60).                   
               10  GCCC2971-BANK-TRANSIT           PIC X(5).                    
               10  GCCC2971-BANK-INSTITUTION       PIC X(3).                    
               10  GCCC2971-BANK-ACCOUNT           PIC X(12).                   
               10  GCCC2971-EMAIL-ADDRESS-WORK     PIC X(60).                   
               10  GCCC2971-EMAIL-ADDRESS-HOME     PIC X(60).                   
      *                                                                         
