           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L 2 9 7 1 PAGE 2                      *        
      *                                                                *        
      *   1. AFP PRINT, ENROLMENT/RE-ENROLMENT APPLICATION             *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 2 9 7 1 PAGE 2                      *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      *  01 - 031022 - JW    GBSS TASK 26184 - CHANGES TO GL2971       *        
      *  02 - 150515 - IBM   TL 236472 ADD BANK DEAILS AND EMAIL IDS   *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL2971-RECORD.                                                       
           05  GL2971-CC-P2                            PIC X.                   
           05  GL2971-FORM-TYPE-P2                     PIC XX.                  
      *                                                                         
           05  GL2971-PAGE-2.                                                   
               10  GL2971-FAMILY-INFO.                                          
                   15  GL2971-SPOUSE-NAME              PIC X(28).               
                   15  GL2971-FAMILY-SPOUSE-DOB.                                
                       20  GL2971-FAM-SPOUSE-DOB-E.                             
                           25 GL2971-FAM-SPOUSE-DOB-EDD  PIC XX.                
                           25 GL2971-FAM-SPOUSE-DOB-ESL1 PIC X.                 
                           25 GL2971-FAM-SPOUSE-DOB-EMMM PIC X(3).              
                           25 GL2971-FAM-SPOUSE-DOB-ESL2 PIC X.                 
                           25 GL2971-FAM-SPOUSE-DOB-EYYYY PIC X(4).             
                           25 FILLER                     PIC X.                 
                       20  GL2971-FAM-SPOUSE-DOB-F                              
                                   REDEFINES GL2971-FAM-SPOUSE-DOB-E.           
                           25 GL2971-FAM-SPOUSE-DOB-FDD  PIC XX.                
                           25 GL2971-FAM-SPOUSE-DOB-FSL1 PIC X.                 
                           25 GL2971-FAM-SPOUSE-DOB-FMMMM PIC X(4).             
                           25 GL2971-FAM-SPOUSE-DOB-FSL2 PIC X.                 
                           25 GL2971-FAM-SPOUSE-DOB-FYYYY PIC X(4).             
      *                                                                         
                   15  GL2971-FAM-SPOUSE-GENDER.                                
                       20  GL2971-FAM-SPOUSE-MALE      PIC X.                   
                       20  GL2971-FAM-SPOUSE-FEMALE    PIC X.                   
                   15  GL2971-FAM-SPOUSE-RELSHP        PIC X.                   
                   15  GL2971-NAME-CHILD    OCCURS 8                            
                                                       PIC X(28).               
                   15  GL2971-DOB-CHILD     OCCURS 8.                           
                       20  GL2971-CHILD-DOB-E.                                  
                           25  GL2971-CHILD-DOB-EDD                             
                                                       PIC XX.                  
                           25  GL2971-CHILD-DOB-ESL1                            
                                                       PIC X.                   
                           25  GL2971-CHILD-DOB-EMMM                            
                                                       PIC X(3).                
                           25  GL2971-CHILD-DOB-ESL2                            
                                                       PIC X.                   
                           25  GL2971-CHILD-DOB-EYYYY                           
                                                       PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL2971-CHILD-DOB-F                                   
                                         REDEFINES  GL2971-CHILD-DOB-E.         
                           25  GL2971-CHILD-DOB-FDD                             
                                                       PIC XX.                  
                           25  GL2971-CHILD-DOB-FSL1                            
                                                       PIC X.                   
                           25  GL2971-CHILD-DOB-FMMMM                           
                                                       PIC X(4).                
                           25  GL2971-CHILD-DOB-FSL2                            
                                                       PIC X.                   
                           25  GL2971-CHILD-DOB-FYYYY                           
                                                       PIC X(4).                
      *                                                                         
      *                                                                         
                   15  GL2971-CHILD-GENDER     OCCURS 8.                        
                       20  GL2971-CHILD-MALE           PIC X.                   
                       20  GL2971-CHILD-FEMALE         PIC X.                   
      *                                                                         
                   15  GL2971-CHILD-RELSHP     OCCURS 8                         
                                                       PIC X.                   
      *                                                                         
                   15  GL2971-CHILD-STUDENT    OCCURS 8.                        
                       20  GL2971-CHILD-STUDENT-YES    PIC X.                   
                       20  GL2971-CHILD-STUDENT-NO     PIC X.                   
      *                                                                         
                   15  GL2971-CHILD-DISABLED   OCCURS 8.                        
                       20  GL2971-CHILD-DISABLED-YES   PIC X.                   
                       20  GL2971-CHILD-DISABLED-NO    PIC X.                   
      *                                                                         
               10  GL2971-SECT8.                                                
                   15  GL2971-DIRECT-DEPOSIT-DETAILS.                           
                       20 GL2971-BANK-NAME           PIC X(60).                 
                       20 GL2971-BANK-TRANSIT        PIC X(5).                  
                       20 GL2971-BANK-INSTITUTION    PIC X(3).                  
                       20 GL2971-BANK-ACCOUNT        PIC X(12).                 
                   15  GL2971-EMAIL.                                            
                       20 GL2971-EMAIL-ADRS-WORK  PIC X(60).                    
                       20 GL2971-EMAIL-ADRS-HOME  PIC X(60).                    
      *                                                                         
               10  GL2971-SECT9.                                                
      *            GL2971-BENEFICIARY-REQ NO LONGER USED.                       
                   15  FILLER                          PIC X.                   
      *                                                                         
               10  GL2971-SECT10.                                               
                   15  GL2971-ENGLISH-TODAY.                                    
                       20  GL2971-ETODAY-DD            PIC XX.                  
                       20  GL2971-ETODAY-SL1           PIC X.                   
                       20  GL2971-ETODAY-MMM           PIC X(3).                
                       20  GL2971-ETODAY-SL2           PIC X.                   
                       20  GL2971-ETODAY-YYYY          PIC X(4).                
                       20  FILLER                      PIC X.                   
                   15  GL2971-FRENCH-TODAY                                      
                               REDEFINES  GL2971-ENGLISH-TODAY.                 
                       20  GL2971-FTODAY-DD            PIC XX.                  
                       20  GL2971-FTODAY-SL1           PIC X.                   
                       20  GL2971-FTODAY-MMMM          PIC X(4).                
                       20  GL2971-FTODAY-SL2           PIC X.                   
                       20  GL2971-FTODAY-YYYY          PIC X(4).                
      *                                                                         
               10  GL2971-SECT11.                                               
                   15  GL2971-COMMENTS                 PIC X(75).               
                   15  GL2971-REGION-P2                PIC X(5).                
      *                                                                         
