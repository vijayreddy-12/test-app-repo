           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L 3 5 7 4 PAGE 2                      *        
      *                                                                *        
      *   1. AFP PRINT, ENROLMENT/RE-ENROLMENT APPLICATION (HCSA)      *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 3 5 7 4 PAGE 2                      *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE      PGR   DESCRIPTION                              *        
      *  --   ------    ---   ---------------------------------------  *        
      *                                                                *        
      *  00   20020529  JVH   NEW COPYBOOK                             *        
      *  01 - 20150515  IBM   TL 236472 ADD BANK DEAILS AND EMAIL ADRS *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL3574-RECORD.                                                       
           05  GL3574-CC-P2                            PIC X.                   
           05  GL3574-FORM-TYPE-P2                     PIC XX.                  
      *                                                                         
           05  GL3574-PAGE-2.                                                   
               10  GL3574-SECT5.                                                
                   15  GL3574-SPOUSE-IND               PIC X.                   
      *                                                                         
                   15  GL3574-SPOUSE-HEALTH-COV.                                
                       20  GL3574-SPOUSE-HLTH-COV-YES  PIC X.                   
                       20  GL3574-SPOUSE-HLTH-COV-NO   PIC X.                   
      *                                                                         
                   15  GL3574-SPOUSE-HC-EFF-DATE.                               
                       20 GL3574-SPOUSE-HC-EFF-EDATE.                           
                           25 GL3574-SPOUSE-HC-EFF-EDD  PIC XX.                 
                           25 GL3574-SPOUSE-HC-EFF-ESL1 PIC X.                  
                           25 GL3574-SPOUSE-HC-EFF-EMMM PIC X(3).               
                           25 GL3574-SPOUSE-HC-EFF-ESL2 PIC X.                  
                           25 GL3574-SPOUSE-HC-EFF-EYYYY PIC X(4).              
                           25 FILLER                    PIC X.                  
                       20 GL3574-SPOUSE-HC-EFF-FDATE                            
                                REDEFINES GL3574-SPOUSE-HC-EFF-EDATE.           
                           25 GL3574-SPOUSE-HC-EFF-FDD  PIC XX.                 
                           25 GL3574-SPOUSE-HC-EFF-FSL1 PIC X.                  
                           25 GL3574-SPOUSE-HC-EFF-FMMMM PIC X(4).              
                           25 GL3574-SPOUSE-HC-EFF-FSL2 PIC X.                  
                           25 GL3574-SPOUSE-HC-EFF-FYYYY PIC X(4).              
                   15  GL3574-SPOUSE-HC-SCOPE.                                  
                       20  GL3574-SPOUSE-HC-SCOPE-1    PIC X.                   
                       20  GL3574-SPOUSE-HC-SCOPE-2    PIC X.                   
                       20  GL3574-SPOUSE-HC-SCOPE-3    PIC X.                   
                       20  GL3574-SPOUSE-HC-SCOPE-4    PIC X.                   
                   15  GL3574-SPOUSE-DENT-COV.                                  
                       20  GL3574-SPOUSE-DENT-COV-Y    PIC X.                   
                       20  GL3574-SPOUSE-DENT-COV-N    PIC X.                   
                   15  GL3574-SPOUSE-DC-EFF-DATE.                               
                       20 GL3574-SPOUSE-DC-EFF-EDATE.                           
                           25 GL3574-SPOUSE-DC-EFF-EDD  PIC XX.                 
                           25 GL3574-SPOUSE-DC-EFF-ESL1 PIC X.                  
                           25 GL3574-SPOUSE-DC-EFF-EMMM PIC X(3).               
                           25 GL3574-SPOUSE-DC-EFF-ESL2 PIC X.                  
                           25 GL3574-SPOUSE-DC-EFF-EYYYY PIC X(4).              
                           25 FILLER                    PIC X.                  
                       20 GL3574-SPOUSE-DC-EFF-FDATE                            
                                REDEFINES GL3574-SPOUSE-DC-EFF-EDATE.           
                           25 GL3574-SPOUSE-DC-EFF-FDD  PIC XX.                 
                           25 GL3574-SPOUSE-DC-EFF-FSL1 PIC X.                  
                           25 GL3574-SPOUSE-DC-EFF-FMMMM PIC X(4).              
                           25 GL3574-SPOUSE-DC-EFF-FSL2 PIC X.                  
                           25 GL3574-SPOUSE-DC-EFF-FYYYY PIC X(4).              
                   15  GL3574-SPOUSE-DC-SCOPE.                                  
                       20  GL3574-SPOUSE-DC-SCOPE-1    PIC X.                   
                       20  GL3574-SPOUSE-DC-SCOPE-2    PIC X.                   
                       20  GL3574-SPOUSE-DC-SCOPE-3    PIC X.                   
                       20  GL3574-SPOUSE-DC-SCOPE-4    PIC X.                   
      *                                                                         
                   15  GL3574-SPOUSE-DOB.                                       
                       20 GL3574-SPOUSE-DOB-E.                                  
                           25 GL3574-SPOUSE-DOB-EDD    PIC XX.                  
                           25 GL3574-SPOUSE-DOB-ESL1   PIC X.                   
                           25 GL3574-SPOUSE-DOB-EMMM   PIC X(3).                
                           25 GL3574-SPOUSE-DOB-ESL2   PIC X.                   
                           25 GL3574-SPOUSE-DOB-EYYYY  PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3574-SPOUSE-DOB-F                                   
                                      REDEFINES GL3574-SPOUSE-DOB-E.            
                           25 GL3574-SPOUSE-DOB-FDD    PIC XX.                  
                           25 GL3574-SPOUSE-DOB-FSL1   PIC X.                   
                           25 GL3574-SPOUSE-DOB-FMMMM  PIC X(4).                
                           25 GL3574-SPOUSE-DOB-FSL2   PIC X.                   
                           25 GL3574-SPOUSE-DOB-FYYYY  PIC X(4).                
      *                                                                         
                   15  GL3574-COMN-LAW-IND.                                     
                       20  GL3574-COMN-LAW-IND-YES     PIC X.                   
                       20  GL3574-COMN-LAW-IND-NO      PIC X.                   
                   15  GL3574-RELATIONSHIP-START-DATE.                          
                       20 GL3574-REL-ST-EFF-EDATE.                              
                           25 GL3574-REL-ST-EDD        PIC XX.                  
                           25 GL3574-REL-ST-ESL1       PIC X.                   
                           25 GL3574-REL-ST-EMMM       PIC X(3).                
                           25 GL3574-REL-ST-ESL2       PIC X.                   
                           25 GL3574-REL-ST-EYYYY      PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3574-REL-ST-EFF-FDATE                               
                                  REDEFINES GL3574-REL-ST-EFF-EDATE.            
                           25 GL3574-REL-ST-FDD        PIC XX.                  
                           25 GL3574-REL-ST-FSL1       PIC X.                   
                           25 GL3574-REL-ST-FMMMM      PIC X(4).                
                           25 GL3574-REL-ST-FSL2       PIC X.                   
                           25 GL3574-REL-ST-FYYYY      PIC X(4).                
      *                                                                         
               10  GL3574-SECT6.                                                
                   15  GL3574-QUEBEC-AGE.                                       
                       20  GL3574-QUEBEC-AGE-BOX1      PIC X.                   
                       20  GL3574-QUEBEC-AGE-BOX2      PIC X.                   
      *                                                                         
               10  GL3574-FAMILY-INFO.                                          
                   15  GL3574-SPOUSE-NAME              PIC X(28).               
                   15  GL3574-FAMILY-SPOUSE-DOB.                                
                       20  GL3574-FAM-SPOUSE-DOB-E.                             
                           25 GL3574-FAM-SPOUSE-DOB-EDD  PIC XX.                
                           25 GL3574-FAM-SPOUSE-DOB-ESL1 PIC X.                 
                           25 GL3574-FAM-SPOUSE-DOB-EMMM PIC X(3).              
                           25 GL3574-FAM-SPOUSE-DOB-ESL2 PIC X.                 
                           25 GL3574-FAM-SPOUSE-DOB-EYYYY PIC X(4).             
                           25 FILLER                     PIC X.                 
                       20  GL3574-FAM-SPOUSE-DOB-F                              
                                   REDEFINES GL3574-FAM-SPOUSE-DOB-E.           
                           25 GL3574-FAM-SPOUSE-DOB-FDD  PIC XX.                
                           25 GL3574-FAM-SPOUSE-DOB-FSL1 PIC X.                 
                           25 GL3574-FAM-SPOUSE-DOB-FMMMM PIC X(4).             
                           25 GL3574-FAM-SPOUSE-DOB-FSL2 PIC X.                 
                           25 GL3574-FAM-SPOUSE-DOB-FYYYY PIC X(4).             
      *                                                                         
                   15  GL3574-FAM-SPOUSE-GENDER.                                
                       20  GL3574-FAM-SPOUSE-MALE      PIC X.                   
                       20  GL3574-FAM-SPOUSE-FEMALE    PIC X.                   
                   15  GL3574-FAM-SPOUSE-RELSHP        PIC X.                   
                   15  GL3574-NAME-CHILD    OCCURS 5                            
                                                       PIC X(28).               
                   15  GL3574-DOB-CHILD     OCCURS 5.                           
                       20  GL3574-CHILD-DOB-E.                                  
                           25  GL3574-CHILD-DOB-EDD                             
                                                       PIC XX.                  
                           25  GL3574-CHILD-DOB-ESL1                            
                                                       PIC X.                   
                           25  GL3574-CHILD-DOB-EMMM                            
                                                       PIC X(3).                
                           25  GL3574-CHILD-DOB-ESL2                            
                                                       PIC X.                   
                           25  GL3574-CHILD-DOB-EYYYY                           
                                                       PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL3574-CHILD-DOB-F                                   
                                         REDEFINES  GL3574-CHILD-DOB-E.         
                           25  GL3574-CHILD-DOB-FDD                             
                                                       PIC XX.                  
                           25  GL3574-CHILD-DOB-FSL1                            
                                                       PIC X.                   
                           25  GL3574-CHILD-DOB-FMMMM                           
                                                       PIC X(4).                
                           25  GL3574-CHILD-DOB-FSL2                            
                                                       PIC X.                   
                           25  GL3574-CHILD-DOB-FYYYY                           
                                                       PIC X(4).                
      *                                                                         
                   15  GL3574-CHILD-GENDER     OCCURS 5.                        
                       20  GL3574-CHILD-MALE           PIC X.                   
                       20  GL3574-CHILD-FEMALE         PIC X.                   
      *                                                                         
                   15  GL3574-CHILD-RELSHP     OCCURS 5                         
                                                       PIC X.                   
      *                                                                         
                   15  GL3574-CHILD-STUDENT    OCCURS 5.                        
                       20  GL3574-CHILD-STUDENT-YES    PIC X.                   
                       20  GL3574-CHILD-STUDENT-NO     PIC X.                   
      *                                                                         
                   15  GL3574-CHILD-DISABLED   OCCURS 5.                        
                       20  GL3574-CHILD-DISABLED-YES   PIC X.                   
                       20  GL3574-CHILD-DISABLED-NO    PIC X.                   
      *                                                                         
               10  GL3574-SECT8.                                                
                   15  GL3574-DIRECT-DEPOSIT-DETAILS.                           
                       20 GL3574-BANK-NAME           PIC X(60).                 
                       20 GL3574-BANK-TRANSIT        PIC X(5).                  
                       20 GL3574-BANK-INSTITUTION    PIC X(3).                  
                       20 GL3574-BANK-ACCOUNT        PIC X(12).                 
                   15  GL3574-EMAIL.                                            
                       20 GL3574-EMAIL-ADRS-WORK  PIC X(60).                    
                       20 GL3574-EMAIL-ADRS-HOME  PIC X(60).                    
      *                                                                         
               10  GL3574-SECT9.                                                
                   15  GL3574-BENEFICIARY-REQ          PIC X.                   
      *                                                                         
               10  GL3574-SECT10.                                               
                   15  GL3574-ENGLISH-TODAY.                                    
                       20  GL3574-ETODAY-DD            PIC XX.                  
                       20  GL3574-ETODAY-SL1           PIC X.                   
                       20  GL3574-ETODAY-MMM           PIC X(3).                
                       20  GL3574-ETODAY-SL2           PIC X.                   
                       20  GL3574-ETODAY-YYYY          PIC X(4).                
                       20  FILLER                      PIC X.                   
                   15  GL3574-FRENCH-TODAY                                      
                               REDEFINES  GL3574-ENGLISH-TODAY.                 
                       20  GL3574-FTODAY-DD            PIC XX.                  
                       20  GL3574-FTODAY-SL1           PIC X.                   
                       20  GL3574-FTODAY-MMMM          PIC X(4).                
                       20  GL3574-FTODAY-SL2           PIC X.                   
                       20  GL3574-FTODAY-YYYY          PIC X(4).                
      *                                                                         
               10  GL3574-SECT11.                                               
                   15  GL3574-COMMENTS                 PIC X(75).               
                   15  GL3574-REGION-P2                PIC X(5).                
      *                                                                         
