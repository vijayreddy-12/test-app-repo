           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L M A S S                             *        
      *                                                                *        
      *   1. PRINT RECORD MASS CHANGE                                  *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L M A S S                             *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 010103 - JAK   NEW COPYBOOK                              *        
      *  02 - 100805 - IBM   TL124425 ADD NEW FILED ON GLMASS          *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GLMASS-RECORD.                                                       
           05  GLMASS-REC-TYPE                       PIC X.                     
           05  GLMASS-BANNER-CONTROL.                                           
               10  GLMASS-TAT                        PIC XX.                    
               10  GLMASS-BUS-SEG                    PIC X.                     
           05  GLMASS-CONFIRMATION                   PIC X(11).                 
           05  GLMASS-SEQ-NO                         PIC X(5).                  
      *                                                                         
           05  GLMASS-HEADING.                                                  
               10  GLMASS-HEAD-PLAN                  PIC X(7).                  
               10  GLMASS-DATE-TIME                  PIC X(60).                 
               10  GLMASS-LANG                       PIC X.                     
               10  GLMASS-HEAD-INFO.                                            
                   15  GLMASS-SPONSOR                PIC X(60).                 
                   15  GLMASS-ENGLISH-TODAY.                                    
                       20  GLMASS-ETODAY-DD          PIC XX.                    
                       20  GLMASS-ETODAY-SL1         PIC X.                     
                       20  GLMASS-ETODAY-MMM         PIC X(3).                  
                       20  GLMASS-ETODAY-SL2         PIC X.                     
                       20  GLMASS-ETODAY-YYYY        PIC X(4).                  
                       20  FILLER                    PIC X.                     
                   15  GLMASS-FRENCH-TODAY                                      
                               REDEFINES  GLMASS-ENGLISH-TODAY.                 
                       20  GLMASS-FTODAY-DD          PIC XX.                    
                       20  GLMASS-FTODAY-SL1         PIC X.                     
                       20  GLMASS-FTODAY-MMMM        PIC X(4).                  
                       20  GLMASS-FTODAY-SL2         PIC X.                     
                       20  GLMASS-FTODAY-YYYY        PIC X(4).                  
                   15  FILLER                        PIC X(56).                 
                                                                                
           05  GLMASS-DETAIL   REDEFINES    GLMASS-HEADING.                     
                   15  GLMASS-PLAN                   PIC X(7).                  
                   15  GLMASS-ACCOUNT                PIC XXX.                   
                   15  GLMASS-CERT                   PIC X(11).                 
                   15  GLMASS-MBR-NAME.                                         
                       20  GLMASS-MBR-SURNAME        PIC X(40).                 
                       20  GLMASS-MBR-FSTNAME        PIC X(30).                 
                       20  GLMASS-MBR-INITS          PIC X(5).                  
                   15  GLMASS-CHDT-DATE.                                        
                       20  GLMASS-CHDT-E.                                       
                           25 GLMASS-CHDT-EDD        PIC XX.                    
                           25 GLMASS-CHDT-ESL1       PIC X.                     
                           25 GLMASS-CHDT-EMMM       PIC X(3).                  
                           25 GLMASS-CHDT-ESL2       PIC X.                     
                           25 GLMASS-CHDT-EYYYY      PIC X(4).                  
                           25 FILLER                 PIC X.                     
                       20 GLMASS-CHDT-F                                         
                                  REDEFINES GLMASS-CHDT-E.                      
                           25 GLMASS-CHDT-FDD        PIC XX.                    
                           25 GLMASS-CHDT-FSL1       PIC X.                     
                           25 GLMASS-CHDT-FMMMM      PIC X(4).                  
                           25 GLMASS-CHDT-FSL2       PIC X.                     
                           25 GLMASS-CHDT-FYYYY      PIC X(4).                  
                   15  GLMASS-TERM-REASON            PIC XX.                    
                   15  GLMASS-RETURN-DATE.                                      
                       20  GLMASS-RETURN-DT-E.                                  
                           25 GLMASS-RETURN-DT-EDD   PIC XX.                    
                           25 GLMASS-RETURN-DT-ESL1  PIC X.                     
                           25 GLMASS-RETURN-DT-EMMM  PIC X(3).                  
                           25 GLMASS-RETURN-DT-ESL2  PIC X.                     
                           25 GLMASS-RETURN-DT-EYYYY PIC X(4).                  
                           25 FILLER                 PIC X.                     
                       20 GLMASS-RETURN-DT-F                                    
                                  REDEFINES GLMASS-RETURN-DT-E.                 
                           25 GLMASS-RETURN-DT-FDD   PIC XX.                    
                           25 GLMASS-RETURN-DT-FSL1  PIC X.                     
                           25 GLMASS-RETURN-DT-FMMMM PIC X(4).                  
                           25 GLMASS-RETURN-DT-FSL2  PIC X.                     
                           25 GLMASS-RETURN-DT-FYYYY PIC X(4).                  
                   15  GLMASS-SALARY-CHG     PIC ZZ,ZZZ,ZZ9.99.                 
                   15  GLMASS-SALARY-FREQ            PIC X.                     
                   15  GLMASS-WORKING-HOURS          PIC Z9.9.                  
                   15  GLMASS-OCCUPATION-CHG         PIC X(29).                 
                   15  GLMASS-CLASS.                                            
                       20  GLMASS-CLASS-NEW          PIC XXX.                   
                       20  GLMASS-CLASS-OLD          PIC XXX.                   
                   15  GLMASS-ACCT-CHG               PIC XXX.                   
                   15  GLMASS-DIVISION.                                         
                       20  GLMASS-DIV-NEW            PIC XXX.                   
                       20  GLMASS-DIV-OLD            PIC XXX.                   
                   15  GLMASS-EVIDENCE-REQD          PIC X.                     
                   15  GLMASS-MAILED                 PIC X.                     
      *                                                                         
                   15  GLMASS-REGION                 PIC X(5).                  
      *                                                                         
           05  GLMASS-TRAILER   REDEFINES   GLMASS-HEADING.                     
               10  GLMASS-TRAILER-PRTLINE            PIC X(70).                 
      *                                                                         
           05  FILLER                                PIC X(31).                 
