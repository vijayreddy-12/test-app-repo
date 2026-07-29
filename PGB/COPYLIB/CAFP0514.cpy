      ******************************************************************        
      *                                                                *        
      *                        G L 0 5 1 4                             *        
      *                                                                *        
      *   1. AFP PRINT, REQUEST FOR, OR TERMINATION OF OVER-AGE        *        
      *      DEPENDENT COVERAGE                                        *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 0 5 1 4                             *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL0514-RECORD.                                                       
           03  GL0514-CC                               PIC X.                   
           03  GL0514-DATA.                                                     
           05  GL0514-FORM-TYPE                        PIC XX.                  
           05  GL0514-PAGE-1.                                                   
               10  GL0514-DATE-TIME                    PIC X(60).               
               10  GL0514-SELECT-OVERAGE.                                       
                   15  GL0514-SELECT-REQUEST           PIC X.                   
                   15  GL0514-SELECT-TERM-REQ          PIC X.                   
               10  GL0514-GEN-INFO.                                             
                   15  GL0514-SPONSOR                  PIC X(60).               
                   15  GL0514-PLAN     OCCURS 2        PIC X(7).                
                   15  GL0514-ACCT     OCCURS 2        PIC X(3).                
                   15  GL0514-DIV      OCCURS 2        PIC X(3).                
                   15  GL0514-CERT                     PIC X(11).               
                   15  GL0514-MBR-LAST-NAME            PIC X(40).               
                   15  GL0514-MBR-FIRST-NAME           PIC X(30).               
                   15  GL0514-MBR-MID-INIT             PIC X(5).                
                   15  GL0514-MBR-ADDRESS              PIC X(50).               
                   15  GL0514-MBR-CITY                 PIC X(20).               
                   15  GL0514-MBR-PROV                 PIC XX.                  
                   15  GL0514-MBR-POSTCODE             PIC X(10).               
                   15  GL0514-DEP-LAST-NAME            PIC X(21).               
                   15  GL0514-DEP-FIRST-NAME           PIC X(21).               
                   15  GL0514-DEP-RELSHP               PIC X(15).               
                   15  GL0514-DEP-DOB.                                          
                       20  GL0514-DEP-EDOB.                                     
                           25  GL0514-DEP-EDOB-DD      PIC XX.                  
                           25  GL0514-DEP-EDOB-SL1     PIC X.                   
                           25  GL0514-DEP-EDOB-MMM     PIC X(3).                
                           25  GL0514-DEP-EDOB-SL2     PIC X.                   
                           25  GL0514-DEP-EDOB-YYYY    PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL0514-DEP-FDOB                                      
                                REDEFINES  GL0514-DEP-EDOB.                     
                           25  GL0514-DEP-FDOB-DD      PIC XX.                  
                           25  GL0514-DEP-FDOB-SL1     PIC X.                   
                           25  GL0514-DEP-FDOB-MMMM    PIC X(4).                
                           25  GL0514-DEP-FDOB-SL2     PIC X.                   
                           25  GL0514-DEP-FDOB-YYYY    PIC X(4).                
                   15  GL0514-DEP-GENDER.                                       
                       20  GL0514-DEP-MALE             PIC X.                   
                       20  GL0514-DEP-FEMALE           PIC X.                   
                   15  GL0514-DEP-ADDRESS              PIC X(50).               
                   15  GL0514-DEP-CITY                 PIC X(20).               
                   15  GL0514-DEP-PROV                 PIC XX.                  
                   15  GL0514-DEP-POSTCODE             PIC X(10).               
      *                                                                         
               10  GL0514-DIS-DEP-INFO.                                         
                   15  GL0514-DEP-RESIDENT.                                     
                       20  GL0514-DEP-RES-YES          PIC X.                   
                       20  GL0514-DEP-RES-NO           PIC X.                   
                   15  GL0514-DEP-RES-EXPL             PIC X(170).              
                   15  GL0514-DEP-EMPLOYED.                                     
                       20  GL0514-DEP-EMPL-YES         PIC X.                   
                       20  GL0514-DEP-EMPL-NO          PIC X.                   
                   15  GL0514-DEP-DATE-EMPLOYED.                                
                       20  GL0514-DEP-EDTE-EMPL.                                
                           25  GL0514-DEP-EEMPL-DD     PIC XX.                  
                           25  GL0514-DEP-EEMPL-SL1    PIC X.                   
                           25  GL0514-DEP-EEMPL-MMM    PIC X(3).                
                           25  GL0514-DEP-EEMPL-SL2    PIC X.                   
                           25  GL0514-DEP-EEMPL-YYYY   PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL0514-DEP-FDTE-EMPL                                 
                                REDEFINES  GL0514-DEP-EDTE-EMPL.                
                           25  GL0514-DEP-FEMPL-DD     PIC XX.                  
                           25  GL0514-DEP-FEMPL-SL1    PIC X.                   
                           25  GL0514-DEP-FEMPL-MMMM   PIC X(4).                
                           25  GL0514-DEP-FEMPL-SL2    PIC X.                   
                           25  GL0514-DEP-FEMPL-YYYY   PIC X(4).                
                   15  GL0514-DEP-EMPL-TYPE            PIC X(60).               
                   15  GL0514-DEP-GOVT-ELIG.                                    
                       20  GL0514-DEP-GOV-ELIG-YES     PIC X.                   
                       20  GL0514-DEP-GOV-ELIG-NO      PIC X.                   
                   15  GL0514-DEP-GRP-ELIG.                                     
                       20  GL0514-DEP-GRP-ELIG-YES     PIC X.                   
                       20  GL0514-DEP-GRP-ELIG-NO      PIC X.                   
                   15  GL0514-DEP-ELIG-EXPL            PIC X(170).              
                   15  GL0514-DEP-SOLE-SUPP.                                    
                       20  GL0514-DEP-SOLE-SUPP-YES    PIC X.                   
                       20  GL0514-DEP-SOLE-SUPP-NO     PIC X.                   
                   15  GL0514-DEP-SOLE-EXPL            PIC X(170).              
                   15  GL0514-DEP-PHYSIC-MAILED        PIC X.                   
      *                                                                         
               10  GL0514-STUDENT.                                              
                   15  GL0514-SCHOOL-NAME              PIC X(45).               
                   15  GL0514-SCHOOL-LOC               PIC X(30).               
                   15  GL0514-SCHOOL-START-DATE.                                
                       20  GL0514-SCH-START-EDTE.                               
                           25  GL0514-SCH-ST-EDD       PIC XX.                  
                           25  GL0514-SCH-ST-ESL1      PIC X.                   
                           25  GL0514-SCH-ST-EMMM      PIC X(3).                
                           25  GL0514-SCH-ST-ESL2      PIC X.                   
                           25  GL0514-SCH-ST-EYYYY     PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL0514-SCH-START-FDTE                                
                                REDEFINES  GL0514-SCH-START-EDTE.               
                           25  GL0514-SCH-ST-FDD       PIC XX.                  
                           25  GL0514-SCH-ST-FSL1      PIC X.                   
                           25  GL0514-SCH-ST-FMMMM     PIC X(4).                
                           25  GL0514-SCH-ST-FSL2      PIC X.                   
                           25  GL0514-SCH-ST-FYYYY     PIC X(4).                
                   15  GL0514-SCHOOL-END-DATE.                                  
                       20  GL0514-SCH-END-EDTE.                                 
                           25  GL0514-SCH-END-EDD      PIC XX.                  
                           25  GL0514-SCH-END-ESL1     PIC X.                   
                           25  GL0514-SCH-END-EMMM     PIC X(3).                
                           25  GL0514-SCH-END-ESL2     PIC X.                   
                           25  GL0514-SCH-END-EYYYY    PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL0514-SCH-END-FDTE                                  
                                REDEFINES  GL0514-SCH-END-EDTE.                 
                           25  GL0514-SCH-END-FDD      PIC XX.                  
                           25  GL0514-SCH-END-FSL1     PIC X.                   
                           25  GL0514-SCH-END-FMMMM    PIC X(4).                
                           25  GL0514-SCH-END-FSL2     PIC X.                   
                           25  GL0514-SCH-END-FYYYY    PIC X(4).                
      *                                                                         
               10  GL0514-STUDENT-TERM.                                         
                   15  GL0514-TERM-ALL-COV             PIC X.                   
                   15  GL0514-TERM-COV-DEP-NAME        PIC X(21).               
                   15  GL0514-TERM-COV-DATE.                                    
                       20  GL0514-TERM-COV-EDTE.                                
                           25  GL0514-TERM-COV-EDD     PIC XX.                  
                           25  GL0514-TERM-COV-ESL1    PIC X.                   
                           25  GL0514-TERM-COV-EMMM    PIC X(3).                
                           25  GL0514-TERM-COV-ESL2    PIC X.                   
                           25  GL0514-TERM-COV-EYYYY   PIC X(4).                
                           25  FILLER                  PIC X.                   
                       20  GL0514-TERM-COV-FDTE                                 
                                REDEFINES  GL0514-TERM-COV-EDTE.                
                           25  GL0514-TERM-COV-FDD     PIC XX.                  
                           25  GL0514-TERM-COV-FSL1    PIC X.                   
                           25  GL0514-TERM-COV-FMMMM   PIC X(4).                
                           25  GL0514-TERM-COV-FSL2    PIC X.                   
                           25  GL0514-TERM-COV-FYYYY   PIC X(4).                
                   15  GL0514-TERM-COV-REASON          PIC X(80).               
      *                                                                         
               10  GL0514-AUTH.                                                 
                   15  GL0514-ENGLISH-TODAY.                                    
                       20  GL0514-ETODAY-DD            PIC XX.                  
                       20  GL0514-ETODAY-SL1           PIC X.                   
                       20  GL0514-ETODAY-MMM           PIC X(3).                
                       20  GL0514-ETODAY-SL2           PIC X.                   
                       20  GL0514-ETODAY-YYYY          PIC X(4).                
                       20  FILLER                      PIC X.                   
                   15  GL0514-FRENCH-TODAY                                      
                              REDEFINES  GL0514-ENGLISH-TODAY.                  
                       20  GL0514-FTODAY-DD            PIC XX.                  
                       20  GL0514-FTODAY-SL1           PIC X.                   
                       20  GL0514-FTODAY-MMMM          PIC X(4).                
                       20  GL0514-FTODAY-SL2           PIC X.                   
                       20  GL0514-FTODAY-YYYY          PIC X(4).                
                   15  GL0514-REGION                   PIC X(5).                
