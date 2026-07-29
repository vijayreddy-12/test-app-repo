           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L 2 9 7 1 PAGE 1                      *        
      *                                                                *        
      *   1. AFP PRINT, ENROLMENT/RE-ENROLMENT APPLICATION             *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 2 9 7 1 PAGE 1                      *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE       PGR   DESCRIPTION                             *        
      *  --   ------     ---   --------------------------------------  *        
      *                                                                *        
      *  00 - 001031 -   JAK   NEW COPYBOOK                            *        
      *                                                                *        
      *  01 - 010117 -   JAK   ADD ADDRESS V3.1                        *        
      *                                                                *        
      *  02 - 170901 -   JE    RELEASE 3.2 - 2 FIELDS NO LONGER USED   *        
      *                        CHANGED TO FILLER                       *        
      *                        2 FIELDS ADDED TO THE END OF COPYBOOK   *        
      *                                                                *        
      *  03 - 20020528 - JVH   ECOMM 4.2                               *        
      *                        2 FIELDS ADDED TO THE END OF COPYBOOK   *        
      *                                                                *        
      *  04 - 20031022 - JW    GBSS TASK 26184 - CHANGES TO GL2971     *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL2971-RECORD.                                                       
           05  GL2971-CC-P1                            PIC X.                   
           05  GL2971-FORM-TYPE-P1                     PIC XX.                  
           05  GL2971-PAGE-1.                                                   
               10  GL2971-DATE-TIME                    PIC X(60).               
               10  GL2971-SECT1.                                                
                   15  GL2971-PLAN        OCCURS 5     PIC X(7).                
                   15  GL2971-ACCOUNT     OCCURS 5     PIC X(3).                
                   15  GL2971-DIV         OCCURS 5     PIC XXX.                 
                   15  GL2971-CERT                     PIC X(11).               
                   15  GL2971-SPONSOR                  PIC X(60).               
                   15  GL2971-HIRE-DATE.                                        
                       20 GL2971-HIRE-EDATE.                                    
                           25 GL2971-HIRE-DTE-EDD      PIC XX.                  
                           25 GL2971-HIRE-DTE-ESL1     PIC X.                   
                           25 GL2971-HIRE-DTE-EMMM     PIC X(3).                
                           25 GL2971-HIRE-DTE-ESL2     PIC X.                   
                           25 GL2971-HIRE-DTE-EYYYY    PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL2971-HIRE-FDATE                                     
                                    REDEFINES  GL2971-HIRE-EDATE.               
                           25 GL2971-HIRE-DTE-FDD      PIC XX.                  
                           25 GL2971-HIRE-DTE-FSL1     PIC X.                   
                           25 GL2971-HIRE-DTE-FMMMM    PIC X(4).                
                           25 GL2971-HIRE-DTE-FSL2     PIC X.                   
                           25 GL2971-HIRE-DTE-FYYYY    PIC X(4).                
      *                                                                         
                   15  GL2971-PREV-EMPL-DATE.                                   
                       20  GL2971-PREV-EMPL-EDATE.                              
                           25 GL2971-PREV-EMPL-DTE-EDD                          
                                                       PIC XX.                  
                           25 GL2971-PREV-EMPL-DTE-ESL1                         
                                                       PIC X.                   
                           25 GL2971-PREV-EMPL-DTE-EMMM                         
                                                       PIC X(3).                
                           25 GL2971-PREV-EMPL-DTE-ESL2                         
                                                       PIC X.                   
                           25 GL2971-PREV-EMPL-DTE-EYYYY                        
                                                       PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20  GL2971-PREV-EMPL-FDATE                               
                                    REDEFINES  GL2971-PREV-EMPL-EDATE.          
                           25 GL2971-PREV-EMPL-DTE-FDD                          
                                                       PIC XX.                  
                           25 GL2971-PREV-EMPL-DTE-FSL1                         
                                                       PIC X.                   
                           25 GL2971-PREV-EMPL-DTE-FMMMM                        
                                                       PIC X(4).                
                           25 GL2971-PREV-EMPL-DTE-FSL2                         
                                                       PIC X.                   
                           25 GL2971-PREV-EMPL-DTE-FYYYY                        
                                                       PIC X(4).                
      *                                                                         
                   15  GL2971-REHIRE-DATE.                                      
                       20  GL2971-REHIRE-EDATE.                                 
                           25 GL2971-REHIRE-DTE-EDD    PIC XX.                  
                           25 GL2971-REHIRE-DTE-ESL1   PIC X.                   
                           25 GL2971-REHIRE-DTE-EMMM   PIC X(3).                
                           25 GL2971-REHIRE-DTE-ESL2   PIC X.                   
                           25 GL2971-REHIRE-DTE-EYYYY  PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20  GL2971-REHIRE-FDATE                                  
                                    REDEFINES  GL2971-REHIRE-EDATE.             
                           25 GL2971-REHIRE-DTE-FDD    PIC XX.                  
                           25 GL2971-REHIRE-DTE-FSL1   PIC X.                   
                           25 GL2971-REHIRE-DTE-FMMMM  PIC X(4).                
                           25 GL2971-REHIRE-DTE-FSL2   PIC X.                   
                           25 GL2971-REHIRE-DTE-FYYYY  PIC X(4).                
      *                                                                         
                   15  GL2971-OCCUPATION               PIC X(29).               
                   15  GL2971-CLASS       OCCURS 5     PIC XXX.                 
                   15  GL2971-HOURS-X.                                          
                       20  GL2971-HOURS                PIC ZZ9.99.              
                   15  GL2971-EARNINGS-X.                                       
                       20  GL2971-EARNINGS       PIC ZZ,ZZZ,ZZ9.99.             
                   15  GL2971-EVIDENCE-REQD.                                    
                       20  GL2971-EVIDENCE-REQD-YES    PIC X.                   
                       20  GL2971-EVIDENCE-REQD-NO     PIC X.                   
      * RELEASE 3.2 GL2971-MAILED NO LONGER USED                                
                   15  GL2971-MAILED.                                           
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X.                   
      *                                                                         
               10  GL2971-SECT2.                                                
                   15  GL2971-MBR-NAME.                                         
                       20  GL2971-MBR-SURNAME          PIC X(40).               
                       20  GL2971-MBR-FSTNAME          PIC X(30).               
                       20  GL2971-MBR-INITS            PIC X(5).                
                   15  GL2971-MBR-DOB.                                          
                       20  GL2971-MBR-DOB-E.                                    
                           25 GL2971-MBR-DOB-EDD       PIC XX.                  
                           25 GL2971-MBR-DOB-ESL1      PIC X.                   
                           25 GL2971-MBR-DOB-EMMM      PIC X(3).                
                           25 GL2971-MBR-DOB-ESL2      PIC X.                   
                           25 GL2971-MBR-DOB-EYYYY     PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL2971-MBR-DOB-F                                      
                                  REDEFINES GL2971-MBR-DOB-E.                   
                           25 GL2971-MBR-DOB-FDD       PIC XX.                  
                           25 GL2971-MBR-DOB-FSL1      PIC X.                   
                           25 GL2971-MBR-DOB-FMMMM     PIC X(4).                
                           25 GL2971-MBR-DOB-FSL2      PIC X.                   
                           25 GL2971-MBR-DOB-FYYYY     PIC X(4).                
      *                                                                         
                   15  GL2971-MBR-GENDER.                                       
                       20  GL2971-MBR-MALE             PIC X.                   
                       20  GL2971-MBR-FEMALE           PIC X.                   
                   15  GL2971-MBR-PROV                 PIC XX.                  
                   15  GL2971-MBR-LANG.                                         
                       20  GL2971-MBR-LANG-ENG         PIC X.                   
                       20  GL2971-MBR-LANG-FRC         PIC X.                   
      *                                                                         
               10  GL2971-MBR-ADDRESS.                                          
                   15  GL2971-MBR-STREET               PIC X(50).               
                   15  GL2971-MBR-CITY                 PIC X(30).               
                   15  GL2971-MBR-ADDR-PROV            PIC XX.                  
                   15  GL2971-MBR-POSTCODE             PIC X(10).               
      *                                                                         
               10  GL2971-APPLICATION.                                          
                   15  GL2971-DEP-LIFE.                                         
                       20  GL2971-DEP-LIFE-YES         PIC X.                   
                       20  GL2971-DEP-LIFE-NO          PIC X.                   
                   15  GL2971-EHC.                                              
                       20  GL2971-EHC-BOX1             PIC X.                   
                       20  GL2971-EHC-BOX2             PIC X.                   
                       20  GL2971-EHC-BOX3             PIC X.                   
                       20  GL2971-EHC-BOX4             PIC X.                   
                   15  GL2971-DENT.                                             
                       20  GL2971-DENT-BOX1            PIC X.                   
                       20  GL2971-DENT-BOX2            PIC X.                   
                       20  GL2971-DENT-BOX3            PIC X.                   
                       20  GL2971-DENT-BOX4            PIC X.                   
               10  GL2971-SECT4.                                                
                   15  GL2971-QUEBEC-AGE.                                       
                       20  GL2971-QUEBEC-AGE-BOX1      PIC X.                   
                       20  GL2971-QUEBEC-AGE-BOX2      PIC X.                   
               10  GL2971-SECT5.                                                
      * RELEASE 3.2 GL2971-SPOUSE-GENDER NO LONGER USED                         
                   15  GL2971-SPOUSE-GENDER.                                    
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X.                   
                   15  GL2971-SPOUSE-DOB.                                       
                       20 GL2971-SPOUSE-DOB-E.                                  
                           25 GL2971-SPOUSE-DOB-EDD    PIC XX.                  
                           25 GL2971-SPOUSE-DOB-ESL1   PIC X.                   
                           25 GL2971-SPOUSE-DOB-EMMM   PIC X(3).                
                           25 GL2971-SPOUSE-DOB-ESL2   PIC X.                   
                           25 GL2971-SPOUSE-DOB-EYYYY  PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL2971-SPOUSE-DOB-F                                   
                                      REDEFINES GL2971-SPOUSE-DOB-E.            
                           25 GL2971-SPOUSE-DOB-FDD    PIC XX.                  
                           25 GL2971-SPOUSE-DOB-FSL1   PIC X.                   
                           25 GL2971-SPOUSE-DOB-FMMMM  PIC X(4).                
                           25 GL2971-SPOUSE-DOB-FSL2   PIC X.                   
                           25 GL2971-SPOUSE-DOB-FYYYY  PIC X(4).                
      *                                                                         
      * RELEASE 3.2 GL2971-MARITAL-STATUS NO LONGER USED                        
                   15  GL2971-MARITAL-STATUS.                                   
                       20  FILLER                      PIC X.                   
                       20  FILLER                      PIC X.                   
                   15  GL2971-RELATIONSHIP-START-DATE.                          
                       20 GL2971-REL-ST-EFF-EDATE.                              
                           25 GL2971-REL-ST-EDD        PIC XX.                  
                           25 GL2971-REL-ST-ESL1       PIC X.                   
                           25 GL2971-REL-ST-EMMM       PIC X(3).                
                           25 GL2971-REL-ST-ESL2       PIC X.                   
                           25 GL2971-REL-ST-EYYYY      PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL2971-REL-ST-EFF-FDATE                               
                                  REDEFINES GL2971-REL-ST-EFF-EDATE.            
                           25 GL2971-REL-ST-FDD        PIC XX.                  
                           25 GL2971-REL-ST-FSL1       PIC X.                   
                           25 GL2971-REL-ST-FMMMM      PIC X(4).                
                           25 GL2971-REL-ST-FSL2       PIC X.                   
                           25 GL2971-REL-ST-FYYYY      PIC X(4).                
      *                                                                         
                   15  GL2971-SPOUSE-HEALTH-COV.                                
                       20  GL2971-SPOUSE-HLTH-COV-YES  PIC X.                   
                       20  GL2971-SPOUSE-HLTH-COV-NO   PIC X.                   
      *                                                                         
                   15  GL2971-SPOUSE-HC-EFF-DATE.                               
                       20 GL2971-SPOUSE-HC-EFF-EDATE.                           
                           25 GL2971-SPOUSE-HC-EFF-EDD  PIC XX.                 
                           25 GL2971-SPOUSE-HC-EFF-ESL1 PIC X.                  
                           25 GL2971-SPOUSE-HC-EFF-EMMM PIC X(3).               
                           25 GL2971-SPOUSE-HC-EFF-ESL2 PIC X.                  
                           25 GL2971-SPOUSE-HC-EFF-EYYYY PIC X(4).              
                           25 FILLER                    PIC X.                  
                       20 GL2971-SPOUSE-HC-EFF-FDATE                            
                                REDEFINES GL2971-SPOUSE-HC-EFF-EDATE.           
                           25 GL2971-SPOUSE-HC-EFF-FDD  PIC XX.                 
                           25 GL2971-SPOUSE-HC-EFF-FSL1 PIC X.                  
                           25 GL2971-SPOUSE-HC-EFF-FMMMM PIC X(4).              
                           25 GL2971-SPOUSE-HC-EFF-FSL2 PIC X.                  
                           25 GL2971-SPOUSE-HC-EFF-FYYYY PIC X(4).              
                   15  GL2971-SPOUSE-HC-SCOPE.                                  
                       20  GL2971-SPOUSE-HC-SCOPE-1    PIC X.                   
                       20  GL2971-SPOUSE-HC-SCOPE-2    PIC X.                   
                       20  GL2971-SPOUSE-HC-SCOPE-3    PIC X.                   
                       20  GL2971-SPOUSE-HC-SCOPE-4    PIC X.                   
                   15  GL2971-SPOUSE-DENT-COV.                                  
                       20  GL2971-SPOUSE-DENT-COV-Y    PIC X.                   
                       20  GL2971-SPOUSE-DENT-COV-N    PIC X.                   
                   15  GL2971-SPOUSE-DC-EFF-DATE.                               
                       20 GL2971-SPOUSE-DC-EFF-EDATE.                           
                           25 GL2971-SPOUSE-DC-EFF-EDD  PIC XX.                 
                           25 GL2971-SPOUSE-DC-EFF-ESL1 PIC X.                  
                           25 GL2971-SPOUSE-DC-EFF-EMMM PIC X(3).               
                           25 GL2971-SPOUSE-DC-EFF-ESL2 PIC X.                  
                           25 GL2971-SPOUSE-DC-EFF-EYYYY PIC X(4).              
                           25 FILLER                    PIC X.                  
                       20 GL2971-SPOUSE-DC-EFF-FDATE                            
                                REDEFINES GL2971-SPOUSE-DC-EFF-EDATE.           
                           25 GL2971-SPOUSE-DC-EFF-FDD  PIC XX.                 
                           25 GL2971-SPOUSE-DC-EFF-FSL1 PIC X.                  
                           25 GL2971-SPOUSE-DC-EFF-FMMMM PIC X(4).              
                           25 GL2971-SPOUSE-DC-EFF-FSL2 PIC X.                  
                           25 GL2971-SPOUSE-DC-EFF-FYYYY PIC X(4).              
                   15  GL2971-SPOUSE-DC-SCOPE.                                  
                       20  GL2971-SPOUSE-DC-SCOPE-1    PIC X.                   
                       20  GL2971-SPOUSE-DC-SCOPE-2    PIC X.                   
                       20  GL2971-SPOUSE-DC-SCOPE-3    PIC X.                   
                       20  GL2971-SPOUSE-DC-SCOPE-4    PIC X.                   
                   15  GL2971-REGION-P1                PIC X(5).                
      *                                                                         
      * NEW FIELDS ADD FOR RELEASE 3.2                                          
               10  GL2971-SPOUSE-IND                   PIC X.                   
               10  GL2971-COMN-LAW-IND.                                         
                   15  GL2971-COMN-LAW-IND-YES         PIC X.                   
                   15  GL2971-COMN-LAW-IND-NO          PIC X.                   
      *                                                                         
      * NEW FIELDS ADD FOR RELEASE 4.2                                          
               10  GL2971-WAITING-PERIOD-IND.                                   
                   15  GL2971-WAITING-PERIOD-YES       PIC X.                   
                   15  GL2971-WAITING-PERIOD-NO        PIC X.                   
      * GL2971-WAITING-PERIOD-COND NO LONGER USED                               
               10  FILLER                              PIC X.                   
      *            15  GL2971-EMPLOY-STATUS-CHG        PIC X.                   
      *            15  GL2971-COV-PREV-EMPLOYER        PIC X.                   
      *            15  GL2971-WAITING-PERIOD-OTHER     PIC X.                   
