           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L 3 5 7 4 PAGE 1                      *        
      *                                                                *        
      *   1. AFP PRINT, ENROLMENT/RE-ENROLMENT APPLICATION (HCSA)      *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 3 5 7 4 PAGE 1                      *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE       PGR   DESCRIPTION                             *        
      *  --   ------     ---   --------------------------------------  *        
      *                                                                *        
      *  00 - 20020528 - JVH   ECOMM 4.2                               *        
      *                        NEW COPYBOOK                            *        
      *  01   20150511 - IBM   REMOVE GL3574-HCSA-ALLOC-TYPE AND       *        
      *                        WAITING-PERIOD-COND,TL236472            *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL3574-RECORD.                                                       
           05  GL3574-CC-P1                            PIC X.                   
           05  GL3574-FORM-TYPE-P1                     PIC XX.                  
           05  GL3574-PAGE-1.                                                   
               10  GL3574-DATE-TIME                    PIC X(60).               
               10  GL3574-SECT1.                                                
                   15  GL3574-PLAN        OCCURS 5     PIC X(7).                
                   15  GL3574-ACCOUNT     OCCURS 5     PIC X(3).                
                   15  GL3574-DIV         OCCURS 5     PIC XXX.                 
                   15  GL3574-CERT                     PIC X(11).               
                   15  GL3574-SPONSOR                  PIC X(60).               
                   15  GL3574-HIRE-DATE.                                        
                       20 GL3574-HIRE-EDATE.                                    
                           25 GL3574-HIRE-DTE-EDD      PIC XX.                  
                           25 GL3574-HIRE-DTE-ESL1     PIC X.                   
                           25 GL3574-HIRE-DTE-EMMM     PIC X(3).                
                           25 GL3574-HIRE-DTE-ESL2     PIC X.                   
                           25 GL3574-HIRE-DTE-EYYYY    PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3574-HIRE-FDATE                                     
                                    REDEFINES  GL3574-HIRE-EDATE.               
                           25 GL3574-HIRE-DTE-FDD      PIC XX.                  
                           25 GL3574-HIRE-DTE-FSL1     PIC X.                   
                           25 GL3574-HIRE-DTE-FMMMM    PIC X(4).                
                           25 GL3574-HIRE-DTE-FSL2     PIC X.                   
                           25 GL3574-HIRE-DTE-FYYYY    PIC X(4).                
      *                                                                         
                   15  GL3574-PREV-EMPL-DATE.                                   
                       20  GL3574-PREV-EMPL-EDATE.                              
                           25 GL3574-PREV-EMPL-DTE-EDD                          
                                                       PIC XX.                  
                           25 GL3574-PREV-EMPL-DTE-ESL1                         
                                                       PIC X.                   
                           25 GL3574-PREV-EMPL-DTE-EMMM                         
                                                       PIC X(3).                
                           25 GL3574-PREV-EMPL-DTE-ESL2                         
                                                       PIC X.                   
                           25 GL3574-PREV-EMPL-DTE-EYYYY                        
                                                       PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20  GL3574-PREV-EMPL-FDATE                               
                                    REDEFINES  GL3574-PREV-EMPL-EDATE.          
                           25 GL3574-PREV-EMPL-DTE-FDD                          
                                                       PIC XX.                  
                           25 GL3574-PREV-EMPL-DTE-FSL1                         
                                                       PIC X.                   
                           25 GL3574-PREV-EMPL-DTE-FMMMM                        
                                                       PIC X(4).                
                           25 GL3574-PREV-EMPL-DTE-FSL2                         
                                                       PIC X.                   
                           25 GL3574-PREV-EMPL-DTE-FYYYY                        
                                                       PIC X(4).                
      *                                                                         
                   15  GL3574-REHIRE-DATE.                                      
                       20  GL3574-REHIRE-EDATE.                                 
                           25 GL3574-REHIRE-DTE-EDD    PIC XX.                  
                           25 GL3574-REHIRE-DTE-ESL1   PIC X.                   
                           25 GL3574-REHIRE-DTE-EMMM   PIC X(3).                
                           25 GL3574-REHIRE-DTE-ESL2   PIC X.                   
                           25 GL3574-REHIRE-DTE-EYYYY  PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20  GL3574-REHIRE-FDATE                                  
                                    REDEFINES  GL3574-REHIRE-EDATE.             
                           25 GL3574-REHIRE-DTE-FDD    PIC XX.                  
                           25 GL3574-REHIRE-DTE-FSL1   PIC X.                   
                           25 GL3574-REHIRE-DTE-FMMMM  PIC X(4).                
                           25 GL3574-REHIRE-DTE-FSL2   PIC X.                   
                           25 GL3574-REHIRE-DTE-FYYYY  PIC X(4).                
      *                                                                         
                   15  GL3574-WAITING-PERIOD-IND.                               
                       20  GL3574-WAITING-PERIOD-YES   PIC X.                   
                       20  GL3574-WAITING-PERIOD-NO    PIC X.                   
      * REMOVE GL3574-WAITING-PERIOD-COND                                       
      *            15  GL3574-WAITING-PERIOD-COND.                              
      *                20  GL3574-EMPLOY-STATUS-CHG    PIC X.                   
      *                20  GL3574-COV-PREV-EMPLOYER    PIC X.                   
      *                20  GL3574-WAITING-PERIOD-OTHER PIC X.                   
      *                                                                         
                   15  GL3574-OCCUPATION               PIC X(29).               
                   15  GL3574-CLASS       OCCURS 5     PIC XXX.                 
                   15  GL3574-HOURS-X.                                          
                       20  GL3574-HOURS                PIC ZZ9.99.              
                   15  GL3574-EARNINGS-X.                                       
                       20  GL3574-EARNINGS       PIC ZZ,ZZZ,ZZ9.99.             
                   15  GL3574-EVIDENCE-REQD.                                    
                       20  GL3574-EVIDENCE-REQD-YES    PIC X.                   
                       20  GL3574-EVIDENCE-REQD-NO     PIC X.                   
      *                                                                         
               10  GL3574-SECT2.                                                
                   15  GL3574-MBR-NAME.                                         
                       20  GL3574-MBR-SURNAME          PIC X(40).               
                       20  GL3574-MBR-FSTNAME          PIC X(30).               
                       20  GL3574-MBR-INITS            PIC X(5).                
                   15  GL3574-MBR-DOB.                                          
                       20  GL3574-MBR-DOB-E.                                    
                           25 GL3574-MBR-DOB-EDD       PIC XX.                  
                           25 GL3574-MBR-DOB-ESL1      PIC X.                   
                           25 GL3574-MBR-DOB-EMMM      PIC X(3).                
                           25 GL3574-MBR-DOB-ESL2      PIC X.                   
                           25 GL3574-MBR-DOB-EYYYY     PIC X(4).                
                           25 FILLER                   PIC X.                   
                       20 GL3574-MBR-DOB-F                                      
                                  REDEFINES GL3574-MBR-DOB-E.                   
                           25 GL3574-MBR-DOB-FDD       PIC XX.                  
                           25 GL3574-MBR-DOB-FSL1      PIC X.                   
                           25 GL3574-MBR-DOB-FMMMM     PIC X(4).                
                           25 GL3574-MBR-DOB-FSL2      PIC X.                   
                           25 GL3574-MBR-DOB-FYYYY     PIC X(4).                
      *                                                                         
                   15  GL3574-MBR-GENDER.                                       
                       20  GL3574-MBR-MALE             PIC X.                   
                       20  GL3574-MBR-FEMALE           PIC X.                   
                   15  GL3574-MBR-PROV                 PIC XX.                  
                   15  GL3574-MBR-LANG.                                         
                       20  GL3574-MBR-LANG-ENG         PIC X.                   
                       20  GL3574-MBR-LANG-FRC         PIC X.                   
      *                                                                         
               10  GL3574-SECT3.                                                
                   15  GL3574-MBR-ADDRESS.                                      
                       20  GL3574-MBR-STREET           PIC X(50).               
                       20  GL3574-MBR-CITY             PIC X(30).               
                       20  GL3574-MBR-ADDR-PROV        PIC XX.                  
                       20  GL3574-MBR-POSTCODE         PIC X(10).               
      *                                                                         
               10  GL3574-SECT4.                                                
                   15  GL3574-APPLICATION.                                      
                       20  GL3574-DEP-LIFE.                                     
                           25  GL3574-DEP-LIFE-YES     PIC X.                   
                           25  GL3574-DEP-LIFE-NO      PIC X.                   
                       20  GL3574-EHC.                                          
                           25  GL3574-EHC-BOX1         PIC X.                   
                           25  GL3574-EHC-BOX2         PIC X.                   
                           25  GL3574-EHC-BOX3         PIC X.                   
                           25  GL3574-EHC-BOX4         PIC X.                   
                       20  GL3574-DENT.                                         
                           25  GL3574-DENT-BOX1        PIC X.                   
                           25  GL3574-DENT-BOX2        PIC X.                   
                           25  GL3574-DENT-BOX3        PIC X.                   
                           25  GL3574-DENT-BOX4        PIC X.                   
                   15  GL3574-HCSA-APPLICATION.                                 
                       20  GL3574-HCSA-IND.                                     
                           25  GL3574-HCSA-YES         PIC X.                   
                           25  GL3574-HCSA-NO          PIC X.                   
                       20  GL3574-HCSA-PLAN-NUMBER     PIC X(7).                
                       20  GL3574-HCSA-EFF-DATE.                                
                           25  GL3574-HCSA-EFF-EDATE.                           
                               30  GL3574-HCSA-EFF-DTE-EDD                      
                                                       PIC XX.                  
                               30  GL3574-HCSA-EFF-DTE-ESL1                     
                                                       PIC X.                   
                               30  GL3574-HCSA-EFF-DTE-EMMM                     
                                                       PIC X(3).                
                               30  GL3574-HCSA-EFF-DTE-ESL2                     
                                                       PIC X.                   
                               30  GL3574-HCSA-EFF-DTE-EYYYY                    
                                                       PIC X(4).                
                               30  FILLER              PIC X.                   
                           25  GL3574-HCSA-EFF-FDATE                            
                                   REDEFINES  GL3574-HCSA-EFF-EDATE.            
                               30  GL3574-HCSA-EFF-DTE-FDD                      
                                                       PIC XX.                  
                               30  GL3574-HCSA-EFF-DTE-FSL1                     
                                                       PIC X.                   
                               30  GL3574-HCSA-EFF-DTE-FMMMM                    
                                                       PIC X(4).                
                               30  GL3574-HCSA-EFF-DTE-FSL2                     
                                                       PIC X.                   
                               30  GL3574-HCSA-EFF-DTE-FYYYY                    
                                                       PIC X(4).                
                       20  GL3574-HCSA-ALLOC-AMT   PIC Z,ZZZ,ZZ9.99.            
    *****ROMOVE GL3574-HCSA-ALLOC-TYPE, TL236472                                
                   15  GL3574-REGION-P1                PIC X(5).                
