           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C 0 5 1 4                         *        
      *                                                                *        
      *   1. AFP PRINT, REQUEST FOR, OR TERMINATION OF OVER-AGE        *        
      *      DEPENDENT COVERAGE                                        *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C 0 5 1 4                         *        
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
      *01  GCCC0514-RECORD.                                                     
           05  GCCC0514-HEADER.                                                 
               10  GCCC0514-CONF-NBR               PIC 9(11).                   
               10  GCCC0514-SEQ-NBR                PIC 9(5).                    
               10  GCCC0514-FORM-NBR               PIC X(8).                    
               10  GCCC0514-LANG                   PIC X.                       
               10  GCCC0514-CUST-GROUP-NBR         PIC X(7).                    
               10  GCCC0514-CUST-DIV               PIC X(3).                    
               10  GCCC0514-CUST-CERT-NBR          PIC X(10).                   
               10  GCCC0514-WEB-TIMESTAMP.                                      
                   15  GCCC0514-WEB-YYYY           PIC X(4).                    
                   15  GCCC0514-WEB-MM             PIC XX.                      
                   15  GCCC0514-WEB-DD             PIC XX.                      
                   15  GCCC0514-WEB-HH             PIC XX.                      
                   15  GCCC0514-WEB-MIN            PIC XX.                      
                   15  GCCC0514-WEB-SS             PIC XX.                      
                   15  GCCC0514-WEB-NN             PIC XX.                      
           05  GCCC0514-PAGE-1.                                                 
               10  GCCC0514-DATE-TIME              PIC X(60).                   
               10  GCCC0514-SELECT                 PIC X.                       
               10  GCCC0514-GEN-INFO.                                           
                   15  GCCC0514-SPONSOR            PIC X(60).                   
                   15  GCCC0514-GEN-DATA OCCURS 2.                              
                       20  GCCC0514-PLAN           PIC X(7).                    
                       20  GCCC0514-ACCT           PIC X(3).                    
                       20  GCCC0514-DIV            PIC X(3).                    
                   15  GCCC0514-CERT               PIC X(11).                   
                   15  GCCC0514-MBR-LAST-NME       PIC X(40).                   
                   15  GCCC0514-MBR-FIRST-NME      PIC X(30).                   
                   15  GCCC0514-MBR-MID-INIT       PIC X(5).                    
                   15  GCCC0514-MBR-ADDRESS        PIC X(50).                   
                   15  GCCC0514-MBR-CITY           PIC X(20).                   
                   15  GCCC0514-MBR-PROV           PIC XX.                      
                   15  GCCC0514-MBR-POSTCODE       PIC X(10).                   
                   15  GCCC0514-DEP-LAST           PIC X(21).                   
                   15  GCCC0514-DEP-FIRST          PIC X(21).                   
                   15  GCCC0514-DEP-RELSHP         PIC X(15).                   
      *            15  GCCC0514-DEP-RELSHP         PIC X.                       
                   15  GCCC0514-DEP-DOB.                                        
                       20  GCCC0514-DEP-DOB-YYYY   PIC X(4).                    
                       20  GCCC0514-DEP-DOB-MM     PIC XX.                      
                       20  GCCC0514-DEP-DOB-DD     PIC XX.                      
                   15  GCCC0514-DEP-GENDER         PIC X.                       
                   15  GCCC0514-DEP-ADDRESS        PIC X(50).                   
                   15  GCCC0514-DEP-CITY           PIC X(20).                   
                   15  GCCC0514-DEP-PROV           PIC XX.                      
                   15  GCCC0514-DEP-POSTCODE       PIC X(10).                   
      *                                                                         
               10  GCCC0514-DIS-DEP-INFO.                                       
                   15  GCCC0514-RESIDENT           PIC X.                       
                   15  GCCC0514-DDEP-RES-EXPL      PIC X(170).                  
                   15  GCCC0514-EMPLOYED           PIC X.                       
                   15  GCCC0514-DIS-DEP-DATE.                                   
                       20  GCCC0514-DDEP-DATE-YYYY PIC X(4).                    
                       20  GCCC0514-DDEP-DATE-MM   PIC XX.                      
                       20  GCCC0514-DDEP-DATE-DD   PIC XX.                      
                   15  GCCC0514-TYPE               PIC X(60).                   
      *            15  GCCC0514-TYPE               PIC X.                       
                   15  GCCC0514-GOVMT              PIC X.                       
                   15  GCCC0514-GROUP              PIC X.                       
                   15  GCCC0514-DDEP-ELIG-EXPL     PIC X(170).                  
                   15  GCCC0514-SOLE               PIC X.                       
                   15  GCCC0514-DDEP-SOLE-EXPL     PIC X(170).                  
                   15  GCCC0514-DEP-PHYSIC-MAILED  PIC X.                       
      *                                                                         
               10  GCCC0514-STUDENT.                                            
                   15  GCCC0514-SCHOOL             PIC X(45).                   
                   15  GCCC0514-LOC                PIC X(30).                   
                   15  GCCC0514-STUD-ST-DATE.                                   
                       20  GCCC0514-STUD-ST-DATE-YYYY                           
                                                   PIC X(4).                    
                       20  GCCC0514-STUD-ST-DATE-MM                             
                                                   PIC XX.                      
                       20  GCCC0514-STUD-ST-DATE-DD                             
                                                   PIC XX.                      
                   15  GCCC0514-STUD-END-DATE.                                  
                       20  GCCC0514-STUD-END-DATE-YYYY                          
                                                   PIC X(4).                    
                       20  GCCC0514-STUD-END-DATE-MM                            
                                                   PIC XX.                      
                       20  GCCC0514-STUD-END-DATE-DD                            
                                                   PIC XX.                      
      *                                                                         
               10  GCCC0514-STUDENT-TERM.                                       
                   15  GCCC0514-TERMINATE          PIC X.                       
                   15  GCCC0514-DEPENDENT          PIC X(21).                   
                   15  GCCC0514-STUD-TERM-DATE.                                 
                       20  GCCC0514-STUD-TERM-DATE-YYYY                         
                                                   PIC X(4).                    
                       20  GCCC0514-STUD-TERM-DATE-MM                           
                                                   PIC XX.                      
                       20  GCCC0514-STUD-TERM-DATE-DD                           
                                                   PIC XX.                      
                   15  GCCC0514-STUD-TERM-REASON   PIC X(80).                   
      *                                                                         
               10  GCCC0514-AUTH.                                               
                   15  GCCC0514-TODAY.                                          
                       20  GCCC0514-TODAY-YYYY     PIC X(4).                    
                       20  GCCC0514-TODAY-MM       PIC XX.                      
                       20  GCCC0514-TODAY-DD       PIC XX.                      
                   15  GCCC0514-REGION             PIC X.                       
