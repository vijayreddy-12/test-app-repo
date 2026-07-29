           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G L 0 0 0 3                             *        
      *                                                                *        
      *   1. AFP PRINT, REQUEST FOR, OR TERMINATION OF OVER-AGE        *        
      *      DEPENDENT COVERAGE                                        *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G L 0 0 0 3                             *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      *  01 - 010125 - JAK   CHANGES V3.1                              *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GL0003-RECORD.                                                       
           05  GL0003-CC                           PIC X.                       
           05  GL0003-FORM-TYPE                    PIC XX.                      
      *                                                                         
           05  GL0003-PAGE-1.                                                   
               10  GL0003-DATE-TIME                PIC X(60).                   
               10  GL0003-GEN-INFO.                                             
                   15  GL0003-PLAN    OCCURS 5     PIC X(7).                    
                   15  GL0003-ACCOUNT OCCURS 5     PIC XXX.                     
                   15  GL0003-DIV     OCCURS 5     PIC XXX.                     
                   15  GL0003-CERT                 PIC X(11).                   
                   15  GL0003-SPONSOR              PIC X(60).                   
                   15  GL0003-EMPLOYER             PIC X(80).                   
                   15  GL0003-MEMBER-NAME.                                      
                       20  GL0003-MBR-SURNAME      PIC X(40).                   
                       20  GL0003-MBR-FSTNAME      PIC X(30).                   
                       20  GL0003-MBR-INITS        PIC X(5).                    
                   15  GL0003-COMMENTS             PIC X(360).                  
      *                                                                         
               10  GL0003-AUTH.                                                 
                   15  GL0003-ENGLISH-TODAY.                                    
                       20  GL0003-ETODAY-DD        PIC XX.                      
                       20  GL0003-ETODAY-SL1       PIC X.                       
                       20  GL0003-ETODAY-MMM       PIC X(3).                    
                       20  GL0003-ETODAY-SL2       PIC X.                       
                       20  GL0003-ETODAY-YYYY      PIC X(4).                    
                       20  FILLER                  PIC X.                       
                   15  GL0003-FRENCH-TODAY                                      
                               REDEFINES  GL0003-ENGLISH-TODAY.                 
                       20  GL0003-FTODAY-DD        PIC XX.                      
                       20  GL0003-FTODAY-SL1       PIC X.                       
                       20  GL0003-FTODAY-MMMM      PIC X(4).                    
                       20  GL0003-FTODAY-SL2       PIC X.                       
                       20  GL0003-FTODAY-YYYY      PIC X(4).                    
                   15  GL0003-REGION               PIC X(5).                    
