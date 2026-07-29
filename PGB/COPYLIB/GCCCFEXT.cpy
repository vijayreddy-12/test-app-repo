           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C F E X T                         *        
      *                                                                *        
      *      FORM EXTRACT FILE LAYOUT                                  *        
      *      RECORD LENGTH IS VARIABLE FOR EACH FORM NUMBER.           *        
      *      NOTE:                                                     *        
      *      FORM LENGTH IS HELD ON FILE, ANY CHANGES TO THE LENGTH    *        
      *      OR ADDITION OF FORMS MUST BE REFLECTED THERE.             *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C F E X T                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001107 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      *  01 - 010122 - JAK   CHANGES V3.1                              *        
      *                                                                *        
      *  02 - 010613 - JE    CHANGES TO INCLUDE TAT IN SORT FIELDS     *        
      *                      GCCCFEXT-SORT-CAT0                        *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCCFEXT-RECORD.                                                     
           05  GCCCFEXT-HEADER.                                                 
               10  GCCCFEXT-LANG                   PIC X.                       
               10  GCCCFEXT-CONT-STATUS            PIC X.                       
               10  GCCCFEXT-SORT-CAT0              PIC XX.                      
               10  GCCCFEXT-SORT-CAT1              PIC X.                       
               10  GCCCFEXT-SORT-CAT2              PIC X.                       
               10  GCCCFEXT-REGION                 PIC X.                       
               10  GCCCFEXT-BUS-SEG                PIC X.                       
               10  GCCCFEXT-GROUP                  PIC X(7).                    
               10  GCCCFEXT-DIV                    PIC X(3).                    
               10  GCCCFEXT-FORM-NBR               PIC X(8).                    
               10  GCCCFEXT-CERT-ID                PIC X(10).                   
               10  GCCCFEXT-CONF-NBR               PIC S9(11) COMP-3.           
               10  GCCCFEXT-SEQ-NBR                PIC 9(5).                    
               10  GCCCFEXT-FORM-DATA.                                          
                   15  GCCCFEXT-TIMESTAMP          PIC X(60).                   
                   15  FILLER                      PIC X(3896).                 
