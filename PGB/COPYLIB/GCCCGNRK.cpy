           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C G N R K                         *        
      *                                                                *        
      *   1. AFP PRINT, GENERIC COPYBOOK FOR USE IN DB2 CREATE         *        
      *                                                                *        
      *                                                                * E      
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C G N R K                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR     DESCRIPTION                             *        
      *  --   ------   ---     --------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK     NEW COPYBOOK                            *        
      *       20190213 STANLPA REMOVE ELEMENTARY FILLER AT             *        
      *                        END OF RECORD                           *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCCGNRK-RECORD.                                                     
           05  GCCCGNRK-HEADER.                                                 
               10  GCCCGNRK-CONF-NBR               PIC 9(11).                   
               10  GCCCGNRK-SEQ-NBR                PIC 9(5).                    
               10  GCCCGNRK-FORM-NBR               PIC X(8).                    
               10  GCCCGNRK-LANG                   PIC X.                       
               10  GCCCGNRK-CUST-GROUP-NBR         PIC X(7).                    
               10  GCCCGNRK-CUST-DIV               PIC X(3).                    
               10  GCCCGNRK-CUST-CERT-NBR          PIC X(10).                   
               10  FILLER REDEFINES GCCCGNRK-CUST-CERT-NBR.                     
                   15  GCCCGNRK-MASS-PAGE          PIC 9(7).                    
               10  GCCCGNRK-WEB-TIMESTAMP.                                      
                   15  GCCCGNRK-WEB-YYYY           PIC X(4).                    
                   15  GCCCGNRK-WEB-MM             PIC XX.                      
                   15  GCCCGNRK-WEB-DD             PIC XX.                      
                   15  GCCCGNRK-WEB-HH             PIC XX.                      
                   15  GCCCGNRK-WEB-MIN            PIC XX.                      
                   15  GCCCGNRK-WEB-SS             PIC XX.                      
                   15  GCCCGNRK-WEB-NN             PIC XX.                      
           05  GCCCGNRK-DETAIL                     PIC X(3939).                 
