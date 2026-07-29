           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C R P T                           *        
      *                                                                *        
      *   1. REPORT EXTRACT RECORD OUTPUT FROM GCCPFRMP INPUT          *        
      *      TO GCCPFRMR                                               *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C R P T                           *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      *  01 - 010626 - JE    ADD TOTAL RECORDS                         *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCCRPT-RECORD.                                                      
           05  GCCCRPT-RECORD-TYPE                 PIC X.                       
               88  GCCCRPT-TOTAL-REC               VALUE 'T'.                   
               88  GCCCRPT-DETAIL-REC              VALUE 'D'.                   
           05  GCCCRPT-DIST-CAT-GRP.                                            
               10  FILLER                          PIC X(20).                   
               10  GCCCRPT-DIST-CAT-TAT            PIC X(10).                   
           05  GCCCRPT-DETAIL-RECORD.                                           
               10  GCCCRPT-GROUP                   PIC X(7).                    
               10  GCCCRPT-DIVISION                PIC X(3).                    
               10  GCCCRPT-FORM-NAME               PIC X(8).                    
               10  GCCCRPT-MEMBER-NAME             PIC X(48).                   
               10  GCCCRPT-CONFIRM-NO              PIC 9(11).                   
               10  GCCCRPT-SEQUENCE-NO             PIC 9(5).                    
               10  GCCCRPT-CERTIFICATE-NO          PIC X(10).                   
               10  FILLER                          PIC X(17).                   
           05  GCCCRPT-TOTAL-RECORD REDEFINES                                   
                                    GCCCRPT-DETAIL-RECORD.                      
               10  GCCCRPT-TOTAL-LINE              PIC X(70).                   
