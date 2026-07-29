           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C D C N T                         *        
      *                                                                *        
      *   1. CONTROL CARD INPUT TO PURGES GCCPFRMA                     *        
      *                                   GCCPFRMD                     *        
      *                                   GCCPFRMH                     *        
      *                                   GCCPFRMW                     *        
      *                                   GCCPFRMU                     *        
      *                                   GCCPFRMG                     *        
      *                                   GCCPFRMC                     *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C D C N T                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001115 - JAK   NEW COPYBOOK                              *        
      *                                                                *        
      *  01 - 010216 - JE    CHANGE NUMBER OF DAYS TO NNNN             *        
      *  02 - 081003 - WJS   ADDED NEW FIELD FOR SECURITY LOGGING      *        
      *                      - NUMBER OF DAYS FOR TAUDEV PURGE                  
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCCDCNT-RECORD.                                                     
           05  GCCCDCNT-NUMBER-OF-DAYS             PIC 9999.                    
           05  GCCCDCNT-NUMBER-OF-DAYS-2           PIC 9999.                    
           05  FILLER                              PIC X(72).                   
