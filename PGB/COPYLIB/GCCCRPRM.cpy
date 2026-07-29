           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C R P R M                         *        
      *                                                                *        
      *   1. CONTROL CARD INPUT TO GCCPFRME                            *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C P A R M                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 001031 - JAK   NEW COPYBOOK                              *        
      *  01 - 030319 - JW    FIX CONFIRMATION NUMBER PARM.             *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
      *01  GCCCRPRM-RECORD.                                                     
           05  GCCCRPRM-ASTERIX                    PIC X.                       
           05  GCCCRPRM-SENT-TO-PRINT-TS-FROM.                                  
               10 GCCCRPRM-STP-TS-FROM-YYYY        PIC 9(4).                    
               10 GCCCRPRM-STP-TS-FROM-MM          PIC 99.                      
               10 GCCCRPRM-STP-TS-FROM-DD          PIC 99.                      
               10 GCCCRPRM-STP-TS-FROM-HH          PIC 99.                      
               10 GCCCRPRM-STP-TS-FROM-MIN         PIC 99.                      
           05  FILLER                              PIC X.                       
           05  GCCCRPRM-SENT-TO-PRINT-TS-TO.                                    
               10 GCCCRPRM-STP-TS-TO-YYYY          PIC 9(4).                    
               10 GCCCRPRM-STP-TS-TO-MM            PIC 99.                      
               10 GCCCRPRM-STP-TS-TO-DD            PIC 99.                      
               10 GCCCRPRM-STP-TS-TO-HH            PIC 99.                      
               10 GCCCRPRM-STP-TS-TO-MIN           PIC 99.                      
           05  FILLER                              PIC X.                       
           05  GCCCRPRM-FORM-NAME                  PIC X(8).                    
           05  FILLER                              PIC X.                       
           05  GCCCRPRM-GROUP                      PIC X(7).                    
           05  FILLER                              PIC X.                       
           05  GCCCRPRM-DIV                        PIC X(3).                    
           05  FILLER                              PIC X.                       
           05  GCCCRPRM-CERT                       PIC X(10).                   
           05  FILLER                              PIC X.                       
           05  GCCCRPRM-CONF                       PIC X(11).                   
           05  FILLER                              PIC X.                       
