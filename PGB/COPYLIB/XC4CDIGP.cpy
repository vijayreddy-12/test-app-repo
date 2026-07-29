      ******************************************************************        
      *                                                                         
      *    THIS COPYBOOK IS PART OF THE IGRP FILE ACCESS ROUTINE                
      *    A COPYBOOK OF THE SAME NAME EXISTS AS A CPYC TYPE                    
      *    ENSURING THAT CIIBC TYPE MODULES COMPILE PROPERLY                    
      *    INTO CICS AND BATCH PROGRAMS.                                        
      *                                                                         
      ******************************************************************        
      ******************************************************************        
      *                                                                         
      *  CHANGE LOG:         X C 4 C F I G P                                    
      * *************                                                           
      *                                                                         
      *  NO    DATE    PGR                DESCRIPTION                           
      *  --   ------   ---  -----------------------------------------           
      *                                                                         
      *  00 - YYMMDD - AAA  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX           
      *                                                                         
      ******************************************************************        
                                                                                
      *-----------------------------------------------------------------        
      *    COPYBOOK DEFINITION VSAM FILE                                        
      *-----------------------------------------------------------------        
       FD  XC4SORIG-IGRP-FILE                                                   
           DATA RECORD IS XC4SORIG-IGRP-RECORD                                  
           LABEL RECORDS ARE STANDARD.                                          
                                                                                
       01  XC4SORIG-IGRP-RECORD.                                                
           05  XC4SORIG-IGRP-KEY                   PIC 9(7).                    
           05  FILLER                              PIC X(21).                   
                                                                                
