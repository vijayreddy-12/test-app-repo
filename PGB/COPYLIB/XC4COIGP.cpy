      ******************************************************************        
      *    THIS COPYBOOK IS PART OF THE IGRP FILE ACCESS ROUTINE                
      *    A COPYBOOK OF THE SAME NAME EXISTS AS A CPYC TYPE                    
      *    ENSURING THAT CIIBC TYPE MODULES COMPILE PROPERLY                    
      *    INTO CICS AND BATCH PROGRAMS.                                        
      ******************************************************************        
                                                                                
           OPEN INPUT XC4SORIG-IGRP-FILE.                                       
                                                                                
           IF WS-FILE-STATUS = '97'                                             
              SET WS-FILE-STATUS-OK TO TRUE                                     
           END-IF.                                                              
                                                                                
           IF WS-FILE-STATUS (1:1) = '0'                                        
              SET WS-FILE-STATUS-OK TO TRUE                                     
           END-IF.                                                              
                                                                                
