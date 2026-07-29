      ******************************************************************        
      *    THIS COPYBOOK IS PART OF THE IGRP FILE ACCESS ROUTINE                
      *    A COPYBOOK OF THE SAME NAME EXISTS AS A CPYC TYPE                    
      *    ENSURING THAT CIIBC TYPE MODULES COMPILE PROPERLY                    
      *    INTO CICS AND BATCH PROGRAMS.                                        
      ******************************************************************        
           MOVE WS-IGRP-KEY TO XC4SORIG-IGRP-KEY.                               
                                                                                
           READ XC4SORIG-IGRP-FILE                                              
                KEY IS XC4SORIG-IGRP-KEY.                                       
                                                                                
           IF WS-FILE-STATUS = '97'                                             
              SET WS-FILE-STATUS-OK TO TRUE                                     
           END-IF.                                                              
                                                                                
           IF WS-FILE-STATUS (1:1) = '0'                                        
              SET WS-FILE-STATUS-OK TO TRUE                                     
           END-IF.                                                              
                                                                                
           IF WS-FILE-STATUS-OK                                                 
              MOVE XC4SORIG-IGRP-RECORD TO IGRP-RECORD                          
           END-IF.                                                              
