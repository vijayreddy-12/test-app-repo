      ******************************************************************        
      *    THIS COPYBOOK IS PART OF THE IGRP FILE ACCESS ROUTINE                
      *    A COPYBOOK OF THE SAME NAME EXISTS AS A CPYC TYPE                    
      *    ENSURING THAT CIIBC TYPE MODULES COMPILE PROPERLY                    
      *    INTO CICS AND BATCH PROGRAMS.                                        
      ******************************************************************        
                                                                                
           SELECT XC4SORIG-IGRP-FILE ASSIGN       XC4DIGRP                      
                                     ORGANIZATION INDEXED                       
                                     ACCESS       DYNAMIC                       
                                     RECORD KEY   XC4SORIG-IGRP-KEY             
                                     FILE STATUS  WS-FILE-STATUS.               
