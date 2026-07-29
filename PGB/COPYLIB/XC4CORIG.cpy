      ****************************************************************          
      *                                                                         
      * COPYBOOK: XC4CORIG                                                      
      *                                                                         
      * IGRP FILE ACCESS ROUTINE I/O PARMS-- FIXED LENGTH                       
      *                                  -- RECORD LENGTH = 495                 
      *                                                                         
      * C. BERCH APR 2003   CREATION.                                           
      ****************************************************************          
      *01 XC4SORIG-LINKAGE.                                                     
          05 ORIG-INPUT-PARMS.                                                  
             10 ORIG-INPUT-REQUEST           PIC X(2).                          
                88 ORIG-OPEN                           VALUE 'OP'.              
                88 ORIG-CLOSE                          VALUE 'CL'.              
                88 ORIG-READ-KEYED                     VALUE 'RK'.              
             10 ORIG-INPUT-CONTRACT          PIC 9(7).                          
          05 ORIG-OUTPUT-FIELDS.                                                
             10 ORIG-RETURN-CODE             PIC X(2).                          
                88 ORIG-SUCCESSFUL                     VALUE '00'.              
                88 ORIG-IO-ERROR                       VALUE '08'.              
                88 ORIG-NOT-FOUND                      VALUE '16'.              
             10 ORIG-ADMIN-SOURCE            PIC X(3).                          
                88 ORIG-ADMIN-GFM                      VALUE 'GFM'.             
                88 ORIG-ADMIN-GPY                      VALUE 'GPY'.             
                88 ORIG-ADMIN-VO                       VALUE 'VO '.             
             10 ORIG-COVERAGE-SOURCE         PIC X(3).                          
                88 ORIG-COV-GPY                        VALUE 'COV'.             
                88 ORIG-COV-NCV                        VALUE 'NCV'.             
             10 ORIG-CL2-COV-BEN-CODE        PIC X(2).                          
                88 ORIG-COV-BEN-CODE-VALUED            VALUE 'VL'.              
                88 ORIG-COV-BEN-CODE-BLANK             VALUE 'NV'.              
          05 FILLER                          PIC X(61).                         
                                                                                
