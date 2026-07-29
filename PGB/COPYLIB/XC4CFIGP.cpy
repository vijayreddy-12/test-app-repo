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
                                                                                
      * 01 IGRP-RECORD.                                                         
           05 IGRP-KEY.                                                         
               10 IGRP-CONTRACT           PIC 9(7).                             
           05 IGRP-ADMIN-SOURCE       PIC X(3).                                 
               88 IGRP-ADMIN-GFM             VALUE 'GFM'.                       
               88 IGRP-ADMIN-GPY             VALUE 'GPY'.                       
               88 IGRP-ADMIN-VO              VALUE 'VO '.                       
           05 IGRP-COVERAGE-SOURCE    PIC X(3).                                 
               88 IGRP-COV-GPY               VALUE 'COV'.                       
               88 IGRP-COV-NCV               VALUE 'NCV'.                       
           05 FILLER                  PIC X(15).                                
                                                                                
