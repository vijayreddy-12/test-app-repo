      ******************************************************************        
      *    THIS COPYBOOK IS PART OF THE GENERAL CONTENT SERVICE                 
      *       MODULES ARE PREFIXED BY MLCT....                                  
      *    A COPYBOOK OF THE SAME NAME EXISTS AS A CPY TYPE                     
      *    ENSURING THAT CIIBC TYPE MODULES COMPILE PROPERLY                    
      *    INTO CICS AND BATCH PROGRAMS.                                        
      ******************************************************************        
      *    PLEASE NOTE THAT THE ARRAY SIZES IMPACT THE AMOUNT OF       *        
      *    PROGRAM STORAGE REQUIRED TO EXECUTE THE LOOKUP.  SINCE      *        
      *    CICS RUN-UNITS HAVE LIMITED AMOUNTS OF STORAGE, THESE       *        
      *    THESE LIMITATIONS ARE QUITE HIGH. ANY ARRAY SIZE CHANGES    *        
      *    MUST BE ACCOMPANIED BY A CHANGE TO COPYBOOK MLCTCRIN.       *        
      ******************************************************************        
           MOVE +100     TO WS-MAX-LOOKUP-COUNT.                                
           MOVE +32760   TO WS-MAX-INDEX-COUNT.                                 
           MOVE +8192000 TO WS-MAX-RESULTS-SIZE.                                
           MOVE +10      TO WS-ADD-STORAGE-BLOCKS.                              
           MOVE +1000    TO WS-MAX-STORAGE-BLOCKS.                              
                                                                                
