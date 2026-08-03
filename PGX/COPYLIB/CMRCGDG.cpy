      *-----------------------------------------------------------------        
      * THIS IS THE FORMAT OF THE GENERATION RECORD THAT IS USED WITH           
      * THE CMR EXTRACTS.                                                       
      *-----------------------------------------------------------------        
      *---------------------------------------------------------------*         
       05 CMR-GENERATION-REC.                                                   
         10  CMR-RECORD-LENGTH     COMP SYNC   PIC S9(4).                       
         10  FILLER                            PIC XX.                          
         10  CMR-GENERATION.                                                    
             15  CMR-GENERATION-GCHAR          PIC X.                           
             15  CMR-GENERATION-NUMBER         PIC 9(4).                        
             15  CMR-GENERATION-VCHAR          PIC X.                           
             15  CMR-GENERATION-VERSION        PIC 9(2).                        
         10  FILLER                            PIC X(122) VALUE SPACES.         
