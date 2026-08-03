      *-----------------------------------------------------------------        
      * THIS IS THE FORMAT OF THE TRAILER RECORD THAT IS APPENDED TO            
      * THE CMR GIPSY EXTRACT.                                                  
      *-----------------------------------------------------------------        
      *---------------------------------------------------------------*         
       05 CMR-TRAILER.                                                          
         10  CMR-RECORD-TYPE                 PIC X VALUE 'T'.                   
         10  CMR-TRAILER-RUN-DATE.                                              
             15  CMR-TRAILER-RUN-YEAR        PIC 9(4).                          
             15  CMR-TRAILER-RUN-MONTH       PIC 9(2).                          
             15  CMR-TRAILER-RUN-DAY         PIC 9(2).                          
         10  CMR-TRAILER-RUN-TIME            PIC 9(8).                          
         10  CMR-TRAILER-REC-COUNT           PIC 9(8).                          
         10  CMR-TRAILER-GDG                 PIC X(8).                          
         10  CMR-TRAILER-JOBNAME             PIC X(10).                         
         10  CMR-TRAILER-FILENAME            PIC X(15).                         
