      ***********************************************************               
      * RECORD LAYOUT FOR INTERNET REGISTRATION LETTER BACK PAGE                
      * ONLY.  FRONT PAGE COPYBOOK GCCREGL). AFP FORMAT.                        
      ***********************************************************               
      *2003/12/19|J.WODNICKI  |GBSS TASK 28002 - GROUP AND CERT *               
      *          |            |TO BE PRINTED ON BACKPAGE        *               
      ***********************************************************               
      *2004/03/02|W.BASHAM    |GBSS TASK 29272 - ADD VARIABLE   *               
      *          |            |URL TO BACKPAGE                  *               
      ***********************************************************               
           05 GCCCREGB-OUT-LETTER.                                              
              10  GCCCREGB-OUT-CC           PIC X.                              
              10  GCCCREGB-OUT-TYPE         PIC 9(3).                           
              10  GCCCREGB-OUT-GROUP        PIC 9(7).                           
              10  GCCCREGB-OUT-CERT         PIC X(10).                          
WB            10  GCCCREGB-OUT-URL          PIC X(100).                         
              10  FILLER                    PIC X(10).                          
                                                                                
