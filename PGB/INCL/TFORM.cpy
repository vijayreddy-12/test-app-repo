      ******************************************************************        
      * COBOL DECLARATION FOR TABLE TFORM                              *        
      ******************************************************************        
       01  DCLTFORM.                                                            
           10 FORM-ID              PIC S9(11)V USAGE COMP-3.                    
           10 FORM-SEQ-NUM         PIC S9(3)V USAGE COMP-3.                     
           10 FORM-DATA-STRING.                                                 
              49 FORM-DATA-STRING-LEN  PIC S9(4) USAGE COMP.                    
              49 FORM-DATA-STRING-TEXT  PIC X(32000).                           
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *        
      ******************************************************************        
