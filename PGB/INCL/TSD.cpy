      ******************************************************************        
      * COBOL DECLARATION FOR TABLE TSD                                *        
      ******************************************************************        
       01  DCLTSD.                                                              
           10 CONFIRM-ID           PIC S9(11)V USAGE COMP-3.                    
           10 SEQ-NUM              PIC S9(5)V USAGE COMP-3.                     
           10 FORM-CD              PIC X(8).                                    
           10 LANG-CD              PIC X(1).                                    
           10 GROUP-ID             PIC X(7).                                    
           10 DIV-ID               PIC X(3).                                    
           10 CERT-ID              PIC X(10).                                   
           10 WEB-SENT-TS          PIC X(26).                                   
           10 FORM-RECV-TS         PIC X(26).                                   
           10 SENT-TO-PRINT-TS     PIC X(26).                                   
           10 PURGE-TS             PIC X(26).                                   
           10 FORM-ID              PIC S9(11)V USAGE COMP-3.                    
           10 FORM-SEQ-NUM         PIC S9(3)V USAGE COMP-3.                     
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *        
      ******************************************************************        
