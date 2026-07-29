      ******************************************************************        
      * COBOL DECLARATION FOR TABLE TGD                                *        
      ******************************************************************        
       01  DCLTGD.                                                              
           10 GROUP-ID             PIC X(7).                                    
           10 DIV-ID               PIC X(3).                                    
           10 ACC-PM-REGIS-CD      PIC S9(7)V USAGE COMP-3.                     
           10 SPONSOR-NAME         PIC X(60).                                   
           10 SPONSOR-NAME-CAPS    PIC X(60).                                   
           10 REG-CD               PIC X(1).                                    
           10 CONTRACT-STAT-CD     PIC X(1).                                    
           10 CONTRIB-IND          PIC X(1).                                    
           10 AUTH-PMSS-IND        PIC X(1).                                    
           10 AUTH-PA-IND          PIC X(1).                                    
           10 AUTH-PM-ENROL-IND    PIC X(1).                                    
           10 BUS-SEG-CD           PIC X(1).                                    
           10 REG-GROUP-OFFICE     PIC X(5).                                    
           10 CHN-USER-ID          PIC X(20).                                   
           10 CHN-TS               PIC X(26).                                   
           10 MULTI-GROUP-IND      PIC X(1).                                    
           10 TAT                  PIC X(2).                                    
           10 CLM-REG-CD           PIC X(4).                                    
           10 HLTH-DENT-ONLY-IND   PIC X(1).                                    
           10 TMPLT-ID             PIC S9(3)V USAGE COMP-3.                     
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *        
      ******************************************************************        
