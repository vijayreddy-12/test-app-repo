      *=================================================================        
      *=                                                              =*        
      *=  PGW.ADMN.PROD.AGTADDR.VSAM RECORD LAYOUT                    =*00000120
      *=   CREATED VIA:                                               =*        
      *=       PAG.PROD.DUMPMAC.DISK AND                              =*        
      *=       PAG.PROD.DUMPAGEN.DISK                                 =*        
      *=   USING COPYBOOK PEN.PAG.PROD.COPYLIB(SAG51C01)              =*        
      *=================================================================        
                                                                                
       01  AGENT-ADDRESS-RECORD.                                                
           05  AGENT-KEY.                                                       
               10  AGENT-CODE                      PIC X(06).                   
           05  AGENT-BRANCH                        PIC 9(5).                    
           05  FILLER                              PIC X.                       
           05  AGENT-NAME.                                                      
                  09  AGENT-NAME1                  PIC X(25).                   
                  09  AGENT-NAME2                  PIC X(25).                   
           05  AGENT-ADDRESS.                                                   
               10  AGENT-ADDRESS-LINE1             PIC X(25).                   
               10  AGENT-ADDRESS-LINE2             PIC X(25).                   
               10  AGENT-ADDRESS-LINE3             PIC X(25).                   
               10  AGENT-ADDRESS-LINE REDEFINES AGENT-ADDRESS-LINE3.            
                   15  AGENT-CITY                 PIC X(17).                    
                   15  AGENT-POSTAL-SYMBOL        PIC X(2).                     
                   15  AGENT-POSTAL-CODE.                                       
                       20 AGENT-POSTAL-CODE1      PIC X(3).                     
                       20 AGENT-POSTAL-CODE2      PIC X(3).                     
           05  AGENT-IDENT-CODE                    PIC X(6).                    
           05  FILLER                              PIC X(07).                   
