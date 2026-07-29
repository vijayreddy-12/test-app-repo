      *================================================================         
      *    COPYBOOK: MLCTRCIO                                                   
      *    CONTENT LOOKUP SERVICE - DATA SERVICE PROTOCOL                       
      *----------------------------------------------------------------         
      *    MAY/02 F. MUELLER  - CREATED                                         
      *----------------------------------------------------------------         
           05  MLCTGCIO-INPUT.                                                  
               10  MLCTGCIO-VERB              PIC X(16).                        
                   88  GCIO-OBTAIN-KEYED      VALUE 'OBTAIN  KEYED   '.         
                   88  GCIO-OBTAIN-FIRST      VALUE 'OBTAIN  FIRST   '.         
                   88  GCIO-OBTAIN-NEXT       VALUE 'OBTAIN  NEXT    '.         
                   88  GCIO-OBTAIN-FINISH     VALUE 'OBTAIN  FINISH  '.         
                   88  GCIO-OBTAIN-GTEQ       VALUE 'OBTAIN  GTEQ    '.         
                   88  GCIO-STORE             VALUE 'STORE           '.         
                   88  GCIO-MODIFY            VALUE 'MODIFY          '.         
                   88  GCIO-DELETE            VALUE 'DELETE          '.         
               10  MLCTGCIO-LR-NAME           PIC X(16).                        
                   88  GCIO-CONTENT-LR        VALUE 'CONTENT         '.         
               10  MLCTGCIO-KEY-IN            PIC X(64).                        
                                                                                
           05  MLCTGCIO-OUTPUT.                                                 
               10  MLCTGCIO-KEY-OUT           PIC X(64).                        
               10  MLCTGCIO-DATA-LENGTH       PIC S9(04) COMP.                  
               10  MLCTGCIO-RETURN-CODE       PIC X(02).                        
                   88  GCIO-OK                VALUE '00'.                       
                   88  GCIO-NOT-DELETED       VALUE '02'.                       
                   88  GCIO-NOT-FOUND         VALUE '03'.                       
                   88  GCIO-NOT-MODIFIED      VALUE '08'.                       
                   88  GCIO-NOT-STORED        VALUE '12'.                       
                   88  GCIO-INVALID-FUNCTION  VALUE '14'.                       
                   88  GCIO-INVALID-LR        VALUE '15'.                       
                   88  GCIO-INVALID-FUNC-LR   VALUE '16'.                       
                   88  GCIO-ERROR             VALUE '99'.                       
               10  MLCTGCIO-ERROR-STATUS  PIC X(08).                            
               10  MLCTGCIO-FILE-STATUS   REDEFINES                             
                                          MLCTGCIO-ERROR-STATUS.                
                   15  GCIO-FILE-STATUS   PIC X(02).                            
                   15  GCIO-VSAM-STATUS   PIC S9(4) COMP.                       
                   15  GCIO-VSAM-FUNCTION PIC S9(4) COMP.                       
                   15  GCIO-VSAM-FEEDBACK PIC S9(4) COMP.                       
               10  MLCTGCIO-CICS-STATUS   REDEFINES                             
                                          MLCTGCIO-ERROR-STATUS.                
                   15  GCIO-CICS-RESP     PIC S9(8) COMP.                       
                   15  GCIO-CICS-RESP2    PIC S9(8) COMP.                       
                                                                                
