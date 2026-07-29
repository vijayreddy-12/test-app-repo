      ****************************************************************          
      *                                                              *          
      *  MLCTRCLU - GENERAL CONTENT SERVICE LOOKUP PROTOCOL          *          
      *                                                              *          
      *--------------------------------------------------------------*          
      *  CHANGE LOG:                                                 *          
      *                                                              *          
      *  AUTHOR       DATE     DESCRIPTION                           *          
      ****************************************************************          
      *  F. MUELLER   020508   INITIAL CREATION - IEL PROJECT        *          
      ****************************************************************          
      *01  MLCTOCLU-PROTOCOL.                                                   
           05  MLCTOCLU-INPUT.                                                  
               10  OCLU-CONTENT-NAME           PIC X(08).                       
               10  OCLU-CONTENT-ARGUMENT       PIC X(40).                       
               10  OCLU-CONTENT-EFF-DATE       PIC 9(08).                       
               10  OCLU-CONTENT-PROC-DATE      PIC 9(08).                       
               10  OCLU-MAX-DATA-LENGTH        PIC S9(04) COMP.                 
                                                                                
           05  MLCTOCLU-OUTPUT.                                                 
               10  OCLU-RETURN-STATUS.                                          
                   15  OCLU-RETURN-CODE        PIC X(02).                       
                       88  OCLU-RET-OK         VALUE '00'.                      
                       88  OCLU-RET-NOT-FOUND  VALUE '02'.                      
                       88  OCLU-RET-ERROR      VALUE '99'.                      
                   15  OCLU-RETURN-SUB-CODE    PIC X(02).                       
               10  OCLU-OK-STATUS    REDEFINES OCLU-RETURN-STATUS               
                                               PIC X(04).                       
                       88  OCLU-OK                  VALUE '00  '.               
                       88  OCLU-DEFAULT             VALUE '0001'.               
                       88  OCLU-TRUNCATED           VALUE '0002'.               
                       88  OCLU-TRUNC-DEFAULT       VALUE '0003'.               
               10  OCLU-NF-STATUS    REDEFINES OCLU-RETURN-STATUS               
                                               PIC X(04).                       
                       88  OCLU-NOT-FOUND           VALUE '02  '.               
               10  OCLU-ERROR-STATUS REDEFINES OCLU-RETURN-STATUS               
                                               PIC X(04).                       
                       88  OCLU-ERROR               VALUE '99  '.               
                       88  OCLU-INVALID-EFF-DATE    VALUE '9901'.               
                       88  OCLU-INVALID-PROC-DATE   VALUE '9902'.               
                       88  OCLU-INVALID-DATA-LENGTH VALUE '9903'.               
                       88  OCLU-IO-ERROR            VALUE '9904'.               
               10  OCLU-OUTPUT-LENGTH          PIC S9(04) COMP.                 
               10  OCLU-ERROR-DETAILS.                                          
                   15  OCLU-ERR-PGM-ID         PIC X(8).                        
                   15  OCLU-ERR-PGM-STATUS     PIC X(8).                        
                   15  OCLU-ERR-PGM-LVL        PIC X(30).                       
                   15  OCLU-ERR-PGM-DESC       PIC X(60).                       
                                                                                
