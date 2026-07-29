      ****************************************************************          
      *                                                              *          
      *  MLCTRGLU - GENERIC LOOKUP SERVICE PROTOCOL                  *          
      *                                                              *          
      *--------------------------------------------------------------*          
      *  CHANGE LOG:                                                 *          
      *                                                              *          
      *  AUTHOR       DATE     DESCRIPTION                           *          
      ****************************************************************          
      *  F. MUELLER   020508   INITIAL CREATION - IEL PROJECT        *          
      *  N JEFFREY    030113   CHANGE TO IMPROVE PERFORMANCE AND     *          
      *                        REDUCE I/O. ONLY SPECIFIC TABLES CAN  *          
      *                        HAVE DEFAULTS AS PER MLCTRRLU         *          
      *  F. MUELLER   030116   ADD NEW OPTION TO REDUCE I/O WHEN A   *          
      *                        REQUEST FOR MOST CURRENT HISTORY ONLY *          
      ****************************************************************          
           05  MLCTGGLU-INPUT.                                                  
               10  GGLU-LOOKUP-NAME            PIC X(08).                       
               10  GGLU-LOOKUP-ARGUMENT        PIC X(40).                       
               10  GGLU-LOOKUP-EFF-DATE        PIC 9(08).                       
               10  GGLU-LOOKUP-PROC-DATE       PIC 9(08).                       
               10  GGLU-MAX-DATA-LENGTH        PIC S9(4) COMP.                  
               10  GGLU-LOOKUP-LR-NAME         PIC X(16).                       
                   88  GGLU-CONTENT-LR         VALUE 'CONTENT         '.        
               10  GGLU-LOOKUP-OPTION          PIC X(01).                       
                   88  GGLU-NORMAL-LOOKUP      VALUE 'N'.                       
                   88  GGLU-OPTIMIZED-LOOKUP   VALUE 'O'.                       
               10  GGLU-DEFAULT-OPTION         PIC X(01).                       
                   88  GGLU-LOOKUP-DEF-NONE    VALUE ' '.                       
                   88  GGLU-LOOKUP-DEFAULT     VALUE 'D'.                       
               10  GGLU-HISTORY-OPTION         PIC X(01).                       
                   88  GGLU-HISTORY-CURRENT    VALUE ' '.                       
                   88  GGLU-HISTORY-BROWSE     VALUE 'H'.                       
           05  MLCTGGLU-OUTPUT.                                                 
               10  GGLU-RETURN-STATUS.                                          
                   15  GGLU-RETURN-CODE        PIC X(02).                       
                       88  GGLU-RET-OK         VALUE '00'.                      
                       88  GGLU-RET-NOT-FOUND  VALUE '02'.                      
                       88  GGLU-RET-ERROR      VALUE '99'.                      
                   15  GGLU-RETURN-SUB-CODE    PIC X(02).                       
               10  GGLU-OK-STATUS    REDEFINES GGLU-RETURN-STATUS               
                                               PIC X(04).                       
                       88  GGLU-OK                  VALUE '00  '.               
                       88  GGLU-DEFAULT             VALUE '0001'.               
                       88  GGLU-TRUNCATED           VALUE '0002'.               
                       88  GGLU-TRUNC-DEFAULT       VALUE '0003'.               
                       88  GGLU-NON-CRITICAL-ERROR  VALUE '0099'.               
               10  GGLU-NF-STATUS    REDEFINES GGLU-RETURN-STATUS               
                                               PIC X(04).                       
                       88  GGLU-NOT-FOUND           VALUE '02  '.               
                       88  GGLU-TABLE-NOT-FOUND     VALUE '0201'.               
                       88  GGLU-ENTRY-NOT-FOUND     VALUE '0202'.               
               10  GGLU-ERROR-STATUS REDEFINES GGLU-RETURN-STATUS               
                                               PIC X(04).                       
                       88  GGLU-ERROR               VALUE '99  '.               
                       88  GGLU-INVALID-EFF-DATE    VALUE '9901'.               
                       88  GGLU-INVALID-PROC-DATE   VALUE '9902'.               
                       88  GGLU-INVALID-DATA-LENGTH VALUE '9903'.               
                       88  GGLU-IO-ERROR            VALUE '9904'.               
               10  GGLU-OUTPUT-LENGTH          PIC S9(04) COMP.                 
               10  GGLU-ERROR-DETAILS.                                          
                   15  GGLU-ERR-PGM-ID         PIC X(8).                        
                   15  GGLU-ERR-PGM-STATUS     PIC X(8).                        
                   15  GGLU-ERR-PGM-LVL        PIC X(30).                       
                   15  GGLU-ERR-PGM-DESC       PIC X(60).                       
                                                                                
