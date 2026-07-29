      ****************************************************************          
      *                                                              *          
      *  MLCTRRLU - CONTENT LOOKUP SERVICE OVERRIDE REGISTRY         *          
      *                                                              *          
      *  THIS TABLE WILL BE PROVIDED TO SUPPORT ANY FUTURE NEEDS FOR *          
      *  CUSTOMIZATION THAT MAY BE REQUIRED TO DRIVE OPTIMAL LOOKUP  *          
      *  PERFORMANCE.  PLEASE NOTE THAT THE DEFAULT ENTRY MUST BE    *          
      *  PLACED IN THE FINAL ENTRY OF THIS TABLE.                    *          
      *--------------------------------------------------------------*          
      *    A COPYBOOK OF THE SAME NAME EXISTS AS A CPYC TYPE         *          
      *    ENSURING THAT CIIBC TYPE MODULES COMPILE PROPERLY         *          
      *    INTO CICS AND BATCH PROGRAMS.                             *          
      *--------------------------------------------------------------*          
      *  CHANGE LOG:                                                 *          
      *                                                              *          
      *  AUTHOR       DATE     DESCRIPTION                           *          
      ****************************************************************          
      *  F. MUELLER   020606   INITIAL CREATION - IEL PROJECT        *          
      *  N JEFFREY    030113   CHANGE TO IMPROVE PERFORMANCE AND                
      *                        REDUCE IO. ONLY SPECIFIC TABLES CAN              
      *                        HAVE DEFAULTS AS PER MLCTRRLU                    
      ****************************************************************          
        05 MLCTRRLU-NUM-ENTRIES          PIC S9(04) COMP VALUE +6.              
        05 MLCTRRLU-LOOKUP-OVERRIDES.                                           
      *   10 X VALUE 'ALIAS--- NAME---- LRNAME---------- N ' PIC X(37).         
      *   10 X VALUE 'SAMPLE   SAMPLE   SAMPLE-LR        N ' PIC X(37).         
      *   10 X VALUE 'SAMPLOPT SAMPLE   SAMPLE-LR        O ' PIC X(37).         
          10 X VALUE 'CHEQLOGO CHEQLOGO CONTENT          OD' PIC X(37).         
          10 X VALUE 'CLMSADDR CLMSADDR CONTENT          OD' PIC X(37).         
          10 X VALUE 'EOBPRXCP EOBPRXCP CONTENT          OD' PIC X(37).         
          10 X VALUE 'HCSATRAN HCSATRAN CONTENT          OD' PIC X(37).         
          10 X VALUE 'WEBADDRS WEBADDRS CONTENT          OD' PIC X(37).         
          10 X VALUE '******** ******** CONTENT          O ' PIC X(37).         
                                                                                
        05 FILLER REDEFINES MLCTRRLU-LOOKUP-OVERRIDES.                          
           10  MLCTRRLU-OVERRIDE-ENTRY   OCCURS 6 TIMES.                        
               15  RRLU-LOOKUP-ALIAS     PIC X(08).                             
               15  FILLER                PIC X(01).                             
               15  RRLU-LOOKUP-NAME      PIC X(08).                             
               15  FILLER                PIC X(01).                             
               15  RRLU-LOOKUP-LR-NAME   PIC X(16).                             
               15  FILLER                PIC X(01).                             
               15  RRLU-LOOKUP-OPTION    PIC X(01).                             
                   88  RRLU-NORMAL-LOOKUP    VALUE 'N'.                         
                   88  RRLU-OPTIMIZED-LOOKUP VALUE 'O'.                         
               15  RRLU-LOOKUP-DEFAULTS  PIC X(01).                             
                   88  RRLU-LOOKUP-DEF-NONE  VALUE ' '.                         
                   88  RRLU-LOOKUP-DEFAULT   VALUE 'D'.                         
                                                                                
