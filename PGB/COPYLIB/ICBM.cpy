      ****************************************************************          
      *  INTERFACE CONTROL BLOCK (MANUFACTURERS) COPY BOOK           *          
      ****************************************************************          
      *                                                              *          
      *  This is used with the GLH data server in the format:        *          
      *                                                              *          
      *     CALL 'server' USING data-service-verb                    *          
      *                         logical-record-name                  *          
      *                         ICBM.                                *          
      *                                                              *          
      *  30APR93 - Alex Paraschuk - new version that includes all    *          
      *            that's required for both PC and mainframe         *          
      *                                                              *          
      ****************************************************************          
      *01  ICBM.                                                                
           05  LOGICAL-RECORD-NAME                 PIC X(16).                   
           05  ICBM-PROGRAM-NAME                   PIC X(08).                   
           05  LINKAGE-CONTROL.                                                 
               10  LINKAGE-ANCHOR                  PIC X(04).                   
               10  LINKAGE-FILLER                  PIC X(04).                   
           05  LINKAGE-STATUS.                                                  
               10  PROGRAM-LINKAGE-STATUS          PIC X(04).                   
      ****************************************************************          
      *    some codes apply to the PC platform, some to the          *          
      *    mainframe, and some to both platforms                     *          
      ****************************************************************          
                   88  LR-STATUS-OK                VALUE '0000'.                
                   88  PARTIAL-LR-FOUND            VALUE '03  '.                
                   88  PARTIAL-LR-FOUND-04         VALUE '04  '.                
                   88  PARTIAL-LR-FOUND-05         VALUE '05  '.                
                   88  PARTIAL-LR-FOUND-06         VALUE '06  '.                
                   88  LR-NOT-FOUND                VALUE '0307'.                
                   88  KEYED-LR-NOT-FOUND          VALUE '0326'.                
                   88  LR-NOT-STORED               VALUE '1205'                 
                                                         '1299'.                
                   88  LR-DUP-NOT-STORED           VALUE '1205'.                
                   88  LR-NOT-MODIFIED             VALUE '0826'                 
                                                         '0899'.                
                   88  LR-NOT-ERASED               VALUE '0226'                 
                                                         '0299'.                
                   88  VERB-NOT-AVAILABLE          VALUE '9900'.                
                   88  PROGRAM-NAME-NOT-SET        VALUE '9900'.                
                   88  LR-UNKNOWN                  VALUE '9902'.                
                   88  VERB-NOT-RECOGNIZED         VALUE '9903'.                
                   88  MAXIMUM-FILES-OPEN          VALUE '9910'.                
                   88  NO-DATABASE-ACCESSED        VALUE '9914'.                
               10  FILLER REDEFINES PROGRAM-LINKAGE-STATUS.                     
                   15  PLS-PRIMARY                 PIC X(02).                   
                       88  SYSTEM-ERROR-RETURNED   VALUE '99'.                  
                   15  PLS-SECONDARY               PIC X(02).                   
               10  ENVIRONMENT-LINKAGE-STATUS.                                  
                   15  ELS-PRIMARY                 PIC X(02).                   
                   15  ELS-SECONDARY               PIC X(02).                   
