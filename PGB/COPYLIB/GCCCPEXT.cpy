      *****************************************************************         
      *     CUSTOMER PROFILE DATABASE (CPD) CONFIRMATION LETTER       *         
      *           EXTRACT FILE                                        *         
      *                                                               *         
      *       CREATED BY F. MARUCCI                                   *         
      *       USED    BY INTERNET  (PGB)                              *         
      *                                                               *         
      *****************************************************************         
      *    OCT/01/01 FRANK MARUCCI - CREATION                         *         
      *    NOV/05/02 KLYN          - ADD ACTIVATION KEY               *         
      *    AUG/11/03 KLYN          - ADD ADDRESS FOR VO REG ITEMS     *         
      *    SEP/11/06 M. PRANGE     - ADDED REGISTRATION/PREACTIVATION *         
      *                              INDICATOR                        *         
      *    APR/16/07 W. BASHAM     - ADDED CO-NAME                    *         
      *    JUN/06/07 B. CHAPMAN    - ADDED SPONSOR-NAME               *         
      *    JAN/08/08 B. CHAPMAN    - EXPAND SPONSOR-NAME              *         
      *    MAR/18/10 P. PAIK       - ADD ENROLMENT END DATE           *         
      *****************************************************************         
      *                                                                         
      *01  GCCCPEXT.                                                            
                                                                                
           05  GCCCPEXT-GROUP-ID                   PIC 9(7).                    
           05  GCCCPEXT-CERT-ID                    PIC X(10).                   
           05  GCCCPEXT-REGN-DATE                  PIC 9(8).                    
           05  GCCCPEXT-ACTV-KEY                   PIC X(6).                    
           05  GCCCPEXT-REG-PREACT-IND             PIC X(1).                    
               88  GCCCPEXT-REGISTRATION                VALUE 'R'.              
               88  GCCCPEXT-PREACTIVATION               VALUE 'P'.              
           05  FILLER                              PIC X(1).                    
           05  GCCCPEXT-VO-DETAIL.                                              
               10  GCCCPEXT-CUST-ID              PIC S9(11)V                    
                                                       USAGE COMP-3.            
               10  GCCCPEXT-STAT-REAS-CD           PIC X.                       
               10  GCCCPEXT-LANG-CD                PIC X.                       
               10  GCCCPEXT-MAILING-DIV            PIC X(03).                   
               10  GCCCPEXT-LOCATION-CD            PIC S9(07) COMP-3.           
               10  GCCCPEXT-NAME-LINE1             PIC X(30).                   
               10  GCCCPEXT-NAME-LINE2             PIC X(30).                   
               10  GCCCPEXT-CO-NAME                PIC X(30).                   
               10  GCCCPEXT-ADDR-LINE1             PIC X(30).                   
               10  GCCCPEXT-ADDR-LINE2             PIC X(30).                   
               10  GCCCPEXT-ADDR-LINE3             PIC X(30).                   
               10  GCCCPEXT-ADDR-LINE4.                                         
                   15  GCCCPEXT-ADDR-PROV          PIC X(04).                   
                   15  GCCCPEXT-ADDR-POST-CODE     PIC X(10).                   
                   15  GCCCPEXT-ADDR-COUNTRY       PIC X(16).                   
               10  GCCCPEXT-SPONSOR-NAME           PIC X(50).                   
               10  GCCCPEXT-ENROL-DATE             PIC X(56).                   
                                                                                
