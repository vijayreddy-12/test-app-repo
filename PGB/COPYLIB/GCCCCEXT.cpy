      *****************************************************************         
      *     CLIENTS II CONFIRMATION LETTER                            *         
      *           EXTRACT FILE                                        *         
      *                                                               *         
      *       CREATED BY F. MARUCCI                                   *         
      *       USED    BY INTERNET  (PGB)                              *         
      *                                                               *         
      *****************************************************************         
      *    OCT/21/01  FRANK MARUCCI - CREATION                        *         
      *    NOV/05/02  KLYN          - ADD ACTIVATION KEY              *         
      *    FEB/23/04  HNS           - ADD CSC-PHONE AND SITE-URL      *         
      *    JUN/01/05  WB            - ADD LETTER TYPE FOR HP MBRS     *         
      *    MAY/15/06  JPD           - ADD LETTER TEMPLATE ID          *         
      *    SEP/11/06  M. PRANGE     - ADD REGISTRATION/PREACTIVATION  *         
      *                               INDICATOR                       *         
      *    JUN/13/07  BC            - ADD SPONSOR NAME                *         
      *    JAN/08/08  BC            - EXPAND SPONSOR NAME             *         
      *    MAR/24/10  PPAIK         - MKI LETTER ENHANCEMENT          *         
      *****************************************************************         
      *                                                                         
      *01  GCCCCEXT.                                                            
                                                                                
           05  GCCCCEXT-CUST-REPLY-LANG            PIC X(1).                    
           05  GCCCCEXT-MAIL-INSTRUCTION           PIC X(1).                    
           05  GCCCCEXT-CUST-GROUP                 PIC 9(7).                    
           05  GCCCCEXT-CUST-DIVISION              PIC X(3).                    
           05  GCCCCEXT-CUST-CERT                  PIC X(10).                   
           05  GCCCCEXT-CUST-NAME                  PIC X(85).                   
           05  GCCCCEXT-STAT-REAS-CD               PIC X.                       
           05  GCCCCEXT-CUST-ADDR1                 PIC X(30).                   
           05  GCCCCEXT-CUST-ADDR2                 PIC X(30).                   
           05  GCCCCEXT-CUST-ADDR3                 PIC X(30).                   
           05  GCCCCEXT-CUST-ADDR4                 PIC X(30).                   
           05  GCCCCEXT-PLAN-ADMIN-NAME            PIC X(30).                   
           05  GCCCCEXT-CO-NAME1                   PIC X(30).                   
           05  GCCCCEXT-CO-NAME2                   PIC X(30).                   
           05  GCCCCEXT-REGIST-DATE                PIC 9(8).                    
           05  GCCCCEXT-ACTN-KEY                   PIC X(6).                    
HNS        05  GCCCCEXT-CSC-PHONE                  PIC X(20).                   
HNS        05  GCCCCEXT-SITE-URL                   PIC X(100).                  
JPD        05  GCCCCEXT-TEMPLATE-ID                PIC X(3).                    
           05  GCCCCEXT-REG-PREACT-IND             PIC X(1).                    
               88  GCCCCEXT-REGISTRATION                 VALUE 'R'.             
               88  GCCCCEXT-PREACTIVATION                VALUE 'P'.             
           05  GCCCCEXT-SPONSOR-NAME               PIC X(50).                   
           05  GCCCCEXT-ENROL-DATE                 PIC X(56).                   
