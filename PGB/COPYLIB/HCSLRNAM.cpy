      *01  LOGICAL-RECORD-NAMES.                                                
      *----------------------------------------------------------------*        
      *      HCSLRNAM - Logical Record Name Copybook.                  *        
      *                                                                *        
      *      Contains the Logical Record Names for Clients II.         *        
      *                                                                *        
      *      20JUL95 - Trudy Beckberger - Creation                     *        
      *                                                                *        
      *      22AUG95 - Craig White - Fixed typo on LR-HCS-COVERAGE-RO  *        
      *                            - added LR-HCS-EMPL-NARR            *        
      *                            - added LR-HCS-EMPL-NARR-RO         *        
      *                                                                *        
      *      31AUG95 - Craig White - Added LR-CII-TABLES-RO            *        
      *                                                                *        
      *      06FEB96 - Dave Soeder - Added LR-HCS-WSH-RO               *        
      *                            - Added LR-HCS-FPF-RO               *        
      *                            - Added LR-HCS-DRH-RO               *        
      *      15dec97 - Rob Kling   - Added LR-HCS-MTLOG                *        
      *                            - Added LR-HCS-MTLOG-RO             *        
      *      14JAN99 - B. PAYSON   - Added LR-HCS-WLTR                 *        
      *                                                                *        
      *      07SEP99 - P. FREIRE   - Added LR-HCS-IGRP-RO              *        
      *      24SEP99 - B. PAYSON   - Added LR-HCS-EFTBANK-RU           *        
      *                            - Added LR-HCS-EFTHIST-RU           *        
      *                            - Added LR-HCS-EFTHIST-S-RO         *        
      *                            - Added LR-HCS-EFTHIST-T-RO         *        
      *      08OCT99 - B. PAYSON   - Added LR-HCS-EFTBANK-RO           *        
      *                            - Added LR-HCS-EFTHIST-RO           *        
      *      24NOV99 - Regina S.   - Added LR-EMP-AIX-RO               *        
      *                            - Added LR-DFGXRF-AIX-RO            *        
      *      24NOV99 - Alex Braun  - Added LR-HCS-FEE                  *        
      *                            - Added LR-HCS-FEE-RO               *        
      *      01DEC99 - Regina S.   - Added LR-DFG-RO                   *        
      *      10JAN00 - MELANSON.   - Added LR-HCS-DEPOSIT-RU           *        
      *      05NOV01 - C. BERCH    - ADDED LR-HCS-XRE-RO               *        
      *                            - ADDED LR-HCS-XPC-RO               *        
+MAC1 *      10JUL02 - ROB WEBSTER - ADDED LR-HCS-DRH                  *        
+MAC1 *                                    LR-HCS-DPC                  *        
+MAC1 *                                    LR-HCS-CH1                  *        
+MAC1 *                                    LR-HCS-CH2                  *        
+MAC1 *                                    LR-HCS-BTX                  *        
+MAC1 *                                    LR-HCS-BTXC                 *        
+MAC1 *                                    LR-HCS-CVN                  *        
+MAC1 *                                    LR-HCS-CVA.                 *        
      *      14FEB03 - ROB WEBSTER - ADDED LR-HCS-CVx.                 *        
      *      27FEB03 - ROB KLING   - ADDED LR-HCS-CVN-RO.              *        
      *                                    LR-HCS-CVA-RO.              *        
      *      24APR03 - WARREN FINN - ADDED LR-HCS-DINTAB-RO.           *        
      *      02MAY03 - WARREN FINN - ADDED LR-HCS-EEOB-LOG.            *        
      *                                                                *        
      *      10JUL03 - LORNA REMULLA - ADDED LR-XC4CFCOB-RO (VSDSI81)  *        
      *                              - ADDED LR-XC4CFCOB    (VSDSO81)  *        
      *      22SEP03 - LORNA REMULLA - ADDED LR-XC4CFCOB-RO2(VSDSI82)  *        
      *                                                                *        
      *----------------------------------------------------------------*        
           05  CLIENTS-II-LOGICAL-RECORDS.                                      
             07  UPDATE-LOGICAL-RECORDS.                                        
      *  employee record - VSDSO20                                     *        
              10  LR-HCS-EMPLOYEE   VALUE 'HCS-EMPL-BASE   ' PIC X(16).         
      *  employee record (alternate index) - VSDSI79                   *        
              10  LR-EMP-AIX-RO     VALUE 'HCS-EMP3-AIX-RO ' PIC X(16).         
      *  employee narrative record - VSDSO83                           *        
              10  LR-HCS-EMPL-NARR  VALUE 'HCS-EMPL-NARR   ' PIC X(16).         
      *  coverage record - VSDSO21                                     *        
              10  LR-HCS-COVERAGE   VALUE 'HCS-EMPL-COVER  ' PIC X(16).         
      *  member   record - VSDSO22                                     *        
              10  LR-HCS-MEMBER     VALUE 'HCS-MEMBER      ' PIC X(16).         
      *  contract record - VSDSO23                                     *        
              10  LR-HCS-CONTRACT   VALUE 'HCS-CONTRACT    ' PIC X(16).         
      *  division record - VSDSO24                                     *        
              10  LR-HCS-DIVISION   VALUE 'HCS-DIVISION    ' PIC X(16).         
      *  location record - VSDSO25                                     *        
              10  LR-HCS-LOCATION   VALUE 'HCS-LOCATION    ' PIC X(16).         
      *  narrative record- VSDSO26                                     *        
              10  LR-HCS-NARRATIVE  VALUE 'HCS-NARRATIVE   ' PIC X(16).         
      *  bencat index record - VSDSO27                                 *        
              10  LR-HCS-BCI        VALUE 'HCS-BENCAT-IDX  ' PIC X(16).         
      *  bencat   record - VSDSO28                                     *        
              10  LR-HCS-BENCAT     VALUE 'HCS-BENCAT      ' PIC X(16).         
      *  benefit      record - VSDSO29                                 *        
              10  LR-HCS-BENEFIT    VALUE 'HCS-BENEFIT     ' PIC X(16).         
      *  isp-cheque      - VSDSO30                                     *        
              10  LR-HCS-CHEQUE     VALUE 'HCS-ISP-CHEQUE  ' PIC X(16).         
      *  service1        - VSDSO31                                     *        
              10  LR-HCS-SERVICE1   VALUE 'HCS-SERVICE1    ' PIC X(16).         
      *  service2        - VSDSO32                                     *        
              10  LR-HCS-SERVICE2   VALUE 'HCS-SERVICE2    ' PIC X(16).         
      *  pending         - VSDSO33                                     *        
              10  LR-HCS-PENDING    VALUE 'HCS-PENDING     ' PIC X(16).         
      *  diary           - VSDSO34                                     *        
              10  LR-HCS-DIARY      VALUE 'HCS-DIARY       ' PIC X(16).         
      *  eob             - VSDSO35                                     *        
              10  LR-HCS-EOB        VALUE 'HCS-EOB         ' PIC X(16).         
      *  xtr             - VSDSO36                                     *        
              10  LR-HCS-XTR        VALUE 'HCS-XTR         ' PIC X(16).         
      *  fee guide cross reference (alternate index) - VSDSI93                  
              10  LR-DFGXRF-AIX-RO  VALUE 'LR-DFGXRF-AIX-RO' PIC X(16).         
      *  dental fee guide - VSDSI94                                    *        
              10  LR-DFG-RO         VALUE 'LR-DFG-RO       ' PIC X(16).         
      *  dental deviations - VSDSO37                                   *        
              10  LR-HCS-DNTDEVN    VALUE 'HCS-DNTDEVN     ' PIC X(16).         
      *  benefit history - VSDSO38                                     *        
              10  LR-HCS-BENHIST    VALUE 'HCS-BENHIST     ' PIC X(16).         
      *  request         - VSDSO39                                     *        
              10  LR-HCS-REQUEST    VALUE 'HCS-REQUEST     ' PIC X(16).         
      *  deu batch header- VSDSO40                                     *        
              10  LR-HCS-DEU-BATCH  VALUE 'HCS-DEU-BATCH   ' PIC X(16).         
      *  letter history  - VSDSO42                                     *        
              10  LR-LETTER-HISTORY VALUE 'HCS-LETTR-HST   ' PIC X(16).         
      *  eft bank info   - VSDSI60                                     *        
              10  LR-HCS-EFTBANK-RO VALUE 'HCS-EFTBANK-RO  ' PIC X(16).         
      *  eft bank info   - VSDSO51                                     *        
              10  LR-HCS-EFTBANK-RU VALUE 'HCS-EFTBANK-RU  ' PIC X(16).         
      *  eft history     - VSDSI61  (main key)                         *        
              10  LR-HCS-EFTHIST-RO VALUE 'HCS-EFTHIST-RO  ' PIC X(16).         
      *  eft history     - VSDSO52  (main key)                         *        
              10  LR-HCS-EFTHIST-RU VALUE 'HCS-EFTHIST-RU  ' PIC X(16).         
      *  eft hist stat1  - VSDSI54                                     *        
              10  LR-HCS-EFTHIST-S-RO                                           
                              VALUE 'HCS-EFTHIST-S-RO' PIC X(16).               
      *  din table       - VSDSI58                                     *        
              10  LR-HCS-DINTAB-RO                                              
                                 VALUE 'LR-HCS-DINTAB-RO'    PIC X(16).         
      *  eeob log        - VSDSO59  (main key)                         *        
              10  LR-HCS-EEOB-LOG   VALUE 'HCS-EEOB-LOG    ' PIC X(16).         
      *  eft hist stat2  - VSDSI55                                     *        
              10  LR-HCS-EFTHIST-T-RO                                           
                              VALUE 'HCS-EFTHIST-T-RO' PIC X(16).               
      *  mt log record   - VSDSO61                                     *        
              10  LR-HCS-MTLOG      VALUE 'HCS-MTLOG       ' PIC X(16).         
      *  welcome letter history - VSDSI93                              *        
              10  LR-HCS-WLTR       VALUE 'HCS-WLTR-RU     ' PIC X(16).         
            07  READ-ONLY-LOGICAL-RECORDS.                                      
      *  IGRP - VSDSI50                                                *        
              10  LR-HCS-IGRP-RO                                                
                                    VALUE 'HCS-IGRP-RO     ' PIC X(16).         
      *  employee record - VSDSI60                                     *        
              10  LR-HCS-EMPLOYEE-RO                                            
                                    VALUE 'HCS-EMPL-BASE-RO' PIC X(16).         
      *  employee narrative record - VSDSO84                           *        
              10  LR-HCS-EMPL-NARR-RO                                           
                                    VALUE 'HCS-EMPL-NARR-RO' PIC X(16).         
      *  coverage record - VSDSI61                                     *        
              10  LR-HCS-COVERAGE-RO                                            
                                    VALUE 'HCS-EMPL-COVE-RO' PIC X(16).         
      *  mt log record   - VSDSI61                                     *        
              10  LR-HCS-MTLOG-RO                                               
                                    VALUE 'HCS-MTLOG-RO    ' PIC X(16).         
      *  member   record - VSDSI62                                     *        
              10  LR-HCS-MEMBER-RO                                              
                                    VALUE 'HCS-MEMBER-RO   ' PIC X(16).         
      *  contract record - VSDSI63                                     *        
              10  LR-HCS-CONTRACT-RO                                            
                                    VALUE 'HCS-CONTRACT-RO ' PIC X(16).         
      *  division record - VSDSI64                                              
              10  LR-HCS-DIVISION-RO                                            
                                    VALUE 'HCS-DIVISION-RO ' PIC X(16).         
      *  location record - VSDSI65                                     *        
              10  LR-HCS-LOCATION-RO                                            
                                    VALUE 'HCS-LOCATION-RO ' PIC X(16).         
      *  narrative record- VSDSI66                                     *        
              10  LR-HCS-NARRATIVE-RO                                           
                                    VALUE 'HCS-NARRATIVE-RO' PIC X(16).         
      *  bencat index record - VSDSI67                                 *        
              10  LR-HCS-BCI-RO                                                 
                                    VALUE 'HCS-BENCAT-IX-RO' PIC X(16).         
      *  bencat   record - VSDSI68                                     *        
              10  LR-HCS-BENCAT-RO                                              
                                    VALUE 'HCS-BENCAT-RO   ' PIC X(16).         
      *  benefit      record - VSDSI69                                 *        
              10  LR-HCS-BENEFIT-RO                                             
                                    VALUE 'HCS-BENEFIT-RO  ' PIC X(16).         
      *  isp-cheque      - VSDSI70                                     *        
              10  LR-HCS-CHEQUE-RO                                              
                                    VALUE 'HCS-ISP-CHEQU-RO' PIC X(16).         
      *  service1        - VSDSI71                                     *        
              10  LR-HCS-SERVICE1-RO                                            
                                    VALUE 'HCS-SERVICE1-RO ' PIC X(16).         
      *  service2        - VSDSI72                                     *        
              10  LR-HCS-SERVICE2-RO                                            
                                    VALUE 'HCS-SERVICE2-RO ' PIC X(16).         
      *  pending         - VSDSI73                                     *        
              10  LR-HCS-PENDING-RO                                             
                                    VALUE 'HCS-PENDING-RO  ' PIC X(16).         
      *  diary           - VSDSI74                                     *        
              10  LR-HCS-DIARY-RO                                               
                                    VALUE 'HCS-DIARY-RO    ' PIC X(16).         
      *  eob             - VSDSI75                                     *        
              10  LR-HCS-EOB-RO                                                 
                                    VALUE 'HCS-EOB-RO      ' PIC X(16).         
      *  xtr             - VSDSI76                                     *        
              10  LR-HCS-XTR-RO                                                 
                                    VALUE 'HCS-XTR-RO      ' PIC X(16).         
      *  dental deviations - VSDSI77                                   *        
              10  LR-HCS-DNTDEVN-RO                                             
                                    VALUE 'HCS-DNTDEVN-RO  ' PIC X(16).         
      *  benefit history - VSDSI78                                     *        
              10  LR-HCS-BENHIST-RO                                             
                                    VALUE 'HCS-BENHIST-RO  ' PIC X(16).         
      *  request         - VSDSI79                                     *        
              10  LR-HCS-REQUEST-RO                                             
                                    VALUE 'HCS-REQUEST-RO  ' PIC X(16).         
      *  deu batch header- read only - VSDSI80                         *        
              10  LR-HCS-DEU-BATCH-RO                                           
                                    VALUE 'HCS-DEU-BATCH-RO' PIC X(16).         
      *  letter history  - VSDSI82                                     *        
              10  LR-LETTER-HISTORY-RO                                          
                                    VALUE 'HCS-LETTR-HST-RO' PIC X(16).         
      *  Clients II tables - VSDSI85                                            
              10  LR-CII-TABLES-RO                                              
                                    VALUE 'HCS-CII-TABLES-R' PIC X(16).         
      *  Clients II FPF    - VSDSI88                                            
              10  LR-HCS-FPF-RO                                                 
                                    VALUE 'LR-HCS-FPF-RO   ' PIC X(16).         
      *  Clients II WSH    - VSDSI89                                            
              10  LR-HCS-WSH-RO                                                 
                                    VALUE 'LR-HCS-WSH-RO   ' PIC X(16).         
      *  Clients II DRH    - VSDSI90                                            
              10  LR-HCS-DRH-RO                                                 
                                    VALUE 'LR-HCS-DRH-RO   ' PIC X(16).         
      *  Letter history alt indx access RO    - VSDSI44                         
              10  LR-LETTER-HISTALT-RO                                          
                                    VALUE 'HCS-LETTR-ALT-RO' PIC X(16).         
      *  Letter history alte indx access      - VSDSO45                         
              10  LR-LETTER-HISTALT                                             
                                    VALUE 'HCS-LETTR-ALT   ' PIC X(16).         
      *  fee file, read only verison - VSDSI40                                  
              10  LR-HCS-FEE-RO                                                 
                                    VALUE 'HCS-XC4DFEE-RO  ' PIC X(16).         
      *  fee file, update version - VSDSo40                                     
              10  LR-HCS-FEE                                                    
                                    VALUE 'HCS-XC4DFEE     ' PIC X(16).         
      *  Deposit File, VSDSo97                                                  
              10  LR-HCS-DEPOSIT-RU                                             
                                    VALUE 'HCS-DEPOSIT-RU  ' PIC X(16).         
      *  Cross Reference FIle, alternate index - VSDSI86                        
              10  LR-HCS-XPC-RO                                                 
                                    VALUE 'HCS-XPC-RO      ' PIC X(16).         
      *  CROSS REFERENCE FILE, VSDSI87                                          
              10  LR-HCS-XRE-RO                                                 
                                    VALUE 'HCS-XRE-RO      ' PIC X(16).         
+MAC1 *  DRAFT HISTORY FILE (DRAFT ADDRESS)                                     
+MAC1         10  LR-HCS-DRH        VALUE 'LR-HCS-DRH      ' PIC X(16).         
+MAC1 *  PROCESSOR CONTROL COUNTS                                               
+MAC1         10  LR-HCS-DPC        VALUE 'LR-HCS-DPC      ' PIC X(16).         
+MAC1 *  CLAIMS HISTORY 1                                                       
+MAC1         10  LR-HCS-CH1        VALUE 'LR-HCS-CH1      ' PIC X(16).         
+MAC1 *  CLAIMS HISTORY 2                                                       
+MAC1         10  LR-HCS-CH2        VALUE 'LR-HCS-CH2      ' PIC X(16).         
+MAC1 *  BATCH TRANSACTION HISTORY                                              
+MAC1         10  LR-HCS-BTX        VALUE 'LR-HCS-BTX      ' PIC X(16).         
+MAC1 *  BATCH TRANSACTION HISTORY                                              
+MAC1         10  LR-HCS-BTXC       VALUE 'LR-HCS-BTXC     ' PIC X(16).         
+MAC1 *  HISTORY CONVERSION                                                     
+MAC1         10  LR-HCS-CVN        VALUE 'LR-HCS-CVN      ' PIC X(16).         
      *  HISTORY CONVERSION - READ ONLY                                         
              10  LR-HCS-CVN-RO     VALUE 'LR-HCS-CVN-RO   ' PIC X(16).         
+MAC1 *  HISTORY CONVERSION ALT INDEX                                           
+MAC1         10  LR-HCS-CVA        VALUE 'LR-HCS-CVA      ' PIC X(16).         
      *  HISTORY CONVERSION ALT INDEX - READ ONLY                               
              10  LR-HCS-CVA-RO     VALUE 'LR-HCS-CVA-RO   ' PIC X(16).         
+MAC2 *  HISTORY CONVERSION SVC LINE NUMBER                                     
+MAC2         10  LR-HCS-CVX        VALUE 'LR-HCS-CVX      ' PIC X(16).         
      *  COB CONVERSION                                                         
              10  LR-XC4CFCOB-RO    VALUE 'LR-XC4CFCOB-RO  ' PIC X(16).         
              10  LR-XC4CFCOB       VALUE 'LR-XC4CFCOB     ' PIC X(16).         
              10  LR-XC4CFCOB-RO2   VALUE 'LR-XC4CFCOB-RO2 ' PIC X(16).         
      *  HUB: VO COMMISSIONS - VO PSMASTER, VSDSI85, READ ONLY                  
              10  LR-VO-PSMASTER-R  VALUE 'LR-VO-PSMASTER-R' PIC X(16).         
      *  HUB: VO COMMISSIONS - VO PSMASTER, VSDSO86, READ/WRITE                 
              10  LR-VO-PSMASTER    VALUE 'LR-VO-PSMASTER  ' PIC X(16).         
      *  HUB: VO COMMISSIONS - VCM, VSDSI83, READ ONLY                          
              10  VO-CNTRCT-MST-RO  VALUE 'VO-CNTRCT-MST-RO' PIC X(16).         
      *  HUB: VO COMMISSIONS - VCM, VSDSO84, READ/WRITE                         
              10  VO-CNTRCT-MST     VALUE 'VO-CNTRCT-MST   ' PIC X(16).         
