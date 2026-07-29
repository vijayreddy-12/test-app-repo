      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG:         X C 4 C F E M P                           *        
      * *************                                                  *        
      *                                                                *        
      *  NO    DATE    PGR                DESCRIPTION                  *        
      *  --   ------   ---  -----------------------------------------  *        
      *                                                                *        
      *  00 - YYMMDD - AAA  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *        
      *                                                                *        
      *  01 - 870707 - BJK  REDESIGN FOR CONFEDERATION LIFE            *        
      *                                                                *        
      *  02 - 880216 - KES  ADD: 88 LEVELS FOR PROVINCE                *        
      *                                                                *        
      *  03 - 880223 - KES  CHANGE TO FILLER: EMP-EE-STATUS            *        
      *                                                                *        
      *  04 - 880224 - KES  DELETED 88 LEVEL CERTIFICATE-AND-INSURED   *        
      *                     OF EMP-MANUAL-PROCESS-INDICATOR            *        
      *                                                                *        
      *  05 - 950714 - MC   ADD 05 DIRECT-MAIL-IND FIELD               *        
      *                     RE CLDA110A.825                            *        
      *                                                                *        
      *  06 - 970729 - CP   ADDED 'ALPHA' LEVEL ABOVE THE COMP-3       *        
      *                     FIELDS (FOR ELIGIBILITY PROJECT)           *        
      *                                                                *        
      *  07 - 020930 - CWC  EEOB PROJECT - CHANGED UNUSED FIELDS       *        
      *                      EMP-LAST-EVIDENCE-DATE                    *        
      *                      EMP-DATE-LAST-SERVICE-PRIOR               *        
      *                      EMP-INITIAL-CLAIM-DT                      *        
      *                     TO FILLER AND ADDED AN EEOB INDICATOR      *        
      *                     'Y' MEANS ELECTRONIC ON WEB                *        
      *                     'N' MEANS HARDCOPY                         *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA  ****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA  ****************************        
                                                                  SKIP1         
      * 01 EMP-EMPLOYEE-REC.                                                    
           05 EMP-EMPLOYEE-ALT-KEY.                                             
              10 EMP-PHONETIC-NAME                PIC  X(04).                   
              10 EMP-FIRST-INITIAL                PIC  X(01).                   
              10 EMP-EMPLOYEE-KEY.                                              
                 15 EMP-CONTRACT                  PIC  9(06).                   
                 15 EMP-CERT                      PIC  X(10).                   
           05 EMP-FAMILY-NUM.                                                   
              10 EMP-FAMILY-NUMBER                PIC S9(09)     COMP-3.        
           05 EMP-GATE-SM-IND                     PIC  X(01).                   
           05 EMP-CONV-STATUS                     PIC  X(01).                   
           05 EMP-EE-NAME                         PIC  X(30).                   
           05 EMP-EE-OLD-NAME                     PIC  X(30).                   
           05 EMP-EE-SEX                          PIC  X(01).                   
           05 EMP-PROVINCE-CD                     PIC  X(01).                   
              88 VALID-PROVINCE                   VALUE 'A' THRU 'J'.           
              88 BRITISH-COLUMBIA                 VALUE 'A'.                    
              88 ALBERTA                          VALUE 'B'.                    
              88 SASKATCHEWAN                     VALUE 'C'.                    
              88 MANITOBA                         VALUE 'D'.                    
              88 ONTARIO                          VALUE 'E'.                    
              88 QUEBEC                           VALUE 'F'.                    
              88 NEW-BRUNSWICK                    VALUE 'G'.                    
              88 NOVA-SCOTIA                      VALUE 'H'.                    
              88 PRINCE-EDWARD-ISLAND             VALUE 'I'.                    
              88 NEWFOUNDLAND                     VALUE 'J'.                    
              88 YUKON                            VALUE 'Y'.                    
              88 NWT                              VALUE 'N'.                    
              88 OUTSIDE-CANADA                   VALUE 'U'.                    
              88 PROVINCE-OF-RESIDENCE            VALUE 'K'.                    
              88 PROVINCE-OF-TREATMENT            VALUE 'L'.                    
              88 NOT-KNOWN                        VALUE 'S'.                    
              88 NOT-ENTERED                      VALUE SPACES.                 
           05 EMP-SIN                             PIC  X(09).                   
           05 EMP-BIRTH-DT-X.                                                   
              10 EMP-BIRTH-DT                     PIC S9(07)     COMP-3.        
           05 EMP-RETIREMENT-DT-X.                                              
              10 EMP-RETIREMENT-DT                PIC S9(07)     COMP-3.        
           05 EMP-DEP-EFF-DT-X.                                                 
              10 EMP-DEP-EFF-DT                   PIC S9(07)     COMP-3.        
           05 EMP-DEP-TERM-DT-X.                                                
              10 EMP-DEP-TERM-DT                  PIC S9(07)     COMP-3.        
           05 EMP-CONV-EFF-DT-X.                                                
              10 EMP-CONV-EFF-DT                  PIC S9(07)     COMP-3.        
      ***                            CHGLOG START - 07 - 020930 - CWC  *        
           05 EMP-EEOB-IND                        PIC  X(01).                   
              88 EMP-EEOB-VALID             VALUE 'Y' 'N' SPACE.                
              88 EMP-EEOB-NOT-ENTERED       VALUE SPACE.                        
              88 EMP-EEOB-YES               VALUE 'Y'.                          
              88 EMP-EEOB-NO                VALUE 'N'.                          
           05 FILLER                              PIC  X(11).                   
      *    05 EMP-LAST-EVIDENCE-DATE-X.                                         
      *       10 EMP-LAST-EVIDENCE-DATE           PIC S9(07)     COMP-3.        
      *    05 EMP-DATE-LAST-SERVICE-PRIOR-X.                                    
      *       10 EMP-DATE-LAST-SERVICE-PRIOR      PIC S9(07)     COMP-3.        
      *    05 EMP-INITIAL-CLAIM-DT-X.                                           
      *       10 EMP-INITIAL-CLAIM-DT             PIC S9(07)     COMP-3.        
      ***                            CHGLOG END   - 07 - 020930 - CWC  *        
           05 EMP-DATE-ADDED-X.                                                 
              10 EMP-DATE-ADDED                   PIC S9(07)     COMP-3.        
           05 EMP-LANGUAGE-CD                     PIC  X(01).                   
           05 EMP-LOCATION-CD                     PIC  9(02).                   
           05 EMP-GCS-DRAFT-CD-YR                 PIC  X(02).                   
           05 EMP-ADJUSTMENTS.                                                  
              10 EMP-ADJ-OWED-TO-US-AMT-X.                                      
                 15 EMP-ADJ-OWED-TO-US-AMT        PIC S9(07)V99  COMP-3.        
      ***                            CHGLOG START - 05 - 950714 - MC   *        
      ***  05 FILLER                              PIC  X(01).                   
           05 EMP-DIRECT-MAIL-IND                 PIC  X(01).                   
              88 EMP-DIRECT-MAIL-VALID      VALUE 'D' 'H' 'B' SPACE.            
              88 EMP-DIRECT-MAIL-NOT-ENTRY  VALUE ' '.                          
              88 EMP-DIRECT-MAIL-DENTAL     VALUE 'D'.                          
              88 EMP-DIRECT-MAIL-HEALTH     VALUE 'H'.                          
              88 EMP-DIRECT-MAIL-BOTH       VALUE 'B'.                          
      ***                            CHGLOG END   - 05 - 950714 - MC   *        
           05 EMP-HIGHEST-CONDITION-NUM-X.                                      
              10 EMP-HIGHEST-CONDITION-NUM        PIC S9(03)     COMP-3.        
           05 EMP-MANUAL-PROCESS-IND              PIC  X(01).                   
              88 CERTIFICATE                      VALUE 'C'.                    
              88 INSURED                          VALUE 'I'.                    
              88 VALID-VALUE                      VALUE 'C' 'I' SPACE.          
           05 EMP-UPDATE-IND                      PIC  X(01).                   
              88 VALID-VALUE                      VALUE 'M' SPACE.              
           05 EMP-PDT-IND                         PIC  X(01).                   
              88 VALID-VALUE                      VALUE 'Y' SPACE.              
           05 EMP-DECEASED-DATE-X.                                              
              10 EMP-DECEASED-DATE                PIC S9(07) COMP-3.            
                 88 EMP-DECEASED                  VALUE +0000001 THRU           
                                                        +9999999.               
