      * DATE    PGMR    DESCRIPTION                                             
      ******************************************************************        
ESI   *961203  MELANSON  ADDED NEW INTERFACES                                   
ESI   *        CURRIE    ADDED NEW INTERFACES                                   
ESI   *970122  MELANSON  FAKE MAPPING PARM IS NOW AT MAP LEVEL                  
      *        HUNTEMI   ADDED NEW INTERFACE                                    
      *-------------------------------------------------                        
      *01  WS-INCONTRD-AREA.                                                    
           03 IC-CARD.                                                          
              05 IC-CARD-AREA.                                                  
                 10 IC-RUN-DATE.                                                
                    15 IC-RUN-DATE-CC               PIC 9(2).                   
                    15 IC-RUN-DATE-YY               PIC 9(2).                   
                    15 IC-RUN-DATE-MM               PIC 9(2).                   
                    15 IC-RUN-DATE-DD               PIC 9(2).                   
                 10 FILLER                          PIC X(1).                   
                 10 IC-INTERFACE-PATH.                                          
                    15 IC-INTERFACE-AREA            PIC X(02).                  
                       88 IC-IA-CLIENT-CONTRACT   VALUE 'CC'.                   
                       88 IC-IA-ELIGIBILITY       VALUE 'EL'.                   
                       88 IC-IA-CLAIMS-HISTORY    VALUE 'CH'.                   
                    15 IC-INTERFACE-SOURCE          PIC X(03).                  
                    15 IC-INTERFACE-TARGET          PIC X(03).                  
                 10 IC-INTERFACE-AREA-ID REDEFINES IC-INTERFACE-PATH.           
                    15 IC-INTERFACE-AREA-FILLER     PIC X(08).                  
                       88 IC-CC-COV-EXT         VALUE 'CCCOVEXT'.               
ESI                    88 IC-CC-CL2-EXT         VALUE 'CCCL2EXT'.               
                       88 IC-CC-EXT-CL2         VALUE 'CCEXTCL2'.               
                       88 IC-CC-EXT-DMS         VALUE 'CCEXTDMS'.               
                       88 IC-CC-EXT-DML         VALUE 'CCEXTDML'.               
ESI                    88 IC-CC-EXT-ESI         VALUE 'CCEXTESI'.               
                       88 IC-CC-EXT-GIS         VALUE 'CCEXTGIS'.               
                       88 IC-CC-EXT-LCS         VALUE 'CCEXTLCS'.               
                       88 IC-CC-EXT-FTK         VALUE 'CCEXTFTK'.               
                       88 IC-EL-GIP-GEN         VALUE 'ELGIPGEN'.               
ESI                    88 IC-EL-CL2-GEN         VALUE 'ELCL2GEN'.               
                       88 IC-EL-GEN-CL2         VALUE 'ELGENCL2'.               
                       88 IC-EL-GEN-DMS         VALUE 'ELGENDMS'.               
ESI                    88 IC-EL-GEN-ESI         VALUE 'ELGENESI'.               
                       88 IC-EL-GEN-EXT         VALUE 'ELGENEXT'.               
                       88 IC-CH-CL2-CHD         VALUE 'CHCL2CHD'.               
                       88 IC-CH-DMS-CHD         VALUE 'CHDMSCHD'.               
                       88 IC-CH-LCS-CHD         VALUE 'CHLCSCHD'.               
                       88 IC-CH-CL2-PAS         VALUE 'CHCL2PAS'.               
                       88 IC-CH-DMS-PAS         VALUE 'CHDMSPAS'.               
      *                88 IC-CH-DMS-GAS         VALUE 'CHDMSGAS'.               
                       88 IC-CH-DMS-CLI         VALUE 'CHDMSCLI'.               
                       88 IC-CH-DMS-ACT         VALUE 'CHDMSACT'.               
                       88 IC-CH-DMS-GFM-TW      VALUE 'CHDMSGTW'.               
                       88 IC-CH-DMS-EXP         VALUE 'CHDMSEXP'.               
                 10 FILLER                          PIC X(1).                   
                 10 IC-INTERFACE-TEST               PIC X(1).                   
ESI                 88 IC-TEST-RUN           VALUE 'T' 'D' 'N'.                 
ESI                 88 IC-DISPLAY-RUN        VALUE 'D' 'N'.                     
                    88 IC-NOISEY-RUN         VALUE 'N'.                         
ESI   *             88 IC-FAKE-RUN           VALUE 'F'.                         
                 10 FILLER                          PIC X(1).                   
                 10 IC-INTERFACE-SPECIAL-RUN        PIC X(1).                   
                    88 IC-RESTART            VALUE 'R'.                         
                    88 IC-ONE-GROUP          VALUE 'O'.                         
                    88 IC-BYPASS-GROUP       VALUE 'B'.                         
                 10 FILLER                          PIC X(1).                   
                 10 IC-INTERFACE-GROUP              PIC X(7).                   
                 10 FILLER                          PIC X(51).                  
