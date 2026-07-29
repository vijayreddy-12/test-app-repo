      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG:         X C 4 C F M M A                           *        
      * *************                                                  *        
      *                                                                *        
      *  NO    DATE    PGR                DESCRIPTION                  *        
      *  --   ------   ---  -----------------------------------------  *        
      *                                                                *        
      *  01 - 870707 - BJK  REDESIGN FOR CONFEDERATION LIFE            *        
      *                                                                *        
      *  02 - 960408 - CH   ADD EFT BANK ACCOUNT INFO                  *        
      *                                                                *        
      *  00 - YYMMDD - AAA  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA  ****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA  ****************************        
                                                                  SKIP1         
      * 01 MMA-MEM-ADDR-REC.                                                    
           05 MMA-MEMBER-KEY.                                                   
              10 MMA-FAMILY-NUM.                                                
                 15 MMA-FAMILY-NUMBER             PIC S9(09)     COMP-3.        
              10 MMA-FAMILY-MEMBER-ID             PIC  X(02).                   
                 88 VALID-VALUE                   VALUE '99'.                   
           05 MMA-EE-ADDRESS.                                                   
              10 MMA-EE-ADDRESS1                  PIC  X(30).                   
              10 MMA-EE-ADDRESS2                  PIC  X(30).                   
              10 MMA-EE-ADDRESS3                  PIC  X(30).                   
              10 MMA-EE-ADDRESS4                  PIC  X(30).                   
      *** CHGLOG START - 02 - 960408 - CH   ****************************        
           05 MMA-EFT-BANK-INFO.                                                
              10 MMA-EFT-BANK                     PIC  9(04).                   
              10 MMA-EFT-BRANCH                   PIC  9(05).                   
              10 MMA-EFT-ACCOUNT                  PIC  X(12).                   
      *** CHGLOG END   - 02 - 960408 - CH   ****************************        
