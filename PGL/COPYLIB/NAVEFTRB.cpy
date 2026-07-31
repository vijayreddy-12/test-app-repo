      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG          R B C S T 1 5 2                           *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ----     ---   ----------------------------------------  *        
      *                                                                *        
      *  01   040628   TJ    CREATED THIS COPYBOOK BASED ON THE        *        
      *                      ROYAL BANK STANDARD (STD152) CREDIT FILE  *        
      *                      FORMAT SPECIFICATION.                     *        
      *                                                                *        
      *                      SOURCE: JENNIFER ZOSCHKE (6/23/2004)      *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
                                                                                
      * Royal Bank EFT Records (Header, Basic Payment, and Trailer)             
                                                                                
      * the first record is the header record                                   
       01 RBC-EFT-HEADER.                                                       
          10 RBC-EFTH-REC-COUNT                PIC 9(6).                        
          10 RBC-EFTH-REC-TYPE                 PIC X(1).                        
             88 EFTH-REC-TYPE          VALUE 'A'.                               
          10 RBC-EFTH-TRANS-CODE               PIC X(3).                        
             88 EFTH-TRANS-CODE        VALUE 'HDR'.                             
          10 RBC-EFTH-CLIENT-NUM               PIC X(10).                       
             88 EFTH-CLIENT-NUM        VALUE '9069520000'.                      
          10 RBC-EFTH-CLIENT-NAME              PIC X(30).                       
             88 EFTH-CLIENT-NAME       VALUE                                    
                                       'MANUFACTURERS LIFE INS. CO.   '.        
          10 RBC-EFTH-FILE-CREATION-NUM        PIC X(4).                        
          10 RBC-EFTH-FILE-CREATION-DATE       PIC 9(7).                        
          10 RBC-EFTH-CURRENCY-TYPE            PIC X(3).                        
             88 EFTH-CURRENCY-CANADA   VALUE 'CAD'.                             
             88 EFTH-CURRENCY-US       VALUE 'USD'.                             
          10 RBC-EFTH-INPUT-TYPE               PIC X(1).                        
             88 EFTH-INPUT-TYPE        VALUE '1'.                               
          10 RBC-EFTH-FILLER1                  PIC X(15).                       
          10 RBC-EFTH-RESERVED1                PIC X(6).                        
          10 RBC-EFTH-RESERVED2                PIC X(8).                        
          10 RBC-EFTH-RESERVED3                PIC X(9).                        
          10 RBC-EFTH-FILLER2                  PIC X(46).                       
          10 RBC-EFTH-FILLER3                  PIC X(2).                        
          10 RBC-EFTH-CLIENT-OPT-REC           PIC X(1).                        
             88 EFTH-CLIENT-OPT-YES    VALUE 'Y'.                               
             88 EFTH-CLIENT-OPT-NO     VALUE 'N'.                               
                                                                                
      * the basic payment records follow the header record                      
       01 RBC-EFT-DETAIL.                                                       
          10 RBC-EFTD-REC-COUNT                PIC 9(6).                        
          10 RBC-EFTD-REC-TYPE                 PIC X(1).                        
             88 EFTD-REC-TYPE          VALUE 'C'.                               
          10 RBC-EFTD-TRANS-CODE               PIC X(3).                        
             88 EFTD-TRANS-CODE        VALUE '450'.                             
          10 RBC-EFTD-CLIENT-NUM               PIC X(10).                       
             88 EFTD-CLIENT-NUM        VALUE '9069520000'.                      
          10 RBC-EFTD-FILLER1                  PIC X(1).                        
          10 RBC-EFTD-CUSTOMER-NUM             PIC X(19).                       
          10 RBC-EFTD-PAYMENT-NUM              PIC 9(2).                        
          10 RBC-EFTD-CAN-BANK-ID.                                              
             20 EFTD-INST-NUM                  PIC 9(4).                        
             20 EFTD-BRANCH-NUM                PIC 9(5).                        
          10 RBC-EFTD-USA-ROUTING-NUM REDEFINES                                 
             RBC-EFTD-CAN-BANK-ID              PIC 9(9).                        
          10 RBC-EFTD-ACCOUNT-NUM              PIC X(18).                       
          10 RBC-EFTD-FILLER2                  PIC X(1).                        
          10 RBC-EFTD-PAYMENT-AMT              PIC 9(8)V99.                     
          10 RBC-EFTD-SEQ-CTL                  PIC X(6).                        
          10 RBC-EFTD-PAYMENT-DATE             PIC 9(7).                        
          10 RBC-EFTD-CUSTOMER-NAME            PIC X(30).                       
          10 RBC-EFTD-LANGUAGE-CODE            PIC X(1).                        
             88 EFTD-LANGUAGE-CODE-ENG VALUE 'E'.                               
             88 EFTD-LANGUAGE-CODE-FRE VALUE 'F'.                               
          10 RBC-EFTD-RESERVED1                PIC X(1).                        
          10 RBC-EFTD-CLIENT-SHORT-NAME.                                        
             20 RBC-EFTD-FIXED-NAME            PIC X(9).                        
                88 EFTD-FIXED-NAME     VALUE 'MANULIFE '.                       
             20 RBC-EFTD-CLIENT-NUM-2          PIC X(6).                        
          10 RBC-EFTD-DEST-CURRENCY            PIC X(3).                        
             88 EFTD-DEST-CURRENCY-CAD VALUE 'CAD'.                             
             88 EFTD-DEST-CURRENCY-USD VALUE 'USD'.                             
          10 RBC-EFTD-RESERVED2                PIC X(1).                        
          10 RBC-EFTD-DEST-COUNTRY             PIC X(3).                        
             88 EFTD-DEST-COUNTRY-CAN  VALUE 'CAN'.                             
             88 EFTD-DEST-COUNTRY-USA  VALUE 'USA'.                             
          10 RBC-EFTD-FILLER3                  PIC X(2).                        
          10 RBC-EFTD-RESERVED3                PIC X(2).                        
          10 RBC-EFTD-CLIENT-OPT-REC           PIC X(1).                        
             88 EFTD-CLIENT-OPT-YES    VALUE 'Y'.                               
             88 EFTD-CLIENT-OPT-NO     VALUE 'N'.                               
                                                                                
      * the trailer record is the last record                                   
       01 RBC-EFT-TRAILER.                                                      
          10 RBC-EFTT-REC-COUNT                PIC 9(6).                        
          10 RBC-EFTT-REC-TYPE                 PIC X(1).                        
             88 EFTT-REC-TYPE          VALUE 'Z'.                               
          10 RBC-EFTT-TRANS-CODE               PIC X(3).                        
             88 EFTT-TRANS-CODE        VALUE 'TRL'.                             
          10 RBC-EFTT-CLIENT-NUM               PIC X(10).                       
             88 EFTT-CLIENT-NUM        VALUE '9069520000'.                      
          10 RBC-EFTT-TOT-PAYMENT-TRANS        PIC 9(6).                        
          10 RBC-EFTT-TOT-PAYMENT-AMT          PIC 9(12)V99.                    
          10 RBC-EFTT-RESERVED1                PIC 9(6).                        
          10 RBC-EFTT-RESERVED2                PIC 9(14).                       
          10 RBC-EFTT-RESERVED3                PIC 9(2).                        
          10 RBC-EFTT-TOT-OPT-RECS             PIC 9(6).                        
          10 RBC-EFTT-FILLER1                  PIC X(12).                       
          10 RBC-EFTT-SEQ-CTL                  PIC X(6).                        
          10 RBC-EFTT-FILLER2                  PIC X(63).                       
          10 RBC-EFTT-RESERVED4                PIC X(2).                        
          10 RBC-EFTT-FILLER3                  PIC X(1).                        
                                                                                
      ******************************************************************        
      *                      End of Copybook                           *        
      ******************************************************************        
