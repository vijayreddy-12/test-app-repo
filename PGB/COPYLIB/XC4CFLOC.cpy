           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG          X C 4 C F L O C                           *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  01 - 930401 - SC    LASER CHEQUES - ADD MAIL HANDLING FIELDS  *        
      *                      COPY PROCESSING FIELDS AND SPACE FOR      *        
      *                      FUTURE REQUIREMENTS.  MOVE FLOC-CHEQUE-   *        
      *                      TYPE AND MAILING-CODE TO THE END          *        
      *                      FOR MAIL INST/COPY INST/LIST FREQ -       *        
      *                      ELEMENT 1 = WI, 2 = HOSP, 3=DENT/SHE      *        
      *                      (EXCL HOSP)                               *        
      *                                                                *        
      *  02 - 940427 - VB    SR 912 - ADD MEM SUPP FORM AND EXTRA SPACE*        
      *                                                                *        
      *  03 - 950803 - PST   3 CHAR DIV/CLASS                          *        
      *                                                                *        
      *  00 - YYMMDD - AAA   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *        
      *                                                                *        
      ******************************************************************        
      *** CHGLOG START - 00 - YYMMDD - AAA *****************************        
      *** CHGLOG END   - 00 - YYMMDD - AAA *****************************        
           SKIP1                                                                
      * 01 FLOC-LOCATION-RECORD.                                                
           05 FLOC-KEY.                                                         
              10 FLOC-CONTRACT                     PIC S9(7) COMP-3.            
              10 FLOC-DIVISION                     PIC X(3).                    
              10 FLOC-LOCATION                     PIC S9(3) COMP-3.            
           05 FLOC-DATA.                                                        
              10 FLOC-TERMINATION-DATE             PIC S9(7) COMP-3.            
              10 FLOC-MAILING-NAME-GROUP.                                       
                 15 FLOC-MAILING-NAME-HOR-GROUP    OCCURS 2 INDEXED             
                    FLOC-MAILING-NAME-HOR-INDEX.                                
                    20 FLOC-MAILING-NAME           PIC X(30).                   
              10 FLOC-MAILING-ADDRESS-GROUP.                                    
                 15 FLOC-MAILING-ADDRESS-HOR-GROUP OCCURS 4 INDEXED             
                    FLOC-MAILING-ADDRESS-HOR-INDEX.                             
                    20 FLOC-MAILING-ADDRESS        PIC X(30).                   
              10 FLOC-CLAIM-CONTACT                PIC X(30).                   
              10 FLOC-PHONE-GROUP.                                              
                 15 FLOC-AREA-CODE                 PIC S9(3) COMP-3.            
                 15 FLOC-PHONE-NUMBER              PIC S9(7) COMP-3.            
      *----------------------------------------------------------------*        
      * PHONE NUMBER                                                   *        
      * SEE INCONTROL SCREEN TABLE PHONENO                             *        
      *----------------------------------------------------------------*        
                 15 FLOC-EXTENSION                 PIC S9(5) COMP-3.            
      *** CHGLOG START - 01 - 930401 - SC  *****************************        
              10 FLOC-SUMMARY-LIST-BY            PIC X(1).                      
      *----------------------------------------------------------------*        
      * SUMMARY LISTING BY                                             *        
      * SEE INCONTROL SCREEN TABLE SUMFREQ                             *        
      *----------------------------------------------------------------*        
                 88 NO-SUMMARY-BREAKDOWN           VALUE SPACE.                 
                 88 SUMMARY-BY-CONTRACT            VALUE 'C'.                   
                 88 SUMMARY-BY-MAIL-DIV            VALUE 'M'.                   
                 88 SUMMARY-BY-ANAL-DIV            VALUE 'A'.                   
                 88 SUMMARY-BY-LOC                 VALUE 'L'.                   
              10 FLOC-SUMMARY-FREQ-GROUP.                                       
                 15 FLOC-SUMMARY-FREQ-HOR-GROUP   OCCURS 3 INDEXED              
                    FLOC-SUMMARY-FREQ-HOR-INDEX.                                
                    20 FLOC-SUMMARY-FREQUENCY      PIC X(1).                    
      *----------------------------------------------------------------*        
      * SUMMARY FREQUENCY                                              *        
      * SEE INCONTROL SCREEN TABLE SUMFREQ                             *        
      *----------------------------------------------------------------*        
                 88 NO-SUMMARY-FREQUENCY           VALUE SPACE.                 
                 88 DAILY-SUMMARY                  VALUE 'D'.                   
                 88 WEEKLY-SUMMARY                 VALUE 'W'.                   
                 88 BI-WEEKLY-SUMMARY              VALUE 'B'.                   
                 88 MONTHLY-SUMMARY                VALUE 'M'.                   
              10 FLOC-LETTER-COPY-GROUP.                                        
                 15 FLOC-LETTER-COPY-HOR-GROUP     OCCURS 3 INDEXED             
                    FLOC-LETTER-COPY-HOR-INDEX.                                 
                    20 FLOC-LETTER-COPY-INSTRUCT   PIC X(1).                    
      *----------------------------------------------------------------*        
      * LETTER COPY   MAILING INSTRUCTIONS                             *        
      * SEE INCONTROL SCREEN TABLE LTRCOPY                             *        
      *----------------------------------------------------------------*        
                 88 NO-LTR-COPY-INSTRUCTIONS       VALUE SPACE.                 
                 88 NO-COPY-REQUIRED               VALUE 'N'.                   
                 88 USE-LETTER-INSTRUCTION         VALUE 'R'.                   
                 88 MAIL-ASSIGNED-TO-HOLDER        VALUE 'U'.                   
                 88 COURIER-ASSIGNED-TO-HOLDER     VALUE 'V'.                   
                 88 MAIL-COPY-TO-HOLDER            VALUE 'W'.                   
                 88 COURIER-COPY-TO-HOLDER         VALUE 'X'.                   
              10 FLOC-CHEQUE-COPY-GROUP.                                        
                 15 FLOC-CHEQUE-COPY-HOR-GROUP     OCCURS 3 INDEXED             
                    FLOC-CHEQUE-COPY-HOR-INDEX.                                 
                    20 FLOC-CHEQUE-COPY-INSTRUCT   PIC X(1).                    
      *----------------------------------------------------------------*        
      * CHEQUE-COPY   MAILING INSTRUCTIONS                             *        
      * SEE INCONTROL SCREEN TABLE CHQCOPY                             *        
      *----------------------------------------------------------------*        
                 88 NO-CHEQ-COPY-INSTRUCTIONS      VALUE SPACE.                 
                 88 NO-COPY-REQUIRED               VALUE 'N'.                   
                 88 USE-CHEQUE-INSTRUCTION         VALUE 'R'.                   
                 88 MAIL-ASSIGNED-TO-HOLDER        VALUE 'U'.                   
                 88 COURIER-ASSIGNED-TO-HOLDER     VALUE 'V'.                   
                 88 MAIL-COPY-TO-HOLDER            VALUE 'W'.                   
                 88 COURIER-COPY-TO-HOLDER         VALUE 'X'.                   
                 88 MAIL-SUMM-TO-HOLDER            VALUE 'Y'.                   
                 88 COURIER-SUMM-TO-HOLDER         VALUE 'Z'.                   
              10 FLOC-CHEQUE-LETTER-GROUP.                                      
                 15 FLOC-CHEQUE-LETTER-HOR-GROUP   OCCURS 3 INDEXED             
                    FLOC-CHEQUE-LETTER-HOR-INDEX.                               
                    20 FLOC-CHEQUE-LETTER-INSTRUCT PIC X(1).                    
      *----------------------------------------------------------------*        
      * CHEQUE/LETTER MAILING INSTRUCTIONS                             *        
      * SEE INCONTROL SCREEN TABLE CHEQINST                            *        
      *----------------------------------------------------------------*        
                 88 NO-MAILING-INSTRUCTION         VALUE SPACE.                 
                 88 MAIL-TO-INSURED                VALUE 'A'.                   
                 88 MAIL-TO-HOLDER-FLAT            VALUE 'B'.                   
                 88 MAIL-TO-HOLDER-ENV             VALUE 'C'.                   
                 88 COURIER-FROM-OFF-FLAT          VALUE 'D'.                   
                 88 COURIER-FROM-OFF-ENV           VALUE 'E'.                   
                 88 RETURN-TO-OFFICE               VALUE 'F'.                   
                 88 DOLAN-CLAIMS                   VALUE 'G'.                   
              10 FLOC-DENT-CLAIM-FORM-ID           PIC X(8).                    
      *----------------------------------------------------------------*        
      * CLAIM FORM CODE                                                *        
      *----------------------------------------------------------------*        
                 88 NO-DENT-CLAIM-FORM-TO-SEND     VALUE SPACE.                 
              10 FLOC-SHE-CLAIM-FORM-ID            PIC X(8).                    
      *----------------------------------------------------------------*        
      * CLAIM FORM CODE                                                *        
      *----------------------------------------------------------------*        
                 88 NO-SHE-CLAIM-FORM-TO-SEND      VALUE SPACE.                 
              10 FLOC-SR-FORM-ID                   PIC X(8).                    
      *----------------------------------------------------------------*        
      * CLAIM FORM CODE                                                *        
      * SEE INCONTROL SCREEN TABLE FORMCD                              *        
      *----------------------------------------------------------------*        
                 88 NO-SR-FORM-TO-SEND             VALUE SPACE.                 
      *** CHGLOG START - 02 - 940427 - VB  *****************************        
              10 FLOC-MEM-FORM-ID                  PIC X(8).                    
      *----------------------------------------------------------------*        
      * MEMBER'S SUPPLEMENTARY                                         *        
      *----------------------------------------------------------------*        
                 88 NO-MEM-FORM-TO-SEND            VALUE SPACE.                 
      *** CHGLOG END   - 02 - 940427 - VB  *****************************        
              10 FLOC-LOC-PRINT-OFF-GROUP.                                      
      *----------------------------------------------------------------*        
      * LOCAL OFFICE PRINT REQUIRED                                    *        
      * SEE INCONTROL SCREEN TABLE OFFCODE                             *        
      *----------------------------------------------------------------*        
                 15 FLOC-LOC-PRINT-OFF-HOR-GROUP   OCCURS 8 INDEXED             
                    FLOC-LOC-PRINT-OFF-HOR-INDEX.                               
                    20 FLOC-LOCAL-PRINT-OFFICE     PIC X(1).                    
      *** CHGLOG START - 02 - 940427 - VB  *****************************        
      ***     10 FILLER                            PIC X(5).                    
              10 FILLER                            PIC X(17).                   
      *** CHGLOG END   - 02 - 940427 - VB  *****************************        
              10 FLOC-CHEQUE-TYPE                  PIC X(1).                    
      *----------------------------------------------------------------*        
      * CHEQUE TYPE                                                    *        
      * SEE INCONTROL SCREEN TABLE CHEQTYP                             *        
      *----------------------------------------------------------------*        
                 88 NOT-APPLICABLE                 VALUE SPACE.                 
                 88 BELL-CANADA                    VALUE 'C'.                   
                 88 ABITIBI-PRICE                  VALUE 'D'.                   
                 88 BANQUE-NATIONALE-CANADIENNE    VALUE 'E'.                   
                 88 GENERIC                        VALUE 'Y'.                   
                 88 REGULAR                        VALUE '1'.                   
              10 FLOC-MAILING-CODE                 PIC X(1).                    
      *----------------------------------------------------------------*        
      * MAILING CODE                                                   *        
      * SEE INCONTROL SCREEN TABLE MAILCD                              *        
      *----------------------------------------------------------------*        
                 88 NOT-APPLICABLE                 VALUE SPACE.                 
                 88 TO-INSURED                     VALUE '1'.                   
                 88 TO-INSURED-OR-ASSIGNEE         VALUE '2'.                   
                 88 TO-COMPANY                     VALUE '3'.                   
                 88 TO-COMPANY-OR-ASSIGNEE         VALUE '4'.                   
      *** CHGLOG END   - 01 - 930401 - SC  *****************************        
