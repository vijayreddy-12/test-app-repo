      ******************************************************************        
      * COBOL DECLARATION FOR TABLE TCUST                              *        
      ******************************************************************        
       01  DCLTCUST.                                                            
           10 CUST-ID              PIC S9(11)V USAGE COMP-3.                    
           10 ENCRYP-PSWRD         PIC X(22).                                   
           10 PSWRD-EXP-DT         PIC X(10).                                   
           10 PSWRD-REMIND         PIC X(50).                                   
           10 CUST-STAT-CD         PIC X(1).                                    
           10 CUST-STAT-REAS-CD    PIC X(1).                                    
           10 CUST-FIRST-NAME      PIC X(30).                                   
           10 FIRST-NAME-CAPS      PIC X(30).                                   
           10 CUST-INIT            PIC X(1).                                    
           10 CUST-LAST-NAME       PIC X(30).                                   
           10 LAST-NAME-CAPS       PIC X(30).                                   
           10 CUST-ADDR-1          PIC X(60).                                   
           10 CUST-ADDR-2          PIC X(60).                                   
           10 CUST-ADDR-CITY       PIC X(60).                                   
           10 CUST-ADDR-PROV       PIC X(30).                                   
           10 CUST-ADDR-COUNTRY    PIC X(30).                                   
           10 CUST-ADDR-POST-CD    PIC X(9).                                    
           10 EMAIL-ADDR           PIC X(60).                                   
           10 CUST-DOB             PIC X(10).                                   
           10 CUST-GENDER-CD       PIC X(1).                                    
           10 LANG-CD              PIC X(1).                                    
           10 SPONSOR-NAME         PIC X(60).                                   
           10 CHN-USER-ID          PIC X(20).                                   
           10 CHN-TS               PIC X(26).                                   
           10 ROLE                 PIC X(1).                                    
           10 REG-CNFRM-LTR        PIC X(1).                                    
           10 BUS-PHONE-NUM        PIC X(10).                                   
           10 BUS-PHONE-EXT        PIC X(06).                                   
           10 REG-GROUP-OFFICE     PIC X(05).                                   
           10 ACTVTN-KEY           PIC X(6).                                    
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *        
      ******************************************************************        
