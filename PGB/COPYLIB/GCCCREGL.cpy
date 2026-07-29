      ***********************************************************               
      * RECORD LAYOUT FOR INTERNET REGISTRATION (FRONT PAGE ONLY)               
      * BACK PAGE COPYBOOK GCCCREGB.                                            
      *        -- REGISTRATION LETTER AFP FORMAT                                
      ***********************************************************               
      *YYYY/MM/DD|YOUR NAME   |DESCRIPTION                      *               
      *2010/03/24|P PAIK      |MKI LETTER ENHANCEMENT           *               
      ***********************************************************               
           05 GCCCREGL-OUT-LETTER.                                              
              10  GCCCREGL-OUT-CC           PIC X.                              
              10  GCCCREGL-OUT-TYPE         PIC 9(3).                           
              10  GCCCREGL-OUT-GROUP        PIC 9(7).                           
              10  GCCCREGL-OUT-DIV          PIC X(3).                           
              10  GCCCREGL-OUT-CERT         PIC X(10).                          
              10  GCCCREGL-OUT-NAME         PIC X(85).                          
              10  GCCCREGL-OUT-ADDR1        PIC X(30).                          
              10  GCCCREGL-OUT-ADDR2        PIC X(30).                          
              10  GCCCREGL-OUT-ADDR3        PIC X(30).                          
              10  GCCCREGL-OUT-ADDR4        PIC X(30).                          
              10  GCCCREGL-OUT-CO-NAME1     PIC X(30).                          
              10  GCCCREGL-OUT-CO-NAME2     PIC X(30).                          
              10  GCCCREGL-OUT-CURR-DATE    PIC X(11).                          
              10  GCCCREGL-OUT-REGN-DATE    PIC X(11).                          
              10  GCCCREGL-ACTN-KEY         PIC X(6).                           
HNS           10  GCCCREGL-OUT-PHONE        PIC X(20).                          
HNS           10  GCCCREGL-OUT-URL          PIC X(100).                         
BC            10  GCCCREGL-SPONSOR-NAME     PIC X(50).                          
              10  GCCCREGL-ENROL-DATE       PIC X(56).                          
              10  FILLER                    PIC X(13).                          
                                                                                
           05 GCCCREGL-OUT-BANNER                                               
                  REDEFINES GCCCREGL-OUT-LETTER.                                
              10  GCCCREGL-OUT-BANNER-CC    PIC X(1).                           
              10  GCCCREGL-OUT-BANNER-TYPE  PIC 9(3).                           
              10  GCCCREGL-OUT-BANNER-TEXT                                      
                           OCCURS 4 TIMES   PIC X(70).                          
HNS           10  FILLER                    PIC X(216).                         
              10  FILLER                    PIC X(56).                          
                                                                                
