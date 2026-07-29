      ******************************************************************        
      *                                                                *        
      *            COPYLIB DESCRIPTION                                 *        
      *            *******************                                 *        
      *                                                                *        
      *  THIS COPYLIB CONTAINA ALL NECESSARY INFORMATION PASSED IN     *        
      *  THE MANULIFE MESSAGE HEADER, VERSION 2.00                     *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
      *01  MANULIFE-MESSAGE.                                                    
           05  MANULIFE-MSG-HEADER.                                             
               10 HEADER-VERSION-NUMBER               PIC 9(4)V99.              
               10 MESSAGE-TYPE-QUALIFIER              PIC X(8).                 
               10 TYPEID-MAJOR                        PIC X(4).                 
               10 TYPEID-MINOR                        PIC X(4).                 
               10 MESSAGE-TYPE-VERSION-NUMBER         PIC X(6).                 
               10 ENTRY-USERID                        PIC X(12).                
               10 ENTRY-USERROLE                      PIC X(2).                 
               10 ACCESS-CHANNEL                      PIC X(4).                 
               10 SECURITY-LEVEL                      PIC X(2).                 
               10 REPLY-LANGUAGE                      PIC X(3).                 
               10 ERROR-CODES.                                                  
                  15 ERROR-SYSTEM-CODE                PIC X(2).                 
                  15 ERROR-CODE                       PIC X(4).                 
               10 ORIGINAL-RECEIVED-DATE-TIME.                                  
                  15 ORIGINAL-RECEIVED-DATE           PIC X(8).                 
                  15 ORIGINAL-RECEIVED-TIME           PIC X(8).                 
               10 OFFSET-FROM-GMT.                                              
                  15 OFFSET-SIGN                      PIC X.                    
                  15 OFFSET-VALUE                     PIC X(4).                 
               10 ORIGINAL-REPLYTOQ                   PIC X(48).                
               10 ORIGINAL-REPLYTOQMGR                PIC X(48).                
                                                                                
