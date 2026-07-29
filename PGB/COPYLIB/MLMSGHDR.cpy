      *=================================================================        
      *= COPYBOOK MLMSGHDR                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MANULIFE MESSAGE HEADER                                      =*        
      *=                                                              =*        
      *= THE ELEMENTS DEFINED HERE ARE CONTAINED AT THE BEGINNING OF  =*        
      *= THE MESSAGE BODY FOR ANY MESSAGES PRODUCED BY MANULIFE       =*        
      *= APPLICATIONS.                                                =*        
      *=                                                              =*        
      *= LENGTH = 30 BYTES                                            =*        
      *=                                                              =*        
      *=================================================================        
      *#################################################################        
      *#                   MAINTENANCE LOG                            #*        
      *#                   ===============                            #*        
      *#  *PROJECT*     DATE (DD/MM/YY)    INITIALS                   #*        
      *#  ( BUG # )   - DESCRIPTION                                   #*        
      *#                                                              #*        
      *# ETS PROJECT    SEP 06, 1995      R. RANCE                    #*        
      *#              - CREATED                                       #*        
      *#                                                              #*        
      *#################################################################        
                                                                                
      *01  MLMSGHDR.                                                    00000140
           05  MSGHDR-MANULIFE-MSG-HEADER.                                000002
      ****         THIS IS VERSION 1.00                                         
               10  MSGHDR-VERSION                    PIC 9(4)V99.         000002
                                                                                
      ****         A CORPORATE WIDE TRANSACTION TYPE IDENTIFIER.                
      ****         IDENTIFIES A HIGH LEVEL GROUP OF TRANSACTIONS                
      ****         ALSO INDICATES THE LAYOUT OF THE APPLICATION RECORD          
      ****         AND THE APPLICATION THAT ORIGINATED THE MESSAGE.             
               10  MSGHDR-TXN-TYPE                   PIC X(08).           000002
                   88  MSGHDR-ETS                    VALUE 'ETS     '.          
                                                                                
      ****         IDENTIFIES THE TYPE OF TRANSACTION                           
               10  MSGHDR-TXN-SUB-TYPE.                                         
                   15  MSGHDR-TXN-SUB-TYPE-MAJOR     PIC X(04).                 
                   15  MSGHDR-TXN-SUB-TYPE-MINOR     PIC X(04).                 
                                                                                
               10  FILLER                            PIC X(08).           000002
                                                                                
      ****         END OF COPYBOOK                                              
                                                                                
