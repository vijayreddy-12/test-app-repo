      *=================================================================        
      *= COPYBOOK MLX2OUTP                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 RETURN FIELDS                       =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS THE OUTPUT VALUES PASSED BACK TO THE  =*        
      *= CALLING APPLICATION                                          =*        
      *=                                                              =*        
      *=================================================================        
      *#################################################################        
      *#                   MAINTENANCE LOG                            #*        
      *#                   ===============                            #*        
      *#  *PROJECT*     DATE (DD/MM/YY)    INITIALS                   #*        
      *#  ( BUG # )   - DESCRIPTION                                   #*        
      *#                                                              #*        
      *# PARSER V2      SEP/1998          JIM KLAPWYK                 #*        
      *#              - CREATED                                       #*        
      *#                                                              #*        
      *#################################################################        
                                                                                
      *01  MLX2OUTP.                                                            
           05  OUTP-ERROR-IND                         PIC X(1).                 
               88  OUTP-CRITICAL-ERROR              VALUE '8'.                  
               88  OUTP-NON-CRITICAL-ERROR          VALUE '4'.                  
               88  OUTP-NOT-FOUND                   VALUE '2'.                  
               88  OUTP-NO-ERROR                    VALUE '0'.                  
           05  OUTP-TABLE-ENTRIES                     PIC S9(4) COMP.           
           05  OUTP-TABLE.                                                      
               10  OUTP-TABLE-ENTRY OCCURS 10.                                  
                   15  OUTP-LVL1-ID                   PIC X(4).                 
                   15  OUTP-LVL1-OCCUR                PIC S9(4) COMP.           
                   15  OUTP-LVL2-ID                   PIC X(4).                 
                   15  OUTP-LVL2-OCCUR                PIC S9(4) COMP.           
                   15  OUTP-LVL3-ID                   PIC X(4).                 
                   15  OUTP-LVL3-OCCUR                PIC S9(4) COMP.           
                   15  OUTP-ERROR-PACKET              PIC X(4).                 
                   15  OUTP-CODE                      PIC 9(2).                 
           05  OUTP-PACKET                            PIC X(4).                 
           05  OUTP-PACKET-TYPE                       PIC 9(2).                 
           05  OUTP-SEGMENT-VERSION                   PIC X(6).                 
           05  OUTP-TRAILER-OCCUR                     PIC S9(4) COMP.           
           05  OUTP-AREA3-DATA-LENGTH                 PIC S9(8) COMP.           
           05  OUTP-HANDLE-X.                                                   
               10  OUTP-HANDLE                        PIC S9(15) COMP-3.        
