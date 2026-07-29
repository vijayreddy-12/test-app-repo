      *=================================================================        
      *= COPYBOOK MLX2RTRN                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 RETURN FIELDS                       =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS THE RETURN VALUES PASSED BACK TO THE  =*        
      *= CALLING APPLICATION                                          =*        
      *=                                                              =*        
      *=                                                              =*        
      *=================================================================        
      *#################################################################        
      *#                   MAINTENANCE LOG                            #*        
      *#                   ===============                            #*        
      *#  *PROJECT*     DATE (DD/MM/YY)    INITIALS                   #*        
      *#  ( BUG # )   - DESCRIPTION                                   #*        
      *#                                                              #*        
      *# PARSER V2      AUG/1998          JIM KLAPWYK                 #*        
      *#              - CREATED                                       #*        
      *#                                                              #*        
      *#################################################################        
                                                                                
      *01  MLX2RTRN.                                                            
           05  RTRN-CODE                              PIC 9(2).                 
           05  RTRN-INDEX-HANDLE-X.                                             
               10  RTRN-INDEX-HANDLE                  PIC S9(15) COMP-3.        
           05  RTRN-PACKET-TYPE                       PIC 9(2).                 
           05  RTRN-SEGMENT-VERSION                   PIC X(6).                 
           05  RTRN-TRAILER-OCCUR                     PIC S9(4) COMP.           
           05  RTRN-AREA3-DATA-LENGTH                 PIC S9(8) COMP.           
           05  RTRN-WORK-AREA                         PIC X(16000).             
           05  FILLER                                 PIC X(10).                
