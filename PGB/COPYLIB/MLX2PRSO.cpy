      *=================================================================        
      *= COPYBOOK MLX2PRSO                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER RETURN FIELDS                                 =*        
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
                                                                                
      *01  MLX2PRSO.                                                            
           05  PRSO-ERROR-IND                         PIC 9(2).                 
               88  PRSO-CRITICAL-ERROR              VALUE 08.                   
               88  PRSO-NON-CRITICAL-ERROR          VALUE 04.                   
               88  PRSO-NOT-FOUND                   VALUE 02.                   
               88  PRSO-NO-ERROR                    VALUE 00.                   
           05  PRSO-ERROR-COUNT                       PIC 9(2).                 
           05  PRSO-ERROR-TABLE.                                                
               10  PRSO-ERROR-ENTRY OCCURS 10.                                  
                   15  PRSO-LVL1-ID                   PIC X(4).                 
                   15  PRSO-LVL1-OCCUR                PIC 9(4).                 
                   15  PRSO-LVL2-ID                   PIC X(4).                 
                   15  PRSO-LVL2-OCCUR                PIC 9(4).                 
                   15  PRSO-LVL3-ID                   PIC X(4).                 
                   15  PRSO-LVL3-OCCUR                PIC 9(4).                 
                   15  PRSO-ERROR-PACKET              PIC X(4).                 
                   15  PRSO-ERROR-CODE                PIC 9(2).                 
                   15  PRSO-V1-MESSAGE                PIC X(80).                
                                                                                
           05  PRSO-DATA-LENGTH                       PIC S9(8) COMP.           
           05  PRSO-MESSAGE-VERSION                   PIC 9(4)V99.              
           05  PRSO-INDEX-HANDLE                      PIC S9(15) COMP-3.        
                                                                                
