      *=================================================================        
      *= COPYBOOK MLX2COMM                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 COMMON FIELDS                       =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS THE FIELDS USED INTERNALLY THROUGHOUT =*        
      *= THE PARSER                                                   =*        
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
                                                                                
      *01  MLX2COMM.                                                            
           05  COMM-HOLD-INDEX-HANDLE-X.                                        
               10  COMM-HOLD-INDEX-HANDLE             PIC S9(15) COMP-3.        
           05  COMM-INDEX-IND                         PIC X(1).                 
               88  COMM-REBUILD-INDEX               VALUE 'R'.                  
               88  COMM-INDEX-BUILT                 VALUE 'B'.                  
           05  COMM-WORK-AREA                         PIC X(100).               
           05  FILLER                                 PIC X(10).                
