      *=================================================================        
      *= COPYBOOK MLX2TRNS                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 INPUT AREA                          =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS THE FIELDS USED BY THE TRANSLATE      =*        
      *= MODULE                                                       =*        
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
                                                                                
      *01  MLX2TRNS.                                                            
           05  TRNS-ACTION                       PIC X(1).                      
               88  TRNS-ACTION-PUT                 VALUE 'P'.                   
               88  TRNS-ACTION-GET                 VALUE 'G'.                   
           05  TRNS-OPTIONS                      PIC X(1).                      
               88  TRNS-NO-OPTIONS                 VALUE 'N'.                   
               88  TRNS-CONVERT-ACCENTS-AND-LOWER  VALUE 'A'.                   
               88  TRNS-CONVERT-ACCENTS            VALUE 'B'.                   
           05  TRNS-IN-TYPE                      PIC 9(2).                      
           05  TRNS-IN-LENGTH                    PIC S9(8) COMP.                
           05  TRNS-IN-SEGMENT-VERSION           PIC X(6).                      
           05  TRNS-OUT-TYPE                     PIC 9(2).                      
           05  TRNS-OUT-LENGTH                   PIC S9(8) COMP.                
           05  TRNS-OUT-SEGMENT-VERSION          PIC X(6).                      
           05  TRNS-WORK-AREA                    PIC X(999).                    
           05  TRNS-IND                          PIC X(1).                      
               88  TRNS-NO-CHG                     VALUE 'N'.                   
               88  TRNS-CHG                        VALUE 'Y'.                   
           05  TRNS-RETURN                       PIC 9(2).                      
