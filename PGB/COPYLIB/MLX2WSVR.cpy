      *=================================================================        
      *= COPYBOOK MLX2WSVR                                            =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 WORKING STORAGE VARIABLES           =*        
      *=                                                              =*        
      *=================================================================        
      *#################################################################        
      *#                   MAINTENANCE LOG                            #*        
      *#                   ===============                            #*        
      *#  *PROJECT*     DATE (DD/MM/YY)    INITIALS                   #*        
      *#  ( BUG # )   - DESCRIPTION                                   #*        
      *#                                                              #*        
      *#                SEPT2002          TRUDY BECKBERGER            #*        
      *#              - CREATED                                       #*        
      *#                                                              #*        
      *#################################################################        
                                                                                
      *01  MLX2WSVR.                                                            
           05  WS-HOLD-PACKET-TYPE.                                             
               10  WS-HOLD-PACKET                 PIC X(4).                     
                   88  P-SECTION-START      VALUE '9991' THRU '9993'.           
                   88  P-SECTION-END        VALUE '9995' THRU '9997'.           
                   88  P-MESSAGE-END        VALUE '9998'.                       
                   88  P-GROUP-END          VALUE '9999'.                       
      *   *** IF 9994 IS EVER USED, NEED TO REVIEW THIS VARIABLE ***            
                   88  P-SPECIAL-VALUE      VALUE '9991' THRU '9999'.           
                   88  P-SPECIAL-VALUE-END  VALUE '9995' THRU '9999'.           
               10  WS-HOLD-TYPE                   PIC 9(2).                     
                   88  T-GROUP-START        VALUE 20.                           
                   88  T-SEGMENT            VALUE 21 22.                        
                   88  T-SPECIAL-VALUE      VALUE 20 THRU 22.                   
           05  WS-PACKET-TYPE                 PIC X(4).                         
               88  FIELD-LEVEL              VALUE 'FLD '.                       
               88  GROUP-START              VALUE '20  '.                       
               88  GROUP-END                VALUE '9999'.                       
               88  MESSAGE-END              VALUE '9998'.                       
               88  PREDEF-SEGMENT           VALUE '21  ' '22  '.                
               88  SECTION-START            VALUE '9991' '9992'.                
               88  SECTION-END              VALUE '9995' THRU '9997'.           
