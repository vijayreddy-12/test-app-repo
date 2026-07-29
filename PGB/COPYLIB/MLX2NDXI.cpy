      *=================================================================        
      *= COPYBOOK MLX2NDXI                                            =*        
      *= ***** MATCHED TO COPYBOOK MLX2NDXE *****                     =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 INDEX ENTRY INITIALIZATION          =*        
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
                                                                                
      *01  MLX2NDXI.                                                            
           05  NDXE-KEY-INIT.                                                   
               10  FILLER VALUE SPACES        PIC X(4).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
               10  FILLER VALUE SPACES        PIC X(4).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
               10  FILLER VALUE SPACES        PIC X(4).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
               10  FILLER VALUE SPACES        PIC X(4).                         
               10  FILLER VALUE SPACES        PIC X(1).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
           05  NDXE-LVLS-LV-INIT.                                               
               10  FILLER VALUE LOW-VALUES    PIC X(4).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
               10  FILLER VALUE LOW-VALUES    PIC X(4).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
               10  FILLER VALUE LOW-VALUES    PIC X(4).                         
               10  FILLER VALUE LOW-VALUES    PIC X(1).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
           05  NDXE-LVLS-SP-INIT.                                               
               10  FILLER VALUE SPACES        PIC X(4).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
               10  FILLER VALUE SPACES        PIC X(4).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
               10  FILLER VALUE SPACES        PIC X(4).                         
               10  FILLER VALUE SPACES        PIC X(1).                         
               10  FILLER VALUE +0            PIC S9(4) COMP.                   
