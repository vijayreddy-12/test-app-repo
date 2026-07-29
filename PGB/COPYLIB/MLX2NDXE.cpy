      *=================================================================        
      *= COPYBOOK MLX2NDXE                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 INDEX ENTRY                         =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS THE FIELDS FOR A SINGLE INDEX ENTRY   =*        
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
      *#                SEPT2002          TRUDY BECKBERGER            #*        
      *#              - PERFORMANCE CHANGES                           #*        
      *#                  ADDED ADDITIONAL GROUP ITEMS                #*        
      *#                                                              #*        
      *#################################################################        
                                                                                
      *01  MLX2NDXE.                                                            
           05  NDXE-KEY.                                                        
               10  NDXE-KEY-SUBSET.                                             
                   15  NDXE-SECTION-ID        PIC X(4).                         
                   15  NDXE-SECTION-OCCUR     PIC S9(4) COMP.                   
                   15  NDXE-LVLS.                                               
                       20  NDXE-LVL-1-ID      PIC X(4).                         
                       20  NDXE-LVL-1-OCCUR   PIC S9(4) COMP.                   
                       20  NDXE-LVL-2-ID      PIC X(4).                         
                       20  NDXE-LVL-2-OCCUR   PIC S9(4) COMP.                   
                       20  NDXE-LVL-3-ID      PIC X(4).                         
                       20  NDXE-LVL-3-X       PIC X(1).                         
                       20  NDXE-LVL-3-OCCUR   PIC S9(4) COMP.                   
               10  NDXE-PACKET-ID             PIC X(4).                         
           05  NDXE-PACKET-OFFSET             PIC S9(8) COMP.                   
           05  NDXE-SOURCE-AREA               PIC X(1).                         
               88  NDXE-AREA1           VALUE '1'.                              
               88  NDXE-AREA2           VALUE '2'.                              
               88  NDXE-WORK            VALUE 'W'.                              
