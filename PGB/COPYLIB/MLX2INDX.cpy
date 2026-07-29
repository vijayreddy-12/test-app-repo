      *=================================================================        
      *= COPYBOOK MLX2INDX                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 INDEX DEFINITION                    =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS THE FIELDS USED THROUGHOUT THE PARSER =*        
      *= FOR THE INDEX OF THE MESSAGE                                 =*        
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
                                                                                
      *01  MLX2INDX.                                                            
           05  INDX-PACKET-COUNT                  PIC S9(4) COMP.               
           05  INDX-PACKETS-X.                                                  
               10  INDX-PACKETS OCCURS 999                                      
                                DEPENDING ON INDX-PACKET-COUNT                  
                                ASCENDING KEY IS INDX-KEY                       
                                INDEXED BY INDX-XXX.                            
                   15  INDX-KEY.                                                
                       20  INDX-SECTION-ID            PIC X(4).                 
                       20  INDX-SECTION-OCCUR         PIC S9(4) COMP.           
                       20  INDX-GRP-LVL-1-ID          PIC X(4).                 
                       20  INDX-GRP-LVL-1-OCCUR       PIC S9(4) COMP.           
                       20  INDX-GRP-LVL-2-ID          PIC X(4).                 
                       20  INDX-GRP-LVL-2-OCCUR       PIC S9(4) COMP.           
                       20  INDX-GRP-LVL-3-ID          PIC X(4).                 
                       20  INDX-GRP-LVL-3-X           PIC X(1).                 
                       20  INDX-GRP-LVL-3-OCCUR       PIC S9(4) COMP.           
                       20  INDX-PACKET-ID             PIC X(4).                 
                   15  INDX-PACKET-OFFSET             PIC S9(8) COMP.           
                   15  INDX-SOURCE-AREA               PIC X(1).                 
                       88  INDX-AREA1               VALUE '1'.                  
                       88  INDX-AREA2               VALUE '2'.                  
                       88  INDX-WORK                VALUE 'W'.                  
