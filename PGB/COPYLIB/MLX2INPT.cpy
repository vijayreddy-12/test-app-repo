      *=================================================================        
      *= COPYBOOK MLX2INPT                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 INPUT AREA                          =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS THE INPUT FIELDS SET UP BY THE CALLING=*        
      *= MODULE                                                       =*        
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
                                                                                
      *01  MLX2INPT.                                                            
           05  INPT-ACTION                       PIC X(1).                      
               88  INPT-ACTION-GET                    VALUE 'G'.                
               88  INPT-ACTION-PUT                    VALUE 'P'.                
               88  INPT-ACTION-CLEAR                  VALUE 'C'.                
           05  INPT-SECTION                      PIC X(1).                      
               88  INPT-SECTION-HEADER                VALUE 'H'.                
               88  INPT-SECTION-BODY                  VALUE 'B'.                
               88  INPT-SECTION-TRAILER               VALUE 'T'.                
           05  INPT-SECTION-OCCUR                PIC S9(4) COMP.                
           05  INPT-GRP-LVL-1-ID                 PIC X(4).                      
           05  INPT-GRP-LVL-1-OCCUR              PIC S9(4) COMP.                
           05  INPT-GRP-LVL-2-ID                 PIC X(4).                      
           05  INPT-GRP-LVL-2-OCCUR              PIC S9(4) COMP.                
           05  INPT-GRP-LVL-3-ID                 PIC X(4).                      
           05  INPT-GRP-LVL-3-OCCUR              PIC S9(4) COMP.                
           05  INPT-PACKET-ID                    PIC X(4).                      
           05  INPT-PACKET-TYPE                  PIC X(2).                      
           05  INPT-PACKET-LENGTH                PIC S9(4) COMP.                
           05  INPT-SEGMENT-VERSION              PIC X(6).                      
           05  INPT-INDEX-HANDLE-X.                                             
               10  INPT-INDEX-HANDLE             PIC S9(15) COMP-3.             
           05  INPT-MAX-LENGTH-AREA1             PIC S9(4) COMP.                
           05  INPT-MAX-LENGTH-AREA2             PIC S9(4) COMP.                
           05  INPT-MAX-LENGTH-AREA3             PIC S9(4) COMP.                
           05  FILLER                            PIC X(8).                      
