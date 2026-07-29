      *=================================================================        
      *= COPYBOOK MLX2PARS                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER VERSION 2 INPUT AREA                          =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS THE INPUT FIELDS SET UP BY THE CALLING=*        
      *= MODULE                                                       =*        
      *=                                                              =*        
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
                                                                                
      *01  MLX2PARS.                                                            
           05  PARS-ACTION                       PIC X(1).                      
               88  PARS-ACTION-GET                    VALUE 'G'.                
               88  PARS-ACTION-PUT                    VALUE 'P'.                
               88  PARS-ACTION-CLEAR                  VALUE 'C'.                
               88  PARS-ACTION-PARSE-TO-COPY          VALUE 'T'.                
               88  PARS-ACTION-PARSE-FROM-COPY        VALUE 'F'.                
           05  PARS-STRUCTURE-NAME               PIC X(8).                      
           05  PARS-STRUCTURE-NAME2              PIC X(8).                      
           05  PARS-SECTION                      PIC X(1).                      
               88  PARS-SECTION-HEADER                VALUE 'H'.                
               88  PARS-SECTION-BODY                  VALUE 'B'.                
               88  PARS-SECTION-TRAILER               VALUE 'T'.                
           05  PARS-SECTION-OCCUR                PIC S9(4) COMP.                
           05  PARS-GRP-LVL-1-ID                 PIC X(4).                      
           05  PARS-GRP-LVL-1-OCCUR              PIC S9(4) COMP.                
           05  PARS-GRP-LVL-2-ID                 PIC X(4).                      
           05  PARS-GRP-LVL-2-OCCUR              PIC S9(4) COMP.                
           05  PARS-GRP-LVL-3-ID                 PIC X(4).                      
           05  PARS-GRP-LVL-3-OCCUR              PIC S9(4) COMP.                
           05  PARS-PACKET-ID                    PIC X(4).                      
           05  PARS-PACKET-TYPE                  PIC X(2).                      
           05  PARS-PACKET-LENGTH                PIC S9(4) COMP.                
           05  PARS-SEGMENT-VERSION              PIC X(6).                      
           05  PARS-CHAR-OPTIONS                 PIC X.                         
               88  PARS-NO-OPTIONS             VALUE 'N'.                       
               88  PARS-NO-ACCENTS             VALUE 'A'.                       
               88  PARS-NO-LOWERCASE           VALUE 'B'.                       
           05  PARS-RUN-OPTION                   PIC X(1).                      
               88  PARS-NORMAL                 VALUE '0'.                       
               88  PARS-SKIP-DEFAULT           VALUE '1'.                       
           05  PARS-OCCURS-OPTION                PIC X(1).                      
               88  PARS-REMOVE-EMPTY-OCCURS    VALUE ' '.                       
               88  PARS-RETAIN-EMPTY-OCCURS    VALUE 'R'.                       
           05  PARS-INDEX-HANDLE-X.                                             
               10  PARS-INDEX-HANDLE             PIC S9(15) COMP-3.             
           05  PARS-MAX-LENGTH-AREA1             PIC S9(4) COMP.                
           05  PARS-MAX-LENGTH-AREA2             PIC S9(4) COMP.                
           05  PARS-MAX-LENGTH-AREA3             PIC S9(4) COMP.                
           05  PARS-MAX-LENGTH-AREA4             PIC S9(4) COMP.                
           05  FILLER                            PIC X(8).                      
