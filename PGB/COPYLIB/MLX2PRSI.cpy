      *=================================================================        
      *= COPYBOOK MLX2PRSI                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= MESSAGE PARSER CALLING MODULE INPUT AREA                     =*        
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
      *# PARSER V2      SEP/1998          JIM KLAPWYK                 #*        
      *#              - CREATED                                       #*        
      *#                                                              #*        
      *#################################################################        
                                                                                
      *01  MLX2PRSI.                                                            
           05  PRSI-STRUCTURE-NAME1              PIC X(8).                      
           05  PRSI-STRUCTURE-NAME2              PIC X(8).                      
           05  PRSI-SECTION                      PIC X(1).                      
               88  PRSI-SECTION-HEADER                VALUE 'H'.                
               88  PRSI-SECTION-BODY                  VALUE 'B'.                
               88  PRSI-SECTION-TRAILER               VALUE 'T'.                
               88  PRSI-SECTION-ALL                   VALUE ' '.                
           05  PRSI-SECTION-OCCUR                PIC S9(4) COMP.                
           05  PRSI-GRP-LVL-1-ID                 PIC X(4).                      
           05  PRSI-GRP-LVL-1-OCCUR              PIC S9(4) COMP.                
           05  PRSI-GRP-LVL-2-ID                 PIC X(4).                      
           05  PRSI-GRP-LVL-2-OCCUR              PIC S9(4) COMP.                
           05  PRSI-GRP-LVL-3-ID                 PIC X(4).                      
           05  PRSI-GRP-LVL-3-OCCUR              PIC S9(4) COMP.                
           05  PRSI-PACKET-ID                    PIC X(4).                      
           05  PRSI-PACKET-TYPE                  PIC X(2).                      
               88  TYPE-SHORT-INT                     VALUE '01'.               
               88  TYPE-LONG-INT                      VALUE '02'.               
               88  TYPE-2-DECIMAL                     VALUE '03'.               
               88  TYPE-1-DECIMAL                     VALUE '04'.               
               88  TYPE-3-DECIMAL                     VALUE '05'.               
               88  TYPE-4-DECIMAL                     VALUE '06'.               
               88  TYPE-5-DECIMAL                     VALUE '07'.               
               88  TYPE-6-DECIMAL                     VALUE '08'.               
               88  TYPE-ALPHA                         VALUE '09'.               
               88  TYPE-ALPHANUMERIC                  VALUE '10'.               
               88  TYPE-BOOLEAN                       VALUE '11'.               
               88  TYPE-YYYYMMDD                      VALUE '12'.               
               88  TYPE-YYYYMMDDHHMMSSHH              VALUE '13'.               
               88  TYPE-HHMMSSHH                      VALUE '14'.               
               88  TYPE-GROUP-START                   VALUE '20'.               
               88  TYPE-CLOB                          VALUE '21'.               
               88  TYPE-BLOB                          VALUE '22'.               
               88  TYPE-CYYMMDD-PACKED                VALUE '32'.               
               88  TYPE-SHORT-INT-COMP                VALUE '41'.               
               88  TYPE-LONG-INT-COMP                 VALUE '42'.               
               88  TYPE-SHORT-INT-PACKED              VALUE '51'.               
               88  TYPE-LONG-INT-PACKED               VALUE '52'.               
               88  TYPE-2-DECIMAL-PACKED              VALUE '53'.               
               88  TYPE-1-DECIMAL-PACKED              VALUE '54'.               
               88  TYPE-3-DECIMAL-PACKED              VALUE '55'.               
               88  TYPE-4-DECIMAL-PACKED              VALUE '56'.               
               88  TYPE-5-DECIMAL-PACKED              VALUE '57'.               
               88  TYPE-6-DECIMAL-PACKED              VALUE '58'.               
           05  PRSI-PACKET-LENGTH                PIC S9(4) COMP.                
           05  PRSI-SEGMENT-VERSION              PIC X(6).                      
           05  PRSI-CHAR-OPTIONS                 PIC X(1).                      
               88  PRSI-NO-OPTIONS                    VALUE 'N'.                
               88  PRSI-CONVERT-ACCENTS-AND-LOWER     VALUE 'A'.                
               88  PRSI-CONVERT-ACCENTS               VALUE 'B'.                
           05  PRSI-RUN-OPTION                   PIC X(1).                      
               88  PRSI-NORMAL                        VALUE '0'.                
               88  PRSI-SKIP-DEFAULT                  VALUE '1'.                
               88  PRSI-SKIP-VALUE                    VALUE '2'.                
           05  PRSI-OCCURS-OPTION                PIC X(1).                      
               88  PRSI-REMOVE-EMPTY-OCCURS           VALUE ' '.                
               88  PRSI-RETAIN-EMPTY-OCCURS           VALUE 'R'.                
           05  PRSI-INDEX-HANDLE-X.                                             
               10  PRSI-INDEX-HANDLE             PIC S9(15) COMP-3.             
           05  PRSI-MAX-LENGTH-AREA1             PIC S9(4) COMP.                
           05  PRSI-MAX-LENGTH-AREA2             PIC S9(4) COMP.                
           05  PRSI-MAX-LENGTH-AREA3             PIC S9(4) COMP.                
           05  FILLER                            PIC X(10).                     
