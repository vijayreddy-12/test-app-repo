      *=================================================================        
      *= COPYBOOK MLXXPCTL                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= TRANSACTION PARSER SUB-PROGRAM COMMUNICATIONS AREA.          =*        
      *=                                                              =*        
      *= THIS COPYBOOK CONTAINS "CONTROL" FIELDS THAT GET COMMUNICATED=*        
      *= AMONG THE VARIOUS PROCESSES (SUB-PROGRAMS) WITHIN THE        =*        
      *= PARSER                                                       =*        
      *=                                                              =*        
      *= COPYBOOK LENGTH: 80                                          =*        
      *=                                                              =*        
      *=================================================================        
      *#################################################################        
      *#                   MAINTENANCE LOG                            #*        
      *#                   ===============                            #*        
      *#  *PROJECT*     DATE (DD/MM/YY)    INITIALS                   #*        
      *#  ( BUG # )   - DESCRIPTION                                   #*        
      *#                                                              #*        
      *# PARSER         JUL/96            M. PRANGE                   #*        
      *# RE-WRITE     - CREATED                                       #*        
      *#                                                              #*        
      *# ELIGIBILITY    AUG/97            JIM KLAPWYK                 #*        
      *# CLONE        - RETRIEVE AND UPDATE AND DELETE                #*        
      *#                CAPABILITY ADDED                              #*        
      *#################################################################        
                                                                                
      *01  MLXXPCTL.                                                            
      *                                                                         
      *    BATCH CALLS: THE FOLLOWING FIELDS SHOULD BE INITIALIZED              
      *                 BY THE PROGRAM CALLING MLXXPARB                         
      *    CICS  CALLS: THE FOLLOWING FIELDS WILL BE SET UP IN MLXXPARC         
      *                                                                         
           05  PCTL-REQUEST                      PIC X(1).                      
               88  PCTL-REQ-FILE-OPENS                VALUE 'O'.                
               88  PCTL-REQ-PARSE-DATA                VALUE 'P'.                
               88  PCTL-REQ-RETRIEVE                  VALUE 'R'.                
               88  PCTL-REQ-UPDATE                    VALUE 'U'.                
               88  PCTL-REQ-DELETE                    VALUE 'D'.                
               88  PCTL-REQ-FILE-CLOSES               VALUE 'C'.                
           05  PCTL-SRC-STRUCT-TYPE              PIC X(1).                      
               88  PCTL-SRC-STR-TYPE-TAGGED           VALUE 'T'.                
               88  PCTL-SRC-STR-TYPE-COPYBOOK         VALUE 'C'.                
           05  PCTL-SRC-STRUCT-NAME              PIC X(8).                      
           05  PCTL-SRC-2-STRUCT-TYPE            PIC X(1).                      
               88  PCTL-SRC-2-STR-TYPE-NONE           VALUE 'N'.                
               88  PCTL-SRC-2-STR-TYPE-TAGGED         VALUE 'T'.                
               88  PCTL-SRC-2-STR-TYPE-COPYBOOK       VALUE 'C'.                
           05  PCTL-SRC-2-STRUCT-NAME            PIC X(8).                      
           05  PCTL-TGT-STRUCT-TYPE              PIC X(1).                      
               88  PCTL-TGT-STR-TYPE-TAGGED           VALUE 'T'.                
               88  PCTL-TGT-STR-TYPE-COPYBOOK         VALUE 'C'.                
           05  PCTL-TGT-STRUCT-NAME              PIC X(8).                      
           05  PCTL-DEBUG-OPTION                 PIC X(1).                      
               88  PCTL-DEBUG-ON                      VALUE 'Y'.                
               88  PCTL-DEBUG-OFF                     VALUE 'N'.                
           05  PCTL-RUN-OPTION                   PIC X(1).                      
               88  PCTL-RUN-NORMAL                    VALUE '0'.                
               88  PCTL-RUN-SKIP-DEFAULT-PASS         VALUE '1'.                
               88  PCTL-RUN-SKIP-VALUES-PASS          VALUE '2'.                
           05  FILLER                            PIC X(11).                     
      *                                                                         
      *    THE FOLLOWING FIELDS ARE SET WITHIN THE PARSER ITSELF.               
      *                                                                         
           05  PCTL-ENVIRONMENT                  PIC X(8).                      
               88  PCTL-ENV-CICS                      VALUE 'CICS'.             
               88  PCTL-ENV-BATCH                     VALUE 'BATCH'.            
           05  PCTL-IO-MODULE                    PIC X(8).                      
               88  PCTL-IO-MLXXPIOB                   VALUE 'MLXGPIOB'.         
               88  PCTL-IO-MLXXPIOC                   VALUE 'MLXGPIOC'.         
           05  PCTL-PARSING-PASS                 PIC X(1).                      
               88  PCTL-PARSING-FOR-DEFAULTS          VALUE 'D'.                
               88  PCTL-PARSING-FOR-VALUES            VALUE 'V'.                
           05  PCTL-PASS-NUMBER                  PIC 9(1).                      
           05  PCTL-TOTAL-PASSES-REQUIRED        PIC 9(1).                      
           05  PCTL-RETURN-CODE                  PIC X(2).                      
               88  PCTL-RET-OK                        VALUE '00'.               
               88  PCTL-RET-INVALID-REQUEST           VALUE '10'.               
               88  PCTL-RET-OPEN-FAILED               VALUE '20'.               
               88  PCTL-RET-PARSING-FAILED            VALUE '30'.               
               88  PCTL-RET-CLOSE-FAILED              VALUE '40'.               
           05  FILLER                            PIC X(18).                     
