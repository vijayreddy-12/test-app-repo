      *=================================================================        
      *= COPYBOOK MLETEROR                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= ELECTONIC TRANSACTION SERVICES ERROR RECORD                  =*        
      *=                                                              =*        
      *= THIS RECORD IS USED TO PASS ERROR CODES BETWEEN THE MAIN     =*        
      *= ELECTRONIC TRANSACTION PROCESSING MODULES.                   =*        
      *=                                                              =*        
      *=================================================================        
      *#################################################################        
      *#                   MAINTENANCE LOG                            #*        
      *#                   ===============                            #*        
      *#  *PROJECT*     DATE (DD/MM/YY)    INITIALS                   #*        
      *#  ( BUG # )   - DESCRIPTION                                   #*        
      *#                                                              #*        
      *# ETS PROJECT    AUG  22, 1995      JOHN HEIBEIN               #*        
      *#              - CREATED                                       #*        
      *#                                                              #*        
      *# PHASE 2        NOV/96             M. PRANGE                  #*        
      *#              - DISTINGUISH SYSTEM VERSES APPLICATION CRITICAL#*        
      *#                ERRORS                                        #*        
      *#                                                              #*        
      *#################################################################        
                                                                                
      *01  MLETEROR.                                                    00000140
           05  EROR-RETURN-MESSAGE-AREA.                                00000140
               10  EROR-ERROR-SEVERITY-SW               PIC X(01).              
                   88  CRITICAL-ERROR                    VALUE 'Y' 'S'.         
                   88  APPL-CRITICAL-ERROR               VALUE 'Y'.             
                   88  SYSTEM-CRITICAL-ERROR             VALUE 'S'.             
                   88  NON-CRITICAL-ERROR                VALUE ' '.             
               10  EROR-MESSAGE-COUNTER     COMP        PIC 9(04).        000002
               10  EROR-MESSAGE-DETAIL      OCCURS 10.                    000003
                   15  EROR-APPL-ID                     PIC X(04).              
                   15  EROR-APPL-MSG-CODE               PIC 9(05).        000003
                   15  EROR-GROUP-ID-LVL1               PIC 9(04).              
                   15  EROR-GROUP-SEQ-NO-LVL1           PIC 9(04).              
                   15  EROR-GROUP-ID-LVL2               PIC 9(04).              
                   15  EROR-GROUP-SEQ-NO-LVL2           PIC 9(04).              
                   15  EROR-GROUP-ID-LVL3               PIC 9(04).              
                   15  EROR-GROUP-SEQ-NO-LVL3           PIC 9(04).              
                   15  EROR-MESSAGE-DATA                PIC X(80).        000003
               10  FILLER                               PIC X(20).        000002
