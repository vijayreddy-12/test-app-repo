1*******************************************************************************
 *******************************************************************************
 **                                                                           **
 ** ELEMENT BROWSE                                            14NOV25  14:54  **
 **                                                                           **
 **    ENVIRONMENT:   MVSPROD    SYSTEM:    CJ        SUBSYSTEM:  STANDARD    **
 **    TYPE:          COPYBOOK   STAGE ID:  P                                 **
 **    ELEMENT:       CMQV                                                    **
 **                                                                           **
 **    SIGNED OUT TO: HENGKIM                         DELTA TYPE: REVERSE     **
 **                                                                           **
1*******************************************************************************
 *******************************************************************************
                                                                                
 -------------------------- SOURCE LEVEL INFORMATION ---------------------------
                                                                                
1  VVLL SYNC USER     DATE    TIME     STMTS CCID         COMMENT               
   ---- ---- -------- ------- ----- -------- ------------ ----------------------
   0100      DHARKOU  10DEC12 11:08     1111 ILCTMMC500KD ADD COPYBOOK TO D1    
 GENERATED   PPCNDEV  18MAR13 16:55          ILCJMMC500KD MOVE COPYBOOK TO PROD 
 RETRIEVED   HENGKIM  20OCT23 12:33          ADMIN        ADMIN                 
                                                                                
  +0100        *****************************************************************
  +0100        **                                                              *
  +0100        **  FILE NAME:        CMQV                                      *
  +0100        **                                                              *
  +0100        **  DESCRIPTIVE NAME: COBOL copy file for MQI constants         *
  +0100        **                                                              *
  +0100        **  @START_COPYRIGHT@                                           *
  +0100        **  Statement:     Licensed Materials - Property of IBM         *
  +0100        **                                                              *
  +0100        **                 5695-137                                     *
  +0100        **                 (C) Copyright IBM Corporation. 1993, 1997    *
  +0100        **                                                              *
  +0100        **  Status:        Version 1 Release 2                          *
  +0100        **                                                              *
  +0100        **  @END_COPYRIGHT@                                             *
  +0100        **                                                              *
  +0100        **  FUNCTION:         This file declares constants for the      *
  +0100        **                    IBM Message Queue Interface (MQI).        *
  +0100        **                                                              *
  +0100        **  ENVIRONMENT:      MVS/ESA                                   *
  +0100        **                                                              *
  +0100        **  $01 PQ14817 120 980601 PJE   : Add CHANNEL_STOPPED_BY_USER  *
  +0100        **  $02 PQ17659 120 980801 PJE   : well CHANNEL-STOPPED-BY-USER *
  +0100        **  $03 PQ13387 120 980801 CP    : CICS Bridge                  *
  +0100        **  $04 PQ21707 120 990108 PJE   : Add MQAT-NOTES-AGENT         *
  +0100        **  $05 PQ23785 120 990211 PJE   : Correct MQAT-NOTES-AGENT     *
  +0100        **                                                              *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQCIH Structure                           *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQCIH-STRUC-ID PIC X(4) VALUE 'CIH '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQCIH-VERSION-1       PIC S9(9) BINARY VALUE 1.             
  +0100          10 MQCIH-VERSION-2       PIC S9(9) BINARY VALUE 2.             
  +0100          10 MQCIH-CURRENT-VERSION PIC S9(9) BINARY VALUE 2.             
  +0100                                                                         
  +0100        **   Structure Length                                            
  +0100          10 MQCIH-LENGTH-1       PIC S9(9) BINARY VALUE 164.            
  +0100          10 MQCIH-LENGTH-2       PIC S9(9) BINARY VALUE 180.            
  +0100          10 MQCIH-CURRENT-LENGTH PIC S9(9) BINARY VALUE 180.            
  +0100                                                                         
  +0100        **   Flags                                                       
  +0100          10 MQCIH-NONE PIC S9(9) BINARY VALUE 0.                        
  +0100                                                                         
  +0100        **   Return Code                                                 
  +0100          10 MQCRC-OK                    PIC S9(9) BINARY VALUE 0.       
  +0100          10 MQCRC-CICS-EXEC-ERROR       PIC S9(9) BINARY VALUE 1.       
  +0100          10 MQCRC-MQ-API-ERROR          PIC S9(9) BINARY VALUE 2.       
  +0100          10 MQCRC-BRIDGE-ERROR          PIC S9(9) BINARY VALUE 3.       
  +0100          10 MQCRC-BRIDGE-ABEND          PIC S9(9) BINARY VALUE 4.       
  +0100          10 MQCRC-APPLICATION-ABEND     PIC S9(9) BINARY VALUE 5.       
  +0100          10 MQCRC-SECURITY-ERROR        PIC S9(9) BINARY VALUE 6.       
  +0100          10 MQCRC-PROGRAM-NOT-AVAILABLE PIC S9(9) BINARY VALUE 7.       
  +0100          10 MQCRC-BRIDGE-TIMEOUT        PIC S9(9) BINARY VALUE 8.       
  +0100          10 MQCRC-TRANSID-NOT-AVAILABLE PIC S9(9) BINARY VALUE 9.       
  +0100                                                                         
  +0100        **   Unit of Work Control                                        
  +0100          10 MQCUOWC-FIRST    PIC S9(9) BINARY VALUE 17.                 
  +0100          10 MQCUOWC-MIDDLE   PIC S9(9) BINARY VALUE 16.                 
  +0100          10 MQCUOWC-LAST     PIC S9(9) BINARY VALUE 272.                
  +0100          10 MQCUOWC-ONLY     PIC S9(9) BINARY VALUE 273.                
  +0100          10 MQCUOWC-COMMIT   PIC S9(9) BINARY VALUE 256.                
  +0100          10 MQCUOWC-BACKOUT  PIC S9(9) BINARY VALUE 4352.               
  +0100          10 MQCUOWC-CONTINUE PIC S9(9) BINARY VALUE 65536.              
  +0100                                                                         
  +0100        **   Get Wait Interval                                           
  +0100          10 MQCGWI-DEFAULT PIC S9(9) BINARY VALUE -2.                   
  +0100                                                                         
  +0100        **   Link Type                                                   
  +0100          10 MQCLT-PROGRAM     PIC S9(9) BINARY VALUE 1.                 
  +0100          10 MQCLT-TRANSACTION PIC S9(9) BINARY VALUE 2.                 
  +0100                                                                         
  +0100        **   Output Data Length                                          
  +0100          10 MQCODL-AS-INPUT PIC S9(9) BINARY VALUE -1.                  
  +0100                                                                         
  +0100        **   ADS Descriptor                                              
  +0100          10 MQCADSD-NONE      PIC S9(9) BINARY VALUE 0.                 
  +0100          10 MQCADSD-SEND      PIC S9(9) BINARY VALUE 1.                 
  +0100          10 MQCADSD-RECV      PIC S9(9) BINARY VALUE 16.                
  +0100          10 MQCADSD-MSGFORMAT PIC S9(9) BINARY VALUE 256.               
  +0100                                                                         
  +0100        **   Conversational Task                                         
  +0100          10 MQCCT-YES PIC S9(9) BINARY VALUE 1.                         
  +0100          10 MQCCT-NO  PIC S9(9) BINARY VALUE 0.                         
  +0100                                                                         
  +0100        **   Task End Status                                             
  +0100          10 MQCTES-NOSYNC  PIC S9(9) BINARY VALUE 0.                    
  +0100          10 MQCTES-COMMIT  PIC S9(9) BINARY VALUE 256.                  
  +0100          10 MQCTES-BACKOUT PIC S9(9) BINARY VALUE 4352.                 
  +0100          10 MQCTES-ENDTASK PIC S9(9) BINARY VALUE 65536.                
  +0100                                                                         
  +0100        **   Facility                                                    
  +0100          10 MQCFAC-NONE PIC X(8) VALUE LOW-VALUES.                      
  +0100                                                                         
  +0100        **   Function                                                    
  +0100          10 MQCFUNC-MQCONN PIC X(4) VALUE 'CONN'.                       
  +0100          10 MQCFUNC-MQGET  PIC X(4) VALUE 'GET '.                       
  +0100          10 MQCFUNC-MQINQ  PIC X(4) VALUE 'INQ '.                       
  +0100          10 MQCFUNC-MQOPEN PIC X(4) VALUE 'OPEN'.                       
  +0100          10 MQCFUNC-MQPUT  PIC X(4) VALUE 'PUT '.                       
  +0100          10 MQCFUNC-MQPUT1 PIC X(4) VALUE 'PUT1'.                       
  +0100          10 MQCFUNC-NONE   PIC X(4) VALUE SPACES.                       
  +0100                                                                         
  +0100        **   Start Code                                                  
  +0100          10 MQCSC-START     PIC X(4) VALUE 'S   '.                      
  +0100          10 MQCSC-STARTDATA PIC X(4) VALUE 'SD  '.                      
  +0100          10 MQCSC-TERMINPUT PIC X(4) VALUE 'TD  '.                      
  +0100          10 MQCSC-NONE      PIC X(4) VALUE SPACES.                      
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQDLH Structure                           *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQDLH-STRUC-ID PIC X(4) VALUE 'DLH '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQDLH-VERSION-1       PIC S9(9) BINARY VALUE 1.             
  +0100          10 MQDLH-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.             
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQGMO Structure                           *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQGMO-STRUC-ID PIC X(4) VALUE 'GMO '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQGMO-VERSION-1       PIC S9(9) BINARY VALUE 1.             
  +0100          10 MQGMO-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.             
  +0100                                                                         
  +0100        **   Get-Message Options                                         
  +0100          10 MQGMO-WAIT                    PIC S9(9) BINARY VALUE 1.     
  +0100          10 MQGMO-NO-WAIT                 PIC S9(9) BINARY VALUE 0.     
  +0100          10 MQGMO-SYNCPOINT               PIC S9(9) BINARY VALUE 2.     
  +0100          10 MQGMO-SYNCPOINT-IF-PERSISTENT PIC S9(9) BINARY VALUE 4096.  
  +0100          10 MQGMO-NO-SYNCPOINT            PIC S9(9) BINARY VALUE 4.     
  +0100          10 MQGMO-MARK-SKIP-BACKOUT       PIC S9(9) BINARY VALUE 128.   
  +0100          10 MQGMO-BROWSE-FIRST            PIC S9(9) BINARY VALUE 16.    
  +0100          10 MQGMO-BROWSE-NEXT             PIC S9(9) BINARY VALUE 32.    
  +0100          10 MQGMO-MSG-UNDER-CURSOR        PIC S9(9) BINARY VALUE 256.   
  +0100          10 MQGMO-ACCEPT-TRUNCATED-MSG    PIC S9(9) BINARY VALUE 64.    
  +0100          10 MQGMO-SET-SIGNAL              PIC S9(9) BINARY VALUE 8.     
  +0100          10 MQGMO-FAIL-IF-QUIESCING       PIC S9(9) BINARY VALUE 8192.  
  +0100          10 MQGMO-CONVERT                 PIC S9(9) BINARY VALUE 16384. 
  +0100          10 MQGMO-NONE                    PIC S9(9) BINARY VALUE 0.     
  +0100                                                                         
  +0100        **   Wait Interval                                               
  +0100          10 MQWI-UNLIMITED PIC S9(9) BINARY VALUE -1.                   
  +0100                                                                         
  +0100        **   Signal Values                                               
  +0100          10 MQEC-MSG-ARRIVED           PIC S9(9) BINARY VALUE 2.        
  +0100          10 MQEC-WAIT-INTERVAL-EXPIRED PIC S9(9) BINARY VALUE 3.        
  +0100          10 MQEC-WAIT-CANCELED         PIC S9(9) BINARY VALUE 4.        
  +0100          10 MQEC-Q-MGR-QUIESCING       PIC S9(9) BINARY VALUE 5.        
  +0100          10 MQEC-CONNECTION-QUIESCING  PIC S9(9) BINARY VALUE 6.        
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQIIH Structure                           *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQIIH-STRUC-ID PIC X(4) VALUE 'IIH '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQIIH-VERSION-1       PIC S9(9) BINARY VALUE 1.             
  +0100          10 MQIIH-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.             
  +0100                                                                         
  +0100        **   Structure Length                                            
  +0100          10 MQIIH-LENGTH-1 PIC S9(9) BINARY VALUE 84.                   
  +0100                                                                         
  +0100        **   Flags                                                       
  +0100          10 MQIIH-NONE PIC S9(9) BINARY VALUE 0.                        
  +0100                                                                         
  +0100        **   Authenticator                                               
  +0100          10 MQIAUT-NONE PIC X(8) VALUE SPACES.                          
  +0100                                                                         
  +0100        **   Transaction Instance Identifier                             
  +0100          10 MQITII-NONE PIC X(16) VALUE LOW-VALUES.                     
  +0100                                                                         
  +0100        **   Transaction State                                           
  +0100          10 MQITS-IN-CONVERSATION     PIC X VALUE 'C'.                  
  +0100          10 MQITS-NOT-IN-CONVERSATION PIC X VALUE ' '.                  
  +0100                                                                         
  +0100        **   Commit Mode                                                 
  +0100          10 MQICM-COMMIT-THEN-SEND PIC X VALUE '0'.                     
  +0100          10 MQICM-SEND-THEN-COMMIT PIC X VALUE '1'.                     
  +0100                                                                         
  +0100        **   Security Scope                                              
  +0100          10 MQISS-CHECK PIC X VALUE 'C'.                                
  +0100          10 MQISS-FULL  PIC X VALUE 'F'.                                
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQMD Structure                            *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQMD-STRUC-ID PIC X(4) VALUE 'MD  '.                        
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQMD-VERSION-1       PIC S9(9) BINARY VALUE 1.              
  +0100          10 MQMD-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.              
  +0100                                                                         
  +0100        **   Report Options                                              
  +0100          10 MQRO-EXCEPTION                 PIC S9(9) BINARY VALUE       
  +0100               16777216.                                                 
  +0100          10 MQRO-EXCEPTION-WITH-DATA       PIC S9(9) BINARY VALUE       
  +0100               50331648.                                                 
  +0100          10 MQRO-EXCEPTION-WITH-FULL-DATA  PIC S9(9) BINARY VALUE       
  +0100               117440512.                                                
  +0100          10 MQRO-EXPIRATION                PIC S9(9) BINARY VALUE       
  +0100               2097152.                                                  
  +0100          10 MQRO-EXPIRATION-WITH-DATA      PIC S9(9) BINARY VALUE       
  +0100               6291456.                                                  
  +0100          10 MQRO-EXPIRATION-WITH-FULL-DATA PIC S9(9) BINARY VALUE       
  +0100               14680064.                                                 
  +0100          10 MQRO-COA                       PIC S9(9) BINARY VALUE 256.  
  +0100          10 MQRO-COA-WITH-DATA             PIC S9(9) BINARY VALUE 768.  
  +0100          10 MQRO-COA-WITH-FULL-DATA        PIC S9(9) BINARY VALUE 1792. 
  +0100          10 MQRO-COD                       PIC S9(9) BINARY VALUE 2048. 
  +0100          10 MQRO-COD-WITH-DATA             PIC S9(9) BINARY VALUE 6144. 
  +0100          10 MQRO-COD-WITH-FULL-DATA        PIC S9(9) BINARY VALUE 14336.
  +0100          10 MQRO-PAN                       PIC S9(9) BINARY VALUE 1.    
  +0100          10 MQRO-NAN                       PIC S9(9) BINARY VALUE 2.    
  +0100          10 MQRO-NEW-MSG-ID                PIC S9(9) BINARY VALUE 0.    
  +0100          10 MQRO-PASS-MSG-ID               PIC S9(9) BINARY VALUE 128.  
  +0100          10 MQRO-COPY-MSG-ID-TO-CORREL-ID  PIC S9(9) BINARY VALUE 0.    
  +0100          10 MQRO-PASS-CORREL-ID            PIC S9(9) BINARY VALUE 64.   
  +0100          10 MQRO-DEAD-LETTER-Q             PIC S9(9) BINARY VALUE 0.    
  +0100          10 MQRO-DISCARD-MSG               PIC S9(9) BINARY VALUE       
  +0100               134217728.                                                
  +0100          10 MQRO-NONE                      PIC S9(9) BINARY VALUE 0.    
  +0100                                                                         
  +0100        **   Report Options Masks                                        
  +0100          10 MQRO-REJECT-UNSUP-MASK         PIC S9(9) BINARY VALUE       
  +0100               270270464.                                                
  +0100          10 MQRO-ACCEPT-UNSUP-MASK         PIC S9(9) BINARY VALUE       
  +0100               -270532353.                                               
  +0100          10 MQRO-ACCEPT-UNSUP-IF-XMIT-MASK PIC S9(9) BINARY VALUE 261888
  +0100                                                                         
  +0100        **   Message Types                                               
  +0100          10 MQMT-SYSTEM-FIRST PIC S9(9) BINARY VALUE 1.                 
  +0100          10 MQMT-REQUEST      PIC S9(9) BINARY VALUE 1.                 
  +0100          10 MQMT-REPLY        PIC S9(9) BINARY VALUE 2.                 
  +0100          10 MQMT-DATAGRAM     PIC S9(9) BINARY VALUE 8.                 
  +0100          10 MQMT-REPORT       PIC S9(9) BINARY VALUE 4.                 
  +0100          10 MQMT-SYSTEM-LAST  PIC S9(9) BINARY VALUE 65535.             
  +0100          10 MQMT-APPL-FIRST   PIC S9(9) BINARY VALUE 65536.             
  +0100          10 MQMT-APPL-LAST    PIC S9(9) BINARY VALUE 999999999.         
  +0100                                                                         
  +0100        **   Expiry                                                      
  +0100          10 MQEI-UNLIMITED PIC S9(9) BINARY VALUE -1.                   
  +0100                                                                         
  +0100        **   Feedback Values                                             
  +0100          10 MQFB-NONE                   PIC S9(9) BINARY VALUE 0.       
  +0100          10 MQFB-SYSTEM-FIRST           PIC S9(9) BINARY VALUE 1.       
  +0100          10 MQFB-QUIT                   PIC S9(9) BINARY VALUE 256.     
  +0100          10 MQFB-EXPIRATION             PIC S9(9) BINARY VALUE 258.     
  +0100          10 MQFB-COA                    PIC S9(9) BINARY VALUE 259.     
  +0100          10 MQFB-COD                    PIC S9(9) BINARY VALUE 260.     
  +0100          10 MQFB-PAN                    PIC S9(9) BINARY VALUE 275.     
  +0100          10 MQFB-NAN                    PIC S9(9) BINARY VALUE 276.     
  +0100          10 MQFB-CHANNEL-COMPLETED      PIC S9(9) BINARY VALUE 262.     
  +0100          10 MQFB-CHANNEL-FAIL-RETRY     PIC S9(9) BINARY VALUE 263.     
  +0100          10 MQFB-CHANNEL-FAIL           PIC S9(9) BINARY VALUE 264.     
  +0100          10 MQFB-APPL-CANNOT-BE-STARTED PIC S9(9) BINARY VALUE 265.     
  +0100          10 MQFB-TM-ERROR               PIC S9(9) BINARY VALUE 266.     
  +0100          10 MQFB-APPL-TYPE-ERROR        PIC S9(9) BINARY VALUE 267.     
  +0100          10 MQFB-STOPPED-BY-MSG-EXIT    PIC S9(9) BINARY VALUE 268.     
  +0100          10 MQFB-XMIT-Q-MSG-ERROR       PIC S9(9) BINARY VALUE 271.     
  +0100          10 MQFB-DATA-LENGTH-ZERO       PIC S9(9) BINARY VALUE 291.     
  +0100          10 MQFB-DATA-LENGTH-NEGATIVE   PIC S9(9) BINARY VALUE 292.     
  +0100          10 MQFB-DATA-LENGTH-TOO-BIG    PIC S9(9) BINARY VALUE 293.     
  +0100          10 MQFB-BUFFER-OVERFLOW        PIC S9(9) BINARY VALUE 294.     
  +0100          10 MQFB-LENGTH-OFF-BY-ONE      PIC S9(9) BINARY VALUE 295.     
  +0100          10 MQFB-IIH-ERROR              PIC S9(9) BINARY VALUE 296.     
  +0100          10 MQFB-NOT-AUTHORIZED-FOR-IMS PIC S9(9) BINARY VALUE 298.     
  +0100          10 MQFB-IMS-ERROR              PIC S9(9) BINARY VALUE 300.     
  +0100          10 MQFB-IMS-FIRST              PIC S9(9) BINARY VALUE 301.     
  +0100          10 MQFB-IMS-LAST               PIC S9(9) BINARY VALUE 399.     
  +0100          10 MQFB-CICS-INTERNAL-ERROR    PIC S9(9) BINARY VALUE 401.     
  +0100          10 MQFB-CICS-NOT-AUTHORIZED    PIC S9(9) BINARY VALUE 402.     
  +0100          10 MQFB-CICS-BRIDGE-FAILURE    PIC S9(9) BINARY VALUE 403.     
  +0100          10 MQFB-CICS-CORREL-ID-ERROR   PIC S9(9) BINARY VALUE 404.     
  +0100          10 MQFB-CICS-CCSID-ERROR       PIC S9(9) BINARY VALUE 405.     
  +0100          10 MQFB-CICS-ENCODING-ERROR    PIC S9(9) BINARY VALUE 406.     
  +0100          10 MQFB-CICS-CIH-ERROR         PIC S9(9) BINARY VALUE 407.     
  +0100          10 MQFB-CICS-UOW-ERROR         PIC S9(9) BINARY VALUE 408.     
  +0100          10 MQFB-CICS-COMMAREA-ERROR    PIC S9(9) BINARY VALUE 409.     
  +0100          10 MQFB-CICS-APPL-NOT-STARTED  PIC S9(9) BINARY VALUE 410.     
  +0100          10 MQFB-CICS-APPL-ABENDED      PIC S9(9) BINARY VALUE 411.     
  +0100          10 MQFB-CICS-DLQ-ERROR         PIC S9(9) BINARY VALUE 412.     
  +0100          10 MQFB-CICS-UOW-BACKED-OUT    PIC S9(9) BINARY VALUE 413.     
  +0100          10 MQFB-SYSTEM-LAST            PIC S9(9) BINARY VALUE 65535.   
  +0100          10 MQFB-APPL-FIRST             PIC S9(9) BINARY VALUE 65536.   
  +0100          10 MQFB-APPL-LAST              PIC S9(9) BINARY VALUE 999999999
  +0100                                                                         
  +0100        **   Encoding                                                    
  +0100          10 MQENC-NATIVE PIC S9(9) BINARY VALUE 785.                    
  +0100                                                                         
  +0100        **   Encoding Masks                                              
  +0100          10 MQENC-INTEGER-MASK  PIC S9(9) BINARY VALUE 15.              
  +0100          10 MQENC-DECIMAL-MASK  PIC S9(9) BINARY VALUE 240.             
  +0100          10 MQENC-FLOAT-MASK    PIC S9(9) BINARY VALUE 3840.            
  +0100          10 MQENC-RESERVED-MASK PIC S9(9) BINARY VALUE -4096.           
  +0100                                                                         
  +0100        **   Encodings for Binary Integers                               
  +0100          10 MQENC-INTEGER-UNDEFINED PIC S9(9) BINARY VALUE 0.           
  +0100          10 MQENC-INTEGER-NORMAL    PIC S9(9) BINARY VALUE 1.           
  +0100          10 MQENC-INTEGER-REVERSED  PIC S9(9) BINARY VALUE 2.           
  +0100                                                                         
  +0100        **   Encodings for Packed-Decimal Integers                       
  +0100          10 MQENC-DECIMAL-UNDEFINED PIC S9(9) BINARY VALUE 0.           
  +0100          10 MQENC-DECIMAL-NORMAL    PIC S9(9) BINARY VALUE 16.          
  +0100          10 MQENC-DECIMAL-REVERSED  PIC S9(9) BINARY VALUE 32.          
  +0100                                                                         
  +0100        **   Encodings for Floating-Point Numbers                        
  +0100          10 MQENC-FLOAT-UNDEFINED     PIC S9(9) BINARY VALUE 0.         
  +0100          10 MQENC-FLOAT-IEEE-NORMAL   PIC S9(9) BINARY VALUE 256.       
  +0100          10 MQENC-FLOAT-IEEE-REVERSED PIC S9(9) BINARY VALUE 512.       
  +0100          10 MQENC-FLOAT-S390          PIC S9(9) BINARY VALUE 768.       
  +0100                                                                         
  +0100        **   Coded Character-Set Identifiers                             
  +0100          10 MQCCSI-DEFAULT  PIC S9(9) BINARY VALUE 0.                   
  +0100          10 MQCCSI-Q-MGR    PIC S9(9) BINARY VALUE 0.                   
  +0100          10 MQCCSI-EMBEDDED PIC S9(9) BINARY VALUE -1.                  
  +0100                                                                         
  +0100        **   Formats                                                     
  +0100          10 MQFMT-NONE               PIC X(8) VALUE SPACES.             
  +0100          10 MQFMT-ADMIN              PIC X(8) VALUE 'MQADMIN '.         
  +0100          10 MQFMT-CHANNEL-COMPLETED  PIC X(8) VALUE 'MQCHCOM '.         
  +0100          10 MQFMT-CICS               PIC X(8) VALUE 'MQCICS  '.         
  +0100          10 MQFMT-COMMAND-1          PIC X(8) VALUE 'MQCMD1  '.         
  +0100          10 MQFMT-COMMAND-2          PIC X(8) VALUE 'MQCMD2  '.         
  +0100          10 MQFMT-DEAD-LETTER-HEADER PIC X(8) VALUE 'MQDEAD  '.         
  +0100          10 MQFMT-EVENT              PIC X(8) VALUE 'MQEVENT '.         
  +0100          10 MQFMT-IMS                PIC X(8) VALUE 'MQIMS   '.         
  +0100          10 MQFMT-IMS-VAR-STRING     PIC X(8) VALUE 'MQIMSVS '.         
  +0100          10 MQFMT-MD-EXTENSION       PIC X(8) VALUE 'MQHMDE  '.         
  +0100          10 MQFMT-PCF                PIC X(8) VALUE 'MQPCF   '.         
  +0100          10 MQFMT-STRING             PIC X(8) VALUE 'MQSTR   '.         
  +0100          10 MQFMT-TRIGGER            PIC X(8) VALUE 'MQTRIG  '.         
  +0100          10 MQFMT-XMIT-Q-HEADER      PIC X(8) VALUE 'MQXMIT  '.         
  +0100                                                                         
  +0100        **   Priority                                                    
  +0100          10 MQPRI-PRIORITY-AS-Q-DEF PIC S9(9) BINARY VALUE -1.          
  +0100                                                                         
  +0100        **   Persistence Values                                          
  +0100          10 MQPER-PERSISTENT           PIC S9(9) BINARY VALUE 1.        
  +0100          10 MQPER-NOT-PERSISTENT       PIC S9(9) BINARY VALUE 0.        
  +0100          10 MQPER-PERSISTENCE-AS-Q-DEF PIC S9(9) BINARY VALUE 2.        
  +0100                                                                         
  +0100        **   Message Identifier                                          
  +0100          10 MQMI-NONE PIC X(24) VALUE LOW-VALUES.                       
  +0100                                                                         
  +0100        **   Correlation Identifier                                      
  +0100          10 MQCI-NONE        PIC X(24) VALUE LOW-VALUES.                
  +0100          10 MQCI-NEW-SESSION PIC X(24) VALUE                            
  +0100               X'414D51214E45575F53455353494F4E5F434F5252454C4944'.      
  +0100                                                                         
  +0100        **   Accounting Token                                            
  +0100          10 MQACT-NONE PIC X(32) VALUE LOW-VALUES.                      
  +0100                                                                         
  +0100        **   Put Application Types                                       
  +0100          10 MQAT-UNKNOWN     PIC S9(9) BINARY VALUE -1.                 
  +0100          10 MQAT-NO-CONTEXT  PIC S9(9) BINARY VALUE 0.                  
  +0100          10 MQAT-CICS        PIC S9(9) BINARY VALUE 1.                  
  +0100          10 MQAT-MVS         PIC S9(9) BINARY VALUE 2.                  
  +0100          10 MQAT-IMS         PIC S9(9) BINARY VALUE 3.                  
  +0100          10 MQAT-OS2         PIC S9(9) BINARY VALUE 4.                  
  +0100          10 MQAT-DOS         PIC S9(9) BINARY VALUE 5.                  
  +0100          10 MQAT-AIX         PIC S9(9) BINARY VALUE 6.                  
  +0100          10 MQAT-UNIX        PIC S9(9) BINARY VALUE 6.                  
  +0100          10 MQAT-QMGR        PIC S9(9) BINARY VALUE 7.                  
  +0100          10 MQAT-OS400       PIC S9(9) BINARY VALUE 8.                  
  +0100          10 MQAT-WINDOWS     PIC S9(9) BINARY VALUE 9.                  
  +0100          10 MQAT-CICS-VSE    PIC S9(9) BINARY VALUE 10.                 
  +0100          10 MQAT-WINDOWS-NT  PIC S9(9) BINARY VALUE 11.                 
  +0100          10 MQAT-VMS         PIC S9(9) BINARY VALUE 12.                 
  +0100          10 MQAT-GUARDIAN    PIC S9(9) BINARY VALUE 13.                 
  +0100          10 MQAT-VOS         PIC S9(9) BINARY VALUE 14.                 
  +0100          10 MQAT-IMS-BRIDGE  PIC S9(9) BINARY VALUE 19.                 
  +0100          10 MQAT-XCF         PIC S9(9) BINARY VALUE 20.                 
  +0100          10 MQAT-CICS-BRIDGE PIC S9(9) BINARY VALUE 21.                 
  +0100          10 MQAT-NOTES-AGENT PIC S9(9) BINARY VALUE 22.                 
  +0100          10 MQAT-DEFAULT     PIC S9(9) BINARY VALUE 2.                  
  +0100          10 MQAT-USER-FIRST  PIC S9(9) BINARY VALUE 65536.              
  +0100          10 MQAT-USER-LAST   PIC S9(9) BINARY VALUE 999999999.          
  +0100                                                                         
  +0100        **   Group Identifier                                            
  +0100          10 MQGI-NONE PIC X(24) VALUE LOW-VALUES.                       
  +0100                                                                         
  +0100        **   Message Flags                                               
  +0100          10 MQMF-SEGMENTATION-INHIBITED PIC S9(9) BINARY VALUE 0.       
  +0100          10 MQMF-SEGMENTATION-ALLOWED   PIC S9(9) BINARY VALUE 1.       
  +0100          10 MQMF-MSG-IN-GROUP           PIC S9(9) BINARY VALUE 8.       
  +0100          10 MQMF-LAST-MSG-IN-GROUP      PIC S9(9) BINARY VALUE 16.      
  +0100          10 MQMF-SEGMENT                PIC S9(9) BINARY VALUE 2.       
  +0100          10 MQMF-LAST-SEGMENT           PIC S9(9) BINARY VALUE 4.       
  +0100          10 MQMF-NONE                   PIC S9(9) BINARY VALUE 0.       
  +0100                                                                         
  +0100        **   Message Flags Masks                                         
  +0100          10 MQMF-REJECT-UNSUP-MASK         PIC S9(9) BINARY VALUE 4095. 
  +0100          10 MQMF-ACCEPT-UNSUP-MASK         PIC S9(9) BINARY VALUE       
  +0100               -1048576.                                                 
  +0100          10 MQMF-ACCEPT-UNSUP-IF-XMIT-MASK PIC S9(9) BINARY VALUE       
  +0100               1044480.                                                  
  +0100                                                                         
  +0100        **   Original Length                                             
  +0100          10 MQOL-UNDEFINED PIC S9(9) BINARY VALUE -1.                   
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQMDE Structure                           *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQMDE-STRUC-ID PIC X(4) VALUE 'MDE '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQMDE-VERSION-2       PIC S9(9) BINARY VALUE 2.             
  +0100          10 MQMDE-CURRENT-VERSION PIC S9(9) BINARY VALUE 2.             
  +0100                                                                         
  +0100        **   Structure Length                                            
  +0100          10 MQMDE-LENGTH-2 PIC S9(9) BINARY VALUE 72.                   
  +0100                                                                         
  +0100        **   General Flags                                               
  +0100          10 MQMDEF-NONE PIC S9(9) BINARY VALUE 0.                       
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQOD Structure                            *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQOD-STRUC-ID PIC X(4) VALUE 'OD  '.                        
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQOD-VERSION-1       PIC S9(9) BINARY VALUE 1.              
  +0100          10 MQOD-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.              
  +0100                                                                         
  +0100        **   Structure Length                                            
  +0100          10 MQOD-CURRENT-LENGTH PIC S9(9) BINARY VALUE 168.             
  +0100                                                                         
  +0100        **   Object Types                                                
  +0100          10 MQOT-Q          PIC S9(9) BINARY VALUE 1.                   
  +0100          10 MQOT-NAMELIST   PIC S9(9) BINARY VALUE 2.                   
  +0100          10 MQOT-PROCESS    PIC S9(9) BINARY VALUE 3.                   
  +0100          10 MQOT-Q-MGR      PIC S9(9) BINARY VALUE 5.                   
  +0100          10 MQOT-CHANNEL    PIC S9(9) BINARY VALUE 6.                   
  +0100          10 MQOT-RESERVED-1 PIC S9(9) BINARY VALUE 7.                   
  +0100                                                                         
  +0100        **   Extended Object Types                                       
  +0100          10 MQOT-ALL               PIC S9(9) BINARY VALUE 1001.         
  +0100          10 MQOT-ALIAS-Q           PIC S9(9) BINARY VALUE 1002.         
  +0100          10 MQOT-MODEL-Q           PIC S9(9) BINARY VALUE 1003.         
  +0100          10 MQOT-LOCAL-Q           PIC S9(9) BINARY VALUE 1004.         
  +0100          10 MQOT-REMOTE-Q          PIC S9(9) BINARY VALUE 1005.         
  +0100          10 MQOT-SENDER-CHANNEL    PIC S9(9) BINARY VALUE 1007.         
  +0100          10 MQOT-SERVER-CHANNEL    PIC S9(9) BINARY VALUE 1008.         
  +0100          10 MQOT-REQUESTER-CHANNEL PIC S9(9) BINARY VALUE 1009.         
  +0100          10 MQOT-RECEIVER-CHANNEL  PIC S9(9) BINARY VALUE 1010.         
  +0100          10 MQOT-CURRENT-CHANNEL   PIC S9(9) BINARY VALUE 1011.         
  +0100          10 MQOT-SAVED-CHANNEL     PIC S9(9) BINARY VALUE 1012.         
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQPMO Structure                           *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQPMO-STRUC-ID PIC X(4) VALUE 'PMO '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQPMO-VERSION-1       PIC S9(9) BINARY VALUE 1.             
  +0100          10 MQPMO-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.             
  +0100                                                                         
  +0100        **   Structure Length                                            
  +0100          10 MQPMO-CURRENT-LENGTH PIC S9(9) BINARY VALUE 128.            
  +0100                                                                         
  +0100        **   Put-Message Options                                         
  +0100          10 MQPMO-SYNCPOINT                PIC S9(9) BINARY VALUE 2.    
  +0100          10 MQPMO-NO-SYNCPOINT             PIC S9(9) BINARY VALUE 4.    
  +0100          10 MQPMO-NO-CONTEXT               PIC S9(9) BINARY VALUE 16384.
  +0100          10 MQPMO-DEFAULT-CONTEXT          PIC S9(9) BINARY VALUE 32.   
  +0100          10 MQPMO-PASS-IDENTITY-CONTEXT    PIC S9(9) BINARY VALUE 256.  
  +0100          10 MQPMO-PASS-ALL-CONTEXT         PIC S9(9) BINARY VALUE 512.  
  +0100          10 MQPMO-SET-IDENTITY-CONTEXT     PIC S9(9) BINARY VALUE 1024. 
  +0100          10 MQPMO-SET-ALL-CONTEXT          PIC S9(9) BINARY VALUE 2048. 
  +0100          10 MQPMO-ALTERNATE-USER-AUTHORITY PIC S9(9) BINARY VALUE 4096. 
  +0100          10 MQPMO-FAIL-IF-QUIESCING        PIC S9(9) BINARY VALUE 8192. 
  +0100          10 MQPMO-NONE                     PIC S9(9) BINARY VALUE 0.    
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQRMH Structure                           *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQRMH-STRUC-ID PIC X(4) VALUE 'RMH '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQRMH-VERSION-1       PIC S9(9) BINARY VALUE 1.             
  +0100          10 MQRMH-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.             
  +0100                                                                         
  +0100        **   Flags                                                       
  +0100          10 MQRMHF-LAST     PIC S9(9) BINARY VALUE 1.                   
  +0100          10 MQRMHF-NOT-LAST PIC S9(9) BINARY VALUE 0.                   
  +0100                                                                         
  +0100        **   Object Instance Identifier                                  
  +0100          10 MQOII-NONE PIC X(24) VALUE LOW-VALUES.                      
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQTM Structure                            *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQTM-STRUC-ID PIC X(4) VALUE 'TM  '.                        
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQTM-VERSION-1       PIC S9(9) BINARY VALUE 1.              
  +0100          10 MQTM-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.              
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQTMC2 Structure                          *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQTMC-STRUC-ID PIC X(4) VALUE 'TMC '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQTMC-VERSION-1       PIC X(4) VALUE '   1'.                
  +0100          10 MQTMC-VERSION-2       PIC X(4) VALUE '   2'.                
  +0100          10 MQTMC-CURRENT-VERSION PIC X(4) VALUE '   2'.                
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQXQH Structure                           *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Structure Identifier                                        
  +0100          10 MQXQH-STRUC-ID PIC X(4) VALUE 'XQH '.                       
  +0100                                                                         
  +0100        **   Structure Version Number                                    
  +0100          10 MQXQH-VERSION-1       PIC S9(9) BINARY VALUE 1.             
  +0100          10 MQXQH-CURRENT-VERSION PIC S9(9) BINARY VALUE 1.             
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQCLOSE Call                              *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Close Options                                               
  +0100          10 MQCO-NONE         PIC S9(9) BINARY VALUE 0.                 
  +0100          10 MQCO-DELETE       PIC S9(9) BINARY VALUE 1.                 
  +0100          10 MQCO-DELETE-PURGE PIC S9(9) BINARY VALUE 2.                 
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQINQ Call                                *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Character-Attribute Selectors                               
  +0100          10 MQCA-APPL-ID               PIC S9(9) BINARY VALUE 2001.     
  +0100          10 MQCA-BACKOUT-REQ-Q-NAME    PIC S9(9) BINARY VALUE 2019.     
  +0100          10 MQCA-BASE-Q-NAME           PIC S9(9) BINARY VALUE 2002.     
  +0100          10 MQCA-CHANNEL-AUTO-DEF-EXIT PIC S9(9) BINARY VALUE 2026.     
  +0100          10 MQCA-COMMAND-INPUT-Q-NAME  PIC S9(9) BINARY VALUE 2003.     
  +0100          10 MQCA-CREATION-DATE         PIC S9(9) BINARY VALUE 2004.     
  +0100          10 MQCA-CREATION-TIME         PIC S9(9) BINARY VALUE 2005.     
  +0100          10 MQCA-DEAD-LETTER-Q-NAME    PIC S9(9) BINARY VALUE 2006.     
  +0100          10 MQCA-DEF-XMIT-Q-NAME       PIC S9(9) BINARY VALUE 2025.     
  +0100          10 MQCA-ENV-DATA              PIC S9(9) BINARY VALUE 2007.     
  +0100          10 MQCA-FIRST                 PIC S9(9) BINARY VALUE 2001.     
  +0100          10 MQCA-INITIATION-Q-NAME     PIC S9(9) BINARY VALUE 2008.     
  +0100          10 MQCA-LAST                  PIC S9(9) BINARY VALUE 4000.     
  +0100          10 MQCA-LAST-USED             PIC S9(9) BINARY VALUE 2026.     
  +0100          10 MQCA-NAMELIST-DESC         PIC S9(9) BINARY VALUE 2009.     
  +0100          10 MQCA-NAMELIST-NAME         PIC S9(9) BINARY VALUE 2010.     
  +0100          10 MQCA-NAMES                 PIC S9(9) BINARY VALUE 2020.     
  +0100          10 MQCA-PROCESS-DESC          PIC S9(9) BINARY VALUE 2011.     
  +0100          10 MQCA-PROCESS-NAME          PIC S9(9) BINARY VALUE 2012.     
  +0100          10 MQCA-Q-DESC                PIC S9(9) BINARY VALUE 2013.     
  +0100          10 MQCA-Q-MGR-DESC            PIC S9(9) BINARY VALUE 2014.     
  +0100          10 MQCA-Q-MGR-NAME            PIC S9(9) BINARY VALUE 2015.     
  +0100          10 MQCA-Q-NAME                PIC S9(9) BINARY VALUE 2016.     
  +0100          10 MQCA-REMOTE-Q-MGR-NAME     PIC S9(9) BINARY VALUE 2017.     
  +0100          10 MQCA-REMOTE-Q-NAME         PIC S9(9) BINARY VALUE 2018.     
  +0100          10 MQCA-STORAGE-CLASS         PIC S9(9) BINARY VALUE 2022.     
  +0100          10 MQCA-TRIGGER-DATA          PIC S9(9) BINARY VALUE 2023.     
  +0100          10 MQCA-USER-DATA             PIC S9(9) BINARY VALUE 2021.     
  +0100          10 MQCA-XMIT-Q-NAME           PIC S9(9) BINARY VALUE 2024.     
  +0100                                                                         
  +0100        **   Integer-Attribute Selectors                                 
  +0100          10 MQIA-APPL-TYPE                PIC S9(9) BINARY VALUE 1.     
  +0100          10 MQIA-AUTHORITY-EVENT          PIC S9(9) BINARY VALUE 47.    
  +0100          10 MQIA-BACKOUT-THRESHOLD        PIC S9(9) BINARY VALUE 22.    
  +0100          10 MQIA-CHANNEL-AUTO-DEF         PIC S9(9) BINARY VALUE 55.    
  +0100          10 MQIA-CHANNEL-AUTO-DEF-EVENT   PIC S9(9) BINARY VALUE 56.    
  +0100          10 MQIA-CODED-CHAR-SET-ID        PIC S9(9) BINARY VALUE 2.     
  +0100          10 MQIA-COMMAND-LEVEL            PIC S9(9) BINARY VALUE 31.    
  +0100          10 MQIA-CPI-LEVEL                PIC S9(9) BINARY VALUE 27.    
  +0100          10 MQIA-CURRENT-Q-DEPTH          PIC S9(9) BINARY VALUE 3.     
  +0100          10 MQIA-DEF-INPUT-OPEN-OPTION    PIC S9(9) BINARY VALUE 4.     
  +0100          10 MQIA-DEF-PERSISTENCE          PIC S9(9) BINARY VALUE 5.     
  +0100          10 MQIA-DEF-PRIORITY             PIC S9(9) BINARY VALUE 6.     
  +0100          10 MQIA-DEFINITION-TYPE          PIC S9(9) BINARY VALUE 7.     
  +0100          10 MQIA-DIST-LISTS               PIC S9(9) BINARY VALUE 34.    
  +0100          10 MQIA-FIRST                    PIC S9(9) BINARY VALUE 1.     
  +0100          10 MQIA-HARDEN-GET-BACKOUT       PIC S9(9) BINARY VALUE 8.     
  +0100          10 MQIA-HIGH-Q-DEPTH             PIC S9(9) BINARY VALUE 36.    
  +0100          10 MQIA-INDEX-TYPE               PIC S9(9) BINARY VALUE 57.    
  +0100          10 MQIA-INHIBIT-EVENT            PIC S9(9) BINARY VALUE 48.    
  +0100          10 MQIA-INHIBIT-GET              PIC S9(9) BINARY VALUE 9.     
  +0100          10 MQIA-INHIBIT-PUT              PIC S9(9) BINARY VALUE 10.    
  +0100          10 MQIA-LAST                     PIC S9(9) BINARY VALUE 2000.  
  +0100          10 MQIA-LAST-USED                PIC S9(9) BINARY VALUE 57.    
  +0100          10 MQIA-LOCAL-EVENT              PIC S9(9) BINARY VALUE 49.    
  +0100          10 MQIA-MAX-HANDLES              PIC S9(9) BINARY VALUE 11.    
  +0100          10 MQIA-MAX-MSG-LENGTH           PIC S9(9) BINARY VALUE 13.    
  +0100          10 MQIA-MAX-PRIORITY             PIC S9(9) BINARY VALUE 14.    
  +0100          10 MQIA-MAX-Q-DEPTH              PIC S9(9) BINARY VALUE 15.    
  +0100          10 MQIA-MAX-UNCOMMITTED-MSGS     PIC S9(9) BINARY VALUE 33.    
  +0100          10 MQIA-MSG-DELIVERY-SEQUENCE    PIC S9(9) BINARY VALUE 16.    
  +0100          10 MQIA-MSG-DEQ-COUNT            PIC S9(9) BINARY VALUE 38.    
  +0100          10 MQIA-MSG-ENQ-COUNT            PIC S9(9) BINARY VALUE 37.    
  +0100          10 MQIA-NAME-COUNT               PIC S9(9) BINARY VALUE 19.    
  +0100          10 MQIA-OPEN-INPUT-COUNT         PIC S9(9) BINARY VALUE 17.    
  +0100          10 MQIA-OPEN-OUTPUT-COUNT        PIC S9(9) BINARY VALUE 18.    
  +0100          10 MQIA-PERFORMANCE-EVENT        PIC S9(9) BINARY VALUE 53.    
  +0100          10 MQIA-PLATFORM                 PIC S9(9) BINARY VALUE 32.    
  +0100          10 MQIA-Q-DEPTH-HIGH-EVENT       PIC S9(9) BINARY VALUE 43.    
  +0100          10 MQIA-Q-DEPTH-HIGH-LIMIT       PIC S9(9) BINARY VALUE 40.    
  +0100          10 MQIA-Q-DEPTH-LOW-EVENT        PIC S9(9) BINARY VALUE 44.    
  +0100          10 MQIA-Q-DEPTH-LOW-LIMIT        PIC S9(9) BINARY VALUE 41.    
  +0100          10 MQIA-Q-DEPTH-MAX-EVENT        PIC S9(9) BINARY VALUE 42.    
  +0100          10 MQIA-Q-SERVICE-INTERVAL       PIC S9(9) BINARY VALUE 54.    
  +0100          10 MQIA-Q-SERVICE-INTERVAL-EVENT PIC S9(9) BINARY VALUE 46.    
  +0100          10 MQIA-Q-TYPE                   PIC S9(9) BINARY VALUE 20.    
  +0100          10 MQIA-REMOTE-EVENT             PIC S9(9) BINARY VALUE 50.    
  +0100          10 MQIA-RETENTION-INTERVAL       PIC S9(9) BINARY VALUE 21.    
  +0100          10 MQIA-SCOPE                    PIC S9(9) BINARY VALUE 45.    
  +0100          10 MQIA-SHAREABILITY             PIC S9(9) BINARY VALUE 23.    
  +0100          10 MQIA-START-STOP-EVENT         PIC S9(9) BINARY VALUE 52.    
  +0100          10 MQIA-SYNCPOINT                PIC S9(9) BINARY VALUE 30.    
  +0100          10 MQIA-TIME-SINCE-RESET         PIC S9(9) BINARY VALUE 35.    
  +0100          10 MQIA-TRIGGER-CONTROL          PIC S9(9) BINARY VALUE 24.    
  +0100          10 MQIA-TRIGGER-DEPTH            PIC S9(9) BINARY VALUE 29.    
  +0100          10 MQIA-TRIGGER-INTERVAL         PIC S9(9) BINARY VALUE 25.    
  +0100          10 MQIA-TRIGGER-MSG-PRIORITY     PIC S9(9) BINARY VALUE 26.    
  +0100          10 MQIA-TRIGGER-TYPE             PIC S9(9) BINARY VALUE 28.    
  +0100          10 MQIA-USAGE                    PIC S9(9) BINARY VALUE 12.    
  +0100                                                                         
  +0100        **   Integer Attribute Value Denoting "Not Applicable"           
  +0100          10 MQIAV-NOT-APPLICABLE PIC S9(9) BINARY VALUE -1.             
  +0100          10 MQIAV-UNDEFINED      PIC S9(9) BINARY VALUE -2.             
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to MQOPEN Call                               *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Open Options                                                
  +0100          10 MQOO-INPUT-AS-Q-DEF           PIC S9(9) BINARY VALUE 1.     
  +0100          10 MQOO-INPUT-SHARED             PIC S9(9) BINARY VALUE 2.     
  +0100          10 MQOO-INPUT-EXCLUSIVE          PIC S9(9) BINARY VALUE 4.     
  +0100          10 MQOO-BROWSE                   PIC S9(9) BINARY VALUE 8.     
  +0100          10 MQOO-OUTPUT                   PIC S9(9) BINARY VALUE 16.    
  +0100          10 MQOO-INQUIRE                  PIC S9(9) BINARY VALUE 32.    
  +0100          10 MQOO-SET                      PIC S9(9) BINARY VALUE 64.    
  +0100          10 MQOO-SAVE-ALL-CONTEXT         PIC S9(9) BINARY VALUE 128.   
  +0100          10 MQOO-PASS-IDENTITY-CONTEXT    PIC S9(9) BINARY VALUE 256.   
  +0100          10 MQOO-PASS-ALL-CONTEXT         PIC S9(9) BINARY VALUE 512.   
  +0100          10 MQOO-SET-IDENTITY-CONTEXT     PIC S9(9) BINARY VALUE 1024.  
  +0100          10 MQOO-SET-ALL-CONTEXT          PIC S9(9) BINARY VALUE 2048.  
  +0100          10 MQOO-ALTERNATE-USER-AUTHORITY PIC S9(9) BINARY VALUE 4096.  
  +0100          10 MQOO-FAIL-IF-QUIESCING        PIC S9(9) BINARY VALUE 8192.  
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to All Calls                                 *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Connection Handle                                           
  +0100          10 MQHC-DEF-HCONN PIC S9(9) BINARY VALUE 0.                    
  +0100                                                                         
  +0100        **   String Lengths                                              
  +0100          10 MQ-ABEND-CODE-LENGTH         PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-ACCOUNTING-TOKEN-LENGTH   PIC S9(9) BINARY VALUE 32.     
  +0100          10 MQ-APPL-IDENTITY-DATA-LENGTH PIC S9(9) BINARY VALUE 32.     
  +0100          10 MQ-APPL-NAME-LENGTH          PIC S9(9) BINARY VALUE 28.     
  +0100          10 MQ-APPL-ORIGIN-DATA-LENGTH   PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-ATTENTION-ID-LENGTH       PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-AUTHENTICATOR-LENGTH      PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-BRIDGE-NAME-LENGTH        PIC S9(9) BINARY VALUE 24.     
  +0100          10 MQ-CANCEL-CODE-LENGTH        PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-CHANNEL-DATE-LENGTH       PIC S9(9) BINARY VALUE 12.     
  +0100          10 MQ-CHANNEL-DESC-LENGTH       PIC S9(9) BINARY VALUE 64.     
  +0100          10 MQ-CHANNEL-NAME-LENGTH       PIC S9(9) BINARY VALUE 20.     
  +0100          10 MQ-CHANNEL-TIME-LENGTH       PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-CONN-NAME-LENGTH          PIC S9(9) BINARY VALUE 264.    
  +0100          10 MQ-CORREL-ID-LENGTH          PIC S9(9) BINARY VALUE 24.     
  +0100          10 MQ-CREATION-DATE-LENGTH      PIC S9(9) BINARY VALUE 12.     
  +0100          10 MQ-CREATION-TIME-LENGTH      PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-EXIT-DATA-LENGTH          PIC S9(9) BINARY VALUE 32.     
  +0100          10 MQ-EXIT-NAME-LENGTH          PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-EXIT-USER-AREA-LENGTH     PIC S9(9) BINARY VALUE 16.     
  +0100          10 MQ-FACILITY-LENGTH           PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-FACILITY-LIKE-LENGTH      PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-FORMAT-LENGTH             PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-FUNCTION-LENGTH           PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-GROUP-ID-LENGTH           PIC S9(9) BINARY VALUE 24.     
  +0100          10 MQ-LTERM-OVERRIDE-LENGTH     PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-LUWID-LENGTH              PIC S9(9) BINARY VALUE 16.     
  +0100          10 MQ-MCA-JOB-NAME-LENGTH       PIC S9(9) BINARY VALUE 28.     
  +0100          10 MQ-MCA-NAME-LENGTH           PIC S9(9) BINARY VALUE 20.     
  +0100          10 MQ-MCA-USER-DATA-LENGTH      PIC S9(9) BINARY VALUE 32.     
  +0100          10 MQ-MFS-MAP-NAME-LENGTH       PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-MODE-NAME-LENGTH          PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-MSG-HEADER-LENGTH         PIC S9(9) BINARY VALUE 4000.   
  +0100          10 MQ-MSG-ID-LENGTH             PIC S9(9) BINARY VALUE 24.     
  +0100          10 MQ-NAMELIST-DESC-LENGTH      PIC S9(9) BINARY VALUE 64.     
  +0100          10 MQ-NAMELIST-NAME-LENGTH      PIC S9(9) BINARY VALUE 48.     
  +0100          10 MQ-OBJECT-INSTANCE-ID-LENGTH PIC S9(9) BINARY VALUE 24.     
  +0100          10 MQ-PASSWORD-LENGTH           PIC S9(9) BINARY VALUE 12.     
  +0100          10 MQ-PROCESS-APPL-ID-LENGTH    PIC S9(9) BINARY VALUE 256.    
  +0100          10 MQ-PROCESS-DESC-LENGTH       PIC S9(9) BINARY VALUE 64.     
  +0100          10 MQ-PROCESS-ENV-DATA-LENGTH   PIC S9(9) BINARY VALUE 128.    
  +0100          10 MQ-PROCESS-NAME-LENGTH       PIC S9(9) BINARY VALUE 48.     
  +0100          10 MQ-PROCESS-USER-DATA-LENGTH  PIC S9(9) BINARY VALUE 128.    
  +0100          10 MQ-PROGRAM-NAME-LENGTH       PIC S9(9) BINARY VALUE 20.     
  +0100          10 MQ-PUT-APPL-NAME-LENGTH      PIC S9(9) BINARY VALUE 28.     
  +0100          10 MQ-PUT-DATE-LENGTH           PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-PUT-TIME-LENGTH           PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-Q-DESC-LENGTH             PIC S9(9) BINARY VALUE 64.     
  +0100          10 MQ-Q-NAME-LENGTH             PIC S9(9) BINARY VALUE 48.     
  +0100          10 MQ-Q-MGR-DESC-LENGTH         PIC S9(9) BINARY VALUE 64.     
  +0100          10 MQ-Q-MGR-NAME-LENGTH         PIC S9(9) BINARY VALUE 48.     
  +0100          10 MQ-REMOTE-SYS-ID-LENGTH      PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-SHORT-CONN-NAME-LENGTH    PIC S9(9) BINARY VALUE 20.     
  +0100          10 MQ-START-CODE-LENGTH         PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-STORAGE-CLASS-LENGTH      PIC S9(9) BINARY VALUE 8.      
  +0100          10 MQ-TOTAL-EXIT-DATA-LENGTH    PIC S9(9) BINARY VALUE 999.    
  +0100          10 MQ-TOTAL-EXIT-NAME-LENGTH    PIC S9(9) BINARY VALUE 999.    
  +0100          10 MQ-TP-NAME-LENGTH            PIC S9(9) BINARY VALUE 64.     
  +0100          10 MQ-TRAN-INSTANCE-ID-LENGTH   PIC S9(9) BINARY VALUE 16.     
  +0100          10 MQ-TRANSACTION-ID-LENGTH     PIC S9(9) BINARY VALUE 4.      
  +0100          10 MQ-TRIGGER-DATA-LENGTH       PIC S9(9) BINARY VALUE 64.     
  +0100          10 MQ-USER-ID-LENGTH            PIC S9(9) BINARY VALUE 12.     
  +0100                                                                         
  +0100        **   Completion Codes                                            
  +0100          10 MQCC-OK      PIC S9(9) BINARY VALUE 0.                      
  +0100          10 MQCC-WARNING PIC S9(9) BINARY VALUE 1.                      
  +0100          10 MQCC-FAILED  PIC S9(9) BINARY VALUE 2.                      
  +0100          10 MQCC-UNKNOWN PIC S9(9) BINARY VALUE -1.                     
  +0100                                                                         
  +0100        **   Reason Codes                                                
  +0100          10 MQRC-NONE                      PIC S9(9) BINARY VALUE 0.    
  +0100          10 MQRC-ALIAS-BASE-Q-TYPE-ERROR   PIC S9(9) BINARY VALUE 2001. 
  +0100          10 MQRC-ALREADY-CONNECTED         PIC S9(9) BINARY VALUE 2002. 
  +0100          10 MQRC-BACKED-OUT                PIC S9(9) BINARY VALUE 2003. 
  +0100          10 MQRC-BUFFER-ERROR              PIC S9(9) BINARY VALUE 2004. 
  +0100          10 MQRC-BUFFER-LENGTH-ERROR       PIC S9(9) BINARY VALUE 2005. 
  +0100          10 MQRC-CHAR-ATTR-LENGTH-ERROR    PIC S9(9) BINARY VALUE 2006. 
  +0100          10 MQRC-CHAR-ATTRS-ERROR          PIC S9(9) BINARY VALUE 2007. 
  +0100          10 MQRC-CHAR-ATTRS-TOO-SHORT      PIC S9(9) BINARY VALUE 2008. 
  +0100          10 MQRC-CONNECTION-BROKEN         PIC S9(9) BINARY VALUE 2009. 
  +0100          10 MQRC-DATA-LENGTH-ERROR         PIC S9(9) BINARY VALUE 2010. 
  +0100          10 MQRC-DYNAMIC-Q-NAME-ERROR      PIC S9(9) BINARY VALUE 2011. 
  +0100          10 MQRC-ENVIRONMENT-ERROR         PIC S9(9) BINARY VALUE 2012. 
  +0100          10 MQRC-EXPIRY-ERROR              PIC S9(9) BINARY VALUE 2013. 
  +0100          10 MQRC-FEEDBACK-ERROR            PIC S9(9) BINARY VALUE 2014. 
  +0100          10 MQRC-GET-INHIBITED             PIC S9(9) BINARY VALUE 2016. 
  +0100          10 MQRC-HANDLE-NOT-AVAILABLE      PIC S9(9) BINARY VALUE 2017. 
  +0100          10 MQRC-HCONN-ERROR               PIC S9(9) BINARY VALUE 2018. 
  +0100          10 MQRC-HOBJ-ERROR                PIC S9(9) BINARY VALUE 2019. 
  +0100          10 MQRC-INHIBIT-VALUE-ERROR       PIC S9(9) BINARY VALUE 2020. 
  +0100          10 MQRC-INT-ATTR-COUNT-ERROR      PIC S9(9) BINARY VALUE 2021. 
  +0100          10 MQRC-INT-ATTR-COUNT-TOO-SMALL  PIC S9(9) BINARY VALUE 2022. 
  +0100          10 MQRC-INT-ATTRS-ARRAY-ERROR     PIC S9(9) BINARY VALUE 2023. 
  +0100          10 MQRC-SYNCPOINT-LIMIT-REACHED   PIC S9(9) BINARY VALUE 2024. 
  +0100          10 MQRC-MAX-CONNS-LIMIT-REACHED   PIC S9(9) BINARY VALUE 2025. 
  +0100          10 MQRC-MD-ERROR                  PIC S9(9) BINARY VALUE 2026. 
  +0100          10 MQRC-MISSING-REPLY-TO-Q        PIC S9(9) BINARY VALUE 2027. 
  +0100          10 MQRC-MSG-TYPE-ERROR            PIC S9(9) BINARY VALUE 2029. 
  +0100          10 MQRC-MSG-TOO-BIG-FOR-Q         PIC S9(9) BINARY VALUE 2030. 
  +0100          10 MQRC-MSG-TOO-BIG-FOR-Q-MGR     PIC S9(9) BINARY VALUE 2031. 
  +0100          10 MQRC-NO-MSG-AVAILABLE          PIC S9(9) BINARY VALUE 2033. 
  +0100          10 MQRC-NO-MSG-UNDER-CURSOR       PIC S9(9) BINARY VALUE 2034. 
  +0100          10 MQRC-NOT-AUTHORIZED            PIC S9(9) BINARY VALUE 2035. 
  +0100          10 MQRC-NOT-OPEN-FOR-BROWSE       PIC S9(9) BINARY VALUE 2036. 
  +0100          10 MQRC-NOT-OPEN-FOR-INPUT        PIC S9(9) BINARY VALUE 2037. 
  +0100          10 MQRC-NOT-OPEN-FOR-INQUIRE      PIC S9(9) BINARY VALUE 2038. 
  +0100          10 MQRC-NOT-OPEN-FOR-OUTPUT       PIC S9(9) BINARY VALUE 2039. 
  +0100          10 MQRC-NOT-OPEN-FOR-SET          PIC S9(9) BINARY VALUE 2040. 
  +0100          10 MQRC-OBJECT-CHANGED            PIC S9(9) BINARY VALUE 2041. 
  +0100          10 MQRC-OBJECT-IN-USE             PIC S9(9) BINARY VALUE 2042. 
  +0100          10 MQRC-OBJECT-TYPE-ERROR         PIC S9(9) BINARY VALUE 2043. 
  +0100          10 MQRC-OD-ERROR                  PIC S9(9) BINARY VALUE 2044. 
  +0100          10 MQRC-OPTION-NOT-VALID-FOR-TYPE PIC S9(9) BINARY VALUE 2045. 
  +0100          10 MQRC-OPTIONS-ERROR             PIC S9(9) BINARY VALUE 2046. 
  +0100          10 MQRC-PERSISTENCE-ERROR         PIC S9(9) BINARY VALUE 2047. 
  +0100          10 MQRC-PERSISTENT-NOT-ALLOWED    PIC S9(9) BINARY VALUE 2048. 
  +0100          10 MQRC-PRIORITY-EXCEEDS-MAXIMUM  PIC S9(9) BINARY VALUE 2049. 
  +0100          10 MQRC-PRIORITY-ERROR            PIC S9(9) BINARY VALUE 2050. 
  +0100          10 MQRC-PUT-INHIBITED             PIC S9(9) BINARY VALUE 2051. 
  +0100          10 MQRC-Q-DELETED                 PIC S9(9) BINARY VALUE 2052. 
  +0100          10 MQRC-Q-FULL                    PIC S9(9) BINARY VALUE 2053. 
  +0100          10 MQRC-Q-NOT-EMPTY               PIC S9(9) BINARY VALUE 2055. 
  +0100          10 MQRC-Q-SPACE-NOT-AVAILABLE     PIC S9(9) BINARY VALUE 2056. 
  +0100          10 MQRC-Q-TYPE-ERROR              PIC S9(9) BINARY VALUE 2057. 
  +0100          10 MQRC-Q-MGR-NAME-ERROR          PIC S9(9) BINARY VALUE 2058. 
  +0100          10 MQRC-Q-MGR-NOT-AVAILABLE       PIC S9(9) BINARY VALUE 2059. 
  +0100          10 MQRC-REPORT-OPTIONS-ERROR      PIC S9(9) BINARY VALUE 2061. 
  +0100          10 MQRC-SECOND-MARK-NOT-ALLOWED   PIC S9(9) BINARY VALUE 2062. 
  +0100          10 MQRC-SECURITY-ERROR            PIC S9(9) BINARY VALUE 2063. 
  +0100          10 MQRC-SELECTOR-COUNT-ERROR      PIC S9(9) BINARY VALUE 2065. 
  +0100          10 MQRC-SELECTOR-LIMIT-EXCEEDED   PIC S9(9) BINARY VALUE 2066. 
  +0100          10 MQRC-SELECTOR-ERROR            PIC S9(9) BINARY VALUE 2067. 
  +0100          10 MQRC-SELECTOR-NOT-FOR-TYPE     PIC S9(9) BINARY VALUE 2068. 
  +0100          10 MQRC-SIGNAL-OUTSTANDING        PIC S9(9) BINARY VALUE 2069. 
  +0100          10 MQRC-SIGNAL-REQUEST-ACCEPTED   PIC S9(9) BINARY VALUE 2070. 
  +0100          10 MQRC-STORAGE-NOT-AVAILABLE     PIC S9(9) BINARY VALUE 2071. 
  +0100          10 MQRC-SYNCPOINT-NOT-AVAILABLE   PIC S9(9) BINARY VALUE 2072. 
  +0100          10 MQRC-TRIGGER-CONTROL-ERROR     PIC S9(9) BINARY VALUE 2075. 
  +0100          10 MQRC-TRIGGER-DEPTH-ERROR       PIC S9(9) BINARY VALUE 2076. 
  +0100          10 MQRC-TRIGGER-MSG-PRIORITY-ERR  PIC S9(9) BINARY VALUE 2077. 
  +0100          10 MQRC-TRIGGER-TYPE-ERROR        PIC S9(9) BINARY VALUE 2078. 
  +0100          10 MQRC-TRUNCATED-MSG-ACCEPTED    PIC S9(9) BINARY VALUE 2079. 
  +0100          10 MQRC-TRUNCATED-MSG-FAILED      PIC S9(9) BINARY VALUE 2080. 
  +0100          10 MQRC-UNKNOWN-ALIAS-BASE-Q      PIC S9(9) BINARY VALUE 2082. 
  +0100          10 MQRC-UNKNOWN-OBJECT-NAME       PIC S9(9) BINARY VALUE 2085. 
  +0100          10 MQRC-UNKNOWN-OBJECT-Q-MGR      PIC S9(9) BINARY VALUE 2086. 
  +0100          10 MQRC-UNKNOWN-REMOTE-Q-MGR      PIC S9(9) BINARY VALUE 2087. 
  +0100          10 MQRC-WAIT-INTERVAL-ERROR       PIC S9(9) BINARY VALUE 2090. 
  +0100          10 MQRC-XMIT-Q-TYPE-ERROR         PIC S9(9) BINARY VALUE 2091. 
  +0100          10 MQRC-XMIT-Q-USAGE-ERROR        PIC S9(9) BINARY VALUE 2092. 
  +0100          10 MQRC-NOT-OPEN-FOR-PASS-ALL     PIC S9(9) BINARY VALUE 2093. 
  +0100          10 MQRC-NOT-OPEN-FOR-PASS-IDENT   PIC S9(9) BINARY VALUE 2094. 
  +0100          10 MQRC-NOT-OPEN-FOR-SET-ALL      PIC S9(9) BINARY VALUE 2095. 
  +0100          10 MQRC-NOT-OPEN-FOR-SET-IDENT    PIC S9(9) BINARY VALUE 2096. 
  +0100          10 MQRC-CONTEXT-HANDLE-ERROR      PIC S9(9) BINARY VALUE 2097. 
  +0100          10 MQRC-CONTEXT-NOT-AVAILABLE     PIC S9(9) BINARY VALUE 2098. 
  +0100          10 MQRC-SIGNAL1-ERROR             PIC S9(9) BINARY VALUE 2099. 
  +0100          10 MQRC-OBJECT-ALREADY-EXISTS     PIC S9(9) BINARY VALUE 2100. 
  +0100          10 MQRC-OBJECT-DAMAGED            PIC S9(9) BINARY VALUE 2101. 
  +0100          10 MQRC-RESOURCE-PROBLEM          PIC S9(9) BINARY VALUE 2102. 
  +0100          10 MQRC-ANOTHER-Q-MGR-CONNECTED   PIC S9(9) BINARY VALUE 2103. 
  +0100          10 MQRC-UNKNOWN-REPORT-OPTION     PIC S9(9) BINARY VALUE 2104. 
  +0100          10 MQRC-STORAGE-CLASS-ERROR       PIC S9(9) BINARY VALUE 2105. 
  +0100          10 MQRC-COD-NOT-VALID-FOR-XCF-Q   PIC S9(9) BINARY VALUE 2106. 
  +0100          10 MQRC-SUPPRESSED-BY-EXIT        PIC S9(9) BINARY VALUE 2109. 
  +0100          10 MQRC-FORMAT-ERROR              PIC S9(9) BINARY VALUE 2110. 
  +0100          10 MQRC-SOURCE-CCSID-ERROR        PIC S9(9) BINARY VALUE 2111. 
  +0100          10 MQRC-SOURCE-INTEGER-ENC-ERROR  PIC S9(9) BINARY VALUE 2112. 
  +0100          10 MQRC-SOURCE-DECIMAL-ENC-ERROR  PIC S9(9) BINARY VALUE 2113. 
  +0100          10 MQRC-SOURCE-FLOAT-ENC-ERROR    PIC S9(9) BINARY VALUE 2114. 
  +0100          10 MQRC-TARGET-CCSID-ERROR        PIC S9(9) BINARY VALUE 2115. 
  +0100          10 MQRC-TARGET-INTEGER-ENC-ERROR  PIC S9(9) BINARY VALUE 2116. 
  +0100          10 MQRC-TARGET-DECIMAL-ENC-ERROR  PIC S9(9) BINARY VALUE 2117. 
  +0100          10 MQRC-TARGET-FLOAT-ENC-ERROR    PIC S9(9) BINARY VALUE 2118. 
  +0100          10 MQRC-NOT-CONVERTED             PIC S9(9) BINARY VALUE 2119. 
  +0100          10 MQRC-CONVERTED-MSG-TOO-BIG     PIC S9(9) BINARY VALUE 2120. 
  +0100          10 MQRC-TRUNCATED                 PIC S9(9) BINARY VALUE 2120. 
  +0100          10 MQRC-NO-EXTERNAL-PARTICIPANTS  PIC S9(9) BINARY VALUE 2121. 
  +0100          10 MQRC-PARTICIPANT-NOT-AVAILABLE PIC S9(9) BINARY VALUE 2122. 
  +0100          10 MQRC-OUTCOME-MIXED             PIC S9(9) BINARY VALUE 2123. 
  +0100          10 MQRC-OUTCOME-PENDING           PIC S9(9) BINARY VALUE 2124. 
  +0100          10 MQRC-BRIDGE-STARTED            PIC S9(9) BINARY VALUE 2125. 
  +0100          10 MQRC-BRIDGE-STOPPED            PIC S9(9) BINARY VALUE 2126. 
  +0100          10 MQRC-ADAPTER-STORAGE-SHORTAGE  PIC S9(9) BINARY VALUE 2127. 
  +0100          10 MQRC-UOW-IN-PROGRESS           PIC S9(9) BINARY VALUE 2128. 
  +0100          10 MQRC-ADAPTER-CONN-LOAD-ERROR   PIC S9(9) BINARY VALUE 2129. 
  +0100          10 MQRC-ADAPTER-SERV-LOAD-ERROR   PIC S9(9) BINARY VALUE 2130. 
  +0100          10 MQRC-ADAPTER-DEFS-ERROR        PIC S9(9) BINARY VALUE 2131. 
  +0100          10 MQRC-ADAPTER-DEFS-LOAD-ERROR   PIC S9(9) BINARY VALUE 2132. 
  +0100          10 MQRC-ADAPTER-CONV-LOAD-ERROR   PIC S9(9) BINARY VALUE 2133. 
  +0100          10 MQRC-BO-ERROR                  PIC S9(9) BINARY VALUE 2134. 
  +0100          10 MQRC-DH-ERROR                  PIC S9(9) BINARY VALUE 2135. 
  +0100          10 MQRC-MULTIPLE-REASONS          PIC S9(9) BINARY VALUE 2136. 
  +0100          10 MQRC-OPEN-FAILED               PIC S9(9) BINARY VALUE 2137. 
  +0100          10 MQRC-ADAPTER-DISC-LOAD-ERROR   PIC S9(9) BINARY VALUE 2138. 
  +0100          10 MQRC-CNO-ERROR                 PIC S9(9) BINARY VALUE 2139. 
  +0100          10 MQRC-CICS-WAIT-FAILED          PIC S9(9) BINARY VALUE 2140. 
  +0100          10 MQRC-DLH-ERROR                 PIC S9(9) BINARY VALUE 2141. 
  +0100          10 MQRC-HEADER-ERROR              PIC S9(9) BINARY VALUE 2142. 
  +0100          10 MQRC-SOURCE-LENGTH-ERROR       PIC S9(9) BINARY VALUE 2143. 
  +0100          10 MQRC-TARGET-LENGTH-ERROR       PIC S9(9) BINARY VALUE 2144. 
  +0100          10 MQRC-SOURCE-BUFFER-ERROR       PIC S9(9) BINARY VALUE 2145. 
  +0100          10 MQRC-TARGET-BUFFER-ERROR       PIC S9(9) BINARY VALUE 2146. 
  +0100          10 MQRC-IIH-ERROR                 PIC S9(9) BINARY VALUE 2148. 
  +0100          10 MQRC-PCF-ERROR                 PIC S9(9) BINARY VALUE 2149. 
  +0100          10 MQRC-DBCS-ERROR                PIC S9(9) BINARY VALUE 2150. 
  +0100          10 MQRC-OBJECT-NAME-ERROR         PIC S9(9) BINARY VALUE 2152. 
  +0100          10 MQRC-OBJECT-Q-MGR-NAME-ERROR   PIC S9(9) BINARY VALUE 2153. 
  +0100          10 MQRC-RECS-PRESENT-ERROR        PIC S9(9) BINARY VALUE 2154. 
  +0100          10 MQRC-OBJECT-RECORDS-ERROR      PIC S9(9) BINARY VALUE 2155. 
  +0100          10 MQRC-RESPONSE-RECORDS-ERROR    PIC S9(9) BINARY VALUE 2156. 
  +0100          10 MQRC-ASID-MISMATCH             PIC S9(9) BINARY VALUE 2157. 
  +0100          10 MQRC-PMO-RECORD-FLAGS-ERROR    PIC S9(9) BINARY VALUE 2158. 
  +0100          10 MQRC-PUT-MSG-RECORDS-ERROR     PIC S9(9) BINARY VALUE 2159. 
  +0100          10 MQRC-CONN-ID-IN-USE            PIC S9(9) BINARY VALUE 2160. 
  +0100          10 MQRC-Q-MGR-QUIESCING           PIC S9(9) BINARY VALUE 2161. 
  +0100          10 MQRC-Q-MGR-STOPPING            PIC S9(9) BINARY VALUE 2162. 
  +0100          10 MQRC-DUPLICATE-RECOV-COORD     PIC S9(9) BINARY VALUE 2163. 
  +0100          10 MQRC-PMO-ERROR                 PIC S9(9) BINARY VALUE 2173. 
  +0100          10 MQRC-API-EXIT-NOT-FOUND        PIC S9(9) BINARY VALUE 2182. 
  +0100          10 MQRC-API-EXIT-LOAD-ERROR       PIC S9(9) BINARY VALUE 2183. 
  +0100          10 MQRC-REMOTE-Q-NAME-ERROR       PIC S9(9) BINARY VALUE 2184. 
  +0100          10 MQRC-INCONSISTENT-PERSISTENCE  PIC S9(9) BINARY VALUE 2185. 
  +0100          10 MQRC-GMO-ERROR                 PIC S9(9) BINARY VALUE 2186. 
  +0100          10 MQRC-CICS-BRIDGE-RESTRICTION   PIC S9(9) BINARY VALUE 2187. 
  +0100          10 MQRC-TMC-ERROR                 PIC S9(9) BINARY VALUE 2191. 
  +0100          10 MQRC-PAGESET-FULL              PIC S9(9) BINARY VALUE 2192. 
  +0100          10 MQRC-PAGESET-ERROR             PIC S9(9) BINARY VALUE 2193. 
  +0100          10 MQRC-NAME-NOT-VALID-FOR-TYPE   PIC S9(9) BINARY VALUE 2194. 
  +0100          10 MQRC-UNEXPECTED-ERROR          PIC S9(9) BINARY VALUE 2195. 
  +0100          10 MQRC-UNKNOWN-XMIT-Q            PIC S9(9) BINARY VALUE 2196. 
  +0100          10 MQRC-UNKNOWN-DEF-XMIT-Q        PIC S9(9) BINARY VALUE 2197. 
  +0100          10 MQRC-DEF-XMIT-Q-TYPE-ERROR     PIC S9(9) BINARY VALUE 2198. 
  +0100          10 MQRC-DEF-XMIT-Q-USAGE-ERROR    PIC S9(9) BINARY VALUE 2199. 
  +0100          10 MQRC-NAME-IN-USE               PIC S9(9) BINARY VALUE 2201. 
  +0100          10 MQRC-CONNECTION-QUIESCING      PIC S9(9) BINARY VALUE 2202. 
  +0100          10 MQRC-CONNECTION-STOPPING       PIC S9(9) BINARY VALUE 2203. 
  +0100          10 MQRC-ADAPTER-NOT-AVAILABLE     PIC S9(9) BINARY VALUE 2204. 
  +0100          10 MQRC-MSG-ID-ERROR              PIC S9(9) BINARY VALUE 2206. 
  +0100          10 MQRC-CORREL-ID-ERROR           PIC S9(9) BINARY VALUE 2207. 
  +0100          10 MQRC-FILE-SYSTEM-ERROR         PIC S9(9) BINARY VALUE 2208. 
  +0100          10 MQRC-NO-MSG-LOCKED             PIC S9(9) BINARY VALUE 2209. 
  +0100          10 MQRC-FILE-NOT-AUDITED          PIC S9(9) BINARY VALUE 2216. 
  +0100          10 MQRC-CONNECTION-NOT-AUTHORIZED PIC S9(9) BINARY VALUE 2217. 
  +0100          10 MQRC-MSG-TOO-BIG-FOR-CHANNEL   PIC S9(9) BINARY VALUE 2218. 
  +0100          10 MQRC-CALL-IN-PROGRESS          PIC S9(9) BINARY VALUE 2219. 
  +0100          10 MQRC-RMH-ERROR                 PIC S9(9) BINARY VALUE 2220. 
  +0100          10 MQRC-Q-MGR-ACTIVE              PIC S9(9) BINARY VALUE 2222. 
  +0100          10 MQRC-Q-MGR-NOT-ACTIVE          PIC S9(9) BINARY VALUE 2223. 
  +0100          10 MQRC-Q-DEPTH-HIGH              PIC S9(9) BINARY VALUE 2224. 
  +0100          10 MQRC-Q-DEPTH-LOW               PIC S9(9) BINARY VALUE 2225. 
  +0100          10 MQRC-Q-SERVICE-INTERVAL-HIGH   PIC S9(9) BINARY VALUE 2226. 
  +0100          10 MQRC-Q-SERVICE-INTERVAL-OK     PIC S9(9) BINARY VALUE 2227. 
  +0100          10 MQRC-UNIT-OF-WORK-NOT-STARTED  PIC S9(9) BINARY VALUE 2232. 
  +0100          10 MQRC-CHANNEL-AUTO-DEF-OK       PIC S9(9) BINARY VALUE 2233. 
  +0100          10 MQRC-CHANNEL-AUTO-DEF-ERROR    PIC S9(9) BINARY VALUE 2234. 
  +0100          10 MQRC-CFH-ERROR                 PIC S9(9) BINARY VALUE 2235. 
  +0100          10 MQRC-CFIL-ERROR                PIC S9(9) BINARY VALUE 2236. 
  +0100          10 MQRC-CFIN-ERROR                PIC S9(9) BINARY VALUE 2237. 
  +0100          10 MQRC-CFSL-ERROR                PIC S9(9) BINARY VALUE 2238. 
  +0100          10 MQRC-CFST-ERROR                PIC S9(9) BINARY VALUE 2239. 
  +0100          10 MQRC-INCOMPLETE-GROUP          PIC S9(9) BINARY VALUE 2241. 
  +0100          10 MQRC-INCOMPLETE-MSG            PIC S9(9) BINARY VALUE 2242. 
  +0100          10 MQRC-INCONSISTENT-CCSIDS       PIC S9(9) BINARY VALUE 2243. 
  +0100          10 MQRC-INCONSISTENT-ENCODINGS    PIC S9(9) BINARY VALUE 2244. 
  +0100          10 MQRC-INCONSISTENT-UOW          PIC S9(9) BINARY VALUE 2245. 
  +0100          10 MQRC-INVALID-MSG-UNDER-CURSOR  PIC S9(9) BINARY VALUE 2246. 
  +0100          10 MQRC-MATCH-OPTIONS-ERROR       PIC S9(9) BINARY VALUE 2247. 
  +0100          10 MQRC-MDE-ERROR                 PIC S9(9) BINARY VALUE 2248. 
  +0100          10 MQRC-MSG-FLAGS-ERROR           PIC S9(9) BINARY VALUE 2249. 
  +0100          10 MQRC-MSG-SEQ-NUMBER-ERROR      PIC S9(9) BINARY VALUE 2250. 
  +0100          10 MQRC-OFFSET-ERROR              PIC S9(9) BINARY VALUE 2251. 
  +0100          10 MQRC-ORIGINAL-LENGTH-ERROR     PIC S9(9) BINARY VALUE 2252. 
  +0100          10 MQRC-SEGMENT-LENGTH-ZERO       PIC S9(9) BINARY VALUE 2253. 
  +0100          10 MQRC-UOW-NOT-AVAILABLE         PIC S9(9) BINARY VALUE 2255. 
  +0100          10 MQRC-WRONG-GMO-VERSION         PIC S9(9) BINARY VALUE 2256. 
  +0100          10 MQRC-WRONG-MD-VERSION          PIC S9(9) BINARY VALUE 2257. 
  +0100          10 MQRC-GROUP-ID-ERROR            PIC S9(9) BINARY VALUE 2258. 
  +0100          10 MQRC-INCONSISTENT-BROWSE       PIC S9(9) BINARY VALUE 2259. 
  +0100          10 MQRC-XQH-ERROR                 PIC S9(9) BINARY VALUE 2260. 
  +0100          10 MQRC-SRC-ENV-ERROR             PIC S9(9) BINARY VALUE 2261. 
  +0100          10 MQRC-SRC-NAME-ERROR            PIC S9(9) BINARY VALUE 2262. 
  +0100          10 MQRC-DEST-ENV-ERROR            PIC S9(9) BINARY VALUE 2263. 
  +0100          10 MQRC-DEST-NAME-ERROR           PIC S9(9) BINARY VALUE 2264. 
  +0100          10 MQRC-TM-ERROR                  PIC S9(9) BINARY VALUE 2265. 
  +0100          10 MQRC-CHANNEL-STOPPED-BY-USER   PIC S9(9) BINARY VALUE 2279. 
  +0100          10 MQRC-HCONFIG-ERROR             PIC S9(9) BINARY VALUE 2280. 
  +0100          10 MQRC-FUNCTION-ERROR            PIC S9(9) BINARY VALUE 2281. 
  +0100          10 MQRC-CHANNEL-STARTED           PIC S9(9) BINARY VALUE 2282. 
  +0100          10 MQRC-CHANNEL-STOPPED           PIC S9(9) BINARY VALUE 2283. 
  +0100          10 MQRC-CHANNEL-CONV-ERROR        PIC S9(9) BINARY VALUE 2284. 
  +0100          10 MQRC-SERVICE-NOT-AVAILABLE     PIC S9(9) BINARY VALUE 2285. 
  +0100          10 MQRC-INITIALIZATION-FAILED     PIC S9(9) BINARY VALUE 2286. 
  +0100          10 MQRC-TERMINATION-FAILED        PIC S9(9) BINARY VALUE 2287. 
  +0100          10 MQRC-UNKNOWN-Q-NAME            PIC S9(9) BINARY VALUE 2288. 
  +0100          10 MQRC-SERVICE-ERROR             PIC S9(9) BINARY VALUE 2289. 
  +0100          10 MQRC-Q-ALREADY-EXISTS          PIC S9(9) BINARY VALUE 2290. 
  +0100          10 MQRC-USER-ID-NOT-AVAILABLE     PIC S9(9) BINARY VALUE 2291. 
  +0100          10 MQRC-UNKNOWN-ENTITY            PIC S9(9) BINARY VALUE 2292. 
  +0100          10 MQRC-UNKNOWN-AUTH-ENTITY       PIC S9(9) BINARY VALUE 2293. 
  +0100          10 MQRC-UNKNOWN-REF-OBJECT        PIC S9(9) BINARY VALUE 2294. 
  +0100          10 MQRC-CHANNEL-ACTIVATED         PIC S9(9) BINARY VALUE 2295. 
  +0100          10 MQRC-CHANNEL-NOT-ACTIVATED     PIC S9(9) BINARY VALUE 2296. 
  +0100          10 MQRC-UOW-CANCELED              PIC S9(9) BINARY VALUE 2297. 
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to Queue Attributes                          *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Queue Types                                                 
  +0100          10 MQQT-LOCAL  PIC S9(9) BINARY VALUE 1.                       
  +0100          10 MQQT-MODEL  PIC S9(9) BINARY VALUE 2.                       
  +0100          10 MQQT-ALIAS  PIC S9(9) BINARY VALUE 3.                       
  +0100          10 MQQT-REMOTE PIC S9(9) BINARY VALUE 6.                       
  +0100                                                                         
  +0100        **   Extended Queue Types                                        
  +0100          10 MQQT-ALL PIC S9(9) BINARY VALUE 1001.                       
  +0100                                                                         
  +0100        **   Queue Definition Types                                      
  +0100          10 MQQDT-PREDEFINED        PIC S9(9) BINARY VALUE 1.           
  +0100          10 MQQDT-PERMANENT-DYNAMIC PIC S9(9) BINARY VALUE 2.           
  +0100          10 MQQDT-TEMPORARY-DYNAMIC PIC S9(9) BINARY VALUE 3.           
  +0100                                                                         
  +0100        **   Inhibit Get                                                 
  +0100          10 MQQA-GET-INHIBITED PIC S9(9) BINARY VALUE 1.                
  +0100          10 MQQA-GET-ALLOWED   PIC S9(9) BINARY VALUE 0.                
  +0100                                                                         
  +0100        **   Inhibit Put                                                 
  +0100          10 MQQA-PUT-INHIBITED PIC S9(9) BINARY VALUE 1.                
  +0100          10 MQQA-PUT-ALLOWED   PIC S9(9) BINARY VALUE 0.                
  +0100                                                                         
  +0100        **   Queue Shareability                                          
  +0100          10 MQQA-SHAREABLE     PIC S9(9) BINARY VALUE 1.                
  +0100          10 MQQA-NOT-SHAREABLE PIC S9(9) BINARY VALUE 0.                
  +0100                                                                         
  +0100        **   Back-Out Hardening                                          
  +0100          10 MQQA-BACKOUT-HARDENED     PIC S9(9) BINARY VALUE 1.         
  +0100          10 MQQA-BACKOUT-NOT-HARDENED PIC S9(9) BINARY VALUE 0.         
  +0100                                                                         
  +0100        **   Message Delivery Sequence                                   
  +0100          10 MQMDS-PRIORITY PIC S9(9) BINARY VALUE 0.                    
  +0100          10 MQMDS-FIFO     PIC S9(9) BINARY VALUE 1.                    
  +0100                                                                         
  +0100        **   Trigger Control                                             
  +0100          10 MQTC-OFF PIC S9(9) BINARY VALUE 0.                          
  +0100          10 MQTC-ON  PIC S9(9) BINARY VALUE 1.                          
  +0100                                                                         
  +0100        **   Trigger Types                                               
  +0100          10 MQTT-NONE  PIC S9(9) BINARY VALUE 0.                        
  +0100          10 MQTT-FIRST PIC S9(9) BINARY VALUE 1.                        
  +0100          10 MQTT-EVERY PIC S9(9) BINARY VALUE 2.                        
  +0100          10 MQTT-DEPTH PIC S9(9) BINARY VALUE 3.                        
  +0100                                                                         
  +0100        **   Queue Usage                                                 
  +0100          10 MQUS-NORMAL       PIC S9(9) BINARY VALUE 0.                 
  +0100          10 MQUS-TRANSMISSION PIC S9(9) BINARY VALUE 1.                 
  +0100                                                                         
  +0100        **   Distribution Lists                                          
  +0100          10 MQDL-SUPPORTED     PIC S9(9) BINARY VALUE 1.                
  +0100          10 MQDL-NOT-SUPPORTED PIC S9(9) BINARY VALUE 0.                
  +0100                                                                         
  +0100        **   Index Type                                                  
  +0100          10 MQIT-NONE      PIC S9(9) BINARY VALUE 0.                    
  +0100          10 MQIT-MSG-ID    PIC S9(9) BINARY VALUE 1.                    
  +0100          10 MQIT-CORREL-ID PIC S9(9) BINARY VALUE 2.                    
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to Process-Definition Attributes             *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Application Types                                           
  +0100        **   See values for "Put Application Types" under MQMD           
  +0100                                                                         
  +0100                                                                         
  +0100        *****************************************************************
  +0100        **  Values Related to Queue-Manager Attributes                  *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   Command Level                                               
  +0100          10 MQCMDL-LEVEL-1   PIC S9(9) BINARY VALUE 100.                
  +0100          10 MQCMDL-LEVEL-101 PIC S9(9) BINARY VALUE 101.                
  +0100          10 MQCMDL-LEVEL-110 PIC S9(9) BINARY VALUE 110.                
  +0100          10 MQCMDL-LEVEL-114 PIC S9(9) BINARY VALUE 114.                
  +0100          10 MQCMDL-LEVEL-120 PIC S9(9) BINARY VALUE 120.                
  +0100          10 MQCMDL-LEVEL-200 PIC S9(9) BINARY VALUE 200.                
  +0100          10 MQCMDL-LEVEL-201 PIC S9(9) BINARY VALUE 201.                
  +0100          10 MQCMDL-LEVEL-221 PIC S9(9) BINARY VALUE 221.                
  +0100          10 MQCMDL-LEVEL-320 PIC S9(9) BINARY VALUE 320.                
  +0100          10 MQCMDL-LEVEL-500 PIC S9(9) BINARY VALUE 500.                
  +0100                                                                         
  +0100        **   Platform                                                    
  +0100          10 MQPL-MVS        PIC S9(9) BINARY VALUE 1.                   
  +0100          10 MQPL-OS2        PIC S9(9) BINARY VALUE 2.                   
  +0100          10 MQPL-AIX        PIC S9(9) BINARY VALUE 3.                   
  +0100          10 MQPL-UNIX       PIC S9(9) BINARY VALUE 3.                   
  +0100          10 MQPL-OS400      PIC S9(9) BINARY VALUE 4.                   
  +0100          10 MQPL-WINDOWS    PIC S9(9) BINARY VALUE 5.                   
  +0100          10 MQPL-WINDOWS-NT PIC S9(9) BINARY VALUE 11.                  
  +0100          10 MQPL-NATIVE     PIC S9(9) BINARY VALUE 1.                   
  +0100                                                                         
  +0100        **   Syncpoint Availability                                      
  +0100          10 MQSP-AVAILABLE     PIC S9(9) BINARY VALUE 1.                
  +0100          10 MQSP-NOT-AVAILABLE PIC S9(9) BINARY VALUE 0.                
  +0100                                                                         
  +0100        **   Channel Auto Definition                                     
  +0100          10 MQCHAD-DISABLED PIC S9(9) BINARY VALUE 0.                   
  +0100          10 MQCHAD-ENABLED  PIC S9(9) BINARY VALUE 1.                   
  +0100                                                                         
  +0100        **   Distribution Lists                                          
  +0100        **   See values for "Distribution Lists" under Queue Attributes  
  +0100                                                                         
