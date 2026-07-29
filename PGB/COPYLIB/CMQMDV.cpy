1*******************************************************************************
 *******************************************************************************
 **                                                                           **
 ** ELEMENT BROWSE                                            14NOV25  14:51  **
 **                                                                           **
 **    ENVIRONMENT:   MVSPROD    SYSTEM:    CJ        SUBSYSTEM:  STANDARD    **
 **    TYPE:          COPYBOOK   STAGE ID:  P                                 **
 **    ELEMENT:       CMQMDV                                                  **
 **                                                                           **
 **                                                   DELTA TYPE: REVERSE     **
 **                                                                           **
1*******************************************************************************
 *******************************************************************************
                                                                                
 -------------------------- SOURCE LEVEL INFORMATION ---------------------------
                                                                                
1  VVLL SYNC USER     DATE    TIME     STMTS CCID         COMMENT               
   ---- ---- -------- ------- ----- -------- ------------ ----------------------
   0100      DHARKOU  10DEC12 11:08       75 ILCTMMC500KD ADD COPYBOOK TO D1    
 GENERATED   PPCNDEV  18MAR13 16:55          ILCJMMC500KD MOVE COPYBOOK TO PROD 
                                                                                
  +0100        *****************************************************************
  +0100        **                                                              *
  +0100        **  FILE NAME:        CMQMDV                                    *
  +0100        **                                                              *
  +0100        **  DESCRIPTIVE NAME: COBOL copy file for MQMD structure        *
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
  +0100        **  FUNCTION:         This file declares the MQMD structure,    *
  +0100        **                    which is part of the IBM Message Queue    *
  +0100        **                    Interface (MQI).                          *
  +0100        **                                                              *
  +0100        **  ENVIRONMENT:      MVS/ESA                                   *
  +0100        **                                                              *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   MQMD structure                                              
  +0100          10 MQMD.                                                       
  +0100        **    Structure identifier                                       
  +0100           15 MQMD-STRUCID          PIC X(4) VALUE 'MD  '.               
  +0100        **    Structure version number                                   
  +0100           15 MQMD-VERSION          PIC S9(9) BINARY VALUE 1.            
  +0100        **    Report options                                             
  +0100           15 MQMD-REPORT           PIC S9(9) BINARY VALUE 0.            
  +0100        **    Message type                                               
  +0100           15 MQMD-MSGTYPE          PIC S9(9) BINARY VALUE 8.            
  +0100        **    Expiry time                                                
  +0100           15 MQMD-EXPIRY           PIC S9(9) BINARY VALUE -1.           
  +0100        **    Feedback or reason code                                    
  +0100           15 MQMD-FEEDBACK         PIC S9(9) BINARY VALUE 0.            
  +0100        **    Data encoding                                              
  +0100           15 MQMD-ENCODING         PIC S9(9) BINARY VALUE 785.          
  +0100        **    Coded character set identifier                             
  +0100           15 MQMD-CODEDCHARSETID   PIC S9(9) BINARY VALUE 0.            
  +0100        **    Format name                                                
  +0100           15 MQMD-FORMAT           PIC X(8) VALUE SPACES.               
  +0100        **    Message priority                                           
  +0100           15 MQMD-PRIORITY         PIC S9(9) BINARY VALUE -1.           
  +0100        **    Message persistence                                        
  +0100           15 MQMD-PERSISTENCE      PIC S9(9) BINARY VALUE 2.            
  +0100        **    Message identifier                                         
  +0100           15 MQMD-MSGID            PIC X(24) VALUE LOW-VALUES.          
  +0100        **    Correlation identifier                                     
  +0100           15 MQMD-CORRELID         PIC X(24) VALUE LOW-VALUES.          
  +0100        **    Backout counter                                            
  +0100           15 MQMD-BACKOUTCOUNT     PIC S9(9) BINARY VALUE 0.            
  +0100        **    Name of reply-to queue                                     
  +0100           15 MQMD-REPLYTOQ         PIC X(48) VALUE SPACES.              
  +0100        **    Name of reply queue manager                                
  +0100           15 MQMD-REPLYTOQMGR      PIC X(48) VALUE SPACES.              
  +0100        **    User identifier                                            
  +0100           15 MQMD-USERIDENTIFIER   PIC X(12) VALUE SPACES.              
  +0100        **    Accounting token                                           
  +0100           15 MQMD-ACCOUNTINGTOKEN  PIC X(32) VALUE LOW-VALUES.          
  +0100        **    Application data relating to identity                      
  +0100           15 MQMD-APPLIDENTITYDATA PIC X(32) VALUE SPACES.              
  +0100        **    Type of application that put the message                   
  +0100           15 MQMD-PUTAPPLTYPE      PIC S9(9) BINARY VALUE 0.            
  +0100        **    Name of application that put the message                   
  +0100           15 MQMD-PUTAPPLNAME      PIC X(28) VALUE SPACES.              
  +0100        **    Date when message was put                                  
  +0100           15 MQMD-PUTDATE          PIC X(8) VALUE SPACES.               
  +0100        **    Time when message was put                                  
  +0100           15 MQMD-PUTTIME          PIC X(8) VALUE SPACES.               
  +0100        **    Application data relating to origin                        
  +0100           15 MQMD-APPLORIGINDATA   PIC X(4) VALUE SPACES.               
  +0100                                                                         
