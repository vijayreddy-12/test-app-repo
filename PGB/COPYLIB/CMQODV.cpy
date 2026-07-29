1*******************************************************************************
 *******************************************************************************
 **                                                                           **
 ** ELEMENT BROWSE                                            14NOV25  14:52  **
 **                                                                           **
 **    ENVIRONMENT:   MVSPROD    SYSTEM:    CJ        SUBSYSTEM:  STANDARD    **
 **    TYPE:          COPYBOOK   STAGE ID:  P                                 **
 **    ELEMENT:       CMQODV                                                  **
 **                                                                           **
 **                                                   DELTA TYPE: REVERSE     **
 **                                                                           **
1*******************************************************************************
 *******************************************************************************
                                                                                
 -------------------------- SOURCE LEVEL INFORMATION ---------------------------
                                                                                
1  VVLL SYNC USER     DATE    TIME     STMTS CCID         COMMENT               
   ---- ---- -------- ------- ----- -------- ------------ ----------------------
   0100      DHARKOU  10DEC12 11:08       41 ILCTMMC500KD ADD COPYBOOK TO D1    
 GENERATED   PPCNDEV  18MAR13 16:55          ILCJMMC500KD MOVE COPYBOOK TO PROD 
                                                                                
  +0100        *****************************************************************
  +0100        **                                                              *
  +0100        **  FILE NAME:        CMQODV                                    *
  +0100        **                                                              *
  +0100        **  DESCRIPTIVE NAME: COBOL copy file for MQOD structure        *
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
  +0100        **  FUNCTION:         This file declares the MQOD structure,    *
  +0100        **                    which is part of the IBM Message Queue    *
  +0100        **                    Interface (MQI).                          *
  +0100        **                                                              *
  +0100        **  ENVIRONMENT:      MVS/ESA                                   *
  +0100        **                                                              *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   MQOD structure                                              
  +0100          10 MQOD.                                                       
  +0100        **    Structure identifier                                       
  +0100           15 MQOD-STRUCID         PIC X(4) VALUE 'OD  '.                
  +0100        **    Structure version number                                   
  +0100           15 MQOD-VERSION         PIC S9(9) BINARY VALUE 1.             
  +0100        **    Object type                                                
  +0100           15 MQOD-OBJECTTYPE      PIC S9(9) BINARY VALUE 1.             
  +0100        **    Object name                                                
  +0100           15 MQOD-OBJECTNAME      PIC X(48) VALUE SPACES.               
  +0100        **    Object queue manager name                                  
  +0100           15 MQOD-OBJECTQMGRNAME  PIC X(48) VALUE SPACES.               
  +0100        **    Dynamic queue name                                         
  +0100           15 MQOD-DYNAMICQNAME    PIC X(48) VALUE 'CSQ.*'.              
  +0100        **    Alternate user identifier                                  
  +0100           15 MQOD-ALTERNATEUSERID PIC X(12) VALUE SPACES.               
  +0100                                                                         
