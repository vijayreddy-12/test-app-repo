1*******************************************************************************
 *******************************************************************************
 **                                                                           **
 ** ELEMENT BROWSE                                            14NOV25  14:53  **
 **                                                                           **
 **    ENVIRONMENT:   MVSPROD    SYSTEM:    CJ        SUBSYSTEM:  STANDARD    **
 **    TYPE:          COPYBOOK   STAGE ID:  P                                 **
 **    ELEMENT:       CMQPMOV                                                 **
 **                                                                           **
 **                                                   DELTA TYPE: REVERSE     **
 **                                                                           **
1*******************************************************************************
 *******************************************************************************
                                                                                
 -------------------------- SOURCE LEVEL INFORMATION ---------------------------
                                                                                
1  VVLL SYNC USER     DATE    TIME     STMTS CCID         COMMENT               
   ---- ---- -------- ------- ----- -------- ------------ ----------------------
   0100      DHARKOU  10DEC12 11:08       47 ILCTMMC500KD ADD COPYBOOK TO D1    
 GENERATED   PPCNDEV  18MAR13 16:55          ILCJMMC500KD MOVE COPYBOOK TO PROD 
                                                                                
  +0100        *****************************************************************
  +0100        **                                                              *
  +0100        **  FILE NAME:        CMQPMOV                                   *
  +0100        **                                                              *
  +0100        **  DESCRIPTIVE NAME: COBOL copy file for MQPMO structure       *
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
  +0100        **  FUNCTION:         This file declares the MQPMO structure,   *
  +0100        **                    which is part of the IBM Message Queue    *
  +0100        **                    Interface (MQI).                          *
  +0100        **                                                              *
  +0100        **  ENVIRONMENT:      MVS/ESA                                   *
  +0100        **                                                              *
  +0100        *****************************************************************
  +0100                                                                         
  +0100        **   MQPMO structure                                             
  +0100          10 MQPMO.                                                      
  +0100        **    Structure identifier                                       
  +0100           15 MQPMO-STRUCID          PIC X(4) VALUE 'PMO '.              
  +0100        **    Structure version number                                   
  +0100           15 MQPMO-VERSION          PIC S9(9) BINARY VALUE 1.           
  +0100        **    Options that control the action of MQPUT or MQPUT1         
  +0100           15 MQPMO-OPTIONS          PIC S9(9) BINARY VALUE 0.           
  +0100        **    Reserved                                                   
  +0100           15 MQPMO-TIMEOUT          PIC S9(9) BINARY VALUE -1.          
  +0100        **    Object handle of input queue                               
  +0100           15 MQPMO-CONTEXT          PIC S9(9) BINARY VALUE 0.           
  +0100        **    Reserved                                                   
  +0100           15 MQPMO-KNOWNDESTCOUNT   PIC S9(9) BINARY VALUE 0.           
  +0100        **    Reserved                                                   
  +0100           15 MQPMO-UNKNOWNDESTCOUNT PIC S9(9) BINARY VALUE 0.           
  +0100        **    Reserved                                                   
  +0100           15 MQPMO-INVALIDDESTCOUNT PIC S9(9) BINARY VALUE 0.           
  +0100        **    Resolved name of destination queue                         
  +0100           15 MQPMO-RESOLVEDQNAME    PIC X(48) VALUE SPACES.             
  +0100        **    Resolved name of destination queue manager                 
  +0100           15 MQPMO-RESOLVEDQMGRNAME PIC X(48) VALUE SPACES.             
  +0100                                                                         
