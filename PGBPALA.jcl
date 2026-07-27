//TGBLAGD  JOB (),'GBLAGD',CLASS=C,MSGCLASS=A,NOTIFY=&SYSUID                    
//*********************************************************************         
//*                                                                   *         
//*  LIST OF ACTIVE GROUP/DIVISIONS WITH GB INTERNET ACCESS REPORT    *         
//*                                                                   *         
//*********************************************************************         
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*GBSET  INCLUDE MEMBER=J2SETD                                                 
//GBSET  INCLUDE MEMBER=J2SETP                                                  
//GBLOAD INCLUDE MEMBER=J2DB2                                                   
//*********************************************************************         
//*                                                                   *         
//*    PRINT LIST OF ACTIVE GROUP/DIVISIONS WITH GB INTERNET ACCESS  **         
//*    REPORT IN RDS                                                 **         
//*                                                                   *         
//*********************************************************************         
//*                                                                   *         
//*--------------------------------------------------------------*    *         
//* HISTORY                                                           *         
//* 20AUG08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT     *         
//*                                                                   *         
//* MAY2013   - GROUP NUMBER EXPANSION PROJECT                                  
//*********************************************************************         
//*                                                                             
//REPORT  EXEC GBLAGD,CSTAT=TGW,CLVL=RC.PH1.PGB,DBVERS='TYT.HCS'                
