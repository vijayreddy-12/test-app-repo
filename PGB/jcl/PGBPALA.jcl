//TGBPALA JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,                                 
//  NOTIFY=&SYSUID                                                              
//*********************************************************************         
//*                                                                   *         
//*  PLAN ADMINISTRATOR LOGIN ACTIVITY REPORT                         *         
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
//*    PRINT PLAN ADMINISTRATOR LOGIN ACTIVITY REPORT TO RDS         **         
//*                                                                   *         
//*********************************************************************         
//*                                                                   *         
//*--------------------------------------------------------------*    *         
//* HISTORY                                                           *         
//* 20AUG08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT     *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//REPORT  EXEC GBPALA,                                                          
//             IFSTAT=TGB,IFLVL=PH1                                             
//GBPALA10.SYSTSIN DD DSN=TGW.RC.PH1.PGB.CTLCARDS(GCCPFRML)                     
