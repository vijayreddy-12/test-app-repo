//TGBPMAR  JOB (),'SRCESCAN',CLASS=C,MSGCLASS=A,TIME=1440,                      
//  NOTIFY=&SYSUID                                                              
//*********************************************************************         
//*                                                                   *         
//*  PLAN MEMBER REGISTRATION/LOGIN ACTIVITY REPORT                   *         
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
//*    PRINT PLAN MEMBER REGISTRATION/LOGIN ACTIVITY REPORT TO RDS   **         
//*                                                                   *         
//*********************************************************************         
//*                                                                   *         
//*--------------------------------------------------------------*    *         
//* HISTORY                                                           *         
//* 20AUG08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT     *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//REPORT  EXEC GBPMAR,CSTAT=TGW.RC,CLVL=PH1.PGB,                                
//             IFSTAT=TGB,IFLVL=PH1                                             
