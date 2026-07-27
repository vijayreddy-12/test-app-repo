//TGBMPRGA JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,                                
//  NOTIFY=&SYSUID                                                              
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*GBSET  INCLUDE MEMBER=JBSETD                                                 
//GBSET  INCLUDE MEMBER=JBSETP                                                  
//GBLOAD INCLUDE MEMBER=JBDB2                                                   
//*                                                                             
//*********************************************************************         
//*                                                                   *         
//* HISTORY                                                           *         
//* 20AUG08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT     *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//TSTXFR  EXEC DBCTEP2,                                                         
//             SSID='DDBC'                                                      
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
  DELETE FROM DBCYCD1.TSTXFR_PGBMPRGA_TEST                                      
  WHERE CREATE_TS < CURRENT TIMESTAMP - 1 MONTHS;                               
//                                                                              
