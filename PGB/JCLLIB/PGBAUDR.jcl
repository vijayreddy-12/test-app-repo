//PGBAUDR  JOB (GP-GB91),GB,CLASS=P,REGION=5000K,                               
//             MSGCLASS=A                                                       
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PYC                                                                  
//*********************************************************************         
//*                                                                   *         
//*  CPM AUDIT REPORT                                                 *         
//*                                                                   *         
//*********************************************************************         
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*GBSET  INCLUDE MEMBER=JBSETD                                                 
//GBSET  INCLUDE MEMBER=JBSETP                                                  
//GBLOAD INCLUDE MEMBER=JBDB2I                                                  
//*                                                                             
//*********************************************************************         
//*                                                                   *         
//*    PRINT FEEDBACK TO RDS REPORT                                  **         
//*                                                                   *         
//*********************************************************************         
//*                                                                   *         
//*--------------------------------------------------------------*    *         
//* HISTORY                                                           *         
//* 20AUG08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT     *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//REPORT  EXEC GBAUDR                                                           
/*                                                                              
