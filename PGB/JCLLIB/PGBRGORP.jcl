//PGBRGORP JOB (GP-GB91),GB,CLASS=P,REGION=0M,                                  
//             MSGCLASS=A                                                       
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PYC                                                                  
//*********************************************************************         
//*                                                                   *         
//*                                                            *      *         
//*  CUSTOMER PROFILE/REGISTRATION SYSTEM                      *      *         
//*  LISTING OF GROUP/CERTS WITH EMAIL ADDR AND PWD REMINDERS  *      *         
//*  - PRODUCES GDG ON A WEEKLY BASIS FOR FRAUD INVESTIGATIONS *      *         
//*                                                            *      *         
//*                                                                   *         
//*********************************************************************         
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*                                                                             
//*********************************************************************         
//*                                                                   *         
//* HISTORY                                                           *         
//* 20AUG08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT     *         
//*                                                                   *         
//* MAY2013   - GROUP NUMBER EXPANSION PROJECT                                  
//*********************************************************************         
//*                                                                             
//STEP1   EXEC GBRGORP                                                          
