//PGBPURG  JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PYCUTIL                                                              
//*********************************************************************         
//*                                                                   *         
//*  CUSTOMER PROFILE DATABASE PURGE                                  *         
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
//* HISTORY                                                           *         
//* 20AUG08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT     *         
//* 23OCT17  T324209      REINSTATE THIS JOB TO RUN ONLY THREE STEPS  *         
//*                       TO PERFORM PURGE ONLY ON TFORM,TSD,TAUDEV,  *         
//*                       TLH & TSSOLH TABLES.                        *         
//*                                                                   *         
//* 13MAR18  T324209      CHANGED LOGIN ID TO PYCUTIL.                *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//PURG01  EXEC GBPURG                                                           
/*                                                                              
