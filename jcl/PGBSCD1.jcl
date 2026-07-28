//TGBSCD1 JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,                                 
//  NOTIFY=&SYSUID                                                              
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*GBSET  INCLUDE MEMBER=J2SETD                                                 
//GBSET  INCLUDE MEMBER=J2SETP                                                  
//GBLOAD INCLUDE MEMBER=J2FULL                                                  
//*********************************************************************         
//*                                                                   *         
//* JOB:          PGBSCD1                                             *         
//*                                                                   *         
//* AUTHOR:       JUDY ELKINS                                         *         
//*                                                                   *         
//* FREQUENCY:    MONTHLY                                             *         
//*                                                                   *         
//* MODIFICATION LOG:                                                 *         
//* ~~~~~~~~~~~~~~~~~                                                 *         
//* 2004-03-15    -  CREATION                                         *         
//* 2008-08-20    -  UPGRADED IN ENTERPRISE COMPILER PROJECT          *         
//*                                                                   *         
//* DESCRIPTION:                                                      *         
//* ~~~~~~~~~~~~                                                      *         
//*                                                                   *         
//* STEP GB4000   - SEND OUT EMAIL NOTIFICATIONS FOR THE CA REPORING  *         
//*                 REAL TIME REPORTING (DISABILITY)                  *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//GB4000  EXEC GB4000,DSTAT=TYT,DLVL=PH1,RUNTYPE=TYTHSCA1,                      
//             OUTSTAT=TGB,OUTLVL=PH1                                           
//GB400005.SORTIN DD DSN=TYT.WH.PH1.TYTHSCA1.REPORT.EMAILS                      
//GB400010.SYSTSIN DD DSN=TGW.RC.PH1.PGB.CTLCARDS(GCCPSCD2)                     
//*DLSO20DD DSN=TGB.WH.PH1.REPORT.EMAILS.SENT&GEN1                              
