//PGBMPRGP JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A                                                                
//*LOGONID PYCUTIL                                                              
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
//* 16MAR20  C MUNCAN     ADDED STEPS TO CHANGE LOCK SIZE TO "ANY"    *         
//*                       THEN PERFORM THE PURGE/DELETE AND REVERT    *         
//*                       BACK TO LOCK SIZE "ROW"                     *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//LOCKANY   EXEC DBCTEP2,SSID='PDBC'                                            
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
SET CURRENT SCHEMA='DBCYCP1';                                                   
  ALTER TABLESPACE DYC01P.ZSTXFR LOCKSIZE ANY;                                  
//*                                                                             
//COUNT   EXEC DBCTEP2,                                                         
//             SSID='PDBC'                                                      
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
SET CURRENT SCHEMA='DBCYCP1';                                                   
  SELECT COUNT(*) FROM TSTXFR                                                   
  WHERE CREATE_TS < CURRENT TIMESTAMP - 1 MONTHS;                               
//*                                                                             
//TSTXFR  EXEC DBCTEP2,                                                         
//             SSID='PDBC'                                                      
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
SET CURRENT SCHEMA='DBCYCP1';                                                   
  DELETE FROM TSTXFR                                                            
  WHERE CREATE_TS < CURRENT TIMESTAMP - 1 MONTHS;                               
//*                                                                             
//LOCKROW   EXEC DBCTEP2,SSID='PDBC'                                            
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
SET CURRENT SCHEMA='DBCYCP1';                                                   
  ALTER TABLESPACE DYC01P.ZSTXFR LOCKSIZE ROW;                                  
//*                                                                             
