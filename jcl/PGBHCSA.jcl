//TGBHCSA JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,                                 
//  NOTIFY=&SYSUID                                                              
//*********************************************************************         
//*                                                                   *         
//*  DELETE OLD HCSA SERVICES FROM DB2 TBL TEHCSA                     *         
//*  IE. TIMESTAMP OF PREV DAY AND OLDER                              *         
//*                                                                   *         
//*********************************************************************         
//JOBLIB    DD DSN=PCX.DB2.DSNLOAD,                                             
//             DISP=SHR                                                         
//          DD DSN=PCX.DB2.RUNLIB.LOAD,                                         
//             DISP=SHR                                                         
//*                                                                             
//HCSADEL EXEC DBCTEP2,                                                         
//             SSID='DDBC'                                                      
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
  DELETE FROM DBCYCD1.TEHCSA_PGBHCSA_TEST                                       
  WHERE DATE (CHN_TS ) < CURRENT DATE;                                          
