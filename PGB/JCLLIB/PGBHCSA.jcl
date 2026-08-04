//PGBHCSA  JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A                                                                
//*LOGONID PYCUTIL                                                              
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
//             SSID='PDBC'                                                      
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
  DELETE FROM DBCYCP1.TEHCSA                                                    
  WHERE DATE (CHN_TS ) < CURRENT DATE;                                          
