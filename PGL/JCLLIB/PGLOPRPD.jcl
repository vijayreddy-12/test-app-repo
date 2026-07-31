//TGLOPRPD JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,NOTIFY=&SYSUID                  
//GBPROC JCLLIB ORDER=(TGW.RC.PH1.PGL.PROCLIB)                                  
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
//* JOB:          PGLOPRPD                                            *         
//* AUTHOR:       ELMER DELGADO                                       *         
//* FREQUENCY:    DAILY                                               *         
//* DESCRIPTION:  OPERA (EFT)                                         *         
//*               PROCESS EFT PAYMENT DETAIL REPORT FILE RECEIVED     *         
//*               FROM ROYAL BANK BY:                                 *         
//*               - MAKING BACKUP COPY                                *         
//*               - DELETING THE RECEIVED FILE                        *         
//*                                                                   *         
//* MODIFICATION LOG:                                                 *         
//* ~~~~~~~~~~~~~~~~~                                                 *         
//* 2006-01-17    -  CREATION                                         *         
//* 2008-10-07    -  IBM GR - UPGRADED IN ENTERPRISE COMPILER PROJECT *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//JS0010  EXEC GLBRSPD,                                                         
//             APLID='OPR',              <== OPERA EFT                          
//             ILVL1='TGL',                                                     
//             OLVL1='TGL'                                                      
