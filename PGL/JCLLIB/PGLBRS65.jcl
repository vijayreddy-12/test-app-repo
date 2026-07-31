//TGLBRS65 JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,NOTIFY=&SYSUID                  
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
//* JOB:          PGLBRS65                                            *         
//* AUTHOR:       TARIQ NOOR                                          *         
//* FREQUENCY:    DAILY                                               *         
//* DESCRIPTION:  LH CLAIMS (AFFINITY) CHEQUE ISSUE FILE FOR BRS      *         
//*               TO PRINT CONTROL DETAIL AND SUMMARY REPORTS FOR BRS *         
//*                                                                   *         
//* MODIFICATION LOG:                                                 *         
//* ~~~~~~~~~~~~~~~~~                                                 *         
//* 2004-10-04    -  CREATION                                         *         
//*                                                                   *         
//* 2008-10-07    -  IBM GR - UPGRADED IN ENTERPRISE COMPILER PROJECT *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//JS0010  EXEC GLBRS010,                                                        
//             APLID='LHA',                                                     
//             APLID1='LHA',                                                    
//             PARMX='LA',                                                      
//             RPTD='(5,PGLPR005)',                                             
//             RPTS='(5,PGLPR006)',                                             
//             SRCE='UNISYS',                                                   
//             ILVL1='TGL',OLVL1='TGL',ILVL3='TGW',PLVL3='RC.PH1'               
//PS0010.SORTIN DD DSN=TGL.LHA.BRS.CHEQ.ISSUE.TEST                              
//PS0030.SYSUT2 DD DSN=TGL.RC.PH1.LHA.ISSUE.FEED                                
