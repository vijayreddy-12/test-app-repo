//TGLBRS81 JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,NOTIFY=&SYSUID                  
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
//* JOB:          PGLBRS81                                            *         
//* FREQUENCY:    DAILY                                               *         
//* DESCRIPTION:  LH CLAIMS SUBSCRIBER FT FILE PROCESSING FOR BRS     *         
//*               TO PRINT PAYMENT REGISTER AND CONTROL TOTALS        *         
//*               TO TRANSMIT FILE TO ROYAL BANK                      *         
//*                                                                   *         
//* MODIFICATION LOG:                                                 *         
//* ~~~~~~~~~~~~~~~~~                                                 *         
//* 2018-11-02   -  RE-ACTIVATE THE JOB FOR SUBSCRIBER CLAIMS WITH EFT*         
//*                 FOR AFFINITY LH ONLINE CLAIMS SUBMISSION (OCS).   *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//JS0010  EXEC GLBRS040,ILVL1='TGL',OLVL1='TGL',                                
//             CNTLDSN='TGW.RC.PH1.PGL.CTLCARDS',                               
//             APLID='LHC',                 <== LH CLAIMS                       
//             FDTYPE='.SUBS',              <== FEED NAME                       
//             NDMPARM='BRSNDMLS',          <== CONTROL CARDS                   
//             PARMX='LH',                  <== LH CLAIMS (EFT)                 
//             RPTC='(5,PGLPR003)',         <== RDS REPORT ID                   
//             RPTR='(5,PGLPR004)',         <== RDS REPORT ID                   
//             SRCE='UNISYS'                <== SOURCE                          
//PS0060.NDMBATCH EXEC PGM=DMBATCH,COND=(0,LE)                                  
