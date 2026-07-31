//TGLBRS80 JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,NOTIFY=&SYSUID                  
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
//* JOB:          PGLBRS80                                            *         
//* AUTHOR:       TARIQ NOOR                                          *         
//* FREQUENCY:    DAILY                                               *         
//* DESCRIPTION:  LH CLAIMS EFT FILE PROCESSING FOR BRS               *         
//*               TO PRINT PAYMENT REGISTER AND CONTROL TOTALS        *         
//*               TO TRANSMIT FILE TO ROYAL BANK                      *         
//*                                                                   *         
//* MODIFICATION LOG:                                                 *         
//* ~~~~~~~~~~~~~~~~~                                                 *         
//* 2004-10-04    -  CREATION                                         *         
//*                                                                   *         
//* 2008-10-07    -  IBM GR - UPGRADED IN ENTERPRISE COMPILER PROJECT *         
//*                                                                   *         
//* JUN2012          IDMSDB2  GAEDATSR REPLACEMENT                    *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//JS0010  EXEC GLBRS040,ILVL1='TGL',OLVL1='TGL',                                
//             CNTLDSN='TGW.RC.PH1.PGL.CTLCARDS',                               
//             APLID='LHC',                 <== LH CLAIMS                       
//             FDTYPE='',                   <== FEED NAME                       
//             NDMPARM='BRSNDMLH',          <== CONTROL CARDS                   
//             PARMX='LH',                  <== LH CLAIMS (EFT)                 
//             RPTC='(5,PGLPR001)',         <== RDS REPORT ID                   
//             RPTR='(5,PGLPR002)',         <== RDS REPORT ID                   
//             SRCE='UNISYS'                <== SOURCE                          
//PS0060.NDMBATCH EXEC PGM=DMBATCH,COND=(0,LE)                                  
