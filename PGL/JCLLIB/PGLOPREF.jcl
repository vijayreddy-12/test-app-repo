//TGLOPREF JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,NOTIFY=&SYSUID                  
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
//* JOB:          PGLOPREF                                            *         
//* AUTHOR:       ELMER DELGADO                                       *         
//* FREQUENCY:    DAILY                                               *         
//* DESCRIPTION:  OPERA EFT FILE PROCESSING FOR BRS                   *         
//*               TO PRINT PAYMENT REGISTER AND CONTROL TOTALS        *         
//*               TO TRANSMIT FILE TO ROYAL BANK                      *         
//*                                                                   *         
//* MODIFICATION LOG:                                                 *         
//* ~~~~~~~~~~~~~~~~~                                                 *         
//* 2006-01-17    -  CREATION                                         *         
//* 2008-05-29    -  ADD PARAMETER TO INDICATE NOT A SUBSCRIBER FEED  *         
//* 2008-10-07    -  IBM GR - UPGRADED IN ENTERPRISE COMPILER PROJECT *         
//* JUN2012          IDMSDB2  GAEDATSR REPLACEMENT                    *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//JS0010  EXEC GLBRS040,CNTLDSN='TGW.RC.PH1.PGL.CTLCARDS',                      
//             APLID='OPR',              <== OPERA                              
//             FDTYPE='',                <== FEED TYPE (NOT SUBSCR)             
//             NDMPARM='BRSNDMOP',       <== CONTROL CARDS                      
//             PARMX='OP',               <== OPERA (EFT)                        
//             RPTC='(5,PGLPR032)',      <== RDS REPORT ID                      
//             RPTR='(5,PGLPR033)',      <== RDS REPORT ID                      
//             ILVL1='TGL',                                                     
//             SRCE='CLSERV',            <== SOURCE CODE                        
//             OLVL1='TGL'                                                      
//PS0010.SYSUT1 DD DSN=TGL.CLSERV.OPR.EFT.RECEIVED.DEC1225                      
//PS0020.DLSI10 DD DSN=TGL.CLSERV.OPR.EFT.RECEIVED.DEC1225                      
//PS0050.INFILE DD DSN=TGL.CLSERV.OPR.EFT.RECEIVED.DEC1225                      
