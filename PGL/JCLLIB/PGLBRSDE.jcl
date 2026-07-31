//TGLBRSDE JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,NOTIFY=&SYSUID                  
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*GBSET  INCLUDE MEMBER=JBSETD                                                 
//GBSET  INCLUDE MEMBER=JBSETP                                                  
//*                                                                             
//*********************************************************************         
//*                                                                   *         
//* JOB:          PGLBRSDE                                            *         
//* AUTHOR:       TARIQ NOOR                                          *         
//* FREQUENCY:    DAILY                                               *         
//* DESCRIPTION:  TO DELETE THE FILE JOB AFTER SENDING THE FILES TO   *         
//*               ROYAL BANK                                          *         
//*                                                                   *         
//* MODIFICATION LOG:                                                 *         
//* ~~~~~~~~~~~~~~~~~                                                 *         
//* 2004-12-15    -  CREATION                                         *         
//*                                                                   *         
//* 2006-01-17    -  ADDED THE DELETE OF THE OPERA EFT FILE           *         
//*                                                                   *         
//* 2008-10-07    -  IBM GR - UPGRADED IN ENTERPRISE COMPILER PROJECT *         
//*********************************************************************         
//*  JOB         : TO DELETE LHC EFT FILE                             *         
//*              : TO DELETE LHC SUBSCRIBER EFT FILE                  *         
//*              : TO DELETE HDC EFT FILE                             *         
//*              : TO DELETE OPR EFT FILE                             *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//STEP005 EXEC PGM=IEFBR14                                                      
//SYSPRINT  DD SYSOUT=*                                                         
//SYSOUT    DD SYSOUT=*                                                         
//DD1       DD DSN=TGL.UNISYS.LHC.EFT.RECEIVED.TEST,                            
//             DISP=(MOD,DELETE),                                               
//             UNIT=PROD,                                                       
//             SPACE=(TRK,1)                                                    
//DD2       DD DSN=TGL.UNISYS.LHC.EFT.RECEIVED.SUBS.TEST,                       
//             DISP=(MOD,DELETE),                                               
//             UNIT=PROD,                                                       
//             SPACE=(TRK,1)                                                    
//DD4       DD DSN=TGL.CLSERV.OPR.EFT.RECEIVED.DEC1225.TEST,                    
//             DISP=(MOD,DELETE),                                               
//             UNIT=PROD,                                                       
//             SPACE=(TRK,1)                                                    
