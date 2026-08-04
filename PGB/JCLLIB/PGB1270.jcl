//PGB1270  JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PYC                                                                  
//*JCLPREP KEEP ROOM                                                            
//*********************************************************************         
//*                                                                   *         
//*  INTERNET MEMBER KEY IDENTIFIER (MKI) JOB                         *         
//*  - THIS JOB PRINTS AN MKI LETTER FOR NEWLY (PRE/)REGISTERED       *         
//*    MEMBERS TO THE PLAN MEMBER WEB SITE                            *         
//*                                                                   *         
//*********************************************************************         
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*GBSET  INCLUDE MEMBER=J2SETD                                                 
//GBSET  INCLUDE MEMBER=J2SETP                                                  
//GBLOAD INCLUDE MEMBER=J2DB2                                                   
//*                                                                             
//OUT1    OUTPUT COPIES=1,FORMDEF=UFT35B,                                       
//             FORMS=EFORM,PAGEDEF=UFT35B                                       
//*                                                                             
//PSTEP1  EXEC GB1200,                                                          
//             EXTPGM='(PGBICDB6)',                                             
//             GB120045='(GB12004B)',                                           
//             TMPLPGM='(PGBICDB8)',                                            
//             USRCTRL='(MKIADHOC)'                                             
