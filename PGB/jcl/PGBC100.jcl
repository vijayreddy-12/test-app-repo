//TGBC100 JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,                                 
//  NOTIFY=&SYSUID                                                              
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*                                                                             
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*                                                                             
//*GBSET  INCLUDE MEMBER=J2SETD                                                 
//GBSET  INCLUDE MEMBER=J2SETP                                                  
//GBLOAD INCLUDE MEMBER=J2BTCH                                                  
//*********************************************************************         
//*  JOB DESCRIPTION - PGBC100     JOB 1 OF 2                                   
//*  ---------------                                                            
//*  CPD - UNLOAD TCUST AND TGCT TABLES                                         
//*********************************************************************         
//*--------------------------------------------------------------*              
//* HISTORY:                                                                    
//* OCT2016  C360R2 - CREATED                                                   
//*--------------------------------------------------------------*              
//*                                                                             
//STEP1 EXEC GBC100,DSTAT=TGB,DBNM=DYC01D,CSTAT=TGW,                            
//           CLVL=RC.PH1.PGB,DSSID=DDBC                                         
