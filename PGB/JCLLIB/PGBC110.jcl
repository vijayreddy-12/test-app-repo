//PGBC110  JOB (GP-GG91),GG,CLASS=P,MSGCLASS=A                                  
//*        TYPRUN=SCAN,                                                         
/*JOBPARM R=M04A,L=9                                                            
//*LOGONID PYC                                                                  
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
//*  JOB DESCRIPTION - PGBC110     JOB 2 OF 2                                   
//*  ---------------                                                            
//*  CPD - GET CPD DELTAS                                                       
//*********************************************************************         
//*--------------------------------------------------------------*              
//* HISTORY:                                                                    
//* OCT2016  C360R2 - CREATED                                                   
//*--------------------------------------------------------------*              
//*                                                                             
//STEP1    EXEC GBC110                                                          
