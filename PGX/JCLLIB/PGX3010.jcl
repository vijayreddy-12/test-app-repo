//PGX3010  JOB (GP-GX91),GX,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PGX                                                                  
//GBPROC JCLLIB ORDER=(PEN.GLHNEW.PROD.STANDARD.PROCLIB)                        
//*GBPROC JCLLIB ORDER=(TEN.GLHNEW.DEV.STANDARD.PROCLIB,                        
//*      TEN.GLHNEW.TEST.STANDARD.PROCLIB,                                      
//*      TEN.GLHNEW.ACCEPT.STANDARD.PROCLIB,                                    
//*      PEN.GLHNEW.PROD.STANDARD.PROCLIB)                                      
//*GBSET  INCLUDE MEMBER=J2SETD                                                 
//GBSET  INCLUDE MEMBER=J2SETP                                                  
//GBLOAD INCLUDE MEMBER=J2DB2                                                   
//*********************************************************************         
//*  JOB DESCRIPTION - PGX3010                                                  
//*  ---------------                                                            
//*  CMR STREAM - BUILD THE CMR GIPSY FEED                                      
//*********************************************************************         
//*--------------------------------------------------------------*              
//* HISTORY                                                                     
//* 01OCT08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT               
//* JUN2013  GROUP NUMBER EXPANSION PROJECT - 7-DIGIT COMPLIANT                 
//*--------------------------------------------------------------*              
//*********************************************************************         
//* BACKUP PREVIOUS ISPRVEXT AND CREATE NEW ONE                                 
//*********************************************************************         
//*                                                                             
//*--------------------------------------------------------------------*        
//*      THIS JCL  IS NOW OBSOLETE                                     *        
//*      (LGIPS DECOMISSIONING)                                        *        
//*      IF CONTENTS ARE REQUIRED, USE THE PREVIOUS VERSION (-1)       *        
//*      OBSOLETED AS OF JUL, 2023                                     *        
//*--------------------------------------------------------------------*        
//*                                                                             
//OBSOLETE EXEC GX3010A,                   <== LEAVE ORIGINAL PROC              
//             COND=(0,NE)                                                      
