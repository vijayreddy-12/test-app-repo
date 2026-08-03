//PGX3550  JOB (GP-GX91),GX,CLASS=P,MSGCLASS=A                                  
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
//*  JOB DESCRIPTION - PGX3550                                                  
//*  ---------------                                                            
//*  CMR STREAM - PROCESS THE VO LTD FEED                                       
//*********************************************************************         
//*--------------------------------------------------------------*              
//* HISTORY                                                                     
//* 01OCT08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT               
//*--------------------------------------------------------------*              
//*                                                                             
//PROC01  EXEC GX3550                                                           
