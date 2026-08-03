//PGX3230  JOB (GP-GX91),GX,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PGX                                                                  
//GBPROC JCLLIB ORDER=(PEN.GLHNEW.PROD.STANDARD.PROCLIB)                        
//*GBPROC JCLLIB ORDER=(TEN.GLHNEW.DEV.STANDARD.PROCLIB,                        
//*      TEN.GLHNEW.TEST.STANDARD.PROCLIB,                                      
//*      TEN.GLHNEW.ACCEPT.STANDARD.PROCLIB,                                    
//*      PEN.GLHNEW.PROD.STANDARD.PROCLIB)                                      
//*GBSET  INCLUDE MEMBER=JBSETD                                                 
//GBSET  INCLUDE MEMBER=JBSETP                                                  
//*********************************************************************         
//*  JOB DESCRIPTION - PGX3230                                                  
//*  ---------------                                                            
//*  CMR STREAM - FTP THE CMR LCS FEEDS TO THE SERVER                           
//*********************************************************************         
//*--------------------------------------------------------------*              
//* HISTORY                                                                     
//* 01OCT08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT               
//*--------------------------------------------------------------*              
//*                                                                             
//PROC01  EXEC GX3230                                                           
