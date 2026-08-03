//PGX3510W JOB (GP-GX91),GX,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PGX                                                                  
//***LOGONID PGX                                                                
//GBPROC JCLLIB ORDER=(PEN.GLHNEW.PROD.STANDARD.PROCLIB)                        
//*GBPROC JCLLIB ORDER=(TEN.GLHNEW.DEV.STANDARD.PROCLIB,                        
//*      TEN.GLHNEW.TEST.STANDARD.PROCLIB,                                      
//*      TEN.GLHNEW.ACCEPT.STANDARD.PROCLIB,                                    
//*      PEN.GLHNEW.PROD.STANDARD.PROCLIB)                                      
//*GBSET  INCLUDE MEMBER=JBSETD                                                 
//GBSET  INCLUDE MEMBER=JBSETP                                                  
//GBLOAD INCLUDE MEMBER=JBBTCH                                                  
//**************************************************************                
//*  JOB DESCRIPTION - PGX3510W                                                 
//*  ---------------                                                            
//*  CMR STREAM - WATCH JOB THAT WAITS FOR VO CLIENT STRUCTURE FILES.           
//*  THIS JOB WILL DELETE THE TRIGGER FILE AND THEN COPY THE                    
//*  UPLOADED FILES TO GDGS.                                                    
//*  THE TRIGGER FILE IS: PGX.PROD.VOTOCMR.TRIGGER                              
//**************************************************************                
//*--------------------------------------------------------------*              
//* HISTORY                                                                     
//* 01OCT08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT               
//*--------------------------------------------------------------*              
//*                                                                             
//PROC01  EXEC GX3510W                                                          
