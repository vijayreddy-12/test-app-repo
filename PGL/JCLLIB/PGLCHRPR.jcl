//TGLCHRPR JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,NOTIFY=&SYSUID                  
//GBPROC JCLLIB ORDER=(TGW.RC.PH1.PGL.PROCLIB)                                  
//GBSET  INCLUDE MEMBER=JBSETP                                                  
//GBLOAD INCLUDE MEMBER=JBFULL                                                  
//*********************************************************************         
//*                                                                   *         
//* HISTORY                                                           *         
//* 07JUN13  GB SHUTDOWN PROJECT - CHRYSLER OPTIONAL BILLING (NEW JOB)*         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//GLCHRPR1 EXEC GLCHRPRP,                                                       
//            CBACCNT='TGL',                                                    
//            NODE3='RC.PH1'                                                    
