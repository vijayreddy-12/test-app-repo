//TGLCHRAG JOB (),'SRCESCAN',CLASS=K,MSGCLASS=A,NOTIFY=&SYSUID                  
//*GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                        
//GBPROC JCLLIB ORDER=(TGW.RC.PH1.PGL.PROCLIB,                                  
//             PEN.GROUP.PROD.STANDARD.PROCLIB)                                 
//GBSET  INCLUDE MEMBER=JBSETP                                                  
//GBLOAD INCLUDE MEMBER=JBFULL                                                  
//*********************************************************************         
//*                                                                   *         
//* HISTORY                                                           *         
//* 07JUN13  GB SHUTDOWN PROJECT - CHRYSLER OPTIONAL BILLING (NEW JOB)*         
//*                                                                  *          
//*********************************************************************         
//*                                                                             
//GLCHRAG1 EXEC GLCHRAGP,                                                       
//            CBACCNT='TGL',                                                    
//            NODE3='RC.PH1'                                                    
