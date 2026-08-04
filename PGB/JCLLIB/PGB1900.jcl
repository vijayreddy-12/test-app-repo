//PGB1900  JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PYC                                                                  
//*JCLPREP KEEP ROOM                                                            
//*********************************************************************         
//*                                                                   *         
//*  ECOMMERCE - CPM - LOAD GROUP/DIVISIONS                           *         
//*  - THIS JOB LOADS THE INFORMATION SELECTED IN PGB1800 TO THE      *         
//*    CUSTOMER PROFILE DATABASE                                      *         
//*                                                                   *         
//*********************************************************************         
//* BASHAWE - AUG 17/05 - TAKE OUT THE GFM FEED                       *         
//* IBM GR  - AUG 20/08 - UPGRADED IN ECU PROJECT                     *         
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
//PSTEP1  EXEC GB1900                                                           
