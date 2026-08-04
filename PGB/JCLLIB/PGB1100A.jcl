//PGB1100A JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
//*JCLPREP KEEP ROOM                                                            
/*JOBPARM R=PKUP,L=200                                                          
//*LOGONID PYC                                                                  
//*********************************************************************         
//*                                                                   *         
//*  CUSTOMER PROFILE/REGISTRATION SYSTEM                             *         
//*  E-FORMS AFP PRINT (WITH OPTIONAL RE-PRINT                        *         
//*  - INTERNET UNPRINTED E-FORMS ARE READ FROM THE                   *         
//*    CUSTOMER PROFILE DB. THE FORM INFO IS THEN                     *         
//*    FORMATTED/SORTED AND PRINTED ON AFP FORMS.                     *         
//*                                                                   *         
//* ----------------------------------------------------------------  *         
//* YEATEAN - JUN 13/17 - CREATE FOR NON-SALARY CHANGE/NON TERMINATION*         
//*                       PROCESSING                                  *         
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
//*********************************************************************         
//*                                                                   *         
//*          PYT.PROCLIB,PYI.PROCLIB)                                 *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//OUT1    OUTPUT COPIES=1,FORMDEF=UFR08B,                                       
//             FORMS=EFORM,PAGEDEF=UFR08B                                       
//*                                                                             
//PSTEP2  EXEC GB1100A                                                          
