//PGB1100B JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
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
//* YEATEAN - JUN 13/17 - CREATE NON-SALARY CHANGE PROCESSING         *         
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
//*                                                                             
//*--------------------------------------------------------------------*        
//*      THIS JCL  IS NOW OBSOLETE                                     *        
//*      (LGIPS DECOMISSIONING)                                        *        
//*      IF CONTENTS ARE REQUIRED, USE THE PREVIOUS VERSION (-1)       *        
//*      OBSOLETED AS OF JUL, 2023                                     *        
//*--------------------------------------------------------------------*        
//*                                                                             
//OBSOLETE EXEC GB1100B,                   <== LEAVE ORIGINAL PROC              
//             COND=(0,NE)                                                      
