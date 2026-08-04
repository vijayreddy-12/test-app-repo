//PGB1090  JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
//*JCLPREP KEEP ROOM                                                            
/*JOBPARM R=PKUP,L=200                                                          
//*LOGONID PYC                                                                  
//*********************************************************************         
//*                                                                   *         
//*  AUDIT REPORT OF SALARY CHANGE REPORTS AND TERMINATION REPORT     *         
//*                                                                   *         
//*  SALARY AND TERM REPORTS ARE COMPARED WITH A GIPC FILE(DAREG7).   *         
//*  THE OUTPUT FILES ARE IN CSV FORMAT AND                           *         
//*  WILL BE TRANSMITTED TO A LAN FOLDER THROUGH FTP.                 *         
//*                                                                   *         
//*********************************************************************         
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*********************************************************************         
//*                                                                             
//*--------------------------------------------------------------------*        
//*      THIS JCL  IS NOW OBSOLETE                                     *        
//*      (LGIPS DECOMISSIONING)                                        *        
//*      IF CONTENTS ARE REQUIRED, USE THE PREVIOUS VERSION (-1)       *        
//*      OBSOLETED AS OF JUL, 2023                                     *        
//*--------------------------------------------------------------------*        
//*                                                                             
//OBSOLETE EXEC GB1090,                    <== LEAVE ORIGINAL PROC              
//             COND=(0,NE)                                                      
