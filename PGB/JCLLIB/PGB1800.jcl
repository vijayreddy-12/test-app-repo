//PGB1800  JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PGB                                                                  
//*JCLPREP KEEP ROOM                                                            
//*********************************************************************         
//*                                                                   *         
//*  ECOMMERCE - CPM - GROUP DIVISION EXTRACTS                        *         
//*  - THIS JOB EXTRACTS GROUP/DIV INFORMATION FROM GIPSY (GB1800A)   *         
//*    AND GFM (GB1800B)                                              *         
//*                                                                   *         
//*********************************************************************         
//* BASHAWE - AUG 17/05 - TAKE OUT THE GFM FEED                       *         
//* IBM GR  - AUG 20/08 - UPGRADED IN ECU PROJECT                     *         
//* IBM GR  - JAN 07/15 - TL234928 LOGONID CHNAGED TO PGB             *         
//* IBM GR  - FEB 06/15 - TL236841 LOGONID CHNAGED TO PYT AGAIN       *         
//*           FEB 23/15 - TL236841 LOGONID CHNAGED TO PGB                       
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
//*--------------------------------------------------------------------*        
//*      THIS JCL  IS NOW OBSOLETE                                     *        
//*      (LGIPS DECOMISSIONING)                                        *        
//*      IF CONTENTS ARE REQUIRED, USE THE PREVIOUS VERSION (-1)       *        
//*      OBSOLETED AS OF JUL, 2023                                     *        
//*--------------------------------------------------------------------*        
//*                                                                             
//OBSOLETE EXEC GB1800A,                    <== LEAVE ORIGINAL PROC             
//             COND=(0,NE)                                                      
