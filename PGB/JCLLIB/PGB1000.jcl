//PGB1000  JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A,L=200                                                          
//*LOGONID PYC                                                                  
//*********************************************************************         
//*                                                                   *         
//*  CUSTOMER PROFILE/REGISTRATION SYSTEM                             *         
//*  E-FORMS QUEUE UNLOAD                                             *         
//*  - INTERNET E-FORM SUBMISSIONS ARE READ FROM AN MQ                *         
//*    SERIES QUEUE AND FORMATTED.  THE FORM INFO IS THEN             *         
//*    STORED ON THE CUSTOMER PROFILE DATABASE (DB2)                  *         
//*                                                                   *         
//*********************************************************************         
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*GBSET  INCLUDE MEMBER=J2SETD                                                 
//GBSET  INCLUDE MEMBER=J2SETP                                                  
//GBLOAD INCLUDE MEMBER=J2FULL                                                  
//*********************************************************************         
//*                                                                   *         
//*  MQ QUEUE TO FILE UTILITY  (PROC MQUNLOAD)                        *         
//*                                                                   *         
//*******************************************************             *         
//*                                                                   *         
//*  QUEUE PARM PASSED INTO GFILETOQ IS USED FOR                      *         
//*  CONNECTING TO THE PROPER QUEUE MANAGER AND                       *         
//*  IDENTIFYING WHICH IS THE REQUEST QUEUE TO                        *         
//*  BE PROCESSED.                                                    *         
//*                                                                   *         
//*           __VALID VALUES ARE 'CDQ1' 'CTQ1' 'CAQ1' 'CPQ1'.         *         
//*           | - TO INDICATE WHICH QUEUE MANAGER JOB IS IN USE       *         
//*           |                                                       *         
//*           |             __UP TO 48 CHARACTER QUEUE NAME ALLOWED   *         
//*           |             | - E.G. ISD2.YT.REQUEST.0304             *         
//*           |             |                                         *         
//*           |             |                                         *         
//*           |             |                                         *         
//*           |             |                                         *         
//*           |             |                                         *         
//*           |             |                                         *         
//*          _|_   _________|__________                               *         
//*         /   \ /                    \                              *         
//*  PARM=('CDQ1','ISD2.YT.REQUEST.0304')                             *         
//*                                                                   *         
//*********************************************************************         
//*                                                                   *         
//*--------------------------------------------------------------*    *         
//* HISTORY                                                           *         
//* 20AUG08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT     *         
//*                                                                   *         
//* MAY2013   - GROUP NUMBER EXPANSION PROJECT                                  
//*********************************************************************         
//*                                                                             
//STEP10  EXEC MQUNLODE                                                         
//QTOFILE.DLSI10 DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(QTOFFORM),             
//             DISP=SHR                                                         
//*********************************************************************         
//*                                                                   *         
//*  REFORMAT QUEUE MESSAGE UTILITY  (PROC MRFORMAT)                  *         
//*    FORMAT MQ MESSAGES INTO COPYBOOK FORMAT                        *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//STEP20  EXEC MRFORMTE                                                         
//REFORMAT.DLSI10 DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(RFMTFORM),            
//             DISP=SHR                                                         
//REFORMAT.DLSI70 DD DSN=PGB.REGN.PROD.FORM.MQ(+1),                             
//             DISP=SHR                                                         
//REFORMAT.DLSO71 DD DSN=&&FORMAT,                                              
//             DISP=(NEW,PASS),                                                 
//             UNIT=WORK,                                                       
//             SPACE=(CYL,(10,10),RLSE),                                        
//             DCB=(RECFM=VB,LRECL=4000,BLKSIZE=27998)                          
//*********************************************************************         
//*                                                                   *         
//*  PROC GB1000 -- UPDATE CUSTOMER PROFILE DB                        *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//STEP30  EXEC GB1000                                                           
//*********************************************************************         
//*                                                                   *         
//*  PROC MQTRIGON -- RESET TRIGGER ON MQ QUEUE                       *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//STEP40  EXEC MQTRGONE,                                                        
//             MQTRPARM=('CPQ1','MLI.Q.YT.FORM.SUBMIT')                         
