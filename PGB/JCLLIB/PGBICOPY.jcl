//PGBICOPY JOB (GP-GB91),GB,CLASS=P,MSGCLASS=A                                  
/*JOBPARM R=M04A                                                                
//*LOGONID PYCUTIL                                                              
//GBPROC JCLLIB ORDER=(PEN.GROUP.PROD.STANDARD.PROCLIB)                         
//*GBPROC JCLLIB ORDER=(TEN.GROUP.DEV.STANDARD.PROCLIB,                         
//*      TEN.GROUP.TEST.STANDARD.PROCLIB,                                       
//*      TEN.GROUP.ACCEPT.STANDARD.PROCLIB,                                     
//*      PEN.GROUP.PROD.STANDARD.PROCLIB)                                       
//*GBSET  INCLUDE MEMBER=JBSETD                                                 
//GBSET  INCLUDE MEMBER=JBSETP                                                  
//GBLOAD INCLUDE MEMBER=JBDB2                                                   
//*                                                                             
//*********************************************************************         
//*                                                                   *         
//* JOB:       PGBICOPY                                               *         
//*                                                                   *         
//* FREQUENCY: MONTHY PRIOR TO PURGE (PGBPURG)                        *         
//*                                                                   *         
//* MODIFICATION LOG:                                                 *         
//*                                                                   *         
//* 2001-05-22   -  CREATION                                          *         
//* 2002-01-07   -  ADDED TGDFA AND TPDFDS                            *         
//*              -  REMOVED TPMSSA                                    *         
//* 2002-05-21   -  ADDED TADVSR, TGDADV, TPAR, TEEL, TDMS, TSVC      *         
//* 2002-09-13   -  ADDED TEXP                                        *         
//* 2003-05-06   -  CORRECT LABEL AND VOLUME PARAMETERS TO CREATE ONLY*         
//*                 ONE TAPE                                          *         
//* 2005-03-11    -  ADDED ZADVME                                     *         
//*               -  ADDED ZMFEXCPT,ZMFGRPPL,ZMFGROUP,ZMFDRUGE,ZMFPLAN*         
//*               -  ZMFDRUG,ZMFTC                                    *         
//* 2005-07-12    -  ADDED ZWSCT                                      *         
//* 2008-08-20    -  UPGRADED IN ENTERPRISE COMPILER PROJECT          *         
//*                                                                   *         
//*********************************************************************         
//* TAKE AN IMAGE COPY OF ALL THE PGB TABLES                          *         
//*********************************************************************         
//* UNLDA1S USING UTILITY DSNTIAUL                                    *         
//*                                                                   *         
//*********************************************************************         
//*                                                                             
//UNLD1   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTCSR.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTCSR.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(1,SL),                                                    
//             VOL=(,RETAIN)                                                    
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTCSR),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD2   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTCS.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTCS.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(2,SL),                                                    
//             VOL=(,RETAIN,REF=*.UNLD1.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTCS),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD3   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTCOS.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTCOS.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(3,SL),                                                    
//             VOL=(,RETAIN,REF=*.UNLD2.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTCOS),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD4   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTRGO.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTRGO.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(4,SL),                                                    
//             VOL=(,RETAIN,REF=*.UNLD3.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTRGO),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD5   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTFRN.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTFRN.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(5,SL),                                                    
//             VOL=(,RETAIN,REF=*.UNLD4.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTFRN),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD6   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TFRMAC.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TFRMAC.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(6,SL),                                                    
//             VOL=(,RETAIN,REF=*.UNLD5.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TFRMAC),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD7   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TFNCT.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TFNCT.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(7,SL),                                                    
//             VOL=(,RETAIN,REF=*.UNLD6.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TFNCT),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD8   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTROL.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTROL.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(8,SL),                                                    
//             VOL=(,RETAIN,REF=*.UNLD7.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTROL),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD9   EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTDPT.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTDPT.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(9,SL),                                                    
//             VOL=(,RETAIN,REF=*.UNLD8.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTDPT),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD10  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTFNN.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTFNN.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(10,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD9.SYSREC00)                               
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTFNN),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD11  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCPMUS.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCPMUS.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(11,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD10.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCPMUS),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD12  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTFRM.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTFRM.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(12,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD11.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTFRM),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD13  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTET.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTET.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(13,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD12.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTET),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD14  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTREG.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTREG.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(14,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD13.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTREG),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD15  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTAC.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTAC.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(15,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD14.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTAC),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD16  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCPMU.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCPMU.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(16,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD15.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCPMU),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD17  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTPAS.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTPAS.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(17,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD16.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTPAS),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD18  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTCUS.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTCUS.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(18,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD17.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTCUS),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD19  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTRGS.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTRGS.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(19,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD18.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTRGS),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD20  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTPRV.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTPRV.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(20,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD19.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTPRV),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD21  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTCAT.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTCAT.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(21,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD20.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTCAT),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD22  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTBT.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTBT.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(22,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD21.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTBT),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD23  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCTBS.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCTBS.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(23,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD22.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCTBS),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD24  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TUT.MTH.CRD(+1),                             
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TUT.MTH(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(24,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD23.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TUT),                       
//             DISP=SHR                                                         
//*                                                                             
//UNLD25  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TGCT.MTH.CRD(+1),                            
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TGCT.MTH(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(25,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD24.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TGCT),                      
//             DISP=SHR                                                         
//*                                                                             
//UNLD26  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TCUST.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TCUST.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(26,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD25.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TCUST),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD27  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TLH.MTH.CRD(+1),                             
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TLH.MTH(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(27,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD26.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TLH),                       
//             DISP=SHR                                                         
//*                                                                             
//UNLD28  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TEH.MTH.CRD(+1),                             
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TEH.MTH(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(28,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD27.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TEH),                       
//             DISP=SHR                                                         
//*                                                                             
//UNLD29  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TPAA.MTH.CRD(+1),                            
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TPAA.MTH(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(29,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD28.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TPAA),                      
//             DISP=SHR                                                         
//*                                                                             
//UNLD30  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TAG.MTH.CRD(+1),                             
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TAG.MTH(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(30,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD29.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TAG),                       
//             DISP=SHR                                                         
//*                                                                             
//UNLD31  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TRG.MTH.CRD(+1),                             
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TRG.MTH(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(31,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD30.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TRG),                       
//             DISP=SHR                                                         
//*                                                                             
//UNLD32  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TGD.MTH.CRD(+1),                             
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TGD.MTH(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(32,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD31.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TGD),                       
//             DISP=SHR                                                         
//*                                                                             
//UNLD33  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TSD.MTH.CRD(+1),                             
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TSD.MTH(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(33,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD32.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TSD),                       
//             DISP=SHR                                                         
//*                                                                             
//UNLD34  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TFORM.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TFORM.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(34,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD33.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TFORM),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD35  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TMXKEY.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TMXKEY.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(35,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD34.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TMXKEY),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD36  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TGDFA.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TGDFA.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(36,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD35.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TGDFA),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD37  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TPDFDS.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TPDFDS.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(37,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD36.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TPDFDS),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD38  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TADVSR.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TADVSR.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(38,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD37.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TADVSR),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD39  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TGDADV.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TGDADV.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(39,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD38.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TGDADV),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD40  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TPAR.MTH.CRD(+1),                            
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TPAR.MTH(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(40,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD39.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TPAR),                      
//             DISP=SHR                                                         
//*                                                                             
//UNLD41  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TEEL.MTH.CRD(+1),                            
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TEEL.MTH(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(41,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD40.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TEEL),                      
//             DISP=SHR                                                         
//*                                                                             
//UNLD42  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TDMS.MTH.CRD(+1),                            
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TDMS.MTH(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(42,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD41.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TDMS),                      
//             DISP=SHR                                                         
//*                                                                             
//UNLD43  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TSVC.MTH.CRD(+1),                            
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TSVC.MTH(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(43,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD42.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TSVC),                      
//             DISP=SHR                                                         
//*                                                                             
//UNLD44  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TEXP.MTH.CRD(+1),                            
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TEXP.MTH(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(44,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD43.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TEXP),                      
//             DISP=SHR                                                         
//*                                                                             
//UNLD45  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TADVME.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TADVME.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(45,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD44.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TADVME),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD46  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TEXCPT.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TEXCPT.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(46,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD45.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TEXCPT),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD47  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TGRPPL.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TGRPPL.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(47,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD46.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TGRPPL),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD48  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TGROUP.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TGROUP.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(48,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD47.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TGROUP),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD49  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TDRUGE.MTH.CRD(+1),                          
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TDRUGE.MTH(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(49,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD48.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TDRUGE),                    
//             DISP=SHR                                                         
//*                                                                             
//UNLD50  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TPLAN.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TPLAN.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(50,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD49.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TPLAN),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD51  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TDRUG.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TDRUG.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(51,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD50.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TDRUG),                     
//             DISP=SHR                                                         
//*                                                                             
//UNLD52  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TTC.MTH.CRD(+1),                             
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TTC.MTH(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(52,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD51.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TTC),                       
//             DISP=SHR                                                         
//*                                                                             
//UNLD53  EXEC PGM=IKJEFT01,                                                    
//             DYNAMNBR=20,                                                     
//             REGION=0M,                                                       
//             TIME=1440                                                        
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
//SYSUDUMP  DD SYSOUT=A                                                         
//SYSPUNCH  DD DSN=PGB.UNLD.DYC01P.TWSCT.MTH.CRD(+1),                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=PROD,                                                       
//             SPACE=(80,(10,10),RLSE),                                         
//             DCB=(PGB.MODEL,RECFM=FB,LRECL=80,BLKSIZE=8000,BUFNO=20)          
//SYSREC00  DD DSN=PGB.UNLD.DYC01P.TWSCT.MTH(+1),                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             UNIT=(VTAPE,,DEFER),                                             
//             DCB=(PGB.MODEL,BUFNO=50),                                        
//             LABEL=(53,SL),                                                   
//             VOL=(,RETAIN,REF=*.UNLD52.SYSREC00)                              
//SYSTSIN   DD DSN=PYI.CONTROL(PDBC),                                           
//             DISP=SHR                                                         
//          DD DSN=PYI.CONTROL(DSNTIAUT),                                       
//             DISP=SHR                                                         
//SYSIN     DD DSN=PEN.GROUP.PROD.STANDARD.CTLCARDS(TWSCT),                     
//             DISP=SHR                                                         
