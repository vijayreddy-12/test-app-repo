      *01  GCCCSCD1.                                                    00010000
      *****************************************************************         
      *     REAL TIME REPORTING - SCHEDULER COPYBOOK USED FOR         *         
      *                           EMAIL NOTIFICATIONS                 *         
      *                                                               *         
      *       CREATED BY JUDY ELKINS                                  *         
      *       USED    BY MAINFRAME COMPONENT OF REAL TIME REPORTING   *         
      *                  SCHEDULER                                    *         
      *                                                               *         
      *****************************************************************         
      *    FEB/04 JUDY ELKINS                                         *         
      *           - USED TO BUILD EMAIL NOTIFICATIONS WHEN            *         
      *             REPORTS ARE READY FOR PROCESSING                  *         
      *                                                               *         
      *****************************************************************         
          05  GCCCSCD1-USER-ID                    PIC X(20).                    
          05  GCCCSCD1-REQ-DS                     PIC X(100).                   
          05  FILLER                              PIC X(80).                    
