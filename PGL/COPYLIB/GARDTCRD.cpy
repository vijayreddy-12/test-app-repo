      *01  GARDTCRD.                                                            
      *----------------------------------------------------------------*        
      *  COPY BOOK FOR DATE CARD PGW.PROD.DATECARD(0)                  *        
      *                                                                *        
      *  14 JUN 2002 - JACKIE SASSO - CREATION                         *        
      *                                                                *        
      *  08 SEP 2003 - MIKE COOPER  - ADD COLUMN LAST DAY OF MONTH     *        
      *----------------------------------------------------------------*        
      *  CURRENT,NEXT,LEAP+1,LEAP-1                                    *        
      *----------------------------------------------------------------*        
           05  GARDTCRD-COMMENT                 PIC X(1).                       
           05  GARDTCRD-IDENTIFIER              PIC X(8).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-YYYYMMMDD.                                              
               10  GARDTCRD-YYYY                PIC 9(4).                       
               10  GARDTCRD-MMM                 PIC X(3).                       
               10  GARDTCRD-DD                  PIC 9(2).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-DAYMONYY.                                               
               10  GARDTCRD-DAY                 PIC 9(2).                       
               10  GARDTCRD-MON                 PIC X(3).                       
               10  GARDTCRD-YY                  PIC 9(2).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-Y4M2D2.                                                 
               10  GARDTCRD-Y4                  PIC 9(4).                       
               10  GARDTCRD-M2                  PIC 9(2).                       
               10  GARDTCRD-D2                  PIC 9(2).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-4Y-2M-2D.                                               
               10  GARDTCRD-4Y                  PIC 9(4).                       
               10  FILLER                       PIC X(1).                       
               10  GARDTCRD-2M                  PIC 9(2).                       
               10  FILLER                       PIC X(1).                       
               10  GARDTCRD-2D                  PIC 9(2).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-YEAR4DDD.                                               
               10  GARDTCRD-YEAR4               PIC 9(4).                       
               10  GARDTCRD-DDD                 PIC 9(3).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-YYYDDD.                                                 
               10  GARDTCRD-JUL-YYY             PIC 9(3).                       
               10  GARDTCRD-JUL-DDD             PIC 9(3).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-CL2-YYY-MM-DD.                                          
               10  GARDTCRD-CL2-YYY             PIC 9(3).                       
               10  GARDTCRD-CL2-MM              PIC 9(2).                       
               10  GARDTCRD-CL2-DD              PIC 9(2).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-QUARTER                 PIC X(2).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-WEEKDAY                 PIC X(9).                       
           05  FILLER                           PIC X(1).                       
           05  GARDTCRD-LAST-DAY-OF-MONTH.                                      
               10  GARDTCRD-LAST-YYYY           PIC 9(4).                       
               10  GARDTCRD-LAST-MMM            PIC X(3).                       
               10  GARDTCRD-LAST-DD             PIC 9(2).                       
           05  FILLER                           PIC X(107).                     
      *                                                                         
