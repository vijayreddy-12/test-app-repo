      *************************                                                 
       IDENTIFICATION DIVISION.                                                 
      *************************                                                 
                                                                                
      *This program has been examined/renovated for Year 2000 compliance.       
       PROGRAM-ID.    INCGDUMP.                                                 
      *DATE-WRITTEN.  JULY 1995.                                                
      *DATE-COMPILED.                                                           
                                                                                
      *----------------------------------------------------------------         
      * DESCRIPTION:                                                            
      *     INCGDUMP - INTERFACE ABEND DUMP ROUTINE.                            
      *     THIS MODULE PERFORMS DUMP PROCESSING WHEN A SEVERE ERROR            
      *     IS ENCOUNTERED BY ONE OF THE CLIENT/CONTRACT, ELIGIBILITY           
      *     OR CLAIM HISTORY INTERFACE MODULES.                                 
      *     THIS MODULE PRINTS ERROR MESSAGES ON THE MASTER CONSOLE AND         
      *     JES2 EXECUTION LOG FOR FATAL OR NON-FATAL ERROR.                    
      *     WHEN A FATAL ERROR IS ENCOUNTERED, THIS MODULE PERFORMS             
      *     DUMP PROCESSING AND JOB WILL BE TERMINATED.                         
      *                                                                         
      * PARAMETERS:                                                             
      *     NONE                                                                
      *                                                                         
      * INPUT:                                                                  
      *     IN LINKAGE SECTION - COPYBOOK 'INRGDUMP'.                           
      *                                                                         
      * OUTPUT FILES:                                                           
      *     ERROR MESSAGES ON MASTER CONSOLE AND JES2 EXECUTION LOG.            
      *                                                                         
      * CALLING MODULES:                                                        
      *     ANYONE OF THE CLIENT/CONTRACT, ELIGIBILITY OR CLAIMS                
      *     HISTORY INTERFACE MODULES.                                          
      *                                                                         
      * CALLED MODULES:                                                         
      *     WTL      - WRITE TO LOG UTILITY                                     
      *     ZPARDUMP - DUMPING UTILITY                                          
      *                                                                         
      * HISTORY:                                                                
      *       JUL 95 -  JOHN LAM     NEW PROGRAM                                
      *----------------------------------------------------------------         
      *----------------------------------------------------------------         
      *                                                                         
      *  AUG/95 NAYYAR SHAUKAT.                                                 
      *       - AMENDMENTS MADE TO CORRECT LOGICAL ERROR IN                     
      *         3000-WRAP-UP  ROUTINE AS IT WAS NOT EXECUTING                   
      *       - ZPARDUMP MODULE IN CASE OF ID-WARNING.                          
      *---------------------------------------------------------------          
      *                                                                         
      *  JUL/08 IBM GR   - UPGRADED IN ENTERPRISE COMPILER PROJECT              
      *---------------------------------------------------------------          
                                                                                
                                                                                
           EJECT                                                                
      **********************                                                    
       ENVIRONMENT DIVISION.                                                    
      **********************                                                    
                                                                                
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-370.                                                
       OBJECT-COMPUTER. IBM-370.                                                
                                                                                
       DATA DIVISION.                                                           
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-START                            PIC X(33)                        
                 VALUE 'INCGDUMP START OF WORKING STORAGE'.                     
                                                                                
       01  WS-WORK-AREA.                                                        
                                                                                
           05  WS-SYSDATE.                                                      
               10  WS-SYSDATE-YY               PIC 9(2).                        
               10  WS-SYSDATE-MM               PIC 9(2).                        
               10  WS-SYSDATE-DD               PIC 9(2).                        
           05  WS-SYSTIME.                                                      
               10  WS-SYSTIME-HH               PIC 9(2).                        
               10  WS-SYSTIME-MM               PIC 9(2).                        
               10  WS-SYSTIME-SS               PIC 9(2).                        
               10  FILLER                      PIC 9(2).                        
       01  SNAPAID     VALUE 'SNAPAID'     PIC X(8).                            
       01  WTL         VALUE 'WTL'          PIC X(8).                           
       01  CEE3ABD     VALUE 'CEE3ABD'      PIC X(8).                           
                                                                                
       01  ABDCODE                 PIC S9(4) COMP VALUE +1043.                  
       01  ABDCLEANUP              PIC S9(4) COMP VALUE +1.                     
                                                                                
                                                                                
                                                                                
       01  WS-IEPARM               PIC 9.                                       
           88 WS-PARM-WARNING                 VALUE 5.                          
           88 WS-PARM-FATAL                   VALUE 1.                          
                                                                                
       01  INTERFACE-ERROR-MESSAGE-AREA.                                        
           COPY INRGEMSG.                                                       
                                                                                
       01  FILLER                              PIC X(31)                        
                 VALUE 'INCGDUMP END OF WORKING STORAGE'.                       
                                                                                
           EJECT                                                                
      *****************                                                         
       LINKAGE SECTION.                                                         
      *****************                                                         
                                                                                
       01  ICBM.                                                                
           COPY ICBM.                                                           
                                                                                
       01  INRGCTRL.                                                            
           COPY INRGCTRL.                                                       
                                                                                
       01  INTERFACE-DUMP-AREA.                                                 
           COPY INRGDUMP.                                                       
                                                                                
                                                                                
           EJECT                                                                
      *******************                                                       
       PROCEDURE DIVISION USING ICBM                                            
                                INRGCTRL                                        
                                INTERFACE-DUMP-AREA.                            
                                                                                
           PERFORM 1000-HOUSEKEEPING                                            
              THRU 1000-HOUSEKEEPING-EXIT.                                      
                                                                                
           PERFORM 2000-PROCESSING                                              
              THRU 2000-PROCESSING-EXIT.                                        
                                                                                
           PERFORM 3000-WRAP-UP                                                 
              THRU 3000-WRAP-UP-EXIT.                                           
                                                                                
           GOBACK.                                                              
                                                                                
      *****************************************************************         
      *    END OF MAIN PROGRAM LOGIC.                                           
      *****************************************************************         
                                                                                
                                                                                
           EJECT                                                                
       1000-HOUSEKEEPING.                                                       
                                                                                
           INITIALIZE WS-WORK-AREA.                                             
                                                                                
           ACCEPT  WS-SYSDATE   FROM DATE.                                      
           ACCEPT  WS-SYSTIME   FROM TIME.                                      
                                                                                
       1000-HOUSEKEEPING-EXIT.                                                  
           EXIT.                                                                
                                                                                
                                                                                
           EJECT                                                                
       2000-PROCESSING.                                                         
                                                                                
      *****************************************************************         
      * 1         PRINT PROGRAM NAME AND ABEND MESSAGE.                         
      *****************************************************************         
                                                                                
           MOVE   ID-PROGRAM-NAME      TO IE-PROGRAM-NAME.                      
                                                                                
           IF     ID-WARNING                                                    
             SET  IE-WARNING-ERROR-MSG TO TRUE                                  
             SET  WS-PARM-WARNING      TO TRUE                                  
                                                                                
           ELSE                                                                 
             SET  IE-FATAL-ERROR-MSG   TO TRUE                                  
             SET  WS-PARM-FATAL        TO TRUE                                  
                                                                                
           END-IF.                                                              
                                                                                
                                                                                
           MOVE   IE-ABEND-MESSAGES    TO IE-ABEND-MSG.                         
           MOVE   IE-ERROR-LINE-1      TO IE-MSG-CONTENT.                       
                                                                                
           PERFORM 2100-CALL-WTL                                                
              THRU 2100-CALL-WTL-EXIT.                                          
                                                                                
      *****************************************************************         
      * 2          PRINT ERROR MESSAGE.                                         
      *****************************************************************         
                                                                                
           MOVE   ID-ERROR-MESSAGE     TO IE-ERROR-MESSAGE.                     
           MOVE   IE-ERROR-LINE-2      TO IE-MSG-CONTENT.                       
                                                                                
           PERFORM 2100-CALL-WTL                                                
              THRU 2100-CALL-WTL-EXIT.                                          
                                                                                
                                                                                
      *****************************************************************         
      * 3         PRINT ACTION.                                                 
      *****************************************************************         
                                                                                
           MOVE   ID-ACTION           TO IE-ACTION.                             
           MOVE   IE-ERROR-LINE-3     TO IE-MSG-CONTENT.                        
                                                                                
           PERFORM 2100-CALL-WTL                                                
              THRU 2100-CALL-WTL-EXIT.                                          
                                                                                
                                                                                
      *****************************************************************         
      * 4         PRINT DATE AND TIME.                                          
      *****************************************************************         
                                                                                
           MOVE   WS-SYSDATE-YY       TO IE-ABEND-DATE-YY.                      
           MOVE   WS-SYSDATE-MM       TO IE-ABEND-DATE-MM.                      
           MOVE   WS-SYSDATE-DD       TO IE-ABEND-DATE-DD.                      
           MOVE   WS-SYSTIME-HH       TO IE-ABEND-TIME-HH.                      
           MOVE   WS-SYSTIME-MM       TO IE-ABEND-TIME-MM.                      
           MOVE   WS-SYSTIME-SS       TO IE-ABEND-TIME-SS.                      
           MOVE   IE-ERROR-LINE-4     TO IE-MSG-CONTENT.                        
                                                                                
           PERFORM 2100-CALL-WTL                                                
              THRU 2100-CALL-WTL-EXIT.                                          
                                                                                
                                                                                
      *****************************************************************         
      * 5         PRINT FILE/TABLE NAME.                                        
      *****************************************************************         
                                                                                
           MOVE   ID-LR-NAME          TO IE-LR-NAME.                            
           MOVE   IE-ERROR-LINE-5     TO IE-MSG-CONTENT.                        
                                                                                
           PERFORM 2100-CALL-WTL                                                
              THRU 2100-CALL-WTL-EXIT.                                          
                                                                                
                                                                                
      *****************************************************************         
      * 6         PRINT FILE STATUS.                                            
      *****************************************************************         
                                                                                
           MOVE    ID-LR-STATUS        TO IE-LR-STATUS                          
           MOVE    IE-ERROR-LINE-6     TO IE-MSG-CONTENT                        
           PERFORM 2100-CALL-WTL                                                
              THRU 2100-CALL-WTL-EXIT.                                          
                                                                                
                                                                                
      *****************************************************************         
      * 7         PRINT SQL CODE.                                               
      *****************************************************************         
                                                                                
           MOVE   ID-SQL-CODE         TO IE-SQL-CODE                            
           MOVE   IE-ERROR-LINE-7     TO IE-MSG-CONTENT                         
           PERFORM 2100-CALL-WTL                                                
              THRU 2100-CALL-WTL-EXIT.                                          
                                                                                
                                                                                
      *****************************************************************         
      * 8         PRINT GROUP, A/C, CERTIFICATE.                                
      *****************************************************************         
                                                                                
           MOVE   ID-GROUP            TO IE-GROUP                               
           MOVE   ID-AC               TO IE-AC                                  
           MOVE   ID-CERTIFICATE-N    TO IE-CERTIFICATE                         
           MOVE   IE-ERROR-LINE-8     TO IE-MSG-CONTENT                         
           PERFORM 2100-CALL-WTL                                                
              THRU 2100-CALL-WTL-EXIT.                                          
                                                                                
       2000-PROCESSING-EXIT.                                                    
           EXIT.                                                                
                                                                                
                                                                                
           EJECT                                                                
       2100-CALL-WTL.                                                           
                                                                                
           CALL    WTL  USING  IE-MESSAGES                                      
                               IE-WTL-FLAG                                      
                               IE-REPLY.                                        
                                                                                
       2100-CALL-WTL-EXIT.                                                      
           EXIT.                                                                
                                                                                
                                                                                
           EJECT                                                                
       3000-WRAP-UP.                                                            
                                                                                
           CALL SNAPAID.                                                        
           MOVE +13 TO RETURN-CODE.                                             
           CALL CEE3ABD    USING ABDCODE, ABDCLEANUP.                           
           DISPLAY 'TIME TO GO'.                                                
                                                                                
       3000-WRAP-UP-EXIT.                                                       
           EXIT.                                                                
                                                                                
                                                                                
      *     END   OF   PROGRAM.                                                 
                                                                                
