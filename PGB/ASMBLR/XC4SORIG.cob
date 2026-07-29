CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    XC4SORIG.                                                 
      *AUTHOR.        MIKE UKRAINEC.                                            
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.                                                            
      *DATE-COMPILED.                                                           
      *----------------------------------------------------------------*        
      *                                                                         
      *  SYSTEM    : CLIENTS II                                                 
      *                                                                         
      *  LANGUAGE  : COBOL II                                                   
      *                                                                         
      *  XC4DIGRP ACCESS ROUTINE                                                
      *                                                                         
      *  THIS MODULE WILL READ INFORMATION FROM THE IGRP FILE IF                
      *  THE INPUT CONTRACT EXISTS ON IT.                                       
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  APR 2003   C BERCH    CREATED                                     
      *  002 JUL 2008    IBM GR     UPGRADED IN ECU PROJECT                     
      *----------------------------------------------------------------*        
      /                                                                         
       ENVIRONMENT DIVISION.                                                    
                                                                                
                                                                                
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
                                                                                
         COPY XC4CIIGP.                                                         
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
         COPY XC4CDIGP.                                                         
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 PROGRAM-LIST.                                                         
          05 WS-PROGRAM-ID                  PIC X(08) VALUE 'XC4SORIG'.         
                                                                                
       01  FILLER                           PIC X(40) VALUE                     
               '**   XC4SORIG WORKING STORAGE BEGINS  **'.                      
                                                                                
       01  WS-VARIABLES.                                                        
           05 WS-IGRP-KEY                   PIC 9(7).                           
           05 WS-IGRP-LEN                   PIC S9(4) COMP.                     
           05 WS-RESP                       PIC S9(4) COMP.                     
                                                                                
       01  WS-FILE-STATUS                    PIC XX.                            
              88  WS-FILE-STATUS-OK                     VALUE '00'.             
              88  WS-FILE-STATUS-NOTFND                 VALUE '23'.             
              88  WS-FILE-STATUS-OTHER                  VALUE '99'.             
                                                                                
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      *  IGRP DEFINITION FILE LAYOUT                                            
      *----------------------------------------------------------------*        
       01  IGRP-RECORD.                                                         
           COPY XC4CFIGP.                                                       
                                                                                
       01  FILLER                           PIC X(40) VALUE                     
               '***  XC4SORIG WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  PROVIDER ACCESS ROUTINE PARMS.                                         
      *----------------------------------------------------------------*        
      *    DFHEIBLK.  IN CICS VERSION ONLY                                      
                                                                                
       01  XC4CORIG.                                                            
           COPY XC4CORIG.                                                       
                                                                                
                                                                                
      /                                                                         
       PROCEDURE DIVISION USING                                                 
      * CICS VERS WILL INSERT    DFHEIBLK                                       
                                 XC4CORIG.                                      
                                                                                
      ****************************************************************          
      *    MAINLINE                                                             
      ****************************************************************          
       0000-MAINLINE.                                                           
                                                                                
           INITIALIZE ORIG-OUTPUT-FIELDS.                                       
           SET ORIG-SUCCESSFUL TO TRUE.                                         
                                                                                
           IF ORIG-OPEN                                                         
              PERFORM 8000-OPEN    THRU 8000-OPEN-EXIT                          
           END-IF.                                                              
                                                                                
           IF ORIG-READ-KEYED                                                   
              PERFORM 2000-READ    THRU 2000-READ-EXIT                          
              PERFORM 3000-PROCESS THRU 3000-PROCESS-EXIT                       
           END-IF.                                                              
                                                                                
           IF ORIG-CLOSE                                                        
              PERFORM 9000-CLOSE   THRU 9000-CLOSE-EXIT                         
           END-IF.                                                              
                                                                                
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
                                                                                
                                                                                
      /                                                                         
      ****************************************************************          
      *    READ A RECORD FROM THE PROVIDER FILE (LOGIC IN COPYBOOK)             
      *    (BOTH CICS AND BATCH HAVE THEIR OWN CALL VERSIONS.)                  
      ****************************************************************          
       2000-READ.                                                               
                                                                                
      ***  SET UP THE KEY TO LOOK FOR.                                          
           MOVE ORIG-INPUT-CONTRACT       TO WS-IGRP-KEY.                       
                                                                                
      ***  PERFORM THE KEYED READ.                                              
           COPY XC4CRIGP.                                                       
                                                                                
       2000-READ-EXIT.                                                          
           EXIT.                                                                
                                                                                
                                                                                
      ****************************************************************          
      *    SET UP THE RETURN OUTPUT TO THE CALLING PROGRAM BASED ON             
      *    THE RESULTS OF THE PROVIDER FILE LOOK-UP.                            
      ****************************************************************          
       3000-PROCESS.                                                            
                                                                                
           IF WS-FILE-STATUS-OK                                                 
              SET  ORIG-SUCCESSFUL      TO TRUE                                 
                                                                                
              MOVE IGRP-ADMIN-SOURCE    TO ORIG-ADMIN-SOURCE                    
              MOVE IGRP-COVERAGE-SOURCE TO ORIG-COVERAGE-SOURCE                 
              IF ORIG-ADMIN-GFM                                                 
                 SET ORIG-COV-BEN-CODE-BLANK TO TRUE                            
              ELSE                                                              
                 SET ORIG-COV-BEN-CODE-VALUED TO TRUE                           
              END-IF                                                            
           ELSE                                                                 
              IF WS-FILE-STATUS-NOTFND                                          
      *          SET ORIG-NOT-FOUND    TO TRUE                                  
                 SET ORIG-ADMIN-GFM    TO TRUE                                  
                 SET ORIG-COV-NCV      TO TRUE                                  
                 SET ORIG-COV-BEN-CODE-BLANK TO TRUE                            
             ELSE                                                               
                SET ORIG-IO-ERROR      TO TRUE                                  
             END-IF                                                             
           END-IF.                                                              
                                                                                
       3000-PROCESS-EXIT.                                                       
           EXIT.                                                                
                                                                                
                                                                                
      ****************************************************************          
      *    OPEN THE INPUT FILE (PROVIDER FILE)                                  
      *    (NEEDED FOR BATCH ONLY, CICS VERSION IS A DUMMY.)                    
      ****************************************************************          
       8000-OPEN.                                                               
                                                                                
      ***  OPEN THE FILE FOR KEYED READING.                                     
           COPY XC4COIGP.                                                       
                                                                                
           IF WS-FILE-STATUS-OK                                                 
              SET  ORIG-SUCCESSFUL   TO TRUE                                    
           ELSE                                                                 
              SET ORIG-IO-ERROR      TO TRUE                                    
           END-IF.                                                              
                                                                                
       8000-OPEN-EXIT.                                                          
           EXIT.                                                                
                                                                                
      ****************************************************************          
      *    CLOSE THE INPUT FILE (PROVIDER FILE)                                 
      *    (NEEDED FOR BATCH ONLY, CICS VERSION IS A DUMMY.)                    
      ****************************************************************          
       9000-CLOSE.                                                              
                                                                                
      ***  CLOSE THE FILE                                                       
           COPY XC4CCIGP.                                                       
                                                                                
       9000-CLOSE-EXIT.                                                         
           EXIT.                                                                
                                                                                
