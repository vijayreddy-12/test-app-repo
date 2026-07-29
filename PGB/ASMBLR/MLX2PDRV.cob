CBL TRUNC(OPT),LIST,DATA(31)                                                    
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    MLX2PDRV.                                                 
      *              PROGRAM CONVERTED BY                                       
      *              CCCA FOR OS/390 & MVS & VM 5648-B05                        
      *              CONVERSION DATE 07/24/08 07:27:31.                         
      *AUTHOR.        JIM KLAPWYK.                                              
      *INSTALLATION.  MANULIFE.                                                 
      *DATE-WRITTEN.                                                            
      *DATE-COMPILED.                                                           
      *----------------------------------------------------------------*        
      *                                                                         
      *  SYSTEM    : GROUP ELIGIBILITY                                          
      *                                                                         
      *  LANGUAGE  : COBOL II                                                   
      *                                                                         
      *                                                                         
      *--HISTORY LOG--------------------------------------------------          
      *  SEQ  DATE       DESIGNER   DESCRIPTION                                 
      *  ---  ---------  ---------  --------------------------------            
      *  001  MAR 1998   J KLAPWYK  CREATED                                     
      *----------------------------------------------------------------*        
      /                                                                         
       ENVIRONMENT DIVISION.                                                    
                                                                                
       CONFIGURATION SECTION.                                                   
                                                                                
       SOURCE-COMPUTER. IBM-370.                                                
       OBJECT-COMPUTER. IBM-370.                                                
                                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
      /                                                                         
       WORKING-STORAGE SECTION.                                                 
       01  FILLER                     PIC X(40) VALUE                           
               '**   MLX2PDRV WORKING STORAGE BEGINS  **'.                      
                                                                                
       01  VARIABLES.                                                           
           05  WS-RETURNS.                                                      
               10  PBIX-RETURN        PIC 9(2).                                 
               10  PPUT-RETURN        PIC 9(2).                                 
               10  PGET-RETURN        PIC 9(2).                                 
               10  PCLR-RETURN        PIC 9(2).                                 
                                                                                
       01  WS-CALLED-MODULES.                                                   
           05  MLX2PBIX               PIC X(8) VALUE 'MLX2PBIX'.                
           05  MLX2PPUT               PIC X(8) VALUE 'MLX2PPUT'.                
           05  MLX2PGET               PIC X(8) VALUE 'MLX2PGET'.                
           05  MLX2PCLR               PIC X(8) VALUE 'MLX2PCLR'.                
                                                                                
      *----------------------------------------------------------------*        
      *  INDEX DEFINITION                                                       
      *----------------------------------------------------------------*        
       01  INDEX-DEFN.                                                          
           COPY MLX2INDX.                                                       
                                                                                
      *----------------------------------------------------------------*        
      *  COMMON FIELDS                                                          
      *----------------------------------------------------------------*        
       01  COMM-FIELDS.                                                         
           COPY MLX2COMM.                                                       
                                                                                
       01  FILLER                    PIC X(40) VALUE                            
               '***  MLX2PDRV WORKING STORAGE ENDS   ***'.                      
                                                                                
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *  INPUT AREA                                                             
      *----------------------------------------------------------------*        
       01  INPUT-PARMS.                                                         
           COPY MLX2INPT.                                                       
      *----------------------------------------------------------------*        
      *  RETURN AREA                                                            
      *----------------------------------------------------------------*        
       01  PARSER-RETURNS.                                                      
           COPY MLX2RTRN.                                                       
      *----------------------------------------------------------------*        
      *  COMMON AREA                                                            
      *----------------------------------------------------------------*        
       01  AREA1                     PIC X(1).                                  
       01  AREA2                     PIC X(1).                                  
       01  AREA3                     PIC X(1).                                  
                                                                                
      /                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION USING INPUT-PARMS                                     
                                PARSER-RETURNS                                  
                                AREA1                                           
                                AREA2                                           
                                AREA3.                                          
      *----------------------------------------------------------------*        
                                                                                
      *                                                                         
       0000-MAINLINE.                                                           
            PERFORM 1000-INIT THRU                                              
                    1000-INIT-EXIT.                                             
                                                                                
            IF RTRN-CODE = 0                                                    
               PERFORM 2000-PARSE THRU                                          
                       2000-PARSE-EXIT                                          
            END-IF.                                                             
                                                                                
            PERFORM 3000-SETUP-RETURN THRU                                      
                    3000-SETUP-RETURN-EXIT.                                     
       0000-MAINLINE-EXIT.                                                      
           GOBACK.                                                              
      /                                                                         
      *----------------------------------------------------------------*        
      * INITIALIZATION ROUTINE.                                                 
      *----------------------------------------------------------------*        
       1000-INIT.                                                               
           MOVE 0            TO RTRN-CODE                                       
                                RTRN-TRAILER-OCCUR                              
                                RTRN-PACKET-TYPE                                
                                RTRN-INDEX-HANDLE.                              
                                                                                
           MOVE SPACE        TO RTRN-SEGMENT-VERSION.                           
                                                                                
           IF INPT-ACTION-CLEAR                                                 
           AND INPT-SECTION = SPACE                                             
              NEXT SENTENCE                                                     
           ELSE                                                                 
              IF INPT-INDEX-HANDLE-X = LOW-VALUES                               
              OR COMM-HOLD-INDEX-HANDLE-X = LOW-VALUES                          
              OR INPT-INDEX-HANDLE NOT = COMM-HOLD-INDEX-HANDLE                 
              OR COMM-REBUILD-INDEX                                             
                 PERFORM 4000-BUILD-INDEX THRU                                  
                         4000-BUILD-INDEX-EXIT                                  
                 IF PBIX-RETURN NOT = 0                                         
                    MOVE PBIX-RETURN TO RTRN-CODE                               
                 END-IF                                                         
              END-IF                                                            
           END-IF.                                                              
                                                                                
       1000-INIT-EXIT.                                                          
           EXIT.                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      * CALL APPROPRIATE PARSER MODULE BASED ON INPUT VARIABLES                 
      *----------------------------------------------------------------*        
       2000-PARSE.                                                              
           EVALUATE TRUE                                                        
              WHEN INPT-ACTION-GET                                              
                 PERFORM 4200-GET THRU                                          
                         4200-GET-EXIT                                          
                 IF PGET-RETURN NOT = 0                                         
                    MOVE PGET-RETURN TO RTRN-CODE                               
                    GO TO 2000-PARSE-EXIT                                       
                 END-IF                                                         
              WHEN INPT-ACTION-PUT                                              
                 PERFORM 4300-PUT THRU                                          
                         4300-PUT-EXIT                                          
                 IF PPUT-RETURN NOT = 0                                         
                    MOVE PPUT-RETURN TO RTRN-CODE                               
                    GO TO 2000-PARSE-EXIT                                       
                 END-IF                                                         
              WHEN INPT-ACTION-CLEAR                                            
                 PERFORM 4400-CLR THRU                                          
                         4400-CLR-EXIT                                          
                 IF PCLR-RETURN NOT = 0                                         
                    MOVE PCLR-RETURN TO RTRN-CODE                               
                    GO TO 2000-PARSE-EXIT                                       
                 END-IF                                                         
              WHEN OTHER                                                        
                 MOVE 52 TO RTRN-CODE                                           
                 GO TO 2000-PARSE-EXIT                                          
           END-EVALUATE.                                                        
                                                                                
       2000-PARSE-EXIT.                                                         
           EXIT.                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      * SET UP RETURNS                                                          
      *----------------------------------------------------------------*        
       3000-SETUP-RETURN.                                                       
           IF RTRN-CODE NOT = 0                                                 
           AND RTRN-CODE NOT = 08                                               
              GO TO 3000-SETUP-RETURN-EXIT                                      
           END-IF.                                                              
                                                                                
           PERFORM 3500-HANDLE THRU                                             
                   3500-HANDLE-EXIT.                                            
                                                                                
           MOVE COMM-HOLD-INDEX-HANDLE TO RTRN-INDEX-HANDLE.                    
                                                                                
       3000-SETUP-RETURN-EXIT.                                                  
           EXIT.                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      * BUILD INDEX HANDLE                                                      
      *----------------------------------------------------------------*        
       3500-HANDLE.                                                             
      *                                                                         
      *   BATCH OR CICS                                                         
      *     CALL TO RECIEVE SYSTEM TIME                                         
      *       TO BE PLACED IN COMM-INDEX-HANDLE                                 
      *                                                                         
      *                                                                         
           COPY MLX2HNDL.                                                       
                                                                                
       3500-HANDLE-EXIT.                                                        
           EXIT.                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      * CALL BUILD INDEX MODULE                                                 
      *----------------------------------------------------------------*        
       4000-BUILD-INDEX.                                                        
      *                                                                         
      * BATCH OR CICS CALL                                                      
      *                                                                         
      *    CALL MLX2PBIX USING INPUT-PARMS                                      
      *                        COMM-FIELDS                                      
      *                        INDEX-DEFN                                       
      *                        AREA1                                            
      *                        PBIX-RETURN.                                     
      *                                                                         
           COPY MLX2CBIX.                                                       
                                                                                
       4000-BUILD-INDEX-EXIT.                                                   
           EXIT.                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      * CALL TO GET MODULE                                                      
      *----------------------------------------------------------------*        
       4200-GET.                                                                
      *                                                                         
      * BATCH OR CICS CALL                                                      
      *                                                                         
      *          CALL MLX2PGET USING INPUT-PARMS                                
      *                              PARSER-RETURNS                             
      *                              COMM-FIELDS                                
      *                              INDEX-DEFN                                 
      *                              MSG-AREA1                                  
      *                              MSG-AREA3                                  
      *                              PGET-RETURNS.                              
      *                                                                         
           COPY MLX2CGET.                                                       
                                                                                
       4200-GET-EXIT.                                                           
           EXIT.                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      * CALL TO PUT MODULE                                                      
      *----------------------------------------------------------------*        
       4300-PUT.                                                                
      *                                                                         
      * BATCH OR CICS CALL                                                      
      *                                                                         
      *          CALL MLX2PPUT USING INPUT-PARMS                                
      *                              PARSER-RETURNS                             
      *                              COMM-FIELDS                                
      *                              INDEX-DEFN                                 
      *                              AREA1                                      
      *                              AREA2                                      
      *                              AREA3                                      
      *                              PPUT-RETURN.                               
      *                                                                         
           COPY MLX2CPUT.                                                       
                                                                                
       4300-PUT-EXIT.                                                           
           EXIT.                                                                
      /                                                                         
      *----------------------------------------------------------------*        
      * CALL TO CLEAR MODULE                                                    
      *----------------------------------------------------------------*        
       4400-CLR.                                                                
      *                                                                         
      * BATCH OR CICS CALL                                                      
      *                                                                         
      *          CALL MLX2PCLR USING INPUT-PARMS                                
      *                              PARSER-RETURNS                             
      *                              COMM-FIELDS                                
      *                              INDEX-DEFN                                 
      *                              AREA1                                      
      *                              AREA2                                      
      *                              AREA3                                      
      *                              PCLR-RETURN.                               
      *                                                                         
           COPY MLX2CCLR.                                                       
                                                                                
       4400-CLR-EXIT.                                                           
           EXIT.                                                                
      /                                                                         
