SCANLINE START                                                                  
SCANLINE AMODE 24                                IBM-ECU                00002000
SCANLINE RMODE 24                                IBM-ECU                00002000
*                                                                               
*  CALL WITH 4 PARAMETERS:                                                      
*       1) THE AREA TO BE SCANNED                                               
*       2) FULLWORD CONTAINING THE NUMBER OF BYTES TO BE SCANNED                
*       3) FILL CHARACTER IN THE AREA TO BE SCANNED                             
*       4) RETURN AREA - AN AREA CONTAINING DOUBLEWORDS FOR EACH                
*          POSSIBLE REPLY. THE FIRST FULLWORD CONTAINS THE ADDRESS              
*          OF THE REPLY AND THE SECOND FULLWORD CONTAINS THE NUMBER             
*          OF BYTES IN THE REPLY                                                
*                                                                               
*  THE SEARCH STRING IS DELIMITED BY A DOUBLEWORD CONTAINING ZEROES             
*                                                                               
* HISTORY                                                                       
* 18JUN08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT                 
*--------------------------------------------------------------*                
*                                                                               
         PRINT NOGEN                                                            
         REGEQU                                                                 
         SAVE  (14,12)                                                          
         BALR  R11,R0                                                           
         USING *,R11,R12                                                        
         LA    R12,2048(R11)                                                    
         LA    R12,2048(R12)                                                    
         ST    R13,MYSAVE+4                                                     
         LA    R13,MYSAVE                                                       
         LM    R7,R10,0(R1)                                                     
*                                                                               
         L     R8,0(R8)                                                         
         AR    R8,R7                                                            
         BCTR  R8,0                                                             
*                                                                               
         SR    R4,R4                                                            
         IC    R4,0(R9)                                                         
         MVI   TRTABLE1,X'FF'                                                   
         MVC   TRTABLE1+1(255),TRTABLE1                                         
         LA    R5,TRTABLE1                                                      
         AR    R5,R4                                                            
         MVI   0(R5),X'00'                                                      
         MVI   TRTABLE2,X'00'                                                   
         MVC   TRTABLE2+1(255),TRTABLE2                                         
         LA    R5,TRTABLE2                                                      
         AR    R5,R4                                                            
         MVI   0(R5),X'FF'                                                      
*                                                                               
GETNEXT  EQU   *                                                                
         CR    R7,R8                                                            
         BH    GOBACK                                                           
         L     R4,=F'255'                                                       
         LR    R5,R8                                                            
         SR    R5,R7                                                            
         CR    R4,R5                                                            
         BNH   *+6                                                              
         LR    R4,R5                                                            
         STC   R4,TRAN1+1                                                       
         SR    R1,R1                                                            
TRAN1    TRT   0(0,R7),TRTABLE1                                                 
         BZ    GOBACK                                                           
         ST    R1,0(R10)                                                        
         LR    R6,R1                                                            
         L     R4,=F'255'                                                       
         LR    R5,R8                                                            
         SR    R5,R6                                                            
         CR    R4,R5                                                            
         BNH   *+6                                                              
         LR    R4,R5                                                            
         STC   R4,TRAN2+1                                                       
         SR    R1,R1                                                            
TRAN2    TRT   0(0,R6),TRTABLE2                                                 
         BNZ   *+8                                                              
         LA    R1,1(R8)                                                         
         LR    R7,R1                                                            
         SR    R1,R6                                                            
         ST    R1,4(R10)                                                        
         LA    R10,8(R10)                                                       
         B     GETNEXT                                                          
*                                                                               
GOBACK   EQU   *                                                                
         XC    0(8,R10),0(R10)                                                  
         L     R13,MYSAVE+4                                                     
         RETURN (14,12)                                                         
*                                                                               
MYSAVE   DS    18F                                                              
TRTABLE1 DS    CL256                                                            
TRTABLE2 DS    CL256                                                            
         END                                                                    
