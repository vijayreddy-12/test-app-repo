*******************************************************************             
* HISTORY LOG                                                                   
* 02JUL082008   IBM GR   UPGRADED IN ENTERPRISE COMPILER PROJECT                
*******************************************************************             
ZPARDUMP START                                                                  
         PRINT NOGEN                                                            
         COPY  REGISTER                                                         
*                                                                               
*  HOUSEKEEPING.                                                                
*                                                                               
         SAVE  (14,12),,*                                                       
         BALR  R12,0                                                            
         USING *,R12                                                            
         ST    R13,MYSAVE+4                                                     
         LA    R13,MYSAVE                                                       
         LM    R3,R5,0(R1)                                                      
*                                                                               
*  GO DO THE REQUESTED FUNCTION.                                                
*                                                                               
         CLI   0(R3),C'1'                                                       
         BE    OPTION1                                                          
         CLI   0(R3),C'2'                                                       
         BE    OPTION2                                                          
         CLI   0(R3),C'3'                                                       
         BE    OPTION3                                                          
         CLI   0(R3),C'4'                                                       
         BE    OPTION4                                                          
         CLI   0(R3),C'5'                                                       
         BE    OPTION5                                                          
         CLI   0(R3),C'6'                                                       
         BE    OPTION6                                                          
         CLI   0(R3),C'7'                                                       
         BE    OPTION7                                                          
         CLI   0(R3),C'8'                                                       
         BE    OPTION8                                                          
         B     GOBACK                                                           
*                                                                               
*  DO OPTION 1.                                                                 
*                                                                               
OPTION1  EQU   *                                                                
         WTO   'ZPARDUMP PRODUCED - ABEND REQUESTED'                            
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,STORAGE=((4),(5)),PDATA=(PSW,REGS)                   
         CLOSE SNAPDCB                                                          
         ABEND 0                                                                
*                                                                               
*  DO OPTION 2.                                                                 
*                                                                               
OPTION2  EQU   *                                                                
         WTO   'ZPARDUMP PRODUCED - ABEND REQUESTED'                            
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,PDATA=(JPA,PSW,REGS)                                 
         CLOSE SNAPDCB                                                          
         ABEND 0                                                                
*                                                                               
*  DO OPTION 3.                                                                 
*                                                                               
OPTION3  EQU   *                                                                
         B     OPTION1                                                          
*                                                                               
*  DO OPTION 4.                                                                 
*                                                                               
OPTION4  EQU   *                                                                
         B     OPTION2                                                          
*                                                                               
*  DO OPTION 5.                                                                 
*                                                                               
OPTION5  EQU   *                                                                
         WTO   'ZPARDUMP PRODUCED - PROGRAM CONTINUING'                         
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,STORAGE=((4),(5)),PDATA=(PSW,REGS)                   
         CLOSE SNAPDCB                                                          
         B     GOBACK                                                           
*                                                                               
*  DO OPTION 6.                                                                 
*                                                                               
OPTION6  EQU   *                                                                
         WTO   'ZPARDUMP PRODUCED - PROGRAM CONTINUING'                         
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,PDATA=(JPA,PSW,REGS)                                 
         CLOSE SNAPDCB                                                          
         B     GOBACK                                                           
*                                                                               
*  DO OPTION 7.                                                                 
*                                                                               
OPTION7  EQU   *                                                                
         B     OPTION5                                                          
*                                                                               
*  DO OPTION 8.                                                                 
*                                                                               
OPTION8  EQU   *                                                                
         B     OPTION6                                                          
*                                                                               
*  RETURN TO THE CALLING PROGRAM.                                               
*                                                                               
GOBACK   EQU   *                                                                
         SR    R15,R15                                                          
         L     R13,MYSAVE+4                                                     
         RETURN (14,12),T,RC=(15)                                               
*                                                                               
MYSAVE   DS    18F                THIS PROGRAM'S SAVE AREA                      
*                                                                               
         DS    0D                                                               
SNAPDCB  DCB   DDNAME=SYSUDUMP,                                        C        
               DSORG=PS,                                               C        
               RECFM=VBA,                                              C        
               BLKSIZE=1632,                                           C        
               LRECL=125,                                              C        
               MACRF=(W)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         END                                                                    
