*********H************************************************************* 00041   
*     ***H     DATE       - 16APR94                                     00042   
*     ***H     PROGRAMMER - D.B. EMBURY                                 00043   
*     ***H     ACTION     - RATIONALIZED THE I.T.S. AND GLH VERSIONS OF 00045   
*     ***H                  THIS MODULE.  THE I.T.S. VERSION HAD        00045   
*     ***H                  LOGIC INCLUDED TO STOP (ABEND) ANY ATTEMPT  00045   
*     ***H                  TO GET A REPLY FROM THE CONSOLE.  THIS      00045   
*     ***H                  HAS BEEN ELIMINATED, THUS MAKING IT         00045   
*     ***H                  ESSENTIALLY EQUIVALENT TO THE GLH VERSION.  00045   
*     ***H                  WHERE ANY LOGIC IS DIFFERENT, THE OTHER     00045   
*     ***H                  VERSION'S STATEMENTS ARE INCLUDED           00045   
*     ***H                  AS COMMENTS.                                00045   
*********H************************************************************* 00046   
         SPACE 2                                                        00047   
OPGOSSIP START                                                                  
         COPY  REGISTER                                                         
*                                                                               
*  REGISTER 1  - WORK REGISTER                                                  
*  REGISTER 2  - WORK REGISTER                                                  
*  REGISTER 3  - PARAMETER 1 FROM CALLING PROGRAM                               
*  REGISTER 4  - PARAMETER 2 FROM CALLING PROGRAM                               
*  REGISTER 5  - PARAMETER 3 FROM CALLING PROGRAM                               
*  REGISTER 6  - POINTS TO MESSAGE TEXT AREA IN WTO MACRO                       
*  REGISTER 7  - POINTS TO ROUTE CODES IN WTO MACRO                             
*  REGISTER 8  - POINTS TO MESSAGE TEXT AREA IN WTOR MACRO                      
*  REGISTER 9  - POINTS TO ROUTE CODES IN WTOR MACRO                            
*  REGISTER 10 - WORK REGISTER                                                  
*  REGISTER 11 - BASE REGISTER                                                  
*  REGISTER 12 - BASE REGISTER                                                  
*                                                                               
         SAVE  (14,12),,*                                                       
         BALR  R11,R0                                                           
         USING *,R11,R12                                                        
         LA    R12,2048(R11)                                                    
         LA    R12,2048(R12)                                                    
         ST    R13,MYSAVE+4                                                     
         LA    R13,MYSAVE                                                       
         LM    R3,R5,0(R1)                                                      
         USING PARM1,R3                                                         
         USING PARM2,R4                                                         
         USING PARM3,R5                                                         
         LA    R6,OPMACRO1+8                                                    
         LA    R7,OPMACRO1+131                                                  
         LA    R8,OPMACRO2+16                                                   
         LA    R9,OPMACRO2+139                                                  
*                                                                               
*  INITIALIZE THE ERROR CODE. THEN VERIFY THAT THE PARAMETER                    
*  STRING CONTAINS ONLY ZEROES AND ONES.                                        
*                                                                               
         MVI   ERRCODE,X'00'                                                    
         TRT   PARM1SW,MUSTBE01                                                 
         BNZ   GOTANERR                                                         
*                                                                               
*  USE A MESSAGE LENGTH OF 1 TO 121.                                            
*                                                                               
CHKLGTH  EQU   *                                                                
         ZAP   WORKLGTH,MSGLGTH                                                 
         CP    MSGLGTH,=P'1'                                                    
         BL    GOTANERR                                                         
         CP    MSGLGTH,=P'121'                                                  
         BNH   CLRAREAS                                                         
         ZAP   WORKLGTH,=P'121'                                                 
*                                                                               
*  CLEAR THE MESSAGE AREAS AND THE REPLY AREA AND THEN MOVE THE                 
*  MESSAGE TEXT TO BE DISPLAYED.                                                
*                                                                               
CLRAREAS EQU   *                                                                
         MVI   PRNTLINE,C' '                                                    
         MVC   PRNTLINE+1(132),PRNTLINE                                         
         MVI   0(R6),C' '                                                       
         MVC   1(120,R6),0(R6)                                                  
         MVI   0(R8),C' '                                                       
         MVC   1(120,R8),0(R8)                                                  
         MVI   REPLY,C' '                                                       
         MVC   REPLY+1(114),REPLY                                               
         ZAP   WORKFLD,WORKLGTH                                                 
         CVB   R10,WORKFLD                                                      
         BCTR  R10,R0                                                           
         EX    R10,MOVEMSG1                                                     
         EX    R10,MOVEMSG2                                                     
         EX    R10,MOVEMSG3                                                     
*                                                                               
*  OPEN THE PRINT DCB IF PRINTING IS REQUIRED.                                  
*                                                                               
         CLI   PRINTSW,C'0'                                                     
         BE    SETCODES                                                         
         OPEN  (PRINTDCB,(OUTPUT))                                              
*                                                                               
*  SET UP THE CONSOLE ROUTE CODES IF REQUIRED.                                  
*                                                                               
SETCODES EQU   *                                                                
         CLC   RCODES,ZEROES                                                    
         BE    PUTIT                                                            
         XC    0(2,R7),0(R7)                                                    
         XC    0(2,R9),0(R9)                                                    
         LA    R1,RCODES                                                        
         LA    R2,WTOCODES                                                      
         LA    R10,16                                                           
CODELOOP EQU   *                                                                
         CLI   0(R1),C'0'                                                       
         BE    CODEXIT                                                          
         OC    0(2,R7),0(R2)                                                    
         OC    0(2,R9),0(R2)                                                    
CODEXIT  EQU   *                                                                
         LA    R1,1(R1)                                                         
         LA    R2,2(R2)                                                         
         BCT   R10,CODELOOP                                                     
*                                                                               
*  WRITE TO THE PRINTER AND/OR CONSOLE AS REQUIRED.                             
*                                                                               
PUTIT    EQU   *                                                                
         CLI   PRINTSW,C'0'                                                     
         BE    PUTITA                                                           
         PUT   PRINTDCB,PRNTLINE                                                
PUTITA   EQU   *                                                                
         CLC   RCODES,ZEROES                                                    
         BE    CLOSEIT                                                          
         CLI   REPLYSW,C'1'                                                     
         BE    PUTITB              WE NEED OPERATOR INTERVENTION                
         BAL   R10,NOREPLY                                                      
         B     CLOSEIT                                                          
********C************************************************************** 00041   
*     **C*     THE FOLLOWING 7 COMMENT LINES ARE FROM THE               00042   
*     **C*     I.T.S. VERSION.                                          00042   
********C************************************************************** 00041   
*                                                                               
*  THE TORONTO SYSTEM DOES NOT WANT ANY INTERROGATION OF THE OPERATOR           
*  CONSOLE; THEREFORE WE WILL JUST DO A 'WTO' RATHER THAN A 'WTOR'              
*  INSTRUCTION AND THEN ABEND THE JOB.                                          
*       THE ORIGINAL CODE IS STILL INTACT BUT COMMENTED OUT.                    
*       ALL CODE THAT HAS BEEN ADDED IS STAMPED AS DT0786                       
*                                                                               
PUTITB   EQU   *                                                                
********C************************************************************** 00041   
*     **C*     THE FOLLOWING 8 STATEMENTS (COMMENTED OUT) ARE FROM      00042   
*     **C*     THE I.T.S. VERSION.                                      00042   
********C************************************************************** 00041   
**        BAL   R10,GETREPLY                                                    
*         BAL   R10,NOREPLY         DISPLAY THE QUESTION      DT0786            
*         CLI   PRINTSW,C'0'        IS THE PRINT DCB OPEN     DT0786            
**        BE    CLOSEIT                                                         
*         BNE   PRTABEND            YES.. PRINT ABEND MESSAGE DT0786            
*         OPEN  (PRINTDCB,(OUTPUT))                           DT0786            
*         PUT   PRINTDCB,PRNTLINE   PRINT CONSOLE MESSAGE     DT0786            
*PRTABEND EQU   *                                             DT0786            
         BAL   R10,GETREPLY                                                     
         CLI   PRINTSW,C'0'                                                     
         BE    CLOSEIT                                                          
         MVI   PRNTLINE,C' '                                                    
         MVC   PRNTLINE+1(132),PRNTLINE                                         
********C************************************************************** 00041   
*     **C*     THE FOLLOWING 4 STATEMENTS (COMMENTED OUT) ARE FROM      00042   
*     **C*     THE I.T.S. VERSION.                                      00042   
********C************************************************************** 00041   
**        MVC   PRNTLINE+1(115),REPLY                                           
*         LA    R2,5                 LOOP CONTROL             DT0786            
*PRTLOOP  EQU   *                                             DT0785            
*         MVC   PRNTLINE+1(80),ABENDMSG                       DT0786            
         MVC   PRNTLINE+1(115),REPLY                                            
         PUT   PRINTDCB,PRNTLINE                                                
********C************************************************************** 00041   
*     **C*     THE FOLLOWING 3 STATEMENTS (COMMENTED OUT) ARE FROM      00042   
*     **C*     THE I.T.S. VERSION.                                      00042   
********C************************************************************** 00041   
*         BCT   R2,PRTLOOP                                    DT0786            
*         CLOSE (PRINTDCB)                                    DT0786            
*         ABEND 999                                           DT0786            
*                                                                               
*  CLOSE THE PRINT DCB IF PRINTING WAS REQUIRED.                                
*                                                                               
CLOSEIT  EQU   *                                                                
         CLI   PRINTSW,C'0'                                                     
         BE    GOBACK                                                           
         CLOSE (PRINTDCB)                                                       
*                                                                               
*  RETURN TO THE CALLING PROGRAM.                                               
*                                                                               
GOBACK   EQU   *                                                                
         SR    R15,R15                                                          
         L     R13,MYSAVE+4                                                     
         RETURN (14,12),T,RC=(15)                                               
*                                                                               
*  GOT AN ERROR. FLAG IT AND GET THE HELL OUT.                                  
*                                                                               
GOTANERR EQU   *                                                                
         MVI   ERRCODE,X'FF'                                                    
         B     GOBACK                                                           
*                                                                               
*  WRITE TO THE OPERATOR CONSOLE WITH NO REPLY.                                 
*                                                                               
NOREPLY  EQU   *                                                                
         B     OPMACRO1                                                         
         DS    0D                                                               
OPMACRO1 WTO   '                                                       C        
                                                                       C        
                         ',                                            C        
               ROUTCDE=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)                 
         BR    R10                                                              
*                                                                               
*  WRITE TO THE OPERATOR CONSOLE AND GET A REPLY.                               
*                                                                               
GETREPLY EQU   *                                                                
         XC    ECBAD,ECBAD                                                      
         B     OPMACRO2                                                         
         DS    0D                                                               
OPMACRO2 WTOR  '                                                       C        
                                                                       C        
                         ',(5),115,ECBAD,                              C        
               ROUTCDE=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)                 
         WAIT  ECB=ECBAD                                                        
         BR    R10                                                              
*                                                                               
*  WORKING STORAGE.                                                             
*                                                                               
         DC    C'***** OPGOSSIP EYECATCHER *****'                               
         DS    0F                                                               
MOVEMSG1 MVC   PRNTLINE+1(0),MSGTEXT                                            
MOVEMSG2 MVC   0(0,R6),MSGTEXT                                                  
MOVEMSG3 MVC   0(0,R8),MSGTEXT                                                  
********C************************************************************** 00041   
*     **C*     THE FOLLOWING 1 STATEMENT (COMMENTED OUT) IS FROM        00042   
*     **C*     THE I.T.S. VERSION.                                      00042   
********C************************************************************** 00041   
*ABENDMSG DC    CL80'*** JOB ABENDED DUE TO CONSOLE INTERROGATION ***'          
         DS    0F                                                               
MYSAVE   DS    18F           THIS PROGRAM'S SAVE AREA                           
WORKFLD  DS    D             WORK FIELD FOR CVB INSTRUCTION                     
WORKLGTH DS    PL2           LENGTH OF MESSAGE DISPLAYED                        
ECBAD    DS    F             EVENT CONTROL BLOCK                                
PRNTLINE DS    CL133         PRINT LINE TO SYSPRINT                             
ZEROES   DC    16C'0'                                                           
         DS    0F                                                               
MUSTBE01 DC    256X'FF'                                                         
         ORG   MUSTBE01+C'0'                                                    
         DC    2X'00'                                                           
         ORG                                                                    
         DS    0F                                                               
WTOCODES DC    B'1000000000000000'                                              
         DC    B'0100000000000000'                                              
         DC    B'0010000000000000'                                              
         DC    B'0001000000000000'                                              
         DC    B'0000100000000000'                                              
         DC    B'0000010000000000'                                              
         DC    B'0000001000000000'                                              
         DC    B'0000000100000000'                                              
         DC    B'0000000010000000'                                              
         DC    B'0000000001000000'                                              
         DC    B'0000000000100000'                                              
         DC    B'0000000000000000'     NOT USED                                 
         DC    B'0000000000000000'     NOT USED                                 
         DC    B'0000000000000000'     NOT USED                                 
         DC    B'0000000000000000'     NOT USED                                 
         DC    B'0000000000000000'     NOT USED                                 
         DS    0F                                                               
********C************************************************************** 00041   
*     **C*     THE FOLLOWING 3 STATEMENTS (COMMENTED OUT) ARE FROM      00042   
*     **C*     THE I.T.S. VERSION.                                      00042   
********C************************************************************** 00041   
*PRINTDCB DCB   DDNAME=SYSPRINT,                                                
*               DSORG=PS,                                                       
*               MACRF=(PM)                                                      
PRINTDCB DCB   DDNAME=SYSPRINT,                                        C        
               DSORG=PS,BLKSIZE=121,RECFM=F,LRECL=121,                 C        
               MACRF=(PM)                                                       
*                                                                               
         LTORG                                                                  
*                                                                               
*  PARAMETER1 FROM CALLING PROGRAM.                                             
*                                                                               
PARM1    DSECT                                                                  
PARM1SW  DS    0CL18                                                            
RCODES   DS    CL16                                                             
REPLYSW  DS    CL1                                                              
PRINTSW  DS    CL1                                                              
MSGLGTH  DS    PL2                                                              
ERRCODE  DS    CL1                                                              
*                                                                               
*  PARAMETER2 FROM CALLING PROGRAM.                                             
*                                                                               
PARM2    DSECT                                                                  
MSGTEXT  DS    CL1                                                              
*                                                                               
*  PARAMETER3 FROM CALLING PROGRAM.                                             
*                                                                               
PARM3    DSECT                                                                  
REPLY    DS    CL115                                                            
*                                                                               
         END                                                                    
