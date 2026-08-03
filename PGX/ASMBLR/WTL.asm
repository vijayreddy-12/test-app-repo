WTL      START                                                                  
         PRINT NOGEN                                                            
         COPY  REGISTER                                                         
*                                                                               
*  REGISTER 3  - PARAMETER 1 FROM CALLING PROGRAM                               
*  REGISTER 4  - PARAMETER 2 FROM CALLING PROGRAM                               
*  REGISTER 5  - PARAMETER 3 FROM CALLING PROGRAM                               
*  REGISTER 6  - PARAMETER 4 FROM CALLING PROGRAM                               
*  REGISTER 9  - WORK REGISTER                                                  
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
         LM    R3,R6,0(R1)                                                      
         USING WTLP1,R3                                                         
         USING WTLP2,R4                                                         
         USING WTLP3,R5                                                         
         USING WTLP4,R6                                                         
*                                                                               
*  INITIALIZE FIELDS FOR THE CALL TO OPGOSSIP.                                  
*                                                                               
         LA    R9,MSG                                                           
         ST    R9,OPGSPPRM+4                                                    
         MVC   RCODES,=C'0000000000000000'                                      
         MVI   REPLYSW,C'0'                                                     
         MVI   PRINTSW,C'0'                                                     
         ZAP   TXTLGTH,MSGLGTH                                                  
         SP    TXTLGTH,=P'1'                                                    
*                                                                               
*  DETERMINE HOW MANY PARAMETERS HAVE BEEN PASSED.                              
*                                                                               
         CLM   R3,B'1000',=X'80'                                                
         BE    DEFAULT2                                                         
         CLM   R4,B'1000',=X'80'                                                
         BE    DEFAULT1                                                         
         B     DEFAULT0                                                         
*                                                                               
*  SET UP THE DEFAULTS FOR BOTH FLAG AND REPLY.                                 
*                                                                               
DEFAULT2 EQU   *                                                                
         MVI   SAVFLAG,C'P'                                                     
         MVC   SAVREPLY,=C'NO '                                                 
         B     GODOIT                                                           
*                                                                               
*  SAVE THE PASSED FLAG AND SET UP THE DEFAULT FOR REPLY.                       
*                                                                               
DEFAULT1 EQU   *                                                                
         MVC   SAVFLAG,FLAG                                                     
         MVC   SAVREPLY,=C'NO '                                                 
         B     GODOIT                                                           
*                                                                               
*  SAVE THE PASSED FLAG AND REPLY FIELDS.                                       
*                                                                               
DEFAULT0 EQU   *                                                                
         MVC   SAVFLAG,FLAG                                                     
         MVC   SAVREPLY,REPLY                                                   
*                                                                               
*  SET UP THE REST OF PARAMETER1 AND THEN CALL OPGOSSIP. IF A                   
*  REPLY IS EXPECTED TRY TO GET THE REPLY FROM THE WTL RESPONSE                 
*  FILE FIRST. IF THE MESSAGE CANNOT BE PROCESSED THE OPERATOR                  
*  WILL HAVE TO GET INVOLVED.                                                   
*                                                                               
GODOIT   EQU   *                                                                
         CLI   SAVFLAG,C'E'                                                     
         BE    GOBACK                                                           
         CLI   SAVFLAG,C'C'                                                     
         BE    GODOITA                                                          
         CLI   SAVFLAG,C'P'                                                     
         BE    GODOITB                                                          
         CLI   SAVFLAG,C'B'                                                     
         BE    GODOITC                                                          
GODOITA  EQU   *                                                                
         MVI   RCODES+10,C'1'                                                   
         B     GODOITD                                                          
GODOITB  EQU   *                                                                
         MVI   PRINTSW,C'1'                                                     
         B     GODOITD                                                          
GODOITC  EQU   *                                                                
         MVI   RCODES+10,C'1'                                                   
         MVI   PRINTSW,C'1'                                                     
GODOITD  EQU   *                                                                
         CLC   SAVREPLY(2),=C'NO'                                               
         BE    GODOITE                                                          
         BAL   R9,GETRESP                                                       
         MVI   REPLYSW,C'1'                                                     
         CLI   GOTREPLY,C'1'                                                    
         BE    SCANRESP                                                         
GODOITE  EQU   *                                                                
         LA    R1,OPGSPPRM                                                      
         CALL  OPGOSSIP                                                         
*                                                                               
*  IF NO REPLY WAS EXPECTED JUST GET OUT AT THIS POINT. IF A REPLY              
*  CAME BACK SCAN BACKWARD THRU THE REPLY LOOKING FOR THE FIRST                 
*  NON BLANK BYTE. THEN MOVE THE REPLY UP TO AND INCLUDING THAT                 
*  BYTE BACK TO THE APPLICATION PROGRAM.                                        
*                                                                               
SCANRESP EQU   *                                                                
         CLI   REPLYSW,C'1'                                                     
         BNE   GOBACK                                                           
         LA    R9,OPGSP3+114                                                    
         LA    R10,115                                                          
SCANLOOP EQU   *                                                                
         CLI   0(R9),C' '                                                       
         BNE   SCANEND                                                          
         BCTR  R9,0                                                             
         BCT   R10,SCANLOOP                                                     
SCANEND  EQU   *                                                                
         LTR   R10,R10                                                          
         BZ    GOBACK                                                           
         BCTR  R10,0                                                            
         EX    R10,MOVEMSG                                                      
         B     GOBACK                                                           
MOVEMSG  MVC   REPLAREA(0),OPGSP3                                               
*                                                                               
*  RETURN TO THE CALLING PROGRAM.                                               
*                                                                               
GOBACK   EQU   *                                                                
         SR    R15,R15                                                          
         L     R13,MYSAVE+4                                                     
         RETURN (14,12),T,RC=(15)                                               
*                                                                               
*  READ THRU THE WTL RESPONSE FILE TO SEE IF THE REPLY TO THIS                  
*  MESSAGE CAN BE FOUND. IF THE MESSAGE IS LONGER THAN 40 BYTES                 
*  USE THE FIRST 40. THE REPLY CAN BE UP TO 40 BYTES LONG AND                   
*  IS SET UP AS IF IT HAD BEEN ENTERED FROM THE CONSOLE. IF THE                 
*  MESSAGE CANNOT BE PROCESSED THE OPERATOR WILL BE ASKED TO                    
*  SUPPLY THE RESPONSE.                                                         
*                                                                               
GETRESP  EQU   *                                                                
         MVI   GOTREPLY,C'0'                                                    
         CLI   FILESW,C'1'                                                      
         BE    GETRESPA                                                         
         BHR   R9                                                               
         MVI   FILESW,C'1'                                                      
         RDJFCB WTLFILE                                                         
         LTR   R15,R15                                                          
         BZ    GETRESPA                                                         
         MVI   FILESW,C'2'                                                      
         BR    R9                                                               
GETRESPA EQU   *                                                                
         ZAP   WORKFLD1,TXTLGTH                                                 
         CP    TXTLGTH,=P'40'                                                   
         BNH   GETRESPB                                                         
         ZAP   WORKFLD1,=P'40'                                                  
GETRESPB EQU   *                                                                
         CVB   R10,WORKFLD1                                                     
         MVI   WORKFLD2,C' '                                                    
         MVC   WORKFLD2+1(39),WORKFLD2                                          
         BCTR  R10,0                                                            
         EX    R10,GETMSG                                                       
         OPEN  (WTLFILE,(INPUT))                                                
GETRESPC EQU   *                                                                
         GET   WTLFILE,WTLRECRD                                                 
         CLC   WTLRECRD(40),WORKFLD2                                            
         BNE   GETRESPC                                                         
         LA    R1,OPGSPPRM                                                      
         CALL  OPGOSSIP                                                         
         LA    R10,WTLRECRD+40                                                  
         ST    R10,OPGSPPRM+4                                                   
         ZAP   TXTLGTH,=P'40'                                                   
         LA    R1,OPGSPPRM                                                      
         CALL  OPGOSSIP                                                         
         MVI   OPGSP3,C' '                                                      
         MVC   OPGSP3+1(114),OPGSP3                                             
         MVC   OPGSP3(40),WTLRECRD+40                                           
         MVI   GOTREPLY,C'1'                                                    
GETRESPD EQU   *                                                                
         CLOSE (WTLFILE)                                                        
         BR    R9                                                               
GETMSG   MVC   WORKFLD2(0),MSG                                                  
*                                                                               
*  WORKING STORAGE.                                                             
*                                                                               
         DC    C'***** WTL      EYECATCHER *****'                               
MYSAVE   DS    18F           THIS PROGRAM'S SAVE AREA                           
SAVFLAG  DS    CL1                                                              
SAVREPLY DS    CL3                                                              
FILESW   DC    CL1'0'                                                           
GOTREPLY DS    CL1                                                              
WORKFLD1 DS    D                                                                
WORKFLD2 DS    CL40                                                             
WTLRECRD DS    CL80                                                             
*                                                                               
OPGSPPRM DC    A(OPGSP1)                                                        
         DS    F                                                                
         DC    A(OPGSP3)                                                        
OPGSP1   DS    0CL21                                                            
RCODES   DS    CL16                                                             
REPLYSW  DS    CL1                                                              
PRINTSW  DS    CL1                                                              
TXTLGTH  DS    PL2                                                              
ERRORIND DS    CL1                                                              
OPGSP3   DS    CL115                                                            
*                                                                               
WTLFILE  DCB   DDNAME=WTLANSER,                                        C        
               DSORG=PS,                                               C        
               EODAD=GETRESPD,                                         C        
               EROPT=ABE,                                              C        
               EXLST=JFCBXIT,                                          C        
               MACRF=(GM)                                                       
         DS    0F                                                               
JFCBXIT  DC    XL1'07',AL3(JFCBAREA)                                            
         DC    XL4'80000000'                                                    
         DS    0F                                                               
JFCBAREA DS    CL176                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*  DESCRIPTION OF PARAMETER 1 FROM APPLICATION PROGRAM.                         
*                                                                               
WTLP1    DSECT                                                                  
MSGLGTH  DS    PL2                                                              
CC       DS    CL1                                                              
MSG      DS    CL1                                                              
*                                                                               
*  DESCRIPTION OF PARAMETER 2 FROM APPLICATION PROGRAM.                         
*                                                                               
WTLP2    DSECT                                                                  
FLAG     DS    CL1                                                              
*                                                                               
*  DESCRIPTION OF PARAMETER 3 FROM APPLICATION PROGRAM.                         
*                                                                               
WTLP3    DSECT                                                                  
REPLY    DS    CL3                                                              
*                                                                               
*  DESCRIPTION OF PARAMETER 4 FROM APPLICATION PROGRAM.                         
*                                                                               
WTLP4    DSECT                                                                  
REPLAREA DS    CL1                                                              
         END                                                                    
