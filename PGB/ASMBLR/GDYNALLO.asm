GDYNALLO START                                                                  
*                                                                               
*                                                                               
*                                                                               
* GDYNALLO CAN DYNAMICALLY ALLOCATE OR UNALLOCATE A RESOURCE. CALL              
* GDYNALLO WITH TWO PARAMETERS. PAD FIELDS ON THE RIGHT WITH BLANKS             
* IF REQUIRED. FIELDS WHICH ARE NOT REQUIRED SHOULD CONTAIN BLANKS.             
*                                                                               
*     PARM1:                                                                    
*                                                                               
*        ACTION   DS    CL1   A - ALLOCATE, U - UNALLOCATE                      
*        DDNAM    DS    CL8   DDNAME                                            
*        DSNAM    DS    CL44  DATA SET NAME                                     
*        MEMBR    DS    CL8   MEMBER NAME IF A PDS                              
*        STATUS   DS    CL3   DATA SET STATUS - E.G. SHR                        
*        DISPN    DS    CL7   NORMAL DISPOSITION - E.G. CATLG                   
*        DISPC    DS    CL7   CONDITIONAL DISPOSITION - E.G. DELETE             
*        SPCTYPE  DS    CL1   SPACE TYPE ... T - TRACKS, C - CYLINDERS          
*        RLSE     DS    CL1   NON-BLANK TO RELEASE UNUSED SPACE                 
*        UNCLOSE  DS    CL1   NON-BLANK TO UNALLOCATE RESOURCE AT CLOSE         
*        PRIM     DS    PL3   PRIMARY SPACE ALLOCATION                          
*        SCND     DS    PL3   SECONDARY SPACE ALLOCATION                        
*        DIREC    DS    PL3   DIRECTORY ALLOCATION IN BLOCKS                    
*        UNIT     DS    CL6   UNIT NAME                                         
*        BLKSZ    DS    PL3   BLOCK SIZE                                        
*        LRECL    DS    PL3   RECORD LENGTH                                     
*        RECFM    DS    CL3   RECORD FORMAT                                     
*        DSORG    DS    CL2   DATA SET ORGANIZATION                             
*        SYSOUTC  DS    CL1   SYSOUT CLASS                                      
*        SYSOUTF  DS    CL4   SYSOUT FORM NUMBER                                
*        SYSOUTL  DS    PL4   SYSOUT LIMIT                                      
*        SYSOUTN  DS    PL2   SYSOUT NUMBER OF COPIES                           
*        SYSOUTH  DS    CL1   NON-BLANK TO HOLD SYSOUT DATA SET                 
*        FCB      DS    CL4   FORMS CONTROL BUFFER                              
*        WORKSTN  DS    CL8   REMOTE WORK STATION                               
*        USERID   DS    CL8   USERID                                            
*        DUMMY    DS    CL5   DUMMY FILE INDICATED BY WORD DUMMY                
*                 DS    CL95  FOR FUTURE EXPANSION IF NECESSARY                 
*                                                                               
*     PARM2:                                                                    
*                                                                               
*        RETCODE  DS    PL8   RETURN CODE, VALUE OF R15, 0 MEANS OK             
*                                                                               
* NOTE THAT ALL FIELDS ARE NOT NECESSARILY REQUIRED. IT DEPENDS ON              
* THE PARTICULAR APPLICATION. A RETURN CODE OF 999 MEANS AN INVALID             
* PARAMETER FIELD WAS FOUND.                                                    
*                                                                               
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
         REGEQU                                                                 
         SAVE  (14,12),,*                                                       
         BALR  R11,R0                                                           
         USING *,R11,R12                                                        
         LA    R12,2048(R11)                                                    
         LA    R12,2048(R12)                                                    
         ST    R13,MYSAVE+4                                                     
         LA    R13,MYSAVE                                                       
*                                                                               
         LM    R3,R4,0(R1)                                                      
         USING PASSPRM1,R3                                                      
         USING PASSPRM2,R4                                                      
*                                                                               
         XC    TUPLS,TUPLS                                                      
         LA    R5,TUPLS-4                                                       
*                                                                               
         CLI   PACTION,C'A'                                                     
         BE    VERB01                                                           
         CLI   PACTION,C'U'                                                     
         BE    VERB02                                                           
         ZAP   PRETCODE,=P'999'                                                 
         B     GOBACK                                                           
VERB01   EQU   *                                                                
         MVI   VERBCD,X'01'                                                     
         B     FIELD01                                                          
VERB02   EQU   *                                                                
         MVI   VERBCD,X'02'                                                     
*                                                                               
FIELD01  EQU   *                                                                
         CLC   PDDNAM,BLANKS                                                    
         BE    FIELD02                                                          
         CLI   PACTION,C'A'                                                     
         BE    FIELD01A                                                         
         CLI   PACTION,C'U'                                                     
         BE    FIELD01B                                                         
         B     FIELD02                                                          
FIELD01A EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,DDNAM                                                         
         ST    R6,0(R5)                                                         
         B     FIELD01C                                                         
FIELD01B EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,UNDDNAM                                                       
         ST    R6,0(R5)                                                         
FIELD01C EQU   *                                                                
         LA    R6,PDDNAM                                                        
         ST    R6,SCANPARM                                                      
         MVC   SCANLGTH,=F'8'                                                   
         MVI   FILLBYTE,C' '                                                    
         LA    R1,SCANPARM                                                      
         CALL  SCANLINE                                                         
         LA    R6,RETAREA                                                       
         L     R6,4(R6)                                                         
         STH   R6,DDNAML                                                        
         MVC   DDNAMD,PDDNAM                                                    
         STH   R6,UNDDNAML                                                      
         MVC   UNDDNAMD,PDDNAM                                                  
*                                                                               
FIELD02  EQU   *                                                                
         CLC   PDSNAM,BLANKS                                                    
         BE    FIELD03                                                          
         CLI   PACTION,C'A'                                                     
         BE    FIELD02A                                                         
         CLI   PACTION,C'U'                                                     
         BE    FIELD02B                                                         
         B     FIELD03                                                          
FIELD02A EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,DSNAM                                                         
         ST    R6,0(R5)                                                         
         B     FIELD02C                                                         
FIELD02B EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,UNDSNAM                                                       
         ST    R6,0(R5)                                                         
FIELD02C EQU   *                                                                
         LA    R6,PDSNAM                                                        
         ST    R6,SCANPARM                                                      
         MVC   SCANLGTH,=F'44'                                                  
         MVI   FILLBYTE,C' '                                                    
         LA    R1,SCANPARM                                                      
         CALL  SCANLINE                                                         
         LA    R6,RETAREA                                                       
         L     R6,4(R6)                                                         
         STH   R6,DSNAML                                                        
         MVC   DSNAMD,PDSNAM                                                    
         STH   R6,UNDSNAML                                                      
         MVC   UNDSNAMD,PDSNAM                                                  
*                                                                               
FIELD03  EQU   *                                                                
         CLC   PMEMBR,BLANKS                                                    
         BE    DOWEGOON                                                         
         CLI   PACTION,C'A'                                                     
         BE    FIELD03A                                                         
         CLI   PACTION,C'U'                                                     
         BE    FIELD03B                                                         
         B     DOWEGOON                                                         
FIELD03A EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,MEMBR                                                         
         ST    R6,0(R5)                                                         
         B     FIELD03C                                                         
FIELD03B EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,UNMEMBR                                                       
         ST    R6,0(R5)                                                         
FIELD03C EQU   *                                                                
         LA    R6,PMEMBR                                                        
         ST    R6,SCANPARM                                                      
         MVC   SCANLGTH,=F'8'                                                   
         MVI   FILLBYTE,C' '                                                    
         LA    R1,SCANPARM                                                      
         CALL  SCANLINE                                                         
         LA    R6,RETAREA                                                       
         L     R6,4(R6)                                                         
         STH   R6,MEMBRL                                                        
         MVC   MEMBRD,PMEMBR                                                    
         STH   R6,UNMEMBRL                                                      
         MVC   UNMEMBRD,PMEMBR                                                  
*                                                                               
DOWEGOON EQU   *                                                                
         CLI   PACTION,C'A'                                                     
         BE    FIELD04                                                          
         LA    R5,4(R5)                                                         
         LA    R6,UNALLOC                                                       
         ST    R6,0(R5)                                                         
         B     DOIT                                                             
*                                                                               
FIELD04  EQU   *                                                                
         CLC   PSTATS,=C'OLD'                                                   
         BE    FIELD04A                                                         
         CLC   PSTATS,=C'MOD'                                                   
         BE    FIELD04B                                                         
         CLC   PSTATS,=C'NEW'                                                   
         BE    FIELD04C                                                         
         CLC   PSTATS,=C'SHR'                                                   
         BE    FIELD04D                                                         
         B     FIELD05                                                          
FIELD04A EQU   *                                                                
         MVI   STATSD,X'01'                                                     
         B     FIELD04Z                                                         
FIELD04B EQU   *                                                                
         MVI   STATSD,X'02'                                                     
         B     FIELD04Z                                                         
FIELD04C EQU   *                                                                
         MVI   STATSD,X'04'                                                     
         B     FIELD04Z                                                         
FIELD04D EQU   *                                                                
         MVI   STATSD,X'08'                                                     
FIELD04Z EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,STATS                                                         
         ST    R6,0(R5)                                                         
*                                                                               
FIELD05  EQU   *                                                                
         CLC   PDISPN,=C'UNCATLG'                                               
         BE    FIELD05A                                                         
         CLC   PDISPN,=C'CATLG  '                                               
         BE    FIELD05B                                                         
         CLC   PDISPN,=C'DELETE '                                               
         BE    FIELD05C                                                         
         CLC   PDISPN,=C'KEEP   '                                               
         BE    FIELD05D                                                         
         B     FIELD06                                                          
FIELD05A EQU   *                                                                
         MVI   DISPND,X'01'                                                     
         B     FIELD05Z                                                         
FIELD05B EQU   *                                                                
         MVI   DISPND,X'02'                                                     
         B     FIELD05Z                                                         
FIELD05C EQU   *                                                                
         MVI   DISPND,X'04'                                                     
         B     FIELD05Z                                                         
FIELD05D EQU   *                                                                
         MVI   DISPND,X'08'                                                     
FIELD05Z EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,DISPN                                                         
         ST    R6,0(R5)                                                         
*                                                                               
FIELD06  EQU   *                                                                
         CLC   PDISPC,=C'UNCATLG'                                               
         BE    FIELD06A                                                         
         CLC   PDISPC,=C'CATLG  '                                               
         BE    FIELD06B                                                         
         CLC   PDISPC,=C'DELETE '                                               
         BE    FIELD06C                                                         
         CLC   PDISPC,=C'KEEP   '                                               
         BE    FIELD06D                                                         
         B     FIELD07                                                          
FIELD06A EQU   *                                                                
         MVI   DISPCD,X'01'                                                     
         B     FIELD06Z                                                         
FIELD06B EQU   *                                                                
         MVI   DISPCD,X'02'                                                     
         B     FIELD06Z                                                         
FIELD06C EQU   *                                                                
         MVI   DISPCD,X'04'                                                     
         B     FIELD06Z                                                         
FIELD06D EQU   *                                                                
         MVI   DISPCD,X'08'                                                     
FIELD06Z EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,DISPC                                                         
         ST    R6,0(R5)                                                         
*                                                                               
FIELD07  EQU   *                                                                
         CLI   PSPCTYPE,C'T'                                                    
         BE    FIELD07A                                                         
         CLI   PSPCTYPE,C'C'                                                    
         BE    FIELD07B                                                         
         B     FIELD08                                                          
FIELD07A EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,TRKS                                                          
         ST    R6,0(R5)                                                         
         B     FIELD08                                                          
FIELD07B EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,CYL                                                           
         ST    R6,0(R5)                                                         
*                                                                               
FIELD08  EQU   *                                                                
         CLI   PRLSE,C' '                                                       
         BE    FIELD09                                                          
         LA    R5,4(R5)                                                         
         LA    R6,RLSE                                                          
         ST    R6,0(R5)                                                         
*                                                                               
FIELD09  EQU   *                                                                
         CLI   PUNCLOSE,C' '                                                    
         BE    FIELD10                                                          
         LA    R5,4(R5)                                                         
         LA    R6,UNCLOSE                                                       
         ST    R6,0(R5)                                                         
*                                                                               
FIELD10  EQU   *                                                                
         CLC   PPRIM,BLANKS                                                     
         BE    FIELD11                                                          
         CP    PPRIM,ZERO                                                       
         BE    FIELD11                                                          
         LA    R5,4(R5)                                                         
         ZAP   WORKFLD,PPRIM                                                    
         CVB   R6,WORKFLD                                                       
         STCM  R6,B'0111',PRIMD                                                 
         LA    R6,PRIM                                                          
         ST    R6,0(R5)                                                         
*                                                                               
FIELD11  EQU   *                                                                
         CLC   PSCND,BLANKS                                                     
         BE    FIELD12                                                          
         CP    PSCND,ZERO                                                       
         BE    FIELD12                                                          
         LA    R5,4(R5)                                                         
         ZAP   WORKFLD,PSCND                                                    
         CVB   R6,WORKFLD                                                       
         STCM  R6,B'0111',SCNDD                                                 
         LA    R6,SCND                                                          
         ST    R6,0(R5)                                                         
*                                                                               
FIELD12  EQU   *                                                                
         CLC   PDIREC,BLANKS                                                    
         BE    FIELD13                                                          
         CP    PDIREC,ZERO                                                      
         BE    FIELD13                                                          
         LA    R5,4(R5)                                                         
         ZAP   WORKFLD,PDIREC                                                   
         CVB   R6,WORKFLD                                                       
         STCM  R6,B'0111',DIRECD                                                
         LA    R6,DIREC                                                         
         ST    R6,0(R5)                                                         
*                                                                               
FIELD13  EQU   *                                                                
         CLC   PUNIT,BLANKS                                                     
         BE    FIELD14                                                          
         LA    R5,4(R5)                                                         
         LA    R6,PUNIT                                                         
         ST    R6,SCANPARM                                                      
         MVC   SCANLGTH,=F'6'                                                   
         MVI   FILLBYTE,C' '                                                    
         LA    R1,SCANPARM                                                      
         CALL  SCANLINE                                                         
         LA    R6,RETAREA                                                       
         L     R6,4(R6)                                                         
         STH   R6,UNITL                                                         
         MVC   UNITD,PUNIT                                                      
         LA    R6,UNIT                                                          
         ST    R6,0(R5)                                                         
*                                                                               
FIELD14  EQU   *                                                                
         CLC   PBLKSZ,BLANKS                                                    
         BE    FIELD15                                                          
         CP    PBLKSZ,ZERO                                                      
         BE    FIELD15                                                          
         LA    R5,4(R5)                                                         
         ZAP   WORKFLD,PBLKSZ                                                   
         CVB   R6,WORKFLD                                                       
         STH   R6,BLKSZD                                                        
         LA    R6,BLKSZ                                                         
         ST    R6,0(R5)                                                         
*                                                                               
FIELD15  EQU   *                                                                
         CLC   PLRECL,BLANKS                                                    
         BE    FIELD16                                                          
         CP    PLRECL,ZERO                                                      
         BE    FIELD16                                                          
         LA    R5,4(R5)                                                         
         ZAP   WORKFLD,PLRECL                                                   
         CVB   R6,WORKFLD                                                       
         STH   R6,LRECLD                                                        
         LA    R6,LRECL                                                         
         ST    R6,0(R5)                                                         
*                                                                               
FIELD16  EQU   *                                                                
         CLC   PRECFM,BLANKS                                                    
         BE    FIELD17                                                          
         CLI   PRECFM,C'V'                                                      
         BE    FIELD16A                                                         
         CLI   PRECFM,C'F'                                                      
         BE    FIELD16B                                                         
         CLI   PRECFM,C'U'                                                      
         BE    FIELD16C                                                         
         B     FIELD17                                                          
FIELD16A EQU   *                                                                
         MVI   RECFMD,X'40'                                                     
         B     FIELD16D                                                         
FIELD16B EQU   *                                                                
         MVI   RECFMD,X'80'                                                     
         B     FIELD16D                                                         
FIELD16C EQU   *                                                                
         MVI   RECFMD,X'C0'                                                     
FIELD16D EQU   *                                                                
         CLI   PRECFM+1,C'B'                                                    
         BE    FIELD16E                                                         
         CLI   PRECFM+1,C'A'                                                    
         BE    FIELD16F                                                         
         B     FIELD16Z                                                         
FIELD16E EQU   *                                                                
         OI    RECFMD,X'10'                                                     
         CLI   PRECFM+2,C'A'                                                    
         BNE   FIELD16Z                                                         
FIELD16F EQU   *                                                                
         OI    RECFMD,X'04'                                                     
FIELD16Z EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,RECFM                                                         
         ST    R6,0(R5)                                                         
*                                                                               
FIELD17  EQU   *                                                                
         CLC   PDSORG,=C'PS'                                                    
         BE    FIELD17A                                                         
         CLC   PDSORG,=C'PO'                                                    
         BE    FIELD17B                                                         
         B     FIELD18                                                          
FIELD17A EQU   *                                                                
         MVC   DSORGD,=X'4000'                                                  
         B     FIELD17Z                                                         
FIELD17B EQU   *                                                                
         MVC   DSORGD,=X'0200'                                                  
FIELD17Z EQU   *                                                                
         LA    R5,4(R5)                                                         
         LA    R6,DSORG                                                         
         ST    R6,0(R5)                                                         
*                                                                               
FIELD18  EQU   *                                                                
         CLI   PSYSOUTC,C' '                                                    
         BE    FIELD19                                                          
         MVC   SYSOUTCD,PSYSOUTC                                                
         LA    R5,4(R5)                                                         
         LA    R6,SYSOUTC                                                       
         ST    R6,0(R5)                                                         
*                                                                               
FIELD19  EQU   *                                                                
         CLC   PSYSOUTF,BLANKS                                                  
         BE    FIELD20                                                          
         LA    R5,4(R5)                                                         
         LA    R6,PSYSOUTF                                                      
         ST    R6,SCANPARM                                                      
         MVC   SCANLGTH,=F'4'                                                   
         MVI   FILLBYTE,C' '                                                    
         LA    R1,SCANPARM                                                      
         CALL  SCANLINE                                                         
         LA    R6,RETAREA                                                       
         L     R6,4(R6)                                                         
         STH   R6,SYSOUTFL                                                      
         MVC   SYSOUTFD,PSYSOUTF                                                
         LA    R6,SYSOUTF                                                       
         ST    R6,0(R5)                                                         
*                                                                               
FIELD20  EQU   *                                                                
         CLC   PSYSOUTL,BLANKS                                                  
         BE    FIELD21                                                          
         CP    PSYSOUTL,ZERO                                                    
         BE    FIELD21                                                          
         LA    R5,4(R5)                                                         
         ZAP   WORKFLD,PSYSOUTL                                                 
         CVB   R6,WORKFLD                                                       
         STCM  R6,B'0111',SYSOUTLD                                              
         LA    R6,SYSOUTL                                                       
         ST    R6,0(R5)                                                         
*                                                                               
FIELD21  EQU   *                                                                
         CLC   PSYSOUTN,BLANKS                                                  
         BE    FIELD22                                                          
         CP    PSYSOUTN,ZERO                                                    
         BE    FIELD22                                                          
         LA    R5,4(R5)                                                         
         ZAP   WORKFLD,PSYSOUTN                                                 
         CVB   R6,WORKFLD                                                       
         STC   R6,SYSOUTND                                                      
         LA    R6,SYSOUTN                                                       
         ST    R6,0(R5)                                                         
*                                                                               
FIELD22  EQU   *                                                                
         CLI   PSYSOUTH,C' '                                                    
         BE    FIELD23                                                          
         LA    R5,4(R5)                                                         
         LA    R6,SYSOUTH                                                       
         ST    R6,0(R5)                                                         
*                                                                               
FIELD23  EQU   *                                                                
         CLC   PFCB,BLANKS                                                      
         BE    FIELD24                                                          
         LA    R5,4(R5)                                                         
         LA    R6,PFCB                                                          
         ST    R6,SCANPARM                                                      
         MVC   SCANLGTH,=F'4'                                                   
         MVI   FILLBYTE,C' '                                                    
         LA    R1,SCANPARM                                                      
         CALL  SCANLINE                                                         
         LA    R6,RETAREA                                                       
         L     R6,4(R6)                                                         
         STH   R6,FCBL                                                          
         MVC   FCBD,PFCB                                                        
         LA    R6,FCB                                                           
         ST    R6,0(R5)                                                         
*                                                                               
FIELD24  EQU   *                                                                
         CLC   PWORKSTN,BLANKS                                                  
         BE    FIELD25                                                          
         LA    R5,4(R5)                                                         
         LA    R6,PWORKSTN                                                      
         ST    R6,SCANPARM                                                      
         MVC   SCANLGTH,=F'8'                                                   
         MVI   FILLBYTE,C' '                                                    
         LA    R1,SCANPARM                                                      
         CALL  SCANLINE                                                         
         LA    R6,RETAREA                                                       
         L     R6,4(R6)                                                         
         STH   R6,WORKSTNL                                                      
         MVC   WORKSTND,PWORKSTN                                                
         LA    R6,WORKSTN                                                       
         ST    R6,0(R5)                                                         
*                                                                               
FIELD25  EQU   *                                                                
         CLC   PUSERID,BLANKS                                                   
         BE    FIELD26                                                          
         LA    R5,4(R5)                                                         
         LA    R6,PUSERID                                                       
         ST    R6,SCANPARM                                                      
         MVC   SCANLGTH,=F'8'                                                   
         MVI   FILLBYTE,C' '                                                    
         LA    R1,SCANPARM                                                      
         CALL  SCANLINE                                                         
         LA    R6,RETAREA                                                       
         L     R6,4(R6)                                                         
         STH   R6,USERIDL                                                       
         MVC   USERIDD,PUSERID                                                  
         LA    R6,USERID                                                        
         ST    R6,0(R5)                                                         
*                                                                               
FIELD26  EQU   *                                                                
         CLC   PDUMMY,=C'DUMMY'                                                 
         BNE   DOIT                                                             
         LA    R5,4(R5)                                                         
         LA    R6,DUMMY                                                         
         ST    R6,0(R5)                                                         
*                                                                               
DOIT     EQU   *                                                                
         SR    R15,R15                                                          
         LA    R1,RBP                                                           
         DYNALLOC                                                               
         CVD   R15,WORKFLD                                                      
         MVN   WORKFLD+7(1),=X'0C'                                              
         MVC   PRETCODE,WORKFLD                                                 
*                                                                               
GOBACK   EQU   *                                                                
         SR    R15,R15                                                          
         L     R13,MYSAVE+4                                                     
         RETURN (14,12),T,RC=(15)                                               
*                                                                               
MYSAVE   DS    18F                                                              
WORKFLD  DS    D                                                                
BLANKS   DC    44C' '                                                           
ZERO     DC    PL1'0'                                                           
*                                                                               
SCANPARM DS    F                                                                
         DC    A(SCANLGTH)                                                      
         DC    A(FILLBYTE)                                                      
         DC    A(RETAREA)                                                       
SCANLGTH DS    F                                                                
FILLBYTE DS    CL1                                                              
RETAREA  DS    40D                                                              
*                                                                               
         DS    0F                                                               
RBP      DC    X'80'                                                            
         DC    AL3(RB)                                                          
RB       DC    X'14'                                                    02694000
VERBCD   DS    CL1                                                      02695000
         DC    6X'00'                                                   02696000
         DC    A(TUPLS)                                                 02697000
         DC    8X'00'                                                   02698000
         DS    0F                                                               
TUPLS    DS    CL160               ROOM FOR 40 TEXT UNITS               02699000
         DC    X'80000000'         DELIMITER                            02700000
         DS    0F                                                               
DDNAM    DC    AL2(DALDDNAM),X'0001'                                    02824500
DDNAML   DS    CL2                                                      02824700
DDNAMD   DS    CL8                                                      02824801
         DS    0F                                                               
DSNAM    DC    AL2(DALDSNAM),X'0001'                                    02825000
DSNAML   DS    CL2                                                      02827000
DSNAMD   DS    CL44                                                     02828000
         DS    0F                                                               
MEMBR    DC    AL2(DALMEMBR),X'0001'                                    02825000
MEMBRL   DS    CL2                                                      02827000
MEMBRD   DS    CL8                                                      02828000
         DS    0F                                                               
STATS    DC    AL2(DALSTATS),X'00010001'                                02825000
STATSD   DS    CL1                                                      02827000
         DS    0F                                                               
DISPN    DC    AL2(DALNDISP),X'00010001'                                02825000
DISPND   DS    CL1                                                      02827000
         DS    0F                                                               
DISPC    DC    AL2(DALCDISP),X'00010001'                                02825000
DISPCD   DS    CL1                                                      02827000
         DS    0F                                                               
TRKS     DC    AL2(DALTRK),X'0000'                                      02825000
         DS    0F                                                               
CYL      DC    AL2(DALCYL),X'0000'                                      02825000
         DS    0F                                                               
RLSE     DC    AL2(DALRLSE),X'0000'                                     02825000
         DS    0F                                                               
UNCLOSE  DC    AL2(DALCLOSE),X'0000'                                    02825000
         DS    0F                                                               
PRIM     DC    AL2(DALPRIME),X'00010003'                                02825000
PRIMD    DS    CL3                                                      02827000
         DS    0F                                                               
SCND     DC    AL2(DALSECND),X'00010003'                                02825000
SCNDD    DS    CL3                                                      02827000
         DS    0F                                                               
DIREC    DC    AL2(DALDIR),X'00010003'                                  02825000
DIRECD   DS    CL3                                                      02827000
         DS    0F                                                               
UNIT     DC    AL2(DALUNIT),X'0001'                                     02825000
UNITL    DS    CL2                                                      02827000
UNITD    DS    CL6                                                      02828000
         DS    0F                                                               
BLKSZ    DC    AL2(DALBLKSZ),X'00010002'                                02825000
BLKSZD   DS    CL2                                                      02827000
         DS    0F                                                               
LRECL    DC    AL2(DALLRECL),X'00010002'                                02825000
LRECLD   DS    CL2                                                      02827000
         DS    0F                                                               
RECFM    DC    AL2(DALRECFM),X'00010001'                                02825000
RECFMD   DS    CL1                                                      02827000
         DS    0F                                                               
DSORG    DC    AL2(DALDSORG),X'00010002'                                02825000
DSORGD   DS    CL2                                                      02827000
         DS    0F                                                               
SYSOUTC  DC    AL2(DALSYSOU),X'00010001'                                02825000
SYSOUTCD DS    CL1                                                      02827000
         DS    0F                                                               
SYSOUTF  DC    AL2(DALSFMNO),X'0001'                                    02825000
SYSOUTFL DS    CL2                                                      02827000
SYSOUTFD DS    CL4                                                      02827000
         DS    0F                                                               
SYSOUTL  DC    AL2(DALOUTLM),X'00010003'                                02825000
SYSOUTLD DS    CL3                                                      02827000
         DS    0F                                                               
SYSOUTN  DC    AL2(DALCOPYS),X'00010001'                                02825000
SYSOUTND DS    CL1                                                      02827000
         DS    0F                                                               
SYSOUTH  DC    AL2(DALSHOLD),X'0000'                                    02825000
         DS    0F                                                               
FCB      DC    AL2(DALFCBIM),X'0001'                                    02825000
FCBL     DS    CL2                                                      02827000
FCBD     DS    CL4                                                      02827000
         DS    0F                                                               
WORKSTN  DC    AL2(DALSUSER),X'0001'                                    02825000
WORKSTNL DS    CL2                                                      02827000
WORKSTND DS    CL8                                                      02827000
         DS    0F                                                               
USERID   DC    AL2(DALUSRID),X'0001'                                    02825000
USERIDL  DS    CL2                                                      02827000
USERIDD  DS    CL8                                                      02827000
         DS    0F                                                               
DUMMY    DC    AL2(DALDUMMY),X'0000'                                    02825000
         DS    0F                                                               
UNDDNAM  DC    AL2(DUNDDNAM),X'0001'                                    02824500
UNDDNAML DS    CL2                                                      02824700
UNDDNAMD DS    CL8                                                      02824801
         DS    0F                                                               
UNDSNAM  DC    AL2(DUNDSNAM),X'0001'                                    02825000
UNDSNAML DS    CL2                                                      02827000
UNDSNAMD DS    CL44                                                     02828000
         DS    0F                                                               
UNMEMBR  DC    AL2(DUNMEMBR),X'0001'                                    02825000
UNMEMBRL DS    CL2                                                      02827000
UNMEMBRD DS    CL8                                                      02828000
         DS    0F                                                               
UNALLOC  DC    AL2(DUNUNALC),X'0000'                                    02825000
*                                                                               
         LTORG                                                                  
*                                                                               
PASSPRM1 DSECT                                                                  
PACTION  DS    CL1    TELLS WHAT TO DO                                          
*                        A - ALLOCATE                                           
*                        U - UNALLOCATE                                         
PDDNAM   DS    CL8    DDNAME                                                    
PDSNAM   DS    CL44   DSNAME                                                    
PMEMBR   DS    CL8    MEMBER NAME                                               
PSTATS   DS    CL3    DATA SET STATUS                                           
*                        OLD                                                    
*                        MOD                                                    
*                        NEW                                                    
*                        SHR                                                    
PDISPN   DS    CL7    DATA SET NORMAL DISPOSITION                               
*                        UNCATLG                                                
*                        CATLG                                                  
*                        DELETE                                                 
*                        KEEP                                                   
PDISPC   DS    CL7    DATA SET CONDITIONAL DISPOSITION                          
*                        UNCATLG                                                
*                        CATLG                                                  
*                        DELETE                                                 
*                        KEEP                                                   
PSPCTYPE DS    CL1    TYPE OF SPACE TO BE ALLOCATED                             
*                        T - TRACKS                                             
*                        C - CYLINDERS                                          
PRLSE    DS    CL1    RELEASE UNUSED SPACE - NONBLANK WILL DO SO                
PUNCLOSE DS    CL1    UNALLOCATE WHEN CLOSED - NONBLANK WILL DO SO              
PPRIM    DS    PL3    PRIMARY SPACE ALLOCATION                                  
PSCND    DS    PL3    SECONDARY SPACE ALLOCATION                                
PDIREC   DS    PL3    DIRECTORY ALLOCATION IN BLOCKS                            
PUNIT    DS    CL6    UNIT NAME                                                 
PBLKSZ   DS    PL3    BLOCK SIZE                                                
PLRECL   DS    PL3    RECORD LENGTH                                             
PRECFM   DS    CL3    RECORD FORMAT                                             
PDSORG   DS    CL2    DATA SET ORGANIZATION                                     
PSYSOUTC DS    CL1    SYSOUT CLASS                                              
PSYSOUTF DS    CL4    SYSOUT FORM NUMBER                                        
PSYSOUTL DS    PL4    SYSOUT LIMIT                                              
PSYSOUTN DS    PL2    SYSOUT NUMBER OF COPIES                                   
PSYSOUTH DS    CL1    SYSOUT HOLD QUEUE - NONBLANK WILL DO SO                   
PFCB     DS    CL4    FORMS CONTROL BUFFER                                      
PWORKSTN DS    CL8    REMOTE WORK STATION                                       
PUSERID  DS    CL8    USERID                                                    
PDUMMY   DS    CL5    DUMMY FILE INDICATED BY WORD DUMMY                        
         DS    CL95   FOR FUTURE EXPANSION IF NECESSARY                         
*                                                                               
PASSPRM2 DSECT                                                                  
PRETCODE DS    PL8    RETURN CODE - VALUE OF R15 RETURNED HERE                  
*                                                                               
         IEFZB4D2                                                               
*                                                                               
         END                                                                    
