*******U**************************************************************          
*     *U**                 Z P A R D U M P                                      
*     *U**                                                                      
*     *U**   THIS ROUTINE CAUSES AN IBM SNAP DUMP OF THE BATCH REGION.          
*     *U**   VARIOUS FLAVOURS OF DUMP ARE CREATED, BASED ON THE                 
*     *U**   INPUT REQUEST.                                                     
*     *U**                                                                      
*     *U**   NOTE: THIS WAS ORIGINALLY A DOS PROGRAM. THE MOVE TO               
*     *U**         OS/MVS MADE CERTAIN OF THE REQUEST CODES REDUNDANT           
*     *U**         AND THEY NOW POINT TO THE EQUIVALENT CODES FOR               
*     *U**         UPWARD COMPATIBLITY.                                         
*     *U**                                                                      
*     *U**     PARAMETER    USAGE      DESCRIPTION                              
*     *U**                                                                      
*     *U**     ACTION-CODE  RECEIVED   ONE-CHARACTER REQUEST CODE:              
*     *U**                                                                      
*     *U**                             '1' - DUMP PSW, REGISTERS AND            
*     *U**                                   STORAGE BETWEEN LOW-ADDR           
*     *U**                                   AND HI-ADDR; THEN ABEND.           
*     *U**                                                                      
*     *U**                             '2' - DUMP PSW, REGISTERS AND            
*     *U**                                   PARTITION'S MAIN MEMORY            
*     *U**                                   THEN ABEND.                        
*     *U**                                                                      
*     *U**                             '3' - SAME AS '1'                        
*     *U**                                                                      
*     *U**                             '4' - SAME AS '2'                        
*     *U**                                                                      
*     *U**                             '5' - DUMP PSW, REGISTERS AND            
*     *U**                                   STORAGE BETWEEN LOW-ADDR           
*     *U**                                   AND HI-ADDRS; THEN RETURN.         
*     *U**                                                                      
*     *U**                             '6' - DUMP PSW, REGISTERS AND            
*     *U**                                   PARTITION'S MAIN MEMORY            
*     *U**                                   THEN RETURN.                       
*     *U**                                                                      
*     *U**                             '7' - SAME AS '5'                        
*     *U**                                                                      
*     *U**                             '8' - SAME AS '6'                        
*     *U**                                                                      
*     *U**                             '9' - DUMP THE ENTIRE PROGRAM            
*     *U**                                   ADDRESS SPACE, INCLUDING           
*     *U**                                   SUBPOOLS, THEN RETURN.             
*     *U**                                                                      
*     *U**     LOW-ADDR     RECEIVED   PLACE TO START THE STORAGE DUMP          
*     *U**                  (OPTIONAL) FOR REQUESTS TYPE 1 AND 5                
*     *U**                                                                      
*     *U**     HI-ADDR      RECEIVED   PLACE TO STOP THE STORAGE DUMP           
*     *U**                  (OPTIONAL) FOR REQUESTS TYPE 1 AND 5                
*     *U**                                                                      
*     *U**   IF THE REQUEST-CODE IS INVALID, A CODE OF X'08' IS                 
*     *U**   RETURNED IN REGISTER 15.                                           
*******U**************************************************************          
         SPACE 2                                                                
*********H************************************************************          
*     ***H     DATE       - 29SEP95                                             
*     ***H     PROGRAMMER - DAVE EMBURY                                         
*     ***H     ACTION     - CREATED FROM THE ORIGINAL VERSION BY                
*     ***H                  A) ALTERING THE LINKAGE TO MAKE IT                  
*     ***H                  SERIALLY REUSABLE; (B) ADDING THE OPTION            
*     ***H                  TO CAUSE A COMPLETE DUMP OF ALL MEMORY IN           
*     ***H                  THE PARTITION, INCLUDING SUBPOOLS; AND,             
*     ***H                  (C) ELIMINATED THE SPURIOUS MESSAGES BEING          
*     ***H                  WRITTEN TO THE OPERATOR.                            
*********H************************************************************          
         EJECT                                                                  
ZPARDUMP TMENTRE AMODE=24,RMODE=24                                              
         LM    R3,R5,0(R1)         PICK UP THE INPUT PARAMETERS,                
         CLI   0(R3),C'1'              CHECK THE REQUEST CODE AND GO TO         
         BE    OPTION1                 THE APPROPRIATE DUMP LOGIC.              
         CLI   0(R3),C'2'                                                       
         BE    OPTION2                                                          
         CLI   0(R3),C'3'                                                       
         BE    OPTION1                                                          
         CLI   0(R3),C'4'                                                       
         BE    OPTION2                                                          
         CLI   0(R3),C'5'                                                       
         BE    OPTION5                                                          
         CLI   0(R3),C'6'                                                       
         BE    OPTION6                                                          
         CLI   0(R3),C'7'                                                       
         BE    OPTION5                                                          
         CLI   0(R3),C'8'                                                       
         BE    OPTION6                                                          
         CLI   0(R3),C'9'                                                       
         BE    OPTION9                                                          
         TMLEAVE RC=8              IF NOT VALID - RETURN WITH CODE = 8          
         EJECT                                                                  
OPTION1  EQU   *                                                                
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,STORAGE=((R4),(R5)),PDATA=(PSW,REGS)                 
         CLOSE SNAPDCB                                                          
         ABEND 0                                                                
         EJECT                                                                  
OPTION2  EQU   *                                                                
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,PDATA=(JPA,PSW,REGS)                                 
         CLOSE SNAPDCB                                                          
         ABEND 0                                                                
         EJECT                                                                  
OPTION5  EQU   *                                                                
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,STORAGE=((4),(5)),PDATA=(PSW,REGS)                   
         CLOSE SNAPDCB                                                          
         TMLEAVE RC=0                                                           
         EJECT                                                                  
OPTION6  EQU   *                                                                
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,PDATA=(JPA,PSW,REGS)                                 
         CLOSE SNAPDCB                                                          
         TMLEAVE RC=0                                                           
         EJECT                                                                  
OPTION9  EQU   *                                                                
         OPEN  (SNAPDCB,(OUTPUT))                                               
         SNAP  DCB=SNAPDCB,PDATA=ALL                                            
         CLOSE SNAPDCB                                                          
         TMLEAVE RC=0                                                           
         EJECT                                                                  
SNAPDCB  DCB   DDNAME=SYSUDUMP,DSORG=PS,RECFM=VBA,BLKSIZE=1632,        *        
               LRECL=125,MACRF=(W)                                              
         END                                                                    
