GC2TODAY   START  0                     RETURN TODAY'S DATE                     
*--------------------------------------------------------------*                
* HISTORY                                                                       
* 01JUN08  ECU PROJECT  UPGRADED IN ENTERPRISE COMPILER PROJECT                 
*--------------------------------------------------------------*                
*                                                                               
*-------------------------------------------------------------*                 
* THIS PROGRAM IS EQUIVALENT TO SEVERAL OTHER PROGRAMS                          
* ANY CHANGES MADE TO THIS PROGRAM SHOULD ALSO BE MADE TO:                      
*                                                                               
*     GVSTODAY - GLHNEW ENDEVOR                                                 
*     GC2TODAY - GLHSYS LAN - STDSERV                                           
*     GACTODAY - GLHSYS LAN - STDSERV                                           
*                                                                               
*-------------------------------------------------------------*                 
GC2TODAY   AMODE 31                                                             
           SAVE   (14,12),,*                                                    
           BALR   R12,0                  ADDRESS SPACE ADDRESSABILITY           
           USING  *,R12                                                         
           L      R5,0(R1)               PARAMETER ADDRESS INTO R5              
           ST     R13,ZSAVE+4            STD LINKAGE                            
           LA     R13,ZSAVE                 "                                   
           TIME   ,SYSDATE,LINKAGE=SYSTEM,ZONE=LT,DATETYPE=YYYYMMDD             
           MVI    SYSDATE+12,X'0F'       DUMMY SIGN - PACKED DEC.               
           SRP    SYSDATE+8(5),64-1,0     DIVIDE BY 10                          
           OI     SYSDATE+12,X'0F'       DUMMY SIGN - PACKED DEC.               
           UNPK   0(8,R5),SYSDATE+8(5)    UNPACK FOR CALLER                     
           XR     R15,R15                NO ERRORS (OR PRISONERS)               
           L      R13,ZSAVE+4              CALLER'S SAVE AREA                   
           RETURN (14,12),T,RC=(15)          CIAO!                              
SYSDATE    DC     16X'00'                TIME(1-8) & DATE (9-12)                
ZSAVE      DS     36F                    MY SAVE SAREA                          
           REGEQU                        REGISTER EQUATES                       
           END    GC2TODAY                                                      
