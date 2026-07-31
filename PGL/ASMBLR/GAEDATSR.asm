*******U**************************************************************  00003   
*     *U**             G A E D A T S R                                  00003   
*     *U**                                                              00017   
*     *U** THIS MODULE IS MANULIFE FINANCIAL'S (CAN. DIV.) DATA         00004   
*     *U** SERVICE INTERFACE FOR MAINFRAME BATCH PROGRAMS ACCESSING     00005   
*     *U** IDMS DATABASES, SEQUENTIAL AND VSAM FILES. IT IS CALLED      00005   
*     *U** DYNAMICALLY AND ITS PURPOSE IS TO PASS CONTROL THROUGH       00005   
*     *U** TO THE DATA SERVICE MAINLINE (DATSRVB) WHICH                 00006   
*     *U** DOES THE LOGICAL RECORD PROCESSING.                          00006   
*     *U**                                                              00017   
*     *U**     PARAMETER       USAGE            DESCRIPTION             00007   
*     *U**                                                              00008   
*     *U** PROCESS-VERB       RECEIVED        ACTION VERB, REQUESTING   00009   
*     *U**                                    SPECIFIC LR- PROCESS      00010   
*     *U**                                    (EG. STORE, ERASE, ETC.)  00010   
*     *U**                                                              00011   
*     *U** LOGICAL-RECORD     RECEIVED &      THE AREA IN WORKING       00012   
*     *U**                    RETURNED        STORAGE WHERE THE LR      00013   
*     *U**                                    WILL BE STORE'D FROM      00014   
*     *U**                                    OR OBTAIN'D INTO.         00014   
*     *U**                                                              00011   
*     *U** ICBM               RECEIVED &      INTERFACE CONTROL BLOCK   00020   
*     *U**                    RETURNED        (MANULIFE)                00021   
*******U**************************************************************  00003   
         SPACE 1                                                                
*********H************************************************************* 00041   
*     ***H     DATE       - 25NOV94                                     00042   
*     ***H     PROGRAMMER - DAVE EMBURY                                 00043   
*     ***H     ACTION     - CREATED AS A DYNAMICALLY LOADABLE VERSION   00045   
*     ***H                  OF GAEDATSB/DATASRVB.                       00045   
*********H************************************************************* 00046   
         EJECT                                                                  
GAEDATSR TMENTER SUB,AMODE=24,RMODE=24                                  00001   
CLOSE    EQU   X'F0'                                                            
DATASRST NOP   DATASRCT            FIRST-TIME GATE                              
         MVI   DATASRST+1,CLOSE    CLOSE THE GATE                               
         LR    R2,R1               SAVE ADDRESS OF PARAMETER LIST               
         LOAD  EP=DATSRVB          LOAD DATSRVB INTO VIRTUAL STORAGE    00068   
         ST    R0,DATSRVBA         SAVE THE ADDRESS THEREOF                     
         LR    R1,R2               RESTORE ADDRESS OF PARAMETER LIST            
DATASRCT EQU   *                                                                
         L     R15,DATSRVBA        PICK UP THE ADDRESS OF DATSRVB               
         BALR  R14,R15             LINK TO DATSRVB                              
         TMLEAVE                      THEN EXIT                         00070   
         SPACE                                                                  
DATSRVBA DS    A                   ADDRESS OF DATSRVB                           
         SPACE                                                                  
         END                                                            00072   
