       CBL FLAG(I),TRUNC(BIN)                                           00060000
       IDENTIFICATION DIVISION.                                         00060000
       PROGRAM-ID.    GCCPFRMO.                                         00070000
                                                                        00080000
      ****************************************************************  00090000
      *  GROUP BENEFITS E-BUSINESS TO READ GIPSY DETAIL                 00100000
      *                                                                 00110000
      *                                                                 00160000
      * CALLING MODULES                                                 00170000
      *     GCCPFRMN                                                    00180000
      *                                                                 00190000
      * CALLED MODULES                                                  00200000
      *     GACCDDET - READ GIPSY DETAIL                                00210000
      *                                                                 00220000
      *                                                                 00290000
      ****************************************************************  00300000
      * DATE       NAME        DESCRIPTION                              00310000
      * ---------  ----------  ---------------------------------------  00320000
      * 21MAR2002  J.ELKINS    CREATION                                 00330000
      *                                                                 00330000
      * 15AUG2008  IBM         ENTERPRISE COMPILER UPGRADE(ECU)         00330000
      ****************************************************************  00340000
                                                                        00350000
       ENVIRONMENT DIVISION.                                            00360000
       CONFIGURATION SECTION.                                           00370000
       SOURCE-COMPUTER.  IBM-370-165.                                   00380000
       OBJECT-COMPUTER.  IBM-370-165.                                   00390000
                                                                        00400000
       INPUT-OUTPUT SECTION.                                            00410000
                                                                        00420000
       FILE-CONTROL.                                                    00430000
                                                                        00440000
       DATA DIVISION.                                                   00441000
                                                                        00442000
       FILE SECTION.                                                    00443000
                                                                        00444000
       WORKING-STORAGE SECTION.                                         00450000
                                                                        00460000
       01  WS-CONSTANTS.                                                00470000
           05  FILLER                   PIC X(32) VALUE                 00480000
               '*** GCCPFRMO WORKING STORAGE ***'.                      00490000
                                                                        00500000
       LINKAGE SECTION.                                                 00510000
                                                                        02226000
      ***************************************************************** 02227001
      *** GIPSY DETAIL VSAM ACCESS PARAMETERS                           02228001
      ***************************************************************** 02229001
       01  GACCDDET-PARMS.                                              02229101
           05  GACCDDET-FUNCTION COMP    PIC S9(4).                     02229201
           05  GACCDDET-KEY.                                            02229301
               10  GACCDDET-GROU COMP-3  PIC S9(7).                     02229401
               10  GACCDDET-ACCT         PIC X(3).                      02229501
               10  GACCDDET-CERT COMP-3  PIC S9(9).                     02229601
           05  GACCDDET-WORK             PIC X(9).                      02229701
      *                                                                 02229801
                                                                        02229901
       01  GACCDDET-ERROR-CODE           PIC X(3).                      02230001
                                                                        02230701
      ***************************************************************** 02230801
      *** GIPSY DETAIL VSAM RECORD.                                     02230901
      ***************************************************************** 02231001
                                                                        02231101
      *01  GIPSY-DETAIL-RECORD.                                         02231401
           COPY GDETEXP7.                                               02231501
       PROCEDURE DIVISION USING GACCDDET-PARMS                          02232001
                                GACCDDET-ERROR-CODE                     02233001
                                GDETEXP7.                               02234001
                                                                        02240000
       0000-MAINLINE.                                                   02250000
                                                                        02260000
           CALL 'GACCDDE7'        USING  GACCDDET-PARMS                 02261001
                                         GACCDDET-ERROR-CODE            02262001
                                         GDETEXP7                       02263001
                                                                        02340000
           GOBACK.                                                      02350000
                                                                        02360000
       0000-EXIT.                                                       02370000
           EXIT.                                                        02380000
                                                                        02390000
