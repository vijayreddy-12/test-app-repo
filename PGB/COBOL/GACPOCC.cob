       CBL TRUNC(BIN)                                                           
       IDENTIFICATION DIVISION.                                         00020000
       PROGRAM-ID.    GACPOCC.                                          00030000
      *AUTHOR.        JUDY ELKINS.                                      00040000
      *INSTALLATION.  MANULIFE.                                         00050000
      *DATE-WRITTEN.                                                    00060000
      *DATE-COMPILED.                                                   00070000
      *----------------------------------------------------------------*00080000
      *                                                                 00090000
      *  SYSTEM    : E-BUSINESS                                         00100000
      *                                                                 00110000
      *  LANGUAGE  : COBOL II                                           00120000
      *                                                                 00130000
      *  PROGRAM DESCRIPTION:                                           00131001
      *                                                                 00132001
      *  THIS MODULE WILL BE PASSED AN OCCUPATION DESCRIPTION.  THE     00140001
      *  DESCRIPTION WAS SELECTED FROM A DROP DOWN MENU ON THE E-FORM   00150000
      *  GL2971 AND SUBMITTED FOR PROCESSING.  THIS MODULE WILL         00151000
      *  RETURN A GIPSY CODE AND CATEGORY DESCRIPTION TO MATCH THE      00152001
      *  DESCRIPTION SUBMITTED.                                         00153001
      *                                                                 00160000
      *  CALLING MODULES:                                               00161001
      *    GCCPFRMF                                                     00162001
      *                                                                 00163001
      *  CALLED MODULES:                                                00164001
      *    NONE                                                         00165001
      *                                                                 00166001
      *  COPYBOOKS                                                      00167001
      *    GACCOCC                                                      00168001
      *    GACCETBL                                                     00169017
      *    GACCFTBL                                                     00169117
      *                                                                 00169201
      *  INPUT PARAMETERS                                               00169301
      *    TRANSACTION TYPE                                             00169401
      *    LANGUAGE                                                     00169501
      *    DESCRIPTION                                                  00169601
      *                                                                 00169701
      *  OUTPUT PARAMETERS                                              00169801
      *    RETURN CODE                                                  00169901
      *    GIPSY CODE                                                   00170001
      *    CATEGORY                                                     00170101
      *                                                                 00170201
      *--HISTORY LOG--------------------------------------------------  00171000
      *  SEQ  DATE       DESIGNER   DESCRIPTION                         00180000
      *  ---  ---------  ---------  --------------------------------    00190000
      *  001  SEP 2001   J ELKINS   CREATED                             00200000
      *                                                                 00200000
      *  002  AUG 2008   IBM        ENTERPRISE COMPILER UPGRADE(ECU)    00200000
      *----------------------------------------------------------------*00210000
                                                                        00220000
       ENVIRONMENT DIVISION.                                            00230000
                                                                        00240000
       CONFIGURATION SECTION.                                           00250000
                                                                        00260000
       SOURCE-COMPUTER. IBM-370.                                        00270000
       OBJECT-COMPUTER. IBM-370.                                        00280000
                                                                        00290000
       INPUT-OUTPUT SECTION.                                            00300000
                                                                        00310000
       FILE-CONTROL.                                                    00320000
                                                                        00330000
       DATA DIVISION.                                                   00340000
       FILE SECTION.                                                    00350000
                                                                        00360000
       WORKING-STORAGE SECTION.                                         00370000
       01  FILLER                             PIC X(40) VALUE           00380000
               '**   GACPOCC  WORKING STORAGE BEGINS  **'.              00390000
                                                                        00400000
                                                                        00403000
      * ENGLISH TABLE OF GIPSY CODES, OCCUPATIONS AND CATEGORIES        00403115
                                                                        00403215
       01  GACCETBL-ENGLISH-TABLE.                                      00404015
           COPY GACCETBL.                                               00405015
                                                                        00406001
      * FRENCH TABLE OF GIPSY CODES, OCCUPATIONS AND CATEGORIES         00406115
                                                                        00406215
       01  GACCFTBL-FRENCH-TABLE.                                       00406315
           COPY GACCFTBL.                                               00406415
                                                                        00406515
       01  WS-VARIABLES.                                                00407002
                                                                        00408002
           05  SUB1                           PIC S9(4) COMP VALUE 0.   00409002
                                                                        00409202
           05  WS-OCC-MATCH-SW                PIC X.                    00409313
               88  WS-OCC-MATCH-FOUND                   VALUE '1'.      00409413
               88  WS-OCC-MATCH-NOT-FOUND               VALUE '2'.      00409513
                                                                        00409602
       01  FILLER                             PIC X(40) VALUE           00411000
               '***  GACPOCC  WORKING STORAGE ENDS   ***'.              00420000
                                                                        00430000
       LINKAGE SECTION.                                                 00440000
      *----------------------------------------------------------------*00450000
      *  GACPOCC PARMS                                                  00460000
      *----------------------------------------------------------------*00470000
       01  GACPOCC-INPUTS.                                              00480000
           COPY GACCOCC.                                                00490000
                                                                        00500000
                                                                        00590000
      *----------------------------------------------------------------*00600000
       PROCEDURE DIVISION USING GACPOCC-INPUTS.                         00610000
      *----------------------------------------------------------------*00660000
                                                                        00661000
                                                                        00662000
      ****************************************************************  00670000
      *    MAINLINE                                                     00680000
      ****************************************************************  00690000
       0000-MAINLINE.                                                   00700000
           PERFORM 1000-INIT                                            00710000
              THRU 1000-EXIT.                                           00720000
                                                                        00730000
           IF GACCOCC-ENGLISH                                           00731016
               PERFORM 2000-FIND-GIPSY-CODE-ENG                         00740016
                  THRU 2000-EXIT                                        00750016
               VARYING SUB1 FROM 1 BY 1                                 00760016
                 UNTIL WS-OCC-MATCH-FOUND                               00780016
                    OR SUB1 >    GACCETBL-MAX-ENTRIES                   00790016
                                                                        00800000
           ELSE                                                         00801016
               PERFORM 2100-FIND-GIPSY-CODE-FR                          00802016
                  THRU 2100-EXIT                                        00803016
               VARYING SUB1 FROM 1 BY 1                                 00804016
                 UNTIL WS-OCC-MATCH-FOUND                               00805016
                    OR SUB1 >    GACCFTBL-MAX-ENTRIES                   00806016
                                                                        00807016
           END-IF.                                                      00808016
                                                                        00809016
           PERFORM 3000-FINISH                                          00810012
              THRU 3000-EXIT.                                           00820012
                                                                        00830000
       0000-MAINLINE-EXIT.                                              00840000
           GOBACK.                                                      00850000
                                                                        00860000
       1000-INIT.                                                       00870002
      ***************************************************************** 00880002
      * INITIALIZE RETURN VALUES IN GACCOCC                           * 00890004
      ***************************************************************** 00900002
                                                                        00910002
           MOVE SPACES                   TO GACCOCC-GIPSY-CODE          00910106
                                            GACCOCC-ENG-CATEGORY        00910204
                                            GACCOCC-FR-CATEGORY.        00910304
                                                                        00910404
           SET WS-OCC-MATCH-NOT-FOUND    TO TRUE.                       00911013
                                                                        00912004
       1000-EXIT.                                                       00920002
           EXIT.                                                        00930002
                                                                        00940004
       2000-FIND-GIPSY-CODE-ENG.                                        00950016
      ***************************************************************** 00960004
      * COMPARE OCCUPATION PASSED IN GACCOCC TO OCCUPATIONS IN        * 00970004
      * GACCETBL UNTIL A MATCH IS FOUND                               * 00971016
      ***************************************************************** 00980004
                                                                        00990004
           IF GACCOCC-ENG-OCC            =  GACCETBL-OCC (SUB1)         00992016
               MOVE GACCETBL-GIPSY-CODE (SUB1)                          00993016
                                         TO GACCOCC-GIPSY-CODE          00994016
               MOVE GACCETBL-CAT (SUB1)  TO GACCOCC-ENG-CATEGORY        00994116
               SET WS-OCC-MATCH-FOUND TO TRUE                           00995016
           END-IF.                                                      01000604
                                                                        01000704
       2000-EXIT.                                                       01001004
           EXIT.                                                        01010004
                                                                        01020004
       2100-FIND-GIPSY-CODE-FR.                                         01021016
      ***************************************************************** 01022016
      * COMPARE OCCUPATION PASSED IN GACCOCC TO OCCUPATIONS IN        * 01023016
      * GACCFTBL UNTIL A MATCH IS FOUND                               * 01024016
      ***************************************************************** 01025016
                                                                        01026016
           IF GACCOCC-FR-OCC             =  GACCFTBL-OCC (SUB1)         01027016
               MOVE GACCFTBL-GIPSY-CODE (SUB1)                          01028016
                                         TO GACCOCC-GIPSY-CODE          01029016
               MOVE GACCFTBL-CAT (SUB1)  TO GACCOCC-FR-CATEGORY         01030016
               SET WS-OCC-MATCH-FOUND TO TRUE                           01040016
           END-IF.                                                      01050016
                                                                        01060016
       2100-EXIT.                                                       01070016
           EXIT.                                                        01080016
                                                                        01081016
       3000-FINISH.                                                     01090016
      ***************************************************************** 01100016
      * SET RETURN CODE                                               * 01110016
      ***************************************************************** 01120016
                                                                        01130016
           IF WS-OCC-MATCH-NOT-FOUND                                    01140016
               SET GACCOCC-MATCH-NOT-FOUND         TO TRUE              01150016
           ELSE                                                         01160016
               SET GACCOCC-SUCCESSFUL          TO TRUE                  01200016
           END-IF.                                                      01220016
                                                                        01230016
       3000-EXIT.                                                       01240016
           EXIT.                                                        01250016
