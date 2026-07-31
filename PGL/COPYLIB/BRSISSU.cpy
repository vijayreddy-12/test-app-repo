      ******************************************************************00001000
      *    COPYBOOK     : BRSISSU                                       00001102
      *    PROJECT      : BANKING RECONCILIATION SYSTEM (BRS)           00001202
      *    DATE         : AUGUST 18, 2004                               00001302
      *    AUTHOR       : TARIQ NOOR                                    00001402
      *                                                                 00001502
      *    DESCRIPTION  : THIS RECORD LAYOUT PROVIDES THE DATA ON       00001602
      *                   CHEQUES ISSUED FROM DIFFERENT APPLICATION     00001702
      *                   TO BRS.                                       00001902
      *                                                                 00002002
      *    RECORD TYPES : 1). DETAIL RECORD                             00002102
      *                   2). TRAILER RECORD                            00002202
      *                                                                 00002302
      ******************************************************************00003002
      *    DETAIL RECORD                                                00003102
      ******************************************************************00004002
       01  BRS-ISSUE-DETAIL-RECORD.                                     00010003
      *                                                                 00011000
           05  DTL-INTERFACE-FILE-TYPE      PIC  X(03).                 00020010
               88  DTL-INTR-FLE-TYP         VALUE  'ISS'.               00020110
      *                                     CONSTANT FOR CHEQUE ISSUES  00021004
           05  DTL-SOURCE-CODE              PIC  X(03).                 00030002
      *                                     VALUE ASSIGNED BY TREASURY  00031001
           05  DTL-BUSINESS-DATE.                                       00040007
               10  DTL-BUS-DTE-CCYY         PIC  X(04).                 00040107
               10  DTL-BUS-DTE-MM           PIC  X(02).                 00040207
               10  DTL-BUS-DTE-DD           PIC  X(02).                 00040307
           05  DTL-ACCOUNT-NUM              PIC  X(05).                 00050002
           05  DTL-MICR-CHEQUE-NUM          PIC  9(10).                 00060002
           05  DTL-CHEQUE-DATE.                                         00070008
               10  DTL-CHQ-DTE-CCYY         PIC  X(04).                 00070107
               10  DTL-CHQ-DTE-MM           PIC  X(02).                 00070207
               10  DTL-CHQ-DTE-DD           PIC  X(02).                 00070307
           05  DTL-CHEQUE-AMOUNT            PIC  9(09)V99.              00080002
           05  DTL-REFERENCE-NUM            PIC  X(13).                 00090002
           05  DTL-POLICY-NUM               PIC  X(12).                 00100002
           05  DTL-CERTIFICATE              PIC  X(10).                 00110002
           05  DTL-DIVISION-NUM             PIC  X(03).                 00120002
           05  DTL-PAYEE-NAME               PIC  X(40).                 00130002
           05  DTL-CLERK-ID                 PIC  X(07).                 00140002
           05  DTL-CL2-CODE                 PIC  X(05).                 00150002
           05  FILLER                       PIC  X(22) VALUE SPACES.    00160002
      *                                                                 00161005
      ******************************************************************00170000
      *    TRAILER RECORD                                               00171002
      ******************************************************************00180002
       01  BRS-ISSUE-TRAILER-RECORD.                                    00190003
      *                                                                 00200002
           05  TRL-BUSINESS-DATE.                                       00211009
               10  TRL-BUS-DTE-CCYY         PIC  X(04).                 00211107
               10  FILLER                   PIC  X      VALUE '-'.      00211207
               10  TRL-BUS-DTE-MM           PIC  X(02).                 00211307
               10  FILLER                   PIC  X      VALUE '-'.      00211407
               10  TRL-BUS-DTE-DD           PIC  X(02).                 00211507
      *                                     FORMAT CCYY-MM-DD           00212002
           05  TRL-SOURCE-CODE              PIC  X(03).                 00213002
           05  TRL-TOTAL-REC-CNT            PIC  9(09).                 00240002
      *                                     VALUE TO BE COMPUTED        00241002
           05  TRL-TOTAL-DOLLAR-AMT         PIC  9(13)V99.              00250002
      *                                     VALUE TO BE COMPUTED        00252002
           05  FILLER                       PIC  X(123) VALUE SPACES.   00260002
      ******************************************************************00270006
