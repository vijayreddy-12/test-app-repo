007200*01  WS-INTERFACE-DUMP-AREA.                                      00740001
007200     03 INTERFACE-DUMP.                                           00741001
    00        05 ID-PROGRAM-NAME               PIC X(08).               00760001
    00        05 ID-ERROR-MESSAGE              PIC X(58).               00770001
    00        05 ID-ACTION                     PIC X(16).               00780001
    00        05 ID-LR-NAME                    PIC X(32).               00790001
    00        05 ID-ERROR-STATUS.                                       00800001
                 10 ID-LR-STATUS               PIC X(08).               00810001
                 10 ID-SQL-CODE                PIC S9(09).              00820001
    00        05 ID-GROUP                      PIC 9(07).               00830001
    00        05 ID-AC                         PIC X(03).               00840001
    00        05 ID-CERTIFICATE.                                        00850001
                 10 ID-CERTIFICATE-N           PIC 9(09).               00860001
                 10 FILLER                     PIC X.                   00870001
    00        05 ID-SEVERITY                   PIC X.                   00880001
                 88 ID-FATAL-ERROR             VALUE 'F'.               00890001
                 88 ID-WARNING                 VALUE 'W'.               00900001
