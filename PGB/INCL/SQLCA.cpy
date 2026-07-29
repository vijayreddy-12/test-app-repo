************************************************************************
******* Standard SQLCA.CPY copybook, defining the SQL communication area
******* data structure. 
******* 
******* It is structured to be format-compatible with the similar DB2
******* communication area data structure, to ensure compatibility
******* with application that depend on how it is mapped in memory.
************************************************************************
       01 SQLCA.
         05  SQLCAID PIC X(08)         VALUE "SQLCA   ".
         05  SQLCABC PIC S9(08) COMP-5 VALUE 777.
         05  SQLCODE PIC S9(08) COMP-5 VALUE 0.
         05  SQLERRM.
           10  SQLERRML PIC S9(04) COMP-5.
           10  SQLERRMC PIC X(70).
         05  SQLERRP PIC X(08).
         05  SQLERRD PIC S9(08) COMP-5 
                     OCCURS 6 TIMES.
         05  SQLWARN.
           10  SQLWARN0 PIC X(01).
           10  SQLWARN1 PIC X(01).
           10  SQLWARN2 PIC X(01).
           10  SQLWARN3 PIC X(01).
           10  SQLWARN4 PIC X(01).
           10  SQLWARN5 PIC X(01).
           10  SQLWARN6 PIC X(01).
           10  SQLWARN7 PIC X(01).
           10  SQLWARN8 PIC X(01).
         05 SQLEXT.
           10  SQLWARN9 PIC X(01).
           10  SQLWARN10 PIC X(01).
           10  SQLWARNA REDEFINES SQLWARN10 
                        PIC X(01).
         05  SQLWARNS REDEFINES SQLWARN
                        PIC X(10).
         05  SQLSTATE PIC X(05).
         05  FILLER   PIC X(21).
