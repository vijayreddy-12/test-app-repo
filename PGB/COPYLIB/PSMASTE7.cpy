       01  PSMASTE7.                                                    00010000
      *****************************************************************         
      *    OCT/10 C. MUNCAN                                                     
      *         - IDMS TO DB2 PROJECT 7-DIGIT GROUP NUMBER EXPANSION            
      *    SEP/17 COGNIZANT                                                     
      *         - ADDED RENEWAL NOTICE PERIOD AND INDICATOR FIELDS AT           
      *         - THE END OF LAYOUT                                             
      *****************************************************************         
           05  PSMRECORD-LENGTH        COMP SYNC   PIC S9(04).          00020000
           05  FILLER                              PIC X(02).           00030000
           05  PSMGROUP-NUMBER         COMP-3      PIC S9(07).          00040000
           05  PSMACCOUNT                          PIC  X(03).          00050000
           05  PSMRECORD-IDENT                     PIC  X(01).          00060000
           05  PSMIDENTITY                         PIC  X(05).          00070000
           05  PSMPOLICY-YEAR              COMP-3  PIC S9(03).          00080000
           05  PSMPAID-CUR-POL-YR      COMP-3      PIC S9(7)V99.        00090000
           05  PSMPAID-LST-POL-YR      COMP-3      PIC S9(7)V99.        00100000
           05  PSMSERV-CUR-POL-YR      COMP-3      PIC S9(5)V99.        00110000
           05  PSMSERV-LST-POL-YR      COMP-3      PIC S9(5)V99.        00120000
           05  PSMLAST-CHG-DATE.                                        00130000
             07  PSMLST-CHG-YR         COMP-3      PIC S9(03).          00140000
             07  PSMLST-CHG-DAY        COMP-3      PIC S9(03).          00150000
           05  PSMNXT-RNEW-DATE.                                        00160000
             07  PSMNXT-RNEW-YR            COMP-3  PIC S9(03).          00170000
             07  PSMNXT-RNEW-DAY           COMP-3  PIC S9(03).          00180000
           05  PSM-SEGMENT-INFO  OCCURS 05  TIMES.                      00190000
             07  PSMPOINT                          PIC  X(01).          00200000
             07  PSMCOMB-IND                       PIC  X(01).          00210000
             07  PSMPREM-FRST-YR-LIFE      COMP-3  PIC S9(7)V99.        00220000
             07  PSMPREM-FRST-YR-LTD       COMP-3  PIC S9(7)V99.        00230000
             07  PSMPREM-FRST-YR-A-S       COMP-3  PIC S9(7)V99.        00240000
             07  PSMPREM-FRST-YR-ADMIN-FEE COMP-3  PIC S9(7)V99.        00250000
             07  PSMRNEW-CUR-YR-LIFE       COMP-3  PIC S9(7)V99.        00260000
             07  PSMRNEW-CUR-YR-LTD        COMP-3  PIC S9(7)V99.        00270000
             07  PSMRNEW-CUR-YR-A-S        COMP-3  PIC S9(7)V99.        00280000
             07  PSMRNEW-CUR-YR-ADMIN-FEE  COMP-3  PIC S9(7)V99.        00290000
             07  PSMRNEW-LST-YR-LIFE       COMP-3  PIC S9(7)V99.        00300000
             07  PSMRNEW-LST-YR-LTD        COMP-3  PIC S9(7)V99.        00310000
             07  PSMRNEW-LST-YR-A-S        COMP-3  PIC S9(7)V99.        00320000
             07  PSMRNEW-LST-YR-ADMIN-FEE  COMP-3  PIC S9(7)V99.        00330000
           05  PSMGROUP-NOTC-PRD                   PIC 9(03).           00050000
           05  PSMGROUP-INDICATOR                  PIC X(01).           00050000
