      ***************************************************************** 00010000
      * DCLGEN TABLE (TCTDPT)                                        *  00020001
      *        LANGUAGE (COBOL)                                       * 00030000
      *        QUOTE                                                  * 00040000
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  * 00050000
      ***************************************************************** 00060000
           EXEC SQL DECLARE TCTDPT TABLE                                00070001
           ( CODE_VALUE                  CHAR(4) NOT NULL,              00080000
             CODE_ENG_DESC               CHAR(50),                      00090000
             CODE_FR_DESC                CHAR(50)                       00100000
           ) END-EXEC.                                                  00140000
