      ***************************************************************** 00010000
      * DCLGEN TABLE (TGDFA)                                          * 00020000
      *        LANGUAGE (COBOL)                                       * 00030000
      *        QUOTE                                                  * 00040000
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  * 00050000
      ***************************************************************** 00060000
           EXEC SQL DECLARE TGDFA TABLE                                 00070000
           ( GROUP_ID                    CHAR(7)  NOT NULL,             00080000
             DIV_ID                      CHAR(3)  NOT NULL,             00090000
             SITE                        CHAR(4)  NOT NULL,             00100000
             FUNCTION_NAME               CHAR(8)  NOT NULL,             00110000
             ACC_IND                     CHAR(1)  NOT NULL,             00111000
             CHN_USER_ID                 CHAR(20) NOT NULL,             00120000
             CHN_TS                      TIMESTAMP NOT NULL,            00130000
             SUBMIT_TYP_CD               CHAR(1)  NOT NULL              00131001
           ) END-EXEC.                                                  00140000
