      ***************************************************************** 00010000
      * DCLGEN TABLE (TCPMUS)                                         * 00020000
      *        LANGUAGE (COBOL)                                       * 00030000
      *        QUOTE                                                  * 00040000
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  * 00050000
      ***************************************************************** 00060000
           EXEC SQL DECLARE TCPMUS TABLE                                00070000
           ( CPM_USER_ID                 CHAR(10) NOT NULL,             00080000
             CPM_FIRST_NAME              CHAR(30) NOT NULL,             00090000
             CPM_LAST_NAME               CHAR(30) NOT NULL,             00100000
             DEPT                        CHAR(4)  NOT NULL,             00110000
             CHN_USER_ID                 CHAR(20) NOT NULL,             00120000
             CHN_TS                      TIMESTAMP NOT NULL             00130000
           ) END-EXEC.                                                  00140000
