      ******************************************************************        
      * DCLGEN TABLE(TCL)                                              *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TCL TABLE                                           
           ( CLIENT_NUM                     CHAR(7) NOT NULL,                   
             LOC_NUM                        CHAR(3) NOT NULL,                   
             LOC_STAT_CD                    CHAR(1) NOT NULL,                   
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_TS                         CHAR(26) NOT NULL                   
           ) END-EXEC.                                                          
