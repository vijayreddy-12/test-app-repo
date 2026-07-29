      ******************************************************************        
      * DCLGEN TABLE(TCPMU)                                            *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TCPMU TABLE                                         
           ( CPM_USER_ID                    CHAR(10) NOT NULL,                  
             CPM_ACC_TYP_CD                 CHAR(6) NOT NULL,                   
             CPM_USER_STAT_CD               CHAR(1) NOT NULL,                   
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_TS                         TIMESTAMP NOT NULL                  
           ) END-EXEC.                                                          
