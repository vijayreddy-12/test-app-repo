      ******************************************************************        
      * DCLGEN TABLE(TGCT)                                             *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TGCT TABLE                                          
           ( GROUP_ID                       CHAR(7) NOT NULL,                   
             CERT_ID                        CHAR(10) NOT NULL,                  
             CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_TS                         TIMESTAMP NOT NULL                  
           ) END-EXEC.                                                          
