      ******************************************************************        
      * DCLGEN TABLE(TUT)                                              *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TUT TABLE                                           
           ( USER_ID                        CHAR(20) NOT NULL,                  
             CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_TS                         TIMESTAMP NOT NULL,                 
             MAPPED_REG_ID                  CHAR(20),                           
             MAPPED_ID                      CHAR(20)                            
           ) END-EXEC.                                                          
