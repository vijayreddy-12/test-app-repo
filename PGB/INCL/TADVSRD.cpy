      ******************************************************************        
      * DCLGEN TABLE(TADVSR                                            *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TADVSR TABLE                                        
           ( CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             ADVISOR_ID                     CHAR(06) NOT NULL,                  
             CHN_TS                         TIMESTAMP NOT NULL                  
           ) END-EXEC.                                                          
