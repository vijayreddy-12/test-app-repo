      ******************************************************************        
      * DCLGEN TABLE(TLH)                                              *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TLH TABLE                                           
           ( CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             LOGIN_TS                       TIMESTAMP NOT NULL,                 
             LOGIN_SUCC_IND                 CHAR(1) NOT NULL,                   
             LOGIN_TYP_CD                   CHAR(1) NOT NULL                    
           ) END-EXEC.                                                          
