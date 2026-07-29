      ******************************************************************        
      * DCLGEN TABLE(TAUDEV)                                           *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TAUDEV TABLE                                        
           ( CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             AUD_TYP_CD                     CHAR(6)        NOT NULL,            
             CHN_USER_ID                    CHAR(20)       NOT NULL,            
             CHN_TS                         TIMESTAMP      NOT NULL,            
             SIGNON_USER_ID                 CHAR(20)       NOT NULL             
           ) END-EXEC.                                                          
