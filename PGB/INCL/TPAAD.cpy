      ******************************************************************        
      * DCLGEN TABLE(TPAA)                                             *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TPAA TABLE                                          
           ( CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             GROUP_ID                       CHAR(7) NOT NULL,                   
             DIV_ID                         CHAR(3) NOT NULL,                   
             PA_STAT_CD                     CHAR(1) NOT NULL,                   
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_TS                         TIMESTAMP NOT NULL,                 
             BILL_PRSNT_IND                 CHAR(1) NOT NULL,                   
             PAY_HIST_IND                   CHAR(1) NOT NULL                    
           ) END-EXEC.                                                          
