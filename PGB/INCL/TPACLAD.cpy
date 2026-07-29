      *****************************************************************         
      * DCLGEN TABLE(TPACLA)                                                    
      *        LANGUAGE(COBOL)                                                  
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS            
      *****************************************************************         
           EXEC SQL DECLARE TPACLA TABLE                                        
           ( CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             CLIENT_NUM                     CHAR(7) NOT NULL,                   
             LOC_NUM                        CHAR(3) NOT NULL,                   
             PA_STAT_CD                     CHAR(1) NOT NULL,                   
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_TS                         TIMESTAMP NOT NULL,                 
             BILL_PRSNT_IND                 CHAR(1) NOT NULL,                   
             PAY_HIST_IND                   CHAR(1) NOT NULL,                   
             EXP_RPT_IND                    CHAR(1) NOT NULL                    
           ) END-EXEC.                                                          
