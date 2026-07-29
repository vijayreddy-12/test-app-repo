      ******************************************************************        
      * DCLGEN TABLE(TEH)                                              *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TEH TABLE                                           
           ( CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             EVENT_TYP_CD                   CHAR(6) NOT NULL,                   
             EVENT_TS                       TIMESTAMP NOT NULL                  
           ) END-EXEC.                                                          
