      ******************************************************************        
      * DCLGEN TABLE(TSD)                                              *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TSD TABLE                                           
           ( CONFIRM_ID                     DECIMAL(11, 0) NOT NULL,            
             SEQ_NUM                        DECIMAL(5, 0) NOT NULL,             
             FORM_CD                        CHAR(8) NOT NULL,                   
             LANG_CD                        CHAR(1) NOT NULL,                   
             GROUP_ID                       CHAR(7) NOT NULL,                   
             DIV_ID                         CHAR(3) NOT NULL,                   
             CERT_ID                        CHAR(10) NOT NULL,                  
             WEB_SENT_TS                    TIMESTAMP NOT NULL,                 
             FORM_RECV_TS                   TIMESTAMP NOT NULL,                 
             SENT_TO_PRINT_TS               TIMESTAMP,                          
             PURGE_TS                       TIMESTAMP,                          
             FORM_ID                        DECIMAL(11, 0) NOT NULL,            
             FORM_SEQ_NUM                   DECIMAL(3, 0) NOT NULL              
           ) END-EXEC.                                                          
