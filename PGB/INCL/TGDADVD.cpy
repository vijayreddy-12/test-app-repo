      ******************************************************************        
      * DCLGEN TABLE(TGDADVD                                           *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TGDADV TABLE                                        
           ( GROUP_ID          CHAR(07) NOT NULL,                               
             DIV_ID            CHAR(03) NOT NULL,                               
             ADVISOR_ID        CHAR(06) NOT NULL,                               
             CHN_TS            TIMESTAMP NOT NULL,                              
             ACC_IND           CHAR(01) NOT NULL WITH DEFAULT,                  
             ADVISOR_AGENCY_ID CHAR(06) NOT NULL                                
           ) END-EXEC.                                                          
