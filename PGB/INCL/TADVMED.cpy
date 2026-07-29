      ******************************************************************        
      * DCLGEN TABLE(DBCYCD3.TADVME)                                   *        
      *        LIBRARY(TYT.IGAPS.DCLGEN(TADVMEMH))                     *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TADVME TABLE                                        
           ( MGR_USER_ID                    CHAR(20) NOT NULL,                  
             EMP_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             REQ_TS                         TIMESTAMP NOT NULL                  
           ) END-EXEC.                                                          
