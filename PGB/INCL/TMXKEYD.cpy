      ******************************************************************        
      * DCLGEN TABLE(TMXKEY)                                           *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TMXKEY TABLE                                        
           ( KEY_TYP                        CHAR(10) NOT NULL,                  
             MAX_KEY_VALUE                  DECIMAL(11, 0) NOT NULL             
           ) END-EXEC.                                                          
