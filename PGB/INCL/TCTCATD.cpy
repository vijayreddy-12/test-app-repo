      ******************************************************************        
      * DCLGEN TABLE(TCTCAT)                                           *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TCTCAT TABLE                                        
           ( CODE_VALUE                     CHAR(6) NOT NULL,                   
             CODE_ENG_DESC                  CHAR(50) NOT NULL,                  
             CODE_FR_DESC                   CHAR(50) NOT NULL                   
           ) END-EXEC.                                                          
