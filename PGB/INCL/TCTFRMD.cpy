      ******************************************************************        
      * DCLGEN TABLE(TCTFRM)                                           *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TCTFRM TABLE                                        
           ( CODE_VALUE                     CHAR(8) NOT NULL,                   
             CODE_ENG_DESC                  CHAR(50) NOT NULL,                  
             CODE_FR_DESC                   CHAR(50) NOT NULL                   
           ) END-EXEC.                                                          
