      ******************************************************************        
      * DCLGEN TABLE(TFORM)                                            *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TFORM TABLE                                         
           ( FORM_ID                        DECIMAL(11, 0) NOT NULL,            
             FORM_SEQ_NUM                   DECIMAL(3, 0) NOT NULL,             
             FORM_DATA_STRING               VARCHAR(32000) NOT NULL             
           ) END-EXEC.                                                          
