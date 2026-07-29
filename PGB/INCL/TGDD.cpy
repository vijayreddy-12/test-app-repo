      ******************************************************************        
      * DCLGEN TABLE(TGD)                                              *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TGD TABLE                                           
           ( GROUP_ID                       CHAR(7) NOT NULL,                   
             DIV_ID                         CHAR(3) NOT NULL,                   
             ACC_PM_REGIS_CD                DECIMAL(7, 0) NOT NULL,             
             SPONSOR_NAME                   CHAR(60) NOT NULL,                  
             SPONSOR_NAME_CAPS              CHAR(60) NOT NULL,                  
             REG_CD                         CHAR(1) NOT NULL,                   
             CONTRACT_STAT_CD               CHAR(1) NOT NULL,                   
             CONTRIB_IND                    CHAR(1) NOT NULL,                   
             AUTH_PMSS_IND                  CHAR(1) NOT NULL,                   
             AUTH_PA_IND                    CHAR(1) NOT NULL,                   
             AUTH_PM_ENROL_IND              CHAR(1) NOT NULL,                   
             BUS_SEG_CD                     CHAR(1) NOT NULL,                   
             REG_GROUP_OFFICE               CHAR(5) NOT NULL,                   
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_TS                         TIMESTAMP NOT NULL,                 
             MULTI_GROUP_IND                CHAR(1) NOT NULL,                   
             TAT                            CHAR(2) NOT NULL,                   
             CLM_REG_CD                     CHAR(4) NOT NULL,                   
             HLTH_DENT_ONLY_IND             CHAR(1) NOT NULL,                   
             TMPLT_ID                       DECIMAL(3, 0) NOT NULL              
           ) END-EXEC.                                                          
