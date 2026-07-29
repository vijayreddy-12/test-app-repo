      ******************************************************************        
      * DCLGEN TABLE(TCUST)                                            *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
           EXEC SQL DECLARE TCUST TABLE                                         
           ( CUST_ID                        DECIMAL(11, 0) NOT NULL,            
             ENCRYP_PSWRD                   CHAR(22) NOT NULL,                  
             PSWRD_EXP_DT                   DATE,                               
             PSWRD_REMIND                   CHAR(50) NOT NULL,                  
             CUST_STAT_CD                   CHAR(1) NOT NULL,                   
             CUST_STAT_REAS_CD              CHAR(1) NOT NULL,                   
             CUST_FIRST_NAME                CHAR(30) NOT NULL,                  
             FIRST_NAME_CAPS                CHAR(30) NOT NULL,                  
             CUST_INIT                      CHAR(1) NOT NULL,                   
             CUST_LAST_NAME                 CHAR(30) NOT NULL,                  
             LAST_NAME_CAPS                 CHAR(30) NOT NULL,                  
             CUST_ADDR_1                    CHAR(60),                           
             CUST_ADDR_2                    CHAR(60),                           
             CUST_ADDR_CITY                 CHAR(60),                           
             CUST_ADDR_PROV                 CHAR(30),                           
             CUST_ADDR_COUNTRY              CHAR(30),                           
             CUST_ADDR_POST_CD              CHAR(9),                            
             EMAIL_ADDR                     CHAR(60) NOT NULL,                  
             CUST_DOB                       DATE,                               
             CUST_GENDER_CD                 CHAR(1),                            
             LANG_CD                        CHAR(1),                            
             SPONSOR_NAME                   CHAR(60),                           
             CHN_USER_ID                    CHAR(20) NOT NULL,                  
             CHN_TS                         TIMESTAMP NOT NULL,                 
             ROLE                           CHAR(1),                            
             REG_CNFRM_LTR                  CHAR(1),                            
             BUS_PHONE_NUM                  CHAR(10),                           
             BUS_PHONE_EXT                  CHAR(06),                           
             REG_GROUP_OFFICE               CHAR(05),                           
             ACTVTN_KEY                     CHAR(6)                             
           ) END-EXEC.                                                          
