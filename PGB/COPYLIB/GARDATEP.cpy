      *01  GUS-DATE-PARAMETERS.                                                 
      *****************************************************************         
      *    GACDATE  PARAMETERS                                         *        
      *    FORMERLAY GUSDATE PARAMETERS                               *         
      *****************************************************************         
           05  VDATE-REQUEST-AREA.                                              
               10  VDATE-REQ-SERVICE                       PIC X.               
               10  VDATE-REQ-BASIS                         PIC X.               
               10  VDATE-REQ-DETAIL                        PIC X.               
               10  VDATE-REQ-LANGUAGE                      PIC X.               
                   88  VDATE-REQ-ENGLISH  VALUE 'E'.                            
                   88  VDATE-REQ-FRENCH   VALUE 'F'.                            
           05  VDATE-RETURN-AREA.                                               
               10  VDATE-RET-IND                           PIC X.               
                   88  VDATE-RET-VALID    VALUE '0'.                            
                   88  VDATE-RET-FAIL     VALUE '1'.                            
               10  VDATE-RET-CODE            COMP-3        PIC S9(3).           
           05  VDATE-DATE-AREA.                                                 
               10  VDATE-EXT-DATE.                                              
                   15  VDATE-EXT-DAY                       PIC 99.              
                   15  VDATE-EXT-MONTH                     PIC XXX.             
                   15  VDATE-EXT-YEAR                      PIC 9999.            
                   15  VDATE-EXT-YEAR-SHORT                                     
                                             REDEFINES VDATE-EXT-YEAR           
                                                           PIC 99.              
               10  VDATE-DDMMYY              REDEFINES VDATE-EXT-DATE.          
                   15  VDATE-DD                            PIC 99.              
                   15  VDATE-MM                            PIC 99.              
                   15  VDATE-YY                            PIC 99.              
               10  VDATE-JULIAN-DATE         REDEFINES VDATE-EXT-DATE.          
                   15  VDATE-JULIAN-CC                     PIC 99.              
                   15  VDATE-JULIAN-YY                     PIC 99.              
                   15  VDATE-JULIAN-DDD                    PIC 9(3).            
               10  VDATE-ALIS-DATE1          COMP-3.                            
                   15  VDATE-ALIS-YEAR1                    PIC S9(3).           
                   15  VDATE-ALIS-DAY1                     PIC S9(3).           
               10  VDATE-ALIS-DATE2          COMP-3.                            
                   15  VDATE-ALIS-YEAR2                    PIC S9(3).           
                   15  VDATE-ALIS-DAY2                     PIC S9(3).           
               10  VDATE-CII-DATE            REDEFINES VDATE-ALIS-DATE2         
                                             COMP-3        PIC S9(7).           
               10  VDATE-ADJUST-AREA         COMP-3.                            
                   15  VDATE-ADJUST-DAYS                   PIC S9(3).           
                   15  VDATE-DAY-OF-WEEK     REDEFINES VDATE-ADJUST-DAYS        
                                                           PIC S9(3).           
                   15  VDATE-ADJUST-MONTHS                 PIC S9(3).           
                   15  VDATE-ADJUST-YEARS                  PIC S9(3).           
               10  VDATE-USE-TIME.                                              
                   15  VDATE-USE                           PIC XX.              
                   15  VDATE-TIME            COMP-3        PIC S9(2).           
               10  VDATE1-YYYYMMDD.                                             
                   15  VDATE1-YYYY                         PIC 9999.            
                   15  VDATE1-MM                           PIC 99.              
                   15  VDATE1-DD                           PIC 99.              
