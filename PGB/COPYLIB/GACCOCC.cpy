           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G A C C O C C                           *        
      *                                                                *        
      *   1. GACCOCC PARAMATERS (PASSED BETWEEN THE CALLING MODULE     *        
      *      AND GACPOCC.  THIS PROGRAM WILL RECEIVE AN OCCUPATION     *        
      *      DESCRIPTION IN FRENCH OR ENGLISH AND RETURN A GIPSY       *        
      *      CODE AND CATEGORY DESCRIPTION                             *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G A C C O C C                           *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 210901 - JE    NEW COPYBOOK                              *        
      *                                                                *        
      ******************************************************************        
      *01  GACCOCC-RECORD.                                                      
           05  GACCOCC-TRANS-TYPE        PIC X(2).                              
               88  GACCOCC-DESC-TO-CODE            VALUE '01'.                  
           05  GACCOCC-RETURN-CODE       PIC X(2).                              
               88  GACCOCC-SUCCESSFUL              VALUE '00'.                  
               88  GACCOCC-MATCH-NOT-FOUND         VALUE '01'.                  
               88  GACCOCC-FATAL-ERROR             VALUE '99'.                  
           05  GACCOCC-LANGUAGE          PIC X(1).                              
               88  GACCOCC-ENGLISH                 VALUE 'E'.                   
               88  GACCOCC-FRENCH                  VALUE 'F'.                   
           05  GACCOCC-ENG-OCC           PIC X(29).                             
           05  GACCOCC-FR-OCC            PIC X(29).                             
           05  GACCOCC-GIPSY-CODE        PIC X(02).                             
           05  GACCOCC-ENG-CATEGORY      PIC X(27).                             
           05  GACCOCC-FR-CATEGORY       PIC X(27).                             
                                                                                
