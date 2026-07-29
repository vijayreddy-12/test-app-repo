           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C C R D L                         *        
      *                                                                *        
      *   1. CONTROL CARD INPUT TO GCCPFRML                            *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C C R D L                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 010009 - JE    NEW COPYBOOK                              *        
      *                                                                *        
      ******************************************************************        
      *01  GCCCCRDL-RECORD.                                                     
           05  FILLER                              PIC X(1).                    
               88  GCCCCRDL-CTL-CARD-PRESENT       VALUE ' '.                   
               88  GCCCCRDL-COMMENT                VALUE '*'.                   
           05  GCCCCRDL-MONTH                      PIC X(2).                    
           05  FILLER                              PIC X(1).                    
           05  GCCCCRDL-YEAR                       PIC X(4).                    
           05  FILLER                              PIC X(71).                   
