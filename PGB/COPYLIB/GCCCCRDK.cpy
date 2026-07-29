           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C C R D K                         *        
      *                                                                *        
      *   1. CONTROL CARD INPUT TO GCCPFRMK                            *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C C R D K                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 010802 - JE    NEW COPYBOOK                              *        
      *                                                                *        
      ******************************************************************        
      *01  GCCCCRDK-RECORD.                                                     
           05  FILLER                              PIC X(1).                    
               88  GCCCCRDK-CTL-CARD-PRESENT       VALUE ' '.                   
               88  GCCCCRDK-COMMENT                VALUE '*'.                   
           05  GCCCCRDK-MONTH1                     PIC X(2).                    
           05  FILLER                              PIC X(1).                    
           05  GCCCCRDK-YEAR1                      PIC X(4).                    
           05  FILLER                              PIC X(1).                    
           05  GCCCCRDK-MONTH2                     PIC X(2).                    
           05  FILLER                              PIC X(1).                    
           05  GCCCCRDK-YEAR2                      PIC X(4).                    
           05  FILLER                              PIC X(1).                    
           05  GCCCCRDK-MONTH3                     PIC X(2).                    
           05  FILLER                              PIC X(1).                    
           05  GCCCCRDK-YEAR3                      PIC X(4).                    
           05  FILLER                              PIC X(54).                   
