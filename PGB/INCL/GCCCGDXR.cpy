           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G C C C G D X R                         *        
      *                                                                *        
      *   1. GROUP/DIVISION EXTRACT RECORD LAYOUT                      *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G C C C 3 5 7 4                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE       PGR   DESCRIPTION                             *        
      *  --   ------     ---   --------------------------------------  *        
      *                                                                *        
      *  00 - 20020613 - JVH   ECOMM 4.2  - NEW COPYBOOK               *        
      *  01 - 20070511 - WB    ADD VO CLIENT AND LOCATION NUMBER                
      ******************************************************************        
      *01  GCCCGDXR-RECORD.                                                     
           05  GCCCGDXR-CONTRACT-ID                PIC X(7).                    
           05  GCCCGDXR-DIVISION-ID                PIC X(3).                    
           05  GCCCGDXR-STATUS-CD                  PIC X.                       
           05  GCCCGDXR-SPONSOR-NAME               PIC X(60).                   
           05  GCCCGDXR-REG-GROUP-OFFICE           PIC X(5).                    
           05  GCCCGDXR-REGION-CD                  PIC X.                       
           05  GCCCGDXR-BUS-SEG-CD                 PIC X.                       
           05  GCCCGDXR-ADVISOR-DATA.                                           
               10  GCCCGDXR-ADVISOR-ID1            PIC X(6).                    
               10  GCCCGDXR-ADVISOR-ID2            PIC X(6).                    
               10  GCCCGDXR-ADVISOR-ID3            PIC X(6).                    
               10  GCCCGDXR-ADVISOR-ID4            PIC X(6).                    
           05  FILLER    REDEFINES GCCCGDXR-ADVISOR-DATA.                       
               10  GCCCGDXR-ADVISOR-ID  OCCURS 4   PIC X(6).                    
           05  GCCCGDXR-EHCRPT-IND                 PIC X.                       
           05  GCCCGDXR-DSBRPT-IND                 PIC X.                       
           05  GCCCGDXR-CLIENT-NUM                 PIC X(7).                    
           05  GCCCGDXR-LOCATION-NUM               PIC X(3).                    
           05  GCCCGDXR-LOC-STATUS-CD              PIC X.                       
