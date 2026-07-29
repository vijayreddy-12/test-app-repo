      ******************************************************************        
      *    THIS COPYBOOK IS PART OF THE GENERAL CONTENT SERVICE                 
      *       MODULES ARE PREFIXED BY MLCT....                                  
      *    A COPYBOOK OF THE SAME NAME EXISTS AS A CPYC TYPE                    
      *    ENSURING THAT CIIBC TYPE MODULES COMPILE PROPERLY                    
      *    INTO CICS AND BATCH PROGRAMS.                                        
      ******************************************************************        
      *    PLEASE NOTE THAT THE ARRAY SIZES IMPACT THE AMOUNT OF       *        
      *    PROGRAM STORAGE REQUIRED TO EXECUTE THE LOOKUP.  SINCE      *        
      *    BATCH RUN-UNITS HAVE VIRTUALLY ENDLESS AMOUNTS OF STORAGE   *        
      *    THESE LIMITATIONS ARE QUITE HIGH. ANY ARRAY SIZE CHANGES    *        
      *    MUST BE ACCOMPANIED BY A CHANGE TO COPYBOOK MLCTCRIN.       *        
      *----------------------------------------------------------------*        
      *  INTERNAL REGISTRY OF LOOKUPS PREVIOUSLY ACCESSED.                      
      *----------------------------------------------------------------*        
       01  LOOKUP-REGISTRY-TABLE.                                               
           05  LURT-COUNT                      PIC S9(04) COMP.                 
           05  LOOKUP-REG-TABLE-ENTRY   OCCURS 100 TIMES.                       
                                                                                
               10  LURT-LOOKUP-NAME            PIC X(08).                       
               10  LURT-LOOKUP-ALGORITHM       PIC X(01).                       
                   88  LURT-DYNAMIC-LOOKUP     VALUE 'D'.                       
                   88  LURT-CACHED-LOOKUP      VALUE 'C'.                       
               10  LURT-SEARCH-PROPERTIES.                                      
                   15  LURT-FIRST-ENTRY        PIC S9(04) COMP.                 
                   15  LURT-LAST-ENTRY         PIC S9(04) COMP.                 
                   15  LURT-DEFAULT-ENTRY      PIC S9(04) COMP.                 
                   15  LURT-NUM-ENTRIES        PIC S9(04) COMP.                 
                   15  LURT-NUM-HITS           PIC S9(08) COMP.                 
               10  LURT-LAST-ACCESS.                                            
                   15  LURT-LAST-SEARCH-KEY    PIC X(64).                       
                   15  LURT-LAST-SEARCH-RESULT PIC X(64).                       
                   15  LURT-LAST-SEARCH-ENTRY  PIC S9(04) COMP.                 
                                                                                
       01  LOOKUP-SEARCH-INDEX.                                                 
           05  LUSI-COUNT                      PIC S9(04) COMP.                 
           05  LOOKUP-SEARCH-INDEX-ENTRY OCCURS 32760 TIMES.                    
               10  LUSI-SEARCH-KEY.                                             
                   15  LUSI-SEARCH-LU-KEY      PIC X(48).                       
                   15  LUSI-SEARCH-EFF-COMP    PIC 9(08).                       
                   15  LUSI-SEARCH-PROC-COMP   PIC 9(08).                       
               10  LUSI-SEARCH-RESULT-TERMDT   PIC 9(08).                       
               10  LUSI-SEARCH-RESULT-OFFSET   PIC S9(08) COMP.                 
               10  LUSI-SEARCH-RESULT-SIZE     PIC S9(04) COMP.                 
                                                                                
       01  LOOKUP-SEARCH-RESULTS.                                               
           05  LUSR-SIZE                       PIC S9(08) COMP.                 
           05  LUSR-AVAILABLE                  PIC S9(08) COMP.                 
           05  LUSR-CAPACITY                   PIC S9(08) COMP.                 
           05  LUSR-DATA-OFFSET                PIC S9(08) COMP.                 
           05  LUSR-DATA-BLOCK-COUNT           PIC S9(04) COMP.                 
           05  LUSR-DATA.                                                       
               10  LUSR-DATA-BLOCK             PIC X(8192)                      
                             OCCURS 1000 TIMES DEPENDING ON                     
                                               LUSR-DATA-BLOCK-COUNT.           
                                                                                
