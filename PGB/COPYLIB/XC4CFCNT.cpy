      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG:         X C 4 C F C N T                           *        
      * *************                                                  *        
      *                                                                *        
      *  NO    DATE    PGR                DESCRIPTION                  *        
      *  --   ------   ---  -----------------------------------------  *        
      *                                                                *        
      *  00 - 020514 - FSM  CREATED FOR CONTENT SERVICE DATA STORE     *        
      *                                                                *        
      ******************************************************************        
      * 01 CONTENT-RECORD.                                                      
           05 CONTENT-KEY.                                                      
              10 CONTENT-LOOKUP-KEY.                                            
                 15 CONTENT-KEY-NAME              PIC  X(08).                   
                 15 CONTENT-KEY-DATA              PIC  X(40).                   
              10 CONTENT-KEY-EFF-DATE-COMP        PIC  9(08).                   
              10 CONTENT-KEY-PROC-DATE-COMP       PIC  9(08).                   
           05 CONTENT-AUDIT-INFO.                                               
              10 CONTENT-CHANGE-USERID            PIC  X(08).                   
              10 CONTENT-CHANGE-DATE              PIC  9(08).                   
              10 CONTENT-EFF-DATE                 PIC  9(08).                   
              10 CONTENT-TERM-DATE                PIC  9(08).                   
              10 CONTENT-STATE                    PIC  X(01).                   
                 88 CONTENT-DRAFT                 VALUE 'D'.                    
                 88 CONTENT-DRAFTED               VALUE 'T'.                    
                 88 CONTENT-APPROVED              VALUE 'A'.                    
                 88 CONTENT-PUBLISHED             VALUE 'P'.                    
                 88 CONTENT-HISTORY               VALUE 'H'.                    
              10 CONTENT-FILLER                   PIC  X(03).                   
           05 CONTENT-DATA                        PIC  X(15000).                
