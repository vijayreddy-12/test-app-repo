      *=================================================================        
      *= COPYBOOK MLXXSETA                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= TRANSACTION PARSER.                                          =*        
      *=                                                              =*        
      *= CHARACTER SET SUBSTITUTION TABLE (A).                        =*        
      *= EBCDIC ACCENTED AND LOWERCASE CHARACTERS GET SUBSTITUTED.    =*        
      *=                                                              =*        
      *=================================================================        
      *#################################################################        
      *#                   MAINTENANCE LOG                            #*        
      *#                   ===============                            #*        
      *#  *PROJECT*     DATE (DD/MM/YY)    INITIALS                   #*        
      *#  ( BUG # )   - DESCRIPTION                                   #*        
      *#                                                              #*        
      *# PARSER         JUL/96            M. PRANGE                   #*        
      *# RE-WRITE     - CREATED                                       #*        
      *#                                                              #*        
      *#################################################################        
       01  SET-A-CHARS-IN                  PIC X(79)                            
           VALUE 'abcdefghijklmnopqrstuvwxyz·ÄÎêü‚´ã©™ú€•ô„®ûﬂ‹ö›ﬁòù¨àî∞        
      -          '±≤¸÷˚ì æËÏÌ≠ıÙ£èÖéÈ‰—˜˙ß'.                                   
                                                                                
       01  SET-A-CHARS-OUT                 PIC X(79)                            
           VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZAAAAAACNEEEEIIIIBAAAAAACNEEE        
      -          'EIIIIYOOOOOUUUUYOOOOOUUUU'.                                   
