      *=================================================================        
      *= COPYBOOK MLXXSETB                                            =*        
      *=                                                              =*        
      *=--------------------------------------------------------------=*        
      *=                                                              =*        
      *= TRANSACTION PARSER.                                          =*        
      *=                                                              =*        
      *= CHARACTER SET SUBSTITUTION TABLE (B).                        =*        
      *= EBCDIC ACCENTED CHARACTERS GET SUBSTITUTED. LOWERCASE CHARS  =*        
      *= ARE STILL ALLOWED.                                           =*        
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
       01  SET-B-CHARS-IN                  PIC X(79)                            
           VALUE 'abcdefghijklmnopqrstuvwxyz·ÄÎêü‚´ã©™ú€•ô„®ûﬂ‹ö›ﬁòù¨àî∞        
      -          '±≤¸÷˚ì æËÏÌ≠ıÙ£èÖéÈ‰—˜˙ß'.                                   
                                                                                
       01  SET-B-CHARS-OUT                 PIC X(79)                            
           VALUE 'abcdefghijklmnopqrstuvwxyzAAAAAACNEEEEIIIIBAAAAAACNEEE        
      -          'EIIIIYOOOOOUUUUYOOOOOUUUU'.                                   
