      *01  DATA-SERVICE-VERBS.                                                  
      *****************************************************************         
      *  This copybook contains data server verbs.                    *         
      *                                                               *         
      *  30MAR93 - Alex Paraschuk - creation                          *         
      *          - this is a drastically modified version of DSVERBS  *         
      *            to be used with the new data server GAEDATSR       *         
      *          - COMMIT-DB has no effect with the PC data server    *         
      *            because each operation is automatically committed  *         
      *                                                               *         
      *****************************************************************         
           05  OBTAIN-FIRST    VALUE 'OBTAIN  FIRST   ' PIC X(16).              
           05  OBTAIN-NEXT     VALUE 'OBTAIN  NEXT    ' PIC X(16).              
           05  OBTAIN-PRIOR    VALUE 'OBTAIN  PRIOR   ' PIC X(16).              
           05  OBTAIN-KEYED    VALUE 'OBTAIN  KEYED   ' PIC X(16).              
           05  OBTAIN-GE       VALUE 'OBTAIN  GE      ' PIC X(16).              
           05  OBTAIN-LE       VALUE 'OBTAIN  LE      ' PIC X(16).              
           05  STORE-LR        VALUE 'STORE           ' PIC X(16).              
           05  MODIFY-LR       VALUE 'MODIFY          ' PIC X(16).              
           05  ERASE-LR        VALUE 'ERASE           ' PIC X(16).              
           05  FINISH-LR       VALUE 'FINISH          ' PIC X(16).              
           05  CLOSE-ALL       VALUE 'CLOSE   ALL     ' PIC X(16).              
           05  COMMIT-DB       VALUE 'COMMIT          ' PIC X(16).              
