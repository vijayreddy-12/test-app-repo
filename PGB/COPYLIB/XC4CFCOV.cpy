           SKIP1                                                                
      *****************************************************************         
      *                                                               *         
      *  CHANGE LOG:         X C 4 C F C O V                          *         
      * *************                                                 *         
      *                                                               *         
      *  NO    DATE    PGR                DESCRIPTION                 *         
      *  --   ------   ---  ----------------------------------------  *         
      *                                                               *         
      *  00 - 950717 - AK   ORIGNAL FOR CLIENT II CONVERSION          *         
      *  01 - 970116 - RC   FIELDS ADDED FOR MANUSCRIPT               *         
      *  02 - 970729 - CP   ADDED 'ALPHA' LEVEL ABOVE THE COMP-3      *         
      *                     FIELDS (ELIGIBILITY PROJECT)              *         
      *                                                               *         
      *** CHGLOG START - 00 - YYMMDD - AAA  ***************************         
      *** CHGLOG END   - 00 - YYMMDD - AAA  ***************************         
           SKIP1                                                                
      *01  COV-COVERAGE-REC.                                                    
           05 COV-EMPLOYEE-KEY.                                                 
              10 COV-FAMILY-NUMBER-X.                                           
                 15 COV-FAMILY-NUMBER    PIC S9(09)   COMP-3.                   
              10 COV-BEN-CODE            PIC  X(02).                            
              10 COV-KEY-DATE-X.                                                
                 15 COV-KEY-DATE         PIC S9(07)   COMP-3.                   
           05 COV-COV-EFF-DATE-X.                                               
              10 COV-COV-EFF-DATE        PIC S9(07)   COMP-3.                   
           05 COV-COV-END-DATE-X.                                               
              10 COV-COV-END-DATE        PIC S9(07)   COMP-3.                   
           05 COV-DATE-UPDATE-X.                                                
              10 COV-DATE-UPDATE         PIC S9(07)   COMP-3.                   
           05 COV-CONTRACT-X.                                                   
              10 COV-CONTRACT            PIC S9(07)   COMP-3.                   
           05 COV-DEP-TYPE               PIC  X(01).                            
              88 SINGLE-COVERAGE         VALUE 'S'.                             
              88 FAMILY-COVERAGE         VALUE 'F'.                             
              88 COUPLE-COVERAGE         VALUE 'C'.                             
           05 COV-CLASS                  PIC X(03).                             
           05 COV-MAILING-DIVISION       PIC X(03).                             
           05 COV-ANALYSIS-GROUPING-X.                                          
              10 COV-ANALYSIS-GROUPING   PIC S9(03)   COMP-3.                   
           05 COV-DR-DUR                 PIC X(01).                             
              88 COV-DUR-GROUP-RULE      VALUE ' '.                             
              88 COV-DUR-NOT-REQUIRED    VALUE 'N'.                             
           05 COV-COV-CODE               PIC X(02).                             
           05 COV-COB-COVERAGE           PIC X(01).                             
              88 COV-COB-UNKNOWN         VALUE ' '.                             
              88 COV-COB-NONE            VALUE 'N'.                             
              88 COV-COB-PRIMARY         VALUE 'P'.                             
              88 COV-COB-SECONDARY       VALUE 'S'.                             
