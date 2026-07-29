       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. GC2DATE.                                                     
      *-------------------------------------------------------------*           
      * THIS PROGRAM IS EQUIVALENT TO SEVERAL OTHER PROGRAMS                    
      * ANY CHANGES MADE TO THIS PROGRAM SHOULD ALSO BE MADE TO:                
      *                                                                         
      *     GVSDATE - GLHNEW ENDEVOR                                            
      *     GC2DATE - GLHSYS LAN - STDSERV                                      
      *     GACDATE - GLHSYS LAN - STDSERV                                      
      *                                                                         
      *-------------------------------------------------------------*           
      *   MODIFICATION LOG                                                      
      *  ---------------------                                                  
      *  15JUL08  IBM GR   UPGRADED IN ECU PROJECT                              
      *                                                                         
      ***************************************************************           
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
           EJECT                                                                
       WORKING-STORAGE SECTION.                                                 
       01  GC2TODAY                         PIC X(8) VALUE 'GC2TODAY'.          
       01  GC2DATEA                         PIC X(8) VALUE 'GC2DATEA'.          
      *01  ITERATION                        PIC S9(5) COMP VALUE 0.             
       01  CONTROL-TABLE.                                                       
           03 CT-VERSION1-5                 PIC XXX  VALUE '1.5'.               
           03 CT-PROGRAM1-5                 PIC X(8) VALUE 'GC2DATEA'.          
           03 CT-VERSION2-0                 PIC XXX  VALUE '2.0'.               
           03 CT-PROGRAM2-0                 PIC X(8) VALUE 'GC2DAT20'.          
       01  FILLER REDEFINES CONTROL-TABLE.                                      
           03  FILLER OCCURS 2.                                                 
               05 CT-VERSION                PIC XXX.                            
               05 CT-PROGRAM                PIC X(8).                           
       01  MISC.                                                                
           03 MAX-ENTRIES COMP              PIC 99   VALUE 2.                   
           03 I           COMP              PIC 99   VALUE ZERO.                
           03 DYNAMIC-PROGRAM               PIC X(8) VALUE SPACES.              
       01  TO-DAY.                                                              
           03 TO-DAY-SYSTEM-DATE.                                               
               05 TO-DAY-YEAR        PIC 9(4) VALUE ZERO.                       
               05 TO-DAY-MONTH       PIC 99   VALUE ZERO.                       
               05 TO-DAY-DAY         PIC 99   VALUE ZERO.                       
           03 TO-DAY-CICS-DATE REDEFINES TO-DAY-SYSTEM-DATE                     
                                     PIC S9(7).                                 
           03  FILLER REDEFINES TO-DAY-SYSTEM-DATE.                             
               05 TO-DAY-CICS-YEAR   PIC 9(4).                                  
               05 TO-DAY-CICS-JULIAN-DAY     PIC S999.                          
               05 TO-DAY-CICS-NULL   PIC X.                                     
       LINKAGE SECTION.                                                         
       01  VAPS-VDATE-PARAMETERS.                                               
           COPY GARDATEP.                                                       
           EJECT                                                                
       PROCEDURE DIVISION USING VAPS-VDATE-PARAMETERS.                          
       0000-MAINLINE.                                                           
      *    ADD +1 TO ITERATION                                                  
           IF TO-DAY = '00000000'                                               
               CALL GC2TODAY USING TO-DAY.                                      
      *    MOVE CT-PROGRAM1-5 TO DYNAMIC-PROGRAM.                               
           CALL GC2DATEA        USING VAPS-VDATE-PARAMETERS TO-DAY.             
           GOBACK.                                                              
