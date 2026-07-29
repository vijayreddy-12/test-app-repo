       IDENTIFICATION DIVISION.                                                 
      *=======================*                                                 
       PROGRAM-ID.   GACCDDE7.                                                  
       AUTHOR.       LIVINGSTONE INBARAJ.                                       
       DATE-WRITTEN. FEB 2013.                                                  
      *****************************************************************         
      *****************************************************************         
      * THIS PROGRAM IS A PASS-THROUGH STUB CALLER OF                 *         
      * THE PROGRAM MANAGES INPUT-OUTPUT OPERATIONS                   *         
      * TO THE GIPSY DETAIL FILE.                                     *         
      *                                                               *         
      * THIS PROGRAM ALLOWS PROGRAMS THAT CALL THE OLD ASSEMBLER      *         
      * VERSION OF THIS PROGRAM TO USE THE NEW COBOL VERSIONS         *         
      * WITHOUT ANY CODING OR LINKAGE CHANGES.                        *         
      *                                                               *         
      * MAINTENANCE HISTORY                                           *         
      * -------------------                                           *         
      *                                                               *         
      * DATE       AUTHOR    DESCRIPTION                              *         
      * --------------------------------                              *         
      *****************************************************************         
      *****************************************************************         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  GACBDET7                  PIC X(8)  VALUE 'GACBDET7'.                
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  GACCDDE7-PARMS.                                                      
           03  GACCDDE7-FUNC         COMP   PIC S9(4)  VALUE +0.                
           03  GACCDDE7-SEARCH-KEY.                                             
               05  GACCDDE7-GROUP    COMP-3 PIC S9(7)  VALUE +0.                
               05  GACCDDE7-ACCT            PIC X(3)   VALUE LOW-VALUES.        
               05  GACCDDE7-CERT     COMP-3 PIC S9(9)  VALUE +0.                
           03  GACCDDE7-WORK                PIC X(9)   VALUE LOW-VALUES.        
                                                                                
       01  GACCDDE7-RETURN                  PIC X(3)   VALUE SPACES.            
                                                                                
       COPY GDETEXP7.                                                           
                                                                                
       PROCEDURE DIVISION USING GACCDDE7-PARMS                                  
                                GACCDDE7-RETURN                                 
                                GDETEXP7.                                       
                                                                                
           CALL GACBDET7 USING GACCDDE7-PARMS                                   
                               GACCDDE7-RETURN                                  
                               GDETEXP7.                                        
                                                                                
           GOBACK.                                                              
                                                                                
