       IDENTIFICATION DIVISION.                                                 
      *=======================*                                                 
       PROGRAM-ID.   GACCDMA7.                                                  
       AUTHOR.       AL TAYLOR.                                                 
       DATE-WRITTEN. FEB 2013.                                                  
      *****************************************************************         
      *****************************************************************         
      * THIS PROGRAM IS A PASS-THROUGH STUB CALLER OF                 *         
      * THE PROGRAM MANAGES INPUT-OUTPUT OPERATIONS                   *         
      * TO THE GIPSY MASTER FILE.                                     *         
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
                                                                                
       01  GACBMAS7                  PIC X(8)  VALUE 'GACBMAS7'.                
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  GACCDMA7-PARMS.                                                      
           03  GACCDMA7-FUNC         COMP   PIC S9(4).                          
           03  GACCDDE7-SEARCH-KEY.                                             
               05  GACCDMA7-GROUP    COMP-3 PIC S9(7).                          
               05  GACCDMA7-ACCT            PIC X(3).                           
               05  FILLER            COMP-3 PIC S9(9).                          
           03  GACCDMA7-WORK                PIC X(9).                           
                                                                                
       01  GACCDMA7-RETURN                  PIC X(3).                           
                                                                                
       COPY GMASTER7.                                                           
                                                                                
       PROCEDURE DIVISION USING GACCDMA7-PARMS                                  
                                GACCDMA7-RETURN                                 
                                GMASTER7.                                       
                                                                                
           CALL GACBMAS7 USING GACCDMA7-PARMS                                   
                                  GACCDMA7-RETURN                               
                                  GMASTER7.                                     
                                                                                
           GOBACK.                                                              
                                                                                
