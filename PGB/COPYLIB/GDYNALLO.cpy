      *                                                                         
      * PARAMETERS FOR CALLING GYNALLO                                          
      *     PARM1                                                               
      *     PARM2                                                               
      *                                                                         
      *                                                                         
       01  GDYNALLO-PARM1.                                                      
      *         A - ALLOCATE  U -UNALLOCATE                                     
           05 GDYN-ACTION                             PIC X.                    
      *         DDNAME                                                          
           05 GDYN-DDNAME                             PIC X(8).                 
      *         DATA SET NAME                                                   
           05 GDYN-DSN                                PIC X(44).                
      *         MEMBER NAME IF A PDS                                            
           05 GDYN-MEMBER                             PIC X(8).                 
      *         DATA SET STATUS, EG SHR                                         
           05 GDYN-STATUS                             PIC X(3).                 
      *         NORMAL DISPOSITION - EG CATLG                                   
           05 GDYN-NORMAL-DISP                        PIC X(7).                 
      *         CONDITIONAL DISPOSITION - EG DELETE                             
           05 GDYN-COND-DISP                          PIC X(7).                 
      *         SPACE TYPE  T - TRACKS, C - CYLINDERS                           
           05 GDYN-SPACE-TYPE                         PIC X.                    
      *         NON-BLANK TO RELEASE UNUSED SPACE                               
           05 GDYN-RLSE                               PIC X.                    
      *         NON-BLANK TO UNALLOCATE RESOURCE AT CLOSE                       
           05 GDYN-UNCLOSE                            PIC X.                    
      *         PRIMARY SPACE ALLOCATION                                        
           05 GDYN-PRIMARY-SPACE                      PIC S9(5) COMP-3.         
      *         SECONDARY SPACE ALLOCATION                                      
           05 GDYN-SECONDARY-SPACE                    PIC S9(5) COMP-3.         
      *         DIRECTORY ALLOCATION IN BLOCKS                                  
           05 GDYN-ALLOCATION                         PIC S9(5) COMP-3.         
      *         UNIT NAME                                                       
           05 GDYN-UNIT-NAME                          PIC X(6).                 
      *         BLOCK SIZE                                                      
           05 GDYN-BLOCK-SIZE                         PIC S9(5) COMP-3.         
      *         RECORD LENGTH                                                   
           05 GDYN-RECORD-LENGTH                      PIC S9(5) COMP-3.         
      *         RECORD FORMAT                                                   
           05 GDYN-RECORD-FORMAT                      PIC X(3).                 
      *         DATA SET ORGANIZATION                                           
           05 GDYN-DATA-SET-ORG                       PIC X(2).                 
      *         SYSOUT CLASS                                                    
           05 GDYN-SYSOUT-CLASS                       PIC X.                    
      *         SYSOUT FORM NUMBER                                              
           05 GDYN-SYSOUT-FORM                        PIC X(4).                 
      *         SYSOUT LIMIT                                                    
           05 GDYN-SYSOUT-LIMIT                       PIC S9(7) COMP-3.         
      *         SYSOUT NUMBER OF COPIES                                         
           05 GDYN-SYSOUT-COPIES                      PIC S9(3) COMP-3.         
      *         NON-BLANK TO HOLD SYSOUT DATA SET                               
           05 GDYN-SYSOUT-HOLD                        PIC X.                    
      *         FORMS CONTROL BUFFER                                            
           05 GDYN-FORMS-CONTROL-BUFFER               PIC X(4).                 
      *         REMOTE WORK STATION                                             
           05 GDYN-REMOTE-WORK-STATION                PIC X(8).                 
      *         USERID                                                          
           05 GDYN-USERID                             PIC X(8).                 
      *         DUMMY FILE INDICATED BY WORD DUMMY                              
           05 GDYN-DUMMY                              PIC X(5).                 
      *         FOR FUTURE EXPANSION IF NECESSARY                               
           05 GDYN-FILLER                             PIC X(95).                
      *                                                                         
       01  GDYNALLO-PARM2.                                                      
      *         RETURN CODE, VALUE OF R15, 0 MEANS OK                           
           05 GDYN-RETURN-CODE               COMP-3   PIC S9(15).               
