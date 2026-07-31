           03 INTERFACE-ERROR-MESSAGE.                                          
              05 IE-ERROR-LINE-1.                                               
                 10 FILLER                     PIC X(22) VALUE                  
                             '*** PROGRAM:'.                                    
                 10 IE-PROGRAM-NAME            PIC X(08).                       
                 10 FILLER                     PIC X(03) VALUE ' - '.           
                 10 IE-ABEND-MSG               PIC X(22).                       
                 10 FILLER                     PIC X(28) VALUE SPACES.          
              05 IE-ERROR-LINE-2.                                               
                 10 FILLER                     PIC X(22) VALUE                  
                             '*** ERROR MESSAGE:'.                              
                 10 IE-ERROR-MESSAGE           PIC X(58).                       
              05 IE-ERROR-LINE-3.                                               
                 10 FILLER                     PIC X(22) VALUE                  
                             '*** ACTION:'.                                     
                 10 IE-ACTION                  PIC X(16).                       
                 10 FILLER                     PIC X(42) VALUE SPACES.          
              05 IE-ERROR-LINE-4.                                               
                 10 FILLER                     PIC X(22) VALUE                  
                             '*** ERROR OCCURRED:'.                             
                 10 IE-ABEND-DATE.                                              
                    15  IE-ABEND-DATE-YY       PIC 9(02).                       
                    15  FILLER                 PIC X(01) VALUE '-'.             
                    15  IE-ABEND-DATE-MM       PIC 9(02).                       
                    15  FILLER                 PIC X(01) VALUE '-'.             
                    15  IE-ABEND-DATE-DD       PIC 9(02).                       
                 10 FILLER                     PIC X(01) VALUE ' '.             
                 10 IE-ABEND-TIME.                                              
                    15  IE-ABEND-TIME-HH       PIC 9(02).                       
                    15  FILLER                 PIC X(01) VALUE ':'.             
                    15  IE-ABEND-TIME-MM       PIC 9(02).                       
                    15  FILLER                 PIC X(01) VALUE ':'.             
                    15  IE-ABEND-TIME-SS       PIC 9(02).                       
                 10 FILLER                     PIC X(40) VALUE SPACES.          
              05 IE-ERROR-LINE-5.                                               
                 10 FILLER                     PIC X(22) VALUE                  
                             '*** FILE/TABLE NAME:'.                            
                 10 IE-LR-NAME                 PIC X(32).                       
                 10 FILLER                     PIC X(26) VALUE SPACES.          
              05 IE-ERROR-LINE-6.                                               
                 10 FILLER                     PIC X(22) VALUE                  
                             '*** FILE STATUS:'.                                
                 10 IE-LR-STATUS               PIC X(08).                       
                 10 FILLER                     PIC X(56) VALUE SPACES.          
              05 IE-ERROR-LINE-7.                                               
                 10 FILLER                     PIC X(22) VALUE                  
                             '*** SQL CODE:'.                                   
                 10 IE-SQL-CODE                PIC Z(08)9-.                     
                 10 FILLER                     PIC X(47) VALUE SPACES.          
              05 IE-ERROR-LINE-8.                                               
                 10 FILLER                     PIC X(22) VALUE                  
                             '*** GROUP INFO:'.                                 
                 10 FILLER                     PIC X(28) VALUE                  
                             '(GROUP/ACCOUNT/CERTIFICATE)'.                     
                 10 IE-GROUP                   PIC 9(07).                       
                 10 FILLER                     PIC X(01) VALUE '/'.             
                 10 IE-AC                      PIC X(03).                       
                 10 FILLER                     PIC X(01) VALUE '/'.             
                 10 IE-CERTIFICATE             PIC Z(09).                       
                 10 FILLER                     PIC X(09).                       
              05  IE-ABEND-MESSAGES        PIC X(22).                           
                 88 IE-WARNING-ERROR-MSG    VALUE                               
                                            'NON FATAL (WARNING)   '.           
                 88 IE-FATAL-ERROR-MSG      VALUE                               
                                            'FATAL(ABNORMAL END)   '.           
              05 IE-MESSAGES.                                                   
                 10 IE-MSG-LENGTH     COMP-3   PIC S9(3) VALUE +121.            
                 10 IE-MSG-CC                  PIC X(01) VALUE ' '.             
                 10 IE-MSG-CONTENT             PIC X(120).                      
              05 IE-WTL-FLAG                   PIC X(01) VALUE 'B'.             
              05 IE-REPLY                      PIC X(03) VALUE 'NO '.           
              05 IE-PARM                       PIC X(01).                       
