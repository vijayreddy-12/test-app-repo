           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G A C C E T B L                         *        
      *                                                                *        
      *   1. THIS IS A TABLE OF GIPSY CODES, OCCUPATIONS AND CATEGORIES*        
      *      FOR THE E-ENROL FORM (ENGLISH)                            *        
      *                                                                *        
      *   2. IF ADDING A NEW GIPSY CODE OR OCCUPATION TO THIS TABLE    *        
      *      ADD 1 TO GACCETBL-MAX-ENTRIES                             *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G A C C E T B L                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 210901 - JE    NEW COPYBOOK                              *        
      *                                                                *        
      ******************************************************************        
      *01  GACCETBL-RECORD.                                                     
           05  GACCETBL-MAX-ENTRIES      PIC S9(4) COMP VALUE +100.             
           05  GACCETBL-TABLE.                                                  
               10  GACCETBL-LINE-1.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CEO, CIO, CFO'.                                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'MANAGEMENT OCCUPATIONS'.                          
               10  GACCETBL-LINE-2.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DIRECTOR'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'MANAGEMENT OCCUPATIONS'.                          
               10  GACCETBL-LINE-3.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'MANAGER'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'MANAGEMENT OCCUPATIONS'.                          
               10  GACCETBL-LINE-4.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRESIDENT'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'MANAGEMENT OCCUPATIONS'.                          
               10  GACCETBL-LINE-5.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'VICE PRESIDENT'.                                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'MANAGEMENT OCCUPATIONS'.                          
               10  GACCETBL-LINE-6.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ACTUARY'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-7.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ARCHITECT'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-8.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CHARTERED ACCOUNTANT'.                            
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-9.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CHEF'.                                            
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-10.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CHIROPRACTOR'.                                    
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-11.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CONSULTANT'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-12.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'COUNSELOR'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-13.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DENTAL HYGIENIST'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-14.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DENTIST'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-15.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ENGINEER'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-16.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'LAWYER/PARTNER'.                                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-17.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'NURSE (CLINIC/DOCTOR''S OFFICE'.                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-18.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'NURSE (OTHER)'.                                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-19.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OPTOMETRIST / OPHTHALMOLOGIST'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-20.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PHARMACIST'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-21.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PHYSICIAN'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-22.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PILOT / CO-PILOT'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-23.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRINCIPAL'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-24.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '07'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TEACHER'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-25.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'THERAPIST'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROFESSIONAL SPECIALITY OCCUP'.                   
               10  GACCETBL-LINE-26.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ADMINISTRATIVE POSITIONS (CLE'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-27.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '07'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ALL SALES'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-28.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ANALYST'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-29.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '07'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'BROKER'.                                          
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-30.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'BUYER'.                                           
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-31.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CASHIER'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-32.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CUSTOMER SERVICE REPRESENTATI'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-33.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DISPATCHER'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-34.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'LAB TECHNICIAN'.                                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-35.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OFFICE STAFF'.                                    
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-36.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROGRAMMER'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-37.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '07'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'REAL ESTATE'.                                     
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-38.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'SHIPPER / RECEIVER'.                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-39.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVEL AGENT'.                                    
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICAL, SALES AND ADMINIST'.                   
               10  GACCETBL-LINE-40.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'APPRENTICE'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-41.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CARPENTER'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-42.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CHILDCARE WORKER'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-43.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CONSTRUCTION WORKER'.                             
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-44.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'COOK'.                                            
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-45.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ELECTRICIAN'.                                     
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-46.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'FIRE / AMBULANCE'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-47.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'FLOORING INSTALLER'.                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-48.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'LANDSCAPER'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-49.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'MECHANIC'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-50.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PAINTER'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-51.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PLUMBER'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-52.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'POLICE'.                                          
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-53.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ROAD CONTRUCTION'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-54.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'SOCIAL WORKER'.                                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-55.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TOOL & DIE'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-56.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'WAITER / WAITRESS'.                               
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-57.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'WELDER'.                                          
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRADES AND SERVICE OCCUPATION'.                   
               10  GACCETBL-LINE-58.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ASSEMBLY LINE'.                                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-59.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'AUTO BODY'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-60.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DRIVER - TRANSPORT'.                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-61.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DRIVER - COURIER'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-62.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'FARM WORKER'.                                     
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-63.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'FOREMAN'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-64.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'GENERAL HELP'.                                    
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-65.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'HOUSEKEEPER'.                                     
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-66.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'JANITORIAL'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-67.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'LABOURER'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-68.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'MACHINE OPERATOR'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-69.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'MAINTENANCE.'.                                    
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-70.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OIL FIELD WORKER'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-71.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'WAREHOUSE AND STORAGE'.                           
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATORS, MANUFA'.                   
               10  GACCETBL-LINE-72.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'RETIREES'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'NOT CLASSIFIED BY AN OCCUPATI'.                   
               10  GACCETBL-LINE-73.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-74.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-75.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-76.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-77.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-78.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-79.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-80.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-81.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-82.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-83.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-84.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-85.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-86.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-87.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-88.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-89.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-90.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-91.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-92.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-93.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-94.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-95.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-96.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-97.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-98.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-99.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
               10  GACCETBL-LINE-100.                                           
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OCCUPATION'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'CATEGORY'.                                        
           05  FILLER                    REDEFINES GACCETBL-TABLE.              
               10  GACCETBL-ENTRY        OCCURS 100 TIMES.                      
                   15  GACCETBL-GIPSY-CODE   PIC X(2).                          
                   15  GACCETBL-OCC          PIC X(29).                         
                   15  GACCETBL-CAT          PIC X(29).                         
