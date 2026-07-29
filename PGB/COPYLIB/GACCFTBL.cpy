           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *                        G A C C F T B L                         *        
      *                                                                *        
      *   1. THIS IS A TABLE OF GIPSY CODES, OCCUPATIONS AND CATEGORIES*        
      *      FOR THE E-ENROL FORM (FRENCH)                             *        
      *                                                                *        
      *   2. IF ADDING A NEW GIPSY CODE OR OCCUPATION TO THIS TABLE    *        
      *      ADD 1 TO GACCFTBL-MAX-ENTRIES                             *        
      *                                                                *        
      ******************************************************************        
           SKIP1                                                                
      ******************************************************************        
      *                                                                *        
      *  CHANGE LOG            G A C C F T B L                         *        
      *  **********                                                    *        
      *                                                                *        
      *  NO   DATE     PGR   DESCRIPTION                               *        
      *  --   ------   ---   ----------------------------------------  *        
      *                                                                *        
      *  00 - 210901 - JE    NEW COPYBOOK                              *        
      *                                                                *        
      ******************************************************************        
      *01  GACCFTBL-RECORD.                                                     
           05  GACCFTBL-MAX-ENTRIES      PIC S9(4) COMP VALUE +100.             
           05  GACCFTBL-TABLE.                                                  
               10  GACCFTBL-LINE-1.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CHEF DE LA DIRECTION, CHEF DE'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'GESTION'.                                         
               10  GACCFTBL-LINE-2.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DIRECTEUR'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'GESTION'.                                         
               10  GACCFTBL-LINE-3.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DIRECTEUR PRINCIPAL'.                             
                   15  FILLER            PIC X(29)                              
                       VALUE 'GESTION'.                                         
               10  GACCFTBL-LINE-4.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRESIDENTS'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'GESTION'.                                         
               10  GACCFTBL-LINE-5.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '01'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'VICE-PRESIDENT'.                                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'GESTION'.                                         
               10  GACCFTBL-LINE-6.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ACTUAIRE'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-7.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ARCHITECTE'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-8.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'AVOCAT/ASSOCIE'.                                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-9.                                             
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CHEF CUISINIER'.                                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-10.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CHIROPRACTICIEN'.                                 
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-11.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'COMPATABLE AGREE'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-12.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CONSEILLER'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-13.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CONSULTANT'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-14.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DENTISTE'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-15.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'DIRECTEUR D ECOLE'.                               
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-16.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '07'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ENSEIGNANT'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-17.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'HYGIENISTE DENTAIRE'.                             
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-18.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'INFIRMIERE (CLINIQUE/CABINET '.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-19.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'INFIRMIERE (AUTRE)'.                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-20.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'INGENIEUR'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-21.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'MEDICIN'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-22.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OPTOMETRISTE/OPHTALMOLOGISTE'.                    
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-23.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '06'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PHARMACIEN'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-24.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PILOTE/COPILOTE'.                                 
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-25.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'THERAPEUTE'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'EMPLOIS DE PROFESSIONNEL'.                        
               10  GACCFTBL-LINE-26.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ACHETEUR'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-27.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'AGENT DE VOYAGES'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-28.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '07'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'AGENT IMMOBILIER'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-29.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ANALYSTE'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-30.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CAISSIER'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-31.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '07'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'COURTIER'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-32.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'EXPEDIEUR/RECEPTIONNAIRE'.                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-33.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PERSONNEL ADMINISTRATIF (COMM'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-34.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PERSONNEL DE BEUEAU'.                             
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-35.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '07'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PERSONNEL DE VENTE'.                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-36.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '02'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PROGRAMMEUR'.                                     
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-37.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'REPARTITEUR'.                                     
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-38.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'REPRESENTANT DU SERVICE A LA '.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-39.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TECHNICIEN DE LABORATOIRE'.                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAIL TECHNIQUE, VENTE ET S'.                   
               10  GACCFTBL-LINE-40.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'APPRENTI'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-41.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CHARPENTIER'.                                     
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-42.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CUISINIER'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-43.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'EDUCATEUR DE LA PETITE ENFANC'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-44.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'ELECTRICIEN'.                                     
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-45.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'MECANICIEN'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-46.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OUTILLEUR-AJUSTEUR'.                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-47.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PAYSAGISTE'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-48.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PEINTRE EN BATIMENT'.                             
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-49.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PLOMBIER'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-50.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'POLICIER'.                                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-51.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'POMPIER/AMBULANCIER'.                             
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-52.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'POSEUR DE REVETEMENTS D''INTER'.                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-53.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'SERVEUR/SERVEUSE'.                                
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-54.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'SOUDEUR'.                                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-55.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAILLEUR DE LA CONSTRUCTIO'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-56.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAILLEUR DE LA CONSTRUCTIO'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-57.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '08'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAILLEUR SOCIAL'.                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'METIERS ET SERVICES'.                             
               10  GACCFTBL-LINE-58.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'AIDE DE SOUTIEN A DOMICILE'.                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-59.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'AIDE GENERAL'.                                    
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-60.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CONCIERGE'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-61.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CONDUCTEUR - MESSAGERIES'.                        
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-62.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CONDUCTEUR - TRANSPORT'.                          
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-63.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'CONTREMAITRE'.                                    
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-64.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'MANOEUVRES'.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-65.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'OPERATEUR DE MACHINE'.                            
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-66.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'PERSONNEL D''ENTRETIEN'.                          
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-67.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'REPARATEUR DE CARROSSERIE'.                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-68.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAILLEUR AGRICOLE'.                            
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-69.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '04'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAILLEUR A LA CHAINE'.                         
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-70.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAILLEUR A L''ENTREPOSAGE E'.                  
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-71.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '05'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'TRAVAILLEUR DANS LES CHAMPS D'.                   
                   15  FILLER            PIC X(29)                              
                       VALUE 'PRODUCTION, OPERATIONS TECHNI'.                   
               10  GACCFTBL-LINE-72.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '03'.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE 'RETRAITES'.                                       
                   15  FILLER            PIC X(29)                              
                       VALUE 'NON CLASSE PAR PROFESSION (RE'.                   
               10  GACCFTBL-LINE-73.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-74.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-75.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-76.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-77.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-78.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-79.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-80.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-81.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-82.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-83.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-84.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-85.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-86.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-87.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-88.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-89.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-90.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-91.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-92.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-93.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-94.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-95.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-96.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-97.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-98.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-99.                                            
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
               10  GACCFTBL-LINE-100.                                           
                   15  FILLER            PIC X(2)                               
                       VALUE '  '.                                              
                   15  FILLER            PIC X(29)                              
                       VALUE '          '.                                      
                   15  FILLER            PIC X(29)                              
                       VALUE '      '.                                          
           05  FILLER                    REDEFINES GACCFTBL-TABLE.              
               10  GACCFTBL-ENTRY        OCCURS 100 TIMES.                      
                   15  GACCFTBL-GIPSY-CODE   PIC X(2).                          
                   15  GACCFTBL-OCC          PIC X(29).                         
                   15  GACCFTBL-CAT          PIC X(29).                         
