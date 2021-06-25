*          DATA SET SPREPC101  AT LEVEL 023 AS OF 10/05/93                      
*PHASE SPC101A                                                                  
         TITLE 'SPC101 - STATION FILE LISTING - PRINT SPECS'                    
SPC101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         SPACE 1                                                                
         SPROG 1,2,3                                                            
         SSPEC H1,57,C'CABLE FILE LISTING'                                      
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,57,C'------------------'                                      
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,3,PAGE                                                        
         SSPEC H3,100,REPORT                                                    
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H7,1,C'MKT'                                                      
         SSPEC H8,1,C'---'                                                      
         SSPEC H7,6,C'MARKET NAME'                                              
         SSPEC H8,6,C'-----------'                                              
         SSPEC H7,31,C'CITY'                                                    
         SSPEC H8,31,C'----'                                                    
         SSPEC H7,56,C'ST'                                                      
         SSPEC H8,56,C'--'                                                      
         SSPEC H7,59,C'STAT'                                                    
         SSPEC H8,59,C'----'                                                    
         SSPEC H7,64,C'CLT'                                                     
         SSPEC H8,64,C'---'                                                     
         SSPEC H7,68,C'REP'                                                     
         SSPEC H8,68,C'---'                                                     
         SSPEC H7,72,C'CABLE SYSTEM NAME'                                       
         SSPEC H8,72,C'-----------------'                                       
         SSPEC H7,97,C'NETWORKS'                                                
         SSPEC H8,97,C'--------'                                                
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H7,1,C'CITY'                                                     
         SSPEC H8,1,C'----'                                                     
         SSPEC H7,26,C'STATE'                                                   
         SSPEC H8,26,C'-----'                                                   
         SSPEC H7,32,C'STATION'                                                 
         SSPEC H8,32,C'-------'                                                 
         SSPEC H7,40,C'CLT'                                                     
         SSPEC H8,40,C'---'                                                     
         SSPEC H7,44,C'REP'                                                     
         SSPEC H8,44,C'---'                                                     
         SSPEC H7,48,C'CABLE SYSTEM NAME'                                       
         SSPEC H8,48,C'-----------------'                                       
         SSPEC H7,73,C'NETWORKS'                                                
         SSPEC H8,73,C'--------'                                                
         SPROG 3                                                                
         SSPEC H7,1,C'STAT'                                                     
         SSPEC H8,1,C'----'                                                     
         SSPEC H7,6,C'CLT'                                                      
         SSPEC H8,6,C'---'                                                      
         SSPEC H7,10,C'REP'                                                     
         SSPEC H8,10,C'---'                                                     
         SSPEC H7,14,C'CABLE SYSTEM NAME'                                       
         SSPEC H8,14,C'-----------------'                                       
         SSPEC H7,39,C'MKT'                                                     
         SSPEC H8,39,C'---'                                                     
         SSPEC H7,44,C'MARKET NAME'                                             
         SSPEC H8,44,C'-----------'                                             
         SSPEC H7,69,C'CITY'                                                    
         SSPEC H8,69,C'----'                                                    
         SSPEC H7,94,C'ST'                                                      
         SSPEC H8,94,C'--'                                                      
         SSPEC H7,98,C'NETWORKS'                                                
         SSPEC H8,98,C'--------'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPREPC101 10/05/93'                                      
         END                                                                    
