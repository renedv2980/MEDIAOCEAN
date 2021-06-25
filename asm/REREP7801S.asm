*          DATA SET REREP7801S AT LEVEL 018 AS OF 03/27/02                      
*PHASE RE7801A,+0                                                               
         TITLE 'SPECS FOR SALESMAN LISTING'                                     
RE7801   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         FSPEC READ,SALESMEN                                                    
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,47,C'SALESPERSON LISTING'                                     
         ASPEC H2,47,19C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,REP                                                         
         ASPEC H4,88,REQUESTOR                                                  
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H7,02,C'INITIALS'                                                
         ASPEC H8,02,C'--------'                                                
         ASPEC H7,17,C'SALESPERSON NAME'                                        
         ASPEC H8,17,16C'-'                                                     
         ASPEC H7,39,C'TELEPHONE NO.'                                           
         ASPEC H8,39,C'-------------'                                           
         ASPEC H7,54,C'TEAM'                                                    
         ASPEC H8,54,C'----'                                                    
         ASPEC H7,60,C'DIVISION/TEAM NAME'                                      
         ASPEC H8,60,C'------------------'                                      
         ASPEC H7,84,C'OFF'                                                     
         ASPEC H8,84,C'---'                                                     
         ASPEC H7,89,C'FAX NUMBER'                                              
         ASPEC H8,89,C'----------'                                              
         ASPEC H7,102,C'LEAVE DATE'                                             
         ASPEC H8,102,C'----------'                                             
         ASPEC H7,114,C'MGR?'                                                   
         ASPEC H8,114,C'----'                                                   
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H7,2,C'SALESPERSON NAME'                                         
         ASPEC H8,2,16C'-'                                                      
         ASPEC H7,27,C'INITIALS'                                                
         ASPEC H8,27,8C'-'                                                      
         ASPEC H7,39,C'TELEPHONE NO.'                                           
         ASPEC H8,39,C'-------------'                                           
         ASPEC H7,54,C'TEAM'                                                    
         ASPEC H8,54,C'----'                                                    
         ASPEC H7,60,C'DIVISION/TEAM NAME'                                      
         ASPEC H8,60,C'------------------'                                      
         ASPEC H7,84,C'OFF'                                                     
         ASPEC H8,84,C'---'                                                     
         ASPEC H7,89,C'FAX NUMBER'                                              
         ASPEC H8,89,C'----------'                                              
         ASPEC H7,102,C'LEAVE DATE'                                             
         ASPEC H8,102,C'----------'                                             
         ASPEC H7,114,C'MGR?'                                                   
         ASPEC H8,114,C'----'                                                   
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H7,2,C'OFF'                                                      
         ASPEC H8,2,C'---'                                                      
         ASPEC H7,6,C'SALESPERSON NAME'                                         
         ASPEC H8,6,16C'-'                                                      
         ASPEC H7,31,C'INITIALS'                                                
         ASPEC H8,31,8C'-'                                                      
         ASPEC H7,43,C'TELEPHONE NO.'                                           
         ASPEC H8,43,C'-------------'                                           
         ASPEC H7,58,C'TEAM'                                                    
         ASPEC H8,58,C'----'                                                    
         ASPEC H7,64,C'DIVISION/TEAM NAME'                                      
         ASPEC H8,64,C'------------------'                                      
         ASPEC H7,89,C'FAX NUMBER'                                              
         ASPEC H8,89,C'----------'                                              
         ASPEC H7,102,C'LEAVE DATE'                                             
         ASPEC H8,102,C'----------'                                             
         ASPEC H7,114,C'MGR?'                                                   
         ASPEC H8,114,C'----'                                                   
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,43,C'SALESPERSON MERGER LISTING'                              
         ASPEC H2,43,26C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,REP                                                         
         ASPEC H4,88,REQUESTOR                                                  
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H7,02,C'INITIALS'                                                
         ASPEC H8,02,C'--------'                                                
         ASPEC H7,17,C'SALESPERSON NAME'                                        
         ASPEC H8,17,16C'-'                                                     
         ASPEC H7,39,C'TELEPHONE NO.'                                           
         ASPEC H8,39,C'-------------'                                           
         ASPEC H7,54,C'TEAM'                                                    
         ASPEC H8,54,C'----'                                                    
         ASPEC H7,60,C'DIVISION/TEAM NAME'                                      
         ASPEC H8,60,C'------------------'                                      
         ASPEC H7,84,C'OFF'                                                     
         ASPEC H8,84,C'---'                                                     
         ASPEC H7,89,C'FAX NUMBER'                                              
         ASPEC H8,89,C'----------'                                              
         ASPEC H7,105,C'MERGE INITIALS/NAME'                                    
         ASPEC H8,105,C'-------------------'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018REREP7801S03/27/02'                                      
         END                                                                    
