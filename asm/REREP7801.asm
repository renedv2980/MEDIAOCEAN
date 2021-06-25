*          DATA SET REREP7801  AT LEVEL 029 AS OF 04/15/03                      
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
         ASPEC H7,02,C'CODE'                                                    
         ASPEC H8,02,C'REP   MINUS? '                                           
         ASPEC H9,02,C'-------------'                                           
         ASPEC H7,17,C'SALESPERSON NAME'                                        
         ASPEC H8,17,C'EMAIL ADDRESS     '                                      
         ASPEC H9,17,16C'-'                                                     
         ASPEC H7,34,C'EDI?'                                                    
         ASPEC H9,34,4C'-'                                                      
         ASPEC H7,39,C'TELEPHONE NO.'                                           
         ASPEC H9,39,C'-------------'                                           
         ASPEC H7,54,C'TEAM'                                                    
         ASPEC H9,54,C'----'                                                    
         ASPEC H7,60,C'DIVISION/TEAM NAME'                                      
         ASPEC H8,60,C'SALES ASSIST '                                           
         ASPEC H9,60,C'------------------'                                      
         ASPEC H7,84,C'OFF'                                                     
         ASPEC H9,84,C'---'                                                     
         ASPEC H7,89,C'LEAVE DATE'                                              
         ASPEC H8,89,C'SA-EMAIL-ADDRESS'                                        
         ASPEC H9,89,C'------------'                                            
         ASPEC H7,101,C'MGR?'                                                   
         ASPEC H9,101,C'----'                                                   
         ASPEC H7,106,C'FAX NUMBER'                                             
         ASPEC H9,106,C'----------'                                             
         ASPEC H7,120,C'FAX?'                                                   
         ASPEC H8,120,C'S/A?'                                                   
         ASPEC H9,120,C'----'                                                   
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H7,2,C'SALESPERSON NAME'                                         
         ASPEC H8,2,C'EMAIL ADDRESS     '                                       
         ASPEC H9,2,16C'-'                                                      
         ASPEC H7,27,C'CODE'                                                    
         ASPEC H9,27,4C'-'                                                      
         ASPEC H7,34,C'EDI?'                                                    
         ASPEC H9,34,4C'-'                                                      
         ASPEC H7,39,C'TELEPHONE NO.'                                           
         ASPEC H8,39,C'REP   MINUS? '                                           
         ASPEC H9,39,C'-------------'                                           
         ASPEC H7,54,C'TEAM'                                                    
         ASPEC H9,54,C'----'                                                    
         ASPEC H7,60,C'DIVISION/TEAM NAME'                                      
         ASPEC H8,60,C'SALES ASSIST '                                           
         ASPEC H9,60,C'------------------'                                      
         ASPEC H7,84,C'OFF'                                                     
         ASPEC H9,84,C'---'                                                     
         ASPEC H7,89,C'LEAVE DATE'                                              
         ASPEC H8,89,C'SA-EMAIL-ADDRESS'                                        
         ASPEC H9,89,C'------------'                                            
         ASPEC H7,101,C'MGR?'                                                   
         ASPEC H9,101,C'----'                                                   
         ASPEC H7,106,C'FAX NUMBER'                                             
         ASPEC H9,106,C'----------'                                             
         ASPEC H7,120,C'FAX?'                                                   
         ASPEC H8,120,C'S/A?'                                                   
         ASPEC H9,120,C'----'                                                   
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H7,2,C'OFF'                                                      
         ASPEC H9,2,C'---'                                                      
         ASPEC H7,6,C'SALESPERSON NAME'                                         
         ASPEC H8,6,C'EMAIL ADDRESS     '                                       
         ASPEC H9,6,16C'-'                                                      
         ASPEC H7,31,C'CODE'                                                    
         ASPEC H9,31,4C'-'                                                      
         ASPEC H7,38,C'EDI?'                                                    
         ASPEC H9,38,4C'-'                                                      
         ASPEC H7,43,C'TELEPHONE NO.'                                           
         ASPEC H8,43,C'REP   MINUS? '                                           
         ASPEC H9,43,C'-------------'                                           
         ASPEC H7,58,C'TEAM'                                                    
         ASPEC H9,58,C'----'                                                    
         ASPEC H7,64,C'DIVISION/TEAM NAME'                                      
         ASPEC H8,64,C'SALES ASSIST '                                           
         ASPEC H9,64,C'------------------'                                      
         ASPEC H7,89,C'LEAVE DATE'                                              
         ASPEC H8,89,C'SA-EMAIL-ADDRESS'                                        
         ASPEC H9,89,C'----------'                                              
         ASPEC H7,101,C'MGR?'                                                   
         ASPEC H9,101,C'----'                                                   
         ASPEC H7,106,C'FAX NUMBER'                                             
         ASPEC H9,106,C'----------'                                             
         ASPEC H7,120,C'FAX?'                                                   
         ASPEC H8,120,C'S/A?'                                                   
         ASPEC H9,120,C'----'                                                   
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
         ASPEC H7,02,C'CODE'                                                    
         ASPEC H9,02,C'----'                                                    
         ASPEC H7,17,C'SALESPERSON NAME'                                        
         ASPEC H9,17,16C'-'                                                     
         ASPEC H7,39,C'TELEPHONE NO.'                                           
         ASPEC H9,39,C'-------------'                                           
         ASPEC H7,54,C'TEAM'                                                    
         ASPEC H9,54,C'----'                                                    
         ASPEC H7,60,C'DIVISION/TEAM NAME'                                      
         ASPEC H8,60,C'EMAIL ADDRESS     '                                      
         ASPEC H9,60,C'------------------'                                      
         ASPEC H7,84,C'OFF'                                                     
         ASPEC H9,84,C'---'                                                     
         ASPEC H7,89,C'FAX NUMBER'                                              
         ASPEC H9,89,C'----------'                                              
         ASPEC H7,105,C'MERGE INITIALS/NAME'                                    
         ASPEC H9,105,C'-------------------'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029REREP7801 04/15/03'                                      
         END                                                                    
