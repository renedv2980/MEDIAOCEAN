*          DATA SET REREP7A01  AT LEVEL 025 AS OF 08/06/96                      
*PHASE RE7A01A,+0                                                               
         TITLE 'SPECS FOR DEVELOPEMENTAL SALESMAN LISTING'                      
RE7A01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,42,C'DEVELOPMENTAL SALESPERSON LISTING'                       
         ASPEC H2,42,33C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,REP                                                         
         ASPEC H4,88,REQUESTOR                                                  
         SPACE 1                                                                
         ASPEC H7,02,C'INITIALS'                                                
         ASPEC H8,02,C'--------'                                                
         ASPEC H7,17,C'SALESPERSON NAME'                                        
         ASPEC H8,17,16C'-'                                                     
         ASPEC H7,39,C'TELEPHONE NO.'                                           
         ASPEC H8,39,C'-------------'                                           
         ASPEC H7,54,C'FAX NUMBER'                                              
         ASPEC H8,54,C'----------'                                              
         ASPEC H7,70,C'LEAVE DATE'                                              
         ASPEC H8,70,C'----------'                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025REREP7A01 08/06/96'                                      
         END                                                                    
