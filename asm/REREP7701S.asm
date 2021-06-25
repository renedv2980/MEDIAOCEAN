*          DATA SET REREP7701S AT LEVEL 005 AS OF 03/31/95                      
*PHASE RE7701A,+0                                                               
         TITLE 'SPECS FOR POINT PERSON LISTING'                                 
RE7701   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,47,C'POINT PERSON LISTING'                                    
         ASPEC H2,47,19C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,C'REPRESENTATIVE'                                           
         ASPEC H4,88,REQUESTOR                                                  
         ASPEC H7,02,C'CODE'                                                    
         ASPEC H8,02,C'----'                                                    
         ASPEC H7,09,C'NAME'                                                    
         ASPEC H8,09,C'----'                                                    
         ASPEC H7,33,C'TELEPHONE'                                               
         ASPEC H8,33,C'---------'                                               
         ASPEC H7,57,C'REP'                                                     
         ASPEC H8,57,C'---'                                                     
         ASPEC H7,63,C'OFF'                                                     
         ASPEC H8,63,C'---'                                                     
         ASPEC H7,69,C'SALESPERSON'                                             
         ASPEC H8,69,C'-----------'                                             
         ASPEC H7,96,C'LEAVE DATE'                                              
         ASPEC H8,96,C'----------'                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP7701S03/31/95'                                      
         END                                                                    
