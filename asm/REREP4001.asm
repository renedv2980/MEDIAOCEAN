*          DATA SET REREP4001  AT LEVEL 011 AS OF 08/31/00                      
*          DATA SET REREP4001  AT LEVEL 010 AS OF 06/04/80                      
*PHASE RE4001A                                                                  
         TITLE 'SPECS FOR SALESMAN REPORT'                                      
RE4001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC GET,ADVERTISER                                                   
         FSPEC GET,AGENCY                                                       
         RSPEC MAXLINES,56                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC F1,1,REQDETS                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,49,C'SALESPERSON REPORT'                                      
         ASPEC H2,49,18C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,REP                                                         
         ASPEC H4,46,PERIOD                                                     
         ASPEC H4,88,REQUESTOR                                                  
         ASPEC H5,46,BASIS                                                      
         ASPEC H7,2,OFFICE                                                      
         ASPEC H8,88,TEAM                                                       
         ASPEC H7,46,SALESMAN                                                   
         ASPEC H7,88,DIVISION                                                   
         ASPEC H10,2,C'STATION  CONTRACT  ADVERTISER CODE/NAME'                 
         ASPEC H11,2,C'-------   NUMBER   --------------------'                 
         ASPEC H10,47,C'AGENCY CODE/NAME'                                       
         ASPEC H11,47,C'----------------'                                       
         ASPEC H10,77,C'PRODUCT                  AMOUNT'                        
         ASPEC H11,77,C'-------                  ------'                        
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC F1,1,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011REREP4001 08/31/00'                                      
         END                                                                    
