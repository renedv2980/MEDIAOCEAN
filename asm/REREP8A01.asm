*          DATA SET REREP8A01  AT LEVEL 007 AS OF 08/31/00                      
*          DATA SET REREP8A01  AT LEVEL 006 AS OF 11/05/84                      
*PHASE RE8A01A                                                                  
         TITLE 'SALES ACTIVITY REPORT SPECS'                                    
RE8A01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC GET,ADVERTISER                                                   
         FSPEC GET,AGENCY                                                       
         FSPEC GET,PRODUCT                                                      
         SPACE                                                                  
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,89,RENUM                                                      
         ASPEC H1,103,PAGE                                                      
         ASPEC H3,1,REP                                                         
         SPACE                                                                  
         SPROG 0                                                                
         ASPEC H1,46,C'SALES ACTIVITY REPORT'                                   
         ASPEC H2,46,21C'-'                                                     
         SPACE                                                                  
         SPROG 1                                                                
         ASPEC H1,46,C'SALESPERSON WORKSHEET'                                   
         ASPEC H2,46,21C'-'                                                     
         SPACE                                                                  
         SPROG 0,1                                                              
         ASPEC H6,2,C'ADVERTISER'                                               
         ASPEC H6,23,C'PRODUCT'                                                 
         SPACE                                                                  
         SPROG 0                                                                
         ASPEC H6,44,C'DEMO'                                                    
         ASPEC H6,54,C'BREAKING'                                                
         ASPEC H6,65,C'STATION CONTRACT PENDING'                                
         ASPEC H6,91,C'AMOUNT  OUTDATED  W/L'                                   
         SPACE                                                                  
         SPROG 1                                                                
         ASPEC H6,46,C'DEMO'                                                    
         ASPEC H6,55,C'STATION'                                                 
         ASPEC H6,63,C'CONTRACT'                                                
         ASPEC H6,73,C'AMOUNT'                                                  
         ASPEC H6,81,C'COMPETITION'                                             
         SPACE                                                                  
         SPROG 0,1                                                              
         ASPEC H7,2,C'AGENCY'                                                   
         ASPEC H7,23,C'SCHEDULE'                                                
         SPACE                                                                  
         SPROG 0                                                                
         ASPEC H7,38,C'LEN'                                                     
         ASPEC H7,44,C'OBJECTIVE'                                               
         ASPEC H7,54,8C'-'                                                      
         ASPEC H7,65,7C'-'                                                      
         ASPEC H7,74,C'NUMBER'                                                  
         ASPEC H7,82,7C'-'                                                      
         ASPEC H7,90,C'ORDERED'                                                 
         ASPEC H7,99,8C'-'                                                      
         ASPEC H7,109,3C'-'                                                     
         SPACE                                                                  
         SPROG 1                                                                
         ASPEC H7,40,C'LEN'                                                     
         ASPEC H7,46,4C'-'                                                      
         ASPEC H7,55,7C'-'                                                      
         ASPEC H7,64,C'NUMBER'                                                  
         ASPEC H7,72,C'ORDERED'                                                 
         ASPEC H7,81,11C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007REREP8A01 08/31/00'                                      
         END                                                                    
