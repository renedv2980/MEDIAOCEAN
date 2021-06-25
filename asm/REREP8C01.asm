*          DATA SET REREP8C01  AT LEVEL 010 AS OF 08/31/00                      
*          DATA SET REREP8C01  AT LEVEL 009 AS OF 02/01/89                      
*PHASE RE8C01A                                                                  
         TITLE 'SALESPERSON SUCCESS REPORT SPECS'                               
RE8C01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC GET,ADVERTISER                                                   
         FSPEC GET,AGENCY                                                       
         FSPEC GET,PRODUCT                                                      
         SPACE                                                                  
         SPROG 0,1,2,3                                                          
         ASPEC H1,2,RUN                                                         
         ASPEC H1,44,C'SALESPERSON SUCCESS REPORT'                              
         ASPEC H1,89,RENUM                                                      
         ASPEC H1,102,PAGE                                                      
         ASPEC H2,44,26C'-'                                                     
         ASPEC H3,1,REP                                                         
         ASPEC H3,89,REQUESTOR                                                  
         SPACE                                                                  
         SPROG 0                                                                
         ASPEC H7,2,C'ADVERTISER'                                               
         ASPEC H7,23,C'PRODUCT'                                                 
         ASPEC H7,45,C'DEMO'                                                    
         ASPEC H7,55,C'CONTRACT'                                                
         ASPEC H7,65,C'WON'                                                     
         ASPEC H7,70,C'AMOUNT'                                                  
         ASPEC H7,78,C'LOST'                                                    
         ASPEC H7,84,C'COMPETITION'                                             
         SPACE                                                                  
         ASPEC H8,2,C'AGENCY'                                                   
         ASPEC H8,23,C'SCHEDULE'                                                
         ASPEC H8,40,C'LEN'                                                     
         ASPEC H8,45,4C'-'                                                      
         ASPEC H8,56,C'NUMBER'                                                  
         ASPEC H8,65,3C'-'                                                      
         ASPEC H8,70,6C'-'                                                      
         ASPEC H8,78,4C'-'                                                      
         ASPEC H8,84,11C'-'                                                     
         SPACE                                                                  
         SPROG 1                                                                
         ASPEC H6,41,C'SALESPERSON DEMOGRAPHIC ANALYSIS'                        
         ASPEC H9,15,C'DEMO'                                                    
         ASPEC H10,15,4C'-'                                                     
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H6,50,C'OFFICE SUMMARY'                                          
         ASPEC H9,6,C'SALESPERSON'                                              
         ASPEC H10,6,11C'-'                                                     
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H6,36,C'OFFICE SUMMARY FOR NETWORK OPPORTUNITIES'                
         ASPEC H9,6,C'SALESPERSON'                                              
         ASPEC H10,6,11C'-'                                                     
         SPACE 1                                                                
         SPROG 1,2,3                                                            
         ASPEC H9,27,C'ACCOUNTS'                                                
         ASPEC H9,42,C'WINS'                                                    
         ASPEC H9,53,C'LOSSES'                                                  
         ASPEC H9,67,C'WIN'                                                     
         ASPEC H9,79,C'WIN'                                                     
         ASPEC H9,89,C'PERCENT OF'                                              
         SPACE                                                                  
         ASPEC H10,27,C'RESOLVED'                                               
         ASPEC H10,42,4C'-'                                                     
         ASPEC H10,53,6C'-'                                                     
         ASPEC H10,65,C'PERCENT'                                                
         ASPEC H10,77,C'DOLLARS'                                                
         ASPEC H10,89,C'TOTAL DOLLARS'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REREP8C01 08/31/00'                                      
         END                                                                    
