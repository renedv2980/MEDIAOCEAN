*          DATA SET ACREPZ801  AT LEVEL 011 AS OF 08/16/00                      
*PHASE ACZ801A                                                                  
         TITLE 'SPECS FOR DELETE COST BUCKETS'                                  
ACZ801   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFILE                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,38,C'DELETE COST BUCKETS   '                                  
         ACDEF H2,38,C'-------------------   '                                  
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H09,2,C'ACCOUNT CODE'                                            
         ACDEF H10,2,C'------------'                                            
         ACDEF H09,21,C'CONTRA CODE'                                            
         ACDEF H10,21,C'-----------'                                            
         ACDEF H09,41,C'CONTRA NAME'                                            
         ACDEF H10,41,C'-----------'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREPZ801 08/16/00'                                      
         END                                                                    
