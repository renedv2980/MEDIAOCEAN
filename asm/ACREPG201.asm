*          DATA SET ACREPG201  AT LEVEL 005 AS OF 08/17/00                      
*PHASE ACG201A                                                                  
         TITLE 'PRINT SPEC FOR ACREPG2 - PROFIT/LOSS REPORT'                    
ACG201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,98,PAGE                                                       
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,84,REQUESTOR                                                  
         ASPEC H4,84,C'PERIOD'                                                  
         ASPEC H4,2,UNIT                                                        
         ASPEC H5,2,LEDGER                                                      
         SPROG 0                                                                
         ASPEC H1,46,C'PROFIT/LOSS REPORT'                                      
         ASPEC H2,46,C'------------------'                                      
         SPROG 1                                                                
         ASPEC H1,45,C'BALANCE SHEET REPORT'                                    
         ASPEC H2,45,C'--------------------'                                    
