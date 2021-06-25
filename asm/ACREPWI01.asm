*          DATA SET ACREPWI01  AT LEVEL 001 AS OF 03/05/98                      
*PHASE ACWI01A,+0                                                               
         TITLE 'WORKING INVESTMENT REPORT'                                      
ACWI01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF WIDTH,198                                                        
         ACDEF GETOPT,NO                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7,8                                          
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,070,C'WORKING INVESTMENT REPORT'                              
         ACDEF H1,130,C'FORMAT'                                                 
         ACDEF H2,130,PAGE                                                      
*                                                                               
         ACDEF SPROG,0,1,4,5                                                    
         ACDEF H8,003,C'CODE NAME'                                              
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H6,031,C'GROUP'                                                  
         ACDEF H8,028,C'CAT'                                                    
         ACDEF H8,034,C'DIV'                                                    
         ACDEF H8,039,C'CLI'                                                    
*                                                                               
         ACDEF SPROG,2,3,6,7                                                    
         ACDEF H8,003,C'OFFICE NAME'                                            
*                                                                               
         ACDEF SPROG,2,3                                                        
         ACDEF H8,035,C'OFF'                                                    
         ACDEF H8,039,C'CLI'                                                    
*                                                                               
         ACDEF SPROG,0,2,4,6                                                    
         ACDEF H6,044,C'OPENING BALANCE'                                        
         ACDEF H7,048,C'PRIOR TO'                                               
         ACDEF H6,076,C'ACCOUNTS RECEIVABLES (+)'                               
         ACDEF H8,065,C'BILLED'                                                 
         ACDEF H8,080,C'COLLECTED'                                              
         ACDEF H8,099,C'NET A/R'                                                
         ACDEF H6,118,C'(-)'                                                    
         ACDEF H7,115,C'ACCOUNTS'                                               
         ACDEF H8,115,C'PAYABLES'                                               
         ACDEF H6,135,C'(+)'                                                    
         ACDEF H7,131,C'PRODUCTION'                                             
         ACDEF H8,131,C'INVENTORY'                                              
         ACDEF H6,145,C'WORKING INVESTMENT'                                     
         ACDEF H7,152,C'AS OF'                                                  
*                                                                               
         ACDEF SPROG,1,3,5,7                                                    
         ACDEF H8,047,C'SP LEDGER'                                              
         ACDEF H8,066,C'SS LEDGER'                                              
         ACDEF H8,085,C'SU LEDGER'                                              
         ACDEF H8,104,C'SV LEDGER'                                              
         ACDEF H8,123,C'SZ LEDGER'                                              
         ACDEF H8,142,C'CLIENT TOTAL'                                           
*                                                                               
         ACDEF SPROG,8                                                          
         ACDEF H8,003,C'CLIENT NAME'                                            
         ACDEF H7,042,C'CLIENT'                                                 
         ACDEF H8,042,C'CODE'                                                   
         ACDEF H8,049,C'SR ACCOUNT'                                             
         ACDEF H8,071,C'BILLED'                                                 
         ACDEF H8,088,C'COLLECTED'                                              
         ACDEF H8,110,C'NET'                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPWI01 03/05/98'                                      
         END                                                                    
