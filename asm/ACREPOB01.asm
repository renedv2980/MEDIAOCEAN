*          DATA SET ACREPOB01  AT LEVEL 005 AS OF 08/17/00                      
*PHASE ACOB01A                                                                  
ACOB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF RESET                                                            
         ACDEF GETOPT,N                                                         
         ACDEF H1,2,RUN                                                         
         ACDEF H1,58,C'Office balances'                                         
         ACDEF H2,58,C'---------------'                                         
         ACDEF H4,58,REPORT                                                     
         ACDEF H5,58,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H6,58,REQUESTOR                                                  
         ACDEF M1,2,C'Account'                                                  
         ACDEF M1,17,C'Of'                                                      
         ACDEF M1,29,C'  Low   High  Close'                                     
         ACDEF M2,29,C'  MOA   MOA   MOA  '                                     
         ACDEF M2,50,C'       Debits'                                           
         ACDEF M2,65,C'      Credits'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPOB01 08/17/00'                                      
         END                                                                    
