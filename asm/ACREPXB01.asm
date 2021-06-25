*          DATA SET ACREPXB01  AT LEVEL 007 AS OF 05/20/14                      
*PHASE ACXB01A                                                                  
         TITLE 'FIX ACCOUNT BALANCES'                                           
ACXB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANS                                                       
         ACDEF GETOPT,N                                                         
         ACDEF MODE,PROCLEV                                                     
*                                                                               
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,38,C'Fix Account Balances'                                    
         ACDEF H2,38,C'--------------------'                                    
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H09,2,C'Account code and name'                                   
         ACDEF H10,2,C'---------------------'                                   
         ACDEF H09,52,C'        Debits'                                         
         ACDEF H10,52,C'        Before'                                         
         ACDEF H09,67,C'       Credits'                                         
         ACDEF H10,67,C'        Before'                                         
         ACDEF H09,82,C'        Debits'                                         
         ACDEF H10,82,C'         After'                                         
         ACDEF H09,97,C'       Credits'                                         
         ACDEF H10,97,C'         After'                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPXB01 05/20/14'                                      
         END                                                                    
