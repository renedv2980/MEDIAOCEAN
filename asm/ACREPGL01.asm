*          DATA SET ACREPGL01  AT LEVEL 004 AS OF 07/12/07                      
*PHASE ACGL01A,*                                                                
         TITLE 'SPECS FOR GENERAL LEDGER POSTING'                               
ACGL01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
         ACDEF WIDTH,198                                                        
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,2,MOSFILT                                                     
         ACDEF H2,86,REQUESTOR                                                  
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF F1,2,REQDETS                                                     
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,47,C'STATEMENT OF ACCOUNT'                                    
         ACDEF H2,47,20C'-'                                                     
         ACDEF H10,2,C'CONTRA-ACCOUNT'                                          
         ACDEF H10,25,C'------------TRANSACTION------------'                    
         ACDEF H10,80,C'DEBIT      CREDIT      BALANCE'                         
         ACDEF H11,2,14C'-'                                                     
         ACDEF H11,25,C'BATCH  DATE     REF. CODE NARRATIVE'                    
         ACDEF H11,80,C'-----      ------      -------'                         
         ACDEF SPROG,1                                                          
         ACDEF H10,80,C'           AMOUNT        TOTAL'                         
         ACDEF H11,80,C'           ------        -----'                         
         ACDEF SPROG,2,3                                                        
         ACDEF H1,47,C'CONTRA-ACCOUNT SUMMARY'                                  
         ACDEF H2,47,22C'-'                                                     
         ACDEF H10,2,C'CONTRA-ACCOUNT CODE AND NAME'                            
         ACDEF H10,80,C'DEBIT      CREDIT      BALANCE'                         
         ACDEF H11,2,28C'-'                                                     
         ACDEF H11,80,C'-----      ------      -------'                         
         ACDEF SPROG,3                                                          
         ACDEF H10,80,C'           AMOUNT        TOTAL'                         
         ACDEF H11,80,C'           ------        -----'                         
         ACDEF SPROG,4,5                                                        
         ACDEF H1,47,C'ANALYSIS CODE SUMMARY'                                   
         ACDEF H2,47,21C'-'                                                     
         ACDEF H10,2,C'ANALYSIS CODE AND NAME'                                  
         ACDEF H10,80,C'DEBIT      CREDIT      BALANCE'                         
         ACDEF H11,2,22C'-'                                                     
         ACDEF H11,80,C'-----      ------      -------'                         
         ACDEF SPROG,5                                                          
         ACDEF H10,80,C'           AMOUNT        TOTAL'                         
         ACDEF H11,80,C'           ------        -----'                         
         ACDEF SPROG,6                                                          
         ACDEF H1,45,C'LIST OF INACTIVE ACCOUNTS'                               
         ACDEF H2,45,25C'-'                                                     
         ACDEF H10,2,C'ACCOUNT CODE AND NAME'                                   
         ACDEF H10,103,C'BALANCE'                                               
         ACDEF H11,2,21C'-'                                                     
         ACDEF H11,103,C'-------'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPGL01 07/12/07'                                      
         END                                                                    
