*          DATA SET ACREPIH01  AT LEVEL 024 AS OF 08/17/00                      
*PHASE ACIH01A                                                                  
         TITLE 'SPECS FOR H AND K GEN LEDGER TAPE'                              
ACIH01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,81,REPORT                                                     
         ACDEF H1,97,PAGE                                                       
         ACDEF H3,2,COMPANY                                                     
         ACDEF H4,43,MOSFILT                                                    
         ACDEF H7,2,C'OFFICE'                                                   
         ACDEF H7,9,C' ACCOUNT'                                                 
         ACDEF H7,21,C'ORIGIN'                                                  
         ACDEF H7,36,C'DEBITS'                                                  
         ACDEF H7,52,C'CREDITS'                                                 
         ACDEF H7,70,C'NET'                                                     
         SPACE 1                                                                
         ACDEF H1,38,C'GENERAL LEDGER ACCOUNT POSTINGS'                         
         ACDEF H2,38,31C'-'                                                     
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREPIH01 08/17/00'                                      
         END                                                                    
