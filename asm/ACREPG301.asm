*          DATA SET ACREPG301  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACG301A                                                                  
         TITLE 'SPECS FOR GENERAL LEDGER STATEMENT'                             
ACG301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
*&&US*&& RSPEC MAXLINES,54                                                      
*                                  0=DETAILS                                    
*                                  1=DETAILS WITH OPT1=S                        
*                                  2=SUMMARIES                                  
*                                  3=LEDGER SUMMARY                             
*                                  4=REQUEST SUMMARY                            
         SPROG 0,1,2,3,4                                                        
         ASPEC H1,2,RUN                                                         
         ASPEC H1,44,C'GENERAL LEDGER STATEMENT'                                
         ASPEC H2,44,24C'-'                                                     
         ASPEC H1,81,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,81,REQUESTOR                                                  
         ASPEC H8,47,C'    DEBITS          CREDITS    '                         
         ASPEC H8,79,C'    BALANCE       YEAR TO-DATE '                         
         ASPEC F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 0,1,2,3                                                          
         ASPEC H5,2,LEDGER                                                      
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H6,2,C'ACCOUNT'                                                  
         SPACE 1                                                                
         SPROG 0,1                                                              
         ASPEC H8,18,C'--------TRANSACTION---------'                            
         ASPEC H9,18,C'MOS        DATE    REFERENCE'                            
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H8,2,C'CONTRA-ACCOUNT /'                                         
         ASPEC H9,2,C'NUMBER/NAME   / '                                         
         SPACE 1                                                                
         SPROG 1,2,3,4                                                          
         ASPEC H8,2,C'                '                                         
         ASPEC H9,2,C'                '                                         
         SPACE 1                                                                
         SPROG 2,3,4                                                            
         ASPEC H8,16,C'MONTH OF                      '                          
         ASPEC H9,16,C'SERVICE                       '                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPG301 08/17/00'                                      
         END                                                                    
