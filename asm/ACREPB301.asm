*          DATA SET ACREPB301  AT LEVEL 007 AS OF 08/16/00                      
*PHASE ACB301A,+0                                                               
         TITLE 'SPECS FOR MONTHLY BUDGET ANALYSIS'                              
ACB301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H1,44,C'MONTHLY BUDGET ANALYSIS'                                 
         ASPEC H2,44,23C'-'                                                     
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H5,2,C'UNIT'                                                     
         ASPEC H6,2,C'LEDGER'                                                   
         ASPEC H9,2,C'ACCOUNT CODE/NAME     DATA'                               
         ASPEC H10,2,C'-----------------     ----'                              
         ASPEC H9,105,C'TOTAL'                                                  
         ASPEC H10,105,C'-----'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPB301 08/16/00'                                      
         END                                                                    
