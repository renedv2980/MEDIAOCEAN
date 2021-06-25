*          DATA SET ACREP3401  AT LEVEL 004 AS OF 08/16/00                      
*PHASE AC3401A                                                                  
         TITLE 'CLIENT INCOME ANALYSIS'                                         
AC3401   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,44,C'CLIENT INCOME ANALYSIS'                                  
         ASPEC H2,44,C'----------------------'                                  
         ASPEC H1,84,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,84,REQUESTOR                                                  
         ASPEC H8,2,C'PRODUCT/INCOME ACCOUNT'                                   
         ASPEC H9,2,C'----------------------'                                   
         ASPEC H8,29,C'INVOICE       DATE'                                      
         ASPEC H9,29,C'-------       ----'                                      
         ASPEC H8,55,C'  GROSS     '                                            
         ASPEC H9,55,C'  -----     '                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP3401 08/16/00'                                      
         END                                                                    
