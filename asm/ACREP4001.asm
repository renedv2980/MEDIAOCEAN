*          DATA SET ACREP4001  AT LEVEL 007 AS OF 08/16/00                      
*PHASE AC4001A                                                                  
         TITLE 'CLIENT INCOME ANALYSIS'                                         
AC4001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'HISTORICAL CLIENT INCOME ANALYSIS'                       
         ASPEC H2,40,C'---------------------------------'                       
         ASPEC H1,84,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,84,REQUESTOR                                                  
         ASPEC H4,2,C'CLIENT'                                                   
         ASPEC H8,56,C'  GROSS     '                                            
         ASPEC H9,56,C'  -----     '                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREP4001 08/16/00'                                      
         END                                                                    
