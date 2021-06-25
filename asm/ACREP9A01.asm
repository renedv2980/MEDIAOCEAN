*          DATA SET ACREP9A01  AT LEVEL 030 AS OF 08/16/00                      
*PHASE AC9A01A                                                                  
         TITLE 'SPECS FOR CLIENT PROFITABILITY REPORT'                          
AC9A01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
*&&UK*&& RSPEC MAXLINES,57                                                      
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,88,REPORT                                                     
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,88,REQUESTOR                                                  
         SPROG 0                                                                
         ASPEC H1,41,C'CLIENT PROFITABILITY REPORT'                             
         ASPEC H2,41,27C'-'                                                     
         SPROG 1                                                                
         ASPEC H1,41,C'CLIENT REVENUE ANALYSIS'                                 
         ASPEC H2,41,23C'-'                                                     
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H5,2,C'COMPANY/CLIENT COMPARISON'                                
         ASPEC H6,2,110C' '                                                     
         ASPEC H7,45,C'1-BILLING   2-INCOME  3-EXPENSE'                         
         ASPEC H8,45,C'---------   --------  ---------'                         
         ASPEC H7,81,C'4-TIME     DIRECT'                                       
         ASPEC H8,81,C'------     MARGIN'                                       
*&&US                                                                           
         ASPEC H7,98,C'    PROFIT '                                             
         ASPEC H8,98,C'   CONTRIB.'                                             
*&&                                                                             
*&&UK                                                                           
         ASPEC H7,99,C' NET PROFIT'                                             
         ASPEC H8,99,C'  OR LOSS  '                                             
*&&                                                                             
         SPROG 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACREP9A01 08/16/00'                                      
         END                                                                    
