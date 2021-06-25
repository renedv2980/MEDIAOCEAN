*          DATA SET ACREP4501  AT LEVEL 003 AS OF 08/16/00                      
*PHASE AC4501A                                                                  
         TITLE 'SPECS FOR DISTRIBUTION SCHEME REPORT'                           
AC4501   CSECT                                                                  
         SPROG 0                                                                
         SPROG 1,2,3,4                                                          
         ASPEC H1,2,RUN                                                         
         ASPEC H1,52,C'DISTRIBUTION SCHEME REPORT'                              
         ASPEC H1,98,REPORT                                                     
         ASPEC H1,117,PAGE                                                      
         ASPEC H2,2,COMPANY                                                     
         ASPEC H2,52,C'--------------------------'                              
         ASPEC H2,98,REQUESTOR                                                  
         ASPEC H3,2,UNIT                                                        
         ASPEC H3,98,C'SCHEME'                                                  
         ASPEC H4,2,LEDGER                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREP4501 08/16/00'                                      
         END                                                                    
