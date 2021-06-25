*          DATA SET APGM201    AT LEVEL 018 AS OF 08/18/00                      
*PHASE ACM201A                                                                  
         TITLE 'ACM201 - SPECS FOR MANPOWER REPORTING SYSTEM'                   
ACM201   CSECT                                                                  
         PRINT NOGEN                                                            
*        RSPEC REQUEST,NOREP                                                    
         SPROG 1,2,3                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H2,2,REQUESTOR                                                   
*&&US                                                                           
         SPROG 4                                                                
         ASPEC H1,2,RUN,WIDE=198                                                
         ASPEC H2,2,REQUESTOR,WIDE=198                                          
*&&                                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018APGM201   08/18/00'                                      
         END                                                                    
