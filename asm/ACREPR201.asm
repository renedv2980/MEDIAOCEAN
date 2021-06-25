*          DATA SET ACREPR201  AT LEVEL 017 AS OF 09/23/96                      
*PHASE ACR201A,+0                                                               
         TITLE 'REALIZATION REPORT  - EMPLOYEES'                                
ACR201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANSACTIONS                                                
         RSPEC MAXLINES,56                                                      
         ACDEF RESET                                                            
         ACDEF PRORATA,BILL                                                     
*                                                                               
         SPROG 0                                                                
         ASPEC H1,2,RUN,WIDE=198                                                
         ASPEC H1,125,REPORT,WIDE=198                                           
         ASPEC H1,138,PAGE,WIDE=198                                             
         ASPEC H3,125,REQUESTOR,WIDE=198                                        
         ASPEC H4,2,C'OFFICE',WIDE=198                                          
         ASPEC H5,125,PERIOD,WIDE=198                                           
         SPACE 1                                                                
         ASPEC H3,2,C'COMPANY',WIDE=198                                         
         ASPEC H1,67,C'EMPLOYEE REALIZATION REPORT',WIDE=198                    
         ASPEC H2,67,27C'_',WIDE=198                                            
         SPACE 1                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACREPR201 09/23/96'                                      
         END                                                                    
