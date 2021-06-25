*          DATA SET ACREPI401  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACI401A                                                                  
         TITLE 'ACI401 -  CLOSED PROD JOB INTERFACE - SPECS'                    
ACI401   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF MODE,PROCLEV                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,76,REPORT                                                     
         ACDEF H1,90,PAGE                                                       
         ACDEF H3,2,COMPANY                                                     
         ACDEF H7,4,C'YEAR'                                                     
         ACDEF H8,4,C'----'                                                     
         ACDEF H7,14,C'BUDGET CODE'                                             
         ACDEF H8,14,C'-----------'                                             
         ACDEF H7,37,C'JOB NUMBER'                                              
         ACDEF H8,37,C'----------'                                              
         ACDEF H6,58,C'PRESENT'                                                 
         ACDEF H7,58,C'ESTIMATE'                                                
         ACDEF H8,58,C'--------'                                                
         ACDEF H6,74,C'ACTUAL'                                                  
         ACDEF H7,74,C'BILLING'                                                 
         ACDEF H8,74,C'-------'                                                 
         ACDEF H5,89,C'OVER(+)'                                                 
         ACDEF H6,89,C'UNDER(-)'                                                
         ACDEF H7,89,C'ESTIMATE'                                                
         ACDEF H8,89,C'--------'                                                
         ACDEF H6,100,C'PERCENT'                                                
         ACDEF H7,100,C'VARIANCE'                                               
         ACDEF H8,100,C'--------'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPI401 08/17/00'                                      
         END                                                                    
