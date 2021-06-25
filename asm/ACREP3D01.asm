*          DATA SET ACREP3D01  AT LEVEL 011 AS OF 11/21/90                      
*PHASE AC3D01A,+0                                                               
         TITLE 'CLIENT INCOME ANALYSIS'                                         
AC3D01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7,8,9,10,11                                  
         ACDEF H1,2,RUN                                                         
         ACDEF H1,133,REPORT                                                    
         ACDEF H1,148,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
         ACDEF H3,133,REQUESTOR                                                 
         ACDEF H4,133,MOSFILT                                                   
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,71,C'SALES ANALYSIS REPORT '                                  
         ACDEF H2,71,C'CLIENT/INCOME ANALYSIS'                                  
         ACDEF H3,71,C'______________________'                                  
         ACDEF H9,2,C'PRODUCT/INCOME ACCOUNT'                                   
*                                                                               
         ACDEF SPROG,2,3                                                        
         ACDEF H1,71,C'SALES ANALYSIS REPORT '                                  
         ACDEF H2,71,C'INCOME/CLIENT ANALYSIS'                                  
         ACDEF H3,71,C'______________________'                                  
         ACDEF H4,2,C'ACCOUNT'                                                  
         ACDEF H9,2,C'CLIENT / PRODUCT'                                         
*                                                                               
         ACDEF SPROG,4,5                                                        
         ACDEF H1,71,C' SALES ANALYSIS REPORT '                                 
         ACDEF H2,71,C'COSTING/INCOME ANALYSIS'                                 
         ACDEF H3,71,C'_______________________'                                 
         ACDEF H9,2,C'INCOME  ACCOUNT'                                          
*                                                                               
         ACDEF SPROG,6,7                                                        
         ACDEF H1,71,C' SALES ANALYSIS REPORT '                                 
         ACDEF H2,71,C'INCOME/COSTING ANALYSIS'                                 
         ACDEF H3,71,C'_______________________'                                 
         ACDEF H4,2,C'ACCOUNT'                                                  
         ACDEF H9,2,C'COSTING ACCOUNT'                                          
*                                                                               
         ACDEF SPROG,8,9,10,11                                                  
         ACDEF H1,71,C' SALES ANALYSIS SUMMARY '                                
         ACDEF H3,71,C'________________________'                                
         ACDEF SPROG,8                                                          
         ACDEF H2,71,C'CLIENT/INCOME ANALYSIS'                                  
         ACDEF SPROG,9                                                          
         ACDEF H2,71,C'INCOME/CLIENT ANALYSIS'                                  
         ACDEF SPROG,10                                                         
         ACDEF H2,71,C'COSTING/INCOME ANALYSIS'                                 
         ACDEF SPROG,11                                                         
         ACDEF H2,71,C'INCOME/COSTING ANALYSIS'                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREP3D01 11/21/90'                                      
         END                                                                    
