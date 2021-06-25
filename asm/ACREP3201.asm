*          DATA SET ACREP3201  AT LEVEL 012 AS OF 08/16/00                      
*PHASE AC3201A                                                                  
         TITLE 'SALES ANALYSIS SPECS'                                           
AC3201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         RSPEC MAXLINES,55                                                      
         SPACE 2                                                                
         SPROG 0,1,2,3,4,5,6,7,8,9                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,84,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H2,44,21C'-'                                                     
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,84,REQUESTOR                                                  
         ASPEC H8,60,C'GROSS'                                                   
         ASPEC H9,58,9C'-'                                                      
         ASPEC F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 0,1,2,5,8                                                        
         ASPEC H8,2,C'CLIENT'                                                   
         ASPEC H8,11,C'PRODUCT'                                                 
         ASPEC H9,2,6C'-'                                                       
         ASPEC H9,11,7C'-'                                                      
         SPACE 2                                                                
         SPROG 0                                                                
         ASPEC H8,22,C'INVOICE'                                                 
         ASPEC H8,33,C'DATE'                                                    
         ASPEC H9,22,7C'-'                                                      
         ASPEC H9,31,8C'-'                                                      
         SPACE 2                                                                
         SPROG 3,6                                                              
         ASPEC H8,2,C'CLIENT'                                                   
         ASPEC H8,11,C'MEDIA'                                                   
         ASPEC H9,2,6C'-'                                                       
         ASPEC H9,11,5C'-'                                                      
         SPACE 2                                                                
         SPROG 4,7                                                              
         ASPEC H8,2,C'MEDIA'                                                    
         ASPEC H9,2,5C'-'                                                       
         SPACE 1                                                                
         SPROG 5                                                                
         ASPEC H8,22,C'PRODUCT NAME'                                            
         ASPEC H9,22,C'------------'                                            
         SPACE 1                                                                
         SPROG 8                                                                
         ASPEC H8,22,C'MEDIA'                                                   
         ASPEC H9,22,C'-----'                                                   
         SPACE 1                                                                
         SPROG 9                                                                
         ASPEC H8,2,C'CLIENT'                                                   
         ASPEC H9,2,6C'-'                                                       
         ASPEC H8,11,C'PRODUCT GROUP'                                           
         ASPEC H9,11,C'-------------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREP3201 08/16/00'                                      
         END                                                                    
