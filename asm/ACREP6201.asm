*          DATA SET ACREP6201  AT LEVEL 010 AS OF 12/07/07                      
*PHASE AC6201A                                                                  
         TITLE 'INVENTORY BALANCE SPECS'                                        
AC6201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         FSPEC READ,HISTORY                                                     
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,84,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,84,REQUESTOR                                                  
         ASPEC H1,43,C'INVENTORY BALANCE REPORT'                                
         ASPEC H2,43,24C'-'                                                     
         ASPEC H4,2,C'CLIENT'                                                   
         ASPEC H6,2,C'PRODUCT/JOB'                                              
         SPROG 0                                                                
         ASPEC H7,2,C'-----------'                                              
         ASPEC H6,40,C'------DATE-----'                                         
         ASPEC H7,40,C'OPENED   CLOSED'                                         
         ASPEC H6,60,C'LATEST  ACTIVITY'                                        
         ASPEC H7,60,C'DATE  AND  MONTH'                                        
         ASPEC H6,80,C'CURRENT BALANCE    LAST BILLED'                          
         ASPEC H7,80,C'---------------    -----------'                          
         SPROG 1                                                                
         ASPEC H3,43,C'NET BILLING AMOUNTS'                                     
         ASPEC H4,43,27C'-'                                                     
         ASPEC H6,44,C'ORIGINAL'                                                
         ASPEC H7,44,C'ESTIMATE'                                                
         ASPEC H6,59,C'CURRENT'                                                 
         ASPEC H7,59,C'ESTIMATE'                                                
         ASPEC H6,72,C'NET BILLED'                                              
         ASPEC H7,72,10C'-'                                                     
         SPROG 2                                                                
         ASPEC H3,48,C'BILLING STATUS'                                          
         ASPEC H4,48,14C'-'                                                     
         ASPEC H6,44,C'ORIGINAL'                                                
         ASPEC H7,44,C'ESTIMATE'                                                
         ASPEC H6,59,C'CURRENT'                                                 
         ASPEC H7,59,C'ESTIMATE'                                                
         ASPEC H6,70,C'GROSS BILLED'                                            
         ASPEC H7,70,12C'-'                                                     
         ASPEC H6,90,C'BALANCE'                                                 
         ASPEC H7,90,7C'-'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREP6201 12/07/07'                                      
         END                                                                    
