*          DATA SET ACREP6401  AT LEVEL 008 AS OF 09/09/91                      
*PHASE AC6401A,+0                                                               
         TITLE 'SPECS FOR CLIENT EXPENDITURE ANALYSIS'                          
AC6401   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,42,C'CLIENT EXPENDITURE ANALYSIS'                             
         ASPEC H1,84,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H2,42,27C'-'                                                     
         ACDEF H3,2,ORIGIN                                                      
         ASPEC H3,84,REQUESTOR                                                  
         ASPEC F1,2,REQDETS                                                     
         SPROG 0,1                                                              
         ASPEC H4,2,C'CLIENT'                                                   
         ASPEC H8,71,C'  BILLING'                                               
         ASPEC H8,101,C'UNBILLED'                                               
         ASPEC H9,101,C'ESTIMATE'                                               
         ASPEC H11,55,C'INVOICE   CURRENT       TO         FINAL'               
         ASPEC H12,55,C'NUMBER     MONTH       DATE'                            
         SPROG 0                                                                
         ASPEC H8,2,C'MEDIA/JOB NUMBER/DESCRIPTION'                             
         ASPEC H9,2,28C'-'                                                      
         SPROG 1                                                                
         ASPEC H8,2,C'PRODUCT/MEDIA'                                            
         ASPEC H9,2,C'-------------'                                            
         SPROG 2                                                                
         ASPEC H4,2,C'REQUEST SUMMARY'                                          
         ASPEC H8,2,108C' '                                                     
         ASPEC H9,2,108C' '                                                     
         ASPEC H8,2,C'W-CODE AND NAME'                                          
         ASPEC H8,26,C'NET BILLING'                                             
         ASPEC H8,41,C'GROSS BILLING'                                           
         ASPEC H9,2,15C'-'                                                      
         ASPEC H9,26,11C'-'                                                     
         ASPEC H9,41,13C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREP6401 09/09/91'                                      
         END                                                                    
