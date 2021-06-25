*          DATA SET ACREPP201  AT LEVEL 005 AS OF 08/17/00                      
*PHASE ACP201A                                                                  
         TITLE 'SPECS FOR ORDER ESTIMATE SUMMARY'                               
ACP201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,44,C'ORDER/ESTIMATE SUMMARY'                                  
         ASPEC H1,81,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H2,44,22C'-'                                                     
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,81,REQUESTOR                                                  
         ASPEC H4,2,C'CLIENT'                                                   
         ASPEC H6,2,C'PRODUCT AND JOB'                                          
         ASPEC H7,2,C'------- --- ---'                                          
         ASPEC H6,76,C'CHARGES'                                                 
         ASPEC H7,76,C'-------'                                                 
         ASPEC H6,87,C'BILLING  UNCOMMITTED'                                    
         ASPEC H7,87,C'-------    ESTIMATE'                                     
         ACDEF PRORATA,BILL                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPP201 08/17/00'                                      
         END                                                                    
