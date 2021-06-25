*          DATA SET ACREPP701  AT LEVEL 011 AS OF 08/17/00                      
*PHASE ACP701A                                                                  
         TITLE 'SPECS FOR NEW AGEING'                                           
ACP701   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
*                                                                               
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,C'JOB AGEING'                                              
         ASPEC H2,45,C'----------'                                              
         ASPEC H1,80,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H5,2,C'CLIENT'                                                   
         ASPEC H4,80,REQUESTOR                                                  
         ASPEC H8,92,C'  PRE-     BALANCE'                                      
         ASPEC H9,92,C'BILLING    -------'                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREPP701 08/17/00'                                      
         END                                                                    
