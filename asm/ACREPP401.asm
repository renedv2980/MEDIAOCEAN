*          DATA SET ACREPP401  AT LEVEL 012 AS OF 08/17/00                      
*PHASE ACP401A                                                                  
         TITLE 'SPECS FOR JOB/EXPENSE ORDER REPORT'                             
ACP401   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
*                                                                               
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,80,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,80,REQUESTOR                                                  
         ASPEC H9,2,C'ORDER'                                                    
         ASPEC H10,3,C'NO'                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,43,C'UNMATCHED JOB ORDER DETAIL'                              
         ASPEC H2,43,26C'-'                                                     
         ASPEC H9,10,C'WORK-CODE'                                               
         ASPEC H10,10,C'---------'                                              
         ASPEC H9,31,C'SUPPLIER'                                                
         ASPEC H10,31,C'--------'                                               
         ASPEC H9,48,C'ORDER DATE'                                              
         ASPEC H10,48,C'----------'                                             
         ASPEC H9,66,C'ORDER    COMMISSION         GROSS'                       
         ASPEC H10,65,C'AMOUNT    ----------         -----'                     
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H1,43,C'UNMATCHED EXPENSE ORDER DETAIL'                          
         ASPEC H2,43,C'------------------------------'                          
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         ASPEC H7,2,C'ACCOUNT'                                                  
         ASPEC H9,10,C'SUPPLIER CODE AND NAME'                                  
         ASPEC H10,10,C'----------------------'                                 
         ASPEC H9,60,C'ORDER DATE'                                              
         ASPEC H10,60,C'----------'                                             
         ASPEC H9,74,C'ORDER AMOUNT'                                            
         ASPEC H10,74,C'------------'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREPP401 08/17/00'                                      
         END                                                                    
