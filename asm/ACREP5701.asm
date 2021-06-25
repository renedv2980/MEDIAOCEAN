*          DATA SET ACREP5701  AT LEVEL 018 AS OF 08/16/00                      
*PHASE AC5701A                                                                  
AC5701   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         FSPEC READ,ACCOUNT                                                     
         FSPEC READ,TRANSACTIONS                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,48,C'BANK RECONCILIATION'                                     
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H2,48,19C'-'                                                     
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H5,2,C'ACCOUNT'                                                  
         ASPEC H5,86,C'PERIOD TO'                                               
*&&US                                                                           
         ASPEC H8,2,C'CHECK     CHECK'                                          
         ASPEC H9,2,C'NUMBER    DATE'                                           
*&&                                                                             
*&&UK                                                                           
         ASPEC H8,2,C'CHEQUE    CHEQUE'                                         
         ASPEC H9,2,C'NUMBER     DATE'                                          
*&&                                                                             
         ASPEC H8,20,C'PAYEE'                                                   
         ASPEC H9,20,C'ACCOUNT'                                                 
         ASPEC H8,35,C'PAYEE ACCOUNT NAME'                                      
         ASPEC H8,71,C'CHECK AMOUNT'                                            
         ASPEC H9,35,18C'-'                                                     
         ASPEC H9,71,12C'-'                                                     
         ASPEC F1,2,REQDETS                                                     
         SPROG 1                                                                
         ASPEC H3,86,C'CLEARED/UNCLEARED ITEMS'                                 
         ASPEC H8,9,C'C'                                                        
         ASPEC H9,9,C'-'                                                        
         SPROG 2                                                                
         ASPEC H3,86,C'CLEARED ITEMS ONLY'                                      
         SPROG 3                                                                
         ASPEC H3,86,C'UNCLEARED ITEMS ONLY'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACREP5701 08/16/00'                                      
         END                                                                    
