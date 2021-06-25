*          DATA SET ACREPI101  AT LEVEL 024 AS OF 01/19/01                      
*PHASE ACI101A                                                                  
         TITLE 'SPECS FOR PRODUCTION INTERFACE'                                 
ACI101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
         ACDEF RESET                                                            
*                                                                               
         SPROG 0,1,2,3                                                          
         ASPEC H1,2,RUN,WIDE=198                                                
         ASPEC H1,81,REPORT,WIDE=198                                            
         ASPEC H1,97,PAGE,WIDE=198                                              
         ASPEC H3,2,COMPANY,WIDE=198                                            
         ASPEC H4,43,PERIOD,WIDE=198                                            
*                                                                               
         SPROG 0,1                                                              
         ASPEC H7,2,C'CLI/PROD/JOB',WIDE=198                                    
         ASPEC H8,2,12C'-',WIDE=198                                             
         ASPEC H7,19,C'OFFICE',WIDE=198                                         
         ASPEC H8,19,6C'-',WIDE=198                                             
         ASPEC H7,28,C'DATE',WIDE=198                                           
         ASPEC H8,28,4C'-',WIDE=198                                             
         ASPEC H7,36,C'NUMBER',WIDE=198                                         
         ASPEC H8,36,6C'-',WIDE=198                                             
*                                                                               
         SPROG 2,3                                                              
         ASPEC H5,2,C'* MEDIA SUMMARY *',WIDE=198                               
         ASPEC H7,2,C'MEDIA/CLIENT',WIDE=198                                    
         ASPEC H8,2,12C'-',WIDE=198                                             
*                                                                               
         SPROG 0,1,2,3                                                          
         ASPEC H7,50,C'RECEIVABLE',WIDE=198                                     
         ASPEC H8,50,10C'-',WIDE=198                                            
         ASPEC H7,67,C'INVENTORY',WIDE=198                                      
         ASPEC H8,67,9C'-',WIDE=198                                             
         ASPEC H7,82,C'COMMISSION',WIDE=198                                     
         ASPEC H8,82,10C'-',WIDE=198                                            
         ASPEC H7,95,C'CASH DISCOUNT',WIDE=198                                  
         ASPEC H8,95,13C'-',WIDE=198                                            
*                                                                               
         SPROG 0,2                                                              
         ASPEC H1,40,C'PRODUCTION BILLING INTERFACE',WIDE=198                   
         ASPEC H2,40,28C'-',WIDE=198                                            
*                                                                               
         SPROG 1,3                                                              
         ASPEC H1,40,C'PRODUCTION BILLING SUMMARY',WIDE=198                     
         ASPEC H2,40,26C'-',WIDE=198                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREPI101 01/19/01'                                      
         END                                                                    
