*          DATA SET ACREPI101S AT LEVEL 023 AS OF 01/17/01                      
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
         ASPEC H1,2,RUN                                                         
         ASPEC H1,81,REPORT                                                     
         ASPEC H1,97,PAGE                                                       
         ASPEC H3,2,COMPANY                                                     
         ASPEC H4,43,PERIOD                                                     
*                                                                               
         SPROG 0,1                                                              
         ASPEC H7,2,C'CLI/PROD/JOB'                                             
         ASPEC H8,2,12C'-'                                                      
         ASPEC H7,19,C'OFFICE'                                                  
         ASPEC H8,19,6C'-'                                                      
         ASPEC H7,28,C'DATE'                                                    
         ASPEC H8,28,4C'-'                                                      
         ASPEC H7,36,C'NUMBER'                                                  
         ASPEC H8,36,6C'-'                                                      
*                                                                               
         SPROG 2,3                                                              
         ASPEC H5,2,C'* MEDIA SUMMARY *'                                        
         ASPEC H7,2,C'MEDIA/CLIENT'                                             
         ASPEC H8,2,12C'-'                                                      
*                                                                               
         SPROG 0,1,2,3                                                          
         ASPEC H7,50,C'RECEIVABLE'                                              
         ASPEC H8,50,10C'-'                                                     
         ASPEC H7,67,C'INVENTORY'                                               
         ASPEC H8,67,9C'-'                                                      
         ASPEC H7,82,C'COMMISSION'                                              
         ASPEC H8,82,10C'-'                                                     
         ASPEC H7,95,C'CASH DISCOUNT'                                           
         ASPEC H8,95,13C'-'                                                     
*                                                                               
         SPROG 0,2                                                              
         ASPEC H1,40,C'PRODUCTION BILLING INTERFACE'                            
         ASPEC H2,40,28C'-'                                                     
*                                                                               
         SPROG 1,3                                                              
         ASPEC H1,40,C'PRODUCTION BILLING SUMMARY'                              
         ASPEC H2,40,26C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACREPI101S01/17/01'                                      
         END                                                                    
