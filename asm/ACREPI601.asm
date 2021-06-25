*          DATA SET ACREPI601  AT LEVEL 007 AS OF 08/17/00                      
*PHASE ACI601A                                                                  
         EJECT                                                                  
ACI601   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         SPACE                                                                  
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,83,REQUESTOR                                                  
         ASPEC H5,2,UNIT                                                        
         ASPEC H6,2,LEDGER                                                      
         SPACE                                                                  
         ASPEC H09,2,C'CLI'                                                     
         ASPEC H10,2,C'---'                                                     
         ASPEC H09,7,C'PROD'                                                    
         ASPEC H10,7,C'----'                                                    
         SPACE 2                                                                
         SPROG 0                                                                
         ASPEC H1,45,C'JOB NUMBERS REPORT'                                      
         ASPEC H2,45,C'------------------'                                      
         ASPEC H09,13,C'PRIM JOB #'                                             
         ASPEC H10,13,C'----------'                                             
         ASPEC H09,25,C'JOB DESCRIPTION'                                        
         ASPEC H10,25,C'---------------'                                        
         ASPEC H09,62,C'OFF'                                                    
         ASPEC H10,62,C'---'                                                    
         ASPEC H09,67,C'COST CODE'                                              
         ASPEC H10,67,C'---------'                                              
         ASPEC H09,78,C'SECNDRY JOB #'                                          
         ASPEC H10,78,C'-------------'                                          
         ASPEC H09,93,C'DATE'                                                   
         ASPEC H10,93,C'----'                                                   
         ASPEC H09,103,C'CLOSED'                                                
         ASPEC H10,103,C'------'                                                
         SPACE 2                                                                
         SPROG 1                                                                
         ASPEC H1,43,C'DUPLICATE JOB NUMBERS'                                   
         ASPEC H2,43,C'---------------------'                                   
         ASPEC H09,13,C'JOB NUMBER'                                             
         ASPEC H10,13,C'----------'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPI601 08/17/00'                                      
         END                                                                    
