*          DATA SET ACREPZN01  AT LEVEL 036 AS OF 06/22/00                      
*PHASE ACZN01A,+0                                                               
         TITLE 'RECOVERY REPORT'                                                
ACZN01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         FSPEC READ,ACCOUNTS                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,100,PAGE                                                      
         ASPEC H2,2,C'COMPANY'                                                  
         ASPEC H3,2,C'UNIT'                                                     
         ASPEC H3,100,REPORT                                                    
         ASPEC H4,2,C'LEDGER'                                                   
         ASPEC H4,100,REQUESTOR                                                 
         ASPEC H5,2,C'PEEL DATE'                                                
*                                                                               
         ASPEC H1,62,C'HISTORY REPORT'                                          
         ASPEC H2,62,C'--------------'                                          
*                                                                               
         ACDEF SPROG,0,1                                                        
         ASPEC H8,03,C'ACCOUNT NUMBER'                                          
         ASPEC H9,03,C'--------------'                                          
         ASPEC H8,23,C'OFFICE'                                                  
         ASPEC H9,23,C'------'                                                  
         ASPEC H8,31,C'PEELED DATE'                                             
         ASPEC H9,31,C'-----------'                                             
*                                                                               
         ACDEF SPROG,2                                                          
         ASPEC H8,03,C'PEELED DATE'                                             
         ASPEC H9,03,C'-----------'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036ACREPZN01 06/22/00'                                      
         END                                                                    
