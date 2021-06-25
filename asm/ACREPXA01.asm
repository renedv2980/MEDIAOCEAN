*          DATA SET ACREPXA01  AT LEVEL 012 AS OF 08/16/00                      
*PHASE ACXA01A,+0                                                               
         TITLE 'CREATE TIME POSTINGS FROM TMS RECORDS'                          
ACXA01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC UPDATE,ACCFILE                                                   
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,83,REQUESTOR                                                  
         ASPEC H1,38,C'TMS POSTINGS FIX'                                        
         ASPEC H2,38,C'----------------'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREPXA01 08/16/00'                                      
         END                                                                    
