*          DATA SET ACREPXE01  AT LEVEL 006 AS OF 08/16/00                      
*PHASE ACXE01A,+0                                                               
         TITLE 'SPECS FOR PEELED JOB CORRECTION REPORT'                         
ACXE01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,83,REQUESTOR                                                  
         ASPEC H5,2,LEDGER                                                      
         ASPEC H1,45,C'PEELED JOB CORRECTION REPORT'                            
         ASPEC H2,45,C'----------------------------'                            
         ASPEC H7,2,C'ACCOUNT CODE'                                             
         ASPEC H8,2,C'------------'                                             
         ASPEC H7,16,C'ACCOUNT NAME'                                            
         ASPEC H8,16,C'------------'                                            
         ASPEC H7,68,C'DEBITS'                                                  
         ASPEC H8,68,C'------'                                                  
         ASPEC H7,84,C'CREDITS'                                                 
         ASPEC H8,84,C'-------'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPXE01 08/16/00'                                      
         END                                                                    
