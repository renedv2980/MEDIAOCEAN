*          DATA SET ACREPXC01  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACXC01A,+0                                                               
         TITLE 'SPECS FOR DELETED CHECKS REPORT'                                
ACXC01   CSECT                                                                  
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
         ASPEC H1,45,C'DELETED CHECKS REPORT'                                   
         ASPEC H2,45,21C'-'                                                     
         ASPEC H7,2,C'ACCOUNT CODE'                                             
         ASPEC H8,2,C'------------'                                             
         ASPEC H7,16,C'ACCOUNT NAME'                                            
         ASPEC H8,16,C'------------'                                            
         ASPEC H7,55,C'CHECK'                                                   
         ASPEC H8,55,C' NO. '                                                   
         ASPEC H7,68,C'DEBITS'                                                  
         ASPEC H8,68,C'------'                                                  
         ASPEC H7,84,C'CREDITS'                                                 
         ASPEC H8,84,C'-------'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPXC01 08/16/00'                                      
         END                                                                    
