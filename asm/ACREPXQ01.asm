*          DATA SET ACREPXQ01  AT LEVEL 005 AS OF 04/10/97                      
*PHASE ACXQ01B,+0                                                               
         TITLE 'SPECS FOR OFFICE BAL CHECK'                                     
ACXQ01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
*                                                                               
         ASPEC H1,2,RUN                                                         
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H4,2,COMPANY                                                     
         ASPEC F1,2,REQDETS                                                     
         ASPEC H1,48,C'UNPEELING REPORT'                                        
         ASPEC H2,48,C'----------------'                                        
         ASPEC H5,2,C'LEDGER'                                                   
         ASPEC H8,2,C'ACCOUNT CODE    ACCOUNT NAME'                             
         ASPEC H9,2,C'------------    ------------'                             
         ASPEC H8,69,C'DEBITS'                                                  
         ASPEC H9,69,C'------'                                                  
         ASPEC H8,86,C'CREDITS'                                                 
         ASPEC H9,86,C'-------'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPXQ01 04/10/97'                                      
         END                                                                    
