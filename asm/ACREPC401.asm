*          DATA SET ACREPC401  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACC401A,+0                                                               
         TITLE 'SPECS FOR PROJECT TIME CONFIRMATION'                            
         PRINT NOGEN                                                            
ACC401   CSECT                                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,43,C'PROJECT TIME CONFIRMATION'                               
         ASPEC H2,43,C'-------------------------'                               
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,101,PAGE                                                      
         ASPEC H3,2,C'COMPANY'                                                  
         ASPEC H3,86,C'PERIOD'                                                  
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H09,2,C'PROJECT CODE'                                            
         ASPEC H10,2,C'------------'                                            
         ASPEC H09,17,C'         PROJECT NAME               '                   
         ASPEC H10,17,C'------------------------------------'                   
         ASPEC H09,55,C'TASK CODE/DESCRIPTION'                                  
         ASPEC H10,55,C'---------------------'                                  
         ASPEC H09,79,C'  DATE  '                                               
         ASPEC H10,79,C'--------'                                               
         ASPEC H09,92,C'  HOURS '                                               
         ASPEC H10,92,C'REPORTED'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPC401 08/16/00'                                      
         END                                                                    
