*          DATA SET ACREPPC01  AT LEVEL 005 AS OF 08/17/00                      
*PHASE ACPC01A                                                                  
         TITLE 'ACPC01 - SPECS FOR PROJECT CONTROL'                             
ACPC01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF READ,TRANSACTIONS                                                
         ACDEF SET,ACCTBAL                                                      
         ACDEF RESET                                                            
         SPACE 1                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,42,C'PROJECT CONTROL REPORT'                                  
         ACDEF H1,85,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H2,42,22C'-'                                                     
         ACDEF H4,85,REQUESTOR                                                  
         ACDEF F1,2,REQDETS                                                     
         ACDEF MAXLINES,55                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPPC01 08/17/00'                                      
         END                                                                    
