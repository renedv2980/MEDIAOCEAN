*          DATA SET ACREPB101  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACB101A,+0                                                               
         TITLE 'ACB101 - SPECS FOR BUDGET LIST.'                                
         PRINT NOGEN                                                            
ACB101   CSECT                                                                  
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF MODE,PROCLEV                                                     
         ACDEF MAXLINES,55                                                      
*                                                                               
         ACDEF H1,48,C'BUDGET LIST'                                             
         ACDEF H2,48,C'-----------'                                             
         ACDEF H1,2,RUN                                                         
         ACDEF H1,84,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H3,84,REQUESTOR                                                  
         ACDEF H4,2,UNIT                                                        
         ACDEF H5,2,LEDGER                                                      
         ACDEF H3,2,COMPANY                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPB101 08/16/00'                                      
         END                                                                    
