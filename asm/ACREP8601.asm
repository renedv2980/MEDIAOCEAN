*          DATA SET ACREP8601  AT LEVEL 002 AS OF 08/16/00                      
*PHASE AC8601A                                                                  
         TITLE 'SPECS FOR T&&E ANALYSIS'                                        
AC8601   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF MODE,PROCLEV                                                     
         ACDEF SPROG,0                                                          
         ACDEF H1,42,C'STAFF T/E ANALYSIS REPORT'                               
         ACDEF H2,42,25C'-'                                                     
         ACDEF M1,2,C'CLIENT CODE - NAME'                                       
         ACDEF SPROG,1                                                          
         ACDEF H1,42,C'CLIENT T/E ANALYSIS REPORT'                              
         ACDEF H2,42,26C'-'                                                     
         ACDEF M1,2,C'STAFF CODE - NAME'                                        
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,81,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H3,2,COMPANY                                                     
         ACDEF H3,81,REQUESTOR                                                  
*&&US*&& ACDEF F1,2,REQDETS                                                     
         ACDEF SPROG,2                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP8601 08/16/00'                                      
         END                                                                    
